#' Picker block
#'
#' A curated column picker for locked, dashboard-style boards (the
#' value-selection sibling of blockr.dm's value filter block): the builder
#' defines one or more *pickers* in the gear, the viewer operates one
#' select per picker and nothing else. Each picker fixes which columns are
#' offered (`choices`), which is picked (`selected`), and -- crucially --
#' the name of the **output column** the pick lands in (`into`). Downstream
#' blocks map to the `into` names, which never change, so switching the
#' pick never requires reconfiguration.
#'
#' Semantics per picker:
#' * single (default): the picked column is *copied* into `into`, with its
#'   label attribute carried along (a chart's axis title follows the pick).
#'   Copies, not renames, so two pickers may pick the same source column
#'   (e.g. a measure against itself).
#' * `multiple = TRUE` (at most one picker): the picked columns pivot long --
#'   values into `into`, the measure identity into `<into>_measure` (a
#'   factor of column *labels*, ready for facetting). Offered-but-unpicked
#'   columns of this picker are dropped so the schema never depends on the
#'   pick.
#'
#' UI follows the blockr design system: `Blockr.Select` controls on the
#' block face (one per picker, labelled by `into`), and the picker
#' definitions in the gear-toggled settings band, like the value filter's
#' filter list.
#'
#' Design records: blockr.docs design-system/target select-controls.html
#' (control style) and measure-switch-proposals.html (schema; the picker
#' generalizes the measure switch to n pickers with editable output names).
#'
#' @param state List with `pickers` -- a list of picker entries, each
#'   `list(into, choices, selected, multiple)`. Empty (default) auto-fills
#'   on first data arrival with one picker over all numeric columns,
#'   `into = "value"`.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_picker_block(
#'       state = list(pickers = list(
#'         list(into = "value", choices = c("Sepal.Length", "Sepal.Width"),
#'              selected = "Sepal.Length", multiple = FALSE)
#'       ))
#'     ),
#'     data = list(data = iris)
#'   )
#' }
#'
#' @return A transform block of class `picker_block`
#' @export
new_picker_block <- function(
  state = list(pickers = list()),
  ...
) {
  pickers <- if (is.null(state$pickers)) list() else state$pickers

  blockr.core::new_transform_block(
    function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        r_pickers <- shiny::reactiveVal(pickers)

        set_if_changed <- function(rv, val) {
          if (!identical(rv(), val)) rv(val)
        }

        col_labels <- shiny::reactive({
          d <- data()
          cols <- colnames(d)
          vapply(
            cols,
            function(nm) {
              lb <- attr(d[[nm]], "label", exact = TRUE)
              if (is.null(lb) || !nzchar(lb)) "" else as.character(lb)
            },
            character(1)
          )
        })

        numeric_cols <- shiny::reactive({
          d <- data()
          colnames(d)[vapply(d, is.numeric, logical(1))]
        })

        # Reconcile with incoming data; auto-fill one picker on first arrival.
        shiny::observeEvent(data(), {
          pks <- normalize_pickers(r_pickers(), colnames(data()))
          if (!length(pks) && length(numeric_cols())) {
            pks <- list(list(
              into = "value",
              choices = numeric_cols(),
              selected = numeric_cols()[[1L]],
              multiple = FALSE
            ))
          }
          set_if_changed(r_pickers, pks)
        })

        # Push the full control state. Length-1 vectors unbox to scalars over
        # the wire; the JS side re-wraps them.
        shiny::observe({
          shiny::req(data())
          labs <- col_labels()
          session$sendCustomMessage(
            session$ns("pk_update"),
            list(
              cfg_options = lapply(numeric_cols(), function(nm) {
                list(value = nm, label = labs[[nm]])
              }),
              pickers = r_pickers()
            )
          )
        })

        # JS -> R: the whole picker list on any change (value-filter style).
        # Sent as a JSON string: a bare array of objects would arrive
        # simplified into a data.frame by Shiny's deserializer.
        shiny::observeEvent(input$pickers, {
          raw <- tryCatch(
            jsonlite::fromJSON(input$pickers, simplifyVector = FALSE),
            error = function(e) NULL
          )
          pks <- normalize_pickers(raw, colnames(data()))
          if (length(pks)) {
            set_if_changed(r_pickers, pks)
          }
        })

        list(
          expr = shiny::reactive(make_picker_expr(r_pickers())),
          state = list(state = shiny::reactive(list(pickers = r_pickers())))
        )
      })
    },
    function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        # Select component + shared block CSS from blockr.dplyr (exported
        # helpers); settings band from the LOCAL copy -- blockr.viz is the
        # canonical source of settings-band.css/js (see viz-block-dep.R).
        blockr.dplyr::blockr_select_dep(),
        blockr.dplyr::blockr_blocks_css_dep(),
        settings_band_dep(),
        shiny::div(
          class = "block-container blockr-picker",
          shiny::div(
            class = "blockr-gear-header",
            shiny::tags$button(
              id = ns("gear"),
              type = "button",
              class = "blockr-gear-btn",
              title = "Pickers"
            )
          ),
          shiny::div(
            id = ns("band"),
            class = "blockr-settings blockr-settings--beak",
            shiny::div(class = "blockr-settings__title", "Pickers"),
            shiny::div(id = ns("rows")),
            shiny::tags$button(
              id = ns("add"),
              type = "button",
              class = "pk-add",
              "+ Add picker"
            )
          ),
          shiny::div(id = ns("face"))
        ),
        picker_block_assets(ns)
      )
    },
    dat_valid = function(data) {
      stopifnot(is.data.frame(data))
    },
    expr_type = "bquoted",
    allow_empty_state = TRUE,
    class = "picker_block",
    ...
  )
}

# Coerce a client- or ctor-supplied picker list into canonical form against
# the current columns: choices restricted to existing columns, selected
# restricted to choices (first choice as fallback), single pick unless
# multiple, at most ONE multiple picker (a second pivot would cross-multiply
# rows), output names non-empty and unique.
normalize_pickers <- function(pickers, cols) {
  if (is.null(pickers) || !length(pickers)) {
    return(list())
  }
  out <- list()
  multi_seen <- FALSE
  for (p in pickers) {
    ch <- intersect(as.character(unlist(p$choices)), cols)
    if (!length(ch)) {
      next
    }
    sel <- intersect(as.character(unlist(p$selected)), ch)
    if (!length(sel)) {
      sel <- ch[[1L]]
    }
    mult <- isTRUE(p$multiple) && !multi_seen
    multi_seen <- multi_seen || mult
    if (!mult && length(sel) > 1L) {
      sel <- sel[[1L]]
    }
    into <- as.character(if (is.null(p$into)) "" else p$into)[[1L]]
    if (is.na(into) || !nzchar(into)) {
      into <- "value"
    }
    out[[length(out) + 1L]] <- list(
      into = into,
      choices = ch,
      selected = sel,
      multiple = mult
    )
  }
  if (length(out)) {
    intos <- make.unique(vapply(out, `[[`, character(1), "into"), sep = "")
    for (i in seq_along(out)) {
      out[[i]]$into <- intos[[i]]
    }
  }
  out
}

# Build the transform expression: per picker a labelled copy, or (multiple)
# a pivot into `into` + `<into>_measure`. `data` stays a free symbol.
make_picker_expr <- function(pickers) {
  if (!length(pickers)) {
    return(quote(identity(data)))
  }
  # Copies run before the pivot: the multiple picker consumes (and drops)
  # source columns that a single picker may also offer, so singles must
  # read the still-wide data.
  is_mult <- vapply(pickers, function(p) isTRUE(p$multiple), logical(1))
  pickers <- c(pickers[!is_mult], pickers[is_mult])
  bquote(
    local({
      pks <- .(pks)
      out <- data
      for (p in pks) {
        labs <- vapply(
          p$choices,
          function(nm) {
            lb <- attr(data[[nm]], "label", exact = TRUE)
            if (is.null(lb) || !nzchar(lb)) nm else as.character(lb)
          },
          character(1)
        )
        if (isTRUE(p$multiple)) {
          # Offered-but-unpicked columns of a multiple picker are dropped:
          # the output schema must not depend on which choices are picked.
          measure_col <- paste0(p$into, "_measure")
          drop <- setdiff(p$choices, p$selected)
          out <- tidyr::pivot_longer(
            out[setdiff(names(out), drop)],
            cols = tidyr::all_of(p$selected),
            names_to = measure_col,
            values_to = p$into
          )
          out[[measure_col]] <- factor(
            labs[out[[measure_col]]],
            levels = unname(labs[p$selected])
          )
          if (length(p$selected) == 1L) {
            attr(out[[p$into]], "label") <- unname(labs[p$selected])
          }
        } else {
          sel <- p$selected[[1L]]
          out[[p$into]] <- out[[sel]]
          attr(out[[p$into]], "label") <- unname(labs[[sel]])
        }
      }
      out
    }),
    list(pks = pickers)
  )
}

# Per-instance init script: renders the gear band's picker rows (into text
# input, choices multi-select, multiple checkbox, remove) and the face
# controls (one Blockr.Select per picker, labelled by `into`), and syncs
# whole-state with the server (value-filter style). The message handler
# registers at parse time so no push is ever dropped; payloads queue until
# the Blockr namespace is ready. Inline while the block settles -- then this
# becomes an inst/js asset behind htmlDependency.
picker_block_assets <- function(ns) {
  js <- sprintf(
    "(function() {
      var NS = { gear: '%s', band: '%s', rows: '%s', add: '%s', face: '%s' };
      var IN = { pickers: '%s' };
      var MSG = '%s';

      var state = { cfgOptions: [], pickers: [] };
      var pending = null, ready = false;

      function toArr(x) {
        if (x === null || x === undefined) return [];
        return Array.isArray(x) ? x : [x];
      }
      function norm(p) {
        return {
          into: String(p.into || ''),
          choices: toArr(p.choices).map(String),
          selected: toArr(p.selected).map(String),
          multiple: !!p.multiple
        };
      }
      function sig(pks) { return JSON.stringify(pks); }
      function optionFor(v) {
        var hit = state.cfgOptions.find(function (o) { return o.value === v; });
        return hit || { value: v, label: '' };
      }
      function el(id) { return document.getElementById(id); }
      function send() {
        // Stringified: a bare array of objects would be simplified into a
        // data.frame on the R side.
        Shiny.setInputValue(IN.pickers, JSON.stringify(state.pickers),
          { priority: 'event' });
      }

      Shiny.addCustomMessageHandler(MSG, function(msg) {
        if (ready) apply(msg); else pending = msg;
      });

      function apply(msg) {
        var cfg = toArr(msg.cfg_options).map(function (o) {
          return { value: String(o.value), label: String(o.label || '') };
        });
        var pks = toArr(msg.pickers).map(norm);
        var changed = sig(pks) !== sig(state.pickers) ||
          JSON.stringify(cfg) !== JSON.stringify(state.cfgOptions);
        state.cfgOptions = cfg;
        if (changed) {
          state.pickers = pks;
          renderBand();
          renderFace();
        }
      }

      function defaultInto() {
        var taken = state.pickers.map(function (p) { return p.into; });
        if (taken.indexOf('value') < 0) return 'value';
        var i = 2;
        while (taken.indexOf('value' + i) >= 0) i++;
        return 'value' + i;
      }

      function renderBand() {
        var host = el(NS.rows);
        host.innerHTML = '';
        state.pickers.forEach(function (p, i) {
          var row = document.createElement('div');
          row.className = 'pk-row';

          var intoWrap = document.createElement('div');
          intoWrap.className = 'pk-into-wrap';
          var intoLabel = document.createElement('label');
          intoLabel.className = 'blockr-label';
          intoLabel.textContent = 'Into';
          var into = document.createElement('input');
          into.type = 'text';
          into.className = 'blockr-text-input pk-into';
          into.value = p.into;
          into.addEventListener('change', function () {
            p.into = into.value;
            renderFace();
            send();
          });
          intoWrap.appendChild(intoLabel);
          intoWrap.appendChild(into);
          row.appendChild(intoWrap);

          var chWrap = document.createElement('div');
          chWrap.className = 'pk-choices';
          var chLabel = document.createElement('label');
          chLabel.className = 'blockr-label';
          chLabel.textContent = 'Columns offered';
          var chHost = document.createElement('div');
          chHost.className = 'blockr-select--bordered';
          chWrap.appendChild(chLabel);
          chWrap.appendChild(chHost);
          row.appendChild(chWrap);
          Blockr.Select.multi(chHost, {
            options: state.cfgOptions,
            selected: p.choices,
            placeholder: 'Columns offered to the viewer\\u2026',
            reorderable: true,
            onChange: function (sel) {
              var vals = toArr(sel).filter(Boolean);
              if (!vals.length) { renderBand(); return; }
              p.choices = vals;
              p.selected = p.selected.filter(function (v) {
                return vals.indexOf(v) >= 0;
              });
              if (!p.selected.length) p.selected = [vals[0]];
              renderFace();
              send();
            }
          });

          var boxWrap = document.createElement('div');
          boxWrap.className = 'pk-multiple blockr-checkbox-row';
          var box = Blockr.checkbox('Multiple', p.multiple, function (checked) {
            if (checked && state.pickers.some(function (q, j) {
              return j !== i && q.multiple;
            })) {
              box.set(false);
              return;
            }
            p.multiple = checked;
            if (!checked && p.selected.length > 1) p.selected = [p.selected[0]];
            renderFace();
            send();
          });
          boxWrap.appendChild(box.el);
          row.appendChild(boxWrap);

          var rm = document.createElement('button');
          rm.type = 'button';
          rm.className = 'pk-remove';
          rm.title = 'Remove picker';
          rm.textContent = '\\u00d7';
          rm.addEventListener('click', function () {
            if (state.pickers.length <= 1) return;
            state.pickers.splice(i, 1);
            renderBand();
            renderFace();
            send();
          });
          row.appendChild(rm);

          host.appendChild(row);
        });
      }

      function renderFace() {
        var host = el(NS.face);
        host.innerHTML = '';
        state.pickers.forEach(function (p) {
          var field = document.createElement('div');
          field.className = 'pk-field';
          var label = document.createElement('label');
          label.className = 'blockr-label';
          label.textContent = p.into;
          var selHost = document.createElement('div');
          selHost.className = 'blockr-select--bordered';
          field.appendChild(label);
          field.appendChild(selHost);
          host.appendChild(field);
          var mode = p.multiple ? 'multi' : 'single';
          var handle = Blockr.Select[mode](selHost, {
            options: p.choices.map(optionFor),
            selected: p.multiple ? p.selected : (p.selected[0] || null),
            placeholder: 'Select\\u2026',
            onChange: function (sel) {
              var vals = toArr(sel).filter(Boolean);
              if (!vals.length) {
                // The last pick stays: restore instead of committing empty.
                handle.setOptions(p.choices.map(optionFor),
                  p.multiple ? p.selected : (p.selected[0] || null));
                return;
              }
              p.selected = vals;
              send();
            }
          });
        });
      }

      function init() {
        if (!window.Blockr || !Blockr.Select || !Blockr.checkbox) {
          setTimeout(init, 50); return;
        }
        var gear = el(NS.gear), band = el(NS.band), add = el(NS.add);
        if (!gear || !band || !add) { setTimeout(init, 50); return; }
        gear.innerHTML = Blockr.icons.gear;
        gear.addEventListener('click', function () {
          var open = band.classList.toggle('blockr-settings--open');
          gear.classList.toggle('blockr-gear-active', open);
        });
        add.addEventListener('click', function () {
          var first = state.cfgOptions.length ? state.cfgOptions[0].value : null;
          if (!first) return;
          state.pickers.push({
            into: defaultInto(),
            choices: state.cfgOptions.map(function (o) { return o.value; }),
            selected: [first],
            multiple: false
          });
          renderBand();
          renderFace();
          send();
        });
        ready = true;
        if (pending) { apply(pending); pending = null; }
      }
      init();
    })();",
    ns("gear"), ns("band"), ns("rows"), ns("add"), ns("face"),
    ns("pickers"),
    ns("pk_update")
  )
  shiny::tagList(
    shiny::tags$style(shiny::HTML(
      ".blockr-picker .pk-field { margin: 2px 0 8px; }
       .blockr-picker .blockr-label { display: block; margin-bottom: 4px; }
       .blockr-picker .pk-row { display: flex; gap: 12px; align-items: flex-end;
         width: 100%; margin-bottom: 8px; }
       .blockr-picker .pk-into { width: 140px; }
       .blockr-picker .pk-choices { flex: 1; min-width: 220px; }
       .blockr-picker .pk-multiple { padding-bottom: 10px; }
       .blockr-picker .pk-remove { border: none; background: none; cursor: pointer;
         color: var(--blockr-grey-400, #9ca3af); font-size: 16px; line-height: 1;
         padding: 0 2px 12px; }
       .blockr-picker .pk-remove:hover { color: var(--blockr-color-danger, #dc3545); }
       .blockr-picker .pk-add { border: 1px dashed var(--blockr-color-border, #e5e7eb);
         background: none; border-radius: 999px; padding: 3px 12px; cursor: pointer;
         font-size: 0.75rem; color: var(--blockr-color-text-muted, #6b7280); }
       .blockr-picker .pk-add:hover { border-color: var(--blockr-color-primary, #2563eb);
         color: var(--blockr-color-primary, #2563eb); }"
    )),
    shiny::tags$script(shiny::HTML(js))
  )
}

register_picker_block <- function() {
  blockr.core::register_block(
    "new_picker_block",
    name = "Picker",
    description = paste0(
      "Curated column pickers for locked dashboards: each picker offers a ",
      "fixed set of columns and lands the pick in a stable, named output ",
      "column, so downstream mappings never change. A multiple picker ",
      "pivots its picks long (into + into_measure) for facetting."
    ),
    category = "transform",
    # ui-radios: an option list with one picked -- the block's whole gesture.
    # Header color comes from the category (transform = #009E73 green).
    icon = "ui-radios",
    guidance = paste0(
      "Use upstream of a chart or table whose mappings should stay fixed ",
      "while the viewer switches what is shown. Define one picker per ",
      "viewer control: `into` names the output column (downstream maps to ",
      "it), `choices` fixes what is offered, `selected` is the current ",
      "pick. Single pickers copy the picked column into `into` (label ",
      "carried), so two pickers may pick the same source. At most one ",
      "picker may set `multiple`: its picks pivot long into `into` + ",
      "`<into>_measure` (factor of column labels) and its unpicked choices ",
      "are dropped -- map a chart's facet to `<into>_measure`."
    ),
    arguments = blockr.core::new_arg_specs(
      state = blockr.core::new_arg_spec(
        "Object with `pickers`: array of picker entries",
        example = list(pickers = list(list(
          into = "value",
          choices = list("Sepal.Length", "Sepal.Width"),
          selected = list("Sepal.Length"),
          multiple = FALSE
        ))),
        type = blockr.core::arg_object(
          pickers = blockr.core::arg_array(
            blockr.core::arg_object(
              into = blockr.core::arg_string(),
              choices = blockr.core::arg_array(blockr.core::arg_string()),
              selected = blockr.core::arg_array(blockr.core::arg_string()),
              multiple = blockr.core::arg_boolean()
            )
          )
        )
      )
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
