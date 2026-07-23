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
#' * `optional = TRUE` (single pickers only): the face gains a leading
#'   `(none)` entry. Selecting it leaves the picker inert -- no output
#'   column -- so a downstream chart mapped to `into` drops that aesthetic
#'   (no legend / no facet strip) rather than showing a single phantom
#'   group. Use it for optional roles like colour or facet; leave it off for
#'   required roles (x/y axes).
#'
#' UI follows the blockr design system: `Blockr.Select` controls on the
#' block face (one per picker, labelled by `into`), and the picker
#' definitions in the gear-toggled settings band, like the value filter's
#' filter list. Every column qualifies as a candidate -- a picker can just
#' as well drive a grouping or color dimension as a measure. A picker's
#' offer list starts empty: the builder curates which columns the viewer
#' sees. An empty offer list is inert (no output column) until it has
#' choices, which also covers clearing it mid-edit to refill.
#'
#' Design records: blockr.docs design-system/target select-controls.html
#' (control style) and measure-switch-proposals.html (schema; the picker
#' generalizes the measure switch to n pickers with editable output names).
#'
#' @param state List with `pickers` -- a list of picker entries, each
#'   `list(into, choices, selected, multiple, optional)`. Empty (default)
#'   seeds one empty, inert picker (`into = "value"`) on first data arrival
#'   for the builder to fill.
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

        # Reconcile with incoming data; seed one EMPTY picker on first arrival
        # so the builder has a row to fill. The offer list starts empty on
        # purpose -- the builder curates which columns the viewer sees; the
        # block does nothing until at least one column is offered.
        shiny::observeEvent(data(), {
          pks <- normalize_pickers(r_pickers())
          if (!length(pks) && ncol(data())) {
            pks <- list(list(
              into = "value",
              choices = character(),
              selected = character(),
              multiple = FALSE
            ))
          }
          set_if_changed(r_pickers, pks)
        })

        # Push the full control state. Length-1 vectors unbox to scalars over
        # the wire; the JS side re-wraps them. ALL columns are candidates in
        # the gear's "Columns offered" pool (a picker may drive a grouping or
        # color dimension, not just a measure); the builder chooses from it.
        shiny::observe({
          shiny::req(data())
          labs <- col_labels()
          session$sendCustomMessage(
            session$ns("pk_update"),
            list(
              cfg_options = lapply(colnames(data()), function(nm) {
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
          pks <- normalize_pickers(raw)
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
            shiny::div(id = ns("rows"), class = "pk-rows"),
            shiny::div(
              class = "blockr-add-row",
              shiny::tags$span(
                id = ns("add"),
                class = "blockr-add-link",
                shiny::tags$span(class = "blockr-add-icon"),
                "Add picker"
              )
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

# Coerce a client- or ctor-supplied picker list into canonical form. This is a
# STRUCTURAL normalization only -- it deliberately does NOT prune choices
# against the data columns. The authored picker definition is the source of
# truth and must survive a data frame that transiently lacks its columns (e.g.
# on board restore, while an upstream block is still restoring its own state and
# briefly emits a frame without them). Pruning here was destructive: intersect
# against such a transient frame wiped the offer/selection permanently, so the
# gear's "Columns offered" and the face pick came back empty after loading.
# Restriction to the columns actually present happens instead in
# make_picker_expr() at eval time, so a picker self-heals when its columns
# return. Rules: selected is a subset of choices (first choice as fallback when
# empty), single pick unless multiple, at most ONE multiple picker (a second
# pivot would cross-multiply rows), output names non-empty and unique.
normalize_pickers <- function(pickers) {
  if (is.null(pickers) || !length(pickers)) {
    return(list())
  }
  out <- list()
  multi_seen <- FALSE
  for (p in pickers) {
    ch <- as.character(unlist(p$choices))
    sel <- intersect(as.character(unlist(p$selected)), ch)
    mult <- isTRUE(p$multiple) && !multi_seen
    multi_seen <- multi_seen || mult
    # `optional` (single pickers only): the viewer may pick "(none)", leaving
    # the picker inert so it emits no output column -- the downstream chart
    # then drops that aesthetic (no legend / no facet). For an OPTIONAL picker
    # an empty selection is a legitimate "(none)", so it is NOT auto-filled to
    # the first choice; a required picker keeps the >=1 backstop.
    opt <- isTRUE(p$optional) && !mult
    # An empty-choices picker is kept, inert: the builder may have just
    # cleared the offer list to refill it (expr skips it meanwhile).
    if (!length(sel) && length(ch) && !opt) {
      sel <- ch[[1L]]
    }
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
      multiple = mult,
      optional = opt
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
  # Inert pickers (choices cleared mid-edit) contribute nothing.
  pickers <- Filter(function(p) length(p$selected) > 0L, pickers)
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
        # Restrict to columns actually present in the data at eval time. A
        # picker whose columns are temporarily absent (an upstream block still
        # settling on restore) is skipped and recovers on its own once they
        # return -- the stored definition is never mutated.
        present <- intersect(p$choices, names(data))
        sel <- intersect(p$selected, present)
        if (!length(sel)) next
        labs <- vapply(
          present,
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
          drop <- setdiff(present, sel)
          out <- tidyr::pivot_longer(
            out[setdiff(names(out), drop)],
            cols = tidyr::all_of(sel),
            names_to = measure_col,
            values_to = p$into
          )
          out[[measure_col]] <- factor(
            labs[out[[measure_col]]],
            levels = unname(labs[sel])
          )
          if (length(sel) == 1L) {
            attr(out[[p$into]], "label") <- unname(labs[sel])
          }
        } else {
          s1 <- sel[[1L]]
          out[[p$into]] <- out[[s1]]
          attr(out[[p$into]], "label") <- unname(labs[[s1]])
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
  # Only the ns() substitution goes through sprintf; the JS body is a plain
  # string appended via paste0. sprintf caps its format string at 8192 chars
  # and the full script exceeds that.
  head_js <- sprintf(
    "var NS = { gear: '%s', band: '%s', rows: '%s', add: '%s', face: '%s' };
      var IN = { pickers: '%s' };
      var MSG = '%s';",
    ns("gear"), ns("band"), ns("rows"), ns("add"), ns("face"),
    ns("pickers"), ns("pk_update")
  )
  js <- paste0(
    "(function() {
      ", head_js, "

      var state = { cfgOptions: [], pickers: [] };
      var pending = null, ready = false;
      // Sentinel for the viewer-facing '(none)' choice of an OPTIONAL picker.
      // Never sent to R: selecting it clears the pick (selected = []), which
      // makes the picker inert -- no output column -- so a downstream chart
      // drops the aesthetic. Blockr.Select shows an option's VALUE as its
      // primary text, so the sentinel IS the label the viewer reads; a real
      // column literally named '(none)' is not a concern for chart data.
      var NONE = '(none)';

      function toArr(x) {
        if (x === null || x === undefined) return [];
        return Array.isArray(x) ? x : [x];
      }
      function norm(p) {
        return {
          into: String(p.into || ''),
          choices: toArr(p.choices).map(String),
          selected: toArr(p.selected).map(String),
          multiple: !!p.multiple,
          optional: !!p.optional
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
          row.className = 'pk-row blockr-row';

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
              // May go empty mid-edit (clear, then refill): the picker
              // goes inert until it has choices again.
              var vals = toArr(sel).filter(Boolean);
              p.choices = vals;
              p.selected = p.selected.filter(function (v) {
                return vals.indexOf(v) >= 0;
              });
              if (!p.selected.length && vals.length) p.selected = [vals[0]];
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
            // Optional is single-picker only; turning Multiple on retires it.
            if (checked) p.optional = false;
            renderBand();
            renderFace();
            send();
          });
          boxWrap.appendChild(box.el);
          row.appendChild(boxWrap);

          // Optional: lets the viewer pick '(none)' to turn the aesthetic off.
          // Meaningless for a multiple picker (deselecting all already yields
          // no pivot), so it is only offered on single pickers.
          if (!p.multiple) {
            var optWrap = document.createElement('div');
            optWrap.className = 'pk-optional blockr-checkbox-row';
            var optBox = Blockr.checkbox('Optional', p.optional,
              function (checked) {
                p.optional = checked;
                // Leaving optional while on '(none)': restore a real pick so a
                // now-required picker is not stuck inert.
                if (!checked && !p.selected.length && p.choices.length) {
                  p.selected = [p.choices[0]];
                }
                renderFace();
                send();
              });
            optWrap.appendChild(optBox.el);
            row.appendChild(optWrap);
          }

          var rm = document.createElement('button');
          rm.type = 'button';
          rm.className = 'blockr-row-remove';
          rm.title = 'Remove picker';
          rm.innerHTML = Blockr.icons.x;
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
          var isOpt = !!p.optional && !p.multiple;
          // Optional single pickers offer a leading '(none)' entry; picking it
          // (or clearing) commits an empty selection = inert = aesthetic off.
          var opts = p.choices.map(optionFor);
          if (isOpt) opts = [{ value: NONE, label: '' }].concat(opts);
          function faceSel() {
            if (p.multiple) return p.selected;
            return p.selected[0] || (isOpt ? NONE : null);
          }
          var handle = Blockr.Select[mode](selHost, {
            options: opts,
            selected: faceSel(),
            placeholder: 'Select\\u2026',
            onChange: function (sel) {
              var vals = toArr(sel).filter(Boolean);
              if (isOpt && (!vals.length || vals[0] === NONE)) {
                // '(none)' or cleared: commit the empty pick (no restore).
                p.selected = [];
                send();
                return;
              }
              if (!vals.length) {
                // Required picker: the last pick stays -- restore, don't commit.
                handle.setOptions(opts, faceSel());
                return;
              }
              p.selected = vals.filter(function (v) { return v !== NONE; });
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
        var addIcon = add.querySelector('.blockr-add-icon');
        if (addIcon) addIcon.innerHTML = Blockr.icons.plus;
        gear.addEventListener('click', function () {
          var open = band.classList.toggle('blockr-settings--open');
          gear.classList.toggle('blockr-gear-active', open);
        });
        add.addEventListener('click', function () {
          // Start empty: the builder curates which columns this picker offers.
          state.pickers.push({
            into: defaultInto(),
            choices: [],
            selected: [],
            multiple: false,
            optional: false
          });
          renderBand();
          renderFace();
          send();
        });
        ready = true;
        if (pending) { apply(pending); pending = null; }
      }
      init();
    })();"
  )
  shiny::tagList(
    shiny::tags$style(shiny::HTML(
      ".blockr-picker .pk-field { margin: 2px 0 8px; }
       .blockr-picker .blockr-label { display: block; margin-bottom: 4px; }
       /* The rows host must own the full band width in the flex settings band
          (the band is display:flex;flex-wrap:wrap), else it sizes to content
          and each picker card sits in a cramped flex track. */
       .blockr-picker .pk-rows { flex: 1 1 100%; min-width: 0; }
       /* Each picker is a vertical card (was a horizontal row that overflowed
          narrow blocks): fields stack full width and shrink to fit, mirroring
          blockr.dm's value filter. .blockr-row supplies the card border +
          hover-reveal remove; we only flip its axis to column. */
       .blockr-picker .pk-row { flex-direction: column; align-items: stretch;
         gap: 8px; min-height: 0; padding: 10px 12px; position: relative; }
       .blockr-picker .pk-into-wrap { width: 100%; min-width: 0; }
       .blockr-picker .pk-into { width: 100%; box-sizing: border-box; }
       .blockr-picker .pk-choices { width: 100%; min-width: 0; }
       .blockr-picker .pk-choices .blockr-select--bordered { min-width: 0; }
       .blockr-picker .pk-multiple,
       .blockr-picker .pk-optional { padding-bottom: 0; }
       .blockr-picker .pk-row .blockr-row-remove { position: absolute; top: 6px;
         right: 6px; margin: 0; }"
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
          multiple = FALSE,
          optional = FALSE
        ))),
        type = blockr.core::arg_object(
          pickers = blockr.core::arg_array(
            blockr.core::arg_object(
              into = blockr.core::arg_string(),
              choices = blockr.core::arg_array(blockr.core::arg_string()),
              selected = blockr.core::arg_array(blockr.core::arg_string()),
              multiple = blockr.core::arg_boolean(),
              optional = blockr.core::arg_boolean()
            )
          )
        )
      )
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
