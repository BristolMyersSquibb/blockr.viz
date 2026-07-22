#' Measure switch block
#'
#' A curated measure picker for locked boards: the builder fixes which
#' columns are offered (`choices`), the viewer picks one (or several, with
#' `multiple = TRUE`). The block always emits the long shape -- a `measure`
#' column (factor of column *labels*) plus a `value` column -- even for a
#' single pick, so a downstream chart mapped to `value` (and optionally
#' facetted by `measure`) never sees a schema change when the selection
#' grows or shrinks. Offered-but-unpicked measures are dropped for the same
#' reason.
#'
#' UI follows the blockr design system: the viewer control is a
#' `Blockr.Select` (single mode, or multi with tags when `multiple`), and
#' the builder settings (measures offered, allow multiple) live in the
#' gear-toggled in-flow settings band, exactly like blockr.dplyr's
#' pivot-longer block. An empty selection is never committed: the control
#' snaps back to the last non-empty pick.
#'
#' Design record: blockr.docs/design-system/target/measure-switch-proposals.html
#' (control style superseded by select-controls.html -- blockr-select
#' everywhere).
#'
#' @param choices Character vector of columns offered to the viewer. Empty
#'   (default) auto-fills with all numeric columns on first data arrival.
#' @param selected Character vector of currently picked column(s). Empty
#'   defaults to the first choice.
#' @param multiple Allow picking several measures at once (viewer select
#'   switches to tag mode; downstream typically facets by `measure`).
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A transform block of class `measure_switch_block`
#' @export
new_measure_switch_block <- function(
  choices = character(),
  selected = character(),
  multiple = FALSE,
  ...
) {
  choices <- as.character(if (is.null(choices)) character() else unlist(choices))
  selected <- as.character(if (is.null(selected)) character() else unlist(selected))
  multiple <- isTRUE(multiple)

  blockr.core::new_transform_block(
    function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        r_choices <- shiny::reactiveVal(choices)
        r_selected <- shiny::reactiveVal(selected)
        r_multiple <- shiny::reactiveVal(multiple)

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

        # Reconcile config with incoming data; auto-fill on first arrival.
        shiny::observeEvent(data(), {
          ch <- intersect(r_choices(), colnames(data()))
          if (!length(ch)) {
            ch <- numeric_cols()
          }
          set_if_changed(r_choices, ch)
          sel <- intersect(r_selected(), ch)
          if (!length(sel) && length(ch)) {
            sel <- ch[[1L]]
          }
          if (!r_multiple() && length(sel) > 1L) {
            sel <- sel[[1L]]
          }
          set_if_changed(r_selected, sel)
        })

        # Push the full control state to the client. Length-1 vectors
        # unbox to scalars over the wire; the JS side re-wraps them.
        shiny::observe({
          shiny::req(data())
          labs <- col_labels()
          opt <- function(cols) {
            lapply(cols, function(nm) list(value = nm, label = labs[[nm]]))
          }
          session$sendCustomMessage(
            session$ns("ms_update"),
            list(
              cfg_options = opt(numeric_cols()),
              options = opt(r_choices()),
              choices = as.list(r_choices()),
              selected = as.list(r_selected()),
              multiple = r_multiple()
            )
          )
        })

        # Builder side: which columns are offered.
        shiny::observeEvent(input$choices, {
          ch <- intersect(as.character(unlist(input$choices)), colnames(data()))
          if (!length(ch)) {
            return()
          }
          set_if_changed(r_choices, ch)
          sel <- intersect(r_selected(), ch)
          if (!length(sel)) {
            sel <- ch[[1L]]
          }
          set_if_changed(r_selected, sel)
        })

        shiny::observeEvent(input$multiple, {
          set_if_changed(r_multiple, isTRUE(input$multiple))
          if (!isTRUE(input$multiple) && length(r_selected()) > 1L) {
            r_selected(r_selected()[[1L]])
          }
        }, ignoreInit = TRUE)

        # Viewer side: never commit an empty selection (the client snaps
        # back on its own; this is the backstop).
        shiny::observeEvent(input$selected, {
          raw <- as.character(unlist(input$selected))
          sel <- intersect(raw, r_choices())
          if (length(sel)) {
            set_if_changed(r_selected, sel)
          }
        }, ignoreNULL = FALSE, ignoreInit = TRUE)

        list(
          expr = shiny::reactive({
            sel <- r_selected()
            if (!length(sel)) {
              # No pick yet (fresh block, data pending): pass through, but
              # keep the expression a call, not a bare symbol.
              quote(identity(data))
            } else {
              bquote(
                local({
                  cols <- .(cols)
                  unpicked <- .(unpicked)
                  labs <- vapply(
                    cols,
                    function(nm) {
                      lb <- attr(data[[nm]], "label", exact = TRUE)
                      if (is.null(lb) || !nzchar(lb)) nm else as.character(lb)
                    },
                    character(1)
                  )
                  # Offered-but-unpicked measures are dropped, not carried:
                  # the output schema (non-measure columns + measure + value)
                  # must not depend on which choice is currently picked.
                  out <- tidyr::pivot_longer(
                    data[setdiff(names(data), unpicked)],
                    cols = tidyr::all_of(cols),
                    names_to = "measure",
                    values_to = "value"
                  )
                  out$measure <- factor(
                    labs[match(out$measure, cols)],
                    levels = unname(labs)
                  )
                  if (length(cols) == 1L) {
                    attr(out$value, "label") <- unname(labs)[[1L]]
                  }
                  out
                }),
                list(cols = sel, unpicked = setdiff(r_choices(), sel))
              )
            }
          }),
          state = list(
            choices = r_choices,
            selected = r_selected,
            multiple = r_multiple
          )
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
          class = "block-container measure-switch",
          shiny::div(
            class = "blockr-gear-header",
            shiny::tags$button(
              id = ns("gear"),
              type = "button",
              class = "blockr-gear-btn",
              title = "Settings"
            )
          ),
          shiny::div(
            id = ns("band"),
            class = "blockr-settings blockr-settings--beak",
            shiny::div(class = "blockr-settings__title", "Settings"),
            shiny::div(
              class = "ms-field",
              shiny::tags$label(class = "blockr-label", "Measures offered"),
              shiny::div(
                id = ns("cfg_choices"),
                class = "blockr-select--bordered"
              )
            ),
            shiny::div(id = ns("cfg_multiple"), class = "blockr-checkbox-row")
          ),
          shiny::div(
            class = "ms-field",
            shiny::tags$label(
              id = ns("viewer_label"),
              class = "blockr-label",
              "Measure"
            ),
            shiny::div(id = ns("viewer"), class = "blockr-select--bordered")
          )
        ),
        measure_switch_script(ns)
      )
    },
    dat_valid = function(data) {
      stopifnot(is.data.frame(data))
    },
    expr_type = "bquoted",
    allow_empty_state = c("choices", "selected"),
    class = "measure_switch_block",
    ...
  )
}

# Per-instance init script: instantiates the design-system controls
# (Blockr.Select viewer + config picker, Blockr.checkbox, gear/band toggle)
# and applies server pushes. Message handler registers at parse time so no
# push is ever dropped; payloads queue until the Blockr namespace is ready.
# Inline for the sandbox prototype -- on graduation this becomes an inst/js
# asset behind htmlDependency, ideally via blockr.dplyr's JS-block factory.
measure_switch_script <- function(ns) {
  js <- sprintf(
    "(function() {
      var NS = { gear: '%s', band: '%s', viewer: '%s', viewerLabel: '%s',
                 cfgChoices: '%s', cfgMultiple: '%s' };
      var IN = { selected: '%s', choices: '%s', multiple: '%s' };
      var MSG = '%s';

      var state = { options: [], cfgOptions: [], selected: [],
                    multiple: false };
      var viewer = null, cfg = null, box = null;
      var pending = null, ready = false;

      function toArr(x) {
        if (x === null || x === undefined) return [];
        return Array.isArray(x) ? x : [x];
      }

      Shiny.addCustomMessageHandler(MSG, function(msg) {
        if (ready) apply(msg); else pending = msg;
      });

      function el(id) { return document.getElementById(id); }

      function buildViewer() {
        var host = el(NS.viewer);
        if (viewer) { viewer.destroy(); host.innerHTML = ''; }
        var mode = state.multiple ? 'multi' : 'single';
        viewer = Blockr.Select[mode](host, {
          options: state.options,
          selected: state.multiple ? state.selected : (state.selected[0] || null),
          placeholder: 'Select measure\\u2026',
          onChange: function(sel) {
            var vals = toArr(sel).filter(Boolean);
            if (!vals.length) {
              // Last measure stays selected: restore instead of committing.
              viewer.setOptions(state.options, state.multiple
                ? state.selected : (state.selected[0] || null));
              return;
            }
            state.selected = vals;
            Shiny.setInputValue(IN.selected, vals);
          }
        });
        el(NS.viewerLabel).textContent = state.multiple ? 'Measures' : 'Measure';
      }

      function apply(msg) {
        state.cfgOptions = toArr(msg.cfg_options);
        state.options = toArr(msg.options);
        state.selected = toArr(msg.selected);
        var multi = !!msg.multiple;
        var rebuild = !viewer || multi !== state.multiple;
        state.multiple = multi;
        if (rebuild) buildViewer();
        else viewer.setOptions(state.options, state.multiple
          ? state.selected : (state.selected[0] || null));
        cfg.setOptions(state.cfgOptions, toArr(msg.choices));
        box.set(state.multiple);
      }

      function init() {
        if (!window.Blockr || !Blockr.Select || !Blockr.checkbox) {
          setTimeout(init, 50); return;
        }
        var gear = el(NS.gear), band = el(NS.band);
        if (!gear || !band) { setTimeout(init, 50); return; }
        gear.innerHTML = Blockr.icons.gear;
        gear.addEventListener('click', function() {
          var open = band.classList.toggle('blockr-settings--open');
          gear.classList.toggle('blockr-gear-active', open);
        });
        cfg = Blockr.Select.multi(el(NS.cfgChoices), {
          options: [],
          selected: [],
          placeholder: 'Columns offered to the viewer\\u2026',
          reorderable: true,
          onChange: function(sel) {
            var vals = toArr(sel).filter(Boolean);
            if (vals.length) Shiny.setInputValue(IN.choices, vals);
          }
        });
        box = Blockr.checkbox('Allow multiple picks', false, function(checked) {
          Shiny.setInputValue(IN.multiple, checked);
        });
        el(NS.cfgMultiple).appendChild(box.el);
        ready = true;
        if (pending) { apply(pending); pending = null; }
      }
      init();
    })();",
    ns("gear"), ns("band"), ns("viewer"), ns("viewer_label"),
    ns("cfg_choices"), ns("cfg_multiple"),
    ns("selected"), ns("choices"), ns("multiple"),
    ns("ms_update")
  )
  shiny::tagList(
    shiny::tags$style(shiny::HTML(
      ".measure-switch .ms-field { margin: 2px 0 8px; }
       .measure-switch .ms-field .blockr-label { display: block; margin-bottom: 4px; }"
    )),
    shiny::tags$script(shiny::HTML(js))
  )
}

register_measure_switch_block <- function() {
  blockr.core::register_block(
    "new_measure_switch_block",
    name = "Measure Switch",
    description = paste0(
      "One curated selector: pick which measure column(s) flow downstream. ",
      "Emits a long measure + value shape (measure holds column labels), so ",
      "a chart mapped to value never needs reconfiguring when the pick ",
      "changes. (tidyr: pivot_longer)"
    ),
    category = "transform",
    # ui-radios: an option list with one picked -- the block's whole gesture.
    # Header color comes from the category (transform = #009E73 green).
    icon = "ui-radios",
    guidance = paste0(
      "Use upstream of a chart or table whose mappings should stay fixed ",
      "while the viewer switches the measure. `choices` fixes which columns ",
      "are offered, `selected` is the current pick, `multiple` allows ",
      "several at once (downstream typically facets by `measure`). The ",
      "output always has a `measure` column (factor of column labels) and a ",
      "`value` column, plus all columns not in `choices` -- offered-but-",
      "unpicked measures are dropped so the schema never depends on the pick."
    ),
    arguments = blockr.core::new_arg_specs(
      choices = blockr.core::new_arg_spec(
        "Array of column names offered to the viewer",
        example = list("Sepal.Length", "Sepal.Width"),
        type = blockr.core::arg_array(blockr.core::arg_string())
      ),
      selected = blockr.core::new_arg_spec(
        "Array of currently picked column name(s), subset of choices",
        example = list("Sepal.Length"),
        type = blockr.core::arg_array(blockr.core::arg_string())
      ),
      multiple = blockr.core::new_arg_spec(
        "Boolean -- allow picking several measures at once",
        example = FALSE,
        type = blockr.core::arg_boolean()
      )
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
