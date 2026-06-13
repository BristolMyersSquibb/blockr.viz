#' Tile Block (KPI renderer)
#'
#' A pure renderer for the bold display of a handful of important numbers —
#' the KPI-card / scorecard family. Parallel to the `chart` and `table`
#' interactive renderers: all shaping (`summarize` / `describe` / a lag-join
#' for comparisons) happens upstream; the tile does no arithmetic. It maps
#' columns to slots and draws them, in one of two layouts of identical
#' content (`cards` or `table`), and is opt-in clickable as a filter (`drill`)
#' using the same transport as the chart / table.
#'
#' # Input contract
#'
#' The canonical input is a long "tile frame": one row per (group, measure)
#' cell. Wide input is also accepted — when `value` names multiple numeric
#' columns each becomes a measure (measure name = column name) and the
#' renderer pivots to long internally (covers the one-row
#' `summarize(across())` case).
#'
#' # Slots
#'
#' `overline` (supertext) · `value` (the main number, + `format` + `unit`) ·
#' one `secondary` (a precomputed reference column drawn with a `style`) ·
#' `caption` (subtext). The secondary `style` is one of `plain` (show the
#' reference), `delta` (▲▼ % colored by sign × `good_when`), `fill` (a
#' progress bar to a fraction), `pill` (a status chip). `delta` / `target` /
#' `max` are not distinct elements — they are one `secondary` + a style.
#'
#' @param value Character vector of numeric column name(s). Multiple columns
#'   => wide input (each becomes a measure).
#' @param by Group/facet column (matrix rows / card clusters). `""` = none.
#' @param measure Measure-label column for long input. `""` = use the `value`
#'   column names as measure names.
#' @param layout `"cards"` or `"table"`. Same slots either way.
#' @param overline,caption Supertext / subtext. A column name reads per-cell;
#'   any other non-empty string is a literal. `overline` defaults to the
#'   measure label.
#' @param secondary Precomputed reference column drawn as the secondary.
#'   `""` = none.
#' @param style Secondary display style: `plain` / `delta` / `fill` / `pill`.
#' @param good_when Polarity for delta / fill / pill coloring: `"up"` (an
#'   increase is good) or `"down"` (a decrease is good — e.g. churn).
#' @param format Value format: `"auto"` (inferred) / `"int"` / `"pct"` /
#'   `"usd"` / `"compact"`.
#' @param unit Unit suffix shown next to the value / in the matrix header.
#' @param measures Optional per-measure override list, keyed by measure name,
#'   each `list(style, good_when, format, unit)`. Lets a matrix draw Revenue
#'   as `delta` and Budget as `fill`. Falls back to the flat defaults.
#' @param drill Logical; when `TRUE` a card / matrix-row click emits a
#'   categorical filter on the `by` column downstream. Off by default; only
#'   active when `by` is mapped.
#' @param filter_col,filter_value Click-filter state. Kept as constructor
#'   params so the filter round-trips through save/restore (blockr.core
#'   deserializes a block via its constructor formals).
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A transform block of class `tile_block`. Its `result` is the
#'   upstream data filtered by the last click (passthrough when no drill),
#'   exactly like [new_table_block()].
#'
#' @seealso [tile_demo_data()].
#' @export
new_tile_block <- function(value = character(),
                           by = "",
                           measure = "",
                           layout = "cards",
                           overline = "",
                           caption = "",
                           secondary = "",
                           style = "plain",
                           good_when = "up",
                           format = "auto",
                           unit = "",
                           measures = list(),
                           drill = FALSE,
                           filter_col = NULL,
                           filter_value = NULL,
                           ...) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        r_value     <- shiny::reactiveVal(as.character(value))
        r_by        <- shiny::reactiveVal(by)
        r_measure   <- shiny::reactiveVal(measure)
        r_layout    <- shiny::reactiveVal(layout)
        r_overline  <- shiny::reactiveVal(overline)
        r_caption   <- shiny::reactiveVal(caption)
        r_secondary <- shiny::reactiveVal(secondary)
        r_style     <- shiny::reactiveVal(style)
        r_good_when <- shiny::reactiveVal(good_when)
        r_format    <- shiny::reactiveVal(format)
        r_unit      <- shiny::reactiveVal(unit)
        r_measures  <- shiny::reactiveVal(measures)
        r_drill     <- shiny::reactiveVal(isTRUE(drill))
        r_filter_col   <- shiny::reactiveVal(filter_col)
        r_filter_value <- shiny::reactiveVal(filter_value)

        # Only write when the value actually changes. The JS echoes the full
        # config on any popover change, so a blind set would re-render on
        # every echo (the R->JS->R loop guard the chart / table use).
        upd <- function(rv, v) {
          if (!identical(shiny::isolate(rv()), v)) rv(v)
        }

        # JS -> R: gear config edits + drill clicks via the `_action` input.
        shiny::observeEvent(input$tile_block_action, {
          msg <- input$tile_block_action
          if (is.null(msg)) return()
          act <- msg$action %||% "config"
          if (identical(act, "filter")) {
            upd(r_filter_col, msg$column)
            upd(r_filter_value, msg$values)
          } else if (identical(act, "config")) {
            p <- msg$param
            v <- msg$value
            switch(
              p,
              value     = upd(r_value, as.character(v)),
              by        = upd(r_by, tk_blank(v)),
              measure   = upd(r_measure, tk_blank(v)),
              secondary = upd(r_secondary, tk_blank(v)),
              style     = upd(r_style, v %||% "plain"),
              good_when = upd(r_good_when, v %||% "up"),
              format    = upd(r_format, v %||% "auto"),
              unit      = upd(r_unit, as.character(v %||% "")),
              overline  = upd(r_overline, tk_blank(v)),
              caption   = upd(r_caption, tk_blank(v)),
              layout    = upd(r_layout, v %||% "cards"),
              drill     = upd(r_drill, isTRUE(v) || identical(v, "true")),
              NULL
            )
          }
        })

        output$tile_result <- shiny::renderUI({
          d <- data()
          shiny::req(is.data.frame(d))
          tile_html(
            d,
            value = r_value(), by = r_by(), measure = r_measure(),
            layout = r_layout(), overline = r_overline(),
            caption = r_caption(), secondary = r_secondary(),
            style = r_style(), good_when = r_good_when(),
            format = r_format(), unit = r_unit(), measures = r_measures(),
            drill = r_drill(), elem_id = ns("tile_block")
          )
        })

        list(
          expr = shiny::reactive({
            col  <- r_filter_col()
            vals <- r_filter_value()
            if (is.null(col) || is.null(vals) || length(vals) == 0) {
              blockr.core::bbquote(dplyr::filter(.(data), TRUE))
            } else if (length(vals) == 1) {
              blockr.core::bbquote(
                dplyr::filter(.(data), .data[[.(col)]] == .(val)),
                list(col = col, val = vals[[1]])
              )
            } else {
              blockr.core::bbquote(
                dplyr::filter(.(data), .data[[.(col)]] %in% .(vals)),
                list(col = col, vals = vals)
              )
            }
          }),
          state = list(
            value = r_value, by = r_by, measure = r_measure,
            layout = r_layout, overline = r_overline, caption = r_caption,
            secondary = r_secondary, style = r_style, good_when = r_good_when,
            format = r_format, unit = r_unit, measures = r_measures,
            drill = r_drill, filter_col = r_filter_col,
            filter_value = r_filter_value
          )
        )
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(shiny::uiOutput(ns("tile_result")))
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) stop("Input must be a data frame")
    },
    # Optional roles only — clearing a non-listed field would wedge the block
    # (see reference_blockr_allow_empty_state_wedge). The enum fields
    # (layout/style/good_when/format) always carry a value and are omitted.
    allow_empty_state = c("value", "by", "measure", "overline", "caption",
      "secondary", "unit", "measures", "drill", "filter_col", "filter_value"),
    external_ctrl = c("value", "by", "measure", "layout", "overline",
      "caption", "secondary", "style", "good_when", "format", "unit",
      "measures", "drill", "filter_col", "filter_value"),
    expr_type = "bquoted",
    class = "tile_block",
    ...
  )
}

#' Normalize a column-picker value to "" (none) or the picked column.
#' @noRd
tk_blank <- function(v) {
  if (is.null(v) || length(v) == 0L) return("")
  v <- as.character(v)[1]
  if (is.na(v) || identical(v, "(none)") || !nzchar(v)) "" else v
}

#' HTML dependency for the tile renderer.
#'
#' Mirrors [drilldown_table_dep()]: the shared blockr.dplyr CSS/JS (gear,
#' popover, Blockr.Select, icons), the drilldown-chart popover CSS (the dd-*
#' classes the config engine emits), then the shared config engine
#' (drilldown-config.js) which must load before the tile JS.
#' @noRd
tile_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "blockr-blocks-css",
      version = paste0(utils::packageVersion("blockr.dplyr"), ".2"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = c("blockr-blocks.css", "blockr-select.css")
    ),
    htmltools::htmlDependency(
      name = "blockr-select-js",
      version = paste0(utils::packageVersion("blockr.dplyr"), ".2"),
      src = system.file("js", package = "blockr.dplyr"),
      script = c("blockr-core.js", "blockr-select.js")
    ),
    htmltools::htmlDependency(
      name = "drilldown-chart-css",
      version = paste0(utils::packageVersion("blockr.bi"), ".24"),
      src = system.file("css", package = "blockr.bi"),
      stylesheet = "drilldown-chart.css"
    ),
    htmltools::htmlDependency(
      name = "tile-block",
      version = utils::packageVersion("blockr.bi"),
      src = system.file(package = "blockr.bi"),
      # drilldown-config.js (the shared engine) must load before tile-block.js.
      script = c("js/drilldown-config.js", "js/tile-block.js"),
      stylesheet = "css/tile-block.css"
    )
  )
}

#' Build arguments metadata for the tile block (registry / LLM surface).
#'
#' Exposes every role to the assistant and MCP. Flat string / array args (no
#' nested AST) so the model fills them reliably; the per-measure `measures`
#' override is an author-only escape hatch and is intentionally omitted from
#' the AI surface.
#' @noRd
tile_arguments <- function() {
  structure(
    c(
      value = paste0(
        "Numeric column(s) shown as the big number. One column for a long ",
        "tile frame; MULTIPLE columns for wide input (each column becomes a ",
        "measure / card, measure name = column name). Required."
      ),
      by = paste0(
        "Group / facet column: clusters cards and drives the matrix rows in ",
        "the table layout. Optional; \"\" = a single ungrouped set of cards."
      ),
      measure = paste0(
        "Measure-label column, for LONG input (one row per group x measure). ",
        "Leave \"\" for wide input — then the `value` column names are the ",
        "measure names."
      ),
      layout = "Layout: \"cards\" (grid of cards) or \"table\" (aligned matrix).",
      secondary = paste0(
        "A PRECOMPUTED reference column drawn beside the value (a delta, a ",
        "fraction, a status). The renderer does no arithmetic — compute the ",
        "comparison upstream. Optional; \"\" = no secondary."
      ),
      style = paste0(
        "How to draw the secondary: \"plain\" (show the reference), \"delta\" ",
        "(arrow + %, colored by sign), \"fill\" (progress bar to a fraction), ",
        "\"pill\" (status chip)."
      ),
      good_when = paste0(
        "Polarity for coloring: \"up\" (an increase is good, default) or ",
        "\"down\" (a decrease is good, e.g. churn / cost)."
      ),
      format = paste0(
        "Value format: \"auto\" (inferred from name + range), \"int\", ",
        "\"pct\", \"usd\", or \"compact\" (1.2M)."
      ),
      unit = "Unit suffix shown next to the value / in the matrix header.",
      overline = paste0(
        "Supertext above the value. A column name reads per-cell; any other ",
        "string is a literal label. Defaults to the measure name."
      ),
      caption = "Subtext below the value: a column name (per-cell) or a literal.",
      drill = paste0(
        "When TRUE, clicking a card / matrix row emits a categorical filter ",
        "on the `by` column downstream (the same contract as the chart / ",
        "table). Off by default; only meaningful with `by` set."
      )
    ),
    examples = list(
      value = list("revenue"), by = "region", measure = "",
      layout = "cards", secondary = "rev_delta", style = "delta",
      good_when = "up", format = "auto", unit = "", overline = "",
      caption = "", drill = TRUE
    ),
    prompt = paste(
      "Bold display of a handful of important numbers — the KPI-card /",
      "scorecard renderer. A PURE renderer: do all shaping upstream",
      "(summarize / describe / a lag-join for comparisons); the tile does no",
      "arithmetic. Map `value` to the number(s) — one column for a long",
      "frame, or several columns for wide input (each becomes a card). Add a",
      "precomputed `secondary` column and a `style` (delta / fill / pill /",
      "plain) for a comparison; set `good_when` for the polarity. Use `by`",
      "to cluster by a group and to drive the matrix rows in the table",
      "layout. Set `drill = TRUE` to make a card / row click filter",
      "downstream on `by`. Not a chart (no trends — that is the chart",
      "renderer) and not a dense data table."
    )
  )
}
