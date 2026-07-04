#' Tile Block (KPI renderer)
#'
#' The bold display of a handful of important numbers -- the KPI-card /
#' scorecard family. Parallel to the `chart` and `table` interactive
#' renderers. It maps columns to slots and draws them, in one of two layouts
#' of identical content (`cards` or `table`), and is opt-in clickable as a
#' filter (`drill`) using the same transport as the chart / table. Like the
#' table it can aggregate in place (`group` + `summaries`, the shared
#' aggregation vocabulary) into grand-total or per-group cards; secondaries
#' (delta / fill / pill) stay precomputed upstream (a tile is not a pivot).
#'
#' # Input contract
#'
#' The canonical input is a long "tile frame": one row per (group, measure)
#' cell. Wide input is also accepted -- when `value` names multiple numeric
#' columns each becomes a measure (measure name = column name) and the
#' renderer pivots to long internally (covers the one-row
#' `summarize(across())` case).
#'
#' # Slots
#'
#' `overline` (supertext), `value` (the main number, + `format` + `unit`) ,
#' one `secondary` (a precomputed reference column drawn with a `style`) ,
#' `caption` (subtext). The secondary `style` is one of `plain` (show the
#' reference), `delta` (up/down % colored by sign x `good_when`), `fill` (a
#' progress bar to a fraction), `pill` (a status chip). `delta` / `target` /
#' `max` are not distinct elements -- they are one `secondary` + a style.
#'
#' @param value Character vector of numeric column name(s). Multiple columns
#'   => wide input (each becomes a measure).
#' @param group Grouping column: clusters cards / matrix rows, and the
#'   `group_by` column when `summaries` aggregate the input in place. `""` = no
#'   grouping.
#' @param name Column naming each KPI (long input). `""` = use the `value`
#'   column names as measure names.
#' @param summaries Optional in-block aggregations: a list, each entry
#'   `list(func, cols)` (`func` one of count / count_distinct / mean /
#'   median / sum / min / max). Each metric becomes a card. With `group` set,
#'   one cluster per group level; with `group` empty, grand-total cards. A
#'   display projection only -- the block's data output stays the raw input
#'   filtered by the click. Empty (default) = no aggregation.
#' @param layout `"cards"` or `"table"`. Same slots either way.
#' @param overline,caption Supertext / subtext. A column name reads per-cell;
#'   any other non-empty string is a literal. `overline` defaults to the
#'   measure ("Name") label and is LEGACY: it has no gear control and is not
#'   in the registry (two pickers fed the same visual slot and read as
#'   duplicates) -- the arg stays accepted so saved boards keep rendering.
#' @param secondary Precomputed reference column drawn as the secondary.
#'   `""` = none.
#' @param style Secondary display style: `plain` / `delta` / `fill` / `pill`.
#' @param good_when LEGACY, ignored: polarity is always `"up"` (an increase
#'   reads good). The arg stays accepted so saved boards restore; there is no
#'   gear control and no registry entry.
#' @param format How the number is formatted (never a currency guess):
#'   `"number"` (separators + smart decimals), `"compact"` (1.2M / 38.4K), or
#'   `"percent"` (a fraction x100 + %).
#' @param unit Free-text unit label shown next to the value / in the matrix
#'   header (e.g. `"USD"`, `"CHF"`, `"apples"`). This is how you label a
#'   currency -- the renderer never infers `$`.
#' @param drill Logical; when `TRUE` a card / matrix-row click emits a
#'   categorical filter downstream. The filter column is structurally
#'   determined (never user-picked): the `group` column when grouped, else
#'   the `name` column on an ungrouped long KPI list. Off by
#'   default; inert when the tile has neither (bare KPI, grand totals).
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
#' @examplesIf interactive()
#' new_tile_block()
#' @export
new_tile_block <- function(value = character(),
                           group = character(),
                           name = "",
                           summaries = list(),
                           layout = "cards",
                           overline = "",
                           caption = "",
                           secondary = "",
                           style = "plain",
                           good_when = "up",
                           format = "number",
                           unit = "",
                           drill = FALSE,
                           filter_col = NULL,
                           filter_value = NULL,
                           ...) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        r_value     <- shiny::reactiveVal(as.character(value))
        r_group     <- shiny::reactiveVal(as.character(group))
        r_name      <- shiny::reactiveVal(name)
        r_summaries   <- shiny::reactiveVal(dd_parse_summaries(summaries))
        r_layout    <- shiny::reactiveVal(layout)
        r_overline  <- shiny::reactiveVal(overline)
        r_caption   <- shiny::reactiveVal(caption)
        r_secondary <- shiny::reactiveVal(secondary)
        r_style     <- shiny::reactiveVal(style)
        r_good_when <- shiny::reactiveVal(good_when)
        r_format    <- shiny::reactiveVal(format)
        r_unit      <- shiny::reactiveVal(unit)
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
              group     = upd(r_group, tk_group(v)),
              name      = upd(r_name, tk_blank(v)),
              summaries   = upd(r_summaries, dd_parse_summaries(v)),
              secondary = upd(r_secondary, tk_blank(v)),
              style     = upd(r_style, v %||% "plain"),
              good_when = upd(r_good_when, v %||% "up"),
              format    = upd(r_format, v %||% "number"),
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
            value = r_value(), group = r_group(), measure = r_name(),
            summaries = r_summaries(), layout = r_layout(),
            overline = r_overline(), caption = r_caption(),
            secondary = r_secondary(), style = r_style(),
            # Polarity is ALWAYS "up" (an increase reads good): the gear
            # control and registry arg are gone (Christoph); the ctor arg is
            # legacy-ignored so old saved boards restore without erroring.
            good_when = "up", format = r_format(), unit = r_unit(),
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
            value = r_value, group = r_group, name = r_name,
            summaries = r_summaries, layout = r_layout, overline = r_overline,
            caption = r_caption, secondary = r_secondary, style = r_style,
            good_when = r_good_when, format = r_format, unit = r_unit,
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
    # Optional roles only -- clearing a non-listed field would wedge the block
    # (see reference_blockr_allow_empty_state_wedge). The enum fields
    # (layout/style/good_when/format) always carry a value and are omitted.
    allow_empty_state = c("value", "group", "name", "summaries", "overline",
      "caption", "secondary", "unit", "drill", "filter_col", "filter_value"),
    external_ctrl = c("value", "group", "name", "summaries", "layout",
      "overline", "caption", "secondary", "style", "good_when", "format",
      "unit", "drill", "filter_col", "filter_value"),
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

#' Normalize the single-group picker value to `character(0)` (none) or the one
#' picked column. The tile groups by a single dimension (a KPI clusters by one),
#' so `group` is stored as a length-0/1 character vector.
#' @noRd
tk_group <- function(v) {
  g <- as.character(unlist(v))
  g <- g[!is.na(g) & nzchar(g) & !g %in% "(none)"]
  if (length(g)) g[1L] else character()
}

#' HTML dependency for the tile renderer.
#'
#' Mirrors the table block dependency: the shared blockr.dplyr CSS/JS (gear,
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
      name = "chart-css",
      version = paste0(utils::packageVersion("blockr.viz"), ".27"),
      src = system.file("css", package = "blockr.viz"),
      stylesheet = "chart.css"
    ),
    settings_band_dep(),
    # Shared aggregation vocabulary + gear engine (one dep, one version — see
    # drilldown_shared_dep()). Before tile-block.js, which reads both globals.
    drilldown_shared_dep(),
    htmltools::htmlDependency(
      name = "tile-block",
      version = paste0(utils::packageVersion("blockr.viz"), ".9"),
      src = system.file(package = "blockr.viz"),
      script = "js/tile-block.js",
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
  new_block_args(
    value = new_block_arg(
      paste0(
        "Numeric column(s) shown as the big number. One column for a long ",
        "tile frame; MULTIPLE columns for wide input (each column becomes a ",
        "measure / card, measure name = column name). Required."
      ),
      example = list("revenue"),
      type = arg_array(arg_string())
    ),
    group = new_block_arg(
      paste0(
        "Grouping column: clusters cards / drives the matrix rows, and is the ",
        "dplyr::group_by column when `summaries` aggregate the input in place. ",
        "One column (a KPI clusters by a single dimension). Optional; \"\" = a ",
        "single ungrouped set of cards (or grand-total cards when `summaries` is ",
        "set)."
      ),
      example = "region",
      type = arg_string()
    ),
    name = new_block_arg(
      paste0(
        "The column NAMING each KPI, for LONG input (one row per group x ",
        "measure); the name shows above the value and drives per-KPI number ",
        "formatting and the matrix columns. Leave \"\" for wide input \u2014 then ",
        "the `value` column names are the KPI names. (Gear label: \"Name\".)"
      ),
      example = "",
      type = arg_string()
    ),
    summaries = new_block_arg(
      paste0(
        "In-block aggregations shown as cards: a list, each entry ",
        "`{func, cols}`. `func` is one of \"count\", \"count_distinct\", ",
        "\"mean\", \"median\", \"sum\", \"min\", \"max\"; `cols` the numeric ",
        "column(s) it reduces. Empty `cols` on a NUMERIC aggregation = ALL ",
        "numeric columns except those claimed by another entry; empty for ",
        "\"count\" (needs no column). One card per metric. With ",
        "`group` empty the summaries reduce the whole frame to grand-total cards; ",
        "with `group` set, one cluster of cards per group level. This is a ",
        "DISPLAY projection \u2014 a card / row click still drills to the raw rows, ",
        "and downstream receives the raw (filtered) data. Empty = no ",
        "aggregation (the tile renders precomputed input, as before)."
      ),
      example = list(
        list(func = "mean", cols = list("revenue")),
        list(func = "count", cols = list())
      )
    ),
    layout = new_block_arg(
      "Layout: \"cards\" (grid of cards) or \"table\" (aligned matrix).",
      example = "cards",
      type = arg_enum(c("cards", "table"))
    ),
    secondary = new_block_arg(
      paste0(
        "A PRECOMPUTED reference column drawn beside the value (a delta, a ",
        "fraction, a status). The renderer does no arithmetic \u2014 compute the ",
        "comparison upstream. Optional; \"\" = no secondary."
      ),
      example = "rev_delta",
      type = arg_string()
    ),
    style = new_block_arg(
      paste0(
        "How to draw the secondary: \"plain\" (show the reference), \"delta\" ",
        "(arrow + %, colored by sign), \"fill\" (progress bar to a fraction), ",
        "\"pill\" (status chip)."
      ),
      example = "delta",
      type = arg_enum(c("plain", "delta", "fill", "pill"))
    ),
    format = new_block_arg(
      paste0(
        "How the NUMBER is formatted (never a currency guess): \"number\" ",
        "(separators + smart decimals, default), \"compact\" (1.2M / 38.4K), ",
        "or \"percent\" (a fraction x100 + %)."
      ),
      example = "compact",
      type = arg_enum(c("number", "compact", "percent"))
    ),
    unit = new_block_arg(
      paste0(
        "Free-text unit label shown next to the value / in the matrix header ",
        "(e.g. \"USD\", \"CHF\", \"apples\", \"kg\"). This is how you label a ",
        "currency \u2014 the renderer never infers \"$\"."
      ),
      example = "USD",
      type = arg_string()
    ),
    caption = new_block_arg(
      "Subtext below the value: a column name (per-cell) or a literal.",
      example = "",
      type = arg_string()
    ),
    drill = new_block_arg(
      paste0(
        "When TRUE, clicking a card / matrix row emits a categorical filter ",
        "downstream (the same contract as the chart / table). The filter ",
        "column is determined by the tile's structure, never picked: the ",
        "`group` column when grouped, else the `name` column on an ",
        "ungrouped long KPI list. Off by default; inert when the tile has ",
        "neither (a bare single KPI, or grand-total metric cards)."
      ),
      example = TRUE,
      type = arg_boolean()
    )
  )
}

#' Construction guidance for the tile block
#' @noRd
tile_guidance <- function() {
  paste(
    "Bold display of a handful of important numbers \u2014 the KPI-card /",
    "scorecard renderer. Map `value` to the number(s) \u2014 one column for a",
    "long frame, or several columns for wide input (each becomes a card). It",
    "can aggregate in place: set `summaries` (a list of {func, cols}) to reduce",
    "the input to grand-total cards, or add `group` for one cluster of cards",
    "per group level \u2014 the same vocabulary as the table. Leave `summaries`",
    "empty and shape upstream to render precomputed numbers as-is. Add a",
    "precomputed `secondary` column and a `style` (delta / fill / pill /",
    "plain) for a comparison; set `good_when` for the polarity. Use `group`",
    "to cluster and to drive the matrix rows in the table layout. Set",
    "`drill = TRUE` to make a card / row click filter downstream on `group`.",
    "Not a chart (no trends \u2014 that is the chart renderer) and not a dense",
    "data table."
  )
}
