#' Drilldown Table
#'
#' Tabular sibling to the drilldown chart. Renders a plain rectangular
#' data frame as an interactive HTML table (sticky header, client-side
#' sort and search). Two optional capabilities, both off by default:
#'
#' - **Coloring.** When `color` is a [drilldown_table_color()] spec,
#'   numeric cells get a value-to-background scale (diverging for
#'   correlation matrices, sequential for heatmaps). Purely
#'   presentational; never changes the data.
#' - **Drill-down.** When `drill` names a column, clicking a row
#'   emits the same categorical filter message the drilldown chart
#'   emits, so existing filter links keep working.
#'
#' @param data A data frame.
#' @param label_col Row-stub column. Defaults to the first column.
#' @param value_cols Columns rendered as the table body. Defaults to
#'   every column except `label_col`.
#' @param color `NULL` (plain table) or a [drilldown_table_color()]
#'   list.
#' @param drill `NULL` (no drill-down) or a column name; clicking a
#'   row emits a categorical filter on that column's value.
#' @param elem_id Shiny namespaced id used to build the `_action`
#'   input name. Required for `drill` to do anything.
#' @param digits Number format of numeric cells, as `format` on
#'   [new_table_block()]: `"number"` (default), `"compact"`, `"percent"`
#'   or a number of decimals, printed exactly.
#' @param max_height CSS max-height of the scroll container.
#' @param row_hex,row_color Optional per-row colouring. `row_color` names a
#'   column whose values drive a row background scale; `row_hex` supplies
#'   explicit per-row hex colours. Both default `NULL` (no row colouring).
#' @param gear Draw the settings cogwheel? `FALSE` for a caller whose display
#'   options are fixed and who has no block state to store an edit in -- the
#'   gear writes its edits back over the same `_action` input the drill uses,
#'   so a caller that does not handle them would swallow every one.
#' @param download_slot Optional tag for the toolbar's download control,
#'   placed beside the search box. A ready-made tag rather than a `download =
#'   TRUE` flag on purpose: the writers need a session to register their
#'   handlers on, which this wrapper does not have and should not ask for.
#'   [dt_download_control()] builds the tag for a caller that does.
#'
#' @details
#' **Structured "Table 1" input.** When `data` follows the dotted-column
#' convention that [summary_table()] emits -- the wide display grid with
#' `.group<k>` / `.group<k>_level`, `.variable*`, `.label`, `.indent` columns
#' (summary_table's internal long `.fmt` form is rejected with an error) --
#' the renderer switches to the *structured* layout: row-side section
#' headers synthesized from grouping-value runs with collapse/expand
#' toggles, `.indent` row stubs, and
#' multi-level column spanners parsed from `|`-delimited column names (each
#' leaf carries its header text in `attr(col, "label")`). This is the same
#' structure `html_table()` renders, folded into the interactive table so
#' one renderer covers flat *and* structured tables (drill / cell colour
#' still apply to the flat path). A plain rectangular frame renders exactly
#' as before.
#'
#' @return An [htmltools::tagList()].
#' @examples
#' drilldown_table(head(mtcars), label_col = "mpg")
#' @export
drilldown_table <- function(data,
                            label_col = NULL,
                            value_cols = NULL,
                            color = NULL,
                            drill = NULL,
                            elem_id = NULL,
                            digits = "number",
                            max_height = "600px",
                            row_hex = NULL,
                            row_color = NULL,
                            gear = TRUE,
                            download_slot = NULL) {
  stopifnot(is.data.frame(data))
  # Tidy `.fmt` form (numbers + per-row template + `.group`) -> wide display
  # grid (format-then-spread), and detect the structured / sectioned form.
  # No-op on a plain flat frame.
  data <- fmt_to_wide(data)
  dt_chrome(
    elem_id    = elem_id,
    structured = dt_is_structured(data),
    max_height = max_height,
    gear       = gear,
    download_slot = download_slot,
    # This exported wrapper keeps its historical API (a single
    # drilldown_table_color() spec + row_color); internally the renderer
    # speaks the unified vocabulary -- `shadings` (a LIST of {mode, cols}
    # value-encoding rules) and `color` (the categorical identity column).
    inner      = dt_table_tag(data, label_col, value_cols,
                              shadings = dd_parse_shadings(color),
                              drill = drill, digits = digits,
                              row_hex = row_hex, color = row_color)
  )
}

#' Build just the `<table>` (thead + tbody) plus the data-attributes the table
#' JS reads off it (drill onclick col/idx, colour mode, digits).
#'
#' Split from the chrome (`dt_chrome()`: search bar, gear, scroll container) so
#' a data or config change re-renders ONLY the table -- the chrome renders once
#' and is never rebuilt, so there is no whole-panel blank-out and the search
#' text / scroll position survive a filter. See the block server's
#' `dt_result` (chrome) / `dt_table` (this) output split.
#' @noRd
dt_table_tag <- function(data, label_col = NULL, value_cols = NULL,
                         shadings = list(), drill = NULL, digits = "number",
                         row_hex = NULL, color = NULL,
                         sortable = TRUE, collapsible = TRUE, search = TRUE,
                         download = FALSE, rotate_titles = FALSE,
                         group_cols = NULL,
                         group = character(), summaries = list(),
                         active = NULL, gear_cols = NULL) {
  # The inter-block currency is the wide annotated df; summary_table()'s
  # internal long dialect errors here instead of being silently pivoted.
  reject_long_form(data)
  # Display-option states (sortable / collapsible / search / download) ride on
  # the <table> as data-attributes so the gear popover reads their current
  # value; the renderer honours `sortable` / `collapsible` here, while `search`
  # and the download control are realized by the chrome (dt_chrome /
  # dt_download).
  toggles <- list(sortable = sortable, collapsible = collapsible,
                  search = search, download = download,
                  rotate_titles = rotate_titles)
  if (dt_is_structured(data)) {
    return(dt_table_tag_structured(data, drill, digits, toggles,
                                   active = active))
  }
  # Flat path: the cell model (display / shading / raw vectors) is computed
  # by dt_flat_build() -- ONE builder shared with the block server's
  # data-push payload (dt_flat_payload), so the pasted HTML here and the
  # rows table.js assembles client-side cannot drift. See R/table-push.R and
  # dev/table-data-push-design.md. Numeric columns are formatted with a
  # single vectorized `formatC(format = "f", drop0trailing = TRUE)` call --
  # each value formats independently (no decimal alignment across the
  # column, so 1.5 stays "1.5" while a sibling 2.25 stays "2.25"),
  # byte-identical to the old per-element `format(round(v), nsmall = 0,
  # trim = TRUE)` but ~11x faster (the per-cell `vapply(format())` was ~72%
  # of the build time on a numeric-heavy frame).
  # NOTE (follow-up): to keep the row-color bar visible when scrolling a
  # wide table horizontally, the stub cell's bar should become a dedicated
  # left:0 sticky indicator cell -- which needs the sort/drill column-index
  # map to skip it.
  b <- dt_flat_build(data, label_col, value_cols, shadings, drill, digits,
                     row_hex, color, toggles, group_cols, group, summaries,
                     active, gear_cols)
  if (identical(b$kind, "message")) return(b$tag)
  dt_flat_assemble_tag(b)
}

# ---------------------------------------------------------------------------
# Structured ("Table 1") rendering -- section nesting, indents, spanners.
# Reuses html_table()'s thead / tbody builders so the structure matches the
# static html_table renderer exactly; only the surrounding chrome (the
# table-block wrapper, gear, drill/colour attributes) differs.
# ---------------------------------------------------------------------------

#' Does this (already wide) frame carry the dotted-column structure?
#'
#' True when it has any row-side grouping / identity column (the ARD-named
#' `.group<k>*` / `.variable*` pairs) OR a `.label` stub OR an `.indent` /
#' `.strong` column (the positional fallback dialect) -- the signals
#' [summary_table()] and coercing producers emit and `html_table()` renders.
#' @noRd
dt_is_structured <- function(data) {
  any(grepl("^\\.(group\\d+(_level|_label)?|variable(_level|_label)?|label|indent|strong)$",
            names(data)))
}

#' Numeric cells of a structured frame, printed in the block's number format
#' with the unit in their header. A nested tidy frame (table_nest()) keeps
#' its numbers as numbers; a Table 1 or a converted finished table holds text
#' cells and passes unchanged.
#' @noRd
dt_structured_numbers <- function(data, data_cols, digits = "number",
                                  unit = NULL) {
  num <- data_cols[vapply(data[data_cols], is.numeric, logical(1L))]
  unit <- chr_state(trimws(as.character(unlist(unit))))
  for (cn in num) {
    x <- data[[cn]]
    lbl <- dt_col_label(x, cn) %||% cn
    out <- rep(NA_character_, length(x))
    ok <- !is.na(x)
    out[ok] <- dt_format_num(x[ok], digits)
    attributes(out) <- attributes(x)[setdiff(names(attributes(x)),
                                             c("class", "levels"))]
    if (!is.null(unit)) attr(out, "label") <- paste0(lbl, " (", unit, ")")
    data[[cn]] <- out
  }
  data
}

#' Do a frame's cells hold numbers the number format can change?
#' @noRd
dt_has_number_cells <- function(data) {
  if (!is.data.frame(data)) return(FALSE)
  cols <- names(data)[!startsWith(names(data), ".")]
  any(vapply(data[cols], is.numeric, logical(1L)))
}

#' Build the structured ("Table 1") `<table>` for the interactive table-block.
#'
#' Builds the section-aware `<tbody>` and multi-level `<thead>` with
#' `build_html_tbody()` / `build_html_thead()` (the very builders
#' `html_table()` uses) and tags it with the drill onclick attributes. The
#' surrounding chrome (search bar, gear, scroll container, the
#' `drilldown-table-structured` class that gates the Table-1 CSS) is added once
#' by `dt_chrome()`. Cell colour over `.fmt` strings is not meaningful, so
#' structured tables render uncoloured; the stub column is the drill target
#' when `drill` names it.
#' @noRd
dt_table_tag_structured <- function(data, drill, digits, toggles = NULL,
                                    active = NULL) {
  sortable    <- toggles$sortable %||% TRUE
  collapsible <- toggles$collapsible %||% TRUE
  wrap_titles <- toggles$wrap_titles %||% TRUE
  view <- annotated_structure_view(data)
  data <- view$data
  section_cols <- view$section_cols
  stub_col     <- if (".label" %in% names(data)) ".label" else NULL
  styling_cols <- intersect(c(".indent", ".strong", ".emph"), names(data))
  # Every reserved column (grouping values and their name/label companions)
  # stays out of the data cells or it leaks in as a literal em-dash column.
  data_cols    <- setdiff(names(data),
                          c(view$reserved_cols, stub_col, styling_cols))
  data <- dt_structured_numbers(data, data_cols, digits, toggles$unit)

  if (length(data_cols) == 0L || nrow(data) == 0L) {
    # Structured frames carry no gear-picked mappings, so only two states
    # apply here: no rows vs no data columns left after the structural
    # (identity / .label / styling) columns.
    msg <- if (nrow(data) == 0L) "No rows to display"
           else "No value columns to display"
    return(dt_table_attrs(dt_message_table(msg), NULL, NULL, digits,
                          toggles = toggles, gear_cols = "[]"))
  }

  # Structured drill is opt-in via the gear's picker-less Drill-down checkbox
  # (config param `drill`, "auto" -- same convention as the grouped table /
  # tile) or the `drill=` ctor arg. When on, every data row / section header
  # carries its identity keys as data attributes (see
  # dd_row_drill_attrs) and the click branch in table.js emits the shared
  # grouped-filter payload from them.
  drill_on <- !is.null(drill) && nzchar(drill)

  # Column identity, when the producer stamped it (see annotated_column_keys).
  # Read only while the drill is on: without a drill a click means nothing, so
  # a clickable-looking header would be a lie.
  col_keys <- if (drill_on) annotated_column_keys(data)

  thead <- build_html_thead(
    data, data_cols, stub_col,
    stub_sortable = FALSE,
    sortable = sortable,
    col_keys = col_keys,
    collapsible = collapsible
  )
  nest <- html_is_nest(data, section_cols, styling_cols)
  tbody <- build_html_tbody(data, section_cols, stub_col, data_cols,
                            styling_cols = styling_cols,
                            collapsible = collapsible,
                            drill_keys = drill_on, nest = nest,
                            drill_col = if (drill_on) drill)

  table_tag <- dt_fixed_table_tag(
    thead, tbody,
    structured_colgroup(data, data_cols, stub_col, wrap_names = wrap_titles)
  )
  if (nest) {
    table_tag <- htmltools::tagAppendAttributes(table_tag,
                                                class = "blockr-table-nest")
  }
  # The chrome may have been rendered before the data existed, in which case it
  # carries no `drilldown-table-structured` class. This marker lets table.js
  # promote it onto the container at wire time.
  table_tag <- htmltools::tagAppendAttributes(
    table_tag, `data-dt-structured` = "1"
  )
  if (drill_on) {
    table_tag <- htmltools::tagAppendAttributes(
      table_tag, `data-dt-structured-drill` = "1"
    )
  }
  # Cells inherit their column's claim by POSITION: one map on the table,
  # indexed the way `cell.cellIndex` counts, rather than an attribute per
  # cell (a Table 1 is long, and that would be n x m of the same few strings).
  if (length(col_keys)) {
    table_tag <- htmltools::tagAppendAttributes(
      table_tag,
      `data-dd-colkeys-map` = dd_col_keys_map_json(
        data_cols, col_keys, stub_offset = if (is.null(stub_col)) 0L else 1L
      )
    )
  }

  # "[]" (not absent): a structured frame HAS no pickable columns -- the gear
  # must read that, not fall back to scraping the section-spanner header.
  dt_table_attrs(table_tag, NULL, NULL, digits,
                 toggles = toggles, active = active, gear_cols = "[]",
                 drill_mode = if (drill_on) drill)
}

#' Cell-visual spec for [drilldown_table()]
#'
#' Describes how numeric cells are decorated. Three modes, all purely
#' presentational (the data is never changed):
#'
#' - `"diverging"` -- a value-to-background scale centred on 0 with a
#'   symmetric domain (correlation matrices: -1 white-at-0 +1).
#' - `"sequential"` -- a low-to-high background ramp (heatmaps).
#' - `"bar"` -- an in-cell horizontal data bar proportional to the value
#'   (e.g. "patients with most adverse events"), normalised per column on
#'   absolute magnitude and left-anchored.
#'
#' @param type `"diverging"`, `"sequential"`, or `"bar"`.
#' @param domain `NULL` to infer from the data, or `c(min, max)`.
#'   Ignored for `"bar"` (always per-column). For `"diverging"` the
#'   inferred domain is symmetric around 0.
#' @param palette `NULL` for type defaults, or a character vector of
#'   colors -- length 3 for diverging (low/mid/high), length 2 for
#'   sequential (low/high), length 1 for the bar fill.
#' @param columns `NULL`/empty to apply to **all** numeric columns (a
#'   rule, re-resolved against whatever data arrives -- survives upstream
#'   schema changes), or a character vector to restrict to those columns.
#'   A picked column that no longer exists is silently skipped.
#' @param source `NULL` (the paint reads the shaded column's own values), a
#'   column name (paint every shaded column by THAT column's row values), or
#'   a `{col}` template such as `"{col} (sev)"` (pair each shaded column
#'   with its own companion, re-resolved per render). The cell keeps
#'   displaying its own value while the background encodes the source's --
#'   the AE heatmap's "count shown, worst grade painted". Source columns are
#'   hidden from the display; sources must be numeric. Diverging/sequential
#'   only.
#' @return A list consumed by [drilldown_table()].
#' @examples
#' drilldown_table_color("sequential", domain = c(0, 100))
#' @export
drilldown_table_color <- function(type = c("diverging", "sequential", "bar"),
                                   domain = NULL, palette = NULL,
                                   columns = NULL, source = NULL) {
  type <- match.arg(type)
  list(type = type, domain = domain, palette = palette, columns = columns,
       source = source)
}

# --- internal helpers --------------------------------------------------

#' @noRd
dt_th <- function(name, idx, stub = FALSE, label = NULL, numeric = FALSE,
                  sortable = TRUE, extra = NULL) {
  # `blockr-sortable` is the hook wireSort() binds to; drop it (and the sort
  # arrow) when sorting is turned off so the column is inert.
  base <- if (stub) "blockr-stub-header" else "blockr-col-header leaf"
  cls  <- if (isTRUE(sortable)) paste(base, "blockr-sortable") else base
  # Align the whole header (name + sub-label + sort arrow) to the column's
  # data type, so a flat table reads like the structured one: numeric right,
  # text left. The explicit class wins over the inherited html-table delta.
  cls <- paste(cls, if (isTRUE(numeric)) "dt-col-num" else "dt-col-txt")
  name_span <- htmltools::tags$span(class = "blockr-col-name", name)
  sort_span <- if (isTRUE(sortable)) {
    htmltools::tags$span(class = "blockr-sort-icon")
  }
  if (!is.null(label)) {
    # Labelled column: name on top, then the subtext and the sort arrow
    # share the lower row (label left, arrow right) -- like the html
    # preview, so the arrow doesn't add a row of its own.
    htmltools::tags$th(
      class = cls,
      `data-col-index` = idx,
      name_span,
      htmltools::tags$div(
        class = "dt-th-subrow",
        htmltools::tags$span(class = "blockr-col-label", label),
        sort_span
      ),
      # `extra` is trailing header content the caller owns (the summarize
      # table's column axis). Inside the SAME cell rather than a second header
      # row, so it inherits the sticky behaviour and the column's padding, which
      # is what keeps a tick aligned with the mark below it.
      extra
    )
  } else {
    # Unlabelled column: name and sort arrow share the single row.
    htmltools::tags$th(
      class = cls,
      `data-col-index` = idx,
      htmltools::tags$div(
        class = "dt-th-namerow",
        name_span,
        sort_span
      ),
      extra
    )
  }
}

#' Variable label for a column header, consistent with how the
#' drilldown chart / blockr.dplyr surface `attr(x, "label")`. Returns
#' `NULL` when there is no label or it just repeats the column name.
#' @noRd
dt_col_label <- function(x, name) {
  lbl <- attr(x, "label")
  if (is.null(lbl) || !is.character(lbl) || length(lbl) != 1L) return(NULL)
  lbl <- trimws(lbl)
  if (!nzchar(lbl) || identical(lbl, name)) return(NULL)
  lbl
}

#' Differentiated message for the flat table's non-renderable states, or NULL
#' when the table can render. Wording mirrors the chart's empty states
#' (chart/model-common.js): a configured column no longer in the data names the
#' column and hints at an upstream rename + "re-pick it in the gear"; a
#' required mapping with nothing configured is a pick prompt; a 0-row frame is
#' just "no rows".
#' `value_cols_raw` is the caller's pre-default `value_cols` (NULL = "all but
#' the rowname"), so an EXPLICIT pick whose columns all vanished reads as a
#' config error, never as empty data. A partially-missing explicit pick keeps
#' rendering the surviving columns (the documented silent-skip rule).
#' @noRd
dt_state_message <- function(data, label_col, value_cols, value_cols_raw) {
  missing <- character()
  if (!label_col %in% names(data)) {
    missing <- paste0("Rowname = \"", label_col, "\"")
  }
  raw <- as.character(value_cols_raw %||% character())
  if (length(raw) && !length(value_cols)) {
    missing <- c(missing,
                 paste0("Value = \"", setdiff(raw, names(data)), "\""))
  }
  if (length(missing)) {
    return(paste0(
      "Mapped column not in data: ", paste(missing, collapse = ", "),
      ". A rename, flatten or pivot upstream may have changed the column ",
      "name \u2014 re-pick it in the gear."
    ))
  }
  if (nrow(data) == 0L) return("No rows to display")
  if (!length(value_cols)) return("Pick a Value column in the gear")
  NULL
}

#' @noRd
dt_message_table <- function(msg = "No data") {
  htmltools::tags$table(
    class = "blockr-table",
    htmltools::tags$tbody(
      htmltools::tags$tr(htmltools::tags$td(class = "blockr-data", msg))
    )
  )
}

#' Aggregate the table's input FOR DISPLAY.
#'
#' When `group` names one or more (existing) columns, collapse the frame to one
#' row per group with a column per metric. `summaries` is a list, each element
#' `list(func, cols)`, so one table can carry several summaries at once (e.g.
#' mean(AGE) and sum(DOSE)). Same aggregation vocabulary the chart uses (shared
#' `AGG_FNS`). This is a display projection only; the block's data output stays
#' the raw input filtered by the click (see the block server's `expr`). No group
#' -> the frame is returned unchanged, so the renderer draws the raw table.
#'
#' `na_group` decides what happens to rows whose group key is missing.
#' `"level"` (the default, and the behaviour this has always had) gives them
#' their own cell -- R keys it `NA`, the JS twin keys it `""`, same rows and
#' same numbers. `"drop"` removes them: they are not a category. Missing means
#' `NA` *or* the empty string, matching the JS fold `String(row[g] ?? "")`, so
#' both engines drop the same rows and the golden cross-test still holds.
#'
#' @return `list(data, group, metric_cols)` -- the (aggregated or raw) frame,
#'   the resolved group columns, and the metric column names.
#' @noRd
dd_table_aggregate <- function(data, group, summaries,
                               na_group = c("level", "drop")) {
  na_group <- match.arg(na_group)
  group <- intersect(as.character(group), names(data))
  if (identical(na_group, "drop") && length(group)) {
    # Any missing key drops the row: with several group columns the cell has no
    # complete address, so there is no honest place to put it.
    miss <- lapply(data[group], function(x) {
      x <- as.character(x)
      is.na(x) | !nzchar(x)
    })
    data <- data[!Reduce(`|`, miss), , drop = FALSE]
  }
  plan  <- dd_metric_plan(summaries, data)
  # Aggregation is "active" when the block carries a group and/or a metric.
  # Neither -> the raw frame passes through unchanged (`aggregated = FALSE`).
  if (!length(group) && !length(plan)) {
    return(list(data = data, group = character(), metric_cols = character(),
                aggregated = FALSE))
  }
  # A grouped request with no usable metric still counts; a metric with no group
  # reduces the WHOLE frame to a single totals row (grand totals).
  if (!length(plan)) {
    plan <- list(list(name = "Count", expr = quote(dplyr::n()), label = NULL))
  }
  exprs <- stats::setNames(lapply(plan, `[[`, "expr"),
                           vapply(plan, `[[`, "", "name"))
  out <- if (length(group)) {
    g <- dplyr::group_by(data, dplyr::across(dplyr::all_of(group)))
    as.data.frame(dplyr::summarise(g, !!!exprs, .groups = "drop"),
                  check.names = FALSE)
  } else {
    # Grand totals: an ungrouped summarise -> exactly one row of metric values.
    as.data.frame(dplyr::summarise(data, !!!exprs), check.names = FALSE)
  }
  # Carry a friendly, stat-prefixed label onto each metric column -- the source
  # column's own label is lost in summarise -- so the header reads e.g. bold
  # "BMIBL" over "Mean: Baseline BMI (kg/m^2)" (dt_col_label surfaces it).
  for (p in plan) {
    if (!is.null(p$label) && p$name %in% names(out)) {
      attr(out[[p$name]], "label") <- p$label
    }
  }
  list(data = out, group = group,
       metric_cols = vapply(plan, `[[`, "", "name"), aggregated = TRUE)
}

#' min/max with an empty-group guard.
#'
#' `min(x, na.rm = TRUE)` on an all-NA (or empty) group warns and returns
#' `Inf` (`-Inf` for max) -- neither a real extremum nor the `NA` the rest of
#' the vocabulary yields (mean -> NaN, median -> NA). An empty group has no
#' extremum: return NA (of `x`'s type), matching the JS engine's null and
#' keeping a chart and its table twin on the same number. `na.rm` is accepted
#' (dd_metric_plan composes every numeric aggregation as `fn(x, na.rm = TRUE)`)
#' but missing values are always removed.
#' @noRd
dd_agg_min <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) x[NA_integer_] else min(x, na.rm = TRUE)
}

#' @noRd
dd_agg_max <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) x[NA_integer_] else max(x, na.rm = TRUE)
}

#' Plan the metric columns for a summaries list.
#'
#' For each metric produces `list(name, expr, label)`: `name` is the bold header
#' (the source variable when free, else stat-qualified so it stays unique),
#' `expr` the summarise call, and `label` the grey subtext -- the source column's
#' own label prefixed with the stat, e.g. "Median: Baseline BMI (kg/m^2)". count
#' -> "Count" (no label); count_distinct / numeric aggregations -> one entry per
#' (numeric) column. Non-numeric or missing columns fall away.
#' @noRd
dd_metric_plan <- function(summaries, data) {
  # Words come from the shared AGG_WORDS (R/block-arguments.R), the R twin of
  # the JS AGG_WORDS -- one home per side, tied by the drift test.
  fn_calls <- list(mean = quote(mean), median = quote(stats::median),
                   sum = quote(sum), min = quote(dd_agg_min),
                   max = quote(dd_agg_max))
  orig_label <- function(col) {
    l <- attr(data[[col]], "label", exact = TRUE)
    if (is.character(l) && length(l) == 1L && nzchar(l)) l else col
  }
  # `uniq()` and `add()` accumulate across the whole loop below, so their state
  # is held in an environment and mutated with a plain `<-`: an env is the one
  # R value with reference semantics, which is what the accumulation needs.
  # Do not fold this back into locals + `<<-` -- that is the same write with a
  # less explicit target.
  acc <- new.env(parent = emptyenv())
  acc$used <- character()
  acc$plan <- list()
  uniq <- function(nm, tag) {
    if (!(nm %in% acc$used)) {
      acc$used <- c(acc$used, nm)
      return(nm)
    }
    # Same variable aggregated twice -> qualify the second so the name is unique.
    nm2 <- paste0(nm, " (", tag, ")")
    while (nm2 %in% acc$used) nm2 <- paste0(nm2, " ")
    acc$used <- c(acc$used, nm2)
    nm2
  }
  add <- function(name, expr, label) {
    acc$plan[[length(acc$plan) + 1L]] <-
      list(name = name, expr = expr, label = label)
  }
  # Default-function rule (override semantics): a NUMERIC aggregation whose
  # entry names no columns applies to ALL numeric columns EXCEPT those
  # explicitly claimed by other entries -- dplyr's
  # `summarise(across(where(is.numeric), fn), DOSE = sum(DOSE))` with the
  # explicit entry winning its column. A rule, not a snapshot: it re-resolves
  # against the current frame, so upstream schema changes self-heal (same
  # convention as the empty colour scope = "all numeric"). count needs no
  # columns; count_distinct stays explicit-only (a distinct count over every
  # numeric column is rarely meant).
  claimed <- unique(unlist(lapply(
    summaries %||% list(),
    function(m) as.character(m$cols %||% character())
  )))
  all_num <- names(data)[vapply(data, is.numeric, logical(1))]
  for (m in summaries %||% list()) {
    fn   <- as.character(m$func %||% "count")[1L]
    raw  <- as.character(m$cols %||% character())
    cols <- intersect(raw, names(data))
    # Expand only when NOTHING was picked -- picked-but-dropped-upstream
    # columns must stay a visible no-op, not silently widen to all numerics.
    if (!length(raw) && fn %in% names(fn_calls)) {
      cols <- setdiff(all_num, claimed)
    }
    if (identical(fn, "count")) {
      add(uniq(AGG_WORDS[["count"]], "count"), quote(dplyr::n()),
          "Number of Observations")
    } else if (identical(fn, "count_distinct")) {
      for (col in cols) {
        add(uniq(col, "distinct"),
            # na.rm: NA is missingness, not a distinct value -- and the JS
            # chart aggregation excludes null/NaN, so the same distinct count
            # must read the same number on a chart and its table twin.
            bquote(dplyr::n_distinct(.data[[.(col)]], na.rm = TRUE),
                   list(col = col)),
            paste0(AGG_WORDS[["count_distinct"]], ": ", orig_label(col)))
      }
    } else if (!is.null(fn_calls[[fn]])) {
      fc <- fn_calls[[fn]]
      for (col in cols) {
        if (is.numeric(data[[col]])) {
          add(uniq(col, fn),
              bquote(.(fc)(.data[[.(col)]], na.rm = TRUE),
                     list(fc = fc, col = col)),
              paste0(AGG_WORDS[[fn]], ": ", orig_label(col)))
        }
      }
    }
  }
  acc$plan
}

#' Normalize the `summaries` list from a restored block state (an R list) or from
#' JS (a JSON string) to `list(list(func, cols), ...)`.
#' @noRd
dd_parse_summaries <- function(v) {
  if (is.null(v)) return(list())
  ms <- if (is.character(v) && length(v) == 1L) {
    tryCatch(jsonlite::fromJSON(v, simplifyVector = FALSE),
             error = function(e) list())
  } else if (is.list(v)) {
    v
  } else {
    list()
  }
  lapply(ms, function(m) {
    list(
      func = as.character(m$func %||% "count")[1L],
      cols = as.character(unlist(m$cols %||% character()))
    )
  })
}

#' Serialize the `summaries` list to the JSON the gear reads off `data-dt-summaries`
#' (`func` a scalar, `cols` always an array). JSON lives only at this edge.
#' @noRd
dd_summaries_json <- function(summaries) {
  if (!length(summaries)) return("[]")
  as.character(jsonlite::toJSON(
    lapply(summaries, function(m) {
      list(
        func = jsonlite::unbox(as.character(m$func %||% "count")[1L]),
        cols = as.character(m$cols %||% character())
      )
    }),
    auto_unbox = FALSE
  ))
}

#' Normalize the `shadings` list (cell value-encoding rules) from a restored
#' block state (an R list) or from the gear (a JSON string) to
#' `list(list(mode, cols), ...)`. Mirrors dd_parse_summaries -- the two lists
#' share their shape family (`{func, cols}` / `{mode, cols}`).
#' @noRd
dd_parse_shadings <- function(v) {
  if (is.null(v)) return(list())
  ss <- if (is.character(v) && length(v) == 1L) {
    tryCatch(jsonlite::fromJSON(v, simplifyVector = FALSE),
             error = function(e) list())
  } else if (is.list(v)) {
    # A single drilldown_table_color()-style spec (has $type) reads as one rule.
    if (!is.null(v$type)) list(v) else v
  } else {
    list()
  }
  lapply(ss, function(s) {
    out <- list(
      mode = as.character(s$mode %||% s$type %||% "diverging")[1L],
      cols = as.character(unlist(s$cols %||% s$columns %||% character()))
    )
    # Optional power knobs (per-rule palette / fixed domain / color source)
    # ride along when present -- the gear never writes them, ctor / legacy
    # specs may. `source` decouples the painted value from the displayed one
    # (see dd_shading_visuals).
    if (!is.null(s$palette)) out$palette <- s$palette
    if (!is.null(s$domain)) {
      # A JSON restore delivers c(lo, hi) as a two-element list; the visuals
      # resolver does arithmetic on it, so normalize to numeric here.
      out$domain <- suppressWarnings(as.numeric(unlist(s$domain)))
    }
    if (!is.null(s$source) && nzchar(as.character(s$source)[1L])) {
      out$source <- as.character(s$source)[1L]
    }
    out
  })
}

#' Serialize the `shadings` list to the JSON the gear reads off
#' `data-dt-shadings` (`mode` a scalar, `cols` always an array).
#' @noRd
dd_shadings_json <- function(shadings) {
  if (!length(shadings)) return("[]")
  as.character(jsonlite::toJSON(
    lapply(shadings, function(s) {
      out <- list(
        mode = jsonlite::unbox(as.character(s$mode %||% "diverging")[1L]),
        cols = as.character(s$cols %||% character())
      )
      # `source` must ROUND-TRIP through the gear even though the gear never
      # edits it: JS echoes the whole list back on any rule edit, so a field
      # left out here would be silently dropped by the first unrelated
      # mode/cols change.
      if (!is.null(s$source)) {
        out$source <- jsonlite::unbox(as.character(s$source)[1L])
      }
      out
    }),
    auto_unbox = FALSE
  ))
}

#' Column metadata for the gear popover, stamped on the `<table>` as
#' `data-dt-cols` (JSON `[{name, type}]`; `"[]"` for a structured frame, whose
#' pre-formatted cells offer nothing to pick). Always the block's raw INPUT
#' columns -- NOT the displayed projection -- so the gear's column pickers stay
#' correct while the table shows an aggregated frame, and re-reading the
#' attribute at popover-open time (table.js) picks up upstream schema changes
#' and server-side config edits without a chrome rebuild.
#' @noRd
dt_gear_cols_json <- function(data) {
  data <- tryCatch(fmt_to_wide(data), error = function(e) data)
  if (!is.data.frame(data) || dt_is_structured(data)) return("[]")
  cols <- lapply(names(data), function(nm) {
    list(
      name = nm,
      type = if (is.numeric(data[[nm]])) "numeric" else "categorical"
    )
  })
  as.character(jsonlite::toJSON(cols, auto_unbox = TRUE))
}

#' Resolve the `shadings` rules to per-column cell visuals.
#'
#' Override semantics, mirroring dd_metric_plan: a rule with EMPTY `cols`
#' covers ALL numeric columns except those explicitly claimed by other rules
#' (so `bar on DOSE` + `diverging on []` bars DOSE and shades everything
#' else); explicit-but-dropped columns stay a visible no-op; among competing
#' rules the FIRST wins a column. diverging/sequential pool ONE domain across
#' the rule's resolved columns (a correlation matrix reads on one scale);
#' `bar` normalizes per column on absolute magnitude.
#'
#' **Color source** (diverging/sequential only, ctor-level): a rule may name
#' a `source` -- the column whose values drive the paint while the cell keeps
#' displaying its own. Two forms: a plain column name paints ALL of the
#' rule's columns by that one column's row values (the BI "shade A by B"),
#' and a `{col}` template (e.g. `"{col} (sev)"`) pairs each shaded column
#' with its own companion, re-resolved per render so it follows a dynamic
#' matrix (the AE heatmap: counts displayed, worst grade painted). Columns
#' referenced as a source are paint metadata, not data for the eye: they are
#' excluded from shading candidates here and reported in the `"hidden"`
#' attribute so the caller drops them from the display. The domain pools
#' over the SOURCE values (that is what is being encoded); sources must be
#' numeric.
#'
#' @return named list: column -> `list(kind = "bar", fill, max)` or
#'   `list(kind = "bg", fun)` (a dt_color_fun closure, plus `src` when a
#'   source drives the paint). Unlisted columns render plain. Attribute
#'   `"hidden"`: source columns the display must drop.
#' @noRd
dd_shading_visuals <- function(shadings, data, value_cols) {
  out <- list()
  if (!length(shadings)) return(out)
  num_cols <- value_cols[vapply(data[value_cols], is.numeric, logical(1L))]
  # Pass 1 -- expand every rule's `source` against the columns it could
  # cover, so sources are known BEFORE candidates resolve (a companion
  # column must never itself end up shaded, or displayed).
  expand_src <- function(src, cols) {
    if (grepl("{col}", src, fixed = TRUE)) {
      vapply(cols, function(cn) gsub("{col}", cn, src, fixed = TRUE),
             character(1L))
    } else {
      rep(src, length(cols))
    }
  }
  hidden <- unique(unlist(lapply(shadings, function(s) {
    src <- as.character(s$source %||% "")[1L]
    if (!nzchar(src)) return(character())
    intersect(unique(expand_src(src, num_cols)), names(data))
  })))
  num_cols <- setdiff(num_cols, hidden)
  claimed <- unique(unlist(lapply(
    shadings, function(s) as.character(s$cols %||% character())
  )))
  taken <- character()
  for (s in shadings) {
    mode <- as.character(s$mode %||% "diverging")[1L]
    raw  <- as.character(s$cols %||% character())
    cols <- if (length(raw)) intersect(raw, num_cols)
            else setdiff(num_cols, claimed)
    cols <- setdiff(cols, taken)
    if (!length(cols)) next
    taken <- c(taken, cols)
    if (identical(mode, "bar")) {
      fill <- (s$palette %||% "rgba(37, 99, 235, 0.22)")[1L]
      for (cn in cols) {
        v <- data[[cn]][is.finite(data[[cn]])]
        out[[cn]] <- list(kind = "bar", fill = fill,
                          max = if (length(v)) max(abs(v)) else 0)
      }
    } else {
      # Paired source per column (NA = none resolvable -> that column's
      # paint reads its own values, so a missing companion degrades to the
      # sourceless behaviour instead of rendering plain).
      src <- as.character(s$source %||% "")[1L]
      srcs <- if (nzchar(src)) {
        sc <- expand_src(src, cols)
        ifelse(sc %in% names(data), sc, NA_character_)
      } else {
        rep(NA_character_, length(cols))
      }
      pool_cols <- ifelse(is.na(srcs), cols, srcs)
      vals <- suppressWarnings(as.numeric(unlist(
        data[unique(pool_cols)], use.names = FALSE
      )))
      vals <- vals[is.finite(vals)]
      dom <- s$domain
      if (is.null(dom) && length(vals)) {
        dom <- if (identical(mode, "diverging")) {
          m <- max(abs(vals))   # symmetric around 0 (white at 0)
          c(-m, m)
        } else {
          range(vals)
        }
      }
      if (!is.null(dom) && length(vals) && diff(range(dom)) > 0) {
        fun <- dt_color_fun(mode, dom, s$palette)
        for (k in seq_along(cols)) {
          out[[cols[k]]] <- c(list(kind = "bg", fun = fun),
                              if (!is.na(srcs[k])) list(src = srcs[k]))
        }
      }
    }
  }
  attr(out, "hidden") <- hidden
  out
}

#' Build the ANDed equality filter for a grouped-table row click.
#'
#' Returns a language object `.data[[c1]] == v1 & .data[[c2]] == v2 & ...` from
#' aligned column / value vectors (the clicked row's group keys), for splicing
#' into the block's `dplyr::filter()` expr so downstream gets that group's
#' member rows.
#' @noRd
dd_group_filter_call <- function(cols, vals) {
  conds <- Map(function(cl, vl) bquote(.data[[.(cl)]] == .(vl)),
               as.character(cols), as.character(vals))
  Reduce(function(a, b) bquote(.(a) & .(b)), conds)
}

#' A nested records table's drill as a filter on the input rows.
#'
#' table_nest() puts the k-th `by` column's value in `.group<k>_level`, so a
#' header click on `.group1_level = "Placebo"` filters `by[1] == "Placebo"`.
#' A row click adds its value in the drill column, which is a real column.
#' The "(Missing)" header (`missing = "level"`) claims the rows with no value.
#' @param by The `by` columns, outer first.
#' @param gc,gv The stored claim's columns and values.
#' @noRd
table_nest_drill_expr <- function(by, gc, gv) {
  by <- as.character(unlist(by))
  gc <- as.character(unlist(gc))
  gv <- as.character(unlist(gv))
  if (!length(gc) || length(gc) != length(gv)) {
    return(summarize_drill_expr(list()))
  }
  k <- suppressWarnings(
    as.integer(sub("^\\.group([0-9]+)_level$", "\\1", gc))
  )
  grp <- !is.na(k) & k <= length(by)
  # A row claims its groups and its value in the drill column, a real column.
  real <- is.na(k) & !startsWith(gc, ".")
  cols <- gc
  cols[grp] <- by[k[grp]]
  keep <- grp | real
  summarize_drill_expr(stats::setNames(as.list(gv[keep]), cols[keep]))
}

#' Say what a drill claim claims, in the source data's own words.
#'
#' Structured drill keys are identity columns, so the source terms are in the
#' VALUES: `.variable` holds the variable NAME and `.variable_level` its level,
#' which is what makes "AEDECOD = ABDOMINAL PAIN" out of a pair of dotted
#' columns. Enclosing `.group<k>_level` values come first as a breadcrumb. A
#' header click has group levels and no variable pair; an aggregated table
#' drills on real column names and falls through to `col = value`.
#'
#' Shared by the latched footer ("Filtered: ...") and the transient receipt
#' ("Drilled down to ..."), so one click cannot be described two ways.
#' @param gc,gv Aligned claim columns and values.
#' @param group_names The columns the `.group<k>` levels stand for, outer
#'   first (dt_group_names()). With them a header click reads "ARM = Placebo"
#'   rather than "Placebo".
#' @return A character vector of clause labels, outermost first.
#' @noRd
dt_claim_label <- function(gc, gv, group_names = character()) {

  gc <- as.character(unlist(gc))
  gv <- as.character(unlist(gv))

  if (!length(gc) || length(gc) != length(gv)) {
    return(character())
  }

  glv <- grepl("^\\.group[0-9]+_level$", gc)
  vi <- match(".variable", gc)
  li <- match(".variable_level", gc)

  if (!is.na(vi) && !is.na(li)) {
    c(gv[glv], paste0(gv[vi], " = ", gv[li]))
  } else if (any(glv)) {
    k <- as.integer(sub("^\\.group([0-9]+)_level$", "\\1", gc[glv]))
    nm <- group_names[k]
    real <- !glv & !startsWith(gc, ".")
    c(ifelse(!is.na(nm) & nzchar(nm), paste0(nm, " = ", gv[glv]), gv[glv]),
      if (any(real)) paste0(gc[real], " = ", gv[real]))
  } else {
    paste0(gc, " = ", gv)
  }
}

#' The column each `.group<k>` level of an annotated frame stands for, read
#' off the `.group<k>` column (table_nest() and summary_table() write the
#' column name there). Empty where the frame has none.
#' @noRd
dt_group_names <- function(data) {
  if (!is.data.frame(data) || !nrow(data)) return(character())
  out <- character()
  k <- 1L
  while (paste0(".group", k) %in% names(data)) {
    v <- as.character(data[[paste0(".group", k)]])
    v <- v[!is.na(v) & nzchar(v)]
    out[[k]] <- if (length(v)) v[[1L]] else ""
    k <- k + 1L
  }
  out
}

#' The column half of a structured drill, as stored: a list of
#' `list(column = , values = )`, from the click or from a saved board (where
#' `values` may come back as a list). NULL when there is none.
#' @noRd
dd_col_keys_state <- function(x) {
  if (!is.list(x) || !length(x)) return(NULL)
  out <- lapply(x, function(k) {
    if (!is.list(k)) return(NULL)
    cl <- as.character(unlist(k[["column"]]))
    vl <- as.character(unlist(k[["values"]]))
    if (length(cl) != 1L || !nzchar(cl) || !length(vl)) return(NULL)
    list(column = cl, values = vl)
  })
  out <- Filter(Negate(is.null), out)
  if (length(out)) out
}

#' "ARM = Low dose" for each column claim.
#' @noRd
dd_col_keys_label <- function(ck) {
  ck <- dd_col_keys_state(ck)
  vapply(ck %||% list(), function(k) {
    paste0(k$column, " = ", paste(k$values, collapse = ", "))
  }, character(1L))
}

#' AND a column claim onto an expression that yields source rows:
#' `dplyr::filter(<ex>, ARM %in% "Low dose")`. "(Missing)" claims the rows
#' with no value (summarize_drill_expr()).
#' @noRd
dd_col_keys_filter <- function(ex, ck) {
  ck <- dd_col_keys_state(ck)
  if (!length(ck)) return(ex)
  f <- stats::setNames(lapply(ck, `[[`, "values"),
                       vapply(ck, `[[`, "", "column"))
  out <- summarize_drill_expr(f)
  out[[2L]] <- ex
  out
}

#' Serialize the block's active drill-filter state for the `data-dt-active`
#' table attribute (JSON lives only at this edge, like `summaries`). `active`
#' is `list(col, vals, gcols, gvals)` -- the filter reactiveVals as captured
#' at render time; returns `NULL` when no filter is active so the attribute
#' is simply absent.
#' @noRd
dd_active_filter_json <- function(active) {
  if (is.null(active)) return(NULL)
  gcols <- as.character(unlist(active$gcols %||% character()))
  gvals <- as.character(unlist(active$gvals %||% character()))
  ck <- dd_col_keys_state(active$ckeys)
  # The column half rides along: table.js marks that column.
  col_filters <- if (length(ck)) {
    lapply(ck, function(k) list(column = jsonlite::unbox(k$column),
                                values = k$values))
  }
  if (length(gcols) && length(gcols) == length(gvals)) {
    return(as.character(jsonlite::toJSON(
      c(list(filters = unname(Map(
        function(cl, vl) list(column = jsonlite::unbox(cl),
                              value = jsonlite::unbox(vl)),
        gcols, gvals
      ))), if (length(col_filters)) list(col_filters = col_filters))
    )))
  }
  if (length(col_filters)) {
    return(as.character(jsonlite::toJSON(
      list(filters = list(), col_filters = col_filters)
    )))
  }
  col  <- active$col
  vals <- as.character(unlist(active$vals %||% character()))
  if (is.null(col) || !length(vals)) return(NULL)
  as.character(jsonlite::toJSON(
    # auto_unbox = FALSE keeps the `values` vector an array even at length 1.
    list(column = jsonlite::unbox(as.character(col)[1L]), values = vals),
    auto_unbox = FALSE
  ))
}

#' Drill onclick target for a row click: the column name + its 0-based index in
#' the rendered column order, or NULLs when `drill` is unset / not a column.
#' @noRd
dt_onclick <- function(drill, all_cols) {
  if (is.null(drill)) return(list(col = NULL, idx = NULL))
  m <- match(drill, all_cols)
  if (is.na(m)) return(list(col = NULL, idx = NULL))
  list(col = drill, idx = m - 1L)
}

#' Tag a `<table>` with the data-attributes the table JS reads off it. These
#' live on the table (not the container) so that re-rendering only the table on
#' a data/config change still carries current drill / colour / digits state to
#' the JS, while the chrome stays put.
#' @noRd
dt_table_attrs <- function(table_tag, onclick_col, onclick_idx,
                           digits, color = NULL, shadings = list(),
                           num_cols = NULL,
                           toggles = NULL, group_cols = NULL,
                           group = character(), summaries = list(),
                           active = NULL, gear_cols = NULL,
                           drill_mode = NULL) {
  on_off <- function(x) if (isTRUE(x)) "on" else "off"
  htmltools::tagAppendAttributes(
    table_tag,
    # The gear's pickable columns (raw input schema, dt_gear_cols_json) --
    # re-stamped on every table render so the popover re-reads CURRENT
    # columns when it opens.
    `data-dt-cols` = gear_cols,
    # Active drill filter at render time (JSON; NULL = none). table.js
    # matches rows by their data-raw values and re-applies the dt-row-active
    # highlight, so a restored board (or a config re-render) shows which row
    # drives the downstream filter.
    `data-dt-active` = dd_active_filter_json(active),
    # Current aggregation config, read back by the gear (group comma-joined,
    # summaries a JSON array of {func, cols} -- JSON lives only at this edge).
    `data-dt-group` = paste(group %||% character(), collapse = ","),
    `data-dt-summaries` = dd_summaries_json(summaries),
    # Grouped-table drill: the group columns whose per-row values a click ANDs
    # into a downstream filter (comma-joined; "" = not a grouped table).
    `data-dt-group-cols` = paste(group_cols %||% character(), collapse = ","),
    # The drill MODE for the picker-less variants (structured / aggregated),
    # where `drill` is not a column but a word: "auto" hands the clicked
    # display subset downstream, "source" the records behind it. Stamped so
    # the gear reads back what is actually set -- readGearState used to
    # report a flat "auto" for any structured drill, which would clobber the
    # mode on the next config write.
    `data-dt-drill-mode` = drill_mode,
    `data-dt-onclick-col` = onclick_col,
    `data-dt-onclick-idx` = if (!is.null(onclick_idx)) onclick_idx else NULL,
    # The number format and its unit (format / unit), read back by the gear.
    `data-dt-format` = as.character(digits),
    `data-dt-unit` = toggles$unit %||% "",
    # Categorical identity color ("Color by" -- row tint via the board scale
    # map; the chart's color aesthetic applied to rows) and the value-encoding
    # `shadings` rules ({mode, cols} JSON, only at this edge).
    `data-dt-color` = color %||% "",
    `data-dt-shadings` = dd_shadings_json(shadings),
    # Numeric columns the gear may offer for the shading scope pickers.
    `data-dt-num-cols` = paste(num_cols %||% character(), collapse = ","),
    # Display-option toggles the gear reads back (default ON, except export).
    `data-dt-sortable` = on_off(toggles$sortable %||% TRUE),
    `data-dt-collapsible` = on_off(toggles$collapsible %||% TRUE),
    `data-dt-search` = on_off(toggles$search %||% TRUE),
    `data-dt-download` = on_off(toggles$download %||% FALSE),
    `data-dt-wrap-titles` = on_off(toggles$wrap_titles %||% TRUE),
    # Rotated column titles (matrix tables): the attribute drives the CSS
    # (vertical value-column headers) exactly like wrap-titles above, and
    # the gear reads the current state off it.
    `data-dt-rotate-titles` = on_off(toggles$rotate_titles %||% FALSE)
  )
}

#' Is a download format's writer available? A seam (not an inline
#' requireNamespace) so tests can mock the package-missing state -- see the
#' dt_download renderUI, which shows the entry disabled with the reason on it
#' rather than hiding it, because a toggle the user just switched on that
#' renders NOTHING reads as broken.
#'
#' Each format needs one Suggests-level package: `openxlsx` writes the
#' spreadsheet, `flextable` (with `officer`) typesets the slide. HTML needs
#' none -- the renderer is the package's own.
#'
#' Asked with `system.file()` rather than `requireNamespace()`, because this
#' runs when the CONTROL renders, not when a download is clicked, and
#' requireNamespace() LOADS what it finds: turning the PowerPoint pill on would
#' otherwise pull officer + flextable into the process (measured: 0.49s and
#' 53MB, once per process) purely to decide whether to grey out a menu entry.
#' The writers keep their own requireNamespace() guard, so an installed but
#' unloadable package still fails at click time with a sentence that says so --
#' this probe answers "installed?", not "loadable?", which is the question the
#' menu is actually asking.
#' @noRd
dt_pkg_installed <- function(...) {
  all(vapply(c(...), function(p) nzchar(system.file(package = p)), logical(1L)))
}

#' @noRd
dt_has_openxlsx <- function() {
  dt_pkg_installed("openxlsx")
}

#' @noRd
dt_has_officer <- function() {
  dt_pkg_installed("officer", "flextable")
}

#' Table-block chrome: the scoped CSS, the search/gear header, and the scroll
#' container, with `inner` (a `<table>` tag or a `uiOutput()` slot) dropped into
#' the scroll wrapper. Rendered ONCE per block -- the gear/search/scroll never
#' rebuild -- so only `inner` (the table) re-renders on a filter or gear edit.
#'
#' `structured` may be `NULL`, meaning "not known yet": the chrome is then built
#' without the data (the block server renders it before its pipeline has
#' evaluated, so the control section paints immediately). The delta CSS still
#' ships -- it is scoped to `.drilldown-table-structured`, so it is inert until
#' the class lands -- and table.js promotes the class + `data-dt-structured` off
#' the `<table>` once the body renders. Callers that already hold the data pass
#' `TRUE`/`FALSE` and get the class server-side, as before.
#'
#' `scroll = "page"` makes the box scroll sideways only: the table runs its
#' full length and whatever scrolls around it (a dock panel, the page) scrolls
#' it up and down. table.js moves the header down with that scroller
#' (`followHeader()`), and `max_height` is ignored.
#' @noRd
dt_chrome <- function(elem_id, structured, max_height, inner,
                      search = TRUE, download_slot = NULL,
                      status_slot = NULL, gear = TRUE,
                      scroll = c("box", "page")) {

  scroll <- match.arg(scroll)
  # NULL = undetermined; only a known-flat frame suppresses the delta CSS.
  unknown <- is.null(structured)
  wrapper_id <- paste0(
    "blockr-dt-", sub("^file", "", basename(tempfile("")))
  )

  shared_css <- htmltools::tags$style(
    htmltools::HTML(html_table_shared_css_fallback())
  )

  header_div <- htmltools::tags$div(
    class = "blockr-html-table-header",
    htmltools::tags$div(
      class = "blockr-html-table-toolbar",
      # Search input, suppressed when the block turns `search` off (gear toggle).
      if (isTRUE(search)) {
        htmltools::tags$input(
          type = "search", class = "blockr-search",
          placeholder = "Search\u2026", `aria-label` = "Search table"
        )
      },
      # Optional download control (rendered only for the formats the block
      # turns on: a button for one, a menu for several); sits on the toolbar,
      # outside the gear.
      download_slot
    )
  )

  scroll_style <- if (identical(scroll, "page")) {
    NULL
  } else if (!is.null(max_height)) {
    paste0("max-height:", max_height, ";overflow:auto;")
  } else {
    "overflow:auto;"
  }

  htmltools::tagList(
    shared_css,
    # The delta CSS is the STRUCTURED Table-1 design treatment (section headers,
    # two-tier arm headers, the stub indent + medium stat/value weights). Inject
    # it only for structured tables -- a flat data table should match the canonical
    # html preview (normal-weight cells, plain stub), not carry the Table-1 styling.
    if (isTRUE(structured) || unknown) {
      # The Table-1 typography must land the instant the structured <table> is in
      # the DOM, NOT after table.js promotes `.drilldown-table-structured` onto
      # the container -- otherwise a structured table paints flat for one frame
      # and then restyles (a visible flash on every re-render, e.g. a tab
      # switch). Two scopes, both self-gating so a sibling flat table never
      # picks up the delta:
      #   1. `:has(... [data-dt-structured])` -- applies as soon as the marked
      #      <table> exists inside the container, with no JS in the loop. The
      #      structured <table> carries `data-dt-structured="1"` from the
      #      server, so this is flash-free on first paint and every re-render.
      #   2. `.drilldown-table-structured` -- the class table.js promotes; a
      #      fallback for browsers without :has() (styles after promotion).
      # Scope 1 hangs off `.drilldown-table-container`, the class only this
      # chrome's container carries. The groups and cards containers share
      # `.blockr-html-table-container`, and every DOM change inside one made
      # Chrome re-check the :has() and restyle the whole table under it:
      # 6600 elements per pass on the AE heatmap, several passes per edit.
      htmltools::tags$style(htmltools::HTML(paste0(
        html_table_delta_css(
          scope = paste0(
            ".drilldown-table-container",
            ":has(.blockr-table[data-dt-structured=\"1\"])"
          )
        ),
        html_table_delta_css(scope = ".drilldown-table-structured"),
        html_table_chev_css()
      )))
    },
    drilldown_table_dep(),
    htmltools::tags$div(
      id = wrapper_id,
      class = paste(
        "blockr-html-table-container drilldown-table-container",
        if (isTRUE(structured)) "drilldown-table-structured" else NULL
      ),
      `data-dt-elem-id` = if (!is.null(elem_id)) elem_id else NULL,
      # `gear = FALSE` suppresses the settings cogwheel (table.js reads this).
      # For a caller whose options are fixed and who has nowhere to store an
      # edit -- see the note at buildCogwheel().
      `data-dt-gear` = if (!isTRUE(gear)) "0" else NULL,
      `data-dt-structured` = if (isTRUE(structured)) "1" else NULL,
      `data-initial-expanded` = if (isTRUE(structured)) "1" else NULL,
      header_div,
      htmltools::tags$div(
        class = paste(
          c("blockr-table-wrapper",
            if (identical(scroll, "page")) "dt-scroll-page"),
          collapse = " "
        ),
        style = scroll_style,
        inner
      ),
      # Footer row: the row count on the left, the active-filter status line
      # (only while a filter is set) on the right. The count is written by table.js (it has to
      # follow the client-side search, which the server never sees) and hides
      # itself while empty; the status line is rendered server-side off the
      # block's filter state (see the block server's dt_status output) so it
      # also survives a board restore.
      htmltools::tags$div(
        class = "dt-table-footer",
        htmltools::tags$span(class = "dt-row-count", `aria-live` = "polite"),
        status_slot
      )
    )
  )
}

#' Per-cell inline style for a data bar: a left-anchored CSS gradient whose
#' width is `|v| / mx * 100%` (absolute-magnitude normalisation, no center
#' baseline -- matches the crossfilter block). Vectorised over the column so the
#' table stays on the single-`HTML()` fast path. Returns `""` for a degenerate
#' column (all-zero / non-finite max), i.e. no bar.
#' @noRd
dt_bar_style <- function(v, mx, fill) {
  if (!is.finite(mx) || mx <= 0) return(rep("", length(v)))
  # as.character on the rounded value formats each width independently (no
  # vectorized decimal-alignment, so 100 stays "100", not "100.0").
  pct <- as.character(round(pmax(0, pmin(100, abs(v) / mx * 100)), 2L))
  paste0(" style=\"background:linear-gradient(90deg,", fill, " 0 ", pct,
         "%,transparent ", pct, "%);\"")
}

#' Build the heatmap ramp for a shading rule. The returned function is
#' VECTORIZED over `v` end-to-end (clamp, interpolation t, per-channel lerp,
#' `rgb()`, and the luminance contrast test all take vectors) and returns
#' `list(bg =, fg =)` of hex vectors -- the caller styles a whole column in
#' one call, keeping the shaded table on the same vectorized fast path as
#' dt_bar_style(). A 10k x 10 shaded table used to burn 100k scalar closure
#' calls per render here.
#' Readable text colour over a background: white on dark, near-black on
#' light, by perceived luminance. ONE rule for every painted cell, so a
#' ramp cell and a scale-map cell (the heatmap block's two paint sources)
#' cannot disagree about when text flips to white. Vectorized.
#' @noRd
dt_fg_for_rgb <- function(r, g, b) {
  lum <- (0.299 * r + 0.587 * g + 0.114 * b) / 255
  ifelse(lum < 0.55, "#ffffff", "#111827")
}

#' @noRd
dt_fg_for_hex <- function(hex) {
  m <- grDevices::col2rgb(hex)
  dt_fg_for_rgb(m[1L, ], m[2L, ], m[3L, ])
}

#' @noRd
dt_color_fun <- function(type, domain, palette) {
  lo <- domain[1L]
  hi <- domain[2L]
  hex2rgb <- function(h) grDevices::col2rgb(h)[, 1L]
  # Per-channel r/g/b vectors -> bg hex + contrast-tested fg hex.
  bg_fg <- function(r, g, b) {
    list(bg = grDevices::rgb(r, g, b, maxColorValue = 255),
         fg = dt_fg_for_rgb(r, g, b))
  }

  if (identical(type, "diverging")) {
    pal <- palette %||% viz_palette("diverging", 3L, DT_DIVERGING_FALLBACK)
    c1 <- hex2rgb(pal[1L])
    c2 <- hex2rgb(pal[2L])
    c3 <- hex2rgb(pal[3L])
    # Diverging is centred on 0 (white-at-zero); the domain is symmetric around
    # 0 (set at inference). A meaningful zero is what "diverging" means -- for a
    # correlation matrix this puts white at r = 0 and gives +/- equal saturation.
    mid <- 0
    function(v) {
      v <- pmax(pmin(v, hi), lo)
      low <- v <= mid
      t_lo <- if (mid == lo) rep(0, length(v)) else (v - lo) / (mid - lo)
      t_hi <- if (hi == mid) rep(0, length(v)) else (v - mid) / (hi - mid)
      ch <- function(i) {
        round(ifelse(low, c1[i] + (c2[i] - c1[i]) * t_lo,
                          c2[i] + (c3[i] - c2[i]) * t_hi))
      }
      bg_fg(ch(1L), ch(2L), ch(3L))
    }
  } else {
    pal <- palette %||% viz_palette("sequential", 2L, DT_SEQUENTIAL_FALLBACK)
    c1 <- hex2rgb(pal[1L])
    c2 <- hex2rgb(pal[2L])
    function(v) {
      v <- pmax(pmin(v, hi), lo)
      t <- if (hi == lo) rep(0, length(v)) else (v - lo) / (hi - lo)
      ch <- function(i) round(c1[i] + (c2[i] - c1[i]) * t)
      bg_fg(ch(1L), ch(2L), ch(3L))
    }
  }
}

#' @noRd
drilldown_table_dep <- memoise0(function() {
  htmltools::tagList(
    blockr.ui::controls_dep(),
    # Shared popover CSS (the dd-* section/row/segmented/add classes) lives in
    # chart.css; the table's gear popover reuses it. ONE definition
    # (chart-dep.R) so the cache-busting suffix cannot drift from the chart's.
    chart_css_dep(),
    # Shared aggregation vocabulary + gear engine (one dep, one version -- see
    # drilldown_shared_dep()). Before the table JS, which reads both globals.
    drilldown_shared_dep(),
    htmltools::htmlDependency(
      name = "blockr-viz-table",
      # Suffix bumped when the bundled table JS/CSS changes, to bust the
      # version-pinned asset cache (display-option gear toggles).
      version = paste0(utils::packageVersion("blockr.viz"), ".36"),
      src = system.file(package = "blockr.viz"),
      script = "js/table.js",
      stylesheet = "css/table.css"
    )
  )
})

# --- block -------------------------------------------------------------

#' Build arguments metadata for the drill-down table block
#'
#' Registry/LLM metadata so the assistant and MCP can introspect the table
#' block (the chart block has this; the table was previously invisible).
#' @noRd
table_records_arguments <- function() {
  new_arg_specs(
    # Aggregation: group by column(s), then one or more summaries. Same vocabulary
    # as the chart, but the table carries a LIST of summaries (mean of AGE AND sum
    # of DOSE at once), so it takes `summaries` rather than the chart's single
    # `metric` + `func`.
    group = new_arg_spec(
      paste0(
        "Grouping column(s) to aggregate over. One or more categorical columns ",
        "(nested from outer to inner). Empty = no grouping (a raw row-level ",
        "table)."
      ),
      example = list("SEX", "ARM"),
      type = arg_array(arg_string())
    ),
    summaries = new_arg_spec(
      paste0(
        "The aggregations shown: a list, each entry `{func, cols}`. ",
        "`func` is one of \"count\", \"count_distinct\", \"mean\", ",
        "\"median\", \"sum\", \"min\", \"max\"; `cols` is the numeric ",
        "column(s) it reduces. One entry per aggregation, so mean of AGE and ",
        "sum of DOSE is two entries. Empty `cols` on a NUMERIC aggregation = ",
        "ALL numeric columns except those claimed by another entry (so ",
        "`{mean, []}` + `{sum, [DOSE]}` means DOSE as a sum, everything else ",
        "as a mean); empty for \"count\" (needs no column). With `group` ",
        "empty the summaries reduce the whole frame to ONE grand-total row; ",
        "with `group` set, one row per group level. `summaries` empty = a ",
        "plain row count per group."
      ),
      example = list(
        list(func = "mean", cols = list("AGE")),
        list(func = "sum", cols = list("DOSE"))
      )
    ),
    rowname = new_arg_spec(
      paste0(
        "The single column shown as the row labels (the left-hand stub). Names ",
        "a data column; defaults to the first column."
      ),
      example = "Region",
      type = arg_string()
    ),
    # Populated (explicit value columns array) so the model anchors on setting
    # these to named columns, not leaving them null.
    value = new_arg_spec(
      paste0(
        "The columns rendered as the table body (the data cells). When the user ",
        "names specific value/measure columns, set this to EXACTLY those ",
        "columns -- do not leave it null. Leave null only to mean 'all columns ",
        "except `rowname`'."
      ),
      example = list("Revenue", "Profit"),
      type = arg_array(arg_string())
    ),
    # A `drilldown_table_color()` SPEC object (not a column name); its shape is
    # an author-only nested structure, so the type is left unset and the NULL
    # example (a plain table) is dropped.
    color = new_arg_spec(
      paste0(
        "Categorical identity color (\"Color by\") -- the SAME argument as the ",
        "chart's `color`, applied to rows: names one categorical column whose ",
        "values tint the rows through the board scale map, so a SEX-colored ",
        "table matches the SEX-colored chart. Empty string = no tint; omit ",
        "(null) = a subtle default tint by the rowname column. Presentational ",
        "only; never changes data."
      ),
      example = "SEX",
      type = arg_string()
    ),
    shadings = new_arg_spec(
      paste0(
        "Cell value-encoding rules: a list, each entry `{mode, cols}` (same ",
        "shape family as `summaries`). `mode` is \"diverging\" (correlation ",
        "matrices, centred on 0), \"sequential\" (heatmaps), or \"bar\" (an ",
        "in-cell data bar proportional to the value). Empty `cols` on a rule ",
        "= ALL numeric columns except those claimed by another rule (so ",
        "`{bar, [DOSE]}` + `{diverging, []}` bars DOSE and shades everything ",
        "else). Empty list = plain cells. A diverging/sequential rule may ",
        "add `source`: a column name paints the rule's columns by THAT ",
        "column's row values, a `{col}` template (e.g. \"{col} (sev)\") ",
        "pairs each column with its own companion -- the cell displays its ",
        "own value, the paint encodes the source's (AE heatmap: counts ",
        "shown, worst grade painted). Source columns are hidden from ",
        "display. Presentational only; never changes data."
      ),
      example = list(
        list(mode = "diverging", cols = list())
      )
    ),
    drill = new_arg_spec(
      paste0(
        "Row-click drill-down. Optional; default null = a click is inert ",
        "(drill is opt-in everywhere). RAW table: a column name \u2014 clicking a ",
        "row emits a categorical filter on that column's value (the same ",
        "filter contract as the chart / tile). GROUPED / aggregated table: ",
        "\"auto\" \u2014 clicking a row ANDs its group-key values into the ",
        "downstream filter (the keys are the target; no column choice). ",
        "STRUCTURED (annotated summary) table: \"auto\" \u2014 clicking a row ",
        "filters the annotated frame to that row's identity and names the ",
        "selection under its source column (e.g. an AEDECOD column appears ",
        "on the output), so a downstream dm filter-by-data block can cascade ",
        "the click to the source data. On a structured or aggregated table ",
        "the value is a MODE, not a column: \"auto\" hands the clicked ",
        "subset of the DISPLAYED table downstream, \"source\" hands the ",
        "RECORDS behind the click instead (the 143 female subjects, not the ",
        "\"Sex: F\" row) -- resolved from the claim against the frame the ",
        "producer stamped as the input's `source_data` attribute, and an ",
        "error at eval time if there is none. Both render identically."
      ),
      example = "Region",
      type = arg_string()
    ),
    digits = new_arg_spec(
      "Decimal places for numeric display. Default 2.",
      example = 2L,
      type = arg_integer()
    ),
    ctrl_target = new_arg_spec(
      paste0(
        "BETA. Block id of a value filter block on the SAME board: the ",
        "drill's claim (e.g. SEX = F) is also pushed into that block over ",
        "the board's control channel, so the drill filters a pipeline the ",
        "table has no data link to. Requires drill to be on and the board ",
        "to carry the control bridge extension. Empty (default) = off; the ",
        "drill then behaves exactly as documented above. \"auto\" needs no ",
        "id: it is the board's drill destination, found by class -- the one ",
        "drill filter block, else the one value filter block. Prefer it ",
        "over a typed id."
      ),
      example = "cohort_filter",
      type = arg_string()
    ),
    ctrl_table = new_arg_spec(
      paste0(
        "BETA. Only with `ctrl_target`: the table in the target's dm the ",
        "pushed conditions apply to (e.g. \"adsl\"). Leave empty when the ",
        "target filters a plain data frame."
      ),
      example = "adsl",
      type = arg_string()
    ),
    title = new_arg_spec(
      paste0(
        "Table title shown above the table. Unset = auto (inherits the ",
        "input data frame's label attribute when present); \"\" = ",
        "explicitly no title. Supports {...} tokens resolved against the ",
        "CURRENT data on every render, so the title follows upstream ",
        "filters and pickers: {col} = the distinct values of that column ",
        "collapsed with \", \" (with a value filter upstream, {ARM} reads ",
        "the selected arm), {label(col)} = the column's variable label, ",
        "{n} = row count, {n_distinct(col)} = distinct count. Tokens are ",
        "data lookups, never code."
      ),
      example = "Adverse events by {ARM}",
      type = arg_string()
    ),
    subtitle = new_arg_spec(
      paste0(
        "Subtitle under the title, same {...} tokens as `title`. Unset = ",
        "auto (inherits the input data's subtitle attribute when present, ",
        "e.g. from a composer table); \"\" = explicitly none."
      ),
      example = "Treatment: {ARM}",
      type = arg_string()
    ),
    caption = new_arg_spec(
      paste0(
        "Caption under the table (source / footnote line), same {...} ",
        "tokens as `title`. Unset = auto (inherits the input data's ",
        "caption attribute when present); \"\" = explicitly none."
      ),
      example = "N = {n_distinct(USUBJID)} subjects",
      type = arg_string()
    )
  )
}

#' Argument specs for the table block
#'
#' The records layout's specs from the former table block, the groups
#' layout's from the former summarize table, under the new names
#' (dev/table-block-rewrite.md), plus `layout`.
#' @noRd
table_arguments <- function() {
  pick <- function(specs, map) {
    out <- unclass(specs)[names(map)]
    names(out) <- unname(map)
    out
  }
  rec <- pick(table_records_arguments(), c(
    rowname = "rowname", value = "columns", color = "color",
    shadings = "shadings", drill = "drill",
    ctrl_target = "drill_target", ctrl_table = "drill_table",
    title = "title", subtitle = "subtitle", caption = "caption"
  ))
  grp <- pick(summarize_arguments(), c(
    facet_layout = "facet_layout", sort_by = "sort_by",
    sort_dir = "sort_dir", top_n = "top_n", axis = "axis",
    value_labels = "value_labels", bar_width = "bar_width"
  ))
  rec$columns$description <- paste0(
    "Records layout: the columns to show, in order. Empty = all columns."
  )
  var <- pick(summary_table_arguments(), c(
    stats = "stats", sections = "sections", add_overall = "total",
    overall_label = "total_label", nest_hierarchies = "nest"
  ))
  for (nm in names(var)) {
    var[[nm]]$description <- paste0("Variables layout: ",
                                    var[[nm]]$description)
  }
  do.call(new_arg_specs, c(
    list(
      layout = new_arg_spec(
        paste0(
          "What one row is. \"records\": the input rows as they are ",
          "(listings, model output, correlation matrices, finished tables ",
          "from R code; with `by`, nested under one header per level). ",
          "\"groups\": one row per level of `by`, one column per entry of ",
          "`summaries` (counts, means, distributions as bars, boxes, ",
          "sparklines, heatmap tiles). \"variables\": a Table 1 -- one row ",
          "per statistic or level of each variable in `summaries`, one ",
          "column per level of `by`."
        ),
        example = "groups",
        type = arg_enum(c("records", "groups", "variables"))
      ),
      look = new_arg_spec(
        paste0(
          "How the table is drawn. \"table\" (default); \"compact\": the ",
          "matrix look, rotated column titles and square tile cells (a ",
          "heatmap); \"cards\": one card per summary, one row of cards per ",
          "row, each card the summary's label, its number in large type and ",
          "its glyph (bar, box, point range, dumbbell, sparkline). Cards ",
          "draw the groups layout (one row of cards per level of `by`, one ",
          "row without `by`) and the records layout (one card per column, ",
          "for a precomputed frame such as one with a change column). ",
          "Comparisons and targets are computed upstream; cards add no ",
          "summary keys. `spans` and `expr` summaries are not drawn as cards."
        ),
        example = "cards",
        type = arg_enum(TABLE_LOOKS)
      ),
      by = new_arg_spec(
        paste0(
          "Grouping columns, outer to inner (at most two). Groups layout: ",
          "one row per level, the outer column an expandable parent; empty ",
          "gives one row over all input rows. ",
          "Variables layout: one column per level. Records layout: rows ",
          "nested under one header per level."
        ),
        example = list("TRT01A"),
        type = arg_array(arg_string())
      ),
      summaries = table_summaries_spec(),
      id = new_arg_spec(
        paste0(
          "Variables layout: a column whose distinct values are counted ",
          "instead of rows (a subject id), so a count reads as subjects ",
          "with at least one event, and percentages are of subjects. In ",
          "the groups layout a column counts subjects itself: a simple ",
          "summary with func = \"count_distinct\" and col = the id."
        ),
        example = "USUBJID",
        type = arg_string()
      ),
      pct_of = new_arg_spec(
        paste0(
          "The base of a count's percentage: \"facet\" (default) the rows ",
          "or subjects of the column's own facet level, so arms of ",
          "different size compare; \"total\" the whole table."
        ),
        example = "facet",
        type = arg_enum(c("facet", "total"))
      ),
      missing = new_arg_spec(
        paste0(
          "Missing values of a grouping, colour or facet column (and of a ",
          "Table 1 variable): \"level\" (default) shows them as ",
          "\"(Missing)\", a row a click can filter on; \"drop\" gives them ",
          "no row or level. Same as the chart block's `missing`."
        ),
        example = "drop",
        type = arg_enum(c("level", "drop"))
      ),
      format = new_arg_spec(
        paste0(
          "How numbers print in the records and groups layouts (cells, value ",
          "labels, cards, downloads; not the variables layout): \"number\" ",
          "(default, up to two decimals), \"compact\" (1.2K, 3.4M), ",
          "\"percent\" (a fraction x100 with %) or a whole number of ",
          "decimals from 0 to 6, printed exactly, e.g. 0 for counts. Same as ",
          "the chart block's `format`."
        ),
        example = "number",
        type = arg_string()
      ),
      unit = new_arg_spec(
        paste0(
          "Free-text unit of the numbers (e.g. \"CHF\", \"kg\"); the ",
          "headers of the records layout's numeric columns and of the groups ",
          "layout's columns that are not counts carry it once, a card prints ",
          "it after the number. Never guessed from the data."
        ),
        example = "CHF",
        type = arg_string()
      )
    ),
    rec[c("rowname", "columns", "color", "shadings")],
    grp,
    var,
    rec[c("drill", "drill_target", "drill_table", "title", "subtitle",
          "caption")]
  ))
}

#' The `summaries` argument spec of the table block, under the public keys
#' (R/lane-summaries.R).
#' @noRd
table_summaries_spec <- function() {
  funcs <- c("identity", "mean", "median", "min", "max")
  new_arg_spec(
    paste0(
      "Groups and variables layouts: the summaries, one object per table ",
      "column (groups) or variable (variables). Each object has a type:\n",
      "- simple: func (count, count_distinct, sum, mean, median, min, max, ",
      "identity) of col; show = bar, number, dot or tile. color = a ",
      "categorical column splits the cell (a bar into segments; position = ",
      "stack, dodge or fill). tile: a square cell for heatmaps, painted by ",
      "the worst level of fill (a column) or by its value; with facet, one ",
      "column per level, most frequent first (facet_order = count or ",
      "level).\n",
      "- dist: the distribution of col; show = box, pointrange or text. ",
      "summary = the inner range, as on the chart: median_q1_q3, mean_sd, ",
      "mean_2sd, mean_se, mean_ci95 (table only), p5_p95, p10_p90, ",
      "min_max, or none; for text the printed statistic. whiskers = the ",
      "outer range: tukey or any summary value, or none. style = dot or ",
      "box.\n",
      "- field: col, the group's distinct values joined (a group fact).\n",
      "- series: col over x, a sparkline; optional band = [low, high] ",
      "columns and ref = mean, mean_sd or median_iqr.\n",
      "- spans: x to xend, a swimlane; optional color, label (the event ",
      "column headlining each segment's tooltip), tooltip (extra tooltip ",
      "columns), size = md or lg.\n",
      "- pair: a dumbbell from x to xend, each summarised by x_func / ",
      "xend_func (identity, mean, median, min, max; defaults identity and ",
      "max); optional ref_lo / ref_hi = a number or a column for a shaded ",
      "reference range (an xend outside it is drawn open), ref = a number ",
      "for a dashed line, dash = a column whose non-first levels draw ",
      "dashed.\n",
      "- expr: free R code over the group's rows, text.\n",
      "- custom: fn = a function of the rows, `\\(d) ...`, returning a ",
      "data frame whose columns say what they are (value; from, to; lo, ",
      "q1, mid, q3, hi; text); keep the grouping, use dplyr verbs.\n",
      "Every object takes name (the header), show, color and facet (a ",
      "categorical column: the column repeats once per level on one ",
      "scale). Colour and facet are per column. In the variables layout a ",
      "dist names a numeric variable and a simple count split by color a ",
      "categorical one."
    ),
    example = list(
      list(type = "simple", name = "Subjects", func = "count_distinct",
           col = "USUBJID", show = "bar"),
      list(type = "dist", name = "Duration", col = "DUR",
           summary = "median_q1_q3", show = "box"),
      list(type = "field", name = "Arm", col = "TRT01A")
    ),
    type = arg_array(arg_object(
      type = arg_enum(c("simple", "dist", "field", "series", "spans",
                        "pair", "expr", "custom")),
      name = arg_string(required = FALSE),
      show = arg_string(required = FALSE),
      func = arg_string(required = FALSE),
      col = arg_string(required = FALSE),
      summary = arg_enum(c(LANE_STATS, "none"), required = FALSE),
      whiskers = arg_enum(c(LANE_WHISKERS, "none"), required = FALSE),
      style = arg_enum(c("dot", "box"), required = FALSE),
      color = arg_string(required = FALSE),
      position = arg_enum(c("stack", "dodge", "fill"), required = FALSE),
      facet = arg_string(required = FALSE),
      facet_order = arg_enum(c("count", "level"), required = FALSE),
      fill = arg_string(required = FALSE),
      x = arg_string(required = FALSE),
      xend = arg_string(required = FALSE),
      x_func = arg_enum(funcs, required = FALSE),
      xend_func = arg_enum(funcs, required = FALSE),
      ref_lo = arg_string(required = FALSE),
      ref_hi = arg_string(required = FALSE),
      band = arg_array(arg_string(), required = FALSE),
      ref = arg_string(required = FALSE),
      dash = arg_string(required = FALSE),
      label = arg_string(required = FALSE),
      tooltip = arg_array(arg_string(), required = FALSE),
      size = arg_enum(c("md", "lg"), required = FALSE),
      expr = arg_string(required = FALSE),
      fn = arg_string(required = FALSE),
      denom = arg_string(required = FALSE)
    ))
  )
}


#' The column a groups table starts with when it has none: a count of rows,
#' drawn as a bar. Without `by` that is one row, the number of input rows.
#' The gear opens it, so a new table shows what its first column computes.
#' @noRd
table_groups_seed <- function() {
  list(list(type = "simple", func = "count", show = "bar", name = "Count"))
}

#' Construction guidance for the drill-down table block
#' @noRd
table_guidance <- function() {
  paste0(
    "The table block draws every table. `layout` says what one row is:\n",
    "- \"records\": the input rows as they are -- listings, model output, ",
    "a correlation matrix, or a finished table built in R (a frame from ",
    "dplyr, summary_table(), gtsummary converted with as_annotated_df()). ",
    "`rowname` picks the stub column; `columns` EXACTLY the columns the ",
    "user names (\"Revenue and Profit\" -> columns = [\"Revenue\", ",
    "\"Profit\"]), empty for all. `by` nests the rows under one header per ",
    "level. `shadings` colours numeric cells (diverging for a correlation ",
    "matrix, sequential for magnitudes); `format` is \"number\", ",
    "\"compact\", \"percent\" or a number of decimals, `unit` a unit ",
    "for the headers.\n",
    "- \"groups\": one row per level of `by` (at most two, outer first; the ",
    "outer is an expandable parent), one column per entry of `summaries`. ",
    "Without `by` it is one row over all input rows: the headline numbers ",
    "of a dashboard, usually with look = \"cards\". ",
    "Use it when one row is one category: most frequent adverse events ",
    "(simple count_distinct of USUBJID, show = bar), duration by term ",
    "(dist, show = box), an episode swimlane per subject (spans), a lab ",
    "trajectory (series), baseline to peak (pair). `color` on a summary ",
    "splits its cell (severity); `facet` repeats it per level (treatment ",
    "arm) on one scale; both are per column, never table-wide.\n",
    "- \"variables\": a Table 1. One row per statistic or level of each ",
    "variable in `summaries` (a dist for a numeric variable, a simple count ",
    "split by color for a categorical one), one column per level of `by`. ",
    "`stats` picks the numeric statistics (mean_sd, median_q1_q3, n_pct, ",
    "min_max, ...), `total` adds a Total column, `sections` is an OUTER ",
    "column that contains the variables (SOC over preferred term: ",
    "sections = [\"AEBODSYS\"], the term as the variable).\n",
    "A heatmap is the groups layout with look = \"compact\": by = [arm, ",
    "subject], one simple count with show = \"tile\", facet = the term, ",
    "fill = the grade (painted by the worst level). look = \"cards\" draws ",
    "one card per summary.\n",
    "Counting: only when the user asks for subjects or patients rather ",
    "than events, never for one-row-per-subject data. Groups layout: a ",
    "simple count_distinct with col = USUBJID. Variables layout: ",
    "`id` = USUBJID. ",
    "`pct_of` = \"facet\" (default) takes a count's percentage over its ",
    "arm; \"total\" over the table. `missing` = \"level\" (default) shows ",
    "missing groups as \"(Missing)\"; \"drop\" leaves them out.\n",
    "`drill` makes a row click filter downstream blocks; `drill_target` ",
    "sends the click to a filter block instead. Do NOT set `top_n` for an ",
    "interactive board (the table scrolls); it is for report exhibits.\n",
    "Map common requests: \"demographics by arm\" -> layout = \"variables\", ",
    "by = [\"TRT01A\"], summaries = AGE (dist), SEX and RACE (simple count ",
    "split by color), total = TRUE. \"most frequent AEs per arm\" -> layout ",
    "= \"groups\", by = [\"AEDECOD\"], summaries = ",
    "[simple count_distinct of USUBJID, facet = \"TRT01A\"]. For vertical ",
    "columns, lines or two measures on axes, use the chart block."
  )
}

#' Table block
#'
#' One block for every table: the input rows as they are, or one row per
#' group with a column per summary. `layout` picks what a row is. Input that
#' already is a finished table (an annotated data frame, or an object with an
#' [as_annotated_df()] method such as a composer table) is shown with its
#' structure in the records layout.
#'
#' The block's data output is the input, filtered by the last row click when
#' a drill is on. The table itself is display.
#'
#' @param layout What a row is: `"records"` (the input rows), `"groups"`
#'   (one row per level of `by`, one column per entry of `summaries`) or
#'   `"variables"` (a Table 1: one row per statistic or level of each
#'   variable in `summaries`, one column per level of `by`).
#' @param by Grouping columns, outer first, at most two. Groups layout: one
#'   row per level, the outer one an expandable parent; without `by`, one row
#'   over all input rows. Variables layout: one column per level.
#' @param rowname Records layout: the stub column (first column, row label).
#'   With `by` set, the rows sit under one collapsible header per level of
#'   `by` ([table_nest()]).
#' @param columns Records layout: the columns shown beside the row names, in
#'   order; `NULL` shows all.
#' @param summaries Groups and variables layouts: the table's columns (or
#'   variables), an ordered list of typed summaries, each a
#'   `list(type = , ...)`. Types: `simple` (`func`, `col`; show `"bar"`,
#'   `"number"`, `"dot"` or `"tile"`; a `color` split with `position` =
#'   `"stack"`, `"dodge"` or `"fill"`; a tile's `fill` and `facet_order`),
#'   `dist` (`col`, `summary`, `whiskers`, `style`; show `"box"`,
#'   `"pointrange"` or `"text"`), `field` (`col`), `series` (`x`, `col`,
#'   `band`, `ref`), `spans` (`x`, `xend`, `label`, `tooltip`, `size`),
#'   `pair` (`x`, `xend`, `x_func`, `xend_func`, `ref_lo`, `ref_hi`, `ref`,
#'   `dash`), `expr` and `custom`. Each takes `name`, `show`, and its own
#'   `color` and `facet`. Entries written with the summarize table's keys
#'   (`stat`, `from` / `to`, `lo` / `hi`, `fields`) are read as these. An
#'   empty list in the groups layout is one bar column, a count of rows.
#' @param id Variables layout: a column whose distinct values are counted
#'   instead of rows (a subject identifier, so a count reads as "subjects
#'   with at least one event"). In the groups layout a summary counts
#'   subjects itself, with `func = "count_distinct"` and `col`.
#' @param pct_of The base of a count's percentage: `"facet"` (the rows or
#'   subjects of the column's own facet level) or `"total"` (the table).
#' @param missing Missing values of a grouping, colour or facet column, and
#'   of a Table 1 variable: `"level"` (default; shown as "(Missing)", and a
#'   click on that row gives the rows with no value) or `"drop"` (no row or
#'   level of their own). The records layout keeps every row: `"level"` heads
#'   the rows without a `by` value "(Missing)", `"drop"` leaves that header
#'   blank. As on [new_chart_block()].
#' @param color Records layout: column whose colours tint the rows. `NULL` =
#'   the stub column when the board's scale map has it, `""` = off.
#' @param shadings Records layout: cell colour rules, a list of
#'   `list(mode, cols, domain)` with `mode` one of `"sequential"`,
#'   `"diverging"`, `"bar"`.
#' @param format,unit How the numbers print, as on [new_chart_block()]: in the
#'   records layout's cells, the groups layout's number cells and the value
#'   labels beside its bars, on cards and in the downloads. The variables
#'   layout (Table 1) keeps its statistics' own formats. `format` is
#'   `"number"` (default; up to two decimals, trailing zeros dropped, a value
#'   that would round to 0 keeps two significant digits), `"compact"` (1.2K,
#'   3.4M), `"percent"` (a fraction times 100, with "%") or a whole number of
#'   decimals from 0 to 6, printed exactly (`2`: 4.30). `unit` is free text
#'   (`"CHF"`): it heads the numeric columns of the records layout and the
#'   groups layout's columns that are not counts, "Revenue (CHF)", and a
#'   card prints it after its number. The old `digits` argument reads as
#'   `format`.
#' @param look How the table is drawn: `"table"`, `"compact"` (the matrix
#'   look: rotated column titles, so a column is as wide as its cells, and
#'   square cells for `show = "tile"` summaries, a heatmap) or `"cards"` (one
#'   card per summary, one row of cards per row of the table: its label, its
#'   number in large type and the summary's glyph). Cards draw the groups
#'   layout (one row of cards per level of `by`, a single row without `by`)
#'   and the records layout (one card per column); the variables layout has
#'   no card form and draws as a table. `spans` and `expr` summaries are left
#'   out of the cards. The old `compact = TRUE` reads as `look = "compact"`.
#' @param facet_layout,sort_by,sort_dir,top_n,value_labels,axis,bar_width
#'   Groups layout: column order of faceted summaries, row order, a row cap,
#'   value labels beside the marks, the domain strip under glyph columns, and
#'   the mark length. See the summarize table.
#' @param drill Turns the row click on. In the records layout the column the
#'   click filters on (or `"auto"`, `"source"`); in the groups layout any
#'   value turns it on and the click filters on the row's groups.
#' @param drill_target,drill_table Beta. Send the click to a filter block
#'   instead of filtering this block's output: the target block id, and the
#'   table the claim is resolved in.
#' @param search,sortable,collapsible Show the search input; allow sorting by
#'   column headers; allow collapsing group rows.
#' @param max_height `NULL` (default): the table runs its full length and
#'   scrolls with its panel. A CSS length caps it.
#' @param download `TRUE` (default) offers the table for download (xlsx,
#'   html, pptx), as on [new_chart_block()].
#' @param title,subtitle,caption Text above and below the table, as on
#'   [new_chart_block()]: `NULL` = the input's own label, `""` = none, else a
#'   template resolved against the data.
#' @param stats,sections,total,total_label,nest Variables layout: the
#'   statistics shown for numeric variables (keys of [summary_table()]'s
#'   `stats`), outer row sections, a total column and its label, and nested
#'   categorical hierarchies. See [summary_table()].
#' @param script,script_values Prepare script, as on [new_chart_block()]:
#'   R code run on the input before any layout builds its table, and the
#'   values of its controls. In the records and variables layouts a click
#'   filters the rows the script returned; in the groups layout it filters
#'   the input rows before the script, as on the chart.
#' @param selection Internal: what the last click selected. Saved with the
#'   board so a restored table keeps its drill.
#' @param ... Old argument names (translated), and arguments forwarded to
#'   [blockr.core::new_transform_block()].
#'
#' @return A blockr transform block of class `table_block`.
#' @examplesIf interactive()
#' new_table_block()
#' new_table_block(
#'   layout = "groups", by = "Species",
#'   summaries = list(
#'     list(type = "simple", func = "count", show = "bar"),
#'     list(type = "dist", col = "Sepal.Length", show = "box")
#'   )
#' )
#' @export
new_table_block <- function(layout = "records",
                            # `...` this early on purpose: every argument
                            # after it matches by exact name only, so an old
                            # name such as `value` cannot partially match a
                            # new one such as `value_labels`.
                            ...,
                            by = character(),
                            rowname = NULL,
                            columns = NULL,
                            summaries = list(),
                            id = NULL,
                            pct_of = "facet",
                            missing = "level",
                            color = NULL,
                            shadings = list(),
                            format = "number",
                            unit = "",
                            look = "table",
                            facet_layout = "by_summary",
                            sort_by = "value",
                            sort_dir = "desc",
                            top_n = NULL,
                            value_labels = TRUE,
                            axis = TRUE,
                            bar_width = "fit",
                            drill = NULL,
                            drill_target = "",
                            drill_table = "",
                            search = TRUE,
                            sortable = TRUE,
                            collapsible = TRUE,
                            max_height = NULL,
                            download = TRUE,
                            title = NULL,
                            subtitle = NULL,
                            caption = getOption("blockr.viz.default_caption"),
                            stats = "mean_sd",
                            sections = character(),
                            total = FALSE,
                            total_label = "Total",
                            nest = FALSE,
                            script = NULL,
                            script_values = list(),
                            selection = NULL) {
  # Old argument names in code (the table block's before the rewrite) are
  # translated here, by the same table the board deserializer uses
  # (R/table-args.R). Everything else in `...` goes to new_transform_block().
  dots <- list(...)
  legacy <- intersect(names(dots), table_legacy_names("table"))
  if (length(legacy)) {
    cur <- mget(table_formal_names(), envir = environment())
    tr <- table_legacy_args(c(cur, dots[legacy]), from = "table")
    for (nm in intersect(names(tr), table_formal_names())) {
      assign(nm, tr[[nm]])
    }
    dots <- dots[setdiff(names(dots), legacy)]
  }
  # An old aggregation list passed under the new name.
  if (table_is_agg_summaries(summaries)) {
    summaries <- table_agg_to_summaries(summaries)
  }

  layout <- chr_state(layout) %||% "records"
  if (!layout %in% c("records", "groups", "variables")) layout <- "records"
  look <- table_look(look)
  pct_of <- chr_state(pct_of) %||% "facet"
  if (!pct_of %in% c("facet", "total")) pct_of <- "facet"
  missing <- chr_state(missing) %||% "level"
  if (!missing %in% c("drop", "level")) missing <- "level"
  # Alphabetical order was "label" before it took the chart's "alpha".
  sort_by <- summarize_sort_by(sort_by)
  # Number format of the records layout's cells, the chart's set:
  # "number" (up to two decimals), "compact", "percent" or a whole number of
  # decimals. `unit` heads the numeric columns.
  format <- table_format_state(format)
  unit <- chr_state(trimws(as.character(unlist(unit)))) %||% ""
  sections <- chr_vec_state(sections) %||% character()
  # Heal state poisoned by a pre-#144 DAG copy/paste, where a NULL slot came
  # back as list() (R/state-normalize.R). `color` only collapses the empty
  # list: "" is meaningful there (tint explicitly off).
  by <- chr_vec_state(by)
  rowname <- chr_state(rowname)
  columns <- chr_vec_state(columns)
  color <- null_state(color)
  top_n <- null_state(top_n)
  summaries <- if (is.list(summaries)) summaries else list()
  # Old summary keys (stat, from / to, lo / hi, fields, ...) read as the
  # public ones (R/lane-summaries.R).
  summaries <- lapply(summaries, lane_public_summary)
  if (identical(layout, "groups") && !length(summaries)) {
    summaries <- table_groups_seed()
  }
  shadings <- if (is.list(shadings)) shadings else list()
  script_values <- if (is.list(script_values)) script_values else list()
  # `id` is also the module id inside the server; keep the column apart.
  id_col <- chr_state(id)
  drill <- chr_state(drill)
  drill_target <- chr_state(drill_target) %||% ""
  drill_table <- chr_state(drill_table) %||% ""
  max_height <- chr_state(max_height)
  # LEGACY: the old table default. Every board saved before the table
  # scrolled with its panel carries it, not a choice anyone made.
  if (identical(max_height, "600px")) max_height <- NULL
  title <- title_state(title)
  subtitle <- title_state(subtitle)
  caption <- title_state(caption)
  selection <- if (is.list(selection) && length(selection)) selection

  # Serialization identity, named because the call below goes through
  # do.call(), where blockr.core's default (`sys.parent()`) would not find
  # this function. A restore passes its own ctor / ctor_pkg in `...`.
  if (!"ctor" %in% names(dots)) {
    dots$ctor <- "new_table_block"
    dots$ctor_pkg <- "blockr.viz"
  }

  args <- c(
    list(
      server = function(id, data) {
        shiny::moduleServer(id, function(input, output, session) {
          ns <- session$ns

          r_layout <- shiny::reactiveVal(layout)

          # The prepare script runs before every layout (R/table-prepare.R).
          prep <- table_prepare(data, script, script_values)

          # Settings both layouts share. The block owns them, so a layout
          # switch keeps them.
          shared <- list(
            title = shiny::reactiveVal(title),
            subtitle = shiny::reactiveVal(subtitle),
            caption = shiny::reactiveVal(caption),
            download = shiny::reactiveVal(isTRUE(download)),
            search = shiny::reactiveVal(isTRUE(search)),
            sortable = shiny::reactiveVal(isTRUE(sortable)),
            drill = shiny::reactiveVal(drill),
            max_height = shiny::reactiveVal(max_height),
            ctrl_target = shiny::reactiveVal(drill_target),
            ctrl_table = shiny::reactiveVal(drill_table),
            # The groups and variables layouts read the same variables and
            # grouping: switching between them transposes the table.
            by = shiny::reactiveVal(by),
            summaries = shiny::reactiveVal(summaries),
            id = shiny::reactiveVal(id_col),
            look = shiny::reactiveVal(look),
            pct_of = shiny::reactiveVal(pct_of),
            missing = shiny::reactiveVal(missing),
            format = shiny::reactiveVal(format),
            unit = shiny::reactiveVal(unit),
            # The records engine draws cards only in the records layout: the
            # variables layout has no card form.
            layout = r_layout,
            # The input before nesting: the records gear offers its columns
            # for "Group by".
            input = prep$data,
            # The prepare script, for the records gear (the groups engine
            # takes it as `prep`).
            prep = prep
          )

          # Only the active engine sees data. The other one parks: its
          # reactives stop at req(), so it neither computes nor pushes.
          gate <- function(which) {
            shiny::reactive({
              shiny::req(identical(r_layout(), which))
              prep$data()
            })
          }
          sel_for <- function(which) if (layout %in% which) selection

          var <- table_variables_server(
            "variables", gate("variables"), shared = shared,
            stats = stats, sections = sections, total = total,
            total_label = total_label, nest = nest
          )

          # The records engine draws the records layout (the input rows) and
          # the variables layout (the table summary_table() builds from them).
          rec_data <- shiny::reactive({
            lay <- r_layout()
            shiny::req(lay %in% c("records", "variables"))
            d <- prep$data()
            if (identical(lay, "variables")) {
              args <- table_variables_args(var$config())
              if (!is.null(args)) d <- do.call(summary_table, c(list(d), args))
            } else if (length(shared$by()) && is.data.frame(d)) {
              # Records under headers: the tidy frame nested by `by`.
              d <- table_nest(d, shared$by(), rec_rowname(),
                              missing = shared$missing(),
                              columns = rec$state$value())
            }
            d
          })
          # Read lazily: the records engine is built just below.
          rec_rowname <- function() rec$state$rowname()

          rec_sel <- sel_for(c("records", "variables"))
          rec <- table_records_server(
            "records", rec_data,
            rowname = rowname, value = columns, color = color,
            shadings = shadings, drill = drill, digits = format,
            max_height = max_height,
            filter_column = rec_sel$column, filter_values = rec_sel$values,
            filter_group_cols = rec_sel$group_cols,
            filter_group_vals = rec_sel$group_vals,
            filter_col_keys = rec_sel$col_keys,
            sortable = sortable, collapsible = collapsible, search = search,
            download = download, title = title, subtitle = subtitle,
            caption = caption, ctrl_target = drill_target,
            ctrl_table = drill_table,
            shared = shared
          )

          grp_sel <- sel_for("groups")
          grp <- table_groups_server(
            "groups", gate("groups"),
            id_var = id_col, summaries = summaries, by = by,
            facet_layout = facet_layout, sort_by = sort_by,
            sort_dir = sort_dir, top_n = top_n, max_height = max_height,
            search = search, sortable = sortable, axis = axis,
            value_labels = value_labels, bar_width = bar_width,
            download = download, title = title, subtitle = subtitle,
            caption = caption, drill = drill, prep = prep,
            look = look,
            ctrl_target = drill_target,
            ctrl_table = drill_table,
            filter_column = grp_sel$column, filter_values = grp_sel$values,
            shared = shared
          )

          # Layout switch. Either gear sends it as an ordinary config edit on
          # its engine's action input; the engines ignore the key.
          set_layout <- function(msg) {
            if (is.null(msg) || !identical(msg$action %||% "config", "config") ||
                  !identical(msg$param, "layout")) {
              return()
            }
            v <- as.character(unlist(msg$value))[1L]
            if (isTRUE(v %in% c("records", "groups", "variables")) &&
                  !identical(shiny::isolate(r_layout()), v)) {
              if (identical(v, "groups") &&
                    !length(shiny::isolate(shared$summaries()))) {
                shared$summaries(table_groups_seed())
              }
              r_layout(v)
            }
          }
          # The records engine leaves the prepare script to the block.
          shiny::observeEvent(input[["records-drilldown_table_block_action"]], {
            msg <- input[["records-drilldown_table_block_action"]]
            set_layout(msg)
            if (identical(msg$action %||% "config", "config") &&
                  is.character(msg$param)) {
              prep$config(msg$param[[1L]], msg$value)
            }
          })
          shiny::observeEvent(input[["groups-summarize_table_block_action"]],
                              set_layout(input[["groups-summarize_table_block_action"]]))

          output$layout_css <- shiny::renderUI({
            table_layout_css(ns, r_layout())
          })
          # Both engines' "Rows are" switches draw the layout from this push,
          # never from their own last click (table.js, layoutOf).
          shiny::observe({
            session$sendCustomMessage("blockr-viz-table-layout", list(
              ids = c(ns("records-drilldown_table_block"),
                      ns("groups-summarize_table_block")),
              layout = r_layout()
            ))
          })
          shiny::outputOptions(output, "layout_css", suspendWhenHidden = FALSE)

          # The cards' script and stylesheet, asked for the first time the
          # table turns to cards (a table that starts in cards ships them
          # with its UI). Never withdrawn: a dependency stays loaded.
          output$cards_dep <- shiny::renderUI({
            if (identical(shared$look(), "cards")) table_cards_dep()
          })
          shiny::outputOptions(output, "cards_dep", suspendWhenHidden = FALSE)

          # The groups engine reports failed custom summaries as conditions;
          # they count only while that layout is on.
          cond <- shiny::reactiveValues(error = list())
          shiny::observe({
            cond$error <- if (identical(r_layout(), "groups")) {
              grp$cond$error
            } else {
              list()
            }
          })

          rs <- rec$state
          gs <- grp$state
          r_selection <- function() {
            sel <- if (identical(r_layout(), "groups")) {
              list(column = gs$filter_column(), values = gs$filter_values())
            } else {
              list(column = rs$filter_column(), values = rs$filter_values(),
                   group_cols = rs$filter_group_cols(),
                   group_vals = rs$filter_group_vals(),
                   col_keys = rs$filter_col_keys())
            }
            sel <- sel[!vapply(sel, function(x) is.null(x) || !length(x),
                               logical(1L))]
            if (length(sel)) sel
          }

          list(
            expr = shiny::reactive({
              lay <- r_layout()
              if (identical(lay, "groups")) return(grp$expr())
              ex <- rec$expr()
              if (identical(lay, "variables")) {
                # The drill filter applies to the built table, as it did
                # when a summary table block fed a table block.
                built <- table_variables_call(var$config())
                if (!is.null(built)) {
                  built[[2L]] <- quote(.(data))
                  ex <- table_sub_data(ex, built)
                }
              } else if (length(shared$by())) {
                # Records under headers: the output is the input rows, as
                # without `by`. A header click claims `.group<k>_level`,
                # which is the k-th `by` column. The nesting, `missing` and
                # `columns` shape only what the table shows.
                ex <- table_nest_drill_expr(shared$by(),
                                            rs$filter_group_cols(),
                                            rs$filter_group_vals())
              }
              # The script feeds the table; the click filters what it shows.
              prep$wrap_after(ex)
            }),
            cond = cond,
            state = list(
              layout = r_layout,
              by = gs$by,
              rowname = rs$rowname,
              columns = rs$value,
              summaries = gs$summaries,
              id = gs$id_var,
              color = rs$color,
              shadings = rs$shadings,
              format = rs$digits,
              unit = shared$unit,
              pct_of = shared$pct_of,
              missing = shared$missing,
              look = shared$look,
              facet_layout = gs$facet_layout,
              sort_by = gs$sort_by,
              sort_dir = gs$sort_dir,
              top_n = gs$top_n,
              value_labels = gs$value_labels,
              axis = gs$axis,
              bar_width = gs$bar_width,
              drill = shared$drill,
              drill_target = shared$ctrl_target,
              drill_table = shared$ctrl_table,
              search = shared$search,
              sortable = shared$sortable,
              collapsible = rs$collapsible,
              max_height = shared$max_height,
              download = shared$download,
              title = shared$title,
              subtitle = shared$subtitle,
              caption = shared$caption,
              stats = var$state$stats,
              sections = var$state$sections,
              total = var$state$total,
              total_label = var$state$total_label,
              nest = var$state$nest,
              script = prep$script,
              script_values = prep$values,
              selection = r_selection
            )
          )
        })
      },
      ui = function(id) {
        ns <- shiny::NS(id)
        shiny::tagList(
          # Static first paint: hide the engine that is not on. The server's
          # rule (layout_css) takes over from here.
          table_layout_css(ns, layout),
          shiny::uiOutput(ns("layout_css")),
          if (identical(look, "cards")) table_cards_dep(),
          shiny::uiOutput(ns("cards_dep")),
          htmltools::tags$div(
            id = ns("variables_wrap"), class = "blockr-table-engine",
            table_variables_ui(ns("variables"))
          ),
          htmltools::tags$div(
            id = ns("records_wrap"), class = "blockr-table-engine",
            table_records_ui(ns("records"))
          ),
          htmltools::tags$div(
            id = ns("groups_wrap"), class = "blockr-table-engine",
            table_groups_ui(ns("groups"))
          )
        )
      },
      # Contract check only (dispatch lookup), as before: a composer table
      # or anything with an as_annotated_df() method connects directly.
      dat_valid = validate_annotated_df_input,
      allow_empty_state = c(
        "by", "rowname", "columns", "summaries", "id", "color", "shadings",
        "unit",
        "top_n", "drill", "drill_target", "drill_table", "max_height",
        "title", "subtitle", "caption", "script", "script_values",
        "selection", "sections"
      ),
      external_ctrl = c(
        "layout", "by", "rowname", "columns", "summaries", "id", "color",
        "shadings", "format", "unit", "pct_of", "missing", "look",
        "facet_layout",
        "sort_by",
        "sort_dir", "top_n", "value_labels", "axis", "bar_width", "drill",
        "drill_target", "drill_table", "search", "sortable", "collapsible",
        "max_height", "download", "title", "subtitle", "caption",
        "stats", "sections", "total", "total_label", "nest", "script",
        "script_values"
      ),
      expr_type = "bquoted",
      class = "table_block"
    ),
    dots
  )
  do.call(blockr.core::new_transform_block, args)
}

# Show one engine, hide the other. `!important` so the rule from the server
# beats the static first-paint rule in the UI.
table_layout_css <- function(ns, layout) {
  on <- switch(layout, groups = "groups", variables = c("variables", "records"),
               "records")
  off <- setdiff(c("records", "groups", "variables"), on)
  htmltools::tags$style(htmltools::HTML(paste0(
    paste0("#", ns(paste0(off, "_wrap")), "{display:none!important}",
           collapse = ""),
    paste0("#", ns(paste0(on, "_wrap")), "{display:block!important}",
           collapse = "")
  )))
}
