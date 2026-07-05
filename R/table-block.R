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
#' @param digits Rounding for numeric display. Default `2`.
#' @param max_height CSS max-height of the scroll container.
#' @param row_hex,row_color Optional per-row colouring. `row_color` names a
#'   column whose values drive a row background scale; `row_hex` supplies
#'   explicit per-row hex colours. Both default `NULL` (no row colouring).
#'
#' @details
#' **Structured "Table 1" input.** When `data` follows the dotted-column
#' convention that [summary_table()] emits -- a tidy `.fmt` frame (numeric
#' columns + a per-row `.fmt` template + `.group`), or the already-wide
#' display grid with `.section_*` / `.label` / `.indent` / `.strong` columns --
#' the renderer switches to the *structured* layout: row-side section
#' headers with collapse/expand toggles, `.indent` row stubs, and
#' multi-level column spanners parsed from `|`-delimited column names (each
#' leaf carries its header text in `attr(col, "label")`). This is the same
#' structure [html_table()] renders, folded into the interactive table so
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
                            digits = 2L,
                            max_height = "600px",
                            row_hex = NULL,
                            row_color = NULL) {
  stopifnot(is.data.frame(data))
  # Tidy `.fmt` form (numbers + per-row template + `.group`) -> wide display
  # grid (format-then-spread), and detect the structured / sectioned form.
  # No-op on a plain flat frame.
  data <- fmt_to_wide(data)
  dt_chrome(
    elem_id    = elem_id,
    structured = dt_is_structured(data),
    max_height = max_height,
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
                         shadings = list(), drill = NULL, digits = 2L,
                         row_hex = NULL, color = NULL,
                         sortable = TRUE, collapsible = TRUE, search = TRUE,
                         excel_download = FALSE, group_cols = NULL,
                         group = character(), summaries = list(),
                         active = NULL, gear_cols = NULL) {
  data <- fmt_to_wide(data)
  # Display-option states (sortable / collapsible / search / excel) ride on the
  # <table> as data-attributes so the gear popover reads their current value;
  # the renderer honours `sortable` / `collapsible` here, while `search` /
  # `excel_download` are realized by the chrome (dt_chrome / dt_download).
  toggles <- list(sortable = sortable, collapsible = collapsible,
                  search = search, excel_download = excel_download)
  if (dt_is_structured(data)) {
    return(dt_table_tag_structured(data, drill, digits, toggles,
                                   active = active))
  }
  # Pickable columns for the gear (data-dt-cols). `data` here is the raw
  # input except in the block server's aggregated branch, which displays a
  # projection and passes the raw schema in explicitly.
  if (is.null(gear_cols)) gear_cols <- dt_gear_cols_json(data)

  value_cols_raw <- value_cols
  if (is.null(label_col)) label_col <- names(data)[1L]
  if (is.null(value_cols)) value_cols <- setdiff(names(data), label_col)
  value_cols <- intersect(value_cols, names(data))

  # Differentiated non-renderable states (chart-empty-state parity): a
  # configured column that vanished upstream, a required mapping still
  # unconfigured, and a genuinely 0-row frame are three different problems
  # with three different fixes -- one generic "No data" hid all of them.
  msg <- dt_state_message(data, label_col, value_cols, value_cols_raw)
  if (!is.null(msg)) {
    # The gear must still offer the (current) input columns on a message
    # table -- fixing a vanished-column config happens through it.
    return(dt_table_attrs(dt_message_table(msg), NULL, NULL, digits,
                          color = color, toggles = toggles,
                          gear_cols = gear_cols))
  }

  # ---- cell visuals: value-encoding `shadings` rules ------------------
  # Repeatable rules `list(list(mode, cols))` resolved to per-column visuals
  # (see dd_shading_visuals): explicit cols claim; empty cols = all numeric
  # minus claimed (override rule, re-resolved per render so it survives
  # upstream schema changes); diverging/sequential pool one domain per rule,
  # bars normalize per column.
  shading_vis <- dd_shading_visuals(shadings, data, value_cols)

  # ---- thead ----------------------------------------------------------
  # Per-column numeric flag drives type-based alignment for both the header
  # and the body cells (numeric right, text left).
  num_flag <- vapply(data[value_cols], is.numeric, logical(1L))
  th_cells <- list(dt_th(label_col, 0L, stub = TRUE,
                         label = dt_col_label(data[[label_col]], label_col),
                         sortable = sortable))
  for (i in seq_along(value_cols)) {
    th_cells[[length(th_cells) + 1L]] <- dt_th(
      value_cols[i], i,
      label = dt_col_label(data[[value_cols[i]]], value_cols[i]),
      numeric = num_flag[i],
      sortable = sortable
    )
  }
  thead <- htmltools::tags$thead(htmltools::tags$tr(th_cells))

  # ---- tbody (vectorized) ---------------------------------------------
  # Build the body as a single HTML string instead of one htmltools tag
  # object per cell. For a wide preview (e.g. ADaM ADSL, ~48 columns) the
  # per-cell `tags$td()` construction plus the `renderTags()` tree walk
  # dominated render time -- ~1 s for the full frame, the source of the
  # "drilldown filter takes ~2 s" lag. Column-vectorized string assembly
  # is ~100x faster and emits identical markup (inter-tag whitespace
  # aside). Numeric columns are formatted with a single vectorized
  # `formatC(format = "fg", drop0trailing = TRUE)` call: `"fg"` formats
  # each value independently (no decimal alignment across the column, so
  # 1.5 stays "1.5" while a sibling 2.25 stays "2.25") and `drop0trailing`
  # trims padding -- byte-identical to the old per-element
  # `format(round(v), nsmall = 0, trim = TRUE)` but ~11x faster (the
  # per-cell `vapply(format())` was ~72% of the build time on a
  # numeric-heavy frame).
  # Text content uses htmltools' own escaper (the same one `tags$td()`
  # applies to a text child), so escaping is byte-identical: & < > are
  # escaped, quotes are not.
  esc <- function(x) htmltools::htmlEscape(as.character(x), attribute = FALSE)

  # Drill-relevant columns carry each cell's RAW value on a data-raw
  # attribute (read by wireClick in table.js instead of the displayed text):
  # numeric cells display rounded and NA renders as an em-dash, so a filter
  # built from textContent would match zero rows and silently empty
  # downstream. `as.character(raw)` round-trips exactly -- comparing a
  # numeric column to a character value in the filter expr coerces through
  # the same as.character(). NA cells get NO data-raw (the click is a no-op;
  # see the dt-row-nodrill class below).
  raw_cols <- intersect(unique(c(drill %||% character(),
                                 group_cols %||% character())),
                        c(label_col, value_cols))
  raw_attr <- function(x) {
    ifelse(is.na(x), "", paste0(
      " data-raw=\"",
      htmltools::htmlEscape(as.character(x), attribute = TRUE), "\""
    ))
  }

  col_cells <- vector("list", length(value_cols))
  # Display strings per column, kept for the server-side width estimation
  # below (same strings the cells render, no second formatting pass).
  disp_by_col <- rep(list(character(0L)), length(value_cols))
  for (j in seq_along(value_cols)) {
    col   <- data[[value_cols[j]]]
    keep  <- !is.na(col)
    # Type-based cell alignment matches the header (numeric right, text left).
    td_cls  <- if (num_flag[j]) "blockr-data dt-num" else "blockr-data dt-txt"
    na_cell <- paste0("<td class=\"", td_cls, "\">&mdash;</td>")
    out_j   <- rep(na_cell, length(col))
    if (any(keep)) {
      vk <- col[keep]
      disp <- if (num_flag[j]) {
        formatC(round(as.numeric(vk), digits), format = "f", digits = digits,
                drop0trailing = TRUE, big.mark = "")
      } else {
        as.character(vk)
      }
      disp_by_col[[j]] <- disp
      style <- ""
      sv <- shading_vis[[value_cols[j]]]
      if (num_flag[j] && !is.null(sv)) {
        if (identical(sv$kind, "bar")) {
          # Data bar: left-anchored gradient, width = |v| / column-abs-max.
          # A CSS gradient string keeps the vectorized single-HTML() render
          # (no per-cell DOM node). Text reads on top of the fill.
          style <- dt_bar_style(as.numeric(vk), sv$max, sv$fill)
        } else {
          # Heatmap: sv$fun is vectorized (see dt_color_fun) -- one call
          # styles the whole column, like dt_bar_style above.
          bg <- sv$fun(as.numeric(vk))
          style <- paste0(" style=\"background:", bg$bg, ";color:", bg$fg,
                          ";\"")
        }
      }
      raw <- if (value_cols[j] %in% raw_cols) raw_attr(vk) else ""
      out_j[keep] <- paste0("<td class=\"", td_cls, "\"", raw, style, ">",
                            esc(disp), "</td>")
    }
    col_cells[[j]] <- out_j
  }

  # Categorical scale-map row color (e.g. SEX: F = teal, M = orange) drawn as a
  # subtle accent bar on the left of the row, matching the chart's legend and
  # reading like a selected-row indicator. `row_hex` is a per-row vector (the
  # `row_color` column resolved through the scale map); NA rows get no bar.
  # Drawn with an inset box-shadow so it adds no width (no layout shift) and is
  # independent of the numeric `cell_bg` heatmap above.
  # NOTE (follow-up): to keep the bar visible when scrolling a wide table
  # horizontally, this stub cell's bar should become a dedicated left:0 sticky
  # indicator cell -- which needs the sort/drill column-index map to skip it.
  stub_lbl <- esc(data[[label_col]])
  stub_raw <- if (label_col %in% raw_cols) raw_attr(data[[label_col]]) else ""
  if (!is.null(row_hex) && length(row_hex) == nrow(data)) {
    bar <- ifelse(
      is.na(row_hex), "",
      paste0(" style=\"box-shadow:inset 3px 0 0 0 ", row_hex, ";\"")
    )
    stub_cells <- paste0("<td class=\"blockr-stub blockr-row-bar\"", stub_raw,
                         bar, ">", stub_lbl, "</td>")
  } else {
    stub_cells <- paste0("<td class=\"blockr-stub\"", stub_raw, ">",
                         stub_lbl, "</td>")
  }
  # A row whose drill value(s) include an NA cannot emit a filter (no data-raw
  # -> the click is a no-op); mark it so it doesn't LOOK clickable.
  row_cls <- rep("blockr-data-row", nrow(data))
  if (length(raw_cols)) {
    nodrill <- Reduce(`|`, lapply(raw_cols, function(cn) is.na(data[[cn]])))
    row_cls[nodrill] <- "blockr-data-row dt-row-nodrill"
  }
  row_inner  <- do.call(paste0, c(list(stub_cells), col_cells))
  rows_html  <- paste0("<tr class=\"", row_cls, "\">", row_inner, "</tr>",
                       collapse = "")
  tbody <- htmltools::tags$tbody(htmltools::HTML(rows_html))

  flat_labels <- c(
    dt_col_label(data[[label_col]], label_col) %||% "",
    vapply(value_cols, function(vc) {
      dt_col_label(data[[vc]], vc) %||% ""
    }, character(1L))
  )
  table_tag <- dt_fixed_table_tag(
    thead, tbody,
    dt_colgroup(
      c(label_col, value_cols),
      c(list(as.character(data[[label_col]])), disp_by_col),
      labels = flat_labels
    )
  )
  onclick <- dt_onclick(drill, c(label_col, value_cols))
  dt_table_attrs(table_tag, onclick$col, onclick$idx, digits,
                 color = color, shadings = shadings,
                 num_cols = value_cols[num_flag],
                 toggles = toggles, group_cols = group_cols,
                 group = group, summaries = summaries, active = active,
                 gear_cols = gear_cols)
}

# ---------------------------------------------------------------------------
# Structured ("Table 1") rendering -- section nesting, indents, spanners.
# Reuses html_table()'s thead / tbody builders so the structure matches the
# static html_table renderer exactly; only the surrounding chrome (the
# table-block wrapper, gear, drill/colour attributes) differs.
# ---------------------------------------------------------------------------

#' Does this (already wide) frame carry the dotted-column structure?
#'
#' True when it has any row-side section column (`.section_*`) OR a
#' `.label` stub OR an `.indent` / `.strong` column -- the signals
#' [summary_table()] emits and [html_table()] renders.
#' @noRd
dt_is_structured <- function(data) {
  any(grepl("^\\.(section_\\d+|label|indent|strong)$", names(data)))
}

#' Build the structured ("Table 1") `<table>` for the interactive table-block.
#'
#' Builds the section-aware `<tbody>` and multi-level `<thead>` with
#' `build_html_tbody()` / `build_html_thead()` (the very builders
#' [html_table()] uses) and tags it with the drill onclick attributes. The
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
  all_section_cols <- grep("^\\.section_\\d+$", names(data), value = TRUE)
  # Empty .section_* columns draw no "(missing)" header, but must still be
  # excluded from the data cells (use all_section_cols below) or they leak in
  # as a literal em-dash column.
  section_cols <- nonempty_section_cols(data, all_section_cols)
  stub_col     <- if (".label" %in% names(data)) ".label" else NULL
  styling_cols <- intersect(c(".indent", ".strong", ".emph"), names(data))
  data_cols    <- setdiff(names(data),
                          c(all_section_cols, stub_col, styling_cols))

  if (length(data_cols) == 0L || nrow(data) == 0L) {
    # Structured frames carry no gear-picked mappings, so only two states
    # apply here: no rows vs no data columns left after the structural
    # (.section_* / .label / styling) columns.
    msg <- if (nrow(data) == 0L) "No rows to display"
           else "No value columns to display"
    return(dt_table_attrs(dt_message_table(msg), NULL, NULL, digits,
                          toggles = toggles, gear_cols = "[]"))
  }

  thead <- build_html_thead(data, data_cols, stub_col, stub_sortable = FALSE,
                            sortable = sortable)
  tbody <- build_html_tbody(data, section_cols, stub_col, data_cols,
                            styling_cols = styling_cols,
                            collapsible = collapsible)

  table_tag <- dt_fixed_table_tag(
    thead, tbody,
    structured_colgroup(data, data_cols, stub_col)
  )

  # The stub is column 0 in a structured table; only honour drill when it
  # names the stub (the only categorical row identity that survives the
  # section layout).
  drill_use <- if (!is.null(drill) && !is.null(stub_col) &&
                   identical(drill, stub_col)) {
    stub_col
  } else {
    NULL
  }
  onclick <- dt_onclick(drill_use, c(stub_col %||% character(), data_cols))
  # "[]" (not absent): a structured frame HAS no pickable columns -- the gear
  # must read that, not fall back to scraping the section-spanner header.
  dt_table_attrs(table_tag, onclick$col, onclick$idx, digits,
                 toggles = toggles, active = active, gear_cols = "[]")
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
#' @return A list consumed by [drilldown_table()].
#' @examples
#' drilldown_table_color("sequential", domain = c(0, 100))
#' @export
drilldown_table_color <- function(type = c("diverging", "sequential", "bar"),
                                   domain = NULL, palette = NULL,
                                   columns = NULL) {
  type <- match.arg(type)
  list(type = type, domain = domain, palette = palette, columns = columns)
}

# --- internal helpers --------------------------------------------------

#' @noRd
dt_th <- function(name, idx, stub = FALSE, label = NULL, numeric = FALSE,
                  sortable = TRUE) {
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
      )
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
      )
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
#' (chart.js): a configured column no longer in the data names the column and
#' hints at an upstream rename + "re-pick it in the gear"; a required mapping
#' with nothing configured is a pick prompt; a 0-row frame is just "no rows".
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
#' @return `list(data, group, metric_cols)` -- the (aggregated or raw) frame,
#'   the resolved group columns, and the metric column names.
#' @noRd
dd_table_aggregate <- function(data, group, summaries) {
  group <- intersect(as.character(group), names(data))
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
  used <- character()
  uniq <- function(nm, tag) {
    if (!(nm %in% used)) {
      used <<- c(used, nm)
      return(nm)
    }
    # Same variable aggregated twice -> qualify the second so the name is unique.
    nm2 <- paste0(nm, " (", tag, ")")
    while (nm2 %in% used) nm2 <- paste0(nm2, " ")
    used <<- c(used, nm2)
    nm2
  }
  plan <- list()
  add <- function(name, expr, label) {
    plan[[length(plan) + 1L]] <<- list(name = name, expr = expr, label = label)
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
  plan
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
    # Optional power knobs (per-rule palette / fixed domain) ride along when
    # present -- the gear never writes them, ctor / legacy specs may.
    if (!is.null(s$palette)) out$palette <- s$palette
    if (!is.null(s$domain))  out$domain <- s$domain
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
      list(
        mode = jsonlite::unbox(as.character(s$mode %||% "diverging")[1L]),
        cols = as.character(s$cols %||% character())
      )
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
#' @return named list: column -> `list(kind = "bar", fill, max)` or
#'   `list(kind = "bg", fun)` (a dt_color_fun closure). Unlisted columns
#'   render plain.
#' @noRd
dd_shading_visuals <- function(shadings, data, value_cols) {
  out <- list()
  if (!length(shadings)) return(out)
  num_cols <- value_cols[vapply(data[value_cols], is.numeric, logical(1L))]
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
      vals <- unlist(data[cols], use.names = FALSE)
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
        for (cn in cols) out[[cn]] <- list(kind = "bg", fun = fun)
      }
    }
  }
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
  if (length(gcols) && length(gcols) == length(gvals)) {
    return(as.character(jsonlite::toJSON(
      list(filters = unname(Map(
        function(cl, vl) list(column = cl, value = vl), gcols, gvals
      ))),
      auto_unbox = TRUE
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
                           active = NULL, gear_cols = NULL) {
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
    `data-dt-onclick-col` = onclick_col,
    `data-dt-onclick-idx` = if (!is.null(onclick_idx)) onclick_idx else NULL,
    `data-dt-digits` = as.character(digits),
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
    `data-dt-excel` = on_off(toggles$excel_download %||% FALSE)
  )
}

#' Is the Excel writer available? A seam (not an inline requireNamespace) so
#' tests can mock the openxlsx-missing state (see the dt_download renderUI).
#' @noRd
dt_has_openxlsx <- function() {
  requireNamespace("openxlsx", quietly = TRUE)
}

#' Table-block chrome: the scoped CSS, the search/gear header, and the scroll
#' container, with `inner` (a `<table>` tag or a `uiOutput()` slot) dropped into
#' the scroll wrapper. Rendered ONCE per block -- the gear/search/scroll never
#' rebuild -- so only `inner` (the table) re-renders on a filter or gear edit.
#' The `drilldown-table-structured` class + the Table-1 delta CSS are gated on
#' `structured` here (the one place that knows it), so the flat preview stays
#' plain.
#' @noRd
dt_chrome <- function(elem_id, structured, max_height, inner,
                      search = TRUE, download_slot = NULL,
                      status_slot = NULL) {
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
      # Optional Excel-download control (rendered only when the block toggles it
      # on); sits on the toolbar, outside the gear.
      download_slot
    )
  )

  scroll_style <- if (!is.null(max_height)) {
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
    if (isTRUE(structured)) {
      # Scope the Table-1 typography to `.drilldown-table-structured` (this
      # container carries it). A `<style>` is page-global, and a flat table
      # block shares `.blockr-html-table-container`, so an unscoped delta would
      # leak the medium-weight cells onto a sibling flat table.
      htmltools::tags$style(htmltools::HTML(
        html_table_delta_css(scope = ".drilldown-table-structured")
      ))
    },
    drilldown_table_dep(),
    htmltools::tags$div(
      id = wrapper_id,
      class = paste(
        "blockr-html-table-container drilldown-table-container",
        if (isTRUE(structured)) "drilldown-table-structured" else NULL
      ),
      `data-dt-elem-id` = if (!is.null(elem_id)) elem_id else NULL,
      `data-dt-structured` = if (isTRUE(structured)) "1" else NULL,
      `data-initial-expanded` = if (isTRUE(structured)) "1" else NULL,
      header_div,
      htmltools::tags$div(
        class = "blockr-table-wrapper",
        style = scroll_style,
        inner
      ),
      # Active-filter status line + Reset (chart-footer parity), rendered
      # server-side off the block's filter state (see the block server's
      # dt_status output) so it also survives a board restore.
      status_slot
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
#' @noRd
dt_color_fun <- function(type, domain, palette) {
  lo <- domain[1L]
  hi <- domain[2L]
  hex2rgb <- function(h) grDevices::col2rgb(h)[, 1L]
  # Per-channel r/g/b vectors -> bg hex + contrast-tested fg hex.
  bg_fg <- function(r, g, b) {
    lum <- (0.299 * r + 0.587 * g + 0.114 * b) / 255
    list(bg = grDevices::rgb(r, g, b, maxColorValue = 255),
         fg = ifelse(lum < 0.55, "#ffffff", "#111827"))
  }

  if (identical(type, "diverging")) {
    pal <- palette %||% c("#99000d", "#ffffff", "#08306b")
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
    pal <- palette %||% c("#eef2ff", "#1d4ed8")
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
drilldown_table_dep <- function() {
  htmltools::tagList(
    # Shared blockr.dplyr CSS/JS (gear, popover, rows, Blockr.Select, icons) --
    # same dep names as the chart so they de-dupe on a page with both blocks.
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
    # Shared popover CSS (the dd-* section/row/segmented/add classes) lives in
    # chart.css; the table's gear popover reuses it (de-dups by dep name).
    htmltools::htmlDependency(
      name = "chart-css",
      version = paste0(utils::packageVersion("blockr.viz"), ".33"),
      src = system.file("css", package = "blockr.viz"),
      stylesheet = "chart.css"
    ),
    settings_band_dep(),
    # Shared aggregation vocabulary + gear engine (one dep, one version -- see
    # drilldown_shared_dep()). Before the table JS, which reads both globals.
    drilldown_shared_dep(),
    htmltools::htmlDependency(
      name = "blockr-viz-table",
      # Suffix bumped when the bundled table JS/CSS changes, to bust the
      # version-pinned asset cache (display-option gear toggles).
      version = paste0(utils::packageVersion("blockr.viz"), ".26"),
      src = system.file(package = "blockr.viz"),
      script = "js/table.js",
      stylesheet = "css/table.css"
    )
  )
}

# --- block -------------------------------------------------------------

#' Build arguments metadata for the drill-down table block
#'
#' Registry/LLM metadata so the assistant and MCP can introspect the table
#' block (the chart block has this; the table was previously invisible).
#' @noRd
table_arguments <- function() {
  new_block_args(
    # Aggregation: group by column(s), then one or more summaries. Same vocabulary
    # as the chart, but the table carries a LIST of summaries (mean of AGE AND sum
    # of DOSE at once), so it takes `summaries` rather than the chart's single
    # `metric` + `func`.
    group = new_block_arg(
      paste0(
        "Grouping column(s) to aggregate over. One or more categorical columns ",
        "(nested from outer to inner). Empty = no grouping (a raw row-level ",
        "table)."
      ),
      example = list("SEX", "ARM"),
      type = arg_array(arg_string())
    ),
    summaries = new_block_arg(
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
    rowname = new_block_arg(
      paste0(
        "The single column shown as the row labels (the left-hand stub). Names ",
        "a data column; defaults to the first column."
      ),
      example = "Region",
      type = arg_string()
    ),
    # Populated (explicit value columns array) so the model anchors on setting
    # these to named columns, not leaving them null.
    value = new_block_arg(
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
    color = new_block_arg(
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
    shadings = new_block_arg(
      paste0(
        "Cell value-encoding rules: a list, each entry `{mode, cols}` (same ",
        "shape family as `summaries`). `mode` is \"diverging\" (correlation ",
        "matrices, centred on 0), \"sequential\" (heatmaps), or \"bar\" (an ",
        "in-cell data bar proportional to the value). Empty `cols` on a rule ",
        "= ALL numeric columns except those claimed by another rule (so ",
        "`{bar, [DOSE]}` + `{diverging, []}` bars DOSE and shades everything ",
        "else). Empty list = plain cells. Presentational only; never changes ",
        "data."
      ),
      example = list(
        list(mode = "diverging", cols = list())
      )
    ),
    drill = new_block_arg(
      paste0(
        "Row-click drill-down. Optional; default null = a click is inert ",
        "(drill is opt-in everywhere). RAW table: a column name \u2014 clicking a ",
        "row emits a categorical filter on that column's value (the same ",
        "filter contract as the chart / tile). GROUPED / aggregated table: ",
        "\"auto\" \u2014 clicking a row ANDs its group-key values into the ",
        "downstream filter (the keys are the target; no column choice)."
      ),
      example = "Region",
      type = arg_string()
    ),
    digits = new_block_arg(
      "Decimal places for numeric display. Default 2.",
      example = 2L,
      type = arg_integer()
    )
  )
}

#' Construction guidance for the drill-down table block
#' @noRd
table_guidance <- function() {
  paste(
    "Interactive table (sticky header, client-side sort and search) that can",
    "also act as a click-to-filter control \u2014 the tabular sibling of the",
    "drill-down chart. Two optional capabilities, both off by default:",
    "\n- Coloring: set `cell_color` to a `drilldown_table_color()` spec to give",
    "numeric cells a value-to-background scale (diverging for correlation,",
    "sequential for heatmaps). Presentational only.",
    "\n- Drill-down: set `drill` to a column; clicking a row emits a",
    "categorical filter on that column's value, so downstream blocks filter",
    "\u2014 the same filter contract as the drill-down chart.",
    "\n- Aggregation: set `group` (one or more columns) to collapse the table",
    "in place, one row per group. `summaries` is a list of `{func, cols}`",
    "(count / count_distinct / mean / median / sum / min / max), one entry per",
    "aggregation, so mean(AGE) and sum(DOSE) is two entries. Display-only: a",
    "row click drills to that group's raw rows, and downstream still receives",
    "the raw (filtered) data. Leave `group` empty for a plain row-level table.",
    "\n`rowname`/`value` pick the row-stub and body",
    "columns: set `value` to EXACTLY the columns the user names (e.g.",
    "\"Revenue and Profit\" -> value = [\"Revenue\", \"Profit\"]); leave",
    "them null only when the user does not name specific columns (defaults to",
    "the first column plus the rest)."
  )
}

#' Drilldown Table Block
#'
#' Transform block wrapping [drilldown_table()]. The visible table is
#' rendered from the upstream (pre-filter) data so every row stays
#' clickable; the block's data output is the upstream data filtered by
#' the last click, using the exact same contract as
#' [new_chart_block()] (so existing filter links compose).
#'
#' The block accepts a data frame, or any table-producing object with an
#' [as_annotated_df()] method (e.g. a composer table): the input is coerced
#' through the generic before rendering and filtering, so such objects can be
#' connected directly without an explicit coercion step in between. Plain
#' data frames pass through untouched.
#'
#' @param rowname,value,drill,digits,max_height
#'   Forwarded to the renderer. The block has no in-table title:
#'   the block's own name (card header) serves that role.
#' @param group Optional grouping column(s) for an aggregated table. Default
#'   `NULL` (row-per-observation).
#' @param summaries Summary-column specification for aggregated tables (a list
#'   of aggregations keyed by output column). Default `list()` (none).
#' @param color Categorical identity color ("Color by") -- the SAME argument
#'   as the chart's `color`, applied to rows: one categorical column whose
#'   values tint the rows through the board scale map. `""` = no tint;
#'   `NULL` (default) = a subtle default tint by the rowname column.
#' @param shadings Cell value-encoding rules: a list, each entry
#'   `list(mode, cols)` (mode one of `"diverging"` / `"sequential"` /
#'   `"bar"`; same shape family as `summaries`). Empty `cols` on a rule =
#'   all numeric columns except those claimed by another rule. Default
#'   `list()` (plain cells).
#' @param cell_color,row_color LEGACY, mapped on construction:
#'   `cell_color` (a [drilldown_table_color()] spec) becomes one `shadings`
#'   rule; `row_color` becomes `color`. Kept as formals so saved boards
#'   restore; no gear control, not in the registry.
#' @param filter_column,filter_values Click-filter state (the shared filter
#'   transport names, identical to the chart / tile). Kept as constructor
#'   params so the filter round-trips through save/restore.
#' @param filter_type,filter_range LEGACY, ignored: the table only ever
#'   emits categorical filters (its JS never sets either). Kept as formals so
#'   saved boards restore; serialized as NULL, no gear control, not in the
#'   registry.
#' @param filter_group_cols,filter_group_vals Grouped-table drill filter
#'   state: the aligned group-key columns and values ANDed to filter the raw
#'   input to a clicked row's group. Default `NULL` (no grouped drill active).
#' @param sortable,collapsible,search Logical display toggles (each default
#'   `TRUE`): column sorting, indent-derived collapsible section headers, and
#'   the toolbar search box. Exposed in the block's gear menu.
#' @param excel_download Logical (default `FALSE`). When `TRUE`, an "Excel"
#'   download button appears on the table toolbar; it writes the rendered
#'   (annotated) frame to a styled `.xlsx` via [write_annotated_xlsx()]. Needs
#'   the `openxlsx` package.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#' @return A transform block of class `table_block`.
#' @examplesIf interactive()
#' new_table_block()
#' @export
new_table_block <- function(rowname = NULL,
                                      value = NULL,
                                      group = NULL,
                                      summaries = list(),
                                      color = NULL,
                                      shadings = list(),
                                      cell_color = NULL,
                                      row_color = NULL,
                                      drill = NULL,
                                      digits = 2L,
                                      max_height = "600px",
                                      filter_type = "categorical",
                                      filter_column = NULL,
                                      filter_values = NULL,
                                      filter_range = NULL,
                                      filter_group_cols = NULL,
                                      filter_group_vals = NULL,
                                      sortable = TRUE,
                                      collapsible = TRUE,
                                      search = TRUE,
                                      excel_download = FALSE,
                                      ...) {
  # Legacy args mapped on construction (old saved boards restore through
  # these formals): a cell_color spec reads as one `shadings` rule; row_color
  # reads as `color`. New names win when both are given.
  if (!length(shadings) && !is.null(cell_color)) {
    shadings <- dd_parse_shadings(cell_color)
  }
  if (is.null(color) && !is.null(row_color)) color <- row_color

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        r_rowname    <- shiny::reactiveVal(rowname)
        r_value     <- shiny::reactiveVal(value)
        r_group      <- shiny::reactiveVal(as.character(group))
        r_summaries    <- shiny::reactiveVal(dd_parse_summaries(summaries))
        # `color` = identity tint column (NULL = smart default by rowname,
        # "" = explicitly off); `shadings` = value-encoding rules list.
        r_color    <- shiny::reactiveVal(color)
        r_shadings <- shiny::reactiveVal(dd_parse_shadings(shadings))
        r_drill      <- shiny::reactiveVal(drill)
        r_digits        <- shiny::reactiveVal(digits)
        r_max_height    <- shiny::reactiveVal(max_height)
        r_filter_column <- shiny::reactiveVal(filter_column)
        r_filter_values <- shiny::reactiveVal(filter_values)
        # Grouped-table drill: a row click ANDs the clicked row's group-key
        # values (aligned vectors) to filter the raw input to that group.
        r_filter_group_cols <- shiny::reactiveVal(filter_group_cols)
        r_filter_group_vals <- shiny::reactiveVal(filter_group_vals)
        r_sortable       <- shiny::reactiveVal(isTRUE(sortable))
        r_collapsible    <- shiny::reactiveVal(isTRUE(collapsible))
        r_search         <- shiny::reactiveVal(isTRUE(search))
        r_excel_download <- shiny::reactiveVal(isTRUE(excel_download))

        # Only write a reactiveVal when the value actually changes. JS echoes
        # the full config/filter on any popover change, so a blind set would
        # invalidate (and re-render the table) on every echo -- the
        # "prevent R->JS->R loops" guard the chart block uses. (Mirrors
        # chart-block.R.)
        upd <- function(rv, v) {
          if (!identical(shiny::isolate(rv()), v)) rv(v)
        }
        # Segmented gear toggles emit "on"/"off"; constructor / restore pass a
        # logical. Accept both.
        as_toggle <- function(v) identical(v, "on") || isTRUE(v)

        # What the render paths consume: the input coerced to an annotated
        # data frame (as_annotated_df() is a passthrough for plain data
        # frames). The inner server sees `data` even while `dat_valid` is
        # failing, and a method may refuse a particular value at eval time --
        # so readers below take the tryCatch(ann_data()) + req() route rather
        # than reading `data()` directly: coercion failures then park the
        # render silently and the block condition (from dat_valid / the
        # block expr, which coerces the same way) does the explaining.
        ann_data <- shiny::reactive(as_annotated_df(data()))

        shiny::observeEvent(input$drilldown_table_block_action, {
          msg <- input$drilldown_table_block_action
          if (is.null(msg)) return()
          act <- msg$action %||% "config"
          if (identical(act, "filter")) {
            if (!is.null(msg$filters)) {
              # Grouped drill: [{column, value}, ...] -> aligned vectors, ANDed.
              cols <- vapply(msg$filters, function(f) as.character(f$column),
                             character(1L))
              vals <- vapply(msg$filters, function(f) as.character(f$value),
                             character(1L))
              upd(r_filter_group_cols, cols)
              upd(r_filter_group_vals, vals)
              upd(r_filter_column, NULL)
              upd(r_filter_values, NULL)
            } else {
              upd(r_filter_column, msg$column)
              upd(r_filter_values, msg$values)
              upd(r_filter_group_cols, NULL)
              upd(r_filter_group_vals, NULL)
            }
          } else if (identical(act, "config")) {
            p <- msg$param
            v <- msg$value
            if (identical(p, "shadings")) {
              # JS sends the whole shadings list as a JSON string on any edit
              # (same transport as `summaries`).
              upd(r_shadings, dd_parse_shadings(v))
            } else if (identical(p, "color")) {
              # "" (explicit none from "(none)") is distinct from NULL (unset /
              # smart default), so store "" rather than collapsing to NULL.
              upd(r_color, if (identical(v, "(none)") || !nzchar(v)) "" else v)
            } else if (identical(p, "drill")) {
              upd(r_drill, if (identical(v, "(none)") || !nzchar(v)) NULL else v)
            } else if (identical(p, "group")) {
              upd(r_group, as.character(unlist(v)))
            } else if (identical(p, "summaries")) {
              # JS sends the whole summaries list as a JSON string on any edit.
              upd(r_summaries, dd_parse_summaries(v))
            } else if (identical(p, "digits")) {
              upd(r_digits, as.integer(v))
            } else if (identical(p, "sortable")) {
              upd(r_sortable, as_toggle(v))
            } else if (identical(p, "collapsible")) {
              upd(r_collapsible, as_toggle(v))
            } else if (identical(p, "search")) {
              upd(r_search, as_toggle(v))
            } else if (identical(p, "excel_download")) {
              upd(r_excel_download, as_toggle(v))
            }
          }
        })

        # Split render so a filter (or gear edit) re-renders ONLY the <table>,
        # never the search bar / gear / scroll container -- no whole-panel
        # blank-out, and search text + scroll position survive. `dt_result` is
        # the chrome: it depends only on whether the frame is structured (which
        # is fixed for a given block), so it renders once and is never rebuilt
        # on a filter. `dt_table` is the table body, re-rendered on every data
        # or config change; the gear/drill/colour/digits state travels on the
        # <table>'s data-attributes (see dt_table_attrs()). Mirrors the chart
        # block's "static container + reactive content" shape.
        r_structured <- shiny::reactiveVal(NULL)
        shiny::observe({
          # An unhandled error in a plain observe() is fatal to the Shiny
          # session (client disconnect), unlike a render error which Shiny
          # contains. Never let a coercion or shape/format failure here take
          # down the session -- fall back to a flat layout; the block
          # condition surfaces the actual error as an ordinary in-block
          # message.
          d <- tryCatch(ann_data(), error = function(e) NULL)
          shiny::req(is.data.frame(d))
          upd(r_structured, tryCatch(
            dt_is_structured(fmt_to_wide(d)),
            error = function(e) FALSE
          ))
        })

        output$dt_result <- shiny::renderUI({
          structured <- r_structured()
          shiny::req(!is.null(structured))
          dt_chrome(
            elem_id    = ns("drilldown_table_block"),
            structured = isTRUE(structured),
            max_height = shiny::isolate(r_max_height()),
            inner      = shiny::uiOutput(ns("dt_table")),
            search     = isTRUE(r_search()),
            download_slot = shiny::uiOutput(ns("dt_download"), inline = TRUE),
            status_slot   = shiny::uiOutput(ns("dt_status"))
          )
        })

        # Active-filter status line + Reset (the chart footer's classes, so
        # both read identically). A tiny output of the filter reactiveVals
        # only -- the (potentially heavy) table body never re-renders on a
        # click. Present whenever drill is enabled ("No filter active", chart
        # parity) or a filter is still active (e.g. restored from a saved
        # board); the Reset button is wired by delegation in table.js.
        output$dt_status <- shiny::renderUI({
          gc  <- as.character(unlist(r_filter_group_cols()))
          gv  <- as.character(unlist(r_filter_group_vals()))
          col <- r_filter_column()
          vals <- as.character(unlist(r_filter_values()))
          grouped_on <- length(gc) > 0 && length(gc) == length(gv)
          single_on  <- !is.null(col) && length(vals) > 0
          drill_on   <- !is.null(r_drill()) && nzchar(r_drill())
          if (!grouped_on && !single_on && !drill_on) return(NULL)
          text <- if (grouped_on) {
            paste0("Filtered: ", paste0(gc, " = ", gv, collapse = " & "))
          } else if (single_on) {
            paste0("Filtered: ", col, " = ", paste(vals, collapse = ", "))
          } else {
            "No filter active"
          }
          htmltools::tags$div(
            class = "dd-status-footer",
            htmltools::tags$span(class = "dd-status-text", text),
            if (grouped_on || single_on) {
              htmltools::tags$button(
                type = "button", class = "dd-status-reset", "Reset"
              )
            }
          )
        })

        # Excel download: a control on the chrome toolbar, shown only when the
        # block has `excel_download` on. It writes the rendered (annotated) frame
        # via write_annotated_xlsx() -- same frame, the spreadsheet output.
        # Hand-built download link (the `shiny-download-link` class is what
        # shiny's download binding attaches to) instead of
        # shiny::downloadButton, so it renders as a quiet design-system icon
        # button (table.css) rather than a stock Bootstrap .btn with a
        # FontAwesome icon.
        dl_xlsx_icon <- function() {
          htmltools::HTML(paste0(
            '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ',
            'stroke="currentColor" stroke-width="1.6" stroke-linecap="round" ',
            'stroke-linejoin="round">',
            '<path d="M8 2.5 V10 M4.8 7 L8 10.2 L11.2 7"/>',
            '<path d="M2.5 11.5 V12.8 A1.2 1.2 0 0 0 3.7 14 H12.3 ',
            'A1.2 1.2 0 0 0 13.5 12.8 V11.5"/></svg>'
          ))
        }
        output$dt_download <- shiny::renderUI({
          if (!isTRUE(r_excel_download())) return(NULL)
          if (!dt_has_openxlsx()) {
            # The user just checked the gear's "Excel export" pill: silently
            # rendering NOTHING reads as a broken toggle. Show the button
            # disabled, with the why on the tooltip (blockr-dl-xlsx--off:
            # muted + cursor not-allowed, NOT pointer-events none, so the
            # tooltip still shows). No shiny-download-link class -- there is
            # no handler to bind.
            return(htmltools::tags$a(
              class = "blockr-dl-xlsx blockr-dl-xlsx--off",
              title = "Excel export requires the openxlsx package",
              `aria-label` =
                "Excel export unavailable: requires the openxlsx package",
              `aria-disabled` = "true",
              dl_xlsx_icon()
            ))
          }
          htmltools::tags$a(
            id = ns("dl_xlsx"),
            class = "blockr-dl-xlsx shiny-download-link",
            href = "",
            target = "_blank",
            download = NA,
            title = "Download as Excel",
            `aria-label` = "Download as Excel",
            dl_xlsx_icon()
          )
        })
        output$dl_xlsx <- shiny::downloadHandler(
          filename = function() "table.xlsx",
          content  = function(file) {
            write_annotated_xlsx(fmt_to_wide(ann_data()), file)
          }
        )

        # Board scale map (NULL when the board has none / blockr.theme absent) --
        # the same source the drilldown chart reads. Used to color the
        # categorical row-stub, matching the chart's legend.
        board_scale_map <- dd_board_scale_map()

        output$dt_table <- shiny::renderUI({
          d <- tryCatch(ann_data(), error = function(e) NULL)
          shiny::req(is.data.frame(d))
          # Filter state at render time, ISOLATED: the table body must never
          # re-render on a click (the split-render perf contract) -- the JS
          # keeps the row highlight live between renders, and any fresh
          # render (restore, config edit, new data) re-reads the then-current
          # state here, so the highlight survives those too.
          act <- shiny::isolate(list(
            col   = r_filter_column(),  vals  = r_filter_values(),
            gcols = r_filter_group_cols(), gvals = r_filter_group_vals()
          ))
          # Aggregation is a DISPLAY projection: when `group` is set, draw the
          # summarised frame (one row per group + a measure). The block's data
          # output stays the raw input filtered by the click (see `expr`), and
          # a row click drills on the group keys. No group -> the raw table,
          # exactly as before.
          agg <- dd_table_aggregate(d, r_group(), r_summaries())
          if (isTRUE(agg$aggregated)) {
            gl <- length(agg$group)
            # Grand totals (no group): prepend a "Total" stub so there is always
            # a row label plus at least one metric column to render.
            ad <- agg$data
            if (!gl) {
              ad <- cbind(
                stats::setNames(data.frame("Total", stringsAsFactors = FALSE,
                                           check.names = FALSE), " "),
                ad
              )
            }
            # COLOR applies to the AGGREGATED frame too: shadings resolve
            # against the displayed (aggregated) columns, and "Color by"
            # tints rows whose color column survived the aggregation (a
            # group key; a non-grouped column has no per-row value here and
            # silently yields no tint). Smart default (color NULL) = the
            # first group key, map-bound-only; explicit pick = chart-parity
            # palette fallback (dd_ident_hex).
            arc <- r_color()
            arc_col <- if (is.null(arc)) {
              if (gl) agg$group[1L] else NULL
            } else if (nzchar(arc)) {
              arc
            } else {
              NULL
            }
            return(tryCatch(
              dt_table_tag(
                ad,
                # The displayed frame is a projection; the gear's pickers
                # must keep offering the RAW input schema (group / value /
                # drill choices act on `d`, not on the aggregate).
                gear_cols  = dt_gear_cols_json(d),
                label_col  = if (gl) agg$group[1L] else " ",
                value_cols = if (gl) c(setdiff(agg$group, agg$group[1L]), agg$metric_cols)
                             else agg$metric_cols,
                shadings   = r_shadings(),
                drill      = NULL,
                # Group-keys drill is OPT-IN (checkbox default off, like every
                # drill): the keys wire up only when `drill` is set -- "auto"
                # from the gear checkbox, or any truthy legacy value from a
                # saved board. Empty group_cols -> no row click wiring.
                # (Empty anyway for grand totals -- nothing to key on.)
                group_cols = if (!is.null(r_drill()) && nzchar(r_drill())) {
                  agg$group
                } else {
                  character()
                },
                row_hex    = if (is.null(arc_col)) {
                  NULL
                } else if (is.null(arc)) {
                  dd_row_hex(board_scale_map(), arc_col, ad)
                } else {
                  dd_ident_hex(board_scale_map(), arc_col, ad)
                },
                color      = arc_col,
                group      = r_group(), summaries = r_summaries(),
                digits     = r_digits(),
                sortable    = isTRUE(r_sortable()),
                collapsible = isTRUE(r_collapsible()),
                search      = isTRUE(r_search()),
                excel_download = isTRUE(r_excel_download()),
                active     = act
              ),
              error = function(e) {
                shiny::tags$div(
                  class = "blockr-error", role = "alert",
                  paste0("Table could not be rendered: ", conditionMessage(e))
                )
              }
            ))
          }
          # `color` names the column whose scale-map colors tint the rows
          # (the chart's color aesthetic applied to rows -- "Color by").
          # NULL (unset) = smart default: use the row-stub column. "" =
          # explicitly off (gear "(none)"). A name = that column.
          rc <- r_color()
          rc_col <- if (is.null(rc)) {
            r_rowname() %||% names(d)[1L]
          } else if (nzchar(rc)) {
            rc
          } else {
            NULL
          }
          # Contain any render-time failure (formatting/spread/colour) and
          # show it ON THE PAGE as a red in-block bar instead of letting it
          # escape -- reusing the `blockr-error` style of the framework's
          # condition bar so it reads like an ordinary block error, never a
          # session crash. (Tactical guard; see blockr.core #199 / the design
          # motivation for the first-class side-effect-render seam that would
          # route this through server$conditions() automatically.)
          tryCatch(
            dt_table_tag(
              d,
              label_col  = r_rowname(),
              value_cols = r_value(),
              shadings   = r_shadings(),
              drill      = r_drill(),
              digits     = r_digits(),
              # Explicit "Color by" pick -> dd_ident_hex (scale map first,
              # palette fallback when unbound: chart parity). Smart default
              # (color NULL -> stub column) stays MAP-BOUND-ONLY: an unbound,
              # usually unique, rowname would rainbow every table by default.
              row_hex    = if (is.null(rc_col)) {
                NULL
              } else if (is.null(rc)) {
                dd_row_hex(board_scale_map(), rc_col, d)
              } else {
                dd_ident_hex(board_scale_map(), rc_col, d)
              },
              color      = rc_col,
              group      = r_group(), summaries = r_summaries(),
              sortable    = isTRUE(r_sortable()),
              collapsible = isTRUE(r_collapsible()),
              search      = isTRUE(r_search()),
              excel_download = isTRUE(r_excel_download()),
              active     = act
            ),
            error = function(e) {
              shiny::tags$div(
                class = "blockr-error", role = "alert",
                paste0("Table could not be rendered: ", conditionMessage(e))
              )
            }
          )
        })

        list(
          # The expr coerces before filtering, so the block's data output (and
          # the drill filter) is always the annotated data frame -- one
          # downstream contract whether the input was a plain data frame
          # (passthrough) or a table-producing object such as a composer
          # table. Self-qualified so the emitted code stands alone.
          expr = shiny::reactive({
            gc <- r_filter_group_cols()
            gv <- r_filter_group_vals()
            col  <- r_filter_column()
            vals <- r_filter_values()
            if (length(gc) && length(gc) == length(gv)) {
              # Grouped drill: AND each clicked group key (raw input -> members).
              cond <- dd_group_filter_call(gc, gv)
              blockr.core::bbquote(
                dplyr::filter(blockr.viz::as_annotated_df(.(data)), .(cond)),
                list(cond = cond)
              )
            } else if (is.null(col) || is.null(vals) || length(vals) == 0) {
              blockr.core::bbquote(
                dplyr::filter(blockr.viz::as_annotated_df(.(data)), TRUE)
              )
            } else if (length(vals) == 1) {
              blockr.core::bbquote(
                dplyr::filter(
                  blockr.viz::as_annotated_df(.(data)),
                  .data[[.(col)]] == .(val)
                ),
                list(col = col, val = vals[[1]])
              )
            } else {
              blockr.core::bbquote(
                dplyr::filter(
                  blockr.viz::as_annotated_df(.(data)),
                  .data[[.(col)]] %in% .(vals)
                ),
                list(col = col, vals = vals)
              )
            }
          }),
          state = list(
            rowname       = r_rowname,
            value         = r_value,
            group         = r_group,
            summaries       = r_summaries,
            color         = r_color,
            shadings      = r_shadings,
            # Legacy formals (mapped into color/shadings on construction):
            # serialized as NULL so restored boards re-enter through the new
            # args; blockr.core requires every ctor formal in the state.
            cell_color    = function() NULL,
            row_color     = function() NULL,
            drill      = r_drill,
            digits        = r_digits,
            max_height    = r_max_height,
            filter_column = r_filter_column,
            filter_values = r_filter_values,
            # Legacy formals (never set by the table's JS -- it only emits
            # categorical filters): serialized as NULL, kept as ctor formals
            # so old saved boards restore.
            filter_type   = function() NULL,
            filter_range  = function() NULL,
            filter_group_cols = r_filter_group_cols,
            filter_group_vals = r_filter_group_vals,
            sortable       = r_sortable,
            collapsible    = r_collapsible,
            search         = r_search,
            excel_download = r_excel_download
          )
        )
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(shiny::uiOutput(ns("dt_result")))
    },
    # Shared input contract (see validate_annotated_df_input): contract
    # check only -- dispatch lookup, never the (possibly costly) coercion
    # itself. A method that exists but refuses this particular value errors
    # at eval time instead; core surfaces both the same way.
    dat_valid = validate_annotated_df_input,
    allow_empty_state = c("rowname", "value", "group", "summaries", "color",
      "shadings", "cell_color", "row_color", "drill", "filter_column",
      "filter_values", "filter_type", "filter_range",
      "filter_group_cols", "filter_group_vals"),
    external_ctrl = c("rowname", "value", "group", "summaries",
      "color", "shadings", "drill",
      "digits", "max_height",
      "filter_column", "filter_values",
      "filter_group_cols", "filter_group_vals",
      "sortable", "collapsible", "search", "excel_download"),
    expr_type = "bquoted",
    class = "table_block",
    ...
  )
}

# NB: no block_ui / block_output overrides. The styled table renders in the
# Controls pane (the `ui =` function above, via uiOutput("dt_result")), and the
# block falls back to the transform_block defaults for the output pane -- so the
# Preview pane shows the filtered passthrough data frame as a DT, exactly like
# new_chart_block(). (They were previously nulled out, which left the
# Preview pane blank.)
