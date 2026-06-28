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
#' convention that [summary_table()] emits — a tidy `.fmt` frame (numeric
#' columns + a per-row `.fmt` template + `.group`), or the already-wide
#' display grid with `.section_*` / `.var` / `.label` / `.indent` columns —
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
    inner      = dt_table_tag(data, label_col, value_cols, color, drill, digits,
                              row_hex = row_hex, row_color = row_color)
  )
}

#' Build just the `<table>` (thead + tbody) plus the data-attributes the table
#' JS reads off it (drill onclick col/idx, colour mode, digits).
#'
#' Split from the chrome (`dt_chrome()`: search bar, gear, scroll container) so
#' a data or config change re-renders ONLY the table — the chrome renders once
#' and is never rebuilt, so there is no whole-panel blank-out and the search
#' text / scroll position survive a filter. See the block server's
#' `dt_result` (chrome) / `dt_table` (this) output split.
#' @noRd
dt_table_tag <- function(data, label_col = NULL, value_cols = NULL,
                         color = NULL, drill = NULL, digits = 2L,
                         row_hex = NULL, row_color = NULL,
                         sortable = TRUE, collapsible = TRUE, search = TRUE,
                         excel_download = FALSE) {
  data <- fmt_to_wide(data)
  # Display-option states (sortable / collapsible / search / excel) ride on the
  # <table> as data-attributes so the gear popover reads their current value;
  # the renderer honours `sortable` / `collapsible` here, while `search` /
  # `excel_download` are realized by the chrome (dt_chrome / dt_download).
  toggles <- list(sortable = sortable, collapsible = collapsible,
                  search = search, excel_download = excel_download)
  if (dt_is_structured(data)) {
    return(dt_table_tag_structured(data, drill, color, digits, toggles))
  }

  if (is.null(label_col)) label_col <- names(data)[1L]
  if (is.null(value_cols)) value_cols <- setdiff(names(data), label_col)
  value_cols <- intersect(value_cols, names(data))

  color_mode <- if (is.null(color)) "off" else color$type

  if (nrow(data) == 0L || !label_col %in% names(data) ||
      length(value_cols) == 0L) {
    return(dt_table_attrs(dt_message_table(), NULL, NULL, color_mode, digits,
                          row_color = row_color, toggles = toggles))
  }

  # ---- cell visuals: heatmap shading OR data bars ---------------------
  # `color` is a drilldown_table_color() spec. `color$columns` names which
  # numeric columns get the treatment; empty/NULL = ALL numeric columns
  # (a rule re-resolved here, so it survives upstream schema changes). A
  # picked column that no longer exists is dropped (fail-soft: that column
  # just renders plain). Normalization follows the mode: `bar` is
  # per-column on absolute magnitude; diverging/sequential share one pooled
  # domain across the target columns (so a correlation matrix reads on one
  # scale).
  bar_mode  <- FALSE
  bar_fill  <- NULL
  bar_max   <- NULL    # named per-column abs-max for bars
  cell_bg   <- NULL    # shared value->color closure for diverging/sequential
  targets   <- character()
  if (!is.null(color)) {
    num_cols <- value_cols[vapply(data[value_cols], is.numeric, logical(1L))]
    picked   <- color$columns
    targets  <- if (length(picked)) intersect(picked, num_cols) else num_cols
    if (length(targets)) {
      if (identical(color$type, "bar")) {
        bar_mode <- TRUE
        bar_fill <- (color$palette %||% "rgba(37, 99, 235, 0.22)")[1L]
        bar_max  <- vapply(targets, function(cn) {
          v <- data[[cn]][is.finite(data[[cn]])]
          if (length(v)) max(abs(v)) else 0
        }, numeric(1L))
      } else {
        vals <- unlist(data[targets], use.names = FALSE)
        vals <- vals[is.finite(vals)]
        dom  <- color$domain
        if (is.null(dom) && length(vals)) {
          dom <- if (identical(color$type, "diverging")) {
            m <- max(abs(vals))   # symmetric around 0 (white at 0)
            c(-m, m)
          } else {
            range(vals)
          }
        }
        if (!is.null(dom) && length(vals) && diff(range(dom)) > 0) {
          cell_bg <- dt_color_fun(color$type, dom, color$palette)
        }
      }
    }
  }

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
  # dominated render time — ~1 s for the full frame, the source of the
  # "drilldown filter takes ~2 s" lag. Column-vectorized string assembly
  # is ~100x faster and emits identical markup (inter-tag whitespace
  # aside). Each column is formatted as a vector; per-element `format()`
  # is kept (not a single vectorized call) because vectorized `format()`
  # aligns decimals across the column (1.5 -> "1.50"), which would change
  # the displayed values.
  # Text content uses htmltools' own escaper (the same one `tags$td()`
  # applies to a text child), so escaping is byte-identical: & < > are
  # escaped, quotes are not.
  esc <- function(x) htmltools::htmlEscape(as.character(x), attribute = FALSE)

  col_cells <- vector("list", length(value_cols))
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
        vapply(vk, function(v) {
          format(round(as.numeric(v), digits), nsmall = 0L, trim = TRUE)
        }, character(1L))
      } else {
        as.character(vk)
      }
      style <- ""
      if (num_flag[j] && value_cols[j] %in% targets) {
        if (bar_mode) {
          # Data bar: left-anchored gradient, width = |v| / column-abs-max.
          # A CSS gradient string keeps the vectorized single-HTML() render
          # (no per-cell DOM node). Text reads on top of the fill.
          style <- dt_bar_style(as.numeric(vk), bar_max[[value_cols[j]]], bar_fill)
        } else if (!is.null(cell_bg)) {
          style <- vapply(as.numeric(vk), function(v) {
            bg <- cell_bg(v)
            paste0(" style=\"background:", bg$bg, ";color:", bg$fg, ";\"")
          }, character(1L))
        }
      }
      out_j[keep] <- paste0("<td class=\"", td_cls, "\"", style, ">",
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
  # indicator cell — which needs the sort/drill column-index map to skip it.
  stub_lbl <- esc(data[[label_col]])
  if (!is.null(row_hex) && length(row_hex) == nrow(data)) {
    bar <- ifelse(
      is.na(row_hex), "",
      paste0(" style=\"box-shadow:inset 3px 0 0 0 ", row_hex, ";\"")
    )
    stub_cells <- paste0("<td class=\"blockr-stub blockr-row-bar\"", bar, ">",
                         stub_lbl, "</td>")
  } else {
    stub_cells <- paste0("<td class=\"blockr-stub\">", stub_lbl, "</td>")
  }
  row_inner  <- do.call(paste0, c(list(stub_cells), col_cells))
  rows_html  <- paste0("<tr class=\"blockr-data-row\">", row_inner, "</tr>",
                       collapse = "")
  tbody <- htmltools::tags$tbody(htmltools::HTML(rows_html))

  table_tag <- htmltools::tags$table(class = "blockr-table", thead, tbody)
  onclick <- dt_onclick(drill, c(label_col, value_cols))
  dt_table_attrs(table_tag, onclick$col, onclick$idx, color_mode, digits,
                 row_color = row_color,
                 num_cols = value_cols[num_flag],
                 color_cols = if (!is.null(color)) color$columns else NULL,
                 toggles = toggles)
}

# ---------------------------------------------------------------------------
# Structured ("Table 1") rendering — section nesting, indents, spanners.
# Reuses html_table()'s thead / tbody builders so the structure matches the
# static html_table renderer exactly; only the surrounding chrome (the
# table-block wrapper, gear, drill/colour attributes) differs.
# ---------------------------------------------------------------------------

#' Does this (already wide) frame carry the dotted-column structure?
#'
#' True when it has any row-side section column (`.section_*` / `.var`) OR a
#' `.label` stub OR an `.indent` column — the signals [summary_table()] emits
#' and [html_table()] renders. A plain flat frame has none of these.
#' @noRd
dt_is_structured <- function(data) {
  any(grepl("^\\.(section_\\d+|var|label|indent)$", names(data)))
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
dt_table_tag_structured <- function(data, drill, color, digits, toggles = NULL) {
  sortable    <- toggles$sortable %||% TRUE
  collapsible <- toggles$collapsible %||% TRUE
  all_section_cols <- grep("^\\.(section_\\d+|var)$", names(data), value = TRUE)
  # Empty .section_*/.var columns draw no "(missing)" header, but must still be
  # excluded from the data cells (use all_section_cols below) or they leak in
  # as a literal "—" column.
  section_cols <- nonempty_section_cols(data, all_section_cols)
  stub_col     <- if (".label" %in% names(data)) ".label" else NULL
  styling_cols <- intersect(c(".indent", ".strong", ".emph"), names(data))
  data_cols    <- setdiff(names(data),
                          c(all_section_cols, stub_col, styling_cols))

  if (length(data_cols) == 0L || nrow(data) == 0L) {
    return(dt_table_attrs(dt_message_table(), NULL, NULL, "off", digits,
                          toggles = toggles))
  }

  thead <- build_html_thead(data, data_cols, stub_col, stub_sortable = FALSE,
                            sortable = sortable)
  tbody <- build_html_tbody(data, section_cols, stub_col, data_cols,
                            styling_cols = styling_cols,
                            collapsible = collapsible)

  table_tag <- htmltools::tags$table(class = "blockr-table", thead, tbody)

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
  dt_table_attrs(table_tag, onclick$col, onclick$idx, "off", digits,
                 toggles = toggles)
}

#' Cell-visual spec for [drilldown_table()]
#'
#' Describes how numeric cells are decorated. Three modes, all purely
#' presentational (the data is never changed):
#'
#' - `"diverging"` — a value-to-background scale centred on 0 with a
#'   symmetric domain (correlation matrices: -1 white-at-0 +1).
#' - `"sequential"` — a low-to-high background ramp (heatmaps).
#' - `"bar"` — an in-cell horizontal data bar proportional to the value
#'   (e.g. "patients with most adverse events"), normalised per column on
#'   absolute magnitude and left-anchored.
#'
#' @param type `"diverging"`, `"sequential"`, or `"bar"`.
#' @param domain `NULL` to infer from the data, or `c(min, max)`.
#'   Ignored for `"bar"` (always per-column). For `"diverging"` the
#'   inferred domain is symmetric around 0.
#' @param palette `NULL` for type defaults, or a character vector of
#'   colors — length 3 for diverging (low/mid/high), length 2 for
#'   sequential (low/high), length 1 for the bar fill.
#' @param columns `NULL`/empty to apply to **all** numeric columns (a
#'   rule, re-resolved against whatever data arrives — survives upstream
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
    # share the lower row (label left, arrow right) — like the html
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

#' @noRd
dt_message_table <- function(msg = "No data") {
  htmltools::tags$table(
    class = "blockr-table",
    htmltools::tags$tbody(
      htmltools::tags$tr(htmltools::tags$td(class = "blockr-data", msg))
    )
  )
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
                           color_mode, digits, row_color = NULL,
                           num_cols = NULL, color_cols = NULL,
                           toggles = NULL) {
  on_off <- function(x) if (isTRUE(x)) "on" else "off"
  htmltools::tagAppendAttributes(
    table_tag,
    `data-dt-onclick-col` = onclick_col,
    `data-dt-onclick-idx` = if (!is.null(onclick_idx)) onclick_idx else NULL,
    `data-dt-color-mode` = color_mode,
    `data-dt-digits` = as.character(digits),
    `data-dt-row-color` = row_color %||% "",
    # Numeric columns the gear may offer for the colour/bar scope picker, and
    # the currently-scoped subset ("" = all numeric). Comma-joined.
    `data-dt-num-cols` = paste(num_cols %||% character(), collapse = ","),
    `data-dt-color-cols` = paste(color_cols %||% character(), collapse = ","),
    # Display-option toggles the gear reads back (default ON, except export).
    `data-dt-sortable` = on_off(toggles$sortable %||% TRUE),
    `data-dt-collapsible` = on_off(toggles$collapsible %||% TRUE),
    `data-dt-search` = on_off(toggles$search %||% TRUE),
    `data-dt-excel` = on_off(toggles$excel_download %||% FALSE)
  )
}

#' Table-block chrome: the scoped CSS, the search/gear header, and the scroll
#' container, with `inner` (a `<table>` tag or a `uiOutput()` slot) dropped into
#' the scroll wrapper. Rendered ONCE per block — the gear/search/scroll never
#' rebuild — so only `inner` (the table) re-renders on a filter or gear edit.
#' The `drilldown-table-structured` class + the Table-1 delta CSS are gated on
#' `structured` here (the one place that knows it), so the flat preview stays
#' plain.
#' @noRd
dt_chrome <- function(elem_id, structured, max_height, inner,
                      search = TRUE, download_slot = NULL) {
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
    # it only for structured tables — a flat data table should match the canonical
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
      )
    )
  )
}

#' Per-cell inline style for a data bar: a left-anchored CSS gradient whose
#' width is `|v| / mx * 100%` (absolute-magnitude normalisation, no center
#' baseline — matches the crossfilter block). Vectorised over the column so the
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

#' @noRd
dt_color_fun <- function(type, domain, palette) {
  lo <- domain[1L]
  hi <- domain[2L]
  hex2rgb <- function(h) grDevices::col2rgb(h)[, 1L]
  lerp <- function(a, b, t) round(a + (b - a) * t)
  lum <- function(rgb) {
    (0.299 * rgb[1L] + 0.587 * rgb[2L] + 0.114 * rgb[3L]) / 255
  }
  to_hex <- function(rgb) {
    grDevices::rgb(rgb[1L], rgb[2L], rgb[3L], maxColorValue = 255)
  }

  if (identical(type, "diverging")) {
    pal <- palette %||% c("#99000d", "#ffffff", "#08306b")
    c1 <- hex2rgb(pal[1L])
    c2 <- hex2rgb(pal[2L])
    c3 <- hex2rgb(pal[3L])
    # Diverging is centred on 0 (white-at-zero); the domain is symmetric around
    # 0 (set at inference). A meaningful zero is what "diverging" means — for a
    # correlation matrix this puts white at r = 0 and gives +/- equal saturation.
    mid <- 0
    function(v) {
      v <- max(min(v, hi), lo)
      if (v <= mid) {
        t <- if (mid == lo) 0 else (v - lo) / (mid - lo)
        rgb <- c(lerp(c1[1L], c2[1L], t), lerp(c1[2L], c2[2L], t),
                 lerp(c1[3L], c2[3L], t))
      } else {
        t <- if (hi == mid) 0 else (v - mid) / (hi - mid)
        rgb <- c(lerp(c2[1L], c3[1L], t), lerp(c2[2L], c3[2L], t),
                 lerp(c2[3L], c3[3L], t))
      }
      list(bg = to_hex(rgb), fg = if (lum(rgb) < 0.55) "#ffffff" else "#111827")
    }
  } else {
    pal <- palette %||% c("#eef2ff", "#1d4ed8")
    c1 <- hex2rgb(pal[1L])
    c2 <- hex2rgb(pal[2L])
    function(v) {
      v <- max(min(v, hi), lo)
      t <- if (hi == lo) 0 else (v - lo) / (hi - lo)
      rgb <- c(lerp(c1[1L], c2[1L], t), lerp(c1[2L], c2[2L], t),
               lerp(c1[3L], c2[3L], t))
      list(bg = to_hex(rgb), fg = if (lum(rgb) < 0.55) "#ffffff" else "#111827")
    }
  }
}

#' @noRd
drilldown_table_dep <- function() {
  htmltools::tagList(
    # Shared blockr.dplyr CSS/JS (gear, popover, rows, Blockr.Select, icons) —
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
      version = paste0(utils::packageVersion("blockr.viz"), ".24"),
      src = system.file("css", package = "blockr.viz"),
      stylesheet = "chart.css"
    ),
    htmltools::htmlDependency(
      name = "blockr-viz-table",
      # Suffix bumped when the bundled table JS/CSS changes, to bust the
      # version-pinned asset cache (display-option gear toggles).
      version = paste0(utils::packageVersion("blockr.viz"), ".2"),
      src = system.file(package = "blockr.viz"),
      # drilldown-config.js (the shared engine) must load before the table JS.
      script = c("js/drilldown-config.js", "js/table.js"),
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
  structure(
    c(
      rowname = paste0(
        "The single column shown as the row labels (the left-hand stub). Names ",
        "a data column; defaults to the first column."
      ),
      values = paste0(
        "The columns rendered as the table body (the data cells). When the user ",
        "names specific value/measure columns, set this to EXACTLY those ",
        "columns -- do not leave it null. Leave null only to mean 'all columns ",
        "except `rowname`'."
      ),
      cell_color = paste0(
        "Cell visuals: a `drilldown_table_color()` spec, or null for a plain ",
        "table. NOTE this is a SPEC object, not a column name (unlike the ",
        "drill-down chart's `color`). `type` is \"diverging\" (correlation ",
        "matrices, centred on 0), \"sequential\" (heatmaps), or \"bar\" (an ",
        "in-cell data bar proportional to the value, e.g. counts of adverse ",
        "events). `columns` restricts the effect to named columns; omit it to ",
        "apply to all numeric columns. Presentational only; never changes data."
      ),
      drill = paste0(
        "Column a row click filters downstream on. Optional; default null = a ",
        "click is inert. When set, clicking a row emits a categorical filter ",
        "on that column's value for the row \u2014 the same filter contract as the ",
        "drill-down chart."
      ),
      digits = "Decimal places for numeric display. Default 2."
    ),
    examples = list(
      # Populated (label_col + explicit value_cols array + drill) so the model
      # anchors on setting these to named columns, not leaving them null.
      rowname = "Region", values = list("Revenue", "Profit"),
      cell_color = NULL, drill = "Region",
      digits = 2L
    ),
    prompt = paste(
      "Interactive table (sticky header, client-side sort and search) that can",
      "also act as a click-to-filter control \u2014 the tabular sibling of the",
      "drill-down chart. Two optional capabilities, both off by default:",
      "\n- Coloring: set `cell_color` to a `drilldown_table_color()` spec to give",
      "numeric cells a value-to-background scale (diverging for correlation,",
      "sequential for heatmaps). Presentational only.",
      "\n- Drill-down: set `drill` to a column; clicking a row emits a",
      "categorical filter on that column's value, so downstream blocks filter",
      "\u2014 the same filter contract as the drill-down chart.",
      "\n`rowname`/`values` pick the row-stub and body",
      "columns: set `values` to EXACTLY the columns the user names (e.g.",
      "\"Revenue and Profit\" -> values = [\"Revenue\", \"Profit\"]); leave",
      "them null only when the user does not name specific columns (defaults to",
      "the first column plus the rest)."
    )
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
#' @param rowname,values,cell_color,drill,digits,max_height
#'   Forwarded to [drilldown_table()]. The block has no in-table title:
#'   the block's own name (card header) serves that role.
#' @param row_color Optional per-row colouring spec applied to the whole
#'   row rather than individual cells. Default `NULL` (off).
#' @param filter_type,filter_column,filter_values,filter_range Click
#'   filter state (kept for contract parity with the drilldown chart;
#'   `filter_range` is unused by the table).
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
                                      values = NULL,
                                      cell_color = NULL,
                                      row_color = NULL,
                                      drill = NULL,
                                      digits = 2L,
                                      max_height = "600px",
                                      filter_type = "categorical",
                                      filter_column = NULL,
                                      filter_values = NULL,
                                      filter_range = NULL,
                                      sortable = TRUE,
                                      collapsible = TRUE,
                                      search = TRUE,
                                      excel_download = FALSE,
                                      ...) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        r_rowname    <- shiny::reactiveVal(rowname)
        r_values     <- shiny::reactiveVal(values)
        r_cell_color    <- shiny::reactiveVal(cell_color)
        r_row_color  <- shiny::reactiveVal(row_color)
        r_drill      <- shiny::reactiveVal(drill)
        r_digits        <- shiny::reactiveVal(digits)
        r_max_height    <- shiny::reactiveVal(max_height)
        r_filter_type   <- shiny::reactiveVal(filter_type)
        r_filter_column <- shiny::reactiveVal(filter_column)
        r_filter_values <- shiny::reactiveVal(filter_values)
        r_filter_range  <- shiny::reactiveVal(filter_range)
        r_sortable       <- shiny::reactiveVal(isTRUE(sortable))
        r_collapsible    <- shiny::reactiveVal(isTRUE(collapsible))
        r_search         <- shiny::reactiveVal(isTRUE(search))
        r_excel_download <- shiny::reactiveVal(isTRUE(excel_download))

        # Only write a reactiveVal when the value actually changes. JS echoes
        # the full config/filter on any popover change, so a blind set would
        # invalidate (and re-render the table) on every echo — the
        # "prevent R->JS->R loops" guard the chart block uses. (Mirrors
        # chart-block.R.)
        upd <- function(rv, v) {
          if (!identical(shiny::isolate(rv()), v)) rv(v)
        }
        # Segmented gear toggles emit "on"/"off"; constructor / restore pass a
        # logical. Accept both.
        as_toggle <- function(v) identical(v, "on") || isTRUE(v)

        shiny::observeEvent(input$drilldown_table_block_action, {
          msg <- input$drilldown_table_block_action
          if (is.null(msg)) return()
          act <- msg$action %||% "config"
          if (identical(act, "filter")) {
            upd(r_filter_column, msg$column)
            upd(r_filter_values, msg$values)
            upd(r_filter_type, "categorical")
            upd(r_filter_range, NULL)
          } else if (identical(act, "config")) {
            p <- msg$param
            v <- msg$value
            if (identical(p, "color_mode")) {
              cur <- shiny::isolate(r_cell_color()) %||% cell_color
              if (identical(v, "off")) {
                upd(r_cell_color, NULL)
              } else {
                # Switching mode keeps the current column scope; domain/palette
                # only carry over when the type is unchanged (they are
                # type-specific). columns = NULL/empty means "all numeric".
                same <- !is.null(cur) && identical(cur$type, v)
                upd(r_cell_color, drilldown_table_color(
                  v,
                  domain  = if (same) cur$domain  else NULL,
                  palette = if (same) cur$palette else NULL,
                  columns = if (!is.null(cur)) cur$columns else NULL
                ))
              }
            } else if (identical(p, "color_columns")) {
              # The column-scope multi-select. Empty selection = NULL = "all
              # numeric" (a rule, not a snapshot — self-heals on schema change).
              # Ignored when no colour mode is active.
              cur <- shiny::isolate(r_cell_color())
              if (!is.null(cur)) {
                cols <- as.character(unlist(v))
                if (length(cols) == 0L ||
                    (length(cols) == 1L && !nzchar(cols))) {
                  cols <- NULL
                }
                upd(r_cell_color, drilldown_table_color(
                  cur$type, domain = cur$domain, palette = cur$palette,
                  columns = cols
                ))
              }
            } else if (identical(p, "drill")) {
              upd(r_drill, if (identical(v, "(none)") || !nzchar(v)) NULL else v)
            } else if (identical(p, "row_color")) {
              # "" (explicit none from "(none)") is distinct from NULL (unset /
              # smart default), so store "" rather than collapsing to NULL.
              upd(r_row_color, if (identical(v, "(none)") || !nzchar(v)) "" else v)
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
        # never the search bar / gear / scroll container — no whole-panel
        # blank-out, and search text + scroll position survive. `dt_result` is
        # the chrome: it depends only on whether the frame is structured (which
        # is fixed for a given block), so it renders once and is never rebuilt
        # on a filter. `dt_table` is the table body, re-rendered on every data
        # or config change; the gear/drill/colour/digits state travels on the
        # <table>'s data-attributes (see dt_table_attrs()). Mirrors the chart
        # block's "static container + reactive content" shape.
        r_structured <- shiny::reactiveVal(NULL)
        shiny::observe({
          d <- data()
          shiny::req(is.data.frame(d))
          # An unhandled error in a plain observe() is fatal to the Shiny
          # session (client disconnect), unlike a render error which Shiny
          # contains. Never let a shape/format failure here take down the
          # session — fall back to a flat layout; the render below surfaces
          # the actual error as an ordinary in-block message.
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
            download_slot = shiny::uiOutput(ns("dt_download"), inline = TRUE)
          )
        })

        # Excel download: a button on the chrome toolbar, shown only when the
        # block has `excel_download` on. It writes the rendered (annotated) frame
        # via write_annotated_xlsx() — same frame, the spreadsheet output.
        output$dt_download <- shiny::renderUI({
          if (!isTRUE(r_excel_download())) return(NULL)
          if (!requireNamespace("openxlsx", quietly = TRUE)) return(NULL)
          shiny::downloadButton(
            ns("dl_xlsx"),
            label = NULL,
            class = "blockr-dl-xlsx",
            icon  = shiny::icon("download"),
            title = "Download as Excel"
          )
        })
        output$dl_xlsx <- shiny::downloadHandler(
          filename = function() "table.xlsx",
          content  = function(file) {
            write_annotated_xlsx(fmt_to_wide(data()), file)
          }
        )

        # Board scale map (NULL when the board has none / blockr.theme absent) —
        # the same source the drilldown chart reads. Used to color the
        # categorical row-stub, matching the chart's legend.
        board_scale_map <- dd_board_scale_map()

        output$dt_table <- shiny::renderUI({
          d <- data()
          shiny::req(is.data.frame(d))
          # `row_color` names the column whose scale-map colors tint the rows
          # (like ggplot's aes(color = SEX)). NULL (unset) = smart default: use
          # the row-stub column. "" = explicitly off (gear "(none)"). A name =
          # that column.
          rc <- r_row_color()
          rc_col <- if (is.null(rc)) {
            r_rowname() %||% names(d)[1L]
          } else if (nzchar(rc)) {
            rc
          } else {
            NULL
          }
          # Contain any render-time failure (formatting/spread/colour) and
          # show it ON THE PAGE as a red in-block bar instead of letting it
          # escape — reusing the `blockr-error` style of the framework's
          # condition bar so it reads like an ordinary block error, never a
          # session crash. (Tactical guard; see blockr.core #199 / the design
          # motivation for the first-class side-effect-render seam that would
          # route this through server$conditions() automatically.)
          tryCatch(
            dt_table_tag(
              d,
              label_col  = r_rowname(),
              value_cols = r_values(),
              color      = r_cell_color(),
              drill      = r_drill(),
              digits     = r_digits(),
              row_hex    = if (is.null(rc_col)) NULL else
                             dd_row_hex(board_scale_map(), rc_col, d),
              row_color  = rc_col,
              sortable    = isTRUE(r_sortable()),
              collapsible = isTRUE(r_collapsible()),
              search      = isTRUE(r_search()),
              excel_download = isTRUE(r_excel_download())
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
          expr = shiny::reactive({
            col  <- r_filter_column()
            vals <- r_filter_values()
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
            rowname       = r_rowname,
            values        = r_values,
            cell_color    = r_cell_color,
            row_color     = r_row_color,
            drill      = r_drill,
            digits        = r_digits,
            max_height    = r_max_height,
            filter_type   = r_filter_type,
            filter_column = r_filter_column,
            filter_values = r_filter_values,
            filter_range  = r_filter_range,
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
    dat_valid = function(data) {
      if (!is.data.frame(data)) stop("Input must be a data frame")
    },
    allow_empty_state = c("rowname", "values", "cell_color", "row_color",
      "drill", "filter_column", "filter_values", "filter_range"),
    external_ctrl = c("rowname", "values", "cell_color", "row_color", "drill",
      "digits", "max_height", "filter_type",
      "filter_column", "filter_values", "filter_range",
      "sortable", "collapsible", "search", "excel_download"),
    expr_type = "bquoted",
    class = "table_block",
    ...
  )
}

# NB: no block_ui / block_output overrides. The styled table renders in the
# Controls pane (the `ui =` function above, via uiOutput("dt_result")), and the
# block falls back to the transform_block defaults for the output pane — so the
# Preview pane shows the filtered passthrough data frame as a DT, exactly like
# new_chart_block(). (They were previously nulled out, which left the
# Preview pane blank.)
