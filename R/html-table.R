#' HTML Table Renderer for the Table-Blocks Quartet
#'
#' Renders the output of [summary_table()] (pivoted display grids are
#' produced upstream by composing `summarize` with `tidyr::pivot_wider`)
#' as a hand-rolled HTML table with nested row-side section headers,
#' multi-level column spanners, and client-side collapse/expand of
#' sections. Designed as a dashboard-native alternative to
#' [gt_table()] -- static gt is ideal for print / CSR output, while
#' this renderer is tuned for interactive dashboards.
#'
#' The input contract is the same dotted-column wide tibble that
#' [summary_table()] produces:
#'
#' - `.section_1, ..., .section_k` -- nested row-side section columns,
#'   outermost first. `attr(col, "label")` carries the display label.
#' - `.strong` -- logical, bold header rows (variable labels when
#'   `length(vars) > 1`).
#' - `.label` -- innermost per-row identifier (stat name / factor
#'   level). Rendered as the leftmost row-stub column.
#' - Data columns -- names use `|` as nesting delimiter for multi-level
#'   column spanners (e.g. `"KarXT|Week 2"`). `attr(col, "label")` may
#'   carry HTML-wrapped display text for the leaf column header.
#' - Cells are pre-formatted character strings.
#'
#' v1 deliberately supports only the new wide format. Legacy
#' long-format input from `tidy_summary_block` is routed through
#' [gt_table()] instead.
#'
#' @param data A wide tibble following the dotted-column contract.
#' @param title Optional table title (rendered as a `<caption>`).
#' @param caption Optional trailing caption / footnote.
#' @param default_expanded Logical. When `TRUE` (default), all
#'   sections start expanded. When `FALSE`, all level-1 sections start
#'   collapsed and users click to reveal their contents.
#' @param max_height CSS max-height of the scroll container. Default
#'   `"600px"`. Set to `NULL` to disable scrolling.
#'
#' @return A [htmltools::tagList()] containing the scoped style, the
#'   `<table>` element, and the initialisation script. Drop into any
#'   `shiny::renderUI()` / `shiny::htmlOutput()` slot.
#'
#' @examples
#' tbl <- summary_table(iris, vars = "Sepal.Length", by = "Species")
#' html_table(tbl)
#' @export
html_table <- function(data,
                       title = NULL,
                       caption = NULL,
                       default_expanded = TRUE,
                       max_height = "600px") {
  stopifnot(is.data.frame(data))

  # Tidy `.fmt` form (numbers + per-row template + `.group`) -> wide
  # display grid (format-then-spread). No-op on already-wide input.
  data <- fmt_to_wide(data)

  if (all(c("label", "depth") %in% names(data)) &&
      any(c("col_var", "n", "value") %in% names(data))) {
    stop(
      "html_table() does not support legacy long-format input. ",
      "Use gt_table() instead."
    )
  }

  all_section_cols <- grep("^\\.section_\\d+$", names(data), value = TRUE)
  # Only columns that carry a grouping value render as sections; an entirely
  # empty .section_* column draws no "(missing)" header -- but it must still
  # be kept out of the data cells (exclude `all_section_cols`, not the filtered
  # set, from data_cols) or it would leak in as a literal em-dash column.
  section_cols <- nonempty_section_cols(data, all_section_cols)
  stub_col     <- if (".label" %in% names(data)) ".label" else NULL
  styling_cols <- intersect(c(".indent", ".strong", ".emph"), names(data))
  data_cols    <- setdiff(names(data),
                          c(all_section_cols, stub_col, styling_cols))

  wrapper_id <- paste0("blockr-html-table-", sub("^file", "", basename(tempfile(""))))

  thead <- build_html_thead(data, data_cols, stub_col,
                            stub_sortable = FALSE)
  tbody <- build_html_tbody(data, section_cols, stub_col, data_cols,
                            styling_cols = styling_cols)

  # Use the same class names the canonical blockr.ui preview uses so the
  # shared table_preview_css() rules (typography, padding, hover, sort icons)
  # apply to this table without duplication.
  table_tag <- dt_fixed_table_tag(
    thead, tbody,
    structured_colgroup(data, data_cols, stub_col)
  )

  scroll_style <- if (!is.null(max_height)) {
    paste0("max-height:", max_height, ";overflow:auto;")
  } else {
    "overflow:auto;"
  }

  header_div <- htmltools::tags$div(
    class = "blockr-html-table-header",
    htmltools::tags$div(
      class = "blockr-html-table-title",
      if (!is.null(title) && nzchar(title)) title else htmltools::HTML("&nbsp;")
    ),
    htmltools::tags$div(
      class = "blockr-html-table-toolbar",
      htmltools::tags$input(
        type = "search",
        class = "blockr-search",
        placeholder = "Search\u2026",
        `aria-label` = "Search table"
      )
    )
  )

  footer_div <- if (!is.null(caption) && nzchar(caption)) {
    htmltools::tags$div(class = "blockr-html-table-caption", caption)
  } else {
    NULL
  }

  # Pull the canonical shared CSS from blockr.ui, the single home of the
  # table preview. Our delta CSS below adds the section row groups / collapse
  # toggle / search input / title bar / multi-level header rules that the
  # canonical preview doesn't carry. `table_preview_css()` returns a ready
  # <style> tag.
  shared_css <- blockr.ui::table_preview_css()

  htmltools::tagList(
    shared_css,
    htmltools::tags$style(htmltools::HTML(html_table_delta_css())),
    htmltools::tags$div(
      id = wrapper_id,
      class = "blockr-html-table-container",
      `data-initial-expanded` = if (isTRUE(default_expanded)) "1" else "0",
      header_div,
      htmltools::tags$div(
        class = "blockr-table-wrapper",
        style = scroll_style,
        table_tag
      ),
      footer_div
    ),
    htmltools::tags$script(htmltools::HTML(
      gsub("__WRAPPER_ID__", wrapper_id, html_table_js_template(), fixed = TRUE)
    ))
  )
}

# ---------------------------------------------------------------------------
# Server-computed fixed-layout column widths
# ---------------------------------------------------------------------------
# Widths come from blockr.ui::column_widths_px() - the same estimator the
# canonical table preview uses - so the layout never depends on measuring
# the DOM (the retired lockTableWidths() in table.js read 0 whenever it ran
# against a hidden panel, and the search filter never locked at all, so
# columns reflowed live while typing). Multi-level spanner headers rule out
# per-th widths: in fixed layout the FIRST header row sizes the columns and
# that row holds colspan group cells, so the widths ride a <colgroup>.

#' Assemble the fixed-layout blockr table tag
#' @noRd
dt_fixed_table_tag <- function(thead, tbody, colgroup) {
  htmltools::tags$table(
    class = "blockr-table",
    # width stays 100% via the class: a fixed table's used width is
    # max(100%, sum of columns), so narrow tables fill the panel and wide
    # ones overflow-scroll.
    style = "table-layout: fixed;",
    colgroup,
    thead,
    tbody
  )
}

#' <colgroup> from header texts + display strings (leaf-column DOM order)
#' @noRd
dt_colgroup <- function(header_texts, cells,
                        labels = character(length(header_texts)),
                        extra_px = 0L) {
  widths <- blockr.ui::column_widths_px(
    col_names = header_texts,
    col_labels = labels,
    formatted = lapply(cells, as.character)
  ) + extra_px
  htmltools::tags$colgroup(
    lapply(widths, function(w) {
      htmltools::tags$col(style = sprintf("width: %dpx;", w))
    })
  )
}

#' Plain-text header line for width estimation: honour attr(col, "label"),
#' strip HTML, take the widest line of a multi-line label.
#' @noRd
dt_header_text <- function(data, col) {
  lbl <- attr(data[[col]], "label")
  txt <- if (is.character(lbl) && length(lbl) == 1L && nzchar(lbl)) {
    lbl
  } else {
    p <- strsplit(col, "||", fixed = TRUE)[[1L]]
    p[[length(p)]]
  }
  txt <- gsub("<br */?>", "\n", txt)
  txt <- gsub("<[^>]*>", "", txt)
  lines <- trimws(strsplit(txt, "\n", fixed = TRUE)[[1L]])
  if (!length(lines)) return("")
  lines[[which.max(nchar(lines, type = "width"))]]
}

#' Colgroup for the structured (dotted-column) layout shared by
#' html_table() and the structured table block: optional stub + leaf
#' data columns, with an indent allowance on the stub.
#' @noRd
structured_colgroup <- function(data, data_cols, stub_col) {
  header_texts <- vapply(data_cols, function(col) dt_header_text(data, col),
                         character(1L))
  cells <- lapply(data_cols, function(col) as.character(data[[col]]))
  extra <- rep(0L, length(data_cols))
  if (!is.null(stub_col)) {
    header_texts <- c("", header_texts)
    cells <- c(list(as.character(data[[stub_col]])), cells)
    # Stub rows indent 24px base + 16px per level (build_html_tbody) in
    # place of the plain 16px left padding the estimator assumes.
    max_indent <- if (".indent" %in% names(data)) {
      suppressWarnings(max(c(0, as.numeric(data$.indent)), na.rm = TRUE))
    } else {
      0
    }
    extra <- c(8L + 16L * as.integer(max_indent), extra)
  }
  dt_colgroup(header_texts, cells, extra_px = extra)
}

# ---------------------------------------------------------------------------
# Multi-level <thead> builder
# ---------------------------------------------------------------------------

#' @noRd
build_html_thead <- function(data, data_cols, stub_col, stub_sortable = FALSE,
                             sortable = TRUE) {
  # Global off-switch: when sorting is disabled no header is a sort hook.
  stub_sortable <- isTRUE(stub_sortable) && isTRUE(sortable)
  if (length(data_cols) == 0L) {
    parts <- list()
    depths <- integer(0)
    max_depth <- 1L
  } else {
    parts <- strsplit(data_cols, "||", fixed = TRUE)
    depths <- lengths(parts)
    max_depth <- max(depths)
  }

  stub_offset <- if (is.null(stub_col)) 0L else 1L

  rows <- vector("list", max_depth)

  for (L in seq_len(max_depth)) {
    cells <- list()

    if (L == 1L && !is.null(stub_col)) {
      stub_class <- "blockr-stub-header"
      if (isTRUE(stub_sortable)) stub_class <- paste(stub_class, "blockr-sortable")
      cells[[length(cells) + 1L]] <- htmltools::tags$th(
        class = stub_class,
        rowspan = max_depth,
        `data-col-index` = if (isTRUE(stub_sortable)) 0L else NULL,
        htmltools::HTML("&nbsp;"),
        if (isTRUE(stub_sortable)) {
          htmltools::tags$span(class = "blockr-sort-icon")
        }
      )
    }

    i <- 1L
    while (i <= length(data_cols)) {
      if (depths[i] < L) {
        # Already finalised on an earlier row via rowspan
        i <- i + 1L
        next
      }

      prefix_i <- parts[[i]][seq_len(L)]

      # Extend the merge as long as adjacent columns have depth >= L
      # AND share the same prefix[1:L].
      j <- i
      while (j < length(data_cols)) {
        if (depths[j + 1L] < L) break
        if (!identical(parts[[j + 1L]][seq_len(L)], prefix_i)) break
        j <- j + 1L
      }

      span      <- j - i + 1L
      all_leaf  <- all(depths[i:j] == L)

      if (all_leaf) {
        # At the leaf row, honour attr(col, "label") so that
        # summary_table_block's pre-baked "<strong>...</strong><br>N = ..."
        # HTML headers render correctly. The sort arrow rides the sub-line
        # (N = k) rather than stacking below it -- see leaf_header_content().
        rowspan <- max_depth - L + 1L
        col_sortable <- isTRUE(sortable) && (span == 1L)
        sort_icon <- if (col_sortable) {
          htmltools::tags$span(class = "blockr-sort-icon")
        }
        content <- leaf_header_content(
          data, data_cols[i], parts[[i]][L], span, sort_icon = sort_icon
        )
        cls <- "blockr-col-header leaf"
        if (col_sortable) cls <- paste(cls, "blockr-sortable")
        th_args <- list(
          content,
          class   = cls,
          colspan = span
        )
        if (rowspan > 1L) th_args$rowspan <- rowspan
        if (col_sortable) {
          th_args$`data-col-index` <- (i - 1L) + stub_offset
        }
      } else {
        content <- prefix_i[L]
        th_args <- list(
          content,
          class   = "blockr-col-header group",
          colspan = span
        )
      }

      cells[[length(cells) + 1L]] <- do.call(htmltools::tags$th, th_args)
      i <- j + 1L
    }

    rows[[L]] <- htmltools::tags$tr(cells)
  }

  htmltools::tags$thead(rows)
}

#' @noRd
leaf_header_content <- function(data, col_name, fallback_text, span,
                                sort_icon = NULL) {
  if (span > 1L) {
    # Group spanner -- never sortable, so the icon (if any) is dropped.
    return(fallback_text)
  }
  lbl <- attr(data[[col_name]], "label")
  if (is.null(lbl) || !is.character(lbl) || !nzchar(lbl)) {
    # No label: name and sort arrow share the single row.
    if (is.null(sort_icon)) return(fallback_text)
    return(htmltools::tags$div(
      class = "dt-th-namerow",
      htmltools::tags$span(class = "arm__name", fallback_text),
      sort_icon
    ))
  }
  if (grepl("<", lbl, fixed = TRUE)) {
    # Pre-baked HTML label (legacy / spanner path) -- pass through untouched,
    # with the sort arrow trailing.
    return(htmltools::tagList(htmltools::HTML(lbl), sort_icon))
  }
  # Direction-01 two-tier arm header: "<arm>\nN = <n>" splits into a strong
  # arm name line + a quiet "N = k" sub-line. A label without a newline
  # renders as the arm name alone.
  parts <- strsplit(lbl, "\n", fixed = TRUE)[[1]]
  name_line <- parts[1L]
  n_line <- if (length(parts) > 1L) paste(parts[-1L], collapse = " ") else NULL
  if (!is.null(n_line) && nzchar(n_line)) {
    # Two-tier: arm name on top; the "N = k" sub-line and the sort arrow
    # share the lower row, so the arrow never adds a row of its own.
    htmltools::tagList(
      htmltools::tags$span(class = "arm__name", name_line),
      htmltools::tags$div(
        class = "dt-th-subrow",
        htmltools::tags$span(class = "arm__n num", n_line),
        sort_icon
      )
    )
  } else {
    # Single-line label: name and sort arrow share the one row.
    htmltools::tags$div(
      class = "dt-th-namerow",
      htmltools::tags$span(class = "arm__name", name_line),
      sort_icon
    )
  }
}

# ---------------------------------------------------------------------------
# Section-aware <tbody> builder
# ---------------------------------------------------------------------------

#' Keep only section/var columns that actually carry a grouping value.
#'
#' A `.section_*` column that is entirely NA/blank is not a real
#' grouping dimension -- rendering it would wrap every row under a single
#' "(missing)" header. Drop those so the table renders flat (or indent-only)
#' instead. A *partially* empty column is kept: its gaps still render as
#' "(missing)", which is the right label for a genuine orphan bucket. Shared by
#' [html_table()] and the table block's structured path.
#' @noRd
nonempty_section_cols <- function(data, section_cols) {
  section_cols[vapply(section_cols, function(sc) {
    v <- trimws(as.character(data[[sc]]))
    any(!is.na(v) & nzchar(v))
  }, logical(1L))]
}

#' @noRd
build_html_tbody <- function(data, section_cols, stub_col, data_cols,
                             styling_cols = character(), collapsible = TRUE) {
  ncol_total <- length(data_cols) + (if (is.null(stub_col)) 0L else 1L)
  if (ncol_total == 0L) ncol_total <- 1L

  has_indent <- ".indent" %in% styling_cols
  has_bold   <- ".strong"   %in% styling_cols
  has_italic <- ".emph" %in% styling_cols
  indent_px  <- 16L

  # Vectorized assembly: build the body as a single HTML string instead of
  # one htmltools tag object per cell. The per-cell `tags$td()` construction
  # plus the `renderTags()` tree walk was the render bottleneck (see the flat
  # `drilldown_table()` path). This is the same structured builder kept in
  # lock-step: section-header rows are still interleaved at section restarts,
  # the group-end hairline still tags each group's last data row, and indent /
  # bold / italic / missing handling is preserved byte-for-byte. Summary /
  # pivot "Table 1" output is small today, but this keeps it scaling like the
  # flat table if a large structured frame ever arrives.
  n_rows <- nrow(data)
  if (n_rows == 0L) return(htmltools::tags$tbody())

  esc <- function(x) htmltools::htmlEscape(as.character(x), attribute = FALSE)
  k <- length(section_cols)

  # Per-row section path (rows x levels); NA renders as "(missing)".
  if (k > 0L) {
    path_mat <- vapply(section_cols, function(sc) {
      v <- as.character(data[[sc]])
      v[is.na(data[[sc]])] <- "(missing)"
      v
    }, character(n_rows))
    if (is.null(dim(path_mat))) path_mat <- matrix(path_mat, nrow = n_rows)
  } else {
    path_mat <- matrix(character(), nrow = n_rows, ncol = 0L)
  }

  # diff_from[i]: outermost section level that changed vs the previous row
  # (row 1 changes at level 1); NA when nothing changed -> no header emitted.
  diff_from <- rep(NA_integer_, n_rows)
  if (k > 0L) {
    changed <- matrix(FALSE, n_rows, k)
    for (L in seq_len(k)) {
      col <- path_mat[, L]
      changed[, L] <- c(TRUE, col[-1L] != col[-n_rows])
    }
    for (L in k:1L) diff_from[changed[, L]] <- L
  }

  # A data row is its group's last when the NEXT row restarts the outermost
  # level, or it is the final row (matches the old mark_group_last()).
  next_diff <- c(diff_from[-1L], NA_integer_)
  group_last <- (seq_len(n_rows) == n_rows) |
    (!is.na(next_diff) & next_diff == 1L)

  # Section-header rows: one HTML string per (row, level), blanked on rows
  # where level L does not restart (L < diff_from, or no change at all).
  # When collapsing is off the section header is a static label -- no chevron, no
  # button affordance (wireCollapse also bails via data-dt-collapsible=0).
  chev     <- if (isTRUE(collapsible)) as.character(section_chevron_svg()) else ""
  btn_open <- if (isTRUE(collapsible)) {
    "<button class=\"blockr-section-btn\" type=\"button\" aria-expanded=\"true\">"
  } else {
    "<span class=\"blockr-section-btn blockr-section-btn-static\">"
  }
  btn_close <- if (isTRUE(collapsible)) "</button>" else "</span>"
  header_cols <- vector("list", k)
  for (L in seq_len(k)) {
    sc <- section_cols[L]
    sec_label <- attr(data[[sc]], "label")
    prefix <- if (!is.null(sec_label) &&
                  is.character(sec_label) && nzchar(sec_label) &&
                  sec_label != sc) {
      paste0("<span class=\"blockr-section-label\">",
             esc(paste0(sec_label, ": ")), "</span>")
    } else {
      ""
    }
    hdr <- paste0(
      "<tr class=\"blockr-section-header\" data-level=\"", L, "\">",
      "<td class=\"blockr-section-cell level-", L,
      "\" colspan=\"", ncol_total, "\">",
      btn_open, chev, prefix,
      "<span class=\"blockr-section-value\">", esc(path_mat[, L]),
      "</span>", btn_close, "</td></tr>"
    )
    hdr[is.na(diff_from) | L < diff_from] <- ""
    header_cols[[L]] <- hdr
  }
  header_prefix <- if (k > 0L) do.call(paste0, header_cols) else rep("", n_rows)

  # Per-row styling vectors.
  row_indent <- if (has_indent) {
    n <- suppressWarnings(as.integer(data[[".indent"]]))
    n[is.na(n) | n < 0L] <- 0L
    n
  } else {
    rep(0L, n_rows)
  }
  row_bold <- if (has_bold) {
    b <- suppressWarnings(as.logical(data[[".strong"]]))
    !is.na(b) & b
  } else {
    rep(FALSE, n_rows)
  }
  row_italic <- if (has_italic) {
    b <- suppressWarnings(as.logical(data[[".emph"]]))
    !is.na(b) & b
  } else {
    rep(FALSE, n_rows)
  }

  # A row is a collapse toggle when the row immediately below it is more deeply
  # indented (i.e. it heads a nested group). Derived purely from `.indent` -- no
  # fabricated sections -- so it covers bold block-label headers ("AGE (years)")
  # AND data rows that parent deeper rows (SOC over PT) alike. Clicking its
  # chevron hides/shows that group, down to the next row at <= its own indent.
  toggle_rows <- if (isTRUE(collapsible) && !is.null(stub_col)) {
    c(row_indent[-1L] > row_indent[-n_rows], FALSE)
  } else {
    rep(FALSE, n_rows)
  }

  row_class <- rep("blockr-data-row", n_rows)
  row_class[row_bold]    <- paste(row_class[row_bold], "blockr-bold")
  row_class[row_italic]  <- paste(row_class[row_italic], "blockr-italic")
  row_class[group_last]  <- paste(row_class[group_last], "blockr-group-last")
  row_class[toggle_rows] <- paste(row_class[toggle_rows], "blockr-indent-toggle")

  # Stub + data cells, column-vectorized.
  if (!is.null(stub_col)) {
    stub_style <- ifelse(row_indent > 0L,
      paste0(" style=\"padding-left:", 24L + row_indent * indent_px, "px;\""),
      "")
    # Parent rows get a chevron toggle button before the label; the JS attaches
    # to the button so it never competes with a drill click on the row.
    chev_btn <- paste0(
      "<button class=\"blockr-indent-btn\" type=\"button\" tabindex=\"-1\" ",
      "aria-expanded=\"true\">", as.character(section_chevron_svg()), "</button>")
    stub_inner <- ifelse(toggle_rows,
      paste0(chev_btn, esc(data[[stub_col]])),
      esc(data[[stub_col]]))
    stub_cls <- ifelse(toggle_rows, "blockr-stub blockr-has-toggle", "blockr-stub")
    stub_html <- paste0("<td class=\"", stub_cls, "\"", stub_style, ">",
                        stub_inner, "</td>")
  } else {
    stub_html <- rep("", n_rows)
  }

  data_cell_cols <- lapply(data_cols, function(cn) {
    col <- data[[cn]]
    out <- paste0("<td class=\"blockr-data\">", esc(col), "</td>")
    # Missing values get a muted em-dash so they read as 'no data'.
    out[is.na(col)] <- "<td class=\"blockr-data blockr-dash\">&mdash;</td>"
    out
  })

  row_inner <- do.call(paste0, c(list(stub_html), data_cell_cols))
  data_rows <- paste0("<tr class=\"", row_class, "\" data-indent=\"",
                      row_indent, "\">", row_inner, "</tr>")

  body_html <- paste0(header_prefix, data_rows, collapse = "")
  htmltools::tags$tbody(htmltools::HTML(body_html))
}

#' Direction-01 group-header caret (SVG, not a text glyph). Rotates to encode
#' collapsed state via CSS. Kept as a helper so both the section-header
#' builder and any future reuse share one source of truth.
#' @noRd
section_chevron_svg <- function() {
  htmltools::HTML(
    paste0(
      "<svg class=\"blockr-chev\" viewBox=\"0 0 24 24\" fill=\"none\" ",
      "stroke=\"currentColor\" stroke-width=\"2.4\" stroke-linecap=\"round\" ",
      "stroke-linejoin=\"round\" aria-hidden=\"true\">",
      "<path d=\"M6 9l6 6 6-6\"/></svg>"
    )
  )
}

# ---------------------------------------------------------------------------
# Inline CSS
# ---------------------------------------------------------------------------

#' Delta CSS layered on top of `blockr.ui::table_preview_css()`. Only
#' contains rules for things the canonical preview doesn't have: a title
#' bar above the table, a search input, multi-level column header
#' borders, row-side section header rows, the collapse chevron, and the
#' .indent/.bold/.italic row styling.
#'
#' `scope` is the container class the table-body / header-cell rules hang off.
#' It defaults to `.blockr-html-table-container` (used by standalone
#' [html_table()], whose container carries that class). The drilldown table
#' block passes the narrower `.drilldown-table-structured` so the Table-1
#' typography (medium 500 stat values, the 450 stub, the 13.5px size) is gated
#' to STRUCTURED output only -- a `<style>` tag is page-global, and a flat table
#' shares the `.blockr-html-table-container` class, so an unscoped delta would
#' leak the bold cells onto a sibling flat table. The bare chrome rules (title
#' bar, toolbar, search input) carry no container prefix and stay global on
#' purpose -- they are generic table chrome a flat table needs too.
#' @noRd
html_table_delta_css <- function(scope = ".blockr-html-table-container") {
  css <- paste0(".blockr-html-table-container {
  background: #ffffff;
  font-size: var(--blockr-font-size-base, 0.875rem);
  color: var(--blockr-color-text-primary, #111827);
}
.blockr-html-table-container .blockr-table-wrapper {
  max-height: none;
}
.blockr-html-table-header {
  display: flex;
  align-items: baseline;
  justify-content: space-between;
  gap: 16px;
  padding: 10px 4px;
  border-bottom: 1px solid var(--blockr-color-border, #e5e7eb);
}
.blockr-html-table-title {
  font-size: var(--blockr-font-size-section, 1rem);
  font-weight: var(--blockr-font-weight-semibold, 600);
  color: var(--blockr-color-text-primary, #111827);
  flex: 1 1 auto;
  min-width: 0;
}
.blockr-html-table-toolbar {
  display: flex;
  align-items: center;
  gap: 6px;
  flex: 0 0 auto;
}
input.blockr-search {
  appearance: none;
  -webkit-appearance: none;
  box-sizing: border-box;
  height: var(--blockr-control-h-sm, 30px);
  border: 1px solid var(--blockr-color-border, #e5e7eb);
  border-radius: 4px;
  padding: 4px 8px 4px 26px;
  font: inherit;
  font-size: var(--blockr-font-size-sm, 0.8125rem);
  color: var(--blockr-color-text-primary, #111827);
  background-color: var(--blockr-color-bg-input, #f9fafb);
  background-image: url(\"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='13' height='13' viewBox='0 0 24 24' fill='none' stroke='%236b7280' stroke-width='2.2' stroke-linecap='round' stroke-linejoin='round'><circle cx='11' cy='11' r='7'/><path d='m20 20-3-3'/></svg>\");
  background-repeat: no-repeat;
  background-position: 8px center;
  width: 180px;
  transition: border-color 0.12s, box-shadow 0.12s;
}
input.blockr-search::placeholder { color: var(--blockr-color-text-subtle, #9ca3af); }
input.blockr-search:focus {
  outline: none;
  border-color: var(--blockr-color-primary, #2563eb);
  box-shadow: 0 0 0 3px rgba(37, 99, 235, 0.12);
  background-color: #ffffff;
}
/* ----------------------------------------------------------------
   Direction-01 'Clean clinical' tokens. Mapped onto blockr theme
   custom properties with the design HEX as the fallback, so the
   renderer looks right with or without a blockr theme loaded. Scoped
   to the container so they cannot leak.
   ---------------------------------------------------------------- */
.blockr-html-table-container {
  --stbl-ink-1: var(--blockr-color-text-primary, #111827);
  --stbl-ink-2: var(--blockr-color-text-secondary, #5b6573);
  --stbl-ink-3: var(--blockr-color-text-muted, #9aa3b0);
  --stbl-hair: var(--blockr-color-border, #e8ebef);
  --stbl-hair-strong: var(--blockr-color-border-strong, #dde1e7);
  --stbl-accent: var(--blockr-color-primary, #2563eb);
  --stbl-surface-1: var(--blockr-color-bg, #ffffff);
}
/* Column headers \u2014 quiet uppercase-ish meta on the stat column, and the
   two-tier arm treatment (strong name + soft N sub-line). */
.blockr-html-table-container .blockr-table thead th {
  vertical-align: bottom;
  white-space: normal;
  word-break: normal;
  overflow-wrap: break-word;
  font-size: 11px;
  letter-spacing: 0.02em;
  font-weight: var(--blockr-font-weight-medium, 500);
  color: var(--stbl-ink-2);
  padding: 14px 18px 12px;
  border-bottom: 1px solid var(--stbl-hair-strong);
}
.blockr-html-table-container .blockr-table thead th.blockr-col-header {
  text-align: right;
}
.blockr-html-table-container .blockr-table thead th.blockr-col-header.group {
  text-align: center;
  font-weight: var(--blockr-font-weight-semibold, 600);
  color: var(--stbl-ink-1);
}
.blockr-html-table-container .blockr-table thead th.blockr-col-header.leaf {
  font-size: 13.5px;
  font-weight: var(--blockr-font-weight-semibold, 600);
  color: var(--stbl-ink-1);
  letter-spacing: -0.01em;
}
.blockr-html-table-container .blockr-table thead th .arm__name {
  display: block;
}
.blockr-html-table-container .blockr-table thead th .arm__n {
  display: block;
  font-size: 11px;
  font-weight: 450;
  color: var(--stbl-ink-3);
  letter-spacing: 0.01em;
  margin-top: 3px;
}
.blockr-html-table-container .blockr-table thead th.blockr-col-header.leaf strong {
  font-weight: var(--blockr-font-weight-semibold, 600);
  font-size: 13.5px;
}
.blockr-html-table-container .blockr-table thead th.blockr-stub-header {
  text-align: left;
  border-bottom: 1px solid var(--stbl-hair-strong);
}
/* Stat-label (row-stub) cells \u2014 wrap to 2 lines (never truncate), aligned
   to the top so a wrapped label stays level with its numbers. Typography
   matches the canonical preview (normal weight, base size); the Table-1
   character comes from STRUCTURE (sections, indentation, bold rows), not from a
   heavier default font. The 24px left padding is the indent-0 BASE: nested rows
   add `row_indent * 16px` on top (build_html_tbody), so level 1 sits at 40px,
   level 2 at 56px, etc. Keeping this base BELOW the first indent step is what
   makes the indentation visible \u2014 if it equalled 40px, level-1 rows would not
   step at all. */
.blockr-html-table-container .blockr-table tbody td.blockr-stub {
  text-align: left;
  vertical-align: top;
  white-space: normal;
  overflow: visible;
  text-overflow: clip;
  max-width: none;
  padding: 9px 18px 9px 24px;
  font-size: var(--blockr-font-size-base, 0.875rem);
  font-weight: var(--blockr-font-weight-normal, 400);
  color: var(--stbl-ink-2);
}
/* Value cells \u2014 right-aligned, tabular figures, top-aligned to match the
   wrapping stub. Normal weight like the preview; emphasis (totals, key rows)
   comes from the data via `.bold` rows, not a blanket medium weight. */
.blockr-html-table-container .blockr-table tbody td.blockr-data {
  text-align: right;
  vertical-align: top;
  white-space: nowrap;
  overflow: visible;
  text-overflow: clip;
  max-width: none;
  padding: 9px 18px;
  font-size: var(--blockr-font-size-base, 0.875rem);
  font-weight: var(--blockr-font-weight-normal, 400);
  color: var(--stbl-ink-1);
  font-variant-numeric: tabular-nums;
  font-feature-settings: 'tnum' 1;
}
/* Em-dash for missing values reads as 'no data', not as a real figure. */
.blockr-html-table-container .blockr-table tbody td.blockr-data.blockr-dash {
  color: var(--stbl-ink-3);
}
/* Subtle accent-tinted hover on stat rows. */
.blockr-html-table-container .blockr-table tbody tr.blockr-data-row:hover td {
  background: rgba(37, 99, 235, 0.035);
}
/* A hairline ONLY at the end of each group, not between every row. */
.blockr-html-table-container .blockr-table tbody tr.blockr-group-last td {
  border-bottom: 1px solid var(--stbl-hair);
  padding-bottom: 11px;
}
.blockr-html-table-container .blockr-table tbody tr.blockr-bold td {
  font-weight: var(--blockr-font-weight-semibold, 600);
}
.blockr-html-table-container .blockr-table tbody tr.blockr-italic td {
  font-style: italic;
}
/* Group header row + full-width clickable button. The td carries no
   padding (so the bigger hit target reaches the row edges); the button
   carries it per dir-1. */
.blockr-html-table-container .blockr-table tbody tr.blockr-section-header {
  cursor: pointer;
}
.blockr-html-table-container .blockr-table tbody tr.blockr-section-header td.blockr-section-cell {
  padding: 0;
  text-align: left;
  border-top: none;
}
.blockr-html-table-container .blockr-section-btn {
  display: flex;
  align-items: center;
  gap: 9px;
  width: 100%;
  background: none;
  border: 0;
  cursor: pointer;
  font: inherit;
  text-align: left;
  color: var(--stbl-ink-1);
  padding: 15px 18px 8px;
}
/* Collapsing disabled: the section label is a static span, not a control. */
.blockr-html-table-container .blockr-section-btn-static { cursor: default; }
.blockr-html-table-container .blockr-section-cell.level-2 .blockr-section-btn {
  padding-left: 36px;
}
.blockr-html-table-container .blockr-section-cell.level-3 .blockr-section-btn {
  padding-left: 54px;
}
.blockr-html-table-container .blockr-section-cell.level-4 .blockr-section-btn {
  padding-left: 72px;
}
.blockr-html-table-container .blockr-section-value {
  font-weight: var(--blockr-font-weight-semibold, 600);
  letter-spacing: -0.005em;
  font-size: 14px;
}
.blockr-html-table-container .blockr-section-cell.level-3 .blockr-section-value,
.blockr-html-table-container .blockr-section-cell.level-4 .blockr-section-value {
  font-size: 13px;
}
/* SVG caret \u2014 muted at rest, darkens on hover, rotates to encode state.
   Path is a down-caret (expanded); collapsed rotates it -90deg. */
.blockr-html-table-container .blockr-chev {
  width: 13px;
  height: 13px;
  flex: none;
  color: var(--stbl-ink-3);
  transition: transform 0.2s ease, color 0.15s ease;
}
.blockr-html-table-container .blockr-section-btn:hover .blockr-chev {
  color: var(--stbl-ink-1);
}
.blockr-html-table-container tr.blockr-section-header.collapsed .blockr-chev {
  transform: rotate(-90deg);
}",
"/* Indent-derived collapse: a chevron button sits before the label of any row
   that heads a deeper-indented group. Reuses the section chevron, rotating when
   the row is collapsed. The button is a bare affordance so clicking the label /
   cells still drills. */
.blockr-html-table-container .blockr-indent-btn {
  border: 0;
  background: transparent;
  padding: 0;
  margin-right: 5px;
  cursor: pointer;
  display: inline-flex;
  align-items: center;
  vertical-align: baseline;
  margin-left: -18px;
}
.blockr-html-table-container .blockr-indent-btn:hover .blockr-chev {
  color: var(--stbl-ink-1);
}
.blockr-html-table-container tr.blockr-indent-toggle.collapsed .blockr-chev {
  transform: rotate(-90deg);
}
.blockr-html-table-container .blockr-table tbody tr:last-child td {
  border-bottom: none;
}
.blockr-html-table-container .blockr-section-label {
  color: var(--stbl-ink-3);
  font-weight: var(--blockr-font-weight-normal, 400);
  margin-right: 4px;
}
/* Sticky header + scroll shadow. The header stays put while the body
   scrolls; a soft shadow fades in once the scroll container is scrolled
   (the JS toggles `.scrolled` on .blockr-table-wrapper). */
.blockr-html-table-container .blockr-table thead th {
  position: sticky;
  top: 0;
  z-index: 3;
  background: var(--stbl-surface-1);
}
.blockr-html-table-container .blockr-table-wrapper.scrolled thead th {
  box-shadow: 0 10px 16px -14px rgba(16, 24, 40, 0.4);
}
.blockr-html-table-caption {
  padding: 8px 4px 4px;
  font-size: var(--blockr-font-size-xs, 0.75rem);
  color: var(--blockr-color-text-muted, #6b7280);
}
.blockr-hidden-collapse,
.blockr-hidden-search {
  display: none !important;
}")
  if (!identical(scope, ".blockr-html-table-container")) {
    css <- gsub(".blockr-html-table-container", scope, css, fixed = TRUE)
  }
  css
}

#' Shared table CSS for the drilldown table chrome (`dt_chrome()`). Mirrors
#' the canonical `blockr.ui::table_preview_css()` base rules but adds the
#' `.drilldown-table-container` recalculating-fade suppression the canonical
#' preview (scoped to `.blockr-table-container`) doesn't carry. `html_table()`
#' itself now pulls the canonical CSS straight from blockr.ui.
#' @noRd
html_table_shared_css_fallback <- function() {
  "/* Suppress Shiny's `.recalculating` dim (opacity 0.3) on the drilldown
   table. The table now re-renders in single-digit ms on a filter, so the
   'computing' fade is pure flicker, not useful feedback. blockr.ui already
   disables it for the canonical preview (`:has(.blockr-table-container)`,
   blockr-table-preview.css); the drilldown table uses a different container
   class, so it needs its own rule here. TODO: unify the two preview renderers
   onto one no-fade rule (drop this once the container classes are shared).
   Two selectors: the FIRST matches the inner `dt_table` output that actually
   recalculates on a filter (it lives INSIDE the container); the second matches
   the case where the recalculating output wraps/contains the container (the
   standalone drilldown_table() in a renderUI). */
.drilldown-table-container .shiny-html-output.recalculating,
.shiny-html-output.recalculating:has(.drilldown-table-container) {
  --_shiny-fade-opacity: 1;
  opacity: 1 !important;
}
.blockr-table {
  border-collapse: collapse;
  width: 100%;
  font-size: var(--blockr-font-size-base, 0.875rem);
}
.blockr-table thead {
  position: sticky;
  top: 0;
  background: white;
  z-index: 1;
}
.blockr-table thead tr {
  border-bottom: 1px solid var(--blockr-color-border, #e5e7eb);
}
.blockr-table th {
  text-align: left;
  padding: 10px 16px;
  font-weight: var(--blockr-font-weight-medium, 500);
  color: var(--blockr-color-text-primary, #111827);
  vertical-align: bottom;
  overflow: hidden;
}
.blockr-table tbody tr {
  border-bottom: 1px solid var(--blockr-grey-100, #f3f4f6);
  transition: background-color 0.15s ease;
}
.blockr-table tbody tr:hover {
  background-color: var(--blockr-color-bg-subtle, #f9fafb);
}
.blockr-table td {
  padding: 10px 16px;
  font-size: var(--blockr-font-size-base, 0.875rem);
  color: var(--blockr-color-text-primary, #111827);
  max-width: 200px;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
.blockr-table th.blockr-sortable {
  cursor: pointer;
  user-select: none;
  transition: background-color 0.15s ease;
}
.blockr-table th.blockr-sortable:hover {
  background-color: var(--blockr-color-bg-subtle, #f9fafb);
}
.blockr-sort-icon {
  display: inline-block;
  width: 12px;
  height: 12px;
  font-size: 10px;
  line-height: 12px;
  text-align: center;
}
.blockr-sort-icon-asc::after {
  content: '\\2191';
  color: #374151;
}
.blockr-sort-icon-desc::after {
  content: '\\2193';
  color: #374151;
}
/* Toolbar + search chrome. Generic table chrome (not the structured Table-1
   treatment), so it lives here in the always-injected shared CSS \u2014 the
   drilldown table block injects the structured delta CSS only for Table-1
   output, and the styled search box must survive on flat tables too. */
.blockr-html-table-header {
  display: flex;
  align-items: baseline;
  justify-content: space-between;
  gap: 16px;
  padding: 10px 4px;
  border-bottom: 1px solid var(--blockr-color-border, #e5e7eb);
}
.blockr-html-table-toolbar {
  display: flex;
  align-items: center;
  gap: 6px;
  flex: 0 0 auto;
}
input.blockr-search {
  appearance: none;
  -webkit-appearance: none;
  box-sizing: border-box;
  height: var(--blockr-control-h-sm, 30px);
  border: 1px solid var(--blockr-color-border, #e5e7eb);
  border-radius: 4px;
  padding: 4px 8px 4px 26px;
  font: inherit;
  font-size: var(--blockr-font-size-sm, 0.8125rem);
  color: var(--blockr-color-text-primary, #111827);
  background-color: var(--blockr-color-bg-input, #f9fafb);
  background-image: url(\"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='13' height='13' viewBox='0 0 24 24' fill='none' stroke='%236b7280' stroke-width='2.2' stroke-linecap='round' stroke-linejoin='round'><circle cx='11' cy='11' r='7'/><path d='m20 20-3-3'/></svg>\");
  background-repeat: no-repeat;
  background-position: 8px center;
  width: 180px;
  transition: border-color 0.12s, box-shadow 0.12s;
}
input.blockr-search::placeholder { color: var(--blockr-color-text-subtle, #9ca3af); }
input.blockr-search:focus {
  outline: none;
  border-color: var(--blockr-color-primary, #2563eb);
  box-shadow: 0 0 0 3px rgba(37, 99, 235, 0.12);
  background-color: #ffffff;
}
/* Excel download: a quiet icon button matching the search input's chrome
   (28px, radius 4, bg-input) — an inline-SVG anchor built by the block
   server (no Bootstrap .btn, no icon font). */
a.blockr-dl-xlsx {
  appearance: none;
  box-sizing: border-box;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: var(--blockr-control-h-sm, 30px);
  height: var(--blockr-control-h-sm, 30px);
  flex: 0 0 auto;
  padding: 0;
  margin: 0;
  border: 1px solid var(--blockr-color-border, #e5e7eb);
  border-radius: 4px;
  background-color: var(--blockr-color-bg-input, #f9fafb);
  color: var(--blockr-grey-500, #6b7280);
  line-height: 1;
  cursor: pointer;
  box-shadow: none;
  transition: border-color 0.12s, background-color 0.12s, color 0.12s;
}
a.blockr-dl-xlsx:hover {
  background-color: #ffffff;
  border-color: var(--blockr-grey-300, #d1d5db);
  color: var(--blockr-color-text-primary, #374151);
  text-decoration: none;
}
a.blockr-dl-xlsx:focus-visible {
  outline: none;
  border-color: var(--blockr-color-primary, #2563eb);
  box-shadow: 0 0 0 3px rgba(37, 99, 235, 0.12);
}
/* Shiny toggles .disabled on download links until the handler is ready. */
a.blockr-dl-xlsx.disabled { opacity: 0.45; pointer-events: none; }
a.blockr-dl-xlsx svg { display: block; }"
}

# ---------------------------------------------------------------------------
# Inline JS -- idempotent, scoped to a specific wrapper id
# ---------------------------------------------------------------------------

#' @noRd
html_table_js_template <- function() {
"(function(){
  var root = document.getElementById('__WRAPPER_ID__');
  if (!root || root.getAttribute('data-blockr-initialized') === '1') return;
  root.setAttribute('data-blockr-initialized', '1');
  var table = root.querySelector('table.blockr-table');
  if (!table) return;
  var tbody = table.querySelector('tbody');
  if (!tbody) return;

  // Snapshot original row order for sort reset
  Array.prototype.slice.call(tbody.children).forEach(function(r, idx){
    r.setAttribute('data-orig-index', idx);
  });

  // ---------- Collapse ----------
  // Recompute visibility of every row from section-header collapsed state.
  // Nested collapse is respected: a row is hidden iff any ancestor section
  // header has the .collapsed class.
  function recomputeCollapse(){
    var secStack = []; // [{ level, collapsed }]
    var indStack = []; // [{ indent, collapsed }]
    var rows = Array.prototype.slice.call(tbody.children);
    rows.forEach(function(r){
      var isSec = r.classList.contains('blockr-section-header');
      var isData = r.classList.contains('blockr-data-row');
      var ind = parseInt(r.getAttribute('data-indent'), 10);
      if (isData && !isNaN(ind)) {
        while (indStack.length > 0 && indStack[indStack.length-1].indent >= ind) indStack.pop();
      }
      if (isSec) {
        var lvl = parseInt(r.getAttribute('data-level'), 10);
        while (secStack.length > 0 && secStack[secStack.length-1].level >= lvl) secStack.pop();
      }
      var hidden = secStack.some(function(s){ return s.collapsed; }) ||
                   indStack.some(function(s){ return s.collapsed; });
      if (hidden) r.classList.add('blockr-hidden-collapse');
      else r.classList.remove('blockr-hidden-collapse');
      if (isSec) secStack.push({ level: parseInt(r.getAttribute('data-level'), 10), collapsed: r.classList.contains('collapsed') });
      if (isData && r.classList.contains('blockr-indent-toggle')) indStack.push({ indent: ind, collapsed: r.classList.contains('collapsed') });
    });
  }
  function syncAria(h){
    var btn = h.querySelector('.blockr-section-btn');
    if (btn) btn.setAttribute('aria-expanded', h.classList.contains('collapsed') ? 'false' : 'true');
  }
  root.querySelectorAll('tr.blockr-indent-toggle .blockr-indent-btn').forEach(function(btn){
    btn.addEventListener('click', function(ev){
      ev.stopPropagation();
      ev.preventDefault();
      var h = btn.closest('tr.blockr-indent-toggle');
      if (!h) return;
      h.classList.toggle('collapsed');
      btn.setAttribute('aria-expanded', h.classList.contains('collapsed') ? 'false' : 'true');
      recomputeCollapse();
    });
  });
  function toggleCollapse(h){
    h.classList.toggle('collapsed');
    syncAria(h);
    recomputeCollapse();
  }
  // The group label is a <button> inside the header row; its click bubbles
  // up to this row-level handler, so one listener covers both.
  root.querySelectorAll('tr.blockr-section-header').forEach(function(h){
    h.addEventListener('click', function(ev){
      ev.stopPropagation();
      toggleCollapse(h);
    });
  });
  if (root.getAttribute('data-initial-expanded') === '0') {
    root.querySelectorAll('tr.blockr-section-header').forEach(function(h){
      h.classList.add('collapsed');
      syncAria(h);
    });
    recomputeCollapse();
  }

  // ---------- Sticky-header scroll shadow ----------
  var scrollWrap = root.querySelector('.blockr-table-wrapper');
  if (scrollWrap) {
    var onScroll = function(){
      if (scrollWrap.scrollTop > 2) scrollWrap.classList.add('scrolled');
      else scrollWrap.classList.remove('scrolled');
    };
    scrollWrap.addEventListener('scroll', onScroll, { passive: true });
    onScroll();
  }

  // ---------- Sort ----------
  function parseNum(s){
    if (s == null) return null;
    var m = String(s).match(/-?\\d[\\d,]*(\\.\\d+)?/);
    if (!m) return null;
    return parseFloat(m[0].replace(/,/g, ''));
  }

  var sortState = { col: null, dir: 0 };

  function resetOrder(){
    var all = Array.prototype.slice.call(tbody.children);
    all.sort(function(a, b){
      return parseInt(a.getAttribute('data-orig-index'), 10) -
             parseInt(b.getAttribute('data-orig-index'), 10);
    });
    var frag = document.createDocumentFragment();
    all.forEach(function(r){ frag.appendChild(r); });
    tbody.appendChild(frag);
  }

  function sortByColumn(colIdx){
    if (sortState.col === colIdx) {
      sortState.dir = sortState.dir === 1 ? -1 : (sortState.dir === -1 ? 0 : 1);
    } else {
      sortState.col = colIdx;
      sortState.dir = 1;
    }
    root.querySelectorAll('th.blockr-sortable .blockr-sort-icon').forEach(function(ic){
      ic.classList.remove('blockr-sort-icon-asc', 'blockr-sort-icon-desc');
    });
    if (sortState.dir === 0) {
      sortState.col = null;
      resetOrder();
      return;
    }
    var th = root.querySelector('th.blockr-sortable[data-col-index=\"' + colIdx + '\"]');
    if (th) {
      var icon = th.querySelector('.blockr-sort-icon');
      if (icon) icon.classList.add(sortState.dir === 1 ? 'blockr-sort-icon-asc' : 'blockr-sort-icon-desc');
    }

    var rows = Array.prototype.slice.call(tbody.children);
    var groups = [];
    var current = { headers: [], rows: [] };
    rows.forEach(function(r){
      if (r.classList.contains('blockr-section-header')) {
        if (current.rows.length > 0) {
          groups.push(current);
          current = { headers: [], rows: [] };
        }
        current.headers.push(r);
      } else if (r.classList.contains('blockr-data-row')) {
        current.rows.push(r);
      }
    });
    if (current.headers.length > 0 || current.rows.length > 0) groups.push(current);

    var dir = sortState.dir;
    groups.forEach(function(g){
      g.rows.sort(function(a, b){
        var av = a.children[colIdx] ? a.children[colIdx].textContent.trim() : '';
        var bv = b.children[colIdx] ? b.children[colIdx].textContent.trim() : '';
        var an = parseNum(av);
        var bn = parseNum(bv);
        var cmp;
        if (an !== null && bn !== null) cmp = an - bn;
        else cmp = av.localeCompare(bv);
        return dir * cmp;
      });
    });

    var frag = document.createDocumentFragment();
    groups.forEach(function(g){
      g.headers.forEach(function(h){ frag.appendChild(h); });
      g.rows.forEach(function(r){ frag.appendChild(r); });
    });
    tbody.appendChild(frag);
  }

  root.querySelectorAll('th.blockr-sortable').forEach(function(th){
    th.addEventListener('click', function(e){
      e.stopPropagation();
      var idx = parseInt(th.getAttribute('data-col-index'), 10);
      if (!isNaN(idx)) sortByColumn(idx);
    });
  });

  // ---------- Search ----------
  var searchInput = root.querySelector('input.blockr-search');
  function applySearch(){
    var query = (searchInput ? searchInput.value : '').trim().toLowerCase();
    var rows = Array.prototype.slice.call(tbody.children);
    rows.forEach(function(r){
      if (!r.classList.contains('blockr-data-row')) return;
      if (!query) {
        r.classList.remove('blockr-hidden-search');
        return;
      }
      var text = r.textContent.toLowerCase();
      if (text.indexOf(query) !== -1) r.classList.remove('blockr-hidden-search');
      else r.classList.add('blockr-hidden-search');
    });
    for (var i = rows.length - 1; i >= 0; i--) {
      var r = rows[i];
      if (!r.classList.contains('blockr-section-header')) continue;
      if (!query) { r.classList.remove('blockr-hidden-search'); continue; }
      var level = parseInt(r.getAttribute('data-level'), 10);
      var anyVisible = false;
      for (var j = i + 1; j < rows.length; j++) {
        var n = rows[j];
        if (n.classList.contains('blockr-section-header')) {
          var nLevel = parseInt(n.getAttribute('data-level'), 10);
          if (nLevel <= level) break;
        }
        if (!n.classList.contains('blockr-hidden-search')) { anyVisible = true; break; }
      }
      if (anyVisible) r.classList.remove('blockr-hidden-search');
      else r.classList.add('blockr-hidden-search');
    }
  }
  if (searchInput) {
    searchInput.addEventListener('input', applySearch);
  }
})();"
}
