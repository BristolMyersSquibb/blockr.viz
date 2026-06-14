#' HTML Table Renderer for the Table-Blocks Quartet
#'
#' Renders the output of [summary_table()] or [pivot_table()] as a
#' hand-rolled HTML table with nested row-side section headers,
#' multi-level column spanners, and client-side collapse/expand of
#' sections. Designed as a dashboard-native alternative to
#' [gt_table()] — static gt is ideal for print / CSR output, while
#' this renderer is tuned for interactive dashboards.
#'
#' The input contract is the same dotted-column wide tibble that
#' [summary_table()] produces:
#'
#' - `.section_1, ..., .section_k` — nested row-side section columns,
#'   outermost first. `attr(col, "label")` carries the display label.
#' - `.var` — synthetic variable-name column, present when
#'   `length(vars) > 1`.
#' - `.label` — innermost per-row identifier (stat name / factor
#'   level). Rendered as the leftmost row-stub column.
#' - Data columns — names use `|` as nesting delimiter for multi-level
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
#' @export
html_table <- function(data,
                       title = NULL,
                       caption = NULL,
                       default_expanded = TRUE,
                       max_height = "600px") {
  stopifnot(is.data.frame(data))

  # Tidy `.fmt` form (numbers + per-row template + `.group`) → wide
  # display grid (format-then-spread). No-op on already-wide input.
  data <- fmt_to_wide(data)

  if (all(c("label", "depth") %in% names(data)) &&
      any(c("col_var", "n", "value") %in% names(data))) {
    stop(
      "html_table() does not support legacy long-format input. ",
      "Use gt_table() instead."
    )
  }

  section_cols <- grep("^\\.(section_\\d+|var)$", names(data), value = TRUE)
  stub_col     <- if (".label" %in% names(data)) ".label" else NULL
  styling_cols <- intersect(c(".indent", ".bold", ".italic"), names(data))
  data_cols    <- setdiff(names(data), c(section_cols, stub_col, styling_cols))

  wrapper_id <- paste0("blockr-html-table-", sub("^file", "", basename(tempfile(""))))

  thead <- build_html_thead(data, data_cols, stub_col,
                            stub_sortable = FALSE)
  tbody <- build_html_tbody(data, section_cols, stub_col, data_cols,
                            styling_cols = styling_cols)

  # Use the same class names blockr.extra's preview uses so the shared
  # table_preview_css() rules (typography, padding, hover, sort icons)
  # apply to this table without duplication.
  table_tag <- htmltools::tags$table(
    class = "blockr-table",
    thead,
    tbody
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

  # Pull the shared CSS from blockr.extra if available; else fall back to
  # our local copy of the same rules. Either way, our delta CSS adds the
  # section row groups / collapse toggle / search input / title bar /
  # multi-level header rules that aren't in blockr.extra's preview.
  shared_css <- if (requireNamespace("blockr.extra", quietly = TRUE) &&
                    exists("table_preview_css",
                           envir = asNamespace("blockr.extra"))) {
    blockr.extra::table_preview_css()
  } else {
    htmltools::tags$style(htmltools::HTML(html_table_shared_css_fallback()))
  }

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
# Multi-level <thead> builder
# ---------------------------------------------------------------------------

#' @noRd
build_html_thead <- function(data, data_cols, stub_col, stub_sortable = FALSE) {
  if (length(data_cols) == 0L) {
    parts <- list()
    depths <- integer(0)
    max_depth <- 1L
  } else {
    parts <- strsplit(data_cols, "|", fixed = TRUE)
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
        # (N = k) rather than stacking below it — see leaf_header_content().
        rowspan <- max_depth - L + 1L
        sortable <- (span == 1L)
        sort_icon <- if (sortable) {
          htmltools::tags$span(class = "blockr-sort-icon")
        }
        content <- leaf_header_content(
          data, data_cols[i], parts[[i]][L], span, sort_icon = sort_icon
        )
        cls <- "blockr-col-header leaf"
        if (sortable) cls <- paste(cls, "blockr-sortable")
        th_args <- list(
          content,
          class   = cls,
          colspan = span
        )
        if (rowspan > 1L) th_args$rowspan <- rowspan
        if (sortable) {
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
    # Group spanner — never sortable, so the icon (if any) is dropped.
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
    # Pre-baked HTML label (legacy / spanner path) — pass through untouched,
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

#' @noRd
build_html_tbody <- function(data, section_cols, stub_col, data_cols,
                             styling_cols = character()) {
  ncol_total <- length(data_cols) + (if (is.null(stub_col)) 0L else 1L)
  if (ncol_total == 0L) ncol_total <- 1L

  has_indent <- ".indent" %in% styling_cols
  has_bold   <- ".bold"   %in% styling_cols
  has_italic <- ".italic" %in% styling_cols
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
  chev <- as.character(section_chevron_svg())
  header_cols <- vector("list", k)
  for (L in seq_len(k)) {
    sc <- section_cols[L]
    sec_label <- attr(data[[sc]], "label")
    prefix <- if (!identical(sc, ".var") && !is.null(sec_label) &&
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
      "<button class=\"blockr-section-btn\" type=\"button\" ",
      "aria-expanded=\"true\">", chev, prefix,
      "<span class=\"blockr-section-value\">", esc(path_mat[, L]),
      "</span></button></td></tr>"
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
  } else rep(0L, n_rows)
  row_bold <- if (has_bold) {
    b <- suppressWarnings(as.logical(data[[".bold"]])); !is.na(b) & b
  } else rep(FALSE, n_rows)
  row_italic <- if (has_italic) {
    b <- suppressWarnings(as.logical(data[[".italic"]])); !is.na(b) & b
  } else rep(FALSE, n_rows)

  row_class <- rep("blockr-data-row", n_rows)
  row_class[row_bold]   <- paste(row_class[row_bold], "blockr-bold")
  row_class[row_italic] <- paste(row_class[row_italic], "blockr-italic")
  row_class[group_last] <- paste(row_class[group_last], "blockr-group-last")

  # Stub + data cells, column-vectorized.
  if (!is.null(stub_col)) {
    stub_style <- ifelse(row_indent > 0L,
      paste0(" style=\"padding-left:", 24L + row_indent * indent_px, "px;\""),
      "")
    stub_html <- paste0("<td class=\"blockr-stub\"", stub_style, ">",
                        esc(data[[stub_col]]), "</td>")
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
  data_rows <- paste0("<tr class=\"", row_class, "\">", row_inner, "</tr>")

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

#' @noRd
#'
#' Delta CSS layered on top of `blockr.extra::table_preview_css()`. Only
#' contains rules for things blockr.extra's preview doesn't have: a title
#' bar above the table, a search input, multi-level column header
#' borders, row-side section header rows, the collapse chevron, and the
#' .indent/.bold/.italic row styling.
html_table_delta_css <- function() {
  ".blockr-html-table-container {
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
/* Column headers — quiet uppercase-ish meta on the stat column, and the
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
/* Stat-label (row-stub) cells — wrap to 2 lines (never truncate), aligned
   to the top so a wrapped label stays level with its numbers. */
.blockr-html-table-container .blockr-table tbody td.blockr-stub {
  text-align: left;
  vertical-align: top;
  white-space: normal;
  overflow: visible;
  text-overflow: clip;
  max-width: none;
  padding: 9px 18px 9px 40px;
  font-size: 13.5px;
  font-weight: 450;
  color: var(--stbl-ink-2);
}
/* Value cells — right-aligned, tabular figures, top-aligned to match the
   wrapping stub. */
.blockr-html-table-container .blockr-table tbody td.blockr-data {
  text-align: right;
  vertical-align: top;
  white-space: nowrap;
  overflow: visible;
  text-overflow: clip;
  max-width: none;
  padding: 9px 18px;
  font-size: 13.5px;
  font-weight: 500;
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
/* SVG caret — muted at rest, darkens on hover, rotates to encode state.
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
}"
}

#' @noRd
#'
#' Subset of `blockr.extra::table_preview_css()` mirrored verbatim for
#' standalone use when blockr.extra isn't installed. Keeps html_table()
#' visually consistent without a hard dependency. If blockr.extra is
#' available, prefer its exported helper instead — same source of truth.
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
   treatment), so it lives here in the always-injected shared CSS — the
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
}"
}

# ---------------------------------------------------------------------------
# Inline JS — idempotent, scoped to a specific wrapper id
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
    var stack = []; // [{ level, collapsed }]
    var rows = Array.prototype.slice.call(tbody.children);
    rows.forEach(function(r){
      if (r.classList.contains('blockr-section-header')) {
        var lvl = parseInt(r.getAttribute('data-level'), 10);
        while (stack.length > 0 && stack[stack.length-1].level >= lvl) stack.pop();
        var anyAncestorCollapsed = stack.some(function(s){ return s.collapsed; });
        if (anyAncestorCollapsed) r.classList.add('blockr-hidden-collapse');
        else r.classList.remove('blockr-hidden-collapse');
        stack.push({ level: lvl, collapsed: r.classList.contains('collapsed') });
      } else if (r.classList.contains('blockr-data-row')) {
        var hidden = stack.some(function(s){ return s.collapsed; });
        if (hidden) r.classList.add('blockr-hidden-collapse');
        else r.classList.remove('blockr-hidden-collapse');
      }
    });
  }
  function syncAria(h){
    var btn = h.querySelector('.blockr-section-btn');
    if (btn) btn.setAttribute('aria-expanded', h.classList.contains('collapsed') ? 'false' : 'true');
  }
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
