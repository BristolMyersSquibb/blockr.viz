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
        # HTML headers render correctly.
        content <- leaf_header_content(data, data_cols[i], parts[[i]][L], span)
        rowspan <- max_depth - L + 1L
        sortable <- (span == 1L)
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
          th_args[[length(th_args) + 1L]] <-
            htmltools::tags$span(class = "blockr-sort-icon")
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
leaf_header_content <- function(data, col_name, fallback_text, span) {
  if (span > 1L) {
    return(fallback_text)
  }
  lbl <- attr(data[[col_name]], "label")
  if (is.null(lbl) || !is.character(lbl) || !nzchar(lbl)) {
    return(fallback_text)
  }
  if (grepl("<", lbl, fixed = TRUE)) {
    htmltools::HTML(lbl)
  } else {
    lbl
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

  prev_path <- rep(NA_character_, length(section_cols))
  out <- list()

  n_rows <- nrow(data)
  for (i in seq_len(n_rows)) {
    curr_path <- if (length(section_cols)) {
      vapply(section_cols, function(sc) {
        v <- data[[sc]][i]
        if (is.na(v)) "(missing)" else as.character(v)
      }, character(1))
    } else {
      character(0)
    }

    diff_from <- NA_integer_
    for (L in seq_along(section_cols)) {
      if (is.na(prev_path[L]) || !identical(curr_path[L], prev_path[L])) {
        diff_from <- L
        break
      }
    }

    if (!is.na(diff_from)) {
      for (L in diff_from:length(section_cols)) {
        sc <- section_cols[L]
        sec_label <- attr(data[[sc]], "label")
        prefix_tag <- if (!identical(sc, ".var") &&
                          !is.null(sec_label) &&
                          is.character(sec_label) &&
                          nzchar(sec_label) &&
                          sec_label != sc) {
          htmltools::tags$span(
            class = "blockr-section-label",
            paste0(sec_label, ": ")
          )
        } else {
          NULL
        }

        out[[length(out) + 1L]] <- htmltools::tags$tr(
          class = "blockr-section-header",
          `data-level` = L,
          htmltools::tags$td(
            class = paste0("blockr-section-cell level-", L),
            colspan = ncol_total,
            htmltools::tags$span(
              class = "blockr-toggle",
              htmltools::HTML("&#8250;")
            ),
            " ",
            prefix_tag,
            htmltools::tags$span(
              class = "blockr-section-value",
              curr_path[L]
            )
          )
        )
      }
    }
    prev_path <- curr_path

    row_bold <- if (has_bold) {
      b <- suppressWarnings(as.logical(data[[".bold"]][i]))
      isTRUE(b)
    } else FALSE
    row_italic <- if (has_italic) {
      b <- suppressWarnings(as.logical(data[[".italic"]][i]))
      isTRUE(b)
    } else FALSE
    row_indent <- if (has_indent) {
      n <- suppressWarnings(as.integer(data[[".indent"]][i]))
      if (is.na(n) || n < 0L) 0L else n
    } else 0L

    row_class <- "blockr-data-row"
    if (row_bold)   row_class <- paste(row_class, "blockr-bold")
    if (row_italic) row_class <- paste(row_class, "blockr-italic")

    cells <- list()
    if (!is.null(stub_col)) {
      stub_style <- if (row_indent > 0L) {
        paste0("padding-left:", 16L + row_indent * indent_px, "px;")
      } else NULL
      cells[[length(cells) + 1L]] <- htmltools::tags$td(
        class = "blockr-stub",
        style = stub_style,
        as.character(data[[stub_col]][i])
      )
    }
    for (cn in data_cols) {
      v <- data[[cn]][i]
      cells[[length(cells) + 1L]] <- htmltools::tags$td(
        class = "blockr-data",
        if (is.na(v)) htmltools::HTML("&mdash;") else as.character(v)
      )
    }

    out[[length(out) + 1L]] <- htmltools::tags$tr(
      class = row_class,
      cells
    )
  }

  htmltools::tags$tbody(out)
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
.blockr-html-table-container .blockr-table thead th {
  text-align: center;
  vertical-align: bottom;
  white-space: normal;
  word-break: normal;
  overflow-wrap: break-word;
  padding: 8px 12px;
}
.blockr-html-table-container .blockr-table thead th.blockr-col-header.group {
  font-weight: var(--blockr-font-weight-semibold, 600);
  border-bottom: 1px solid var(--blockr-color-border, #e5e7eb);
}
.blockr-html-table-container .blockr-table thead th.blockr-col-header.leaf {
  border-bottom: 2px solid var(--blockr-grey-300, #d1d5db);
}
.blockr-html-table-container .blockr-table thead th.blockr-col-header.leaf strong {
  font-weight: var(--blockr-font-weight-semibold, 600);
  font-size: var(--blockr-font-size-base, 0.875rem);
}
.blockr-html-table-container .blockr-table thead th.blockr-stub-header {
  text-align: left;
  border-bottom: 2px solid var(--blockr-grey-300, #d1d5db);
}
.blockr-html-table-container .blockr-table tbody td.blockr-stub {
  text-align: left;
  padding-left: 16px;
  font-weight: var(--blockr-font-weight-normal, 400);
}
.blockr-html-table-container .blockr-table tbody td.blockr-data {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
.blockr-html-table-container .blockr-table tbody tr.blockr-bold td {
  font-weight: var(--blockr-font-weight-semibold, 600);
}
.blockr-html-table-container .blockr-table tbody tr.blockr-italic td {
  font-style: italic;
}
.blockr-html-table-container .blockr-table tbody tr.blockr-section-header {
  cursor: pointer;
}
.blockr-html-table-container .blockr-table tbody tr.blockr-section-header td {
  position: relative;
  text-align: left;
  color: var(--blockr-color-text-primary, #111827);
  padding-top: 12px;
  padding-bottom: 4px;
  border-top: 1px solid var(--blockr-color-border, #e5e7eb);
  font-weight: var(--blockr-font-weight-semibold, 600);
  background: #ffffff;
  transition: background 0.08s;
}
.blockr-html-table-container .blockr-table tbody tr.blockr-section-header:hover td {
  background: var(--blockr-color-bg-subtle, #f9fafb);
}
.blockr-html-table-container .blockr-table tbody tr.blockr-section-header[data-level='2'] td {
  padding-left: 32px;
}
.blockr-html-table-container .blockr-table tbody tr.blockr-section-header[data-level='3'] td {
  padding-left: 48px;
  font-size: var(--blockr-font-size-sm, 0.8125rem);
}
.blockr-html-table-container .blockr-table tbody tr.blockr-section-header[data-level='4'] td {
  padding-left: 64px;
  font-weight: var(--blockr-font-weight-medium, 500);
  font-size: var(--blockr-font-size-sm, 0.8125rem);
}
.blockr-html-table-container .blockr-table tbody tr.blockr-section-header:first-child td {
  border-top: none;
}
.blockr-html-table-container .blockr-table tbody tr:last-child td {
  border-bottom: 2px solid var(--blockr-grey-300, #d1d5db);
}
.blockr-html-table-container .blockr-toggle {
  position: absolute;
  left: 2px;
  top: 50%;
  margin-top: -0.55em;
  width: 0.9em;
  color: var(--blockr-color-text-muted, #6b7280);
  font-weight: var(--blockr-font-weight-normal, 400);
  font-size: 1em;
  line-height: 1;
  text-align: center;
  /* glyph points right; rotate to point DOWN when expanded, 0deg when collapsed */
  transform: rotate(90deg);
  transform-origin: center;
  transition: transform 0.12s ease;
}
.blockr-html-table-container tr.blockr-section-header:hover .blockr-toggle {
  color: var(--blockr-color-text-primary, #111827);
}
.blockr-html-table-container tr.blockr-section-header.collapsed .blockr-toggle {
  transform: rotate(0deg);
}
.blockr-html-table-container .blockr-section-label {
  color: var(--blockr-color-text-muted, #6b7280);
  font-weight: var(--blockr-font-weight-normal, 400);
  margin-right: 4px;
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
  ".blockr-table {
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
  var table = root.querySelector('table.blockr-html-table');
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
  function toggleCollapse(h){
    h.classList.toggle('collapsed');
    recomputeCollapse();
  }
  root.querySelectorAll('tr.blockr-section-header').forEach(function(h){
    h.addEventListener('click', function(ev){
      ev.stopPropagation();
      toggleCollapse(h);
    });
  });
  if (root.getAttribute('data-initial-expanded') === '0') {
    root.querySelectorAll('tr.blockr-section-header').forEach(function(h){
      h.classList.add('collapsed');
    });
    recomputeCollapse();
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

# ---------------------------------------------------------------------------
# Block: HTML Table
# ---------------------------------------------------------------------------

#' HTML Table Block
#'
#' Blockr transform block wrapping [html_table()]. Consumes the output
#' of [summary_table()] / [pivot_table()] and renders it as an
#' interactive HTML table with collapsible sections, multi-level
#' column spanners, and sticky headers. Tuned for dashboard use.
#'
#' @param title Optional table title.
#' @param caption Optional trailing caption.
#' @param default_expanded Logical. When `TRUE`, sections start
#'   expanded. Default `TRUE`.
#' @param max_height CSS max-height of the scroll container. Default
#'   `"600px"`.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @export
new_html_table_block <- function(title = "",
                                 caption = "",
                                 default_expanded = TRUE,
                                 max_height = "600px",
                                 ...) {
  lifecycle::deprecate_soft(
    "0.0.0", "new_html_table_block()", "new_table_block()",
    details = paste(
      "Unregistered; table_block now renders structured summaries",
      "interactively (it reuses the html builders). Constructor kept",
      "for board compat."
    )
  )
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        r_title            <- shiny::reactiveVal(title)
        r_caption          <- shiny::reactiveVal(caption)
        r_default_expanded <- shiny::reactiveVal(isTRUE(default_expanded))
        r_max_height       <- shiny::reactiveVal(max_height)

        shiny::observeEvent(input$title,    r_title(input$title))
        shiny::observeEvent(input$caption,  r_caption(input$caption))
        shiny::observeEvent(input$default_expanded,
                            r_default_expanded(isTRUE(input$default_expanded)))
        shiny::observeEvent(input$max_height, r_max_height(input$max_height))

        list(
          expr = shiny::reactive({
            bquote(
              blockr.bi::html_table(
                data             = data,
                title            = .(r_title()),
                caption          = .(r_caption()),
                default_expanded = .(r_default_expanded()),
                max_height       = .(r_max_height())
              )
            )
          }),
          state = list(
            title            = r_title,
            caption          = r_caption,
            default_expanded = r_default_expanded,
            max_height       = r_max_height
          )
        )
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::div(
          class = "block-container",
          shiny::fluidRow(
            shiny::column(6,
              shiny::textInput(ns("title"), "Title",
                               value = title, width = "100%")
            ),
            shiny::column(6,
              shiny::textInput(ns("caption"), "Caption",
                               value = caption, width = "100%")
            )
          ),
          shiny::fluidRow(
            shiny::column(6,
              shiny::checkboxInput(ns("default_expanded"),
                                   "Sections expanded by default",
                                   value = isTRUE(default_expanded))
            ),
            shiny::column(6,
              shiny::textInput(ns("max_height"), "Max height",
                               value = max_height, width = "100%")
            )
          )
        )
      )
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) {
        stop("Input must be a data frame")
      }
      if (all(c("label", "depth") %in% names(data)) &&
          any(c("col_var", "n", "value") %in% names(data))) {
        stop(
          "html_table_block does not support legacy long-format input. ",
          "Use gt_table_block instead."
        )
      }
      invisible(NULL)
    },
    class = "html_table_block",
    external_ctrl = TRUE,
    allow_empty_state = c("title", "caption"),
    ...
  )
}

#' @importFrom blockr.core block_ui
#' @method block_ui html_table_block
#' @export
block_ui.html_table_block <- function(id, x, ...) {
  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "result"))
  )
}

#' @importFrom blockr.core block_output
#' @method block_output html_table_block
#' @export
block_output.html_table_block <- function(x, result, session) {
  shiny::renderUI(result)
}
