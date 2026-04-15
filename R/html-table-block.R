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

  if (all(c("label", "depth") %in% names(data)) &&
      any(c("col_var", "n", "value") %in% names(data))) {
    stop(
      "html_table() does not support legacy long-format input. ",
      "Use gt_table() instead."
    )
  }

  section_cols <- grep("^\\.(section_\\d+|var)$", names(data), value = TRUE)
  stub_col     <- if (".label" %in% names(data)) ".label" else NULL
  data_cols    <- setdiff(names(data), c(section_cols, stub_col))

  wrapper_id <- paste0("blockr-html-table-", sub("^file", "", basename(tempfile(""))))

  stub_is_sortable <- FALSE
  thead <- build_html_thead(data, data_cols, stub_col,
                            stub_sortable = stub_is_sortable)
  tbody <- build_html_tbody(data, section_cols, stub_col, data_cols)

  table_tag <- htmltools::tags$table(
    class = "blockr-html-table",
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

  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(html_table_css())),
    htmltools::tags$div(
      id = wrapper_id,
      class = "blockr-html-table-wrapper",
      `data-initial-expanded` = if (isTRUE(default_expanded)) "1" else "0",
      header_div,
      htmltools::tags$div(
        class = "blockr-html-table-scroll",
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
      if (isTRUE(stub_sortable)) stub_class <- paste(stub_class, "sortable")
      cells[[length(cells) + 1L]] <- htmltools::tags$th(
        class = stub_class,
        rowspan = max_depth,
        `data-col-index` = if (isTRUE(stub_sortable)) 0L else NULL,
        htmltools::HTML("&nbsp;"),
        if (isTRUE(stub_sortable)) {
          htmltools::tags$span(class = "blockr-sort-indicator")
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
        if (sortable) cls <- paste(cls, "sortable")
        th_args <- list(
          content,
          class   = cls,
          colspan = span
        )
        if (rowspan > 1L) th_args$rowspan <- rowspan
        if (sortable) {
          th_args$`data-col-index` <- (i - 1L) + stub_offset
          th_args[[length(th_args) + 1L]] <-
            htmltools::tags$span(class = "blockr-sort-indicator")
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
build_html_tbody <- function(data, section_cols, stub_col, data_cols) {
  ncol_total <- length(data_cols) + (if (is.null(stub_col)) 0L else 1L)
  if (ncol_total == 0L) ncol_total <- 1L

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
              htmltools::HTML("&#9662;")
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

    cells <- list()
    if (!is.null(stub_col)) {
      cells[[length(cells) + 1L]] <- htmltools::tags$td(
        class = "blockr-stub",
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
      class = "blockr-data-row",
      cells
    )
  }

  htmltools::tags$tbody(out)
}

# ---------------------------------------------------------------------------
# Inline CSS
# ---------------------------------------------------------------------------

#' @noRd
html_table_css <- function() {
  ".blockr-html-table-wrapper {
  font-family: ui-sans-serif, system-ui, -apple-system, 'Segoe UI', Roboto,
               'Helvetica Neue', Arial, sans-serif;
  font-size: 14px;
  line-height: 1.45;
  color: #1f2937;
  background: #ffffff;
  border: 1px solid #e5e7eb;
  border-radius: 8px;
  box-shadow: 0 1px 2px rgba(15, 23, 42, 0.04),
              0 1px 3px rgba(15, 23, 42, 0.06);
  overflow: hidden;
}
.blockr-html-table-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 16px;
  padding: 14px 18px;
  border-bottom: 1px solid #f1f3f5;
  background: #ffffff;
}
.blockr-html-table-title {
  font-size: 15px;
  font-weight: 600;
  color: #0f172a;
  letter-spacing: -0.01em;
  flex: 1 1 auto;
  min-width: 0;
}
.blockr-html-table-toolbar {
  display: flex;
  align-items: center;
  gap: 8px;
  flex: 0 0 auto;
}
input.blockr-search {
  appearance: none;
  -webkit-appearance: none;
  box-sizing: border-box;
  border: 1px solid #e5e7eb;
  border-radius: 6px;
  padding: 6px 10px 6px 30px;
  font: inherit;
  font-size: 13px;
  color: #1f2937;
  background-color: #fff;
  background-image: url(\"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='14' height='14' viewBox='0 0 24 24' fill='none' stroke='%236b7280' stroke-width='2.2' stroke-linecap='round' stroke-linejoin='round'><circle cx='11' cy='11' r='7'/><path d='m20 20-3-3'/></svg>\");
  background-repeat: no-repeat;
  background-position: 9px center;
  min-width: 220px;
  transition: border-color 0.12s, box-shadow 0.12s;
}
input.blockr-search::placeholder { color: #9ca3af; }
input.blockr-search:focus {
  outline: none;
  border-color: #6366f1;
  box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.15);
}
.blockr-html-table-scroll {
  overflow: auto;
  background: #fff;
}
.blockr-html-table {
  border-collapse: separate;
  border-spacing: 0;
  width: 100%;
  background: #fff;
}
.blockr-html-table thead th {
  position: sticky;
  top: 0;
  z-index: 2;
  background: #f9fafb;
  color: #4b5563;
  font-size: 11px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.04em;
  text-align: center;
  padding: 10px 14px;
  border-bottom: 1px solid #e5e7eb;
  white-space: nowrap;
}
.blockr-html-table thead th.blockr-col-header.leaf {
  border-bottom: 2px solid #d1d5db;
  color: #111827;
  text-transform: none;
  font-size: 12px;
  letter-spacing: 0;
  font-weight: 600;
}
.blockr-html-table thead th.blockr-col-header.leaf strong {
  font-weight: 700;
  font-size: 13px;
  color: #0f172a;
}
.blockr-html-table thead th.blockr-stub-header {
  text-align: left;
  min-width: 200px;
}
.blockr-html-table thead th.sortable {
  cursor: pointer;
  user-select: none;
  transition: background 0.1s;
}
.blockr-html-table thead th.sortable:hover {
  background: #f3f4f6;
}
.blockr-html-table .blockr-sort-indicator {
  display: inline-block;
  width: 10px;
  margin-left: 6px;
  color: #cbd5e1;
  font-size: 10px;
  vertical-align: baseline;
}
.blockr-html-table .blockr-sort-indicator::before { content: '\\2195'; }
.blockr-html-table thead th.sortable:hover .blockr-sort-indicator { color: #94a3b8; }
.blockr-html-table thead th.sort-asc .blockr-sort-indicator {
  color: #1f2937;
}
.blockr-html-table thead th.sort-asc .blockr-sort-indicator::before { content: '\\2191'; }
.blockr-html-table thead th.sort-desc .blockr-sort-indicator {
  color: #1f2937;
}
.blockr-html-table thead th.sort-desc .blockr-sort-indicator::before { content: '\\2193'; }
.blockr-html-table tbody td {
  padding: 8px 14px;
  border-bottom: 1px solid #f1f3f5;
  white-space: nowrap;
  background: #fff;
}
.blockr-html-table tbody td.blockr-stub {
  color: #374151;
  text-align: left;
  padding-left: 28px;
  font-weight: 500;
}
.blockr-html-table tbody td.blockr-data {
  text-align: right;
  font-variant-numeric: tabular-nums;
  color: #1f2937;
}
.blockr-html-table tbody tr.blockr-data-row { transition: background 0.08s; }
.blockr-html-table tbody tr.blockr-data-row:hover td { background: #eff6ff; }
.blockr-html-table tbody tr.blockr-section-header {
  cursor: pointer;
  user-select: none;
}
.blockr-html-table tbody tr.blockr-section-header td.blockr-section-cell {
  text-align: left;
  color: #0f172a;
  background: #ffffff;
  padding: 14px 14px 6px;
  border-bottom: 1px solid #e5e7eb;
}
.blockr-html-table tbody tr.blockr-section-header[data-level='1'] td {
  font-size: 11px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  color: #4338ca;
}
.blockr-html-table tbody tr.blockr-section-header[data-level='2'] td {
  font-size: 12px;
  font-weight: 600;
  color: #334155;
  padding-left: 28px;
}
.blockr-html-table tbody tr.blockr-section-header[data-level='3'] td {
  font-size: 12px;
  font-weight: 600;
  color: #475569;
  padding-left: 44px;
}
.blockr-html-table tbody tr.blockr-section-header[data-level='4'] td {
  font-size: 12px;
  color: #64748b;
  padding-left: 58px;
}
.blockr-html-table tbody tr.blockr-section-header:hover td { background: #f9fafb; }
.blockr-html-table .blockr-toggle {
  display: inline-block;
  width: 12px;
  color: #9ca3af;
  font-size: 0.85em;
  transition: transform 0.12s ease;
  margin-right: 2px;
}
.blockr-html-table tbody tr.blockr-section-header.collapsed .blockr-toggle {
  transform: rotate(-90deg);
}
.blockr-html-table .blockr-section-label {
  color: #94a3b8;
  font-weight: 400;
  text-transform: none;
  letter-spacing: normal;
  margin-right: 3px;
}
.blockr-html-table-caption {
  padding: 10px 18px 14px;
  font-size: 12px;
  color: #6b7280;
  border-top: 1px solid #f1f3f5;
  background: #fff;
}
.blockr-hidden-collapse,
.blockr-hidden-search {
  display: none !important;
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
  function toggleCollapse(h){
    var level = parseInt(h.getAttribute('data-level'), 10);
    var collapsed = h.classList.toggle('collapsed');
    var n = h.nextElementSibling;
    while (n) {
      if (n.classList && n.classList.contains('blockr-section-header')) {
        var nLevel = parseInt(n.getAttribute('data-level'), 10);
        if (nLevel <= level) break;
      }
      if (collapsed) n.classList.add('blockr-hidden-collapse');
      else n.classList.remove('blockr-hidden-collapse');
      n = n.nextElementSibling;
    }
  }
  root.querySelectorAll('tr.blockr-section-header').forEach(function(h){
    h.addEventListener('click', function(){ toggleCollapse(h); });
  });
  if (root.getAttribute('data-initial-expanded') === '0') {
    root.querySelectorAll('tr.blockr-section-header[data-level=\"1\"]').forEach(function(h){
      if (!h.classList.contains('collapsed')) toggleCollapse(h);
    });
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
    root.querySelectorAll('th.sortable').forEach(function(th){
      th.classList.remove('sort-asc', 'sort-desc');
    });
    if (sortState.dir === 0) {
      sortState.col = null;
      resetOrder();
      return;
    }
    var th = root.querySelector('th.sortable[data-col-index=\"' + colIdx + '\"]');
    if (th) th.classList.add(sortState.dir === 1 ? 'sort-asc' : 'sort-desc');

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

  root.querySelectorAll('th.sortable').forEach(function(th){
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
    allow_empty_state = "title",
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
