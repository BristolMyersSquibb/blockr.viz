#' Write an annotated data frame to a styled Excel (.xlsx) file
#'
#' Renders an [annotated data frame][as_annotated_df()] to an Excel worksheet
#' that keeps the table's structure: row indentation (`.indent` becomes the
#' cell indent), bold rows (`.strong`), and two-level column spanners
#' (`Top||Leaf` becomes merged header cells). The same frame drives the
#' blockr.viz table renderer.
#'
#' @details
#' Input is the annotated-data-frame contract:
#' \itemize{
#'   \item `.label` is the row-stub (first) column; `.indent` is the Excel
#'     cell indent level; `.strong` rows are bold.
#'   \item `.section_1, ..., .section_k` become bold section-header rows
#'     interleaved at each section restart, as in the HTML renderer. These
#'     and any other dot-prefixed columns are not exported as data columns.
#'   \item data columns carry their pre-formatted strings. A name with a
#'     `"||"` (`"Placebo||CAT1"`) is a two-level spanner: the part before the
#'     `"||"` becomes a merged top header cell; `attr(col, "label")`
#'     (`"Placebo\nN = 86"`) is the leaf header text.
#' }
#' The header is frozen, bordered, and bold; numbers right-align, the stub
#' left-aligns. Only openxlsx is needed.
#'
#' @param x An annotated data frame, or any object with an [as_annotated_df()]
#'   method (e.g. a composer `composed_table`), which is coerced first.
#' @param file Path to write the `.xlsx` to.
#' @param title Optional title, written merged-and-bold above the table.
#' @param subtitle Optional subtitle, written merged-and-italic under the
#'   title.
#' @param caption Optional caption / footnote line, written italic below the
#'   table.
#' @param sheet Worksheet name.
#' @param digits Optional number format, the table block's `format`. A number
#'   of decimals gives every numeric data column that cell number format
#'   (`"0.00"` for `digits = 2`): the cell keeps the full value and displays
#'   it rounded. `"number"` shows two decimals on columns that hold fractions
#'   and leaves whole-number columns in the general format. `"compact"`
#'   (1.2K, 3.4M) and `"percent"` (12.3%) format every numeric column. `NULL`
#'   (default) writes every number in the general format.
#'
#' @return `file`, invisibly.
#' @seealso [as_annotated_df()], [new_table_block()]
#' @export
write_annotated_xlsx <- function(x, file, title = NULL, subtitle = NULL,
                                 caption = NULL, sheet = "Table",
                                 digits = NULL) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("write_annotated_xlsx() needs the 'openxlsx' package.", call. = FALSE)
  }
  df <- if (is.data.frame(x)) x else as_annotated_df(x)

  stub_col <- if (".label" %in% names(df)) ".label" else names(df)[1]
  data_cols <- setdiff(names(df), stub_col)
  data_cols <- data_cols[!startsWith(data_cols, ".")]
  n_col <- length(data_cols) + 1L
  n_row <- nrow(df)
  indent <- rep(0L, n_row)
  if (".indent" %in% names(df)) {
    indent <- suppressWarnings(as.integer(df[[".indent"]]))
    indent[is.na(indent)] <- 0L
  }
  bold_row <- rep(FALSE, n_row)
  if (".strong" %in% names(df)) {
    bv <- suppressWarnings(as.logical(df[[".strong"]]))
    bold_row <- !is.na(bv) & bv
  }

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet)
  r <- xlsx_write_titles(wb, sheet, n_col, title, subtitle)
  header_bottom_row <- xlsx_write_header(wb, sheet, df, stub_col, data_cols, r)
  r <- header_bottom_row + 1L

  view <- annotated_structure_view(df)
  df <- view$data
  sec <- xlsx_section_rows(df, view$section_cols, n_row)
  total_out <- n_row + length(sec$hdr_pos)
  xlsx_write_body(wb, sheet, df, stub_col, data_cols, sec, r, total_out)
  xlsx_style_body(wb, sheet, df, data_cols, sec, r, indent, bold_row, digits)

  if (!is.null(caption) && nzchar(caption)) {
    cap_style <- openxlsx::createStyle(
      textDecoration = "italic", halign = "left", fontSize = 9,
      fontColour = "#6b7280")
    # One row per line: a cell does not grow to fit a line break.
    lines <- strsplit(caption, "\n", fixed = TRUE)[[1L]]
    for (i in seq_along(lines)) {
      xlsx_put_line(wb, sheet, lines[[i]], r + total_out + i, n_col, cap_style)
    }
  }

  openxlsx::setColWidths(wb, sheet, cols = 1L, widths = 34)
  if (n_col > 1L) {
    openxlsx::setColWidths(wb, sheet, cols = 2:n_col, widths = 16)
  }
  openxlsx::freezePane(wb, sheet, firstActiveRow = header_bottom_row + 1L)

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}

# A merged line across the table's width.
xlsx_put_line <- function(wb, sheet, text, row, n_col, style) {
  openxlsx::writeData(wb, sheet, text, startRow = row, startCol = 1L)
  openxlsx::mergeCells(wb, sheet, cols = seq_len(n_col), rows = row)
  openxlsx::addStyle(wb, sheet, style, rows = row, cols = 1L)
}

# Title and subtitle, then one spacer row. Returns the next free row.
xlsx_write_titles <- function(wb, sheet, n_col, title, subtitle) {
  r <- 1L
  if (!is.null(title) && nzchar(title)) {
    xlsx_put_line(wb, sheet, title, r, n_col, openxlsx::createStyle(
      textDecoration = "bold", halign = "center", fontSize = 12))
    r <- r + 1L
  }
  if (!is.null(subtitle) && nzchar(subtitle)) {
    xlsx_put_line(wb, sheet, subtitle, r, n_col, openxlsx::createStyle(
      textDecoration = "italic", halign = "center", fontSize = 11,
      fontColour = "#6b7280"))
    r <- r + 1L
  }
  if (r > 1L) r + 1L else r
}

# The header from row `r`: an optional spanner row (merged cells for
# `Top||Leaf` names), then the leaf row. Returns the leaf row.
xlsx_write_header <- function(wb, sheet, df, stub_col, data_cols, r) {
  n_data <- length(data_cols)
  # `top` is the spanner (empty for flat columns); `leaf` is the header text,
  # taken from attr(col, "label") so the Big-N line comes along.
  split_names <- strsplit(data_cols, "||", fixed = TRUE)
  top <- vapply(split_names, function(p) if (length(p) > 1L) p[[1L]] else "",
                character(1))
  leaf <- vapply(seq_len(n_data), function(j) {
    lbl <- attr(df[[data_cols[j]]], "label")
    if (is.null(lbl) || !nzchar(lbl)) utils::tail(split_names[[j]], 1L) else lbl
  }, character(1))
  has_spanner <- any(nzchar(top))

  # Stub header cells: one label per header row (see stub_header_rows()).
  stub_hdr <- stub_header_rows(df[[stub_col]], 1L + has_spanner)
  write_stub_header <- function(i, row, border = NULL, border_style = NULL) {
    if (!nzchar(stub_hdr$label[[i]])) return(invisible())
    openxlsx::writeData(wb, sheet, stub_hdr$label[[i]], startRow = row,
                        startCol = 1L)
    openxlsx::addStyle(wb, sheet, openxlsx::createStyle(
      textDecoration = if (stub_hdr$bold[[i]]) "bold" else NULL,
      halign = "left", valign = "bottom", wrapText = TRUE,
      border = border, borderStyle = border_style %||% "thin"),
      rows = row, cols = 1L)
  }

  if (has_spanner) {
    write_stub_header(1L, r)
    spanner_style <- openxlsx::createStyle(
      textDecoration = "bold", halign = "center", valign = "bottom",
      border = "bottom", borderStyle = "thin")
    runs <- rle(top)
    pos <- 2L
    for (i in seq_along(runs$lengths)) {
      len <- runs$lengths[i]
      if (nzchar(runs$values[i])) {
        cols <- pos:(pos + len - 1L)
        openxlsx::writeData(wb, sheet, runs$values[i], startRow = r,
                            startCol = pos)
        if (len > 1L) openxlsx::mergeCells(wb, sheet, cols = cols, rows = r)
        openxlsx::addStyle(wb, sheet, spanner_style, rows = r, cols = cols,
                           gridExpand = TRUE)
      }
      pos <- pos + len
    }
    r <- r + 1L
  }
  openxlsx::writeData(wb, sheet, t(c("", leaf)), startRow = r, startCol = 1L,
                      colNames = FALSE)
  openxlsx::addStyle(wb, sheet, openxlsx::createStyle(
    textDecoration = "bold", halign = "right", valign = "bottom",
    wrapText = TRUE, border = "bottom", borderStyle = "medium"),
    rows = r, cols = seq_len(length(data_cols) + 1L), gridExpand = TRUE)
  write_stub_header(1L + has_spanner, r, "bottom", "medium")
  r
}

# The body block from row `r`, with the section header texts in the stub
# column at the blank rows. Indexing with NA keeps each column's type.
xlsx_write_body <- function(wb, sheet, df, stub_col, data_cols, sec, r,
                            total_out) {
  body <- data.frame(
    stub = as.character(df[[stub_col]]),
    df[data_cols],
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out_body <- body[rep(NA_integer_, total_out), , drop = FALSE]
  out_body[sec$data_pos, ] <- body
  if (length(sec$hdr_pos)) out_body[[1L]][sec$hdr_pos] <- sec$hdr_txt
  openxlsx::writeData(wb, sheet, out_body, startRow = r, startCol = 1L,
                      colNames = FALSE)
}

# Alignment, indent, bold, number format and cell paint for the body. One
# style per distinct combination, applied to all matching rows at once:
# addStyle is expensive per call.
xlsx_style_body <- function(wb, sheet, df, data_cols, sec, r, indent,
                            bold_row, digits) {
  n_data <- length(data_cols)
  data_xl <- r + sec$data_pos - 1L
  stub_xl <- c(data_xl, r + sec$hdr_pos - 1L)
  stub_indent <- c(indent, sec$hdr_lvl - 1L)
  stub_bold <- c(bold_row, rep(TRUE, length(sec$hdr_pos)))
  for (g in split(seq_along(stub_xl), paste(stub_indent, stub_bold))) {
    st <- openxlsx::createStyle(
      halign = "left", valign = "top", indent = stub_indent[g[1L]],
      textDecoration = if (stub_bold[g[1L]]) "bold" else NULL)
    openxlsx::addStyle(wb, sheet, st, rows = stub_xl[g], cols = 1L,
                       gridExpand = TRUE)
  }
  if (n_data == 0L) return(invisible())

  for (b in c(FALSE, TRUE)) {
    if (!any(bold_row == b)) next
    openxlsx::addStyle(
      wb, sheet,
      openxlsx::createStyle(halign = "right", valign = "top",
                            textDecoration = if (b) "bold"),
      rows = data_xl[bold_row == b], cols = 1L + seq_len(n_data),
      gridExpand = TRUE)
  }
  # Number format on numeric columns, stacked on the alignment. Whole-number
  # columns keep the general format unless `digits` formats every column.
  num_fmt <- xlsx_num_format(digits)
  every <- identical(digits, "compact") || identical(digits, "percent") ||
    (is.numeric(digits) && length(digits) == 1L)
  if (!is.null(num_fmt)) {
    fmt_style <- openxlsx::createStyle(numFmt = num_fmt)
    for (j in seq_len(n_data)) {
      v <- df[[data_cols[j]]]
      if (!is.numeric(v)) next
      v <- v[is.finite(v)]
      if (!length(v) || (!every && all(v == round(v)))) next
      openxlsx::addStyle(wb, sheet, fmt_style, rows = data_xl, cols = j + 1L,
                         stack = TRUE)
    }
  }
  # Per-cell paint (see annotation_cell_paint()): one style per distinct
  # (fill, text colour, bold).
  for (j in seq_len(n_data)) {
    paint <- annotation_cell_paint(df, data_cols[j])
    if (is.null(paint)) next
    on <- which(!is.na(paint$bg))
    key <- paste(paint$bg[on], paint$fg[on], bold_row[on])
    for (g in split(on, key)) {
      i <- g[1L]
      st <- openxlsx::createStyle(
        fgFill = paint$bg[i],
        fontColour = if (!is.na(paint$fg[i])) paint$fg[i],
        textDecoration = if (bold_row[i]) "bold")
      openxlsx::addStyle(wb, sheet, st, rows = data_xl[g], cols = j + 1L,
                         stack = TRUE)
    }
  }
}

# Section-header rows for the interleaved body, placed where the table view
# puts its section rows: for each data row, the
# outermost changed section level and every level below it emit a header.
# Returns positions within the body block: `data_pos` for the data rows,
# `hdr_pos`, `hdr_txt` and `hdr_lvl` for the header rows.
xlsx_section_rows <- function(df, section_cols, n_row) {
  k <- length(section_cols)
  diff_from <- rep(NA_integer_, n_row)
  path_mat <- matrix(character(), nrow = n_row, ncol = 0L)
  if (k > 0L && n_row > 0L) {
    path_mat <- vapply(section_cols, function(sc) {
      v <- as.character(df[[sc]])
      v[is.na(df[[sc]])] <- "(missing)"
      v
    }, character(n_row))
    if (is.null(dim(path_mat))) path_mat <- matrix(path_mat, nrow = n_row)
    for (L in k:1L) {
      col <- path_mat[, L]
      changed <- c(TRUE, col[-1L] != col[-n_row])
      diff_from[changed] <- L
    }
  }
  n_hdr_before <- ifelse(is.na(diff_from), 0L, k - diff_from + 1L)
  data_pos <- seq_len(n_row) + cumsum(n_hdr_before)
  hdr_pos <- integer(0)
  hdr_txt <- character(0)
  hdr_lvl <- integer(0)
  for (L in seq_len(k)) {
    at <- which(!is.na(diff_from) & diff_from <= L)
    if (!length(at)) next
    txt <- path_mat[at, L]
    lbl <- attr(df[[section_cols[L]]], "label")
    if (is.character(lbl) && length(lbl) == 1L && nzchar(lbl) &&
        lbl != section_cols[L]) {
      txt <- paste0(lbl, ": ", txt)
    }
    # A level-L header sits (k - L + 1) rows above its data row.
    hdr_pos <- c(hdr_pos, data_pos[at] - (k - L + 1L))
    hdr_txt <- c(hdr_txt, txt)
    hdr_lvl <- c(hdr_lvl, rep(L, length(at)))
  }
  list(data_pos = data_pos, hdr_pos = hdr_pos, hdr_txt = hdr_txt,
       hdr_lvl = hdr_lvl)
}

# The Excel number format for `digits` decimals ("0", "0.0", ...), or NULL for
# the general format. `digits` may also be a table number format
# (table_format_state()): "number" is two decimals, "percent" one decimal with
# "%", "compact" thousands and millions as 1.2K and 3.4M.
xlsx_num_format <- function(digits) {
  if (identical(digits, "percent")) return("0.0%")
  if (identical(digits, "compact")) {
    return('[>=1000000]0.0,,"M";[>=1000]0.0,"K";General')
  }
  if (identical(digits, "number")) digits <- 2L
  digits <- suppressWarnings(as.integer(digits))
  if (length(digits) != 1L || is.na(digits) || digits < 0L) return(NULL)
  if (digits == 0L) "0" else paste0("0.", strrep("0", digits))
}

# The Excel download: the exhibit's values under its titles.
export_xlsx <- function(x, file) {
  t <- exhibit_titles(x)
  write_annotated_xlsx(exhibit_data(x), file, title = t$title,
                       subtitle = t$subtitle, caption = t$caption,
                       digits = exhibit_meta(x)$digits)
}
