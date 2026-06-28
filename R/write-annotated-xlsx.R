#' Write an annotated data frame to a styled Excel (.xlsx) file
#'
#' Renders an [annotated data frame][as_annotated_df()] to an Excel worksheet
#' that preserves the table's *structure*: row indentation (`.indent` ->
#' native cell indent), bold header rows (`.strong`), and two-level column
#' spanners (`Top||Leaf` -> merged header cells). The same frame that drives the
#' blockr.viz table renderer also drives this export — one structured artifact,
#' two outputs.
#'
#' @details
#' Input is the annotated-data-frame contract:
#' \itemize{
#'   \item `.label` -> the row-stub (first) column; `.indent` -> the Excel cell
#'     indent level (the hierarchy reads in the spreadsheet outline, not via
#'     faked spaces); `.strong` rows are bold.
#'   \item data columns carry their pre-formatted strings. A column named with a
#'     `"||"` (`"Placebo||CAT1"`) is a two-level spanner: the part before the
#'     `"||"` becomes a merged top header cell; `attr(col, "label")`
#'     (`"Placebo\nN = 86"`) is the leaf header text.
#' }
#' The header is frozen, bordered, and bold; numbers right-align, the stub
#' left-aligns. No blockr / Shiny dependency — just the data frame and openxlsx.
#'
#' @param x An annotated data frame, or any object with an [as_annotated_df()]
#'   method (e.g. a composer `composed_table`), which is coerced first.
#' @param file Path to write the `.xlsx` to.
#' @param title Optional title, written merged-and-bold above the table.
#' @param sheet Worksheet name.
#'
#' @return `file`, invisibly.
#' @seealso [as_annotated_df()], [new_table_block()]
#' @export
write_annotated_xlsx <- function(x, file, title = NULL, sheet = "Table") {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("write_annotated_xlsx() needs the 'openxlsx' package.", call. = FALSE)
  }
  df <- if (is.data.frame(x)) x else as_annotated_df(x)

  stub_col   <- if (".label" %in% names(df)) ".label" else names(df)[1]
  styling    <- intersect(c(".indent", ".strong", ".emph"), names(df))
  data_cols  <- setdiff(names(df), c(stub_col, styling))
  n_data     <- length(data_cols)
  n_col      <- n_data + 1L                       # stub + data columns
  n_row      <- nrow(df)

  indent <- if (".indent" %in% names(df)) {
    iv <- suppressWarnings(as.integer(df[[".indent"]]))
    iv[is.na(iv)] <- 0L
    iv
  } else {
    rep(0L, n_row)
  }
  bold_row <- if (".strong" %in% names(df)) {
    bv <- suppressWarnings(as.logical(df[[".strong"]]))
    !is.na(bv) & bv
  } else {
    rep(FALSE, n_row)
  }

  # Two-level column spanners: split data-column NAMES on "||". `top` is the
  # spanner (empty for flat columns); `leaf` is the rendered header text, taken
  # from attr(col, "label") so the Big-N line ("\nN = 86") comes along.
  parts <- strsplit(data_cols, "||", fixed = TRUE)
  top   <- vapply(parts, function(p) if (length(p) > 1L) p[[1L]] else "", character(1))
  leaf  <- vapply(data_cols, function(cn) {
    lbl <- attr(df[[cn]], "label")
    if (is.null(lbl) || !nzchar(lbl)) utils::tail(strsplit(cn, "||", fixed = TRUE)[[1L]], 1L) else lbl
  }, character(1))
  has_spanner <- any(nzchar(top))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet)

  r <- 1L
  # Title (optional): merged across the table, bold, centred.
  if (!is.null(title) && nzchar(title)) {
    openxlsx::writeData(wb, sheet, title, startRow = r, startCol = 1L)
    openxlsx::mergeCells(wb, sheet, cols = seq_len(n_col), rows = r)
    openxlsx::addStyle(wb, sheet, openxlsx::createStyle(
      textDecoration = "bold", halign = "center", fontSize = 12),
      rows = r, cols = 1L)
    r <- r + 2L
  }

  header_style <- openxlsx::createStyle(
    textDecoration = "bold", halign = "center", valign = "bottom",
    wrapText = TRUE, border = "bottom", borderStyle = "medium")
  spanner_style <- openxlsx::createStyle(
    textDecoration = "bold", halign = "center", valign = "bottom",
    border = "bottom", borderStyle = "thin")

  header_top_row <- r
  if (has_spanner) {
    runs <- rle(top)
    pos <- 2L
    for (i in seq_along(runs$lengths)) {
      len <- runs$lengths[i]
      if (nzchar(runs$values[i])) {
        openxlsx::writeData(wb, sheet, runs$values[i], startRow = r, startCol = pos)
        if (len > 1L) {
          openxlsx::mergeCells(wb, sheet, cols = pos:(pos + len - 1L), rows = r)
        }
        openxlsx::addStyle(wb, sheet, spanner_style, rows = r,
                           cols = pos:(pos + len - 1L), gridExpand = TRUE)
      }
      pos <- pos + len
    }
    r <- r + 1L
  }
  # Leaf header row (stub header blank).
  openxlsx::writeData(wb, sheet, t(c("", leaf)), startRow = r, startCol = 1L,
                      colNames = FALSE)
  openxlsx::addStyle(wb, sheet, header_style, rows = r, cols = seq_len(n_col),
                     gridExpand = TRUE)
  header_bottom_row <- r
  r <- r + 1L

  body <- data.frame(
    stub = as.character(df[[stub_col]]),
    df[data_cols],
    check.names = FALSE, stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, sheet, body, startRow = r, startCol = 1L,
                      colNames = FALSE)

  num_style <- openxlsx::createStyle(halign = "right", valign = "top")
  for (i in seq_len(n_row)) {
    row_xl <- r + i - 1L
    stub_style <- openxlsx::createStyle(
      halign = "left", valign = "top", indent = indent[i],
      textDecoration = if (bold_row[i]) "bold" else NULL)
    openxlsx::addStyle(wb, sheet, stub_style, rows = row_xl, cols = 1L)
    cell_num <- if (bold_row[i]) {
      openxlsx::createStyle(halign = "right", valign = "top",
                            textDecoration = "bold")
    } else {
      num_style
    }
    openxlsx::addStyle(wb, sheet, cell_num, rows = row_xl,
                       cols = 2L:n_col, gridExpand = TRUE)
  }

  openxlsx::setColWidths(wb, sheet, cols = 1L, widths = 34)
  openxlsx::setColWidths(wb, sheet, cols = 2L:n_col, widths = 16)
  openxlsx::freezePane(wb, sheet, firstActiveRow = header_bottom_row + 1L)

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}
