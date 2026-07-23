#' Write an annotated data frame to a styled Excel (.xlsx) file
#'
#' Renders an [annotated data frame][as_annotated_df()] to an Excel worksheet
#' that preserves the table's *structure*: row indentation (`.indent` ->
#' native cell indent), bold header rows (`.strong`), and two-level column
#' spanners (`Top||Leaf` -> merged header cells). The same frame that drives the
#' blockr.viz table renderer also drives this export -- one structured artifact,
#' two outputs.
#'
#' @details
#' Input is the annotated-data-frame contract:
#' \itemize{
#'   \item `.label` -> the row-stub (first) column; `.indent` -> the Excel cell
#'     indent level (the hierarchy reads in the spreadsheet outline, not via
#'     faked spaces); `.strong` rows are bold.
#'   \item `.section_1, ..., .section_k` -> bold section-header rows
#'     interleaved at each section restart (mirroring the HTML renderer);
#'     these and any other dot-prefixed columns are structure, never exported
#'     as data columns.
#'   \item data columns carry their pre-formatted strings. A column named with a
#'     `"||"` (`"Placebo||CAT1"`) is a two-level spanner: the part before the
#'     `"||"` becomes a merged top header cell; `attr(col, "label")`
#'     (`"Placebo\nN = 86"`) is the leaf header text.
#' }
#' The header is frozen, bordered, and bold; numbers right-align, the stub
#' left-aligns. No blockr / Shiny dependency -- just the data frame and openxlsx.
#'
#' @param x An annotated data frame, or any object with an [as_annotated_df()]
#'   method (e.g. a composer `composed_table`), which is coerced first.
#' @param file Path to write the `.xlsx` to.
#' @param title Optional title, written merged-and-bold above the table.
#' @param subtitle Optional subtitle, written merged-and-italic under the
#'   title (or where the title would sit, when only a subtitle is given).
#' @param caption Optional caption / footnote line, written italic below the
#'   table.
#' @param sheet Worksheet name.
#'
#' @return `file`, invisibly.
#' @seealso [as_annotated_df()], [new_table_block()]
#' @export
write_annotated_xlsx <- function(x, file, title = NULL, subtitle = NULL,
                                 caption = NULL, sheet = "Table") {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("write_annotated_xlsx() needs the 'openxlsx' package.", call. = FALSE)
  }
  df <- if (is.data.frame(x)) x else as_annotated_df(x)

  stub_col   <- if (".label" %in% names(df)) ".label" else names(df)[1]
  # ALL dot-prefixed columns are structure, not data (the annotated-df
  # contract): .indent/.strong/.emph style rows, the .group<k>*/.variable*
  # identity pairs nest them (rendered as bold section-header rows below,
  # mirroring build_html_tbody()), and any other dotted plumbing column must
  # never leak into the export as a dotted-headed value column.
  data_cols  <- setdiff(names(df), stub_col)
  data_cols  <- data_cols[!startsWith(data_cols, ".")]
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
  # Title / subtitle (optional): each merged across the table, title bold and
  # centred, subtitle italic under it; one blank spacer row after the pair
  # (title alone keeps the historical title-row + spacer layout).
  head_rows <- 0L
  if (!is.null(title) && nzchar(title)) {
    openxlsx::writeData(wb, sheet, title, startRow = r, startCol = 1L)
    openxlsx::mergeCells(wb, sheet, cols = seq_len(n_col), rows = r)
    openxlsx::addStyle(wb, sheet, openxlsx::createStyle(
      textDecoration = "bold", halign = "center", fontSize = 12),
      rows = r, cols = 1L)
    r <- r + 1L
    head_rows <- head_rows + 1L
  }
  if (!is.null(subtitle) && nzchar(subtitle)) {
    openxlsx::writeData(wb, sheet, subtitle, startRow = r, startCol = 1L)
    openxlsx::mergeCells(wb, sheet, cols = seq_len(n_col), rows = r)
    openxlsx::addStyle(wb, sheet, openxlsx::createStyle(
      textDecoration = "italic", halign = "center", fontSize = 11,
      fontColour = "#6b7280"),
      rows = r, cols = 1L)
    r <- r + 1L
    head_rows <- head_rows + 1L
  }
  if (head_rows > 0L) r <- r + 1L

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

  # ---- row-side sections ----------------------------------------------
  # Interleave one bold header row per section restart, exactly where the HTML
  # renderer puts its colspan header rows (see build_html_tbody()): for each
  # data row, the outermost changed level and every level below it emit a
  # header. Header text carries the section column's label prefix when one is
  # set; nesting shows as cell indent (level - 1).
  view <- annotated_structure_view(df)
  df <- view$data
  section_cols <- view$section_cols
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

  # Output positions (1-based within the body block): each data row is pushed
  # down by the header rows emitted so far.
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
    # A level-L header sits (k - L + 1) rows above its data row, below the
    # outer levels emitted at the same restart.
    hdr_pos <- c(hdr_pos, data_pos[at] - (k - L + 1L))
    hdr_txt <- c(hdr_txt, txt)
    hdr_lvl <- c(hdr_lvl, rep(L, length(at)))
  }

  body <- data.frame(
    stub = as.character(df[[stub_col]]),
    df[data_cols],
    check.names = FALSE, stringsAsFactors = FALSE
  )
  # Expand to the interleaved layout: all-NA (blank) rows at the header
  # positions, then drop the header texts into the stub column. Indexing with
  # NA yields NA rows while preserving each data column's type.
  total_out <- n_row + length(hdr_pos)
  out_body <- body[rep(NA_integer_, total_out), , drop = FALSE]
  out_body[data_pos, ] <- body
  if (length(hdr_pos)) out_body[[1L]][hdr_pos] <- hdr_txt
  openxlsx::writeData(wb, sheet, out_body, startRow = r, startCol = 1L,
                      colNames = FALSE)

  # ---- body styles (batched) ------------------------------------------
  # The style space is tiny -- (indent x bold) for the stub column, bold|plain
  # for data cells -- so create ONE style per distinct combo and addStyle it
  # with the full vector of matching rows. addStyle is openxlsx's most
  # expensive call; the previous two-calls-per-row loop made a 10k-row export
  # take minutes.
  data_xl <- r + data_pos - 1L
  stub_xl    <- c(data_xl, r + hdr_pos - 1L)
  stub_indent <- c(indent, hdr_lvl - 1L)          # header indent = level - 1
  stub_bold   <- c(bold_row, rep(TRUE, length(hdr_pos)))
  for (g in split(seq_along(stub_xl), paste(stub_indent, stub_bold))) {
    st <- openxlsx::createStyle(
      halign = "left", valign = "top", indent = stub_indent[g[1L]],
      textDecoration = if (stub_bold[g[1L]]) "bold" else NULL)
    openxlsx::addStyle(wb, sheet, st, rows = stub_xl[g], cols = 1L,
                       gridExpand = TRUE)
  }
  if (n_data > 0L) {
    num_style <- openxlsx::createStyle(halign = "right", valign = "top")
    num_bold  <- openxlsx::createStyle(halign = "right", valign = "top",
                                       textDecoration = "bold")
    data_cols_xl <- 1L + seq_len(n_data)
    if (any(!bold_row)) {
      openxlsx::addStyle(wb, sheet, num_style, rows = data_xl[!bold_row],
                         cols = data_cols_xl, gridExpand = TRUE)
    }
    if (any(bold_row)) {
      openxlsx::addStyle(wb, sheet, num_bold, rows = data_xl[bold_row],
                         cols = data_cols_xl, gridExpand = TRUE)
    }
  }

  # Caption (optional): an italic footnote line one blank row below the body,
  # left-aligned in the stub column (a footnote reads from the margin, unlike
  # the centred title).
  if (!is.null(caption) && nzchar(caption)) {
    cap_r <- r + total_out + 1L
    openxlsx::writeData(wb, sheet, caption, startRow = cap_r, startCol = 1L)
    openxlsx::mergeCells(wb, sheet, cols = seq_len(n_col), rows = cap_r)
    openxlsx::addStyle(wb, sheet, openxlsx::createStyle(
      textDecoration = "italic", halign = "left", fontSize = 9,
      fontColour = "#6b7280"),
      rows = cap_r, cols = 1L)
  }

  openxlsx::setColWidths(wb, sheet, cols = 1L, widths = 34)
  # seq_len() guard: with no data columns 2L:n_col would be the REVERSED 2:1
  # and style/widen columns 2 and 1 instead of nothing.
  if (n_data > 0L) {
    openxlsx::setColWidths(wb, sheet, cols = 1L + seq_len(n_data), widths = 16)
  }
  openxlsx::freezePane(wb, sheet, firstActiveRow = header_bottom_row + 1L)

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}
