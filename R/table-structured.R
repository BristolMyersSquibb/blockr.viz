# The structured table as the browser draws it: an annotated frame (dotted
# structure columns, `||` spanner paths, section columns) as the TableModel
# of inst/js/table/table.d.ts: the browser gets the parts and builds the
# table.
#
#   R/table-structured.R           the model: columns, widths, rows
#   R/table-structured-head.R      the header rows
#   R/table-structured-sections.R  the section headers and the drill claims

#' A structured frame as the browser draws it
#'
#' Reads the frame this way: the `.label`
#' column is the stub, `.indent` / `.strong` / `.emph` style the rows, the
#' `.group<k>_level` columns (and the blocks of `.variable_label`) are the
#' sections, and every other dotted column stays out of the cells.
#'
#' @param data A structured frame (`table_is_structured()`), a Table 1 or the
#'   rows nested under `by` (`table_nest()`).
#' @param drill The drill state; a value other than `NULL` or `""` puts the
#'   claims on the rows, the sections, the headers and the columns. A column
#'   name there is the drill column of rows nested under `by`. The model has
#'   no `drill` field: that one is a flat table's column (TableModel).
#' @param digits The number format for numeric cells.
#' @param unit The unit that heads numeric columns, or `NULL`.
#' @param collapsible Whether the groups fold. The Grid takes this as a flag
#'   with the model (`TableGridFlags`); the model is the same either way.
#' @return A TableModel list: `n`, `stub`, `cols`, `head`, `widths`, `rows`
#'   and `sections`. `NULL` when there is nothing to draw (no rows,
#'   or no data column left after the structure columns), where the old path
#'   drew a message.
#' @noRd
table_structured_view <- function(data, drill = NULL, digits = "number",
                                  unit = NULL, collapsible = TRUE) {
  p <- table_structured_parts(data)
  data <- table_structured_numbers(p$data, p$data_cols, digits, unit)
  if (length(p$data_cols) == 0L || nrow(data) == 0L) {
    return(NULL)
  }
  drill_on <- is.character(drill) && length(drill) == 1L && !is.na(drill) &&
    nzchar(drill)
  col_keys <- if (drill_on) annotated_column_keys(data)
  stub <- p$stub_col
  cols <- c(
    if (!is.null(stub)) list(table_structured_col(data, stub, drill_on)),
    lapply(p$data_cols, function(cn) {
      table_structured_col(data, cn, drill_on, col_keys[[cn]])
    })
  )
  out <- list(
    n = nrow(data),
    stub = if (!is.null(stub)) 0L,
    cols = cols,
    head = table_structured_head(data, p$data_cols, stub, col_keys),
    widths = table_structured_widths(data, p$data_cols, stub),
    rows = table_structured_rows(data, p$section_cols, drill_on, drill),
    sections = table_structured_sections(data, p$section_cols, drill_on)
  )
  table_view_arrays(out[!vapply(out, is.null, logical(1L))])
}

# The frame split into what the table draws: the data with its synthetic
# `.variable_block` column, the section columns, the stub, the styling
# columns and the data columns.
table_structured_parts <- function(data) {
  view <- annotated_structure_view(data)
  data <- view$data
  stub_col <- if (".label" %in% names(data)) ".label"
  styling <- intersect(c(".indent", ".strong", ".emph"), names(data))
  list(
    data = data, section_cols = view$section_cols, stub_col = stub_col,
    data_cols = setdiff(names(data), c(view$reserved_cols, stub_col, styling))
  )
}

# One body column as a TableCol. A missing value is `null` (the browser
# draws the dash) unless the cell is painted, where an empty cell is part of
# the picture. The stub keeps its raw text for the drill; the data columns
# are right-aligned, so they are all numeric to the browser. `keys` is the
# column's half of a cell claim.
table_structured_col <- function(data, name, drill_on, keys = NULL) {
  x <- data[[name]]
  disp <- as.character(x)
  stub <- identical(name, ".label")
  paint <- if (!stub) annotation_cell_paint(data, name)
  if (!is.null(paint)) {
    disp[is.na(disp) & !is.na(paint$bg)] <- ""
  }
  out <- list(
    name = name, num = if (!stub) TRUE, disp = disp,
    raw = if (stub && drill_on) as.character(x),
    bg = paint$bg, fg = paint$fg,
    keys = if (length(keys)) table_structured_claim(
      vapply(keys, function(p) as.character(p[["column"]]), ""),
      lapply(keys, function(p) as.character(p[["values"]]))
    )
  )
  out[!vapply(out, is.null, logical(1L))]
}

# Column widths in px, stub first: the estimate structured_colgroup() makes,
# as numbers. The stub gets room for the deepest indent.
table_structured_widths <- function(data, data_cols, stub_col,
                                    wrap_names = TRUE) {
  header_texts <- vapply(data_cols, function(col) dt_header_text(data, col),
                         character(1L))
  cells <- lapply(data_cols, function(col) as.character(data[[col]]))
  extra <- rep(0L, length(data_cols))
  if (!is.null(stub_col)) {
    header_texts <- c("", header_texts)
    cells <- c(list(as.character(data[[stub_col]])), cells)
    max_indent <- if (".indent" %in% names(data)) {
      suppressWarnings(max(c(0, as.numeric(data$.indent)), na.rm = TRUE))
    } else {
      0
    }
    extra <- c(8L + 16L * as.integer(max_indent), extra)
  }
  as.numeric(blockr.ui::column_widths_px(
    col_names = header_texts,
    col_labels = character(length(header_texts)),
    formatted = lapply(cells, as.character),
    wrap_names = wrap_names
  ) + extra)
}

# Per-row extras: indent, strong and emph from the styling columns, and the
# claim each row makes when the drill is on. `NULL` when the frame has none.
table_structured_rows <- function(data, section_cols, drill_on, drill) {
  flag <- function(col) {
    if (!col %in% names(data)) return(NULL)
    b <- suppressWarnings(as.logical(data[[col]]))
    !is.na(b) & b
  }
  indent <- if (".indent" %in% names(data)) {
    i <- suppressWarnings(as.integer(data[[".indent"]]))
    i[is.na(i) | i < 0L] <- 0L
    i
  }
  keys <- if (drill_on) {
    table_structured_row_keys(data, section_cols, drill_col = drill)
  }
  out <- list(indent = indent, strong = flag(".strong"), emph = flag(".emph"),
              keys = keys)
  out <- out[!vapply(out, is.null, logical(1L))]
  if (length(out)) out
}
