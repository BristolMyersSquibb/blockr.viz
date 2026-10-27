# The records layout's view: the cell model (R/table-model-records.R) as
# the table the browser draws, `derived$table` in the block's payload. Its
# shape is the TableModel of inst/js/table/table.d.ts: column-oriented, one
# vector per column, plain text and values, no markup.

#' A flat records table as the browser draws it
#'
#' @param m dt_flat_build()'s cell model, `kind = "flat"`.
#' @return A TableModel list: `n`, `stub`, `cols`, `head`, `widths`, `rows`
#'   and `drill`.
#' @noRd
table_records_view <- function(m) {
  stopifnot(identical(m$kind, "flat"))
  cols <- c(list(table_view_col(m$stub)), lapply(m$cells, table_view_col))
  head <- c(
    list(table_view_head(m$stub, 0L, stub = TRUE)),
    lapply(seq_along(m$cells), function(j) {
      table_view_head(m$cells[[j]], j)
    })
  )
  out <- list(
    n = m$n, stub = 0L, cols = cols, head = list(head),
    widths = as.numeric(m$widths),
    rows = if (!is.null(m$stub$tint)) list(tint = m$stub$tint),
    drill = m$attr_args$onclick_col
  )
  out[!vapply(out, is.null, logical(1L))]
}

# One column of the cell model as a TableCol: name, numeric flag, display
# text and raw values, and the paint as values.
table_view_col <- function(cc) {
  out <- list(name = cc$name, num = isTRUE(cc$num), disp = cc$disp,
              raw = cc$raw, bg = cc$bg, fg = cc$fg, bar = cc$bar,
              bar_fill = cc$bar_fill)
  out[!vapply(out, is.null, logical(1L))]
}

# A leaf header cell: the column's name, its label on the second line.
table_view_head <- function(cc, col, stub = FALSE) {
  out <- list(text = cc$name, sub = cc$label, col = col,
              stub = if (stub) TRUE)
  out[!vapply(out, is.null, logical(1L))]
}
