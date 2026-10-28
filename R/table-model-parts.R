# The pieces of a TableModel (inst/js/table/table.d.ts) every layout builds
# with: a column, a leaf header cell, and the vectors marked as JSON arrays.
# The model is column-oriented, one vector per column, plain text and values,
# no markup.

#' Per-row vectors as JSON arrays
#'
#' Shiny writes an output's value with `auto_unbox`, so a one-row table's
#' columns would arrive as bare strings. Every vector the TableModel holds
#' per row or per column is marked as an array.
#' @param m A TableModel list.
#' @return The same list.
#' @noRd
table_model_arrays <- function(m) {
  arr <- function(x) if (is.null(x) || is.list(x)) x else I(unname(x))
  per_col <- c("disp", "raw", "key", "bg", "fg", "bar")
  m$cols <- lapply(m$cols, function(cc) {
    cc[intersect(per_col, names(cc))] <- lapply(cc[intersect(per_col,
                                                             names(cc))], arr)
    cc
  })
  if (!is.null(m$rows)) {
    keep <- setdiff(names(m$rows), "keys")
    m$rows[keep] <- lapply(m$rows[keep], arr)
  }
  if (!is.null(m$widths)) m$widths <- arr(m$widths)
  m
}

# One column of the cell model as a TableCol: name, numeric flag, display
# text and raw values, and the paint as values.
table_model_col <- function(cc) {
  out <- list(name = cc$name, num = isTRUE(cc$num), disp = cc$disp,
              raw = cc$raw, bg = cc$bg, fg = cc$fg, bar = cc$bar,
              bar_fill = cc$bar_fill)
  out[!vapply(out, is.null, logical(1L))]
}

# A leaf header cell: the column's name, its label on the second line.
table_model_head <- function(cc, col, stub = FALSE) {
  out <- list(text = cc$name, sub = cc$label, col = col,
              stub = if (stub) TRUE)
  out[!vapply(out, is.null, logical(1L))]
}
