#' Report Call for a Block's Printed Result
#'
#' How a block's result is printed in a rendered document (a blockr.outline
#' report or deck). Returns a call over `var`, the variable the generated
#' script binds the block's result to, such as
#' `blockr.viz::static_table(tbl1)`, or `NULL` when the bare result prints
#' as is. Consumers deparse the call into the document, so it is
#' self-qualified and holds only literal values.
#'
#' The chart block has no method: a document carries the picture the browser
#' captured (see [chart_capture_ids()]).
#'
#' @param x A block object.
#' @param var The variable name (string) holding the block's result in the
#'   generated document.
#' @param ... Reserved.
#'
#' @return A call, or `NULL` for the bare print.
#'
#' @export
report_call <- function(x, var, ...) {
  UseMethod("report_call")
}

#' @rdname report_call
#' @export
report_call.default <- function(x, var, ...) {
  NULL
}

# Whether `v` equals `default`, ignoring the int / dbl and scalar drift of a
# round-trip through state.
identical_default <- function(v, default) {
  if (is.null(default)) {
    return(FALSE)
  }

  length(v) == 1L && length(default) == 1L &&
    !is.na(v) && identical(as.character(v), as.character(default))
}
