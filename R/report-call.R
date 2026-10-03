#' Report Call for a Block's Printed Result
#'
#' How a block's result should be printed in a rendered document (a
#' blockr.outline report or deck). Returns a call object over `var`, the
#' variable the generated script binds the block's result to -- e.g.
#' `blockr.viz::static_table(tbl1)` -- or `NULL` when the bare result prints
#' fine as-is. Consumers (blockr.outline) deparse the call into the document,
#' so it must be self-qualified and contain only literal values.
#'
#' The chart block has no method: its picture is drawn in the browser, and a
#' document carries the picture the browser captured (see
#' [chart_capture_ids()]).
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

# Default comparison that tolerates the length-1 / scalar and int / dbl
# drift a round-trip through state can introduce.
identical_default <- function(v, default) {

  if (is.null(default)) {
    return(FALSE)
  }

  length(v) == 1L && length(default) == 1L &&
    !is.na(v) && identical(as.character(v), as.character(default))
}
