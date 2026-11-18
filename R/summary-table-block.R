#' Summary table block (deprecated)
#'
#' Deprecated: use [new_table_block()] with `layout = "variables"`, which
#' draws the same table and takes the same settings. This constructor
#' translates its arguments (`vars`, `by`, `stats`, `add_overall`, ...) and
#' returns a table block, with a deprecation warning. Boards saved with it
#' restore as table blocks, show a toast that asks the user to save the
#' board, and save as table blocks from then on.
#'
#' @param ... Arguments of the former summary table block, translated to
#'   [new_table_block()]'s.
#'
#' @return A blockr transform block of class `table_block`.
#' @examplesIf interactive()
#' new_summary_table_block(vars = c("AGE", "SEX"), by = "ARM")
#' @export
new_summary_table_block <- function(...) {
  args <- list(...)
  if (is.null(args$ctor)) {
    viz_deprecated_warning("new_summary_table_block",
                           "new_table_block(layout = \"variables\")")
  } else {
    viz_deprecated_toast()
  }
  args <- legacy_table_args(args, from = "summary")
  # A summary table is the variables layout, also when nothing old was set.
  args$layout <- "variables"
  args$ctor <- "new_table_block"
  args$ctor_pkg <- "blockr.viz"
  do.call(new_table_block, args)
}
