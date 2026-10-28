#' Summary table block (superseded)
#'
#' Superseded by [new_table_block()] with `layout = "variables"`, which draws
#' the same table and takes the same settings. This constructor translates
#' its arguments (`vars`, `by`, `stats`, `add_overall`, ...) and returns a
#' table block, so code and boards that use it keep working.
#'
#' @param ... Arguments of the former summary table block, translated to
#'   [new_table_block()]'s.
#'
#' @return A blockr transform block of class `table_block`.
#' @examplesIf interactive()
#' new_summary_table_block(vars = c("AGE", "SEX"), by = "ARM")
#' @export
new_summary_table_block <- function(...) {
  args <- table_legacy_args(list(...), from = "summary")
  args$ctor <- "new_table_block"
  args$ctor_pkg <- "blockr.viz"
  do.call(new_table_block, args)
}
