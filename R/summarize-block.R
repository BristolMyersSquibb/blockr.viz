#' Summarize table block (superseded)
#'
#' Superseded by [new_table_block()] with `layout = "groups"`, which takes the
#' same summaries. This constructor translates its arguments and returns a
#' table block, so code and boards that use it keep working.
#'
#' @param ... Arguments of the former summarize table block (`by`,
#'   `summaries`, `id_var`, `group`, `value`, `func`, ...), translated to
#'   [new_table_block()]'s.
#'
#' @return A blockr transform block of class `table_block`.
#' @examplesIf interactive()
#' new_summarize_table_block(
#'   by = "Species",
#'   summaries = list(list(type = "simple", func = "count", show = "bar"))
#' )
#' @export
new_summarize_table_block <- function(...) {
  args <- table_legacy_args(list(...), from = "summarize")
  # A summarize table is the groups layout, also when nothing old was set.
  args$layout <- "groups"
  # A restore hands over the saved ctor; the result is a table block and
  # saves as one.
  args$ctor <- "new_table_block"
  args$ctor_pkg <- "blockr.viz"
  do.call(new_table_block, args)
}


