#' Summarize table block (superseded)
#'
#' Superseded by [new_table_block()] with `layout = "groups"`, which takes the
#' same summaries. This constructor translates its arguments and returns a
#' table block, so code and boards that use it keep working.
#'
#' @param ... Arguments of the former summarize table block (`by`,
#'   `summaries`, `id_var`, `group`, `value`, `func`, ...), translated to
#'   [new_table_block()]'s. `missing` defaults to `"drop"`: the old block
#'   drew no row for a missing group.
#'
#' @return A blockr transform block of class `table_block`.
#' @examplesIf interactive()
#' new_summarize_table_block(
#'   by = "Species",
#'   summaries = list(list(type = "simple", func = "count", show = "bar"))
#' )
#' @export
new_summarize_table_block <- function(...) {
  args <- legacy_table_args(list(...), from = "summarize")
  args$layout <- "groups"
  # The old summarize table drew no row for a missing group (0.2.200).
  args$missing <- args$missing %||% "drop"
  # The result saves as a table block, also after a restore.
  args$ctor <- "new_table_block"
  args$ctor_pkg <- "blockr.viz"
  do.call(new_table_block, args)
}
