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
  args[c("ctor", "ctor_pkg")] <- NULL
  do.call(new_table_block, args)
}

summary_table_block_dep <- memoise0(function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "summary-table-block-js",
      version = paste0(utils::packageVersion("blockr.viz"), ".5"),
      src = system.file("js", package = "blockr.viz"),
      script = "summary-table-block.js"
    ),
    htmltools::htmlDependency(
      name = "summary-table-block-css",
      version = utils::packageVersion("blockr.viz"),
      src = system.file("css", package = "blockr.viz"),
      stylesheet = "summary-table-block.css"
    )
  )
})
