#' Table block
#'
#' A block that shows its input as an interactive table and, when asked to,
#' filters the blocks downstream by the row a reader clicks. Its output is
#' its input, filtered by the last click.
#'
#' @section Layouts:
#' `layout` decides what a row is:
#'
#' * `"records"`: the input's rows, with search, sort and cell colors
#'   (`shadings`, `color`). With `by`, rows sit under one collapsible header
#'   per level. A finished table from R code (a structured or annotated data
#'   frame, see [as_annotated_df()]) is drawn as it comes.
#' * `"groups"`: one row per level of `by` (or one row without it), one
#'   column per entry of `summaries`: counts and statistics as numbers, bars,
#'   dots, boxes, point ranges, sparklines or spans.
#' * `"variables"`: a Table 1. One row per statistic or level of each
#'   variable in `summaries`, one column per level of `by`, with `stats`,
#'   `sections` and a `total` column.
#'
#' `look` draws any layout as a `"table"`, a `"compact"` matrix (rotated
#' column titles, tiles as a heatmap) or `"cards"` (one card per summary in
#' groups, per column in records).
#'
#' @section Clicks:
#' A click filters only when `drill` is set. In records it names the column
#' a click filters on, or `"auto"`; in groups and variables `"auto"` filters
#' on the clicked row's groups. With `drill_target` set, a click is sent to
#' that filter block instead, and the table keeps no selection of its own.
#'
#' @section Older code and boards:
#' Argument names of blockr.viz before 0.3.0 (`value`, `group`, `pct_of`,
#' `axis`, ...) still work in code, with a warning. Boards saved with the
#' table, tile and summary blocks restore as table blocks. A value an
#' argument does not take is an error. Any other name is kept as a block
#' attribute, for blockr.core and other packages (blockr.dock's `visible`).
#'
#' @eval viz_spec_params("table", dots = paste(
#'   "Arguments for [blockr.core::new_block()] (`block_name`, ...), and",
#'   "argument names of blockr.viz before 0.3.0, which are translated with a",
#'   "warning."
#' ))
#' @eval viz_spec_section("Summaries", paste(
#'   "Each entry of `summaries` is a list with a `type` and the keys of that",
#'   "type. Keys left out take their default."
#' ), table_summary_specs(), key = "type")
#' @eval viz_spec_section("Shadings",
#'   "Each entry of `shadings` is a list with these keys.",
#'   table_shading_spec())
#' @return A table block (class `table_block`), for a blockr board.
#' @seealso [new_chart_block()], [as_annotated_df()]
#' @examples
#' # Records: the input's rows, two columns, filtering on a click
#' new_table_block(columns = c("Species", "Sepal.Length"), drill = "auto")
#'
#' # Groups: per species a row count, a mean and a box of petal lengths
#' new_table_block(
#'   layout = "groups", by = "Species",
#'   summaries = list(
#'     list(type = "simple", func = "count"),
#'     list(type = "simple", func = "mean", col = "Sepal.Length"),
#'     list(type = "dist", col = "Petal.Length")
#'   )
#' )
#'
#' # The same as cards
#' new_table_block(
#'   layout = "groups", look = "cards",
#'   summaries = list(list(type = "simple", func = "count"))
#' )
#'
#' # Variables: a Table 1, one column per species
#' new_table_block(layout = "variables", by = "Species")
#'
#' if (interactive()) {
#'   blockr.core::serve(
#'     new_table_block(layout = "groups", by = "Species",
#'                     summaries = list(list(type = "simple", func = "count")),
#'                     drill = "auto"),
#'     data = list(data = iris)
#'   )
#' }
#' @name new_table_block
NULL
