#' Coerce a table object into an annotated data frame
#'
#' The blockr **annotated data frame** is a plain data frame whose reserved
#' `.`-columns describe a display table's structure: `.label` (row stub),
#' `.indent` (hierarchy depth), `.section_*` (grouping), `.strong` / `.emph`
#' (emphasis), `.fmt` (optional per-row format template); two-level column
#' spanners are encoded in the column name as `Top||Leaf`, and the column /
#' table labels carry the headers / title. It is what the blockr.viz table
#' renderer ([new_table_block()] / [html_table()]) consumes and what
#' [summary_table()] produces.
#'
#' `as_annotated_df()` is the broom-style hub: it coerces a table-producing
#' object (composer, gtsummary, gt, rtables, ...) into that data frame. Producer
#' packages register one S3 method each — e.g. blockr.sandbox provides the
#' composer methods (`as_annotated_df.composed_table` / `.gt_tbl`). The generic
#' lives here, with the renderer that defines the convention; the methods live
#' with their producers (composer is an internal package, so its method stays in
#' blockr.sandbox).
#'
#' @param x A table-producing object that has an `as_annotated_df` method.
#' @param ... Passed on to methods.
#'
#' @return An annotated data frame.
#' @seealso [new_table_block()], [summary_table()]
#' @export
as_annotated_df <- function(x, ...) {
  UseMethod("as_annotated_df")
}
