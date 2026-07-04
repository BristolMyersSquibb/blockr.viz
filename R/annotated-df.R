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
#' packages register one S3 method each -- e.g. blockr.sandbox provides the
#' composer methods (`as_annotated_df.composed_table` / `.gt_tbl`). The generic
#' lives here, with the renderer that defines the convention; the methods live
#' with their producers (composer is an internal package, so its method stays in
#' blockr.sandbox).
#'
#' Consumers call the generic unconditionally: a data frame passes through
#' untouched (it is already the target format), so coercion is idempotent and
#' explicit `as_annotated_df()` steps upstream of a coercing consumer (such as
#' [new_table_block()]) are harmless.
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

#' @rdname as_annotated_df
#' @export
as_annotated_df.data.frame <- function(x, ...) {
  x
}

#' @rdname as_annotated_df
#' @export
as_annotated_df.default <- function(x, ...) {
  stop(
    "No as_annotated_df() method for <",
    paste(class(x), collapse = "/"),
    ">. Supply a data frame, or an object whose package registers an ",
    "as_annotated_df() method (e.g. a composer table via blockr.sandbox).",
    call. = FALSE
  )
}

# Whether `x` would dispatch to a real (non-default) as_annotated_df() method.
# The cheap contract check behind consumer blocks' `dat_valid`: method lookup
# only, never runs the coercion (a method may still refuse a particular value
# at eval time, e.g. composer paged listings). The default method is excluded
# -- it exists only to raise the helpful error above, so counting it would
# make every object "valid".
has_annotated_df_method <- function(x) {
  found <- function(cls) {
    !is.null(utils::getS3method("as_annotated_df", cls, optional = TRUE))
  }
  any(vapply(class(x), found, logical(1L)))
}
