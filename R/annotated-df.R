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

# THE input contract of the renderer blocks (chart / table / tile / gt): a
# data frame (plain or annotated) or an object that coerces through
# as_annotated_df(). One definition, shared by every block's `dat_valid` --
# see dev/table-and-chart-architecture.md, "Shape contract".
can_coerce_annotated_df <- function(x) {
  is.data.frame(x) || has_annotated_df_method(x)
}

# The one `dat_valid` body all renderer blocks use. Contract check only --
# dispatch lookup, never the (possibly costly) coercion itself. A method that
# exists but refuses this particular value errors at eval time instead; core
# surfaces both the same way.
validate_annotated_df_input <- function(data) {
  if (!can_coerce_annotated_df(data)) {
    stop(
      "Input must be a data frame (e.g. a summary_table() annotated frame) ",
      "or an object with an as_annotated_df() method (e.g. a composer ",
      "table); got <", paste(class(data), collapse = "/"), ">"
    )
  }
}

# The reserved annotation columns of the convention (see as_annotated_df()
# roxygen and fmt-cells.R): row stub / hierarchy / grouping / emphasis / the
# `.fmt` template and its `.digits` precision sibling. Other dot-prefixed
# names (`.count`, user columns) are NOT reserved and pass through.
ANNOTATION_COLS <- c(".label", ".indent", ".strong", ".emph", ".fmt",
                     ".digits")

#' Coerce a table object into a plain data frame
#'
#' [as_annotated_df()] followed by dropping the reserved `.`-annotation
#' columns (`.label`, `.indent`, `.strong`, `.emph`, `.fmt`, `.digits`,
#' `.section_*`). This is what the chart / tile blocks consume when a
#' table-producing object (a composer table, a gtsummary table, ...) is
#' connected: a chart of a structured display table charts its **data
#' columns**; the row-structure rendering belongs to the table renderers.
#' Other dot-prefixed columns are not reserved and pass through.
#'
#' @param x A data frame or an object with an [as_annotated_df()] method.
#' @param ... Passed on to [as_annotated_df()].
#'
#' @return A data frame without the reserved annotation columns.
#' @seealso [as_annotated_df()], [new_chart_block()], [new_tile_block()]
#' @export
as_plain_df <- function(x, ...) {
  x <- as_annotated_df(x, ...)
  drop <- names(x) %in% ANNOTATION_COLS |
    grepl("^\\.section_\\d+$", names(x))
  x[, !drop, drop = FALSE]
}

# Renderer-side input coercion for the chart / tile: a plain data frame
# passes through UNTOUCHED (byte-identical to the pre-contract behavior,
# dotted columns included); anything else is coerced + stripped via
# as_plain_df(). Coercion failures return NULL so reader reactives park on
# their `req(is.data.frame(d))` guards instead of erroring an observer (an
# unhandled observer error is fatal to the Shiny session) -- the block
# condition (from dat_valid / the block expr, which coerces the same way)
# does the explaining.
coerce_plain_df <- function(d) {
  if (is.data.frame(d)) return(d)
  tryCatch(as_plain_df(d), error = function(e) NULL)
}

# Rewrite an emitted `dplyr::filter(.(data), <cond>)` call so its data slot
# is coerced: `dplyr::filter(blockr.viz::as_plain_df(.(data)), <cond>)`.
# Used by the chart / tile exprs ONLY when the input is not already a data
# frame, so plain-data-frame inputs keep their exact pre-contract expr and
# the emitted code stands alone (self-qualified).
wrap_plain_df_input <- function(ex) {
  ex[[2L]] <- as.call(list(quote(blockr.viz::as_plain_df), ex[[2L]]))
  ex
}
