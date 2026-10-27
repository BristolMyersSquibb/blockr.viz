# The table's rules between arguments, applied after every change from any
# source. A pure function of the state, the state before the change and the
# data's column kinds, as chart_normalize().

#' Apply the table's cross-argument rules
#'
#' - The groups layout always has a column: with no `summaries` it gets one
#'   count bar, "Count" (as the reference did in the constructor, on a
#'   switch to groups and when the gear removed the last column).
#' - In the variables layout a summary whose variable is also a `by` column
#'   is dropped: it cannot be a row and a column at once.
#' - Picking `sort_by = "data"` resets `sort_dir`, so data order reads
#'   ascending (the gear did this).
#' - The selection goes when the layout, `by` or the drill changes, and is
#'   never held with a drill target.
#' - `unit`, `drill_target` and `drill_table` are trimmed.
#'
#' @param state The table's state: a named list under the spec's names.
#' @param prev The state before the change, or `NULL` (a new block).
#' @param kinds The data's column kinds, or `NULL` without data. No table
#'   rule reads them yet.
#' @return The state after the rules.
#' @noRd
table_normalize <- function(state, prev = NULL, kinds = NULL) {
  state <- trim_settings(state, c("unit", "drill_target", "drill_table"))
  layout <- state[["layout"]] %||% "records"
  if (identical(layout, "groups") && !length(state[["summaries"]])) {
    state$summaries <- list(table_seed_summary())
  }
  if (identical(layout, "variables") && length(state[["by"]])) {
    keep <- !vapply(state[["summaries"]] %||% list(), function(e) {
      any(table_summaries_vars(list(e)) %in% state$by)
    }, logical(1L))
    state["summaries"] <- list(state[["summaries"]][keep])
  }
  if (changed(state, prev, "sort_by") && identical(state[["sort_by"]], "data")) {
    state["sort_dir"] <- list(NULL)
  }
  drop_selection(state, prev, c("layout", "by", "drill"))
}

# The one column a groups table starts with, in the spec's checked form.
table_seed_summary <- function() {
  seed <- list(type = "simple", name = "Count", func = "count", show = "bar")
  spec <- viz_block_spec("table")
  viz_check_value(spec$args$summaries, list(seed), "summaries")[[1L]]
}
