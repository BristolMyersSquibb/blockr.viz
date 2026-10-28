# The table's rules between arguments, applied after every change from any
# source. Pure, as chart_normalize().

# Applies the cross-argument rules to `state`; `prev` is the state before the
# change (NULL for a new block), `kinds` is unused so far.
# - groups: with no `summaries`, one count bar, "Count".
# - variables: a summary whose variable is also a `by` column is dropped.
# - `sort_by = "data"` resets `sort_dir`, so data order reads ascending.
# - the selection goes when `layout`, `by` or `drill` changes, and is never
#   held with a drill target.
# - `unit`, `drill_target` and `drill_table` are trimmed.
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
