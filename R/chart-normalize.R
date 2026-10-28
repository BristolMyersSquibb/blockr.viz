# The chart's rules between arguments, applied after every change from any
# source (constructor, gear, external control, restored board). One pure
# function of the state and the state before the change. Per-argument checks
# are the spec's (viz_check()); these are only the rules that read two
# arguments.

# The aggregated types that cannot lie either way.
NORM_CHART_FIXED <- c("waterfall", "pie", "treemap")

# Apply the rules to `state` (a named list under the spec's names), given
# `prev` (the state before the change, NULL for a new block).
# - Waterfall, pie and treemap have no `orientation`. A bar, boxplot or
#   pointrange keeps it only where it differs from the type's own way
#   (horizontal bars, vertical boxes and point ranges). It maps nothing:
#   `category` and `value` stay as they are when it changes.
# - A histogram has no `y`, orientation or sort.
# - `ymin` and `ymax` are pairs: an unmatched end is dropped. In the gear a
#   half-picked pair is a normal state while editing; in code a mismatched
#   pair is a mistake, so new_chart_block() refuses it before this runs.
# - The selection goes when a mapping or the drill changes, and is never held
#   with a drill target.
# - `unit`, `drill_target` and `drill_table` are trimmed.
# The roles of the other family stay in the state, unused, so a chart
# switched to a scatter and back keeps its category and value.
chart_normalize <- function(state, prev = NULL) {
  state <- trim_settings(state, c("unit", "drill_target", "drill_table"))
  type <- state[["type"]] %||% "bar"
  if (identical(type, "histogram")) {
    state[c("y", "orientation", "sort_by", "sort_dir")] <- list(NULL)
  } else if (type %in% NORM_CHART_FIXED) {
    state["orientation"] <- list(NULL)
  } else if (type %in% CHART_ORIENTED_TYPES &&
               identical(state[["orientation"]],
                         chart_default_orientation(type))) {
    state["orientation"] <- list(NULL)
  }
  state <- chart_pair_intervals(state)
  drop_selection(state, prev, c("type", "category", "value", "x", "y",
                                "color", "facet", "series", "xend", "drill"))
}

chart_pair_intervals <- function(state) {
  lo <- state[["ymin"]]
  hi <- state[["ymax"]]
  if (length(lo) == length(hi)) {
    return(state)
  }
  n <- min(length(lo), length(hi))
  state[c("ymin", "ymax")] <- list(
    if (n) lo[seq_len(n)], if (n) hi[seq_len(n)]
  )
  state
}
