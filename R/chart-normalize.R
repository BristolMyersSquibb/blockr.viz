# The chart's rules between arguments, applied after every change from any
# source: the constructor, the gear, external control, a restored board.
# One pure function of the state, the state before the change and the
# data's column kinds (viz_column_kinds()). Per-argument checks are the
# spec's (viz_check()); these are only the rules that read two arguments.

# Types that can lie either way, and the aggregated types that cannot.
NORM_CHART_ORIENTED <- c("bar", "boxplot", "pointrange")
NORM_CHART_FIXED <- c("waterfall", "pie", "treemap")

#' Apply the chart's cross-argument rules
#'
#' - `orientation` is kept only where the inference from `x`, `y`, `func` and
#'   the column kinds would read the chart the other way (see
#'   dev/chart-cran-requirements.md); without kinds it is left as it is.
#' - Types that cannot lie either way have no `orientation`. A horizontal
#'   bar switched to waterfall, pie or treemap keeps its category: `x` and
#'   `y` swap unless the change set them.
#' - A histogram counts rows per bin of `x`: it has no `y`, no orientation
#'   and no sort.
#' - `ymin` and `ymax` are pairs: an unmatched end is dropped.
#' - The selection goes when a mapping or the drill changes, and is never
#'   held with a drill target.
#' - `unit`, `drill_target` and `drill_table` are trimmed.
#'
#' @param state The chart's state: a named list under the spec's names.
#' @param prev The state before the change, or `NULL` (a new block).
#' @param kinds The data's column kinds, or `NULL` without data.
#' @return The state after the rules.
#' @noRd
chart_normalize <- function(state, prev = NULL, kinds = NULL) {
  state <- trim_settings(state, c("unit", "drill_target", "drill_table"))
  type <- state[["type"]] %||% "bar"
  if (identical(type, "histogram")) {
    state[c("y", "orientation", "sort_by", "sort_dir")] <- list(NULL)
  } else if (type %in% NORM_CHART_FIXED) {
    state <- chart_keep_category(state, prev, kinds)
    state["orientation"] <- list(NULL)
  } else if (type %in% NORM_CHART_ORIENTED) {
    state["orientation"] <- list(chart_orientation(state, kinds))
  }
  state <- chart_pair_intervals(state)
  drop_selection(state, prev, c("type", "x", "y", "color", "facet",
                                "series", "xend", "drill"))
}

# The stored orientation: NULL when the inference agrees, or cannot be
# checked without the data.
chart_orientation <- function(state, kinds) {
  given <- state[["orientation"]]
  if (is.null(given) || is.null(kinds)) {
    return(given)
  }
  inferred <- chart_infer_lie(state, kinds)
  if (identical(inferred, given)) NULL else given
}

# A horizontal bar has its category on y. Switched to a type that is always
# vertical, the category moves to x, unless the same change set x or y.
chart_keep_category <- function(state, prev, kinds) {
  if (is.null(prev) || !(prev[["type"]] %||% "bar") %in% NORM_CHART_ORIENTED ||
        changed(state, prev, "x") || changed(state, prev, "y")) {
    return(state)
  }
  if (!identical(chart_lies(prev, kinds), "horizontal")) {
    return(state)
  }
  state[c("x", "y")] <- list(prev[["y"]], prev[["x"]])
  state
}

# Which way an oriented chart lies: its orientation, else the inference.
chart_lies <- function(state, kinds) {
  state[["orientation"]] %||% chart_infer_lie(state, kinds)
}

# Which way an oriented chart lies when `orientation` is not given
# (dev/chart-cran-requirements.md). The column `func` aggregates sits on the
# value axis, the one it groups by on the category axis.
# - One of x and y set: with a row count on a bar it is the category,
#   otherwise the value.
# - Both set: a numeric x beside a non-numeric y is horizontal; any other
#   pair, or one whose kinds are unknown, is vertical, as in ggplot2.
# - Neither set: bar horizontal, boxplot and pointrange vertical.
chart_infer_lie <- function(state, kinds) {
  type <- state[["type"]] %||% "bar"
  has_x <- length(state[["x"]]) > 0L
  has_y <- length(state[["y"]]) > 0L
  count <- identical(type, "bar") &&
    identical(state[["func"]] %||% "count", "count")
  if (has_x != has_y) {
    return(if (has_x == count) "vertical" else "horizontal")
  }
  if (has_x) {
    num_x <- kind_is_numeric(kinds, state[["x"]])
    num_y <- kind_is_numeric(kinds, state[["y"]])
    return(if (isTRUE(num_x) && isFALSE(num_y)) "horizontal" else "vertical")
  }
  if (identical(type, "bar")) "horizontal" else "vertical"
}

chart_pair_intervals <- function(state) {
  lo <- state[["ymin"]]
  hi <- state[["ymax"]]
  n <- min(length(lo), length(hi))
  if (length(lo) == length(hi)) {
    return(state)
  }
  state[c("ymin", "ymax")] <- list(
    if (n) lo[seq_len(n)], if (n) hi[seq_len(n)]
  )
  state
}
