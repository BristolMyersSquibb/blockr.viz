# Public arguments of new_chart_block() and the chart's internal names.
#
# The constructor, the saved state and the `{@arg}` slots speak the public
# names. The server and inst/js/chart/ keep the names from before 0.3.0:
# `chart_type`, `group`/`value`/`orientation`, `bar_mode`, `tt_fields`,
# `value_lines`, `lo`/`hi`, `band_id`, `count_col`, `count_on`, `na_group`,
# `ctrl_target`/`ctrl_table`, `values` and the four `filter_*` fields. This
# file is the only place the two meet.
#
# Roles are the one translation that is not a rename. Publicly `x` and `y` are
# the screen axes for every type, as in ggplot2: a horizontal bar is
# `y = "category", x = "amount"`. Internally an aggregated chart has a
# category column (`group`), an aggregated column (`value`) and an
# `orientation`.

# Types whose category and value columns are drawn on screen axes, and which
# can lie either way. Waterfall is vertical only, pie and treemap have no axes:
# for those `x` is the category and `y` the value.
CHART_ORIENTED_TYPES <- c("bar", "boxplot", "pointrange")
CHART_AGGREGATED_TYPES <- c("bar", "waterfall", "pie", "treemap",
                            "boxplot", "pointrange")
CHART_DISTRIBUTION_TYPES <- c("boxplot", "pointrange")

# Public roles -> the internal ones the server and the browser read.
# `kinds` (viz_column_kinds()) is NULL when the data is not there.
chart_roles_internal <- function(type, x, y, func, orientation, kinds = NULL) {
  if (identical(type, "histogram")) {
    return(list(group = chart_bin_col(x), value = ".count", x = x, y = NULL,
                orientation = "vertical"))
  }
  if (!type %in% CHART_AGGREGATED_TYPES) {
    return(list(group = NULL, value = ".count", x = x, y = y,
                orientation = orientation))
  }
  orient <- if (type %in% CHART_ORIENTED_TYPES) {
    orientation %||%
      chart_infer_lie(list(type = type, x = x, y = y, func = func), kinds)
  }
  horizontal <- identical(orient, "horizontal")
  group <- if (horizontal) y else x
  value <- if (horizontal) x else y
  if (identical(func %||% "count", "count") &&
        !type %in% CHART_DISTRIBUTION_TYPES) {
    value <- ".count"
  }
  list(group = group, value = value, x = NULL, y = NULL,
       orientation = orient)
}

# How a title's `{highlight}` prints the values of a `highlight` (one column
# and its values, `list(country = "Switzerland")`, or a named character
# vector): "Switzerland", "Switzerland and Germany", "Switzerland, Germany and
# Japan". "" for none or a malformed one.
chart_highlight_text <- function(highlight) {
  # Malformed: no names, an empty name, more than one column, or (for a list)
  # more than one entry.
  nms <- names(highlight)
  if (is.null(nms) || !all(nzchar(nms)) || length(unique(nms)) != 1L ||
        (!is.atomic(highlight) && length(highlight) > 1L)) {
    return("")
  }
  v <- as.character(unlist(highlight, use.names = FALSE))
  v <- unique(v[!is.na(v) & nzchar(v)])
  if (length(v) < 2L) {
    return(paste(v, collapse = ""))
  }
  paste(paste(utils::head(v, -1L), collapse = ", "), "and", utils::tail(v, 1L))
}

# selection (public) <-> filter_type / filters / filter_range / filter_point
# (internal). NULL when nothing is selected, so a chart without a click saves no
# selection.
chart_selection <- function(type = "categorical", filters = NULL,
                            range = NULL, point = NULL) {
  type <- type %||% "categorical"
  if (identical(type, "categorical") && is.null(filters) &&
        is.null(range) && is.null(point)) {
    return(NULL)
  }
  list(type = type, filters = filters, range = range, point = point)
}

