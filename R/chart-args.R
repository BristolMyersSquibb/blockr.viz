# The chart's argument helpers: which types map which roles, the text of
# a highlight and the selection.
#
# An aggregated chart maps a `category` and a `value`; `orientation` only
# says which way the marks lie. `x` and `y` are the screen axes of the types
# that draw rows (scatter, line, area, band), the histogram and the gantt.

# Types that can lie either way, and the way they lie when `orientation` is
# NULL. Waterfall is vertical only; pie and treemap have no axes.
CHART_ORIENTED_TYPES <- c("bar", "boxplot", "pointrange")
CHART_AGGREGATED_TYPES <- c("bar", "waterfall", "pie", "treemap",
                            "boxplot", "pointrange")
CHART_DISTRIBUTION_TYPES <- c("boxplot", "pointrange")

chart_default_orientation <- function(type) {
  if (identical(type, "bar")) "horizontal" else "vertical"
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

