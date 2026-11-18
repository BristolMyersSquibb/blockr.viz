# A column can name another column whose values a reader sees in its place:
#
#   attr(data$id, "blockr_display") <- "short_id"
#
# The chart still groups, sorts, colours and claims by the column's own
# values. Only the printed text reads the other column: the category axis,
# the lanes of a timeline, tooltip heads, legend entries. A producer upstream
# sets the attribute (a clinical package labels its long subject ids by the
# short ones); viz names no columns itself. A column whose display column is
# gone from the data, or a value with no display value, shows itself.
#
# The browser gets one map per labelled column, `derived$display`:
# list(<col> = list(<value> = <label>, ...)). It holds the values the data
# has, so it grows with the distinct values, as the axis does.

#' The display maps of a chart's key columns
#'
#' @param v The chart's settings.
#' @param d The rows the chart draws (`src$plain()`).
#' @param raw The input before the prepare script, or NULL. A script that
#'   subsets with base `[` drops column attributes; the input still has them.
#' @return A named list of maps, or NULL when no key column is labelled.
#' @noRd
chart_display_labels <- function(v, d, raw = NULL) {
  if (!is.data.frame(d)) {
    return(NULL)
  }
  cols <- c(chart_category(v), v$color, v$series, v$facet, v$facet_row,
            if (!identical(v$type, "histogram")) c(v$x, v$y))
  cols <- unique(as.character(unlist(cols)))
  cols <- cols[nzchar(cols) & cols %in% names(d)]
  out <- list()
  for (col in cols) {
    map <- display_map(d, col) %||%
      if (is.data.frame(raw) && col %in% names(raw)) {
        display_map(raw, col, values = d[[col]])
      }
    if (length(map)) out[[col]] <- map
  }
  if (length(out)) out
}

# The display column a column names, or NULL.
display_col <- function(x) {
  to <- attr(x, "blockr_display", exact = TRUE)
  if (is.character(to) && length(to) == 1L && !is.na(to) && nzchar(to)) to
}

# value -> label for `col` of `data`, from the display column the column
# names. With `values`, only those values (the rows the chart draws, when
# `data` is the input). NULL when there is nothing to relabel.
display_map <- function(data, col, values = NULL) {
  to <- display_col(data[[col]])
  if (is.null(to) || identical(to, col) || !to %in% names(data)) {
    return(NULL)
  }
  key <- cell_text(data[[col]])
  lab <- as.character(data[[to]])
  keep <- !duplicated(key) & !is.na(key) & nzchar(key) &
    !is.na(lab) & nzchar(trimws(lab)) & lab != key
  if (!is.null(values)) {
    keep <- keep & key %in% cell_text(values)
  }
  if (!any(keep)) {
    return(NULL)
  }
  as.list(stats::setNames(lab[keep], key[keep]))
}
