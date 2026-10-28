# The groups table's legend and its stub header.

#' The legend: one group per colour column in use, each titled by its column
#' since a table can carry more than one colour dimension.
#' @noRd
groups_color_groups <- function(summaries, data, drawn, scale_map) {
  legend_cols <- unique(unlist(lapply(summaries, function(s) {
    if (groups_color_capable(s)) groups_present(s$color, data)
  })))
  color_groups <- Filter(Negate(is.null), lapply(legend_cols, function(cc) {
    lv <- groups_color_levels(data[[cc]], drawn)
    if (length(lv) < 2L) return(NULL)
    list(column = cc, title = groups_col_name(data, cc), levels = lv,
         palette = groups_level_colors(scale_map, cc, lv, data[[cc]]))
  }))
  # A swimlane's event shapes: one group per shape column, drawn in ink.
  for (s in summaries) {
    lv <- s$.shape_used
    if (!identical(s$type, "spans") || !length(lv)) next
    if (s$event_shape %in% vapply(color_groups, function(g) {
      g$column
    }, "")) next
    color_groups[[length(color_groups) + 1L]] <- list(
      column = s$event_shape, title = groups_col_name(data, s$event_shape),
      levels = lv,
      shapes = (match(lv, s$.shape_levels) - 1L) %% 4L + 1L
    )
  }
  # A tile painted by a worst level gets its own legend group.
  for (s in summaries) {
    if (!identical(s$show, "tile") || is.null(s$.fill)) next
    lv <- s$.fill_levels
    if (!length(lv)) next
    color_groups[[length(color_groups) + 1L]] <- list(
      column = s$.fill,
      title = paste("Worst", groups_col_name(data, s$.fill)),
      levels = lv,
      palette = groups_tile_colors(scale_map, s$.fill, lv, data[[s$.fill]])
    )
  }
  color_groups
}

# The legend as data: a list of titled groups, always present for two or more
# series (colour alone must not carry identity). The table maps colour per
# column, so it can carry several groups, each titled by its column.
#' @noRd
groups_legend_spec <- function(prep) {
  if (!is.null(prep$err)) return(NULL)
  # Colour identity lives in the colour mapping only: a plain facet carries no
  # legend.
  groups <- prep$color_groups
  if (is.null(groups)) {
    if (is.null(prep$color) || length(prep$series) < 2L) return(NULL)
    groups <- list(list(column = prep$color, title = prep$color_title,
                        levels = prep$series, palette = prep$palette))
  }
  if (!length(groups)) return(NULL)
  list(groups = lapply(groups, function(g) {
    # Positional, never by name: a colour column can carry a blank level, and
    # `pal[[""]]` is a subscript error. groups_level_colors() returns the
    # palette in `levels` order. Blank reads as "(Missing)".
    pal <- g$palette
    list(
      # The column's variable label when it has one, as the hover card says.
      title = g$title %||% g$column,
      items = lapply(seq_along(g$levels), function(i) {
        l <- as.character(g$levels[[i]])
        list(
          label = if (nzchar(trimws(l))) l else "(Missing)",
          color = if (i <= length(pal)) unname(pal[[i]]),
          shape = if (!is.null(g$shapes)) g$shapes[[i]]
        )
      })
    )
  }))
}

# A data-frame-level display attribute, or NULL when absent / not a string.
#' @noRd
groups_attr <- function(data, nm) {
  v <- attr(data, nm, exact = TRUE)
  if (is.character(v) && length(v) == 1L && nzchar(v)) v else NULL
}
#' @noRd
groups_label_header <- function(prep) {
  if (isTRUE(prep$all)) return("")
  if (is.null(prep$parent)) prep$group else paste0(prep$parent, " / ", prep$group)
}
