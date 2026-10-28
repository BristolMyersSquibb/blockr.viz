# The groups table's legend and its stub header.

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
          color = if (i <= length(pal)) unname(pal[[i]])
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

