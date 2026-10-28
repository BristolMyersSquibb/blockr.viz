# The legend as data: a list of titled groups, always present for two or more
# series (colour alone must not carry identity). The table maps colour per
# column, so it can carry several groups, each titled by its column.
#' @noRd
summarize_legend_spec <- function(prep) {
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
    # `pal[[""]]` is a subscript error. summarize_level_colors() returns the
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
summarize_attr <- function(data, nm) {
  v <- attr(data, nm, exact = TRUE)
  if (is.character(v) && length(v) == 1L && nzchar(v)) v else NULL
}
#' The two parts of a numeric cell: the value string, and the percent string
#' when the column shows both.
#' @noRd
summarize_num_parts <- function(v, denom = NULL, combined = FALSE,
                                signed = FALSE, pct_only = FALSE,
                                fmt = NULL) {
  if (isTRUE(signed)) {
    return(list(disp = ifelse(
      is.na(v), "",
      paste0(ifelse(v > 0, "+", ifelse(v < 0, "\u2212", "")),
             formatC(abs(v), format = "f", digits = 1L))
    )))
  }
  pct <- if (!is.null(denom) && is.finite(denom) && denom > 0) {
    v / denom * 100
  } else {
    NULL
  }
  if (isTRUE(pct_only)) {
    if (is.null(pct)) return(list(disp = rep("", length(v))))
    return(list(disp = ifelse(
      is.na(v), "", paste0(formatC(pct, format = "f", digits = 1L), "%")
    )))
  }
  # Four significant digits, the budget lane_fmt() prints a statistic at, so a
  # split bar's label and a box's centre read alike. "fg" keeps whole numbers
  # whole. `fmt`, when given, is the label's own precision
  # (summarize_val_digits()).
  n <- ifelse(is.na(v), "", if (is.null(fmt)) {
    formatC(v, format = "fg", digits = 4L, big.mark = "")
  } else {
    fmt(v)
  })
  if (isTRUE(combined) && !is.null(pct)) {
    return(list(
      disp = n,
      pct = ifelse(is.na(v), "",
                   paste0("(", formatC(pct, format = "f", digits = 0L), "%)"))
    ))
  }
  list(disp = n)
}

#' @noRd
summarize_label_header <- function(prep) {
  if (isTRUE(prep$all)) return("")
  if (is.null(prep$parent)) prep$group else paste0(prep$parent, " / ", prep$group)
}

