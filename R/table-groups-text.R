# The groups view's text: what a cell says as plain text (`disp`, which the
# browser searches and, without a `key`, sorts on), the axis of a column, and
# the escapes the cell model carries for the HTML path.

# The text the cell model escaped for markup, as the text it stands for.
table_groups_plain <- function(x) {
  x <- as.character(x)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

# Plain text per row, `NA` (null on the wire) where the cell is empty.
table_groups_text <- function(x, escaped = FALSE) {
  x <- if (escaped) table_groups_plain(x) else as.character(x)
  x[!nzchar(x)] <- NA_character_
  x
}

# The display text of each row of a column. A plain number or text is its
# own text. A mark has its value label; a mark that draws none (the Values
# switch off, a column without labels) has the label it would have had,
# rebuilt from the plan. A swimlane has no single value: its text is the
# tooltips of its segments, which is what the reference's search read.
table_groups_disp <- function(c, p, ctx) {
  if (identical(c$kind, "num")) {
    return(table_groups_text(c$disp, escaped = isTRUE(c$text)))
  }
  if (identical(c$kind, "interval")) {
    return(table_groups_text(vapply(c$tips, function(tt) {
      paste(unique(table_groups_plain(tt)), collapse = " | ")
    }, character(1L))))
  }
  lab <- table_groups_labels(c)
  if (is.null(lab) || !any(nzchar(lab))) {
    p$show_val <- TRUE
    lab <- table_groups_labels(cells_column(ctx, p))
  }
  table_groups_text(lab %||% rep(NA_character_, ctx$n))
}

# The value label of a cell model column, per row; `NULL` when it has none.
# A colour split with one label per level reads "level label, level label".
table_groups_labels <- function(c) {
  if (isTRUE(c$multi)) {
    return(table_groups_join_levels(
      c$levels, lapply(c$lv, function(g) g$disp)
    ))
  }
  if (identical(c$kind, "barsplit") && !is.null(c$ldisp)) {
    return(table_groups_join_levels(c$names, c$ldisp))
  }
  if (is.null(c$disp)) NULL else as.character(c$disp)
}

# One text per row from one label vector per level (`NULL` for a level that
# prints none): the levels with a label, in order, each led by its name.
table_groups_join_levels <- function(levels, labs) {
  keep <- !vapply(labs, is.null, logical(1L))
  if (!any(keep)) {
    return(NULL)
  }
  parts <- Map(function(lv, d) {
    d <- as.character(d)
    ifelse(nzchar(d), paste(lv, d), "")
  }, as.character(levels)[keep], labs[keep])
  vapply(seq_along(parts[[1L]]), function(i) {
    one <- vapply(parts, function(x) x[[i]], character(1L))
    paste(one[nzchar(one)], collapse = ", ")
  }, character(1L))
}

# The scale a mark is drawn on: `domain` (what its positions are percentages
# of) and, with the axis on, `ticks`, the labelled positions of the column
# axis in the domain's units. summarize_axis_strip() draws the same strip as
# HTML: a percentage scale carries its "%" on the last label. `domain`
# fields: `min` and `max` (d0, d1 of summarize_axis_domain()), `date` when
# they are days since 1970-01-01. A swimlane keeps its own domain when the
# axis helper has none (a degenerate span still hovers).
table_groups_scale <- function(c, p, prep, axis) {
  dom <- summarize_axis_domain(p, prep)
  if (is.null(dom)) {
    if (!identical(c$kind, "interval")) {
      return(list())
    }
    return(list(domain = table_groups_compact(list(
      min = c$d0, max = c$d1, date = if (isTRUE(c$dd)) TRUE
    ))))
  }
  out <- list(domain = table_groups_compact(list(
    min = round(dom$d0, 4L), max = round(dom$d1, 4L),
    date = if (isTRUE(dom$date)) TRUE
  )))
  if (isTRUE(axis)) {
    out$ticks <- table_groups_ticks(dom, p)
  }
  out
}

# `{at, labels}` of the axis, or `NULL` when there are fewer than two ticks.
table_groups_ticks <- function(dom, p) {
  t <- if (isTRUE(dom$date)) {
    summarize_axis_date_ticks(dom$d0, dom$d1)
  } else {
    at <- summarize_axis_ticks(
      dom$d0, dom$d1, k = if (identical(p$kind, "interval")) 7L else 4L
    )
    list(at = at, labels = lane_fmt(at))
  }
  if (length(t$at) < 2L) {
    return(NULL)
  }
  last <- length(t$at)
  if (isTRUE(dom$pct)) {
    t$labels[[last]] <- paste0(t$labels[[last]], "%")
  }
  list(at = table_groups_arr(round(t$at, 6L)),
       labels = table_groups_arr(t$labels))
}
