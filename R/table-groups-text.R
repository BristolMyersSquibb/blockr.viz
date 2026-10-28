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

# The display text of each row. A number or text is its own text; a mark has
# its value label, or the label it would have had (rebuilt from the plan) when
# it draws none. A swimlane's text is its segment tooltips.
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

# The mark of a column: summarize_payload_col()'s fields less what the TableCol
# holds itself (`disp`, `v`, `pct`, tile paint, claims). Text the cell model
# escaped is plain here; the browser escapes. A number or text has no mark.
table_groups_mark <- function(c) {
  if (identical(c$kind, "num")) {
    return(NULL)
  }
  out <- summarize_payload_col(c)
  out[c("fkey", "scol", "disp", "v", "pct", "bg", "fg")] <- NULL
  out$tip <- table_groups_tips(out[["tip"]])
  if (!is.null(out$lv)) {
    out$lv <- lapply(out$lv, function(g) {
      g$tip <- table_groups_tips(g[["tip"]])
      g
    })
  }
  if (identical(c$kind, "interval")) {
    out$tips <- lapply(out$tips, table_groups_tips)
    out$segs <- lapply(out$segs, lapply, function(sg) {
      if (length(sg) >= 4L) sg[[4L]] <- table_groups_plain(sg[[4L]])
      sg
    })
  }
  summarize_drop_null(out)
}

table_groups_tips <- function(x) {
  if (!is.null(x)) summarize_arr(table_groups_plain(x))
}

# The column axis the header draws: tick positions (percent of the cell) and
# labels, as summarize_axis_strip() prints them, with the "%" already on the
# last label. `foot` marks a large swimlane, which repeats the axis under the
# table. `NULL` without a scale or with fewer than two ticks.
table_groups_axis <- function(p, prep) {
  dom <- summarize_axis_domain(p, prep)
  if (is.null(dom)) {
    return(NULL)
  }
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
  if (isTRUE(dom$pct)) {
    last <- length(t$labels)
    t$labels[[last]] <- paste0(t$labels[[last]], "%")
  }
  pos <- round((t$at - dom$d0) / (dom$d1 - dom$d0) * 100, 2L)
  table_groups_compact(list(
    at = summarize_arr(pos), labels = summarize_arr(t$labels),
    foot = if (identical(p$kind, "interval") && identical(p$size, "lg")) TRUE
  ))
}
