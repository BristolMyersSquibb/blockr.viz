# Text per row, `NA` (null on the wire) where the cell is empty.
table_groups_text <- function(x) {
  x <- as.character(x)
  x[!nzchar(x)] <- NA_character_
  x
}

# The display text of each row. A number or text is its own text; a mark has
# its value label, or the label it would have had (rebuilt from the plan) when
# it draws none. A swimlane's text is its segment tooltips.
table_groups_disp <- function(c, p, ctx) {
  if (identical(c$kind, "num")) {
    return(table_groups_text(c$disp))
  }
  if (identical(c$kind, "interval")) {
    return(table_groups_text(vapply(c$tips, function(tt) {
      paste(unique(as.character(tt)), collapse = " | ")
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

# The mark of a column: the cell model's own fields less what the TableCol
# holds itself (`disp`, `v`, `pct`, tile paint, claims), per-row vectors as
# arrays. Flags travel only when true, empty references not at all. A number
# or text has no mark.
table_groups_mark <- function(c) {
  if (identical(c$kind, "num")) {
    return(NULL)
  }
  g <- c[setdiff(names(c), c("fkey", "scol", "disp", "v", "pct", "bg", "fg"))]
  if (!any(c$sub %in% TRUE)) g$sub <- NULL
  for (k in c("rf", "rc", "rby", "rbh")) {
    if (length(g[[k]]) && is.na(g[[k]])) g[[k]] <- NULL
  }
  if (!isTRUE(c$multi)) g$ab <- NULL
  if (!length(c$tt)) g$tt <- NULL
  labelled <- !is.null(c$disp) || isTRUE(c$multi) ||
    (identical(c$kind, "barsplit") && !is.null(c$ldisp))
  if (!labelled) g$dw <- NULL
  summarize_drop_null(table_groups_pack(table_groups_fence(g), c$kind,
                                        top = TRUE))
}

# A point range's fence band only where it has an outer range, per level.
table_groups_fence <- function(g) {
  if (!is.null(g$ow) && all(is.na(g$ow))) g[c("ol", "ow")] <- NULL
  if (!is.null(g$lv)) g$lv <- lapply(g$lv, table_groups_fence)
  g
}

# Column-level fields stay scalars (a bar's `fill` is its colour, a pair's is
# one per row); everything else is per row, an array also at one row.
TABLE_GROUPS_SCALARS <- c("kind", "mode", "rf", "d0", "d1", "dw", "rc", "rby",
                          "rbh", "tt")
TABLE_GROUPS_FLAGS <- c("multi", "bare", "dd", "lg")

table_groups_pack <- function(g, kind, top) {
  out <- list()
  for (nm in names(g)) {
    x <- g[[nm]]
    if (is.null(x) || (nm %in% TABLE_GROUPS_FLAGS && !isTRUE(x))) next
    out[[nm]] <- if (nm %in% TABLE_GROUPS_FLAGS ||
                       (top && nm %in% TABLE_GROUPS_SCALARS) ||
                       (top && nm == "fill" && identical(kind, "bar"))) {
      x
    } else if (nm == "segs") {
      lapply(x, function(ss) if (length(ss)) ss else summarize_arr(list()))
    } else if (nm == "lv") {
      lapply(x, table_groups_pack, kind = kind, top = FALSE)
    } else if (is.list(x)) {
      lapply(x, summarize_arr)
    } else {
      summarize_arr(x)
    }
  }
  out
}

# The column axis the header draws: tick positions (percent of the cell) and
# labels, with the "%" already on the last label. `foot` marks a large swimlane, which repeats the axis under the
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

# A vector as a JSON array, also at length one.
summarize_arr <- function(x) {
  I(unname(if (is.factor(x)) as.character(x) else x))
}

summarize_drop_null <- function(x) {
  if (!is.list(x)) return(x)
  x <- x[!vapply(x, is.null, logical(1L))]
  lapply(x, function(e) {
    if (is.list(e) && !inherits(e, "AsIs")) summarize_drop_null(e) else e
  })
}
