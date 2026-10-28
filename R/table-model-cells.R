#' The cell model both consumers read. A column entry has `kind` plus only
#' the vectors that kind uses (`w` percent widths with NA = no fill, `v` sort
#' values, `disp`/`pct`/`dw` for the in-bar label). Widths are rounded to 2dp.
#' `dw` is the label slot width in ch, one number per column so every row
#' reserves the same slot.
#' @noRd
summarize_cells <- function(prep, drill = NULL, active = NULL, cfg = NULL) {
  rows <- prep$rows
  plan <- prep$plan
  n <- nrow(rows)

  # Shared by every column builder. `vals_on` is the Values switch: off drops
  # value labels beside marks (number and text columns are untouched).
  ctx <- list(
    rows = rows, n = n, prep = prep, mx = prep$bar_max,
    mn = prep$bar_min %||% 0, vals_on = !isFALSE(cfg$value_labels)
  )

  cols <- lapply(seq_along(plan), function(i) cells_column(ctx, plan[[i]]))
  cols <- cells_annotate(cols, plan)

  st <- cells_row_state(prep, rows, active)
  # A table without `by` has no group to drill on.
  if (isTRUE(prep$all)) drill <- NULL

  list(
    n = n,
    ncol = length(plan) + 1L,
    nested = !is.null(prep$parent),
    # A compact matrix opens its groups.
    expanded = any(vapply(plan, function(p) isTRUE(p$section), logical(1L))) ||
      isTRUE(cfg$compact),
    label = as.character(rows$.label),      # PLAIN: each consumer escapes
    parent = summarize_parent_labels(rows),
    parent_key = as.character(
      if (!is.null(rows$.parent_key)) rows$.parent_key else rows$.parent
    ),
    level = as.integer(rows$.level),
    parent_row = rows$.is_parent,
    on = st$on,
    on_cell = st$on_cell,
    pick = !is.null(drill),
    # Columns a row click claims, outer to inner (R/summarize-drill.R).
    path = c(summarize_chr1(prep$parent), summarize_chr1(prep$group)),
    cols = cols,
    fold = summarize_fold_text(prep)
  )
}

# One plan entry to its column. The first matching branch wins.
cells_column <- function(ctx, p) {
  # Per-entry scale, falling back to the prep-level one.
  b <- cells_bounds(p$dmin %||% ctx$mn, p$dmax %||% ctx$mx)
  if (identical(p$kind, "bar")) {
    cells_bar(ctx, p)
  } else if (identical(p$kind, "tile")) {
    cells_tile(ctx, p)
  } else if (identical(p$kind, "barsplit")) {
    cells_barsplit(ctx, p)
  } else if (identical(p$kind, "bardiv")) {
    cells_bardiv(ctx, p)
  } else if (p$kind %in% c("box", "pointrange")) {
    cells_lane(ctx, p, b)
  } else if (identical(p$kind, "pair")) {
    cells_pair(ctx, p, b)
  } else if (identical(p$kind, "interval")) {
    cells_interval(ctx, p)
  } else if (identical(p$kind, "sparkline")) {
    cells_sparkline(ctx, p)
  } else if (isTRUE(p$raw) && isTRUE(p$text)) {
    cells_text_raw(ctx, p)
  } else if (identical(p$dispkey, "dist_text")) {
    cells_dist_text(ctx, p)
  } else if (!is.null(p$alt_text) && is.null(ctx$rows[[p$key]])) {
    cells_alt_text(ctx, p)
  } else {
    cells_num(ctx, p)
  }
}

# `v` is the sort key and data-v, rounded to 4dp so both consumers print the
# same string and the payload stays small.
cells_sortv <- function(v) if (is.numeric(v)) round(v, 4L) else v

# NA stays NA: a no-value cell draws no fill, where 0 draws a zero-width
# sliver. Summaries-path plan entries carry their own domain.
cells_pct_w <- function(v, e_mx) {
  w <- if (is.finite(e_mx) && e_mx > 0) {
    pmax(0, pmin(100, abs(v) / e_mx * 100))
  } else {
    rep(0, length(v))
  }
  w[!is.finite(w)] <- 0
  w[is.na(v)] <- NA_real_
  round(w, 2L)
}

# Position and width on the lane marks' shared domain, which need not start at
# zero. Computed from raw values and rounded here so consumers never subtract
# floats (String(24.13 - 10.5) in JS is not "13.63").
cells_bounds <- function(e_mn, e_mx) {
  rng <- if (is.finite(e_mx) && is.finite(e_mn) && e_mx > e_mn) {
    e_mx - e_mn
  } else {
    NA_real_
  }
  list(
    pos = function(v) {
      w <- if (is.finite(rng)) (v - e_mn) / rng * 100 else rep(0, length(v))
      w[!is.finite(w) & !is.na(v)] <- 0
      w[is.na(v)] <- NA_real_
      round(pmax(0, pmin(100, w)), 2L)
    },
    span = function(a, b) {
      w <- if (is.finite(rng)) (b - a) / rng * 100 else rep(0, length(a))
      w[!is.finite(w) & !(is.na(a) | is.na(b))] <- 0
      w[is.na(a) | is.na(b)] <- NA_real_
      round(pmax(0, pmin(100, w)), 2L)
    }
  )
}

# The in-bar value label: the raw measure, with a "(43%)" tail for counting
# measures that carry a base.
cells_val_parts <- function(ctx, p, vraw, signed = FALSE) {
  prep <- ctx$prep
  if (!ctx$vals_on || !isTRUE(p$show_val)) return(NULL)
  dom <- summarize_axis_domain(p, prep)
  dig <- summarize_val_digits(vraw, dom$d0, dom$d1)
  parts <- summarize_num_parts(vraw, denom = p$val_denom,
                               combined = !is.null(p$val_denom),
                               signed = signed,
                               fmt = if (!is.null(prep$format)) {
                                 function(v) dt_format_num(v, prep$format)
                               } else if (!is.na(dig)) {
                                 function(v) summarize_val_str(v, dig)
                               })
  # formatC pads "fg" output; trim before measuring the label slot.
  parts$disp <- trimws(parts$disp)
  len <- nchar(parts$disp) +
    if (is.null(parts$pct)) 0L else ifelse(nzchar(parts$pct),
                                           nchar(parts$pct) + 1L, 0L)
  c(parts, list(dw = max(c(1L, len))))
}

# Hover-card column description (header, sub-line, measure, colour column,
# percentage base, statistic words), plus what a click adds to the row's claim:
# a facet column its level, a colour split the level of the segment hit.
cells_annotate <- function(cols, plan) {
  for (i in seq_along(cols)) {
    cols[[i]]$tt <- summarize_tip_meta(plan[[i]])
  }
  for (i in seq_along(cols)) {
    pl <- plan[[i]]
    if (!is.null(pl$flevel) && !is.null(pl$fcol)) {
      cols[[i]]$fkey <- list(column = pl$fcol, value = as.character(pl$flevel))
    }
    if (identical(cols[[i]]$kind, "barsplit") && !is.null(pl$ccol)) {
      cols[[i]]$scol <- pl$ccol
    }
  }
  cols
}

# A stored click is the row's path plus what a cell added; the cell part goes
# to the client.
cells_row_state <- function(prep, rows, active) {
  path <- c(summarize_chr1(prep$parent), summarize_chr1(prep$group))
  filters <- summarize_drill_filters(active$col, active$vals)
  on_path <- names(filters) %in% path
  on <- summarize_drill_on(
    rows, summarize_chr1(prep$group), summarize_chr1(prep$parent),
    filters[on_path]
  )
  on_cell <- if (any(on) && any(!on_path)) {
    lapply(filters[!on_path], function(v) as.character(v)[[1L]])
  }
  list(on = on, on_cell = on_cell)
}

#' @noRd
summarize_parent_labels <- function(rows) {
  parent <- as.character(rows$.parent)
  key <- if (!is.null(rows$.parent_key)) {
    as.character(rows$.parent_key)
  } else {
    parent
  }
  parent_labels <- stats::setNames(
    as.character(rows$.label[rows$.is_parent]),
    key[rows$.is_parent]
  )
  hit <- parent_labels[parent]
  parent[!is.na(hit)] <- hit[!is.na(hit)]
  parent
}

#' The fold row's text, or NULL when nothing was capped.
#' @noRd
summarize_fold_text <- function(prep) {
  if (!isTRUE(prep$folded > 0L)) return(NULL)
  paste0(
    "Other \u2014 ", prep$folded, " ",
    if (is.null(prep$parent)) "rows" else "groups", " below the cut",
    if (is.finite(prep$fold_max)) {
      paste0(", each with a value \u2264 ",
             format(prep$fold_max, scientific = FALSE, trim = TRUE))
    } else {
      ""
    }
  )
}

#' The hover card's column description. NULL where the cell prints everything.
#' @noRd
summarize_tip_meta <- function(p) {
  if (identical(p$kind, "num")) return(NULL)
  den <- p$val_denom
  out <- list(
    head = summarize_chr1(p$label),
    sub = summarize_chr1(p$sub_label),
    meas = summarize_chr1(p$meas),
    cvar = summarize_chr1(p$cvar),
    den = if (is.numeric(den) && length(den) == 1L && is.finite(den) &&
                den > 0) den,
    words = p$words
  )
  out[!vapply(out, is.null, logical(1L))]
}
