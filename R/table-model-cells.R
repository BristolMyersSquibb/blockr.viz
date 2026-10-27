# Table model: the cell model of the groups layout --------------------------
#
# Pure compute, no markup and no JSON. summarize_cells() turns the prepared
# table (`prep`) into the cell model both consumers read (the HTML emitter and
# the JSON payload in summarize-push.R). This file holds the entry point, the
# scale helpers every column kind shares, the row meta and the hover-card
# column description. One builder per column kind lives in
# table-model-cells-columns.R (bar, tile, split, diverging, text, number),
# table-model-cells-lanes.R (box, point range, pair) and
# table-model-cells-marks.R (swimlane, sparkline). The axis domain, tick and
# value-label helpers are in table-model-cells-axis.R.

#' The cell model: everything both consumers need, computed once.
#'
#' Per-column entries carry `kind` plus only the vectors that kind uses:
#'   bar      -> `w` (percent widths, NA = no value = no fill), `v` (sort
#'               values), `fill`, `sub`, and the in-bar label (`disp`/`pct`/
#'               `dw`) when the plan asks for one
#'   barsplit -> `seg` (list of per-series width vectors), `segv`, `fills`,
#'               `names`, `mode`, plus the in-bar label
#'   bardiv   -> `w`, `v`, `pos` (polarity), plus the signed in-bar label
#'   num      -> `disp` (display strings), `v`; `text` marks a left-aligned
#'               raw field column
#' Widths are already rounded to the 2dp both consumers print. `dw` is the
#' label slot's width in ch -- ONE number per column, computed here, so every
#' row reserves the same slot and the tracks stay aligned.
#' @noRd
summarize_cells <- function(prep, drill = NULL, active = NULL, cfg = NULL) {
  rows <- prep$rows
  plan <- prep$plan
  n <- nrow(rows)

  # What every column builder reads: the rows, the prep (axis domains, number
  # format, palette), the prep-level scale and the Values switch.
  # The table's Values switch (`value_labels`, default on): off drops every
  # value label beside a mark, whatever the mark. Number and text columns ARE
  # their values and are not touched.
  ctx <- list(
    rows = rows, n = n, prep = prep, mx = prep$bar_max,
    mn = prep$bar_min %||% 0, vals_on = !isFALSE(cfg$value_labels)
  )

  cols <- lapply(seq_along(plan), function(i) cells_column(ctx, plan[[i]]))
  cols <- cells_annotate(cols, plan)

  st <- cells_row_state(prep, rows, active)
  # The one row of a table without `by` has no group to drill on and
  # nothing to sort against.
  if (isTRUE(prep$all)) drill <- NULL

  list(
    n = n,
    thead = summarize_thead(prep, sortable = !isTRUE(prep$all) &&
                              isTRUE(cfg$sortable %||% TRUE),
                            cols = cols, axis = isTRUE(cfg$axis %||% TRUE)),
    tfoot = summarize_tfoot(prep, cols = cols,
                            axis = isTRUE(cfg$axis %||% TRUE)),
    ncol = length(plan) + 1L,
    nested = !is.null(prep$parent),
    # A compact matrix opens its groups, as the heatmap did: the subjects are
    # what it shows.
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
    # The columns a row click claims, outer -> inner: a parent row claims the
    # first, a child row both (R/summarize-drill.R).
    path = c(summarize_chr1(prep$parent), summarize_chr1(prep$group)),
    cols = cols,
    fold = summarize_fold_text(prep),
    attrs = summarize_table_attrs(prep, cfg)
  )
}

# One plan entry -> its column of the cell model. The branch order is the
# dispatch: the first entry that matches wins.
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

# `v` is the sort key and the data-v attribute, nothing else, so full float
# precision is pure payload: a faceted percentage like 23.734177215189873
# costs 18 bytes per cell against 5 for 23.73. Rounded HERE so both consumers
# print the same string and the drift guard stays green.
cells_sortv <- function(v) if (is.numeric(v)) round(v, 4L) else v

# NA stays NA: a no-value cell (the identity measure's absent facet) draws
# NO fill at all, where 0 draws the visible zero-width sliver.
# Parametric over the scale: summaries-path plan entries carry their own
# domain (mixed marks must not share one), single-mark paths use the
# prep-level scale.
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

# Position/width on the lane marks' shared domain [bar_min, bar_max]: the
# bar's pct_w() generalized to a domain that need not start at zero (a
# box of change-from-baseline values has a negative lo). Positions and
# widths are BOTH computed from raw values here and shipped rounded, so the
# two consumers never subtract floats themselves (String(24.13 - 10.5) in
# JS is not "13.63").
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

# The in-bar value label: the raw measure (never the width percentage),
# with the counting measures' "(43%)" tail when the plan carries a base.
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
  # formatC pads "fg" output to a common width; harmless in a collapsing
  # HTML cell but it would inflate the label slot -- trim before measuring.
  parts$disp <- trimws(parts$disp)
  len <- nchar(parts$disp) +
    if (is.null(parts$pct)) 0L else ifelse(nzchar(parts$pct),
                                           nchar(parts$pct) + 1L, 0L)
  c(parts, list(dw = max(c(1L, len))))
}

# What the hover card says about each column, beside the row's numbers:
# its header and sub-line (the arm and its N, or the summary's name), the
# measure a bar shows, the colour column a split names its levels by, the
# percentage base, and the statistic words of a distribution. Then what a
# click on a cell adds to the row's claim: a facet column its level, a colour
# split the level of the segment hit (the drill, below).
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

# A stored click is the row's path, plus what a cell added. The row lights
# from its path; the cell part goes to the client, which marks the cell.
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

#' The fold row's text, or NULL when nothing was capped. Never a silent
#' truncation: `top_n` always says what fell below the cut.
#' @noRd
summarize_fold_text <- function(prep) {
  if (!isTRUE(prep$folded > 0L)) return(NULL)
  paste0(
    "Other — ", prep$folded, " ",
    if (is.null(prep$parent)) "rows" else "groups", " below the cut",
    if (is.finite(prep$fold_max)) {
      paste0(", each with a value ≤ ",
             format(prep$fold_max, scientific = FALSE, trim = TRUE))
    } else {
      ""
    }
  )
}

#' The hover card's column description (see summarize_cells()). NULL for the
#' columns whose cell already prints everything (numbers, text).
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
