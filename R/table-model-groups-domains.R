# Table model, groups layout: domains --------------------------------------------
#
# The scale of each column, computed over the ASSEMBLED rows (parents
# included) and shared across a summary's facet copies: one scale per
# summary, never one scale per table. Also the decisions that need to have
# seen the values, such as the diverging bar and the degenerate split.

#' Has a `grouped` colour split anything to lay side by side?
#'
#' Grouped gives every level its own lane whatever the row holds. That is
#' right when a group really does span levels (AE terms by severity), and
#' wrong when the colour column is a group-level FACT: one row per subject
#' coloured by arm draws one bar and N-1 EMPTY lanes, at N times the row
#' height, saying nothing a legend does not already say. (Wanting one lane
#' per level as a reading in its own right is what `facet` is for -- one
#' column per level, on a shared scale.)
#'
#' True when no row anywhere in the column carries more than one level. The
#' caller then stacks instead: with a single part per row the two layouts
#' draw the SAME numbers -- stacking a lone segment is that segment -- and
#' the stacked emitter omits an empty one, in both `summarize_split_html()` and
#' its `splitHtml()` twin. So the cell becomes one bar in that row's colour,
#' with no width, colour or tooltip changed.
#'
#' Decided over the assembled rows for the same reason the diverging bar is
#' (below): the plan is built before any value exists.
#' @noRd
lane_split_degenerate <- function(plan, idx, rows) {
  for (i in idx) {
    p <- plan[[i]]
    if (!identical(p$kind, "barsplit") || !identical(p$mode, "grouped")) {
      return(FALSE)
    }
    cn <- paste0(p$prefix %||% ".s_", p$series)
    cn <- cn[cn %in% names(rows)]
    # Levels present per row, summed across the level columns rather than
    # tabulated per row: shape-safe for a one-row table, where an
    # apply()/vapply() matrix would collapse to a vector.
    per_row <- NULL
    for (x in cn) {
      v <- suppressWarnings(as.numeric(rows[[x]]))
      hit <- as.integer(is.finite(v) & v != 0)
      per_row <- if (is.null(per_row)) hit else per_row + hit
    }
    if (!is.null(per_row) && any(per_row > 1L)) return(FALSE)
  }
  TRUE
}

#' Per-entry domains, computed over the assembled rows and SHARED across a
#' summary's facet copies (same `sid` stem before the `f<j>` suffix).
#' @noRd
lane_summary_domains <- function(plan, rows) {
  stem <- function(sid) sub("f[0-9]+$", "", sid)
  groups <- split(seq_along(plan), vapply(plan, function(p) {
    stem(p$sid %||% "")
  }, character(1L)))
  for (idx in groups) {
    kinds <- unique(vapply(plan[idx], function(p) p$kind, character(1L)))
    kind <- kinds[[1L]]
    plan <- if (identical(kind, "tile")) {
      lane_dom_tile(plan, idx, rows)
    } else if (kind %in% c("bar", "barsplit", "box", "pointrange")) {
      lane_dom_value(plan, idx, rows, kind)
    } else if (identical(kind, "sparkline")) {
      lane_dom_spark(plan, idx, rows)
    } else if (identical(kind, "pair")) {
      lane_dom_pair(plan, idx, rows)
    } else if (identical(kind, "interval")) {
      lane_dom_interval(plan, idx, rows)
    } else {
      plan
    }
  }
  plan
}

# A hair of padding so a glyph at the extreme is not flush against the cell
# edge, and a degenerate (all-equal) column still draws.
lane_dom_pad <- function(rng) {
  if (rng[[2L]] > rng[[1L]]) {
    (rng[[2L]] - rng[[1L]]) * 0.04
  } else {
    max(abs(rng[[1L]]) * 0.04, 0.5)
  }
}

# A tile without a paint column is painted by its value: one ramp across
# the summary's copies, from zero.
lane_dom_tile <- function(plan, idx, rows) {
  # Leaf rows only: a group row's total is many times a cell, and on
  # the same ramp every cell under it would come out pale. A group row
  # clamps at the darkest step instead.
  leaf <- if (!is.null(rows$.level) && any(rows$.level > 0L)) {
    rows[rows$.level > 0L, , drop = FALSE]
  } else {
    rows
  }
  vals <- unlist(lapply(idx, function(i) leaf[[plan[[i]]$key]]))
  vals <- vals[is.finite(vals)]
  for (i in idx) {
    plan[[i]]$dmin <- 0
    plan[[i]]$dmax <- if (length(vals)) max(vals) else 0
  }
  plan
}

# The value columns of a bar, box or point range: POSITION columns only. A
# box also carries `n` (the group size, for the tooltip); it is not a point
# on the value axis, and letting it in stretched the domain to the sample
# size -- a 49-105 mmHg box on a 0-1012 track.
lane_dom_cols <- function(p, kind) {
  if (kind %in% c("bar", "barsplit")) {
    # The split bar scales on its TOTAL, the same number the plain bar
    # uses -- the segments are a share of it, so the two read alike.
    # Side-by-side segments are read one at a time instead, so the axis
    # has to reach the WIDEST of them, not just the pooled value.
    if (identical(p$mode, "grouped")) {
      c(p$key, paste0(p$prefix, p$series))
    } else {
      p$key
    }
  } else {
    pos <- function(cm) unname(cm[setdiff(names(cm), "n")])
    # A colour-split column's levels share ONE scale: every level's
    # glyph is read against the same axis, or the split lies.
    c(pos(p$cols), unlist(lapply(p$lcols %||% list(), pos)))
  }
}

# Every value the summary's copies draw, on the scale they are drawn at.
lane_dom_vals <- function(plan, idx, rows, kind) {
  vals <- numeric()
  for (i in idx) {
    p <- plan[[i]]
    cols <- lane_dom_cols(p, kind)
    # A count with an N is drawn as a percentage of it (summarize_cells()),
    # so its domain is one too: the copies share a percentage scale.
    scale <- if (is.numeric(p$denom) && length(p$denom) == 1L &&
                   is.finite(p$denom) && p$denom > 0) 100 / p$denom else 1
    for (cn in cols) vals <- c(vals, rows[[cn]] * scale)
  }
  vals[is.finite(vals)]
}

# The domain of a bar, box or point range.
lane_dom_value <- function(plan, idx, rows, kind) {
  # Before the domain, because grouped and stacked read different columns
  # for it. Decided for the whole stem at once so a summary's facet copies
  # keep one layout -- a column that switched per copy would put a tall
  # cell beside a short one on the same row.
  if (identical(kind, "barsplit") && lane_split_degenerate(plan, idx, rows)) {
    for (i in idx) plan[[i]]$mode <- "stacked"
  }
  vals <- lane_dom_vals(plan, idx, rows, kind)
  # A bar encodes value as LENGTH from zero, and length has no way to say
  # "negative": mk_pct_w() takes abs(), so a fall of 30 draws as wide as a
  # rise of 30 and an all-negative column collapses to empty tracks. A
  # signed measure (mean/median of a change column, a waterfall's extreme
  # per subject) is the zero-centred DIVERGING bar instead -- same column,
  # same numbers, and every consumer downstream already switches on `kind`
  # (summarize_axis_domain(), summarize_cells(), summarize_cells_html(), the
  # JSON packer and its summarize-table.js twin). Decided here because this
  # is the first point that has seen the assembled values. `barsplit` is
  # deliberately excluded: stacked segments that go negative are a different
  # problem, and it has no diverging form.
  if (identical(kind, "bar") && any(vals < 0)) {
    amx <- if (length(vals)) max(abs(vals)) else 0
    for (i in idx) {
      plan[[i]]$kind <- "bardiv"
      plan[[i]]$dmax <- amx
      plan[[i]]$dmin <- -amx
    }
    return(plan)
  }
  ext <- lane_dom_extent(vals, kind, plan, idx)
  for (i in idx) {
    plan[[i]]$dmax <- ext$dmax
    plan[[i]]$dmin <- ext$dmin
  }
  plan
}

# Zero belongs to marks that encode value as LENGTH from a baseline --
# the bar, and the simple row's dot, which is a bar's worth of meaning
# with less ink. A box or a mean CI encodes value as POSITION: it is
# read against the other rows, not against zero, so pinning zero only
# squashes data that lives far from it (chart parity: `scale: true` on
# the boxplot, blockr.viz d2dd210).
lane_dom_extent <- function(vals, kind, plan, idx) {
  zero <- kind %in% c("bar", "barsplit") ||
    any(vapply(plan[idx], function(p) isTRUE(p$zero), logical(1L)))
  if (zero) {
    dmax <- if (length(vals)) max(c(0, vals)) else 0
    dmin <- if (kind %in% c("bar", "barsplit")) {
      0
    } else {
      if (length(vals)) min(c(0, vals)) else 0
    }
  } else {
    rng <- if (length(vals)) range(vals) else c(0, 1)
    pad <- lane_dom_pad(rng)
    dmin <- rng[[1L]] - pad
    dmax <- rng[[2L]] + pad
  }
  list(dmin = dmin, dmax = dmax)
}

lane_dom_spark <- function(plan, idx, rows) {
  allx <- numeric()
  ally <- numeric()
  for (i in idx) {
    for (p1 in rows[[plan[[i]]$pts]]) {
      allx <- c(allx, p1$x)
      ally <- c(ally, p1$y, p1$lo, p1$hi)
    }
    # A mean +/- sd reference can exceed the observed range.
    r <- plan[[i]]$ref
    if (!is.null(r)) ally <- c(ally, r$center, r$lo, r$hi)
  }
  allx <- allx[is.finite(allx)]
  ally <- ally[is.finite(ally)]
  xd <- if (length(allx)) range(allx) else c(0, 1)
  yd <- if (length(ally)) range(ally) else c(0, 1)
  if (xd[[2L]] <= xd[[1L]]) xd[[2L]] <- xd[[1L]] + 1
  if (yd[[2L]] <= yd[[1L]]) yd[[2L]] <- yd[[1L]] + 1
  for (i in idx) {
    plan[[i]]$dom <- xd
    plan[[i]]$ydom <- yd
  }
  plan
}

lane_dom_pair <- function(plan, idx, rows) {
  # Value as POSITION, like the box: the data range plus a hair, widened
  # to take in the band and the reference line so neither is clipped.
  vals <- numeric()
  for (i in idx) {
    p <- plan[[i]]
    for (cn in c(p$cols, unlist(p$lcols))) vals <- c(vals, rows[[cn]])
    vals <- c(vals, p$ref)
  }
  vals <- vals[is.finite(vals)]
  rng <- if (length(vals)) range(vals) else c(0, 1)
  pad <- lane_dom_pad(rng)
  for (i in idx) {
    plan[[i]]$dmin <- rng[[1L]] - pad
    plan[[i]]$dmax <- rng[[2L]] + pad
  }
  plan
}

lane_dom_interval <- function(plan, idx, rows) {
  ext <- numeric()
  for (i in idx) {
    for (ss in rows[[plan[[i]]$segs]]) {
      for (sg in ss) ext <- c(ext, sg$s, sg$e)
    }
  }
  ext <- ext[is.finite(ext)]
  is_date <- any(vapply(plan[idx], function(p) {
    isTRUE(p$dom_date)
  }, logical(1L)))
  preset <- unlist(lapply(plan[idx], function(p) p$dom), use.names = FALSE)
  preset <- preset[is.finite(preset)]
  has_preset <- length(preset) > 0L
  xd <- if (length(preset)) {
    range(preset)
  } else if (length(ext)) {
    range(ext)
  } else {
    c(0, 1)
  }
  if (xd[[2L]] <= xd[[1L]]) xd[[2L]] <- xd[[1L]] + 1
  if (!has_preset && !is_date && xd[[1L]] >= 0) {
    xd[[1L]] <- 0
    at <- summarize_axis_ticks(xd[[1L]], xd[[2L]], k = 7L)
    if (length(at) >= 2L) {
      step <- at[[2L]] - at[[1L]]
      if (is.finite(step) && step > 0) {
        xd[[2L]] <- ceiling(xd[[2L]] / step) * step
      }
    }
  }
  for (i in idx) plan[[i]]$dom <- xd
  plan
}
