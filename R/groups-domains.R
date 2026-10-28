# The scales the groups marks are drawn against: one domain per column, or
# shared across a facet.

#' Is a `grouped` colour split degenerate: no row anywhere carries more than
#' one level? Grouped would draw one bar and N-1 empty lanes (a colour that is
#' a group-level fact, such as arm on a per-subject row), so the caller stacks;
#' with one part per row both layouts draw the same numbers. Decided over the
#' assembled rows, since the plan is built before any value exists.
#' @noRd
groups_split_degenerate <- function(plan, idx, rows) {
  for (i in idx) {
    p <- plan[[i]]
    if (!identical(p$kind, "barsplit") || !identical(p$mode, "grouped")) {
      return(FALSE)
    }
    cn <- p$scols[p$scols %in% names(rows)]
    # Summed across level columns, not tabulated per row: safe for a one-row
    # table, where apply() would collapse to a vector.
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
groups_summary_domains <- function(plan, rows) {
  groups <- split(seq_along(plan), vapply(plan, function(p) {
    groups_sid_stem(p$sid %||% "")
  }, character(1L)))
  for (idx in groups) {
    kinds <- unique(vapply(plan[idx], function(p) p$kind, character(1L)))
    kind <- kinds[[1L]]
    plan <- if (identical(kind, "tile")) {
      groups_dom_tile(plan, idx, rows)
    } else if (kind %in% c("bar", "barsplit", "box", "pointrange")) {
      groups_dom_value(plan, idx, rows, kind)
    } else if (identical(kind, "sparkline")) {
      groups_dom_spark(plan, idx, rows)
    } else if (identical(kind, "pair")) {
      groups_dom_pair(plan, idx, rows)
    } else if (identical(kind, "interval")) {
      groups_dom_interval(plan, idx, rows)
    } else {
      plan
    }
  }
  plan
}

# A hair of padding so a glyph at the extreme is not flush against the cell
# edge, and a degenerate (all-equal) column still draws.
groups_dom_pad <- function(rng) {
  if (rng[[2L]] > rng[[1L]]) {
    (rng[[2L]] - rng[[1L]]) * 0.04
  } else {
    max(abs(rng[[1L]]) * 0.04, 0.5)
  }
}

# A tile without a paint column is painted by its value: one ramp across
# the summary's copies, from zero.
groups_dom_tile <- function(plan, idx, rows) {
  # Leaf rows only: a group row's total would turn every cell under it pale;
  # it clamps at the darkest step.
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

# Value-axis columns of a bar, box or point range. A box's `n` (group size)
# is left out: it is not a position.
groups_dom_cols <- function(p, kind) {
  if (kind %in% c("bar", "barsplit")) {
    # The split bar scales on its TOTAL, like the plain bar; side-by-side
    # segments are read one at a time, so the axis must reach the widest.
    if (identical(p$mode, "grouped")) {
      c(p$key, p$scols)
    } else {
      p$key
    }
  } else {
    pos <- function(cm) unname(cm[setdiff(names(cm), "n")])
    # A colour split's levels share ONE scale.
    c(pos(p$cols), unlist(lapply(p$lcols %||% list(), pos)))
  }
}

groups_dom_vals <- function(plan, idx, rows, kind) {
  vals <- numeric()
  for (i in idx) {
    p <- plan[[i]]
    cols <- groups_dom_cols(p, kind)
    # A count with an N is drawn as a percentage (groups_cells()), so its
    # domain is too.
    scale <- if (is.numeric(p$denom) && length(p$denom) == 1L &&
                   is.finite(p$denom) && p$denom > 0) 100 / p$denom else 1
    for (cn in cols) vals <- c(vals, rows[[cn]] * scale)
  }
  vals[is.finite(vals)]
}

groups_dom_value <- function(plan, idx, rows, kind) {
  # Decided before the domain (grouped and stacked read different columns) and
  # for the whole stem, so facet copies keep one layout.
  if (identical(kind, "barsplit") && groups_split_degenerate(plan, idx, rows)) {
    for (i in idx) plan[[i]]$mode <- "stacked"
  }
  vals <- groups_dom_vals(plan, idx, rows, kind)
  # A bar encodes value as length from zero, so negatives cannot be drawn. A
  # signed measure becomes the zero-centred DIVERGING bar (`bardiv`); consumers
  # switch on `kind`. `barsplit` is excluded: it has no diverging form.
  if (identical(kind, "bar") && any(vals < 0)) {
    amx <- if (length(vals)) max(abs(vals)) else 0
    for (i in idx) {
      plan[[i]]$kind <- "bardiv"
      plan[[i]]$dmax <- amx
      plan[[i]]$dmin <- -amx
    }
    return(plan)
  }
  ext <- groups_dom_extent(vals, kind, plan, idx)
  for (i in idx) {
    plan[[i]]$dmax <- ext$dmax
    plan[[i]]$dmin <- ext$dmin
  }
  plan
}

# Zero belongs to marks that encode value as length from a baseline (the bar,
# the simple dot). A box or mean CI encodes position, read against other rows,
# so pinning zero would squash data far from it (chart parity: `scale: true`).
groups_dom_extent <- function(vals, kind, plan, idx) {
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
    pad <- groups_dom_pad(rng)
    dmin <- rng[[1L]] - pad
    dmax <- rng[[2L]] + pad
  }
  list(dmin = dmin, dmax = dmax)
}

groups_dom_spark <- function(plan, idx, rows) {
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

groups_dom_pair <- function(plan, idx, rows) {
  # Value as POSITION, like the box: the data range plus padding, widened to
  # take in the band and the reference line.
  vals <- numeric()
  for (i in idx) {
    p <- plan[[i]]
    for (cn in c(p$cols, unlist(p$lcols))) vals <- c(vals, rows[[cn]])
    vals <- c(vals, p$ref)
  }
  vals <- vals[is.finite(vals)]
  rng <- if (length(vals)) range(vals) else c(0, 1)
  pad <- groups_dom_pad(rng)
  for (i in idx) {
    plan[[i]]$dmin <- rng[[1L]] - pad
    plan[[i]]$dmax <- rng[[2L]] + pad
  }
  plan
}

groups_dom_interval <- function(plan, idx, rows) {
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
    at <- cells_axis_ticks(xd[[1L]], xd[[2L]], k = 7L)
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
