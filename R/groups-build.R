# Building each summary's columns on the group frames (leaf and parent rows)
# and its plan entry, the step between the prepared rows and the cell model.

# The frame's column names. Each summary copy writes its columns under its id
# `sid`: ".s<i>" for summary i, ".s<i>f<j>" for its copy at facet level j.
# A column is "<sid>_<part>":
#   v      the value (simple, expr, custom bar and number)
#   t      text (field, expr, a failed custom summary)
#   w      a tile's worst `fill` level
#   bc bl bh wl wh n   a distribution's centre, inner and outer range, size
#   pts last           a sparkline's points and last value
#   segs start nseg    a swimlane's segments, first start and count
#   from to lo hi f d n   a pair's ends, band, colour and dash level, leaf count
# A colour split repeats a mark's parts per level j under the id
# "<sid>_L<j>"; a split bar's level `lv` is "<sid>_S_<lv>".

groups_copy_sid <- function(i, j = NULL) {
  paste0(".s", i, if (!is.null(j)) paste0("f", j))
}

# The id of a summary's copy without its facet level: copies with the same
# stem share a scale.
groups_sid_stem <- function(sid) sub("f[0-9]+$", "", sid)

groups_col <- function(sid, part) paste0(sid, "_", part)

groups_level_sid <- function(sid, j) paste0(sid, "_L", j)

groups_split_col <- function(sid, level) paste0(sid, "_S_", level)

#' Build every summary's columns on the leaf and parent frames and collect the
#' plan entries.
#' @return The state `st`: `summaries`, `leaf`, `par_rows`, `plan` and `primary`
#'   (the first summary and its column id, whose value is the sort key).
#' @noRd
groups_build_columns <- function(summaries, leaf, par_rows, build) {
  st <- list(summaries = summaries, leaf = leaf, par_rows = par_rows,
             plan = list(), primary = NULL)
  for (i in seq_along(summaries)) {
    st <- groups_build_summary(st, i, build)
  }
  st
}

# Facet copies of one summary: one over all rows, or one per facet level.
groups_summary_copies <- function(s, i, build) {
  data <- build$data
  fl <- s$.flevels
  if (is.null(fl)) {
    return(list(list(suffix = groups_copy_sid(i), slice = data, level = NULL)))
  }
  lapply(seq_along(fl), function(j) {
    lv <- fl[[j]]
    list(suffix = groups_copy_sid(i, j),
         slice = data[as.character(data[[s$.facet]]) == lv, ,
                      drop = FALSE],
         level = lv, fcol = s$.facet, qualify = build$qualify,
         named = build$n_faceted > 1L)
  })
}

# One summary's copies: fill the frames, add the plan entries.
groups_build_summary <- function(st, i, build) {
  s <- st$summaries[[i]]
  copies <- groups_summary_copies(s, i, build)
  for (cp in copies) {
    cp <- groups_copy_denom(cp, s, build)
    st$leaf <- groups_fill(st$leaf, build$keys, build$keys, cp$slice, s,
                           cp$suffix)
    if (!is.null(st$par_rows)) {
      st$par_rows <- groups_fill(st$par_rows, build$parent, build$keys,
                                 cp$slice, s, cp$suffix, parent_row = TRUE)
    }
    cp <- groups_series_ref(cp, s)
    entry <- groups_summary_plan(s, cp, build$data, build$scale_map)
    if (!is.null(s$.custom)) entry <- groups_custom_plan(entry, s, cp)
    st$plan <- c(st$plan, list(entry))
  }
  if (is.null(st$primary)) {
    st$primary <- list(s = s, i = i, sid = copies[[1L]]$suffix)
  }
  st
}

groups_copy_denom <- function(cp, s, build) {
  data <- build$data
  # A count's N: subjects (count distinct) or rows (count) in the copy's slice,
  # including rows without a group (groups_denom()). On the one row of a
  # table without `by` the count is its own N and would read 100%, so it is
  # skipped; a colour split keeps its shares.
  same_n <- build$all && is.null(s$.color) &&
    !(build$pct_total && !is.null(cp$level))
  if (identical(s$type, "simple") && !same_n &&
        groups_has_pct(chr_state(s$func) %||% "count")) {
    cp$den <- groups_denom(if (build$pct_total) data else cp$slice,
                              chr_state(s$func) %||% "count",
                              chr_state(s$col))
  }
  # Every facet copy says its level's N: subjects in the slice, rows when the
  # board declares no subject.
  if (!is.null(cp$level)) cp$pop <- groups_pop_n(cp$slice, build$subject)
  cp
}

# The sparkline reference is computed per copy over its slice, so every
# sparkline in the column is oriented against the same line.
groups_series_ref <- function(cp, s) {
  if (identical(s$type, "sparkline") &&
        !identical(s$summary %||% "none", "none")) {
    ys <- as.numeric(cp$slice[[chr_state(s$col)]])
    ys <- ys[is.finite(ys)]
    cp$ref <- switch(s$summary,
      mean = list(center = mean(ys)),
      mean_sd = groups_summarize(ys, "mean_sd"),
      median_q1_q3 = groups_summarize(ys, "median_q1_q3")
    )
  }
  cp
}

#' Fill one target frame (leaf keyed by `tkeys`, or the parent rows) with a
#' summary's `<sid>_*` columns over a data slice.
#' @noRd
groups_fill <- function(target, tkeys, keys, slice, s, sid, parent_row = FALSE) {
  if (isTRUE(parent_row) && identical(s$parent, FALSE)) {
    return(groups_blank_summary(target, s, sid))
  }
  if (!is.null(s$.failed)) {
    target[[groups_col(sid, "t")]] <- rep("\u2013", nrow(target))
    return(target)
  }
  if (!is.null(s$.custom)) {
    return(groups_custom_fill(target, tkeys, s, sid, parent_row))
  }
  switch(s$type,
    simple = groups_fill_simple(target, tkeys, slice, s, sid),
    dist = groups_fill_dist(target, tkeys, slice, s, sid),
    field = groups_fill_field(target, tkeys, slice, s, sid),
    expr = groups_fill_expr(target, tkeys, slice, s, sid),
    sparkline = groups_fill_series(target, tkeys, slice, s, sid),
    pair = groups_pair_fill(target, tkeys, keys, slice, s, sid),
    spans = groups_fill_spans(target, tkeys, slice, s, sid, parent_row),
    target
  )
}

# One aggregated value of `slice` per target row, into column `out`.
groups_put_value <- function(target, slice, out, tkeys, s, f) {
  agg <- groups_aggregate(slice, tkeys, f, chr_state(s$col),
                             chr_state(s$col))
  target[[out]] <- groups_match_col(target, agg, tkeys, ".v")
  target
}

groups_fill_simple <- function(target, tkeys, slice, s, sid) {
  f <- chr_state(s$func) %||% "count"
  target <- groups_put_value(target, slice, groups_col(sid, "v"), tkeys, s, f)
  # A tile's paint: index of the worst `fill` level among the group's rows.
  if (!is.null(s$.fill)) {
    target[[groups_col(sid, "w")]] <- groups_tile_worst(
      target, tkeys, slice, s$.fill, s$.fill_levels
    )
  }
  # The pooled value stays the sort key and label; the split draws the
  # per-level values, named `_L<j>_bc` for the dot and `_S_<level>` for the bar.
  cc <- s$.color
  if (!is.null(cc)) {
    dot <- identical(s$show, "dot")
    for (j in seq_along(s$.levels)) {
      lv <- s$.levels[[j]]
      out <- if (dot) {
        groups_col(groups_level_sid(sid, j), "bc")
      } else {
        groups_split_col(sid, lv)
      }
      target <- groups_put_value(target,
                               slice[as.character(slice[[cc]]) == lv, ,
                                     drop = FALSE], out, tkeys, s, f)
    }
  }
  target
}

groups_put_stats <- function(target, slice, prefix, tkeys, s, stats) {
  agg <- groups_stat_agg(slice, tkeys, chr_state(s$col), stats)
  for (nm in setdiff(names(agg), tkeys)) {
    target[[groups_col(prefix, sub("^\\.", "", nm))]] <-
      groups_match_col(target, agg, tkeys, nm)
  }
  target
}

groups_fill_dist <- function(target, tkeys, slice, s, sid) {
  # `.b` is the centre and (unless inner is off) the inner range, `.w` the
  # outer range. Both styles compute both; the style only decides the drawing.
  stats <- list(.b = groups_dist_centre_stat(s))
  if (!identical(s$whiskers %||% "none", "none")) {
    stats$.w <- s$whiskers
  }
  # The pooled glyph is the sort key and the fallback when no colour splits it.
  target <- groups_put_stats(target, slice, sid, tkeys, s, stats)
  # Colour splits a distribution into one glyph per level, each with its own
  # stat columns; a level with no rows in the group stays NA and draws no lane.
  cc <- s$.color
  if (!is.null(cc)) {
    for (j in seq_along(s$.levels)) {
      lv <- s$.levels[[j]]
      target <- groups_put_stats(target,
                               slice[as.character(slice[[cc]]) == lv, ,
                                     drop = FALSE],
                               groups_level_sid(sid, j), tkeys, s, stats)
    }
  }
  target
}

groups_fill_field <- function(target, tkeys, slice, s, sid) {
  g <- dplyr::group_by(slice, dplyr::across(dplyr::all_of(tkeys)))
  agg <- as.data.frame(dplyr::summarise(
    g, .t = groups_field_join(.data[[chr_state(s$col)]]),
    .groups = "drop"
  ))
  m <- groups_match_field(target, agg, tkeys, ".t")
  m[is.na(m)] <- ""
  target[[groups_col(sid, "t")]] <- m
  target
}

groups_fill_expr <- function(target, tkeys, slice, s, sid) {
  ex <- tryCatch(rlang::parse_expr(chr_state(s$expr)),
                 error = function(e) NULL)
  if (is.null(ex)) {
    target[[groups_col(sid, "t")]] <- rep("(parse error)", nrow(target))
    return(target)
  }
  g <- dplyr::group_by(slice, dplyr::across(dplyr::all_of(tkeys)))
  agg <- tryCatch(
    as.data.frame(dplyr::summarise(g, .t = !!ex, .groups = "drop")),
    error = function(e) NULL
  )
  if (is.null(agg)) {
    target[[groups_col(sid, "t")]] <- rep("(error)", nrow(target))
    return(target)
  }
  v <- agg$.t
  if (is.numeric(v)) {
    agg$.t <- as.numeric(v)
    target[[groups_col(sid, "v")]] <-
      groups_match_col(target, agg, tkeys, ".t")
  } else {
    agg$.t <- as.character(v)
    m <- groups_match_field(target, agg, tkeys, ".t")
    m[is.na(m)] <- ""
    target[[groups_col(sid, "t")]] <- m
  }
  target
}

groups_fill_series <- function(target, tkeys, slice, s, sid) {
  pts <- groups_series_split(slice, target, tkeys, s)
  target[[groups_col(sid, "pts")]] <- I(pts)
  target[[groups_col(sid, "last")]] <- vapply(pts, function(p1) {
    if (length(p1$y)) p1$y[[length(p1$y)]] else NA_real_
  }, numeric(1L))
  target
}

groups_fill_spans <- function(target, tkeys, slice, s, sid,
                              parent_row = FALSE) {
  segs <- groups_spans_split(slice, target, tkeys, s)
  target[[groups_col(sid, "segs")]] <- I(segs)
  target[[groups_col(sid, "start")]] <- vapply(segs, function(ss) {
    if (length(ss)) ss[[1L]]$s else NA_real_
  }, numeric(1L))
  target[[groups_col(sid, "nseg")]] <- lengths(segs)
  # The overlays are one row's facts (its visits, its death); a parent row
  # pools its rows' segments but draws none of them.
  if (groups_spans_has_overlay(s)) {
    target[[groups_col(sid, "ovl")]] <- I(if (isTRUE(parent_row)) {
      rep(list(list()), nrow(target))
    } else {
      groups_spans_overlays(slice, target, tkeys, s)
    })
  }
  target
}

#' Does a spans row draw any overlay?
#' @noRd
groups_spans_has_overlay <- function(s) {
  any(vapply(c("ticks", "band_x", "event_x", "open"), function(nm) {
    !is.null(chr_state(s[[nm]]))
  }, logical(1L)))
}

#' The overlays of a spans row, one list per target row: `tk` tick positions,
#' `bd` line pieces (`s`, `e`), `ev` events (`x`, shape index `k`, level
#' `lv`) and `op`, whether the lane ends open. Each reads only the rows where
#' its own columns are set.
#' @noRd
groups_spans_overlays <- function(slice, target, tkeys, s) {
  col <- function(nm) {
    v <- chr_state(s[[nm]])
    if (!is.null(v) && v %in% names(slice)) v
  }
  num <- function(nm) {
    v <- col(nm)
    if (is.null(v)) NULL else groups_as_num(slice[[v]])
  }
  tk <- num("ticks")
  b0 <- num("band_x")
  b1 <- num("band_xend")
  ex <- num("event_x")
  shp <- col("event_shape")
  opc <- col("open")
  op <- if (!is.null(opc)) as.logical(slice[[opc]])
  lv <- s$.shape_levels
  sk <- groups_key_string(slice, tkeys)
  tkk <- groups_key_string(target, tkeys)
  lapply(tkk, function(k) {
    rows <- which(sk == k)
    out <- list()
    if (!is.null(tk)) {
      v <- tk[rows]
      out$tk <- sort(unique(v[is.finite(v)]))
    }
    if (!is.null(b0) && !is.null(b1)) {
      r <- rows[is.finite(b0[rows]) & is.finite(b1[rows])]
      r <- r[order(b0[r])]
      out$bd <- lapply(r, function(j) {
        list(s = b0[[j]], e = max(b0[[j]], b1[[j]]))
      })
    }
    if (!is.null(ex)) {
      r <- rows[is.finite(ex[rows])]
      r <- r[order(ex[r])]
      out$ev <- lapply(r, function(j) {
        l <- if (!is.null(shp)) as.character(slice[[shp]][[j]])
        kk <- if (!is.null(l) && !is.na(l) && length(lv)) match(l, lv) else NA
        list(x = ex[[j]],
             k = if (is.na(kk)) 1L else (kk - 1L) %% 4L + 1L,
             lv = if (!is.null(l) && !is.na(l)) l)
      })
    }
    if (!is.null(op)) out$op <- isTRUE(any(op[rows], na.rm = TRUE))
    out
  })
}

#' @noRd
groups_key_string <- function(df, keys) {
  do.call(paste, c(lapply(keys, function(k) as.character(df[[k]])),
                   list(sep = "\r")))
}

#' @noRd
groups_as_num <- function(v) {
  if (inherits(v, "Date") || inherits(v, "POSIXct")) as.numeric(v)
  else if (is.numeric(v)) as.numeric(v)
  else NULL
}

#' @noRd
groups_series_split <- function(slice, target, tkeys, s) {
  xs <- groups_as_num(slice[[chr_state(s$x)]])
  ys <- as.numeric(slice[[chr_state(s$col)]])
  band <- as.character(c(s$ymin, s$ymax))
  band <- band[nzchar(band)]
  has_band <- length(band) == 2L && all(band %in% names(slice)) &&
    is.numeric(slice[[band[[1L]]]]) && is.numeric(slice[[band[[2L]]]])
  ok <- if (is.null(xs)) rep(FALSE, nrow(slice)) else is.finite(xs) & is.finite(ys)
  sk <- groups_key_string(slice, tkeys)
  tk <- groups_key_string(target, tkeys)
  lapply(tk, function(k) {
    idx <- which(sk == k & ok)
    idx <- idx[order(xs[idx])]
    list(
      x = xs[idx], y = ys[idx],
      lo = if (has_band) as.numeric(slice[[band[[1L]]]])[idx],
      hi = if (has_band) as.numeric(slice[[band[[2L]]]])[idx]
    )
  })
}

#' @noRd
groups_spans_split <- function(slice, target, tkeys, s) {
  xs <- groups_as_num(slice[[chr_state(s$x)]])
  xe <- groups_as_num(slice[[chr_state(s$xend)]])
  color <- chr_state(s$color)
  color <- if (!is.null(color) && color %in% names(slice)) color
  # Full-data level order (s$.levels), so a facet slice missing a level cannot
  # renumber the fills.
  lv <- s$.levels
  # The label column headlines the segment tooltip and keys the same-event
  # hover highlight; fields add columns.
  lbcol <- chr_state(s$label)
  lbcol <- if (!is.null(lbcol) && lbcol %in% names(slice)) lbcol
  fcols <- intersect(as.character(s$tooltip %||% character()), names(slice))
  ok <- if (is.null(xs) || is.null(xe)) {
    rep(FALSE, nrow(slice))
  } else {
    is.finite(xs) & is.finite(xe)
  }
  sk <- groups_key_string(slice, tkeys)
  tk <- groups_key_string(target, tkeys)
  lapply(tk, function(k) {
    idx <- which(sk == k & ok)
    idx <- idx[order(xs[idx])]
    out <- list()
    for (r in idx) {
      f <- if (!is.null(lv) && !is.null(color)) {
        match(as.character(slice[[color]][[r]]), lv)
      } else {
        1L
      }
      if (is.na(f)) next
      sg <- list(s = xs[[r]], e = max(xe[[r]], xs[[r]]), f = f)
      if (!is.null(lbcol)) {
        lb <- as.character(slice[[lbcol]][[r]])
        if (!is.na(lb) && nzchar(lb)) sg$lb <- lb
      }
      if (length(fcols)) {
        sg$fv <- vapply(fcols, function(fc) {
          v <- as.character(slice[[fc]][[r]])
          if (is.na(v)) "" else v
        }, character(1L))
      }
      out[[length(out) + 1L]] <- sg
    }
    out
  })
}

GROUPS_PAIR_WORDS <- c(identity = "", mean = "Mean", median = "Median",
                     min = "Min", max = "Max")

#' Fill one pair row's columns: `_from`, `_to`, band `_lo` / `_hi`, colour level
#' index `_f`, dash level index `_d` and `_n`. A colour split repeats the set
#' per level under `_L<j>`, from that level's rows.
#' A leaf row applies the row's functions to its own rows. A parent row is the
#' mean of its leaf rows' values instead: at the arm, "baseline to peak per
#' subject" is the mean baseline to the mean peak, where the functions over all
#' the arm's rows would give the first subject's baseline and the highest peak.
#' `_n` counts the leaf rows a parent averages over (NA on leaf rows).
#' @noRd
groups_pair_fill <- function(target, tkeys, keys, slice, s, sid) {
  parent_row <- length(tkeys) < length(keys)
  target <- groups_pair_ends(target, slice, sid, s, tkeys, keys, parent_row)
  target[[groups_col(sid, "f")]] <- groups_pair_level_idx(
    target, slice, s$.color %||% s$color, s$.levels, tkeys
  )
  cc <- s$.color
  if (!is.null(cc)) {
    for (j in seq_along(s$.levels)) {
      target <- groups_pair_ends(
        target,
        slice[as.character(slice[[cc]]) == s$.levels[[j]], , drop = FALSE],
        groups_level_sid(sid, j), s, tkeys, keys, parent_row
      )
    }
  }
  target
}

# One group's value of a pair end: the row's function over its own rows.
groups_pair_agg_one <- function(sl, func, col, kk) {
  if (identical(func, "identity")) {
    g <- dplyr::group_by(sl, dplyr::across(dplyr::all_of(kk)))
    out <- as.data.frame(dplyr::summarise(
      g, .v = groups_agg_first(.data[[col]]), .groups = "drop"
    ))
    out$.v <- as.numeric(out$.v)
    out
  } else {
    groups_aggregate(sl, kk, func, col, col)
  }
}

# Leaf values, one per group of `keys`; rows without a group draw no leaf row,
# so no parent averages over them.
groups_pair_leaf_vals <- function(sl, func, col, keys) {
  out <- groups_pair_agg_one(sl, func, col, keys)
  out[stats::complete.cases(out[keys]), , drop = FALSE]
}

groups_pair_per <- function(target, sl, func, col, tkeys, keys, parent_row) {
  agg <- if (parent_row) {
    lv <- groups_pair_leaf_vals(sl, func, col, keys)
    g <- dplyr::group_by(lv, dplyr::across(dplyr::all_of(tkeys)))
    out <- as.data.frame(dplyr::summarise(
      g, .v = if (any(!is.na(.data$.v))) mean(.data$.v, na.rm = TRUE) else
        NA_real_,
      .groups = "drop"
    ))
    out
  } else {
    groups_pair_agg_one(sl, func, col, tkeys)
  }
  groups_match_col(target, agg, tkeys, ".v")
}

groups_pair_level_idx <- function(target, sl, col, lv, tkeys) {
  if (is.null(col) || !length(lv)) return(rep(NA_integer_, nrow(target)))
  g <- dplyr::group_by(sl, dplyr::across(dplyr::all_of(tkeys)))
  agg <- as.data.frame(dplyr::summarise(
    g, .t = {
      x <- as.character(.data[[col]])
      x <- x[!is.na(x)]
      if (length(x)) x[[1L]] else NA_character_
    },
    .groups = "drop"
  ))
  match(groups_match_field(target, agg, tkeys, ".t"), lv)
}

# Leaf rows under each parent row: what its mean is over.
groups_pair_n_leaf <- function(target, sl, tkeys, keys, parent_row) {
  if (!parent_row) return(rep(NA_integer_, nrow(target)))
  lk <- unique(sl[keys])
  lk <- lk[stats::complete.cases(lk), , drop = FALSE]
  agg <- as.data.frame(dplyr::count(lk, dplyr::across(dplyr::all_of(tkeys)),
                                    name = ".v"))
  as.integer(groups_match_col(target, agg, tkeys, ".v"))
}

# A band bound: a number, or the mean of a column.
groups_pair_band <- function(target, sl, b, tkeys, keys, parent_row) {
  if (is.null(b)) return(rep(NA_real_, nrow(target)))
  if (is.numeric(b)) return(rep(b, nrow(target)))
  groups_pair_per(target, sl, "mean", b, tkeys, keys, parent_row)
}

# Both ends, band, dash index and leaf count of one slice under `prefix` (the
# pooled pair, or one colour level).
groups_pair_ends <- function(target, sl, prefix, s, tkeys, keys, parent_row) {
  target[[groups_col(prefix, "from")]] <- groups_pair_per(
    target, sl, s$x_func, s$x, tkeys, keys, parent_row
  )
  target[[groups_col(prefix, "to")]] <- groups_pair_per(
    target, sl, s$xend_func, s$xend, tkeys, keys, parent_row
  )
  target[[groups_col(prefix, "lo")]] <- groups_pair_band(
    target, sl, s$ref_min, tkeys, keys, parent_row
  )
  target[[groups_col(prefix, "hi")]] <- groups_pair_band(
    target, sl, s$ref_max, tkeys, keys, parent_row
  )
  target[[groups_col(prefix, "n")]] <- groups_pair_n_leaf(
    target, sl, tkeys, keys, parent_row
  )
  dash <- s$linetype
  target[[groups_col(prefix, "d")]] <- if (!is.null(dash) &&
                                          length(s$.dlevels)) {
    groups_pair_level_idx(target, sl, dash, s$.dlevels, tkeys)
  } else {
    rep(NA_integer_, nrow(target))
  }
  target
}
