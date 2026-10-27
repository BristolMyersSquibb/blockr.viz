# Table model, groups layout: building the columns ------------------------------
#
# The per-summary build of the groups preparer. One builder, applied per
# target frame (leaf keyed by `keys`, parents keyed by `parent`) and per data
# slice (pooled = all rows; cell = one slice per facet level). Each
# application fills `<sid>_*` columns and the plan entry for the column is
# described by table-model-groups-plan.R. Also the series, spans and pair
# builders, which are keyed by arbitrary key combos rather than one group
# column.

#' Build every summary's columns on the leaf and parent frames and collect
#' the plan entries. The state `st` carries `summaries`, `leaf`, `par_rows`,
#' `plan` and `primary` (the first summary and its column id, whose value is
#' the sort key).
#' @param ctx The preparer's context: `data`, `keys`, `parent`, `all`,
#'   `pct_total`, `qualify`, `n_faceted`, `scale_map`, `subject`.
#' @noRd
lane_build_columns <- function(summaries, leaf, par_rows, ctx) {
  st <- list(summaries = summaries, leaf = leaf, par_rows = par_rows,
             plan = list(), primary = NULL)
  for (i in seq_along(summaries)) {
    st <- lane_build_summary(st, i, ctx)
  }
  st
}

# The facet copies of one summary: one unfaceted copy over all rows, or one
# per facet level over that level's rows.
lane_summary_copies <- function(s, i, ctx) {
  data <- ctx$data
  fl <- s$.flevels
  if (is.null(fl)) {
    return(list(list(suffix = paste0(".s", i), slice = data, level = NULL)))
  }
  lapply(seq_along(fl), function(j) {
    lv <- fl[[j]]
    list(suffix = paste0(".s", i, "f", j),
         slice = data[as.character(data[[s$.facet]]) == lv, ,
                      drop = FALSE],
         level = lv, fcol = s$.facet, qualify = ctx$qualify,
         named = ctx$n_faceted > 1L)
  })
}

# One summary's copies: fill the frames, add the plan entries. A custom
# function that fails is the user's code: its column stays empty, with the
# error beside it, and every other column draws. The copies already built
# for it go, columns and plan entries, from the leaf and the parent rows
# alike; the failed column is one copy. (When the function failed only on
# the parent rows, the leaf kept that copy's columns and the two frames no
# longer bound into one table.)
lane_build_summary <- function(st, i, ctx) {
  s <- st$summaries[[i]]
  copies <- lane_summary_copies(s, i, ctx)
  sid1 <- copies[[1L]]$suffix
  n0 <- length(st$plan)
  cols0 <- list(leaf = names(st$leaf), par = names(st$par_rows))
  for (cp in copies) {
    cp <- lane_copy_denom(cp, s, ctx)
    res <- lane_fill_copy(st, cp, s, ctx)
    st <- res$st
    if (!is.null(res$failed)) {
      s <- lane_failed_summary(s, res$failed)
      st$summaries[[i]] <- s
      sid1 <- paste0(".s", i)
      st$leaf <- st$leaf[cols0$leaf]
      if (!is.null(st$par_rows)) st$par_rows <- st$par_rows[cols0$par]
      st <- lane_replace_failed(st, s, sid1, n0, ctx)
      break
    }
    cp <- lane_series_ref(cp, s)
    entry <- lane_summary_plan(s, cp, ctx$data, ctx$scale_map)
    if (!is.null(s$.custom)) entry <- lane_custom_plan(entry, s, cp)
    st$plan <- c(st$plan, list(entry))
  }
  if (is.null(st$primary)) st$primary <- list(s = s, sid = sid1)
  st
}

# A copy's N: the denominator of a count's percentage and the population of a
# facet copy.
lane_copy_denom <- function(cp, s, ctx) {
  data <- ctx$data
  # A count's N: the subjects (count distinct) or rows (count) in this
  # copy's slice -- the whole data for an unfaceted column, one level's
  # rows for a facet copy. Over the full slice, so rows without a group
  # (the population join's) are in it (summarize_denom()).
  # On the one row of a table without `by` the count is its own N, so
  # its share would read 100%; a colour split keeps its shares.
  same_n <- ctx$all && is.null(s$.color) &&
    !(ctx$pct_total && !is.null(cp$level))
  if (identical(s$type, "simple") && !same_n &&
        summarize_has_pct(summarize_chr1(s$func) %||% "count")) {
    cp$den <- summarize_denom(if (ctx$pct_total) data else cp$slice,
                              summarize_chr1(s$func) %||% "count",
                              summarize_chr1(s$col))
  }
  # Every facet copy says its level's N, whatever the column shows: the
  # subjects in the slice, rows when the board declares no subject.
  if (!is.null(cp$level)) cp$pop <- summarize_pop_n(cp$slice, ctx$subject)
  cp
}

# Fill the leaf and parent frames for one copy. Returns `list(st, failed)`;
# `failed` is the message of a custom function's error, else NULL. On a
# failure the caller drops what the summary wrote (lane_build_summary()).
lane_fill_copy <- function(st, cp, s, ctx) {
  r <- lane_try_fill(st$leaf, ctx$keys, ctx$keys, cp$slice, s, cp$suffix)
  if (!inherits(r, "lane_custom_error")) {
    st$leaf <- r
    if (!is.null(st$par_rows)) {
      r <- lane_try_fill(st$par_rows, ctx$parent, ctx$keys, cp$slice, s,
                         cp$suffix, parent_row = TRUE)
      if (!inherits(r, "lane_custom_error")) st$par_rows <- r
    }
  }
  failed <- if (inherits(r, "lane_custom_error")) conditionMessage(r)
  list(st = st, failed = failed)
}

# lane_fill() with a custom function's failure returned as the condition.
lane_try_fill <- function(target, tkeys, keys, slice, s, sid,
                          parent_row = FALSE) {
  tryCatch(
    lane_fill(target, tkeys, keys, slice, s, sid, parent_row = parent_row),
    lane_custom_error = function(e) e
  )
}

# A failed custom summary is one text column over all rows: drop the plan
# entries its copies added, fill the dashes and add its single entry.
lane_replace_failed <- function(st, s, sid1, n0, ctx) {
  data <- ctx$data
  cp <- list(suffix = sid1, slice = data, level = NULL)
  st$plan <- st$plan[seq_len(n0)]
  st$leaf <- lane_fill(st$leaf, ctx$keys, ctx$keys, data, s, sid1)
  if (!is.null(st$par_rows)) {
    st$par_rows <- lane_fill(st$par_rows, ctx$parent, ctx$keys, data, s, sid1,
                             parent_row = TRUE)
  }
  st$plan <- c(st$plan,
               list(lane_summary_plan(s, cp, data, ctx$scale_map)))
  st
}

# The series reference is computed per COPY over the slice's values:
# every sparkline in the column is oriented against the same line
# (per facet level, its own level's line).
lane_series_ref <- function(cp, s) {
  if (identical(s$type, "series") &&
        !identical(s$ref %||% "none", "none")) {
    ys <- as.numeric(cp$slice[[summarize_chr1(s$col)]])
    ys <- ys[is.finite(ys)]
    cp$ref <- switch(s$ref,
      mean = list(center = mean(ys)),
      mean_sd = lane_summarize(ys, "mean_sd"),
      median_iqr = lane_summarize(ys, "median_q1_q3")
    )
  }
  cp
}

#' Fill one target frame (leaf keyed by `tkeys`, or the parent rows) with a
#' summary's `<sid>_*` columns over a data slice.
#' @noRd
lane_fill <- function(target, tkeys, keys, slice, s, sid, parent_row = FALSE) {
  if (isTRUE(parent_row) && identical(s$parent, FALSE)) {
    return(lane_blank_summary(target, s, sid))
  }
  if (!is.null(s$.failed)) {
    target[[paste0(sid, "_t")]] <- rep("\u2013", nrow(target))
    return(target)
  }
  if (!is.null(s$.custom)) {
    return(lane_custom_fill(target, tkeys, slice, s, sid))
  }
  switch(s$type,
    simple = lane_fill_simple(target, tkeys, slice, s, sid),
    dist = lane_fill_dist(target, tkeys, slice, s, sid),
    field = lane_fill_field(target, tkeys, slice, s, sid),
    expr = lane_fill_expr(target, tkeys, slice, s, sid),
    series = lane_fill_series(target, tkeys, slice, s, sid),
    pair = lane_pair_fill(target, tkeys, keys, slice, s, sid),
    spans = lane_fill_spans(target, tkeys, slice, s, sid),
    target
  )
}

# One aggregated value of `slice` per target row, into column `out`.
lane_put_value <- function(target, slice, out, tkeys, s, f) {
  agg <- summarize_aggregate(slice, tkeys, f, summarize_chr1(s$col),
                             summarize_chr1(s$col))
  target[[out]] <- summarize_match_col(target, agg, tkeys, ".v")
  target
}

lane_fill_simple <- function(target, tkeys, slice, s, sid) {
  f <- summarize_chr1(s$func) %||% "count"
  target <- lane_put_value(target, slice, paste0(sid, "_v"), tkeys, s, f)
  # A tile's paint: the worst level of `fill` among the group's rows,
  # as an index into the column's levels (NA = no row with a level).
  if (!is.null(s$.fill)) {
    target[[paste0(sid, "_w")]] <- lane_tile_worst(
      target, tkeys, slice, s$.fill, s$.fill_levels
    )
  }
  # The pooled value above stays the sort key and the label either
  # way; the per-level values are what the split draws. The dot names
  # them for lane_color_split()'s geometry keys, the bar for the
  # barsplit emitter's `prefix + level` lookup.
  cc <- s$.color
  if (!is.null(cc)) {
    dot <- identical(s$show, "dot")
    for (j in seq_along(s$.levels)) {
      lv <- s$.levels[[j]]
      out <- if (dot) {
        paste0(sid, "_L", j, "_bc")
      } else {
        paste0(sid, "_S_", lv)
      }
      target <- lane_put_value(target,
                               slice[as.character(slice[[cc]]) == lv, ,
                                     drop = FALSE], out, tkeys, s, f)
    }
  }
  target
}

# The per-group statistics of a distribution column, under `prefix`.
lane_put_stats <- function(target, slice, prefix, tkeys, s, stats) {
  agg <- lane_stat_agg(slice, tkeys, summarize_chr1(s$col), stats)
  for (nm in setdiff(names(agg), tkeys)) {
    target[[paste0(prefix, "_", sub("^\\.", "", nm))]] <-
      summarize_match_col(target, agg, tkeys, nm)
  }
  target
}

lane_fill_dist <- function(target, tkeys, slice, s, sid) {
  # `.b` carries the centre and (unless inner is off) the inner range;
  # `.w` the outer range. Both styles compute both -- the style decides
  # how they are drawn, never which numbers exist.
  stats <- list(.b = lane_dist_centre_stat(s))
  if (!identical(s$outer %||% "none", "none")) {
    stats$.w <- s$outer
  }
  # The pooled glyph is computed either way: it is the column's sort
  # key and its fallback when no colour splits it.
  target <- lane_put_stats(target, slice, sid, tkeys, s, stats)
  # The colour dimension (chart parity: colour splits a distribution
  # into one glyph per level). Each level gets its own stat columns;
  # a level with no rows in this group stays NA and draws no lane.
  cc <- s$.color
  if (!is.null(cc)) {
    for (j in seq_along(s$.levels)) {
      lv <- s$.levels[[j]]
      target <- lane_put_stats(target,
                               slice[as.character(slice[[cc]]) == lv, ,
                                     drop = FALSE],
                               paste0(sid, "_L", j), tkeys, s, stats)
    }
  }
  target
}

lane_fill_field <- function(target, tkeys, slice, s, sid) {
  g <- dplyr::group_by(slice, dplyr::across(dplyr::all_of(tkeys)))
  agg <- as.data.frame(dplyr::summarise(
    g, .t = lane_field_join(.data[[summarize_chr1(s$col)]]),
    .groups = "drop"
  ))
  m <- summarize_match_field(target, agg, tkeys, ".t")
  m[is.na(m)] <- ""
  target[[paste0(sid, "_t")]] <- m
  target
}

lane_fill_expr <- function(target, tkeys, slice, s, sid) {
  ex <- tryCatch(rlang::parse_expr(summarize_chr1(s$expr)),
                 error = function(e) NULL)
  if (is.null(ex)) {
    target[[paste0(sid, "_t")]] <- rep("(parse error)", nrow(target))
    return(target)
  }
  g <- dplyr::group_by(slice, dplyr::across(dplyr::all_of(tkeys)))
  agg <- tryCatch(
    as.data.frame(dplyr::summarise(g, .t = !!ex, .groups = "drop")),
    error = function(e) NULL
  )
  if (is.null(agg)) {
    target[[paste0(sid, "_t")]] <- rep("(error)", nrow(target))
    return(target)
  }
  v <- agg$.t
  if (is.numeric(v)) {
    agg$.t <- as.numeric(v)
    target[[paste0(sid, "_v")]] <-
      summarize_match_col(target, agg, tkeys, ".t")
  } else {
    agg$.t <- as.character(v)
    m <- summarize_match_field(target, agg, tkeys, ".t")
    m[is.na(m)] <- ""
    target[[paste0(sid, "_t")]] <- m
  }
  target
}

lane_fill_series <- function(target, tkeys, slice, s, sid) {
  pts <- lane_series_split(slice, target, tkeys, s)
  target[[paste0(sid, "_pts")]] <- I(pts)
  target[[paste0(sid, "_last")]] <- vapply(pts, function(p1) {
    if (length(p1$y)) p1$y[[length(p1$y)]] else NA_real_
  }, numeric(1L))
  target
}

lane_fill_spans <- function(target, tkeys, slice, s, sid) {
  segs <- lane_spans_split(slice, target, tkeys, s)
  target[[paste0(sid, "_segs")]] <- I(segs)
  target[[paste0(sid, "_start")]] <- vapply(segs, function(ss) {
    if (length(ss)) ss[[1L]]$s else NA_real_
  }, numeric(1L))
  target[[paste0(sid, "_nseg")]] <- lengths(segs)
  target
}

# --- series / spans builders --------------------------------------------------
# Compact re-statements of the single-mark builders in the variables layout,
# keyed by arbitrary key combos rather than one group column. Each returns
# a list-column aligned to `target`, plus stashes the raw extents as
# attributes for the domain pass.

#' @noRd
lane_key_string <- function(df, keys) {
  do.call(paste, c(lapply(keys, function(k) as.character(df[[k]])),
                   list(sep = "\r")))
}

#' @noRd
lane_as_num <- function(v) {
  if (inherits(v, "Date") || inherits(v, "POSIXct")) as.numeric(v)
  else if (is.numeric(v)) as.numeric(v)
  else NULL
}

#' @noRd
lane_series_split <- function(slice, target, tkeys, s) {
  xs <- lane_as_num(slice[[summarize_chr1(s$x)]])
  ys <- as.numeric(slice[[summarize_chr1(s$col)]])
  band <- as.character(s$band %||% character())
  band <- band[nzchar(band)]
  has_band <- length(band) == 2L && all(band %in% names(slice)) &&
    is.numeric(slice[[band[[1L]]]]) && is.numeric(slice[[band[[2L]]]])
  ok <- if (is.null(xs)) rep(FALSE, nrow(slice)) else is.finite(xs) & is.finite(ys)
  sk <- lane_key_string(slice, tkeys)
  tk <- lane_key_string(target, tkeys)
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
lane_spans_split <- function(slice, target, tkeys, s) {
  xs <- lane_as_num(slice[[summarize_chr1(s$x)]])
  xe <- lane_as_num(slice[[summarize_chr1(s$xend)]])
  color <- summarize_chr1(s$color)
  color <- if (!is.null(color) && color %in% names(slice)) color
  # Full-data level order rides on the summary (s$.levels), so a facet
  # slice missing a level cannot renumber the fills.
  lv <- s$.levels
  # Event identity: the label column headlines the segment tooltip and
  # keys the same-event hover highlight; fields append extra columns.
  lbcol <- summarize_chr1(s$label)
  lbcol <- if (!is.null(lbcol) && lbcol %in% names(slice)) lbcol
  fcols <- intersect(as.character(s$fields %||% character()), names(slice))
  ok <- if (is.null(xs) || is.null(xe)) {
    rep(FALSE, nrow(slice))
  } else {
    is.finite(xs) & is.finite(xe)
  }
  sk <- lane_key_string(slice, tkeys)
  tk <- lane_key_string(target, tkeys)
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

# --- pair builder ---------------------------------------------------------------

LANE_PAIR_WORDS <- c(identity = "", mean = "Mean", median = "Median",
                     min = "Min", max = "Max")

#' Fill one pair row's columns on `target`: `_from`, `_to`, the band `_lo` /
#' `_hi` (NA where there is none), the colour level index `_f` and the dash
#' level index `_d`. A colour split adds the same set per level under
#' `_L<j>`, from that level's rows only.
#'
#' A leaf row summarises its own rows with the row's functions. A parent row
#' (the arm above its subjects) is the mean of its leaf rows' values instead:
#' "baseline to peak per subject" read at the arm is the mean baseline to the
#' mean peak, where the row's functions over all of the arm's rows would give
#' the first subject's baseline and the arm's single highest value. The band
#' and every colour level follow the same rule, and `_n` counts the leaf rows
#' a parent averages over (NA on leaf rows; per level under `_L<j>_n`).
#' @noRd
lane_pair_fill <- function(target, tkeys, keys, slice, s, sid) {
  parent_row <- length(tkeys) < length(keys)
  target <- lane_pair_ends(target, slice, sid, s, tkeys, keys, parent_row)
  target[[paste0(sid, "_f")]] <- lane_pair_level_idx(
    target, slice, s$.color %||% s$color, s$.levels, tkeys
  )
  cc <- s$.color
  if (!is.null(cc)) {
    for (j in seq_along(s$.levels)) {
      target <- lane_pair_ends(
        target,
        slice[as.character(slice[[cc]]) == s$.levels[[j]], , drop = FALSE],
        paste0(sid, "_L", j), s, tkeys, keys, parent_row
      )
    }
  }
  target
}

# One group's value of a pair end: the row's function over its own rows, or
# for a parent row the mean of its leaf rows' values.
lane_pair_agg_one <- function(sl, func, col, kk) {
  if (identical(func, "identity")) {
    g <- dplyr::group_by(sl, dplyr::across(dplyr::all_of(kk)))
    out <- as.data.frame(dplyr::summarise(
      g, .v = summarize_agg_first(.data[[col]]), .groups = "drop"
    ))
    out$.v <- as.numeric(out$.v)
    out
  } else {
    summarize_aggregate(sl, kk, func, col, col)
  }
}

# The leaf rows' values: one per group of `keys`, without the rows that
# have no group (they draw no leaf row, so no parent averages over them).
lane_pair_leaf_vals <- function(sl, func, col, keys) {
  out <- lane_pair_agg_one(sl, func, col, keys)
  out[stats::complete.cases(out[keys]), , drop = FALSE]
}

lane_pair_per <- function(target, sl, func, col, tkeys, keys, parent_row) {
  agg <- if (parent_row) {
    lv <- lane_pair_leaf_vals(sl, func, col, keys)
    g <- dplyr::group_by(lv, dplyr::across(dplyr::all_of(tkeys)))
    out <- as.data.frame(dplyr::summarise(
      g, .v = if (any(!is.na(.data$.v))) mean(.data$.v, na.rm = TRUE) else
        NA_real_,
      .groups = "drop"
    ))
    out
  } else {
    lane_pair_agg_one(sl, func, col, tkeys)
  }
  summarize_match_col(target, agg, tkeys, ".v")
}

lane_pair_level_idx <- function(target, sl, col, lv, tkeys) {
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
  match(summarize_match_field(target, agg, tkeys, ".t"), lv)
}

# The leaf rows of the slice under each parent row: what its mean is over.
lane_pair_n_leaf <- function(target, sl, tkeys, keys, parent_row) {
  if (!parent_row) return(rep(NA_integer_, nrow(target)))
  lk <- unique(sl[keys])
  lk <- lk[stats::complete.cases(lk), , drop = FALSE]
  agg <- as.data.frame(dplyr::count(lk, dplyr::across(dplyr::all_of(tkeys)),
                                    name = ".v"))
  as.integer(summarize_match_col(target, agg, tkeys, ".v"))
}

# A band bound: a number, or the mean of a column.
lane_pair_band <- function(target, sl, b, tkeys, keys, parent_row) {
  if (is.null(b)) return(rep(NA_real_, nrow(target)))
  if (is.numeric(b)) return(rep(b, nrow(target)))
  lane_pair_per(target, sl, "mean", b, tkeys, keys, parent_row)
}

# Both ends, the band, the dash index and the leaf count of one slice,
# under `prefix`: once for the pooled pair, once per colour level for the
# split.
lane_pair_ends <- function(target, sl, prefix, s, tkeys, keys, parent_row) {
  target[[paste0(prefix, "_from")]] <- lane_pair_per(
    target, sl, s$from_func, s$from, tkeys, keys, parent_row
  )
  target[[paste0(prefix, "_to")]] <- lane_pair_per(
    target, sl, s$to_func, s$to, tkeys, keys, parent_row
  )
  target[[paste0(prefix, "_lo")]] <- lane_pair_band(
    target, sl, s$lo, tkeys, keys, parent_row
  )
  target[[paste0(prefix, "_hi")]] <- lane_pair_band(
    target, sl, s$hi, tkeys, keys, parent_row
  )
  target[[paste0(prefix, "_n")]] <- lane_pair_n_leaf(
    target, sl, tkeys, keys, parent_row
  )
  dash <- s$dash
  target[[paste0(prefix, "_d")]] <- if (!is.null(dash) &&
                                          length(s$.dlevels)) {
    lane_pair_level_idx(target, sl, dash, s$.dlevels, tkeys)
  } else {
    rep(NA_integer_, nrow(target))
  }
  target
}
