# Summarize table: the generic column-list preparer ---------------------------
#
# The lane chart's deeper model, per _blockr.design/open/summarize-table/:
# one row per group, each column a summarising function over the group's
# rows, rendered as text or as a glyph. `summaries` is an ordered list of
# typed row objects (the blockr.dplyr summarize block's schema plus a
# display axis); this file normalizes it and prepares the same prep
# contract the renderer already walks. Every row type maps onto an
# EXISTING plan kind (bar / box / pointrange / interval / sparkline /
# num), so the emitters and the R/JS drift guard are untouched.

# Row types and the displays each allows. The first display is the type's
# default. "dot" is a single value as a positioned point on the zero-based
# lane -- less ink than a bar when magnitude comparison is not the point.
LANE_ROW_TYPES <- list(
  simple = c("bar", "number", "dot"),
  dist = c("box", "pointrange", "text"),
  field = "text",
  series = "sparkline",
  spans = "interval",
  expr = "text"
)

#' Normalize a summaries list: known types, per-type required fields,
#' `show` within the type's set, `scope` cell/pooled, an auto `name`.
#' Returns the normalized list, or `list(err =)` naming the first broken
#' row (a config error must say which row, not throw a stack trace).
#' @noRd
lane_norm_summaries <- function(summaries) {
  if (!is.list(summaries)) return(list(err = "`summaries` must be a list"))
  out <- vector("list", length(summaries))
  for (i in seq_along(summaries)) {
    s <- summaries[[i]]
    if (!is.list(s)) {
      return(list(err = paste0("Summary ", i, " is not a list")))
    }
    type <- rank_chr1(s$type) %||% "simple"
    if (!type %in% names(LANE_ROW_TYPES)) {
      return(list(err = paste0("Summary ", i, ": unknown type \"", type,
                               "\"")))
    }
    shows <- LANE_ROW_TYPES[[type]]
    show <- rank_chr1(s$show) %||% shows[[1L]]
    if (!show %in% shows) show <- shows[[1L]]
    scope <- rank_chr1(s$scope) %||% "cell"
    if (!scope %in% c("cell", "pooled")) scope <- "cell"
    # Fields are group facts: always pooled (they stand outside faceting).
    if (identical(type, "field")) scope <- "pooled"

    need <- switch(type,
      simple = if (!identical(rank_chr1(s$func) %||% "count", "count")) "col",
      dist = "col",
      field = "col",
      series = c("x", "col"),
      spans = c("x", "xend"),
      expr = "expr"
    )
    for (nm in need) {
      if (is.null(rank_chr1(s[[nm]]))) {
        return(list(err = paste0("Summary ", i, " (", type, "): `", nm,
                                 "` is required")))
      }
    }

    s$type <- type
    s$show <- show
    s$scope <- scope
    s$name <- rank_chr1(s$name) %||% lane_summary_auto_name(s)
    out[[i]] <- s
  }
  out
}

#' @noRd
lane_summary_auto_name <- function(s) {
  switch(s$type,
    simple = {
      f <- rank_chr1(s$func) %||% "count"
      if (identical(f, "count")) "Rows" else
        paste0(AGG_WORDS[[f]] %||% f, " ", rank_chr1(s$col))
    },
    dist = rank_chr1(s$col),
    field = rank_chr1(s$col),
    series = rank_chr1(s$col),
    spans = paste0(rank_chr1(s$x), " → ", rank_chr1(s$xend)),
    expr = "Value",
    "Value"
  )
}

# The fold cap for field cells: join up to LANE_FIELD_JOIN distinct values,
# then "+n more"; past LANE_FIELD_MANY just say how many. Never first():
# an arbitrary representative presented as data is a silent lie, and the
# join makes a broken constancy assumption visible ("Placebo, Active").
LANE_FIELD_JOIN <- 3L
LANE_FIELD_MANY <- 8L

#' @noRd
lane_field_join <- function(x) {
  u <- unique(as.character(x))
  u <- u[!is.na(u) & nzchar(u)]
  n <- length(u)
  if (n == 0L) return("")
  if (n > LANE_FIELD_MANY) return(paste0(n, " values"))
  if (n > LANE_FIELD_JOIN) {
    return(paste0(paste(u[seq_len(LANE_FIELD_JOIN)], collapse = ", "),
                  ", +", n - LANE_FIELD_JOIN, " more"))
  }
  paste(u, collapse = ", ")
}

#' The generic preparer: group_by(by) plus the summaries list.
#'
#' `by` is outer -> inner; the renderer nests one level, so at most two
#' columns (the outer becomes the expandable parent). Facet repeats every
#' cell-scoped summary per level (copies adjacent, by_summary layout);
#' pooled summaries and fields render once. Each plan entry carries its
#' own domain, shared across that summary's facet copies -- mixed marks
#' with different units must not share a scale.
#' @noRd
lane_prepare_summaries <- function(data, by, summaries, facet = NULL,
                                   facet_layout = "by_summary",
                                   sort_by = "value", sort_dir = "desc",
                                   top_n = NULL, scale_map = NULL) {
  bad <- function(msg) list(err = msg)

  by <- as.character(by %||% character())
  by <- by[nzchar(by)]
  if (!length(by)) return(bad("Pick a Group by column in the gear"))
  if (length(by) > 2L) {
    return(bad("Group by supports at most two columns (outer, inner)"))
  }
  miss <- setdiff(by, names(data))
  if (length(miss)) {
    return(bad(paste0("Mapped column not in data: ",
                      paste0("\"", miss, "\"", collapse = ", "),
                      ". Re-pick it in the gear.")))
  }
  group <- by[[length(by)]]
  parent <- if (length(by) == 2L) by[[1L]]
  keys <- c(parent, group)

  summaries <- lane_norm_summaries(summaries)
  if (!is.null(summaries$err)) return(bad(summaries$err))
  if (!length(summaries)) return(bad("Add at least one summary column"))

  present <- function(col) {
    col <- rank_chr1(col)
    if (is.null(col) || !col %in% names(data)) NULL else col
  }
  facet <- present(facet)
  facet_levels <- character()
  if (!is.null(facet)) {
    facet_levels <- rank_levels(data[[facet]])
    if (length(facet_levels) < 2L) {
      return(bad(paste0(
        "Facet column \"", facet, "\" has fewer than two levels; ",
        "nothing to compare across columns."
      )))
    }
  }

  # Every summary's mapped columns must exist -- reported by row name.
  for (s in summaries) {
    cols <- unlist(s[intersect(names(s), c("col", "x", "xend", "band"))])
    cols <- as.character(cols)
    miss <- setdiff(cols[nzchar(cols)], names(data))
    if (length(miss)) {
      return(bad(paste0(
        "Summary \"", s$name, "\": column not in data: ",
        paste0("\"", miss, "\"", collapse = ", "), "."
      )))
    }
  }

  # --- leaf / parent skeletons ----------------------------------------------
  skel <- unique(data[keys])
  skel <- skel[do.call(order, unname(as.list(skel))), , drop = FALSE]
  leaf <- data.frame(.label = as.character(skel[[group]]),
                     .parent = if (is.null(parent)) NA_character_ else
                       as.character(skel[[parent]]),
                     stringsAsFactors = FALSE)
  for (k in keys) leaf[[k]] <- skel[[k]]
  par_rows <- NULL
  if (!is.null(parent)) {
    pv <- rank_levels(data[[parent]])
    par_rows <- data.frame(.label = pv, .parent = NA_character_,
                           stringsAsFactors = FALSE)
    par_rows[[parent]] <- pv
  }

  # --- per-summary build -----------------------------------------------------
  # One builder, applied per target frame (leaf keyed by `keys`, parents
  # keyed by `parent`) and per data slice (pooled = all rows; cell = one
  # slice per facet level). Each application fills `<sid>_*` columns and
  # returns the plan-entry payload description.
  note <- NULL
  plan <- list()
  s_primary <- NULL

  fill <- function(target, tkeys, slice, s, sid) {
    switch(s$type,
      simple = {
        f <- rank_chr1(s$func) %||% "count"
        agg <- rank_aggregate(slice, tkeys, f, rank_chr1(s$col),
                              rank_chr1(s$col))
        target[[paste0(sid, "_v")]] <- rank_match_col(target, agg, tkeys, ".v")
        target
      },
      dist = {
        stats <- list(.b = rank_chr1(s$stat) %||% "median_q1_q3")
        if (identical(s$show, "box")) {
          stats$.w <- rank_chr1(s$whiskers) %||% "tukey"
        }
        agg <- lane_stat_agg(slice, tkeys, rank_chr1(s$col), stats)
        for (nm in setdiff(names(agg), tkeys)) {
          target[[paste0(sid, "_", sub("^\\.", "", nm))]] <-
            rank_match_col(target, agg, tkeys, nm)
        }
        target
      },
      field = {
        g <- dplyr::group_by(slice, dplyr::across(dplyr::all_of(tkeys)))
        agg <- as.data.frame(dplyr::summarise(
          g, .t = lane_field_join(.data[[rank_chr1(s$col)]]),
          .groups = "drop"
        ))
        m <- rank_match_field(target, agg, tkeys, ".t")
        m[is.na(m)] <- ""
        target[[paste0(sid, "_t")]] <- m
        target
      },
      expr = {
        ex <- tryCatch(rlang::parse_expr(rank_chr1(s$expr)),
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
            rank_match_col(target, agg, tkeys, ".t")
        } else {
          agg$.t <- as.character(v)
          m <- rank_match_field(target, agg, tkeys, ".t")
          m[is.na(m)] <- ""
          target[[paste0(sid, "_t")]] <- m
        }
        target
      },
      series = {
        pts <- lane_series_split(slice, target, tkeys, s)
        target[[paste0(sid, "_pts")]] <- I(pts)
        target[[paste0(sid, "_last")]] <- vapply(pts, function(p1) {
          if (length(p1$y)) p1$y[[length(p1$y)]] else NA_real_
        }, numeric(1L))
        target
      },
      spans = {
        segs <- lane_spans_split(slice, target, tkeys, s)
        target[[paste0(sid, "_segs")]] <- I(segs)
        target[[paste0(sid, "_start")]] <- vapply(segs, function(ss) {
          if (length(ss)) ss[[1L]]$s else NA_real_
        }, numeric(1L))
        target[[paste0(sid, "_nseg")]] <- lengths(segs)
        target
      },
      target
    )
  }

  for (i in seq_along(summaries)) {
    s <- summaries[[i]]
    # Level order and date-ness come from the FULL data, never a facet
    # slice: fills and formats must agree across a summary's copies.
    if (identical(s$type, "spans")) {
      cc <- present(s$color)
      s$.levels <- if (!is.null(cc)) rank_levels(data[[cc]])
      s$.date <- inherits(data[[rank_chr1(s$x)]], "Date")
    }
    if (identical(s$type, "series")) {
      s$.date <- inherits(data[[rank_chr1(s$x)]], "Date")
    }
    pooled <- identical(s$scope, "pooled") || is.null(facet)
    copies <- if (pooled) {
      list(list(suffix = paste0(".s", i), slice = data, level = NULL))
    } else {
      lapply(seq_along(facet_levels), function(j) {
        lv <- facet_levels[[j]]
        list(suffix = paste0(".s", i, "f", j),
             slice = data[as.character(data[[facet]]) == lv, , drop = FALSE],
             level = lv)
      })
    }
    for (cp in copies) {
      leaf <- fill(leaf, keys, cp$slice, s, cp$suffix)
      if (!is.null(par_rows)) {
        par_rows <- fill(par_rows, parent, cp$slice, s, cp$suffix)
      }
      plan <- c(plan, list(lane_summary_plan(s, cp, data, scale_map)))
    }
    if (is.null(s_primary)) s_primary <- list(s = s, sid = copies[[1L]]$suffix)
  }

  # `.v` = the FIRST summary's primary value (its sort key): rank order
  # follows the leading column unless sort_by picks another.
  primary_col <- lane_primary_col(s_primary$s, s_primary$sid)
  leaf$.v <- lane_primary_value(leaf, s_primary$s, primary_col)
  if (!is.null(par_rows)) {
    par_rows$.v <- lane_primary_value(par_rows, s_primary$s, primary_col)
  }

  sort_key <- rank_sort_key(sort_by, plan)
  asm <- rank_assemble_rows(leaf, par_rows, parent, sort_key, sort_dir, top_n)
  rows <- asm$rows

  # --- per-entry domains ------------------------------------------------------
  # Computed over the ASSEMBLED rows (parents included) and shared across a
  # summary's facet copies: one scale per summary, never one scale per table.
  plan <- lane_summary_domains(plan, rows)

  # --- facet layout -----------------------------------------------------------
  # by_summary (default): each summary's level copies sit adjacent, in
  # authored order (adjacency is the comparison affordance). by_level: the
  # Table-1 reading -- pooled columns and fields lead, then one column
  # group per facet level spanning the summaries; the header grows a
  # spanning row (rank_thead) and each copy is re-labelled by its SUMMARY
  # (the level moves up into the group header).
  facet_spans <- NULL
  has_cell <- any(vapply(plan, function(p) !is.null(p$flevel), logical(1L)))
  if (identical(rank_chr1(facet_layout), "by_level") &&
        length(facet_levels) && has_cell) {
    lead <- Filter(function(p) is.null(p$flevel), plan)
    groups <- lapply(facet_levels, function(lv) {
      Filter(function(p) identical(p$flevel, lv), plan)
    })
    groups <- lapply(groups, function(g) {
      lapply(g, function(p) {
        p$label <- p$sname %||% p$label
        p$sub_label <- NULL
        p
      })
    })
    plan <- c(lead, do.call(c, groups))
    facet_spans <- list(
      lead = length(lead),
      groups = lapply(seq_along(facet_levels), function(j) {
        list(label = facet_levels[[j]], n = length(groups[[j]]))
      })
    )
  }

  # Legend: the first colour-mapped spans summary (colour identity must not
  # ride on colour alone).
  legend_s <- NULL
  for (s in summaries) {
    if (identical(s$type, "spans") && !is.null(present(s$color))) {
      legend_s <- s
      break
    }
  }
  series_lv <- if (!is.null(legend_s)) rank_levels(data[[legend_s$color]])
  pal <- if (!is.null(legend_s)) {
    rank_level_colors(scale_map, legend_s$color, series_lv)
  } else {
    character()
  }

  list(
    rows = rows, plan = plan, layout = if (is.null(facet)) "simple" else "facet",
    mark = "summaries", bar_max = 0, bar_min = 0,
    group_label = rank_group_label(data, group, parent),
    series = if (length(series_lv) >= 2L) series_lv,
    palette = pal, facet_levels = facet_levels,
    denoms = c(all = nrow(data)), group = group, parent = parent,
    color = if (length(series_lv) >= 2L) legend_s$color,
    facet = facet, compare = NULL,
    folded = asm$folded, fold_max = asm$fold_max,
    n_total = if (is.null(parent)) nrow(leaf) else nrow(par_rows),
    note = note, pct_ok = FALSE, func = "identity",
    facet_spans = facet_spans
  )
}

#' One plan entry for one (summary x facet-copy). The entry carries the
#' leaf column names, the display kind, and (after lane_summary_domains)
#' its own scale.
#' @noRd
lane_summary_plan <- function(s, cp, data, scale_map = NULL) {
  sid <- cp$suffix
  label <- if (is.null(cp$level)) s$name else cp$level
  sub <- if (is.null(cp$level)) NULL else s$name
  base <- list(label = label, sub_label = sub, sid = sid, stype = s$type,
               flevel = cp$level, sname = s$name)
  if (identical(s$type, "simple")) {
    if (identical(s$show, "dot")) {
      # A single value as a positioned point on the zero-based lane: the
      # pointrange emitter with no interval. Less ink than a bar.
      c(base, list(kind = "pointrange", key = paste0(sid, "_v"),
                   cols = c(bc = paste0(sid, "_v")),
                   words = list(center = s$name), show_val = TRUE))
    } else {
      kind <- if (identical(s$show, "bar")) "bar" else "num"
      c(base, list(kind = kind, key = paste0(sid, "_v"),
                   fill = if (identical(kind, "bar")) dd_palette(1L),
                   show_val = identical(kind, "bar")))
    }
  } else if (identical(s$type, "dist")) {
    stat <- rank_chr1(s$stat) %||% "median_q1_q3"
    meta <- LANE_STAT_META[[stat]] %||% LANE_STAT_META$median_q1_q3
    if (identical(s$show, "text")) {
      c(base, list(kind = "num", key = paste0(sid, "_bc"),
                   dispkey = "dist_text",
                   lo = paste0(sid, "_bl"), hi = paste0(sid, "_bh"),
                   sub_label = sub %||% meta$label))
    } else if (identical(s$show, "pointrange")) {
      c(base, list(kind = "pointrange", key = paste0(sid, "_bc"),
                   cols = c(bc = paste0(sid, "_bc"), bl = paste0(sid, "_bl"),
                            bh = paste0(sid, "_bh"), n = paste0(sid, "_n")),
                   words = list(center = meta$center, range = meta$range),
                   sub_label = sub %||% meta$label, show_val = TRUE))
    } else {
      wstat <- rank_chr1(s$whiskers) %||% "tukey"
      wmeta <- LANE_STAT_META[[wstat]] %||% LANE_STAT_META$tukey
      c(base, list(kind = "box", key = paste0(sid, "_bc"),
                   cols = c(bc = paste0(sid, "_bc"), bl = paste0(sid, "_bl"),
                            bh = paste0(sid, "_bh"), n = paste0(sid, "_n"),
                            wl = paste0(sid, "_wl"), wh = paste0(sid, "_wh")),
                   words = list(center = meta$center, range = meta$range,
                                whisk = wmeta$range),
                   sub_label = sub %||% meta$label, show_val = TRUE))
    }
  } else if (identical(s$type, "field")) {
    c(base, list(kind = "num", key = paste0(sid, "_t"), raw = TRUE,
                 text = TRUE,
                 sub_label = sub %||% dt_col_label(data[[s$col]], s$col)))
  } else if (identical(s$type, "expr")) {
    # Numeric vs text is decided by the eval; the plan names the numeric
    # key and rank_cells falls back to the text column when it is absent.
    c(base, list(kind = "num", key = paste0(sid, "_v"),
                 alt_text = paste0(sid, "_t")))
  } else if (identical(s$type, "series")) {
    c(base, list(kind = "sparkline", key = paste0(sid, "_last"),
                 pts = paste0(sid, "_pts"), x = rank_chr1(s$x),
                 dom_date = isTRUE(s$.date), show_val = TRUE))
  } else {
    lv <- s$.levels
    c(base, list(kind = "interval", key = paste0(sid, "_start"),
                 segs = paste0(sid, "_segs"), x = rank_chr1(s$x),
                 xend = rank_chr1(s$xend), levels = lv,
                 dom_date = isTRUE(s$.date),
                 fills = if (!is.null(lv)) {
                   unname(rank_level_colors(scale_map, rank_chr1(s$color),
                                            lv)[lv])
                 } else {
                   dd_palette(1L)
                 }))
  }
}

#' @noRd
lane_primary_col <- function(s, sid) {
  switch(s$type,
    simple = paste0(sid, "_v"),
    dist = paste0(sid, "_bc"),
    field = paste0(sid, "_t"),
    expr = paste0(sid, "_v"),
    series = paste0(sid, "_last"),
    spans = paste0(sid, "_start"),
    paste0(sid, "_v")
  )
}

#' @noRd
lane_primary_value <- function(target, s, col) {
  v <- target[[col]]
  if (is.null(v)) {
    # expr rows may have landed as text.
    v <- target[[sub("_v$", "_t", col)]]
  }
  if (is.null(v)) rep(NA_real_, nrow(target)) else v
}

# --- series / spans builders --------------------------------------------------
# Compact re-statements of the single-mark builders in lane-prepare.R,
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
  xs <- lane_as_num(slice[[rank_chr1(s$x)]])
  ys <- as.numeric(slice[[rank_chr1(s$col)]])
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
  xs <- lane_as_num(slice[[rank_chr1(s$x)]])
  xe <- lane_as_num(slice[[rank_chr1(s$xend)]])
  color <- rank_chr1(s$color)
  color <- if (!is.null(color) && color %in% names(slice)) color
  # Full-data level order rides on the summary (s$.levels), so a facet
  # slice missing a level cannot renumber the fills.
  lv <- s$.levels
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
      out[[length(out) + 1L]] <- list(s = xs[[r]],
                                      e = max(xe[[r]], xs[[r]]), f = f)
    }
    out
  })
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
    if (kind %in% c("bar", "box", "pointrange")) {
      vals <- numeric()
      for (i in idx) {
        p <- plan[[i]]
        cols <- if (identical(kind, "bar")) p$key else unname(p$cols)
        for (cn in cols) vals <- c(vals, rows[[cn]])
      }
      vals <- vals[is.finite(vals)]
      dmax <- if (length(vals)) max(c(0, vals)) else 0
      dmin <- if (identical(kind, "bar")) 0 else {
        if (length(vals)) min(c(0, vals)) else 0
      }
      for (i in idx) {
        plan[[i]]$dmax <- dmax
        plan[[i]]$dmin <- dmin
      }
    } else if (identical(kind, "sparkline")) {
      allx <- numeric()
      ally <- numeric()
      for (i in idx) {
        for (p1 in rows[[plan[[i]]$pts]]) {
          allx <- c(allx, p1$x)
          ally <- c(ally, p1$y, p1$lo, p1$hi)
        }
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
    } else if (identical(kind, "interval")) {
      ext <- numeric()
      for (i in idx) {
        for (ss in rows[[plan[[i]]$segs]]) {
          for (sg in ss) ext <- c(ext, sg$s, sg$e)
        }
      }
      ext <- ext[is.finite(ext)]
      xd <- if (length(ext)) range(ext) else c(0, 1)
      if (xd[[2L]] <= xd[[1L]]) xd[[2L]] <- xd[[1L]] + 1
      for (i in idx) plan[[i]]$dom <- xd
    }
  }
  plan
}
