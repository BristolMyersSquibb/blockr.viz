# Lane chart: per-mark preparation --------------------------------------------
#
# rank_prepare() owns the bar mark (the original surface) and dispatches every
# other mark here. Each preparer returns the same prep contract the renderer
# walks (rows / plan / bar scale / chrome data), so rank_cells() and the
# consumers stay mark-agnostic outside their emitter branch.
#
# The rule for what belongs on this surface: one row is one category, and the
# mark is a horizontal glyph on a shared linear domain confined to a cell
# (_blockr.design/open/lane-chart/spec.md).

#' Shared ordering + fold + row assembly: the tail of rank_prepare(), split out
#' so every mark reuses one definition of "rank order, cap, nest".
#' @noRd
rank_assemble_rows <- function(leaf, par_rows, parent, sort_key, sort_dir,
                               top_n) {
  ord <- function(df) {
    if (is.null(df) || !nrow(df)) return(df)
    v <- if (identical(sort_key, ".label")) df$.label else df[[sort_key]]
    if (is.null(v)) v <- df$.v
    if (is.character(v)) {
      df[order(v, decreasing = identical(sort_dir, "desc")), , drop = FALSE]
    } else {
      df[order(v, decreasing = identical(sort_dir, "desc"), na.last = TRUE), ,
         drop = FALSE]
    }
  }

  folded <- 0L
  fold_max <- NA_real_
  if (is.null(parent)) {
    rows <- ord(leaf)
    if (!is.null(top_n) && is.finite(top_n) && top_n > 0 && nrow(rows) > top_n) {
      rest <- rows[seq.int(top_n + 1L, nrow(rows)), , drop = FALSE]
      folded <- nrow(rest)
      fold_max <- suppressWarnings(max(rest$.v, na.rm = TRUE))
      rows <- rows[seq_len(top_n), , drop = FALSE]
    }
    rows$.level <- 0L
    rows$.is_parent <- FALSE
  } else {
    par_rows <- ord(par_rows)
    if (!is.null(top_n) && is.finite(top_n) && top_n > 0 &&
          nrow(par_rows) > top_n) {
      rest <- par_rows[seq.int(top_n + 1L, nrow(par_rows)), , drop = FALSE]
      folded <- nrow(rest)
      fold_max <- suppressWarnings(max(rest$.v, na.rm = TRUE))
      par_rows <- par_rows[seq_len(top_n), , drop = FALSE]
    }
    par_rows$.level <- 0L
    par_rows$.is_parent <- TRUE
    leaf$.level <- 1L
    leaf$.is_parent <- FALSE
    # Every renderer-internal column is dot-prefixed; the original data
    # columns (the group / parent keys) are not, and rbind() must not demand
    # them from both frames.
    keep <- grep("^\\.", names(leaf), value = TRUE)
    pieces <- list()
    for (i in seq_len(nrow(par_rows))) {
      p <- par_rows[i, , drop = FALSE]
      kids <- ord(leaf[!is.na(leaf$.parent) & leaf$.parent == p$.label, ,
                       drop = FALSE])
      pieces[[length(pieces) + 1L]] <-
        p[, intersect(keep, names(p)), drop = FALSE]
      if (nrow(kids)) {
        pieces[[length(pieces) + 1L]] <- kids[, intersect(keep, names(kids)),
                                              drop = FALSE]
      }
    }
    rows <- do.call(rbind, pieces)
  }
  list(rows = rows, folded = folded, fold_max = fold_max)
}

#' The distribution marks: box and point range.
#'
#' Statistics per (group [x parent] [x facet]) cell, computed in R
#' (lane-stats.R). A box is a point range with one more nested interval:
#' `summary` picks the body (center + inner interval), `whiskers` the box's
#' outer rule -- the sibling canvas spec's vocabulary, so the two gears read
#' as one system. Defaults: the textbook Tukey box (`median_q1_q3` body,
#' `tukey` whiskers) and `mean_se` for the point range.
#' @noRd
lane_prepare_dist <- function(mark, data, group, value, summary, whiskers,
                              parent = NULL, color = NULL, facet = NULL,
                              compare = NULL, fields = NULL,
                              sort_by = "value", sort_dir = "desc",
                              top_n = NULL, scale_map = NULL) {
  bad <- function(msg) list(err = msg)
  is_box <- identical(mark, "box")
  lab_word <- if (is_box) "box" else "point range"

  value <- rank_chr1(value)
  if (is.null(value) || identical(value, ".count")) {
    return(bad(paste0("Pick a Value column to summarize as a ", lab_word)))
  }
  if (!value %in% names(data)) {
    return(bad(paste0("Mapped column not in data: Value = \"", value,
                      "\". Re-pick it in the gear.")))
  }
  if (!is.numeric(data[[value]])) {
    return(bad(paste0("Value column \"", value, "\" is not numeric; a ",
                      lab_word, " summarizes numbers.")))
  }

  present <- function(col) {
    col <- rank_chr1(col)
    if (is.null(col) || !col %in% names(data)) NULL else col
  }
  parent <- present(parent)
  facet <- present(facet)

  # One glyph per row: the colour split and the comparator have no meaning
  # here (a zero-centred difference against a comparator is a bar concept).
  # Reported, never silent.
  note <- NULL
  if (!is.null(rank_chr1(color))) {
    note <- paste0("A ", lab_word, " keeps one glyph per row; ignoring the ",
                   "colour split by \"", rank_chr1(color), "\".")
  }
  if (!is.null(rank_chr1(fields))) {
    note <- paste(c(note, paste0(
      "Extra columns need the as-is measure; a summarized row has no single ",
      "underlying row to read them from."
    )), collapse = " ")
  }

  summary <- rank_chr1(summary)
  if (is.null(summary) || !summary %in% LANE_STATS) {
    summary <- if (is_box) "median_q1_q3" else "mean_se"
  }
  whiskers <- rank_chr1(whiskers) %||% "tukey"
  if (!whiskers %in% LANE_WHISKERS) whiskers <- "tukey"

  stats <- if (is_box) {
    list(.b = summary, .w = whiskers)
  } else {
    list(.b = summary)
  }
  keys <- c(parent, group)

  leaf <- lane_stat_agg(data, keys, value, stats)
  if (!nrow(leaf)) return(bad("No rows to display"))
  leaf$.label <- as.character(leaf[[group]])
  leaf$.parent <- if (is.null(parent)) NA_character_ else
    as.character(leaf[[parent]])
  leaf$.v <- leaf$.bc

  meta <- LANE_STAT_META[[summary]]
  wmeta <- LANE_STAT_META[[whiskers]]
  stat_cols <- function(prefix = ".") {
    out <- c(bc = paste0(prefix, "bc"), bl = paste0(prefix, "bl"),
             bh = paste0(prefix, "bh"), n = paste0(prefix, "n"))
    if (is_box) {
      out <- c(out, wl = paste0(prefix, "wl"), wh = paste0(prefix, "wh"))
    }
    out
  }
  # The tooltip's vocabulary rides on the plan so rank_cells() needs no stat
  # knowledge: what the center and the interval(s) are CALLED.
  words <- list(center = meta$center, range = meta$range,
                whisk = if (is_box) wmeta$range)
  sub_line <- paste0(
    meta$label, if (is_box) paste0(", whiskers ", wmeta$label), ": ",
    dt_col_label(data[[value]], value) %||% value
  )

  plan <- list()
  facet_levels <- character()
  denoms <- c(all = nrow(data))
  if (is.null(facet)) {
    plan <- list(list(kind = mark, label = value, key = ".bc",
                      cols = stat_cols(), words = words,
                      sub_label = sub_line, show_val = TRUE))
  } else {
    facet_levels <- rank_levels(data[[facet]])
    if (length(facet_levels) < 2L) {
      return(bad(paste0(
        "Facet column \"", facet, "\" has fewer than two levels; ",
        "nothing to compare across columns."
      )))
    }
    for (fi in seq_along(facet_levels)) {
      fv <- facet_levels[[fi]]
      sub <- data[as.character(data[[facet]]) == fv, , drop = FALSE]
      fs <- lane_stat_agg(sub, keys, value, stats)
      for (nm in setdiff(names(fs), keys)) {
        leaf[[paste0(".f", fi, "_", sub("^\\.", "", nm))]] <-
          rank_match_col(leaf, fs, keys, nm)
      }
      plan <- c(plan, list(list(
        kind = mark, label = fv, key = paste0(".f", fi, "_bc"),
        cols = stat_cols(paste0(".f", fi, "_")),
        words = words, sub_label = paste0("N = ", nrow(sub)),
        show_val = TRUE
      )))
      denoms[[fv]] <- nrow(sub)
    }
    # The un-faceted stats still order the rows (`.v` = pooled center).
  }

  # --- parent rows ----------------------------------------------------------
  par_rows <- NULL
  if (!is.null(parent)) {
    par_rows <- lane_stat_agg(data, parent, value, stats)
    par_rows$.label <- as.character(par_rows[[parent]])
    par_rows$.parent <- NA_character_
    par_rows$.v <- par_rows$.bc
    if (!is.null(facet)) {
      for (fi in seq_along(facet_levels)) {
        fv <- facet_levels[[fi]]
        sub <- data[as.character(data[[facet]]) == fv, , drop = FALSE]
        fs <- lane_stat_agg(sub, parent, value, stats)
        for (nm in setdiff(names(fs), parent)) {
          par_rows[[paste0(".f", fi, "_", sub("^\\.", "", nm))]] <-
            rank_match_col(par_rows, fs, parent, nm)
        }
      }
    }
  }

  sort_key <- rank_sort_key(sort_by, plan)
  if (identical(sort_key, ".bc")) sort_key <- ".v"
  asm <- rank_assemble_rows(leaf, par_rows, parent, sort_key, sort_dir, top_n)

  # --- shared domain --------------------------------------------------------
  # The trap called out in the spec: the domain must run to the widest extent
  # the mark DRAWS -- the outer interval's hi, not the center -- or upper
  # whiskers clip at the cell edge. Zero stays in the domain for all-positive
  # data (bar parity); a negative lo extends it below zero instead of
  # clamping.
  hi_cols <- grep(if (is_box) "(\\.|_)(wh|bh)$" else "(\\.|_)bh$",
                  names(asm$rows), value = TRUE)
  lo_cols <- grep(if (is_box) "(\\.|_)(wl|bl)$" else "(\\.|_)bl$",
                  names(asm$rows), value = TRUE)
  ext <- unlist(c(asm$rows[hi_cols], asm$rows[lo_cols],
                  asm$rows[grep("(\\.|_)bc$", names(asm$rows), value = TRUE)]))
  ext <- ext[is.finite(ext)]
  bar_max <- if (length(ext)) max(c(0, ext)) else 0
  bar_min <- if (length(ext)) min(c(0, ext)) else 0

  list(
    rows = asm$rows, plan = plan, layout = if (is.null(facet)) "simple" else "facet",
    mark = mark, bar_max = bar_max, bar_min = bar_min,
    group_label = rank_group_label(data, group, parent),
    series = NULL, palette = character(), facet_levels = facet_levels,
    denoms = denoms, group = group, parent = parent, color = NULL,
    facet = facet, compare = NULL, folded = asm$folded,
    fold_max = asm$fold_max,
    n_total = if (is.null(parent)) nrow(leaf) else nrow(par_rows),
    note = note, pct_ok = FALSE, func = "identity",
    summary = summary, whiskers = if (is_box) whiskers
  )
}

#' The interval mark: the adverse-event swimlane.
#'
#' One row per `group` level, each carrying every (x, xend) span from its
#' underlying rows as a segment on ONE shared domain (the observed range of
#' x/xend). `color` colours the segments; `fields` add row columns (read from
#' the group's first underlying row, so they should be group-level facts).
#' @noRd
lane_prepare_interval <- function(data, group, x, xend, color = NULL,
                                  parent = NULL, fields = NULL,
                                  sort_by = "value", sort_dir = "desc",
                                  top_n = NULL, scale_map = NULL) {
  bad <- function(msg) list(err = msg)

  x <- rank_chr1(x)
  xend <- rank_chr1(xend)
  if (is.null(x) || is.null(xend)) {
    return(bad("Pick Start and End columns for the intervals"))
  }
  miss <- setdiff(c(x, xend), names(data))
  if (length(miss)) {
    return(bad(paste0(
      "Mapped column not in data: ",
      paste0("\"", miss, "\"", collapse = ", "), ". Re-pick it in the gear."
    )))
  }
  as_num <- function(v) {
    if (inherits(v, "Date") || inherits(v, "POSIXct")) as.numeric(v)
    else if (is.numeric(v)) as.numeric(v)
    else NULL
  }
  xs <- as_num(data[[x]])
  xe <- as_num(data[[xend]])
  if (is.null(xs) || is.null(xe)) {
    return(bad("Start and End must be numeric or date columns"))
  }

  present <- function(col) {
    col <- rank_chr1(col)
    if (is.null(col) || !col %in% names(data)) NULL else col
  }
  color <- present(color)
  note <- NULL
  if (!is.null(rank_chr1(parent))) {
    note <- "Intervals keep a flat row list; ignoring the nesting."
  }

  series <- if (is.null(color)) character() else rank_levels(data[[color]])
  pal <- if (is.null(color)) {
    character()
  } else {
    rank_level_colors(scale_map, color, series)
  }
  fills <- if (length(series)) unname(pal[series]) else dd_palette(1L)

  ok <- is.finite(xs) & is.finite(xe)
  if (!any(ok)) return(bad("No finite intervals to draw"))
  dmin <- min(c(xs[ok], xe[ok]))
  dmax <- max(c(xs[ok], xe[ok]))
  if (dmax <= dmin) dmax <- dmin + 1

  labels <- as.character(data[[group]])
  lv <- rank_levels(data[[group]])
  segs <- lapply(lv, function(l) {
    idx <- which(labels == l & ok)
    idx <- idx[order(xs[idx])]
    lapply(idx, function(i) {
      list(
        s = xs[[i]], e = max(xe[[i]], xs[[i]]),
        f = if (length(series)) {
          match(as.character(data[[color]][[i]]), series)
        } else {
          1L
        }
      )
    })
  })
  first_start <- vapply(segs, function(ss) {
    if (length(ss)) ss[[1L]]$s else NA_real_
  }, numeric(1L))

  leaf <- data.frame(.label = lv, .parent = NA_character_,
                     .v = first_start, .n = lengths(segs),
                     stringsAsFactors = FALSE)
  leaf[[group]] <- lv
  leaf$.segs <- I(segs)

  # Extra row columns: the group's first underlying row (subject-level facts).
  fields <- as.character(fields %||% character())
  fields <- intersect(setdiff(fields[nzchar(fields)], group), names(data))
  plan <- list(list(
    kind = "interval", label = paste0(x, " → ", xend), key = ".v",
    x = x, xend = xend, sub_label = dt_col_label(data[[x]], x),
    show_val = FALSE
  ))
  plan <- c(plan, list(list(kind = "num", label = "Events", key = ".n",
                            sub_label = "intervals")))
  if (length(fields)) {
    fr <- data[!duplicated(data[[group]]), , drop = FALSE]
    for (fld in fields) {
      leaf[[paste0(".x_", fld)]] <- rank_match_field(leaf, fr, group, fld)
      plan <- c(plan, list(list(
        kind = "num", label = fld, key = paste0(".x_", fld),
        raw = TRUE, text = !is.numeric(data[[fld]]),
        sub_label = dt_col_label(data[[fld]], fld)
      )))
    }
  }

  sort_key <- rank_sort_key(sort_by, plan)
  asm <- rank_assemble_rows(leaf, NULL, NULL, sort_key, sort_dir, top_n)

  list(
    rows = asm$rows, plan = plan, layout = "simple", mark = "interval",
    bar_max = dmax, bar_min = dmin, dom = c(dmin, dmax),
    dom_date = inherits(data[[x]], "Date"),
    group_label = rank_group_label(data, group, NULL),
    series = if (length(series) >= 2L) series else NULL,
    palette = pal, fills = fills, facet_levels = character(),
    denoms = c(all = nrow(data)), group = group, parent = NULL,
    color = if (length(series) >= 2L) color else NULL, facet = NULL,
    compare = NULL, folded = asm$folded, fold_max = asm$fold_max,
    n_total = length(lv), note = note, pct_ok = FALSE, func = "identity"
  )
}

#' The sparkline-with-band mark.
#'
#' One row is one series: `value` over the within-row order column `x`, with
#' an optional `lo`/`hi` band. One shared domain per axis across all rows
#' (x = the observed order range, y = the observed value/band range), so the
#' trajectories are comparable -- the same shared-scale rule as every other
#' mark on this surface.
#' @noRd
lane_prepare_sparkline <- function(data, group, x, value, lo = NULL, hi = NULL,
                                   func = NULL, parent = NULL, fields = NULL,
                                   sort_by = "value", sort_dir = "desc",
                                   top_n = NULL, scale_map = NULL) {
  bad <- function(msg) list(err = msg)

  x <- rank_chr1(x)
  value <- rank_chr1(value)
  if (is.null(x)) return(bad("Pick an Order column (the within-row x)"))
  if (is.null(value) || identical(value, ".count")) {
    return(bad("Pick a Value column to draw"))
  }
  miss <- setdiff(c(x, value), names(data))
  if (length(miss)) {
    return(bad(paste0(
      "Mapped column not in data: ",
      paste0("\"", miss, "\"", collapse = ", "), ". Re-pick it in the gear."
    )))
  }
  as_num <- function(v) {
    if (inherits(v, "Date") || inherits(v, "POSIXct")) as.numeric(v)
    else if (is.numeric(v)) as.numeric(v)
    else NULL
  }
  xs <- as_num(data[[x]])
  if (is.null(xs)) return(bad("The Order column must be numeric or a date"))
  if (!is.numeric(data[[value]])) {
    return(bad(paste0("Value column \"", value, "\" is not numeric")))
  }

  present <- function(col) {
    col <- rank_chr1(col)
    if (is.null(col) || !col %in% names(data)) NULL else col
  }
  lo <- present(lo)
  hi <- present(hi)
  band <- !is.null(lo) && !is.null(hi) &&
    is.numeric(data[[lo]]) && is.numeric(data[[hi]])
  note <- NULL
  if (!is.null(rank_chr1(parent))) {
    note <- "Sparklines keep a flat row list; ignoring the nesting."
  }

  ys <- as.numeric(data[[value]])
  ok <- is.finite(xs) & is.finite(ys)
  if (!any(ok)) return(bad("No finite points to draw"))

  labels <- as.character(data[[group]])
  lv <- rank_levels(data[[group]])
  pts <- lapply(lv, function(l) {
    idx <- which(labels == l & ok)
    idx <- idx[order(xs[idx])]
    list(
      x = xs[idx], y = ys[idx],
      lo = if (band) as.numeric(data[[lo]])[idx],
      hi = if (band) as.numeric(data[[hi]])[idx]
    )
  })

  ally <- c(ys[ok],
            if (band) as.numeric(data[[lo]])[ok],
            if (band) as.numeric(data[[hi]])[ok])
  ally <- ally[is.finite(ally)]
  ymin <- min(ally)
  ymax <- max(ally)
  if (ymax <= ymin) ymax <- ymin + 1
  xmin <- min(xs[ok])
  xmax <- max(xs[ok])
  if (xmax <= xmin) xmax <- xmin + 1

  last_y <- vapply(pts, function(p) {
    if (length(p$y)) p$y[[length(p$y)]] else NA_real_
  }, numeric(1L))

  # An optional companion bar: `func` reduced over the row's own values
  # (mean / median / sum / min / max), drawn as a bar beside the trajectory
  # and RANKING the rows -- "highest mean, plus the trajectory". The bar
  # shares the sparkline's y units and its 0..ymax scale, so the two marks
  # are honestly comparable. The counting funcs (and identity, the
  # constructor's bar-era default `count` included) mean "no bar": the rows
  # rank by last value, the original behaviour.
  func <- rank_chr1(func)
  rank_fn <- if (!is.null(func) &&
                   func %in% c("mean", "median", "sum", "min", "max")) func
  agg <- if (!is.null(rank_fn)) {
    vapply(pts, function(p) {
      if (!length(p$y)) return(NA_real_)
      switch(rank_fn,
             mean = mean(p$y), median = stats::median(p$y),
             sum = sum(p$y), min = min(p$y), max = max(p$y))
    }, numeric(1L))
  }

  leaf <- data.frame(.label = lv, .parent = NA_character_,
                     .v = if (is.null(rank_fn)) last_y else agg,
                     .last = last_y,
                     .n = vapply(pts, function(p) length(p$y), integer(1L)),
                     stringsAsFactors = FALSE)
  leaf[[group]] <- lv
  leaf$.pts <- I(pts)

  fields <- as.character(fields %||% character())
  fields <- intersect(setdiff(fields[nzchar(fields)], group), names(data))
  plan <- list()
  if (!is.null(rank_fn)) {
    plan <- c(plan, list(list(
      kind = "bar", label = rank_measure_label(rank_fn, value), key = ".v",
      fill = dd_palette(1L), sub_label = "ranks the rows", show_val = TRUE
    )))
  }
  plan <- c(plan, list(list(
    kind = "sparkline", label = value, key = ".last", x = x,
    sub_label = paste0(
      dt_col_label(data[[value]], value) %||% value, " over ", x,
      if (band) paste0(", band ", lo, "–", hi)
    ),
    show_val = TRUE
  )))
  if (length(fields)) {
    fr <- data[!duplicated(data[[group]]), , drop = FALSE]
    for (fld in fields) {
      leaf[[paste0(".x_", fld)]] <- rank_match_field(leaf, fr, group, fld)
      plan <- c(plan, list(list(
        kind = "num", label = fld, key = paste0(".x_", fld),
        raw = TRUE, text = !is.numeric(data[[fld]]),
        sub_label = dt_col_label(data[[fld]], fld)
      )))
    }
  }

  sort_key <- rank_sort_key(sort_by, plan)
  asm <- rank_assemble_rows(leaf, NULL, NULL, sort_key, sort_dir, top_n)

  list(
    rows = asm$rows, plan = plan, layout = "simple", mark = "sparkline",
    bar_max = ymax, bar_min = ymin, dom = c(xmin, xmax), ydom = c(ymin, ymax),
    dom_date = inherits(data[[x]], "Date"), band = band,
    group_label = rank_group_label(data, group, NULL),
    series = NULL, palette = character(), facet_levels = character(),
    denoms = c(all = nrow(data)), group = group, parent = NULL, color = NULL,
    facet = NULL, compare = NULL, folded = asm$folded,
    fold_max = asm$fold_max, n_total = length(lv), note = note,
    pct_ok = FALSE, func = "identity"
  )
}

#' Match an arbitrary stat column from `src` into `target` by `keys`
#' (rank_match generalized beyond `.v`; absent keys stay NA -- an absent
#' facet cell draws nothing).
#' @noRd
rank_match_col <- function(target, src, keys, col) {
  tk <- do.call(paste, c(lapply(keys, function(k) as.character(target[[k]])),
                         list(sep = "\r")))
  sk <- do.call(paste, c(lapply(keys, function(k) as.character(src[[k]])),
                         list(sep = "\r")))
  as.numeric(src[[col]][match(tk, sk)])
}
