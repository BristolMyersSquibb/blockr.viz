# What the chart computes in R for the browser beside the rows: the band,
# the smoother and the histogram bins. Pure functions of the state (public
# argument names, as in CHART_SPEC) and the data; the server caches them.
#
#   chart_band()     the band's series, its reference lines, or why it is empty
#   chart_smoother() the trend lines of a scatter
#   chart_hist()     a histogram's bin column and the edges a click filters on
#
# The `*_args()` helpers give the arguments each computation reads, so the
# server can key its cache on exactly those columns and settings.

# A configured column counts only when the data has it.
chart_col_in <- function(data, col) {
  if (length(col) && nzchar(col[[1L]]) && col[[1L]] %in% names(data)) {
    col[[1L]]
  }
}

# The columns named in `args`, the part of the data a computation reads.
chart_args_cols <- function(args, data) {
  intersect(as.character(unlist(args[grepl("_col$|_by$", names(args))])),
            names(data))
}

# --- band ------------------------------------------------------------------

chart_band_args <- function(state, data) {
  if (!identical(state$type, "band") || !is.data.frame(data)) {
    return(NULL)
  }
  list(
    x_col = state$x, y_col = state$y, color_by = state$color,
    series_by = state$series, facet_by = chart_col_in(data, state$facet),
    summary = state$summary %||% "median_q1_q3",
    whiskers = state$whiskers %||% "p10_p90",
    window = state$band_window %||% "adaptive",
    window_size = state$band_size %||% 45,
    min_n = state$band_min_n %||% 12,
    id_col = chart_col_in(data, state$id)
  )
}

# list(series, refs, note): the band per facet and series level, the
# reference lines reduced from `ref_max` / `ref_min`, and a sentence on why
# the band is empty when it is. NULL on any other type.
chart_band <- function(state, data, args = chart_band_args(state, data)) {
  if (is.null(args)) {
    return(NULL)
  }
  series <- tryCatch(
    do.call(compute_band_series, c(list(data), args)),
    error = function(e) {
      warning("chart: band computation failed: ", conditionMessage(e),
              call. = FALSE)
      NULL
    }
  )
  refs <- list(hi = band_reference(data, chart_col_in(data, state$ref_max)),
               lo = band_reference(data, chart_col_in(data, state$ref_min)))
  refs <- refs[!vapply(refs, is.null, logical(1L))]
  list(
    series = series,
    refs = if (length(refs)) refs,
    note = if (is.null(series)) {
      band_empty_reason(data, state$x, state$y, min_n = args$min_n,
                        id_col = args$id_col)
    }
  )
}

# The statistic of a box, a point range or a band over a numeric vector:
# list(center, lo, hi), NULL when empty. The R twin of summarizeStat() in
# inst/js/chart/model-aggregated.js: type 7 quantiles, the sample sd. "tukey"
# is the 1.5 IQR fence clipped to the data, as the boxplot draws it.
summarize_stat <- function(vals, stat = "median_q1_q3") {
  vals <- sort(vals[is.finite(vals)])
  n <- length(vals)
  if (n == 0L) return(NULL)
  q <- function(p) unname(stats::quantile(vals, p, type = 7L, names = FALSE))
  m  <- mean(vals)
  sd <- if (n > 1L) stats::sd(vals) else 0
  se <- sd / sqrt(n)
  switch(
    stat %||% "median_q1_q3",
    mean_sd  = list(center = m, lo = m - sd,     hi = m + sd),
    mean_2sd = list(center = m, lo = m - 2 * sd, hi = m + 2 * sd),
    mean_se  = list(center = m, lo = m - se,     hi = m + se),
    mean_ci95 = {
      h <- if (n > 1L) stats::qt(0.975, n - 1L) * se else 0
      list(center = m, lo = m - h, hi = m + h)
    },
    p5_p95   = list(center = q(.5), lo = q(.05), hi = q(.95)),
    p10_p90  = list(center = q(.5), lo = q(.10), hi = q(.90)),
    min_max  = list(center = q(.5), lo = vals[1L], hi = vals[n]),
    tukey    = {
      q1 <- q(.25)
      q3 <- q(.75)
      iqr <- q3 - q1
      list(center = q(.5),
           lo = max(vals[1L], q1 - 1.5 * iqr),
           hi = min(vals[n],  q3 + 1.5 * iqr))
    },
    list(center = q(.5), lo = q(.25), hi = q(.75))
  )
}

# Half-width of the window around one grid point: `window_size` in x units
# when "fixed"; when "adaptive", wide enough to hold `window_size` distinct
# ids (rows without an id), at most `max_half`.
band_half_width <- function(dist, id_code, n_ids, window, window_size,
                            max_half) {
  if (identical(window, "fixed")) return(window_size)
  ord <- order(dist)
  if (is.null(id_code)) {
    if (length(ord) >= window_size) {
      return(min(dist[ord[window_size]], max_half))
    }
    return(max_half)
  }
  seen <- logical(n_ids)
  cnt <- 0L
  for (j in ord) {
    if (!seen[id_code[j]]) {
      seen[id_code[j]] <- TRUE
      cnt <- cnt + 1L
    }
    if (cnt >= window_size) return(min(dist[j], max_half))
  }
  max_half
}

# The band: at each of `n_grid` points across the x range, the observations
# in a window around it, summarized like a box (`summary` inside, `whiskers`
# outside, "none" for no outer interval). Keyed by facet level, then by
# series (else colour) level, `__all__` where there is none. Each entry
# holds x, center, lo, hi, olo, ohi, n (distinct ids in the window) and hw
# (the half-width); a point with fewer than `min_n` ids is null, so the band
# breaks there. Points outside the local Tukey fence come as out_x, out_y,
# out_id. NULL when nothing can be drawn.
compute_band_series <- function(data, x_col, y_col, color_by, series_by,
                                facet_by = NULL,
                                summary = "median_q1_q3",
                                whiskers = "p10_p90",
                                window = "adaptive",
                                window_size = 45,
                                min_n = 12,
                                id_col = NULL,
                                n_grid = 100L) {
  if (is.null(data) || nrow(data) == 0L) return(NULL)
  if (is.null(x_col) || is.null(y_col)) return(NULL)
  if (!all(c(x_col, y_col) %in% names(data))) return(NULL)
  if (!is.numeric(data[[x_col]]) || !is.numeric(data[[y_col]])) return(NULL)
  if (!is.null(id_col) && !(id_col %in% names(data))) id_col <- NULL

  split_col <- series_by %||% color_by
  if (!is.null(facet_by) && !(facet_by %in% names(data))) facet_by <- NULL

  # Only the columns the band reads, so the row filter and the splits below
  # copy two to five columns rather than the whole frame.
  keep <- is.finite(data[[x_col]]) & is.finite(data[[y_col]])
  cols <- intersect(c(x_col, y_col, split_col, facet_by, id_col), names(data))
  data <- data[keep, cols, drop = FALSE]
  if (nrow(data) == 0L) return(NULL)

  # One grid across the WHOLE x range, not per group: the panels share an x
  # axis, and a per-group grid would put each arm's points at different days
  # so a tooltip could not report them together.
  rng <- range(data[[x_col]])
  if (!is.finite(rng[1L]) || rng[1L] == rng[2L]) return(NULL)
  grid <- seq(rng[1L], rng[2L], length.out = max(10L, as.integer(n_grid)))
  # An adaptive window may not find its quota near the ends of a sparse
  # series; cap it at a quarter of the span so it degrades to "wide" rather
  # than "the whole study".
  max_half <- diff(rng) / 4


  # Keyed facet level -> series level. Both tiers fall back to `__all__`, so
  # an unfaceted, uncoloured chart is simply res$__all__$__all__ and the
  # renderer reads one shape whatever is mapped.
  by_series <- function(d) {
    grp <- if (!is.null(split_col) && split_col %in% names(d)) {
      split(d, as.character(d[[split_col]]))
    } else {
      list(`__all__` = d)
    }
    out <- lapply(grp, band_group, x_col = x_col, y_col = y_col,
                  id_col = id_col, grid = grid, summary = summary,
                  whiskers = whiskers, window = window,
                  window_size = window_size, min_n = min_n,
                  max_half = max_half)
    out <- out[!vapply(out, is.null, logical(1L))]
    if (length(out) == 0L) NULL else out
  }
  panels <- if (is.null(facet_by)) {
    list(`__all__` = data)
  } else {
    split(data, as.character(data[[facet_by]]))
  }
  res <- lapply(panels, by_series)
  res <- res[!vapply(res, is.null, logical(1L))]
  if (length(res) == 0L) NULL else res
}

# The band of one group over the shared grid (see compute_band_series()).
band_group <- function(d, x_col, y_col, id_col, grid, summary, whiskers,
                     window, window_size, min_n, max_half) {
  outer_on <- !is.null(whiskers) && !identical(whiskers, "none")
  xv <- d[[x_col]]
  yv <- d[[y_col]]
  ids <- if (is.null(id_col)) NULL else as.character(d[[id_col]])
  lev <- if (is.null(ids)) NULL else unique(ids)
  id_code <- if (is.null(ids)) NULL else match(ids, lev)
  n_ids <- length(lev)
  ng <- length(grid)
  out <- list(
    center = rep(NA_real_, ng), lo = rep(NA_real_, ng), hi = rep(NA_real_, ng),
    olo = rep(NA_real_, ng), ohi = rep(NA_real_, ng),
    n = integer(ng), hw = rep(NA_real_, ng)
  )
  # Tukey band kept alongside the drawn one: the outlier test is always the
  # Tukey fence, whatever `whiskers` shows.
  tlo <- rep(NA_real_, ng)
  thi <- rep(NA_real_, ng)

  for (k in seq_len(ng)) {
    dist <- abs(xv - grid[k])
    h <- band_half_width(dist, id_code, n_ids, window, window_size, max_half)
    sel <- dist <= h
    np <- if (is.null(ids)) sum(sel) else length(unique.default(id_code[sel]))
    out$n[k] <- np
    out$hw[k] <- h
    if (np < min_n) next
    vals <- yv[sel]
    inner <- summarize_stat(vals, summary)
    tuk <- summarize_stat(vals, "tukey")
    out$center[k] <- inner$center
    out$lo[k] <- inner$lo
    out$hi[k] <- inner$hi
    tlo[k] <- tuk$lo
    thi[k] <- tuk$hi
    if (outer_on) {
      o <- summarize_stat(vals, whiskers)
      out$olo[k] <- o$lo
      out$ohi[k] <- o$hi
    }
  }

  if (all(is.na(out$center))) return(NULL)

  # Outliers: interpolate the local Tukey fence onto each observation.
  ok <- !is.na(tlo)
  if (sum(ok) >= 2L) {
    fl <- stats::approx(grid[ok], tlo[ok], xout = xv, rule = 2L)$y
    fh <- stats::approx(grid[ok], thi[ok], xout = xv, rule = 2L)$y
    is_out <- which(yv < fl | yv > fh)
  } else {
    is_out <- integer()
  }

  # NA -> NULL in the JSON, so the renderer sees a hole it can break the
  # ribbon on rather than a zero.
  num <- function(v) lapply(unname(v), function(z) if (is.na(z)) NULL else z)
  res <- list(
    x = as.list(unname(grid)),
    center = num(out$center), lo = num(out$lo), hi = num(out$hi),
    n = as.list(unname(out$n)), hw = num(round(out$hw, 1L))
  )
  if (outer_on) {
    res$olo <- num(out$olo)
    res$ohi <- num(out$ohi)
  }
  if (length(is_out)) {
    res$out_x <- as.list(unname(xv[is_out]))
    res$out_y <- as.list(unname(yv[is_out]))
    if (!is.null(ids)) res$out_id <- as.list(unname(ids[is_out]))
  }
  res
}

# A reference limit is a column per record (ANRHI), reduced to its median
# for one line, with its range: list(col, value, lo, hi, n_distinct).
band_reference <- function(data, col) {
  if (is.null(col) || !nzchar(col)) return(NULL)
  if (is.null(data) || !(col %in% names(data))) return(NULL)
  v <- data[[col]]
  if (!is.numeric(v)) return(NULL)
  v <- v[is.finite(v)]
  if (!length(v)) return(NULL)
  list(col = col, value = stats::median(v), lo = min(v), hi = max(v),
       n_distinct = length(unique(v)))
}

# Why compute_band_series() drew nothing: the first guard that fires, as a
# sentence. NULL when x or y is unset.
band_empty_reason <- function(data, x_col, y_col, min_n = 12, id_col = NULL) {
  if (is.null(x_col) || is.null(y_col) || !nzchar(x_col) || !nzchar(y_col)) {
    return(NULL)
  }
  if (is.null(data) || nrow(data) == 0L) {
    return("This block received no rows.")
  }
  # A missing column has no label to show, so it is named.
  for (col in c(x_col, y_col)) {
    if (!col %in% names(data)) {
      return(sprintf("Column \"%s\" is not in the data.", col))
    }
  }
  # The others name the column as the chart does: its label, else its name.
  x_lab <- title_column_label(data, x_col)
  y_lab <- title_column_label(data, y_col)
  if (!is.numeric(data[[x_col]])) {
    return(sprintf(
      "Timeline \"%s\" is not numeric \u2014 a band slides a window along a continuous axis, so it needs a numeric column (e.g. ADY).",
      x_lab))
  }
  if (!is.numeric(data[[y_col]])) {
    return(sprintf("Value \"%s\" is not numeric.", y_lab))
  }
  ok <- is.finite(data[[x_col]]) & is.finite(data[[y_col]])
  if (!any(ok)) {
    return(sprintf("Every row is missing \"%s\" or \"%s\".", x_lab, y_lab))
  }
  d <- data[ok, , drop = FALSE]
  if (length(unique(d[[x_col]])) < 2L) {
    return(sprintf(
      "\"%s\" has only one distinct value, so there is no range to slide a window along.",
      x_lab))
  }
  by_id <- !is.null(id_col) && id_col %in% names(d)
  n <- if (by_id) length(unique(d[[id_col]])) else nrow(d)
  sprintf(
    "No window reached the cut-off of %s: the whole series holds only %d. Lower \"Cut below n\", widen the window, or relax the upstream filter.",
    if (by_id) sprintf("%d subjects", min_n) else sprintf("%d rows", min_n),
    n)
}

# --- smoother --------------------------------------------------------------

chart_smoother_args <- function(state, data) {
  sm <- state$smoother
  if (is.null(sm) || identical(sm, "none") ||
        !identical(state$type, "scatter") || !is.data.frame(data)) {
    return(NULL)
  }
  list(
    smoother = sm, x_col = state$x, y_col = state$y, color_by = state$color,
    series_by = state$series, facet_by = chart_col_in(data, state$facet),
    x_log = identical(state$x_transform, "log10"),
    y_log = identical(state$y_transform, "log10")
  )
}

chart_smoother <- function(state, data,
                           args = chart_smoother_args(state, data)) {
  if (is.null(args)) {
    return(NULL)
  }
  tryCatch(
    do.call(compute_smoother_series, c(list(data), args)),
    error = function(e) {
      warning("chart: smoother computation failed: ", conditionMessage(e),
              call. = FALSE)
      NULL
    }
  )
}

# Trend lines, "lm" or "loess", one per series (else colour) level and per
# facet panel: a fit belongs to its panel. Each is list(x, y) over 100 points
# across the group's x range. On a log axis the fit runs on log10 and comes
# back in raw units; rows at or below zero are left out. A loess over more
# than `loess_max` points runs on that many bins of equal count.
compute_smoother_series <- function(data, smoother, x_col, y_col,
                                     color_by, series_by, facet_by = NULL,
                                     loess_max = 5000L, x_log = FALSE,
                                     y_log = FALSE) {
  if (is.null(smoother) || identical(smoother, "none")) return(NULL)
  if (is.null(data) || nrow(data) == 0) return(NULL)
  if (is.null(x_col) || is.null(y_col)) return(NULL)
  if (!all(c(x_col, y_col) %in% names(data))) return(NULL)
  if (!is.numeric(data[[x_col]]) || !is.numeric(data[[y_col]])) return(NULL)

  split_col <- series_by %||% color_by
  # Only the columns the fits read, so the splits below copy two to four
  # columns rather than the whole frame.
  data <- data[intersect(c(x_col, y_col, split_col, facet_by), names(data))]


  drop_empty <- function(x) {
    x <- x[!vapply(x, is.null, logical(1L))]
    if (length(x) == 0L) NULL else x
  }

  fit_groups <- function(d) {
    if (nrow(d) == 0L) return(NULL)
    if (!is.null(split_col) && split_col %in% names(d)) {
      groups <- split(d, as.character(d[[split_col]]))
    } else {
      groups <- list(`__all__` = d)
    }
    drop_empty(lapply(groups, smoother_fit, smoother = smoother,
                      x_col = x_col, y_col = y_col, x_log = x_log,
                      y_log = y_log, loess_max = loess_max))
  }

  if (is.null(facet_by) || !facet_by %in% names(data)) {
    return(fit_groups(data))
  }

  # One fit per panel. split() drops NA levels, so a panel whose facet value
  # is missing gets no entry and the renderer draws no line there -- better
  # than lending it another panel's fit.
  drop_empty(lapply(split(data, as.character(data[[facet_by]])), fit_groups))
}

# One trend line over one group's rows (see compute_smoother_series()).
smoother_fit <- function(d, smoother, x_col, y_col, x_log, y_log,
                         loess_max) {
  keep <- !is.na(d[[x_col]]) & !is.na(d[[y_col]])
  if (isTRUE(x_log)) keep <- keep & d[[x_col]] > 0
  if (isTRUE(y_log)) keep <- keep & d[[y_col]] > 0
  d <- d[keep, , drop = FALSE]
  if (nrow(d) < 3L) return(NULL)
  xv <- d[[x_col]]
  yv <- d[[y_col]]
  if (isTRUE(x_log)) xv <- log10(xv)
  if (isTRUE(y_log)) yv <- log10(yv)
  if (length(unique(xv)) < 2L) return(NULL)
  rng <- range(xv, na.rm = TRUE)
  xs <- seq(rng[1L], rng[2L], length.out = 100L)
  surface <- "direct"
  at <- xs
  if (identical(smoother, "loess") && length(xv) > loess_max) {
    o <- order(xv)
    bin <- ((seq_along(o) - 1) * loess_max) %/% length(o)
    cnt <- tabulate(bin + 1L)
    xv <- rowsum(xv[o], bin, reorder = FALSE)[, 1L] / cnt
    yv <- rowsum(yv[o], bin, reorder = FALSE)[, 1L] / cnt
    surface <- "interpolate"
    # An interpolated surface is NA outside the fitted x, and the outer
    # bin means sit just inside the data's range. Read the ends at the
    # nearest fitted x so the line still spans the whole range.
    at <- pmin(pmax(xs, min(xv)), max(xv))
  }
  ys <- tryCatch({
    if (identical(smoother, "lm")) {
      coefs <- stats::coef(stats::lm(yv ~ xv))
      coefs[1L] + coefs[2L] * xs
    } else if (identical(smoother, "loess")) {
      fit <- stats::loess(yv ~ xv, span = 0.75,
                          control = stats::loess.control(surface = surface))
      as.numeric(stats::predict(fit, newdata = data.frame(xv = at)))
    } else {
      NULL
    }
  }, error = function(e) NULL)
  if (is.null(ys) || all(is.na(ys))) return(NULL)
  if (isTRUE(x_log)) xs <- 10^xs
  if (isTRUE(y_log)) ys <- 10^ys
  list(x = as.list(xs), y = as.list(unname(ys)))
}

# --- histogram -------------------------------------------------------------

# list(col, bin, edges): the bin column (named after x, carrying x's label),
# its values, and the edges a click on a bin filters on. NULL unless the
# chart is a histogram over a numeric x.
chart_hist <- function(state, data) {
  x <- state$x
  if (!identical(state$type, "histogram") || !is.data.frame(data) ||
        !length(x) || !x %in% names(data) || !is.numeric(data[[x]])) {
    return(NULL)
  }
  b <- chart_bins(data[[x]], state$bins)
  attr(b$bin, "label") <- attr(data[[x]], "label") %||% x
  list(col = chart_bin_col(x), bin = b$bin, edges = b$edges)
}

# The data with the histogram's bin column added.
chart_hist_data <- function(data, hist) {
  if (is.null(hist)) {
    return(data)
  }
  data[[hist$col]] <- hist$bin
  data
}

chart_bin_col <- function(x) {
  if (length(x)) paste0(x, "_bin") else NULL
}

# About `bins` bins over the range of `v` at pretty() breaks, as a factor
# labelled "lo-hi", closed on the left, the last on both sides; `edges`
# holds each label's ends.
chart_bins <- function(v, bins = 30) {
  v <- suppressWarnings(as.numeric(v))
  fin <- v[is.finite(v)]
  if (!length(fin)) {
    return(list(bin = factor(rep(NA_character_, length(v))),
                edges = data.frame(label = character(), lo = numeric(),
                                   hi = numeric())))
  }
  br <- pretty(range(fin), n = max(1L, as.integer(bins)))
  if (length(br) < 2L) br <- c(br[[1L]], br[[1L]] + 1)
  fmt <- format(br, trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
  labels <- paste0(utils::head(fmt, -1L), "\u2013", utils::tail(fmt, -1L))
  bin <- cut(v, br, labels = labels, right = FALSE, include.lowest = TRUE)
  list(
    bin = factor(bin, levels = labels),
    edges = data.frame(label = labels, lo = utils::head(br, -1L),
                       hi = utils::tail(br, -1L), last = c(rep(FALSE,
                       length(labels) - 1L), TRUE))
  )
}

# The condition a click on bins means: x inside any of them.
chart_bin_cond <- function(x, labels, edges) {
  if (!is.data.frame(edges)) {
    return(NULL)
  }
  e <- edges[edges$label %in% labels, , drop = FALSE]
  if (!nrow(e)) {
    return(NULL)
  }
  parts <- lapply(seq_len(nrow(e)), function(i) {
    if (isTRUE(e$last[[i]])) {
      bquote(.data[[.(x)]] >= .(e$lo[[i]]) & .data[[.(x)]] <= .(e$hi[[i]]))
    } else {
      bquote(.data[[.(x)]] >= .(e$lo[[i]]) & .data[[.(x)]] < .(e$hi[[i]]))
    }
  })
  Reduce(function(a, b) bquote((.(a)) | (.(b))), parts)
}
