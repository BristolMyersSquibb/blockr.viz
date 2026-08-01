#' Distribution statistic over a numeric vector
#'
#' The R twin of `summarizeStat()` in `inst/js/chart.js`: one shared
#' implementation so the box body, the box whiskers, the point range and the
#' band can never disagree on what a statistic means. Quantiles use type 7,
#' which is the same linear interpolation the JS side does (`i = p * (n - 1)`,
#' interpolate between the bracketing order statistics), and `sd` is the
#' sample sd (n - 1), `0` for a single observation -- again as in JS.
#'
#' `"p10_p90"` exists here and in `SUMMARY_STATS`; it is the band's default
#' outer interval. `"tukey"` returns the 1.5x IQR fences CLIPPED to the
#' observed range (`min(max(x), Q3 + 1.5 IQR)`), which is what chart.js has
#' always computed -- not the strict textbook whisker, which is the most
#' extreme observation lying inside the fence. The two differ whenever no
#' observation sits exactly at the fence. Mirrored deliberately: parity with
#' the boxplot matters more than the finer definition, and changing it here
#' alone would make a band and a boxplot of the same data disagree.
#'
#' Either way the value moves with the quartiles AND with the extremes, which
#' is why a fence-based ribbon wobbles as one subject enters or leaves the
#' window while a fixed quantile does not -- the reason the band defaults to
#' `"p10_p90"`.
#'
#' @param vals Numeric vector; non-finite values are dropped.
#' @param stat One of the `SUMMARY_STATS` values plus `"tukey"`.
#' @return A list with `center`, `lo`, `hi`, or `NULL` for an empty vector.
#' @keywords internal
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

#' Half-width of the window centred on one grid point
#'
#' `"fixed"` is `window_size` in x units. `"adaptive"` grows the window until
#' it holds `window_size` distinct ids, which keeps the band's stability
#' constant as the cohort thins -- the fixed window is simple to explain but
#' goes empty wherever the design is sparse (scheduled visits 56 days apart
#' late in a study leave a narrow window sitting in the gap between them).
#'
#' @param dist Absolute distances from the grid point to every observation.
#' @param id_code Integer subject codes aligned with `dist` (`match()` against
#'   the level set), or `NULL` to count rows. Integer codes and a
#'   pre-allocated seen-vector, not `unique()` on a growing character vector:
#'   the latter is quadratic in the window and this runs once per grid point.
#' @param n_ids Number of distinct subject codes.
#' @param window `"fixed"` or `"adaptive"`.
#' @param window_size Half-width in x units (fixed) or subject count
#'   (adaptive).
#' @param max_half Cap on the adaptive half-width.
#' @return A single numeric half-width.
#' @keywords internal
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

#' Compute distribution-band series per group for a band chart
#'
#' The band is a box plot dragged along a continuous x: at each grid point,
#' take every observation inside a window around it and compute the same
#' statistics a box plot computes. The window is the only genuinely new idea;
#' `summary` / `whiskers` are the existing distribution vocabulary
#' ([summarize_stat()]), so a band and a boxplot of the same data agree.
#'
#' Computed in R rather than the browser, following
#' [compute_smoother_series()]: the interactive chart and [static_chart()]
#' then share ONE implementation, so a deck and the app cannot disagree.
#'
#' Returns a named list keyed by group level (or `__all__`). Each entry holds
#' aligned numeric vectors over the grid: `x`, `center`, `lo`/`hi` (the inner
#' interval, `summary`), `olo`/`ohi` (the outer interval, `whiskers`), `n`
#' (distinct subjects in the window) and `hw` (the window's half-width, which
#' varies under `"adaptive"` and is worth surfacing in a tooltip). Grid points
#' holding fewer than `min_n` subjects are `NULL` in every statistic, so the
#' renderer cuts the band there rather than drawing a line through thin air.
#'
#' `out_x` / `out_y` / `out_id` carry the individual observations outside the
#' LOCAL Tukey fence. Those points are always classified by the Tukey rule,
#' whatever `whiskers` draws -- otherwise a `"p10_p90"` ribbon would flag a
#' fifth of the data as outliers.
#'
#' @param data Data frame.
#' @param x_col Numeric x column (e.g. study day).
#' @param y_col Numeric value column.
#' @param color_by,series_by Grouping columns; the band is computed per
#'   `series_by` if non-NULL else `color_by` else over everything.
#' @param facet_by Facet column. Each panel holds its own cohort, so the band
#'   is computed per facet level as well -- a facet showing one arm's panel
#'   with every arm's band would be silently wrong. The result is keyed by
#'   facet level first, then by series level.
#' @param summary Inner interval, a [summarize_stat()] value.
#' @param whiskers Outer interval; a [summarize_stat()] value or `"none"` to
#'   skip it.
#' @param window `"adaptive"` (default) or `"fixed"`.
#' @param window_size Subject count (adaptive) or half-width in x units
#'   (fixed).
#' @param min_n Minimum distinct subjects in a window; below it the band is
#'   cut.
#' @param id_col Subject id column, counted DISTINCT for `n` and for the
#'   adaptive window. `NULL` counts rows, which over-counts a cohort whenever
#'   one subject contributes several observations to the same window.
#' @param n_grid Grid resolution across the x range.
#' @return A named list or `NULL`.
#' @examples
#' d <- data.frame(day = rep(1:40, 5), val = rnorm(200), id = rep(1:5, each = 40))
#' compute_band_series(d, "day", "val", NULL, NULL, id_col = "id",
#'                     window = "fixed", window_size = 5, min_n = 2)
#' @keywords internal
#' @export
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

  keep <- is.finite(data[[x_col]]) & is.finite(data[[y_col]])
  data <- data[keep, , drop = FALSE]
  if (nrow(data) == 0L) return(NULL)

  split_col <- series_by %||% color_by
  if (!is.null(facet_by) && !(facet_by %in% names(data))) facet_by <- NULL

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

  outer_on <- !is.null(whiskers) && !identical(whiskers, "none")

  band_one <- function(d) {
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

  # Keyed facet level -> series level. Both tiers fall back to `__all__`, so
  # an unfaceted, uncoloured chart is simply res$__all__$__all__ and the
  # renderer reads one shape whatever is mapped.
  by_series <- function(d) {
    grp <- if (!is.null(split_col) && split_col %in% names(d)) {
      split(d, as.character(d[[split_col]]))
    } else {
      list(`__all__` = d)
    }
    out <- lapply(grp, band_one)
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

#' Reference-line value reduced from a column
#'
#' A reference range in ADaM (`ANRHI`, `A1LO`, ...) is a per-record column,
#' not a constant: `pharmaverseadam`'s ALT carries four distinct `ANRHI`
#' values across sites, spanning 32 to 43. A single line therefore has to
#' reduce it, and the label has to admit that it did -- hence `lo`/`hi`
#' alongside `value`, which the renderer prints as "ANRHI 34 (32-43)" when
#' the column is not constant.
#'
#' @param data Data frame.
#' @param col Column name, or `NULL` / `""` for no reference line.
#' @return A list with `col`, `value`, `lo`, `hi`, `n_distinct`, or `NULL`.
#' @keywords internal
#' @export
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

#' Why a distribution band came back empty
#'
#' [compute_band_series()] returns `NULL` for a whole family of ordinary
#' reasons -- a non-numeric timeline, a cohort too thin to clear `min_n`, a
#' filter that emptied the frame. The renderer cannot tell them apart, so
#' without this it draws an empty panel and the reader is left guessing. Call
#' it only when the band IS `NULL`; it re-walks the same guards in order and
#' names the first one that fires.
#'
#' @inheritParams compute_band_series
#' @return A one-sentence explanation, or `NULL` when the required mappings
#'   are simply unset (the renderer's own role gate covers that).
#' @keywords internal
#' @export
band_empty_reason <- function(data, x_col, y_col, min_n = 12, id_col = NULL) {
  if (is.null(x_col) || is.null(y_col) || !nzchar(x_col) || !nzchar(y_col)) {
    return(NULL)
  }
  if (is.null(data) || nrow(data) == 0L) {
    return("This block received no rows.")
  }
  for (col in c(x_col, y_col)) {
    if (!col %in% names(data)) {
      return(sprintf("Column \"%s\" is not in the data.", col))
    }
  }
  if (!is.numeric(data[[x_col]])) {
    return(sprintf(
      "Timeline \"%s\" is not numeric \u2014 a band slides a window along a continuous axis, so it needs a numeric column (e.g. ADY).",
      x_col))
  }
  if (!is.numeric(data[[y_col]])) {
    return(sprintf("Value \"%s\" is not numeric.", y_col))
  }
  ok <- is.finite(data[[x_col]]) & is.finite(data[[y_col]])
  if (!any(ok)) {
    return(sprintf("Every row is missing \"%s\" or \"%s\".", x_col, y_col))
  }
  d <- data[ok, , drop = FALSE]
  if (length(unique(d[[x_col]])) < 2L) {
    return(sprintf(
      "\"%s\" has only one distinct value, so there is no range to slide a window along.",
      x_col))
  }
  by_id <- !is.null(id_col) && id_col %in% names(d)
  n <- if (by_id) length(unique(d[[id_col]])) else nrow(d)
  sprintf(
    "No window reached the cut-off of %s: the whole series holds only %d. Lower \"Cut below n\", widen the window, or relax the upstream filter.",
    if (by_id) sprintf("%d subjects", min_n) else sprintf("%d rows", min_n),
    n)
}
