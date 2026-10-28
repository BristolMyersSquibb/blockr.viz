# Lane statistics are computed in R; the JS only draws finished numbers.
# `mean_ci95` exists only here: a 95% CI needs `qt(0.975, n - 1)`, and the
# normal 1.96 is 31% too narrow at n = 6.

# Mirrors the chart's STAT_META in inst/js/chart/agg.js (drift-tested in
# tests/testthat/test-lane-stats.R).
LANE_STATS <- c(
  "median_q1_q3", "mean_sd", "mean_2sd", "mean_se", "mean_ci95",
  "p5_p95", "p10_p90", "min_max"
)

# Whisker rules: the same vocabulary plus Tukey fences.
LANE_WHISKERS <- c("tukey", LANE_STATS)

# Display metadata per statistic: the select label and the tooltip's words for
# the center and the interval. Glyphs are \u-escaped because R CMD check forbids
# non-ASCII in string literals (\u00b7 middle dot, \u2013 en dash, \u00b1
# plus-minus, \u00d7 times).
LANE_STAT_META <- list(
  median_q1_q3 = list(
    label = "Median \u00b7 Q1\u2013Q3",
    center = "Median",
    range = "Q1\u2013Q3"
  ),
  mean_sd = list(
    label = "Mean \u00b1 SD",
    center = "Mean",
    range = "\u00b11 SD"
  ),
  mean_2sd = list(
    label = "Mean \u00b1 2 SD",
    center = "Mean",
    range = "\u00b12 SD"
  ),
  mean_se = list(
    label = "Mean \u00b1 SE",
    center = "Mean",
    range = "\u00b11 SE"
  ),
  mean_ci95 = list(
    label = "Mean \u00b7 95% CI",
    center = "Mean",
    range = "95% CI"
  ),
  p5_p95 = list(
    label = "5th\u201395th percentile",
    center = "Median",
    range = "P5\u2013P95"
  ),
  p10_p90 = list(
    label = "10th\u201390th percentile",
    center = "Median",
    range = "P10\u2013P90"
  ),
  min_max = list(
    label = "Min\u2013Max",
    center = "Median",
    range = "Min\u2013Max"
  ),
  tukey = list(
    label = "Tukey (1.5\u00d7IQR)",
    center = "Median",
    range = "1.5\u00d7IQR"
  )
)

#' The shared, expensive half of every statistic: one sort per group, so each
#' quantile is an indexed read.
#' @return `list(x = sorted values, n =, mean =, sd =)`; `mean`/`sd` are NA at
#'   n = 0 and `sd` is 0 at n = 1.
#' @noRd
lane_stat_basis <- function(x) {
  x <- as.numeric(x)
  x <- sort(x[is.finite(x)])
  n <- length(x)
  list(
    x = x,
    n = n,
    mean = if (n) mean(x) else NA_real_,
    sd = if (n > 1L) stats::sd(x) else if (n) 0 else NA_real_
  )
}

#' `stats::quantile(type = 7)` off an already-sorted vector, so the sort in
#' [lane_stat_basis()] is paid once.
#' Written `(1 - h) * lo + h * hi` with `quantile.default`'s own guards, not
#' `lo + h * (hi - lo)`: the latter moved ~2.5% of a real table's values by
#' ~1e-12, enough to flip a rounded digit. test-lane-stats.R pins it against
#' `stats::quantile()`.
#' @noRd
lane_q <- function(b, p) {
  if (!b$n) return(NA_real_)
  index <- 1 + (b$n - 1L) * p
  lo <- floor(index)
  qs <- b$x[[lo]]
  hi <- b$x[[ceiling(index)]]
  if (index > lo && hi != qs) {
    h <- index - lo
    qs <- (1 - h) * qs + h * hi
  }
  qs
}

#' One statistic off a prepared basis `b` ([lane_stat_basis()]); `stat` is one
#' of `LANE_WHISKERS`. Matches `summarizeStat()` (chart/model-aggregated.js).
#' `mean_ci95` uses `qt(0.975, n - 1)` and has NA bounds below n = 2, so the
#' cell shows the center alone instead of a zero-width interval.
#' @return `list(center =, lo =, hi =, n =)`, all NA at n = 0.
#' @noRd
lane_summarize_at <- function(b, stat = "median_q1_q3") {
  n <- b$n
  if (n == 0L) {
    return(list(center = NA_real_, lo = NA_real_, hi = NA_real_, n = 0L))
  }
  q <- function(p) lane_q(b, p)
  m <- b$mean
  s <- b$sd
  se <- s / sqrt(n)
  out <- switch(
    stat %||% "median_q1_q3",
    mean_sd = c(m, m - s, m + s),
    mean_2sd = c(m, m - 2 * s, m + 2 * s),
    mean_se = c(m, m - se, m + se),
    mean_ci95 = if (n < 2L) {
      c(m, NA_real_, NA_real_)
    } else {
      c(m, m - stats::qt(0.975, n - 1) * se, m + stats::qt(0.975, n - 1) * se)
    },
    p5_p95 = c(q(0.5), q(0.05), q(0.95)),
    p10_p90 = c(q(0.5), q(0.10), q(0.90)),
    # Sorted, so the extremes are the ends.
    min_max = c(q(0.5), b$x[[1L]], b$x[[n]]),
    tukey = {
      q1 <- q(0.25)
      q3 <- q(0.75)
      iqr <- q3 - q1
      c(q(0.5), max(b$x[[1L]], q1 - 1.5 * iqr), min(b$x[[n]], q3 + 1.5 * iqr))
    },
    # median_q1_q3, and the fallback for an unknown value (chart parity).
    c(q(0.5), q(0.25), q(0.75))
  )
  list(center = out[[1L]], lo = out[[2L]], hi = out[[3L]], n = n)
}

#' One statistic over a numeric vector. For several statistics over the same
#' values, build one [lane_stat_basis()] and call [lane_summarize_at()] each.
#' @inheritParams lane_summarize_at
#' @noRd
lane_summarize <- function(x, stat = "median_q1_q3") {
  lane_summarize_at(lane_stat_basis(x), stat)
}

#' Per-group statistics as a data frame: `keys` columns plus the requested
#' stat columns. `stats` is a named list `out_prefix = stat_name`; each entry
#' adds `<prefix>c` / `<prefix>l` / `<prefix>h` columns (the box calls it with
#' body + whiskers, the point range with one entry). `.n` rides along once.
#' @noRd
lane_stat_agg <- function(data, keys, value, stats) {
  f <- function(v) {
    # One basis per group: the sort every statistic needs, and the row count.
    b <- lane_stat_basis(v)
    out <- list()
    for (nm in names(stats)) {
      s <- lane_summarize_at(b, stats[[nm]])
      out[[paste0(nm, "c")]] <- s$center
      out[[paste0(nm, "l")]] <- s$lo
      out[[paste0(nm, "h")]] <- s$hi
    }
    out$.n <- b$n
    # Runs once per group, where as.data.frame() costs more than the
    # statistics. Every element has length 1, so build the frame directly.
    structure(out, class = "data.frame", row.names = .set_row_names(1L))
  }
  if (!length(keys)) {
    out <- dplyr::summarise(data, f(.data[[value]]))
  } else {
    g <- dplyr::group_by(data, dplyr::across(dplyr::all_of(keys)))
    out <- dplyr::summarise(g, f(.data[[value]]), .groups = "drop")
  }
  as.data.frame(out, check.names = FALSE)
}

# Display formatting for a statistic value: significant digits like the num
# cells (summarize_num_parts), trimmed of formatC's common-width padding.
#' @noRd
lane_fmt <- function(x) {
  ifelse(is.na(x), "", trimws(formatC(x, format = "fg", digits = 4L,
                                      big.mark = "")))
}
