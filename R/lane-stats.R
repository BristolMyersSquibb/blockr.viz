# Lane chart: distribution statistics -----------------------------------------
#
# The lane chart's marks draw FINISHED numbers: R computes every statistic and
# ships positions as percentages plus display values; the JS only draws
# (_blockr.design/open/lane-chart/spec.md, "Statistics are computed in R").
#
# The vocabulary mirrors chart.js's SUMMARY_STATS (inst/js/chart.js) plus
# `mean_ci95`, which exists ONLY here: a 95% CI needs `qt(0.975, n - 1)`, JS
# has no t quantile, and the normal approximation (1.96) is 31% too narrow at
# n = 6 -- real CEDX cells have N of 6, 16, 18, 21. The two definitions are
# mirrored, not shared (one language per surface), with a drift test pinning
# the enum and hand-computed values pinning the behaviour on both sides:
# tests/testthat/test-lane-stats.R.

# MUST mirror chart.js SUMMARY_STATS values (drift-tested), plus mean_ci95.
LANE_STATS <- c(
  "median_q1_q3", "mean_sd", "mean_2sd", "mean_se", "mean_ci95",
  "p5_p95", "min_max"
)

# Whisker rules: the same vocabulary plus Tukey fences (chart.js WHISKER_STATS).
LANE_WHISKERS <- c("tukey", LANE_STATS)

# Display metadata, one row per statistic: the select label, and the words the
# tooltip uses for the center and the interval. Mirrors chart.js's
# SUMMARY_STATS labels where the values overlap (drift-tested).
LANE_STAT_META <- list(
  median_q1_q3 = list(label = "Median · Q1–Q3", center = "Median", range = "Q1–Q3"),
  mean_sd      = list(label = "Mean ± SD",           center = "Mean",   range = "±1 SD"),
  mean_2sd     = list(label = "Mean ± 2 SD",         center = "Mean",   range = "±2 SD"),
  mean_se      = list(label = "Mean ± SE",           center = "Mean",   range = "±1 SE"),
  mean_ci95    = list(label = "Mean · 95% CI",       center = "Mean",   range = "95% CI"),
  p5_p95       = list(label = "5th–95th percentile", center = "Median", range = "P5–P95"),
  min_max      = list(label = "Min–Max",             center = "Median", range = "Min–Max"),
  tukey        = list(label = "Tukey (1.5×IQR)",     center = "Median", range = "1.5×IQR")
)

#' One statistic over a numeric vector: center, lo, hi and n.
#'
#' Definitions identical to chart.js `summarizeStat()`: quantiles by linear
#' interpolation (`stats::quantile(type = 7)` IS the JS formula
#' `p * (n - 1)` + linear interpolation), sample sd (`n - 1`, 0 for a single
#' observation), Tukey fences clipped to the observed extremes. `mean_ci95`
#' uses `stats::qt(0.975, n - 1)` -- NEVER 1.96 (see the header) -- and is
#' undefined below n = 2: NA bounds, so the cell renders the center alone
#' rather than a zero-width interval, which would read as certainty.
#'
#' @param x Numeric values (non-finite dropped).
#' @param stat One of `LANE_WHISKERS` (`LANE_STATS` plus `"tukey"`).
#' @return `list(center =, lo =, hi =, n =)`, all-NA center/bounds at n = 0.
#' @noRd
lane_summarize <- function(x, stat = "median_q1_q3") {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  n <- length(x)
  if (n == 0L) {
    return(list(center = NA_real_, lo = NA_real_, hi = NA_real_, n = 0L))
  }
  q <- function(p) unname(stats::quantile(x, p, type = 7, names = FALSE))
  m <- mean(x)
  s <- if (n > 1L) stats::sd(x) else 0
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
    min_max = c(q(0.5), min(x), max(x)),
    tukey = {
      q1 <- q(0.25)
      q3 <- q(0.75)
      iqr <- q3 - q1
      c(q(0.5), max(min(x), q1 - 1.5 * iqr), min(max(x), q3 + 1.5 * iqr))
    },
    # median_q1_q3, and the fallback for an unknown value (chart.js parity).
    c(q(0.5), q(0.25), q(0.75))
  )
  list(center = out[[1L]], lo = out[[2L]], hi = out[[3L]], n = n)
}

#' Per-group statistics as a data frame: `keys` columns plus the requested
#' stat columns. `stats` is a named list `out_prefix = stat_name`; each entry
#' adds `<prefix>c` / `<prefix>l` / `<prefix>h` columns (the box calls it with
#' body + whiskers, the point range with one entry). `.n` rides along once.
#' @noRd
lane_stat_agg <- function(data, keys, value, stats) {
  f <- function(v) {
    out <- list()
    for (nm in names(stats)) {
      s <- lane_summarize(v, stats[[nm]])
      out[[paste0(nm, "c")]] <- s$center
      out[[paste0(nm, "l")]] <- s$lo
      out[[paste0(nm, "h")]] <- s$hi
    }
    out$.n <- lane_summarize(v, "min_max")$n
    as.data.frame(out, check.names = FALSE)
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
# cells (rank_num_parts), trimmed of formatC's common-width padding.
#' @noRd
lane_fmt <- function(x) {
  ifelse(is.na(x), "", trimws(formatC(x, format = "fg", digits = 4L,
                                      big.mark = "")))
}
