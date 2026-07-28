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
#
# The labels are typeset, not plain ASCII, and R CMD check forbids non-ASCII
# bytes in code (comments are exempt, string literals are not). The glyphs are
# therefore \u-escaped: · MIDDLE DOT, – EN DASH, ± PLUS-MINUS,
# × MULTIPLICATION SIGN. One field per line only because the escapes are
# too wide to keep the old aligned single-line rows under 80 columns.
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

#' The shared, expensive half of every statistic, computed once.
#'
#' A box column asks for TWO statistics over the same values (the body and the
#' whiskers) and then the row count, and each `stats::quantile()` call sorts
#' the vector again -- seven sorts of one group's values where one will do.
#' Sorting up front turns every quantile below into an indexed read, so the
#' cost per group is one `sort()` instead of one per requested probability.
#'
#' @param x Numeric values (non-finite dropped).
#' @return `list(x = sorted values, n =, mean =, sd =)`; `mean`/`sd` are NA at
#'   n = 0 and `sd` is 0 at n = 1, matching the sample-sd convention.
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

#' `stats::quantile(type = 7)` off an already-sorted vector.
#'
#' Type 7 IS `index = (n - 1)p + 1` plus linear interpolation between the
#' bracketing order statistics -- the same formula chart.js uses. Reproduced
#' here rather than called so the sort in [lane_stat_basis()] is paid once.
#'
#' The interpolation is written `(1 - h) * lo + h * hi`, NOT the algebraically
#' equal `lo + h * (hi - lo)`, and the two guards below are `quantile.default`'s
#' own: they make this bit-identical to `stats::quantile()` rather than merely
#' equal to within float tolerance. It matters -- the second form moved ~2.5% of
#' a real AE table's values by ~1e-12, which is invisible until it lands on a
#' rounding boundary and flips a displayed digit (31.59 -> 31.6).
#' test-lane-stats.R pins this against `stats::quantile()`.
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

#' One statistic off a prepared basis: center, lo, hi and n.
#'
#' Definitions identical to chart.js `summarizeStat()`: quantiles by linear
#' interpolation, sample sd (`n - 1`, 0 for a single observation), Tukey
#' fences clipped to the observed extremes. `mean_ci95` uses
#' `stats::qt(0.975, n - 1)` -- NEVER 1.96 (see the header) -- and is
#' undefined below n = 2: NA bounds, so the cell renders the center alone
#' rather than a zero-width interval, which would read as certainty.
#'
#' @param b A [lane_stat_basis()].
#' @param stat One of `LANE_WHISKERS` (`LANE_STATS` plus `"tukey"`).
#' @return `list(center =, lo =, hi =, n =)`, all-NA center/bounds at n = 0.
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
    # Sorted, so the extremes are the ends.
    min_max = c(q(0.5), b$x[[1L]], b$x[[n]]),
    tukey = {
      q1 <- q(0.25)
      q3 <- q(0.75)
      iqr <- q3 - q1
      c(q(0.5), max(b$x[[1L]], q1 - 1.5 * iqr), min(b$x[[n]], q3 + 1.5 * iqr))
    },
    # median_q1_q3, and the fallback for an unknown value (chart.js parity).
    c(q(0.5), q(0.25), q(0.75))
  )
  list(center = out[[1L]], lo = out[[2L]], hi = out[[3L]], n = n)
}

#' One statistic over a numeric vector: center, lo, hi and n.
#'
#' The single-statistic entry point. Callers wanting several statistics over
#' the same values should build one [lane_stat_basis()] and call
#' [lane_summarize_at()] per statistic instead.
#'
#' @inheritParams lane_summarize_at
#' @param x Numeric values (non-finite dropped).
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
    # ONE basis per group: the sort every statistic needs, paid once, and the
    # row count read straight off it (it used to come from a whole extra
    # min_max summary, i.e. another sort to reach a length).
    b <- lane_stat_basis(v)
    out <- list()
    for (nm in names(stats)) {
      s <- lane_summarize_at(b, stats[[nm]])
      out[[paste0(nm, "c")]] <- s$center
      out[[paste0(nm, "l")]] <- s$lo
      out[[paste0(nm, "h")]] <- s$hi
    }
    out$.n <- b$n
    # This runs once per group, so `as.data.frame()` spends more time
    # validating and de-duplicating names than the statistics take to compute.
    # Every element is a length-1 vector by construction -- build the one-row
    # frame directly (the old call passed check.names = FALSE, so there was no
    # name mangling to preserve either).
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
# cells (rank_num_parts), trimmed of formatC's common-width padding.
#' @noRd
lane_fmt <- function(x) {
  ifelse(is.na(x), "", trimws(formatC(x, format = "fg", digits = 4L,
                                      big.mark = "")))
}
