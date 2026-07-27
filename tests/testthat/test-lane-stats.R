# Lane chart statistics: hand-computed values, the n < 2 guard, and the
# R/JS enum drift guards.
#
# The definitions are MIRRORED in chart.js (summarizeStat) rather than shared
# -- one language per surface -- so behaviour is pinned by the same
# hand-computed table on both sides, and the enum by parsing the JS literals.

test_that("each statistic matches hand-computed values (x = 1,2,3,4,10)", {
  x <- c(1, 2, 3, 4, 10)

  s <- lane_summarize(x, "median_q1_q3")
  expect_identical(s$n, 5L)
  expect_equal(c(s$center, s$lo, s$hi), c(3, 2, 4))

  s <- lane_summarize(x, "mean_sd")
  expect_equal(s$center, 4)
  expect_equal(s$lo, 4 - sqrt(50 / 4))
  expect_equal(s$hi, 4 + sqrt(50 / 4))

  s <- lane_summarize(x, "mean_2sd")
  expect_equal(s$hi, 4 + 2 * sqrt(50 / 4))

  s <- lane_summarize(x, "mean_se")
  expect_equal(s$hi - s$center, sqrt(50 / 4) / sqrt(5))

  # Quantile type 7 IS the JS formula p * (n - 1) + linear interpolation.
  s <- lane_summarize(x, "p5_p95")
  expect_equal(c(s$center, s$lo, s$hi), c(3, 1.2, 8.8))

  s <- lane_summarize(x, "min_max")
  expect_equal(c(s$center, s$lo, s$hi), c(3, 1, 10))

  # Tukey fences clamp to the observed extremes: lo fence -1 -> min 1,
  # hi fence 7 stays (10 is an outlier past it).
  s <- lane_summarize(x, "tukey")
  expect_equal(c(s$center, s$lo, s$hi), c(3, 1, 7))
})

test_that("mean_ci95 uses the t quantile, never 1.96", {
  # n = 6, where the two intervals differ by 31%: a future 'optimization'
  # back to the normal approximation must fail loudly here.
  x <- c(1, 2, 3, 4, 5, 6)
  s <- lane_summarize(x, "mean_ci95")
  se <- stats::sd(x) / sqrt(6)
  expect_equal(s$hi - s$center, stats::qt(0.975, 5) * se)
  expect_gt((s$hi - s$lo) / 2, 1.25 * 1.96 * se)
})

test_that("degenerate n: NA bounds below n = 2, sd 0 at n = 1, NA at n = 0", {
  # n = 1: the CI is undefined -- NA bounds, so the cell renders the center
  # alone rather than a zero-width interval, which would read as certainty.
  s <- lane_summarize(5, "mean_ci95")
  expect_equal(s$center, 5)
  expect_true(is.na(s$lo) && is.na(s$hi))
  expect_identical(s$n, 1L)

  # n = 1 arithmetic stats: sd is 0 (chart.js parity), a zero-width interval.
  s <- lane_summarize(5, "mean_sd")
  expect_equal(c(s$center, s$lo, s$hi), c(5, 5, 5))

  # Empty / all-NA input.
  s <- lane_summarize(numeric(), "median_q1_q3")
  expect_identical(s$n, 0L)
  expect_true(is.na(s$center))
  s <- lane_summarize(c(NA_real_, NA_real_), "mean_se")
  expect_identical(s$n, 0L)
})

test_that("an unknown statistic falls back to median_q1_q3 (chart.js parity)", {
  x <- c(1, 2, 3, 4, 10)
  expect_equal(lane_summarize(x, "no_such_stat"),
               lane_summarize(x, "median_q1_q3"))
})

test_that("lane_stat_agg computes per group, box shape carries both stats", {
  d <- data.frame(g = rep(c("a", "b"), each = 5),
                  v = c(1, 2, 3, 4, 10, 2, 4, 6, 8, 10))
  out <- lane_stat_agg(d, "g", "v", list(.b = "median_q1_q3", .w = "tukey"))
  expect_setequal(names(out), c("g", ".bc", ".bl", ".bh", ".wc", ".wl",
                                ".wh", ".n"))
  a <- out[out$g == "a", ]
  expect_equal(a$.bc, 3)
  expect_equal(a$.wh, 7)   # the clamped Tukey fence from the unit test above
  expect_identical(a$.n, 5L)
})

# --- drift guards ------------------------------------------------------------

# Parse `{ value: '...', label: '...' }` literals out of a JS source region.
js_enum <- function(path, anchor) {
  src <- paste(readLines(path, warn = FALSE), collapse = "\n")
  block <- sub(paste0(".*", anchor, "\\s*=\\s*\\["), "", src)
  block <- sub("\\][\\s\\S]*", "", block, perl = TRUE)
  m <- gregexpr("value:\\s*['\"]([^'\"]+)['\"]", block, perl = TRUE)
  vals <- regmatches(block, m)[[1]]
  sub("value:\\s*['\"]([^'\"]+)['\"]", "\\1", vals, perl = TRUE)
}

test_that("LANE_STATS mirrors chart.js SUMMARY_STATS plus mean_ci95", {
  js <- system.file("js", "chart.js", package = "blockr.viz")
  skip_if(!nzchar(js) || !file.exists(js), "chart.js not found")
  chart_stats <- js_enum(js, "SUMMARY_STATS")
  expect_true(length(chart_stats) >= 5L)
  # The one deliberate difference: mean_ci95 exists only where R computes
  # (qt has no JS equivalent; see the spec's NOTE-confidence-intervals.md).
  expect_identical(setdiff(LANE_STATS, chart_stats), "mean_ci95")
  expect_identical(setdiff(chart_stats, LANE_STATS), character(0))
  # Same relative order, so the two selects read identically.
  expect_identical(intersect(LANE_STATS, chart_stats), chart_stats)
})

test_that("rank-table.js LANE_STATS mirrors R's, values and labels", {
  js <- system.file("js", "rank-table.js", package = "blockr.viz")
  skip_if(!nzchar(js) || !file.exists(js), "rank-table.js not found")
  expect_identical(js_enum(js, "var LANE_STATS"), LANE_STATS)
  src <- paste(readLines(js, warn = FALSE), collapse = "\n")
  for (nm in LANE_STATS) {
    lbl <- LANE_STAT_META[[nm]]$label
    expect_true(grepl(lbl, src, fixed = TRUE),
                info = paste0(nm, " label '", lbl, "' missing in JS"))
  }
})
