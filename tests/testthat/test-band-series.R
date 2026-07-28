# Distribution band: the statistics, the windowing, and the guards.
#
# summarize_stat() is the R twin of summarizeStat() in inst/js/chart.js. The
# two are mirrored, not shared (one language per surface), so the values are
# pinned here by hand and the vocabulary is drift-tested against the JS enum --
# the same arrangement test-lane-stats.R uses for LANE_STATS.

# Values on 1:10 under quantile type 7, which is what BOTH sides compute:
# i = p * (n - 1), interpolate between the bracketing order statistics.
test_that("summarize_stat pins each statistic on a known vector", {
  v <- 1:10

  expect_equal(summarize_stat(v, "median_q1_q3"),
               list(center = 5.5, lo = 3.25, hi = 7.75))
  expect_equal(summarize_stat(v, "p10_p90"),
               list(center = 5.5, lo = 1.9, hi = 9.1))
  expect_equal(summarize_stat(v, "p5_p95"),
               list(center = 5.5, lo = 1.45, hi = 9.55))
  expect_equal(summarize_stat(v, "min_max"),
               list(center = 5.5, lo = 1, hi = 10))

  m <- mean(v)
  s <- stats::sd(v)                       # sample sd (n - 1), as in JS
  expect_equal(summarize_stat(v, "mean_sd"),
               list(center = m, lo = m - s, hi = m + s))
  expect_equal(summarize_stat(v, "mean_2sd"),
               list(center = m, lo = m - 2 * s, hi = m + 2 * s))
  expect_equal(summarize_stat(v, "mean_se"),
               list(center = m, lo = m - s / sqrt(10), hi = m + s / sqrt(10)))

  # Tukey: the fences (Q1 - 1.5 IQR, Q3 + 1.5 IQR) CLIPPED to the observed
  # range -- chart.js's long-standing definition, mirrored here for parity.
  # Note this is not the strict textbook whisker (the most extreme observation
  # inside the fence); the two differ unless a point sits on the fence.
  expect_equal(summarize_stat(v, "tukey"),
               list(center = 5.5, lo = 1, hi = 10))   # fences outside the data
  out <- summarize_stat(c(1:10, 100), "tukey")
  expect_lt(out$hi, 100)                  # the outlier is excluded
  expect_equal(out$hi, 8.5 + 1.5 * 5)     # ... at the fence, not at 10
})

test_that("summarize_stat degrades on thin and empty input", {
  expect_null(summarize_stat(numeric()))
  expect_null(summarize_stat(c(NA_real_, NaN, Inf)))
  # A single observation: sd is 0 in both languages, so the interval collapses
  # onto the centre rather than becoming NA.
  expect_equal(summarize_stat(7, "mean_sd"), list(center = 7, lo = 7, hi = 7))
  expect_equal(summarize_stat(7, "median_q1_q3"),
               list(center = 7, lo = 7, hi = 7))
  # Unknown value falls back to the box, matching chart.js's switch default.
  expect_equal(summarize_stat(1:10, "not_a_stat"),
               summarize_stat(1:10, "median_q1_q3"))
})

test_that("summarize_stat covers chart.js SUMMARY_STATS with no drift", {
  js <- system.file("js", "chart.js", package = "blockr.viz")
  skip_if(!nzchar(js) || !file.exists(js), "chart.js not found")
  src <- paste(readLines(js, warn = FALSE), collapse = "\n")
  block <- sub("^[\\s\\S]*?const SUMMARY_STATS = \\[", "", src, perl = TRUE)
  block <- sub("\\][\\s\\S]*", "", block, perl = TRUE)
  m <- gregexpr("value:\\s*['\"]([^'\"]+)['\"]", block, perl = TRUE)
  js_stats <- sub("value:\\s*['\"]([^'\"]+)['\"]", "\\1",
                  regmatches(block, m)[[1]], perl = TRUE)

  expect_true("p10_p90" %in% js_stats)
  # Every statistic the browser offers must be one R can compute, or a band
  # (computed in R) and a boxplot (computed in JS) would disagree about what
  # the same gear setting means.
  for (st in js_stats) {
    res <- summarize_stat(1:10, st)
    expect_type(res, "list")
    expect_named(res, c("center", "lo", "hi"))
    expect_false(identical(res, summarize_stat(1:10, "median_q1_q3")) &&
                   !st %in% c("median_q1_q3"),
                 info = paste0(st, " silently fell through to the default"))
  }
})

# ---- windowing --------------------------------------------------------------

make_data <- function(n_id = 30, days = seq(0, 100, by = 10)) {
  expand <- expand.grid(id = seq_len(n_id), day = days)
  data.frame(
    id = sprintf("S%02d", expand$id),
    day = expand$day,
    val = expand$day / 10 + (expand$id %% 5),
    arm = ifelse(expand$id %% 2 == 0, "A", "B"),
    stringsAsFactors = FALSE
  )
}

test_that("compute_band_series nests facet then series, both defaulting", {
  d <- make_data()

  flat <- compute_band_series(d, "day", "val", NULL, NULL,
                              window = "fixed", window_size = 15, min_n = 2)
  expect_named(flat, "__all__")
  expect_named(flat[["__all__"]], "__all__")

  coloured <- compute_band_series(d, "day", "val", "arm", NULL,
                                  window = "fixed", window_size = 15, min_n = 2)
  expect_named(coloured, "__all__")
  expect_setequal(names(coloured[["__all__"]]), c("A", "B"))

  # Faceting splits the OUTER tier: a panel showing one arm must not carry
  # every arm's band.
  faceted <- compute_band_series(d, "day", "val", NULL, NULL, facet_by = "arm",
                                 window = "fixed", window_size = 15, min_n = 2)
  expect_setequal(names(faceted), c("A", "B"))
  expect_named(faceted[["A"]], "__all__")
})

test_that("compute_band_series emits exactly the keys the renderer reads", {
  d <- make_data()
  b <- compute_band_series(d, "day", "val", NULL, NULL, id_col = "id",
                           window = "fixed", window_size = 15,
                           min_n = 2)[["__all__"]][["__all__"]]
  # inst/js/chart.js reads these by name; a rename here is a silent blank chart.
  expect_true(all(c("x", "center", "lo", "hi", "olo", "ohi", "n", "hw") %in%
                    names(b)))
  expect_length(b$center, length(b$x))
  expect_length(b$n, length(b$x))
  expect_length(b$hw, length(b$x))
})

test_that("the grid spans the whole x range, shared across groups", {
  d <- make_data()
  b <- compute_band_series(d, "day", "val", "arm", NULL,
                           window = "fixed", window_size = 15, min_n = 2)
  xs <- lapply(b[["__all__"]], `[[`, "x")
  # Both arms sit on the SAME grid, or a tooltip could not report them together.
  expect_identical(xs$A, xs$B)
  expect_equal(unlist(xs$A[1]), 0)
  expect_equal(unlist(xs$A[length(xs$A)]), 100)
})

test_that("min_n cuts a window rather than drawing through thin air", {
  d <- make_data(n_id = 10)
  cut <- compute_band_series(d, "day", "val", NULL, NULL, id_col = "id",
                             window = "fixed", window_size = 5, min_n = 999)
  expect_null(cut)

  ok <- compute_band_series(d, "day", "val", NULL, NULL, id_col = "id",
                            window = "fixed", window_size = 5,
                            min_n = 2)[["__all__"]][["__all__"]]
  expect_true(any(!vapply(ok$center, is.null, logical(1))))
})

test_that("id_col counts DISTINCT subjects, not rows", {
  # 4 subjects x 11 days: a row count would see 44 and clear a cut-off that a
  # subject count must not.
  d <- make_data(n_id = 4)
  by_row <- compute_band_series(d, "day", "val", NULL, NULL, id_col = NULL,
                                window = "fixed", window_size = 200, min_n = 10)
  by_subj <- compute_band_series(d, "day", "val", NULL, NULL, id_col = "id",
                                 window = "fixed", window_size = 200,
                                 min_n = 10)
  expect_false(is.null(by_row))   # 44 rows clears 10
  expect_null(by_subj)            # 4 subjects does not
})

test_that("the adaptive window widens where the design is sparse", {
  # Dense early, one lonely late visit: a fixed window would go empty out
  # there, the adaptive one grows to reach its quota.
  d <- rbind(make_data(n_id = 20, days = seq(0, 40, by = 5)),
             make_data(n_id = 20, days = 300))
  b <- compute_band_series(d, "day", "val", NULL, NULL, id_col = "id",
                           window = "adaptive", window_size = 10,
                           min_n = 5)[["__all__"]][["__all__"]]
  hw <- unlist(b$hw)
  expect_true(max(hw) > min(hw))   # the half-width is not constant
})

test_that("outliers use the Tukey fence whatever whiskers draws", {
  d <- make_data(n_id = 30)
  d$val[d$day == 50 & d$id == "S01"] <- 1000

  for (w in c("p10_p90", "tukey", "min_max")) {
    b <- compute_band_series(d, "day", "val", NULL, NULL, id_col = "id",
                             whiskers = w, window = "fixed", window_size = 15,
                             min_n = 2)[["__all__"]][["__all__"]]
    expect_true(1000 %in% unlist(b$out_y),
                info = paste("whiskers =", w))
    # A P10-P90 ribbon would otherwise flag a fifth of the data.
    expect_lt(length(b$out_y %||% list()), nrow(d) / 5)
  }
})

test_that("compute_band_series refuses input it cannot window", {
  d <- make_data()
  expect_null(compute_band_series(d, "arm", "val", NULL, NULL))     # x not numeric
  expect_null(compute_band_series(d, "day", "arm", NULL, NULL))     # y not numeric
  expect_null(compute_band_series(d, "nope", "val", NULL, NULL))    # absent
  expect_null(compute_band_series(d[0, ], "day", "val", NULL, NULL))
  expect_null(compute_band_series(NULL, "day", "val", NULL, NULL))
  # One distinct x: no range to slide along.
  flat <- d[d$day == 0, ]
  expect_null(compute_band_series(flat, "day", "val", NULL, NULL, min_n = 2))
})

# ---- reference limits -------------------------------------------------------

test_that("band_reference reduces a per-record column and keeps its spread", {
  d <- data.frame(hi = c(32, 32, 35, 43), lo = c(6, 6, 6, 6),
                  chr = letters[1:4], stringsAsFactors = FALSE)

  r <- band_reference(d, "hi")
  expect_equal(r$col, "hi")
  expect_equal(r$value, 33.5)          # median, not mean
  expect_equal(c(r$lo, r$hi), c(32, 43))
  expect_equal(r$n_distinct, 3)

  # Constant column: n_distinct 1, so the renderer drops the "(lo-hi)" tail.
  expect_equal(band_reference(d, "lo")$n_distinct, 1)

  expect_null(band_reference(d, "chr"))
  expect_null(band_reference(d, "absent"))
  expect_null(band_reference(d, NULL))
  expect_null(band_reference(d, ""))
})

# ---- the empty-state diagnostic --------------------------------------------

test_that("band_empty_reason names the first guard that fired", {
  d <- make_data()

  # Unset mappings are the role gate's job, not this one's.
  expect_null(band_empty_reason(d, NULL, "val"))
  expect_null(band_empty_reason(d, "", "val"))

  expect_match(band_empty_reason(d[0, ], "day", "val"), "no rows")
  expect_match(band_empty_reason(d, "nope", "val"), "not in the data")
  expect_match(band_empty_reason(d, "arm", "val"), "not numeric")
  expect_match(band_empty_reason(d, "day", "arm"), "not numeric")

  flat <- d[d$day == 0, ]
  expect_match(band_empty_reason(flat, "day", "val"), "one distinct value")

  # The common case: the cohort never reaches the cut-off. The message has to
  # quote BOTH numbers, or the next move is a guessing game.
  msg <- band_empty_reason(d, "day", "val", min_n = 999, id_col = "id")
  expect_match(msg, "999 subjects")
  expect_match(msg, "30")
  msg_rows <- band_empty_reason(d, "day", "val", min_n = 99999)
  expect_match(msg_rows, "rows")
})
