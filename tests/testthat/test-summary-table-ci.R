# The two CI stats exist to reproduce old CDEx's rows, which are
# `tern::analyze_vars()` output. tern is the reference implementation, so it is
# the oracle here rather than hand-written expected values -- if tern changes a
# default, this goes red and we decide, instead of drifting silently.
#
# tern stays in Suggests: it is `Depends: rtables` plus ~90 recursive
# dependencies, far too much weight to carry at runtime for the ~15 lines of
# arithmetic in `ci_mean()` / `ci_median()`.

test_that("ci_mean() matches tern::stat_mean_ci()", {
  skip_if_not_installed("tern")
  for (x in list(mtcars$mpg, mtcars$hp, c(1, 2, 3, 4, 5, 6, 7), c(2.5, 9.75))) {
    expect_equal(
      unname(ci_mean(x)),
      unname(unlist(tern::stat_mean_ci(x, gg_helper = FALSE))),
      tolerance = 1e-12
    )
  }
})

test_that("ci_median() matches tern::stat_median_ci()", {
  skip_if_not_installed("tern")
  for (x in list(mtcars$mpg, mtcars$hp, as.numeric(1:6), as.numeric(1:9))) {
    # as.numeric() drops tern's `conf_level` attribute (the empirical coverage,
    # which we deliberately do not carry) -- the bounds are what must match.
    expect_equal(
      ci_median(x),
      as.numeric(unlist(tern::stat_median_ci(x, gg_helper = FALSE))),
      tolerance = 1e-12
    )
  }
})

test_that("the mean CI is a t-interval, not 1.96 * se", {
  x <- as.numeric(1:10)
  hci <- stats::qt(0.975, df = 9) * stats::sd(x) / sqrt(10)
  expect_equal(ci_mean(x), mean(x) + c(-hci, hci))
  # The normal approximation is narrower; at n = 10 the difference shows well
  # above rounding, so a 1.96 implementation could not pass by accident.
  expect_gt(diff(ci_mean(x)), 2 * 1.96 * stats::sd(x) / sqrt(10))
})

test_that("the mean CI is undefined below 2 observations", {
  expect_identical(ci_mean(3), c(NA_real_, NA_real_))
  expect_identical(ci_mean(numeric()), c(NA_real_, NA_real_))
  expect_false(anyNA(ci_mean(c(3, 4))))
})

test_that("the median CI is undefined below 6 observations", {
  # qbinom(0.025, n, 0.5) is 0 below six, so no pair of order statistics
  # attains the level. tern returns NA there and so do we -- the blank cell in
  # a small subgroup is correct, not missing data.
  for (n in 1:5) {
    expect_identical(ci_median(as.numeric(seq_len(n))), c(NA_real_, NA_real_))
  }
  expect_identical(ci_median(as.numeric(1:6)), c(1, 6))
})

test_that("the median CI bounds are observed values", {
  x <- c(4.2, 1.1, 9.9, 3.3, 7.7, 2.2, 8.8, 5.5)
  expect_true(all(ci_median(x) %in% x))
})

test_that("both CIs ignore NAs", {
  x <- c(1, 2, NA, 4, NA, 6, 7, 8)
  expect_identical(ci_mean(x), ci_mean(x[!is.na(x)]))
  expect_identical(ci_median(x), ci_median(x[!is.na(x)]))
})

test_that("CI stats emit tern's labels and two-decimal cells", {
  out <- summary_table_long(
    mtcars, vars = "mpg", stats = c("mean_ci", "median_ci")
  )
  expect_identical(out$.label, c("Mean 95% CI", "Median 95% CI"))
  expect_identical(
    out$.fmt,
    c("({mean_ci_lwr:2}, {mean_ci_upr:2})",
      "({median_ci_lwr:2}, {median_ci_upr:2})")
  )
  # The rest of the table is one-decimal; the CI rows are two. That asymmetry
  # is tern's, and matching it is the point.
  wide <- summary_table(mtcars, vars = "mpg", stats = c("mean_ci", "median_ci"))
  expect_match(wide$Overall[[1]], "^\\([0-9]+\\.[0-9]{2}, [0-9]+\\.[0-9]{2}\\)$")
})

test_that("an undefined CI renders blank, not \"(NA, NA)\"", {
  # Five rows per group: below the median CI's six-observation floor.
  df <- data.frame(x = as.numeric(1:10), g = rep(c("a", "b"), each = 5))
  wide <- summary_table(df, vars = "x", by = "g", stats = "median_ci")
  expect_true(all(is.na(wide$a) | !nzchar(wide$a)))
  expect_true(all(is.na(wide$b) | !nzchar(wide$b)))
})

test_that("quartiles use tern's quantile type, not R's default", {
  skip_if_not_installed("tern")
  ctrl <- tern::control_analyze_vars()
  expect_identical(ctrl$quantile_type, 2)
  for (x in list(mtcars$mpg, mtcars$hp, mtcars$wt)) {
    expect_equal(
      c(quantile_tern(x, 0.25), quantile_tern(x, 0.75)),
      unname(stats::quantile(x, c(0.25, 0.75), type = ctrl$quantile_type))
    )
  }
  # Not a distinction without a difference: on mpg the two types disagree in
  # the first decimal, which is what a side-by-side comparison would flag.
  expect_false(
    isTRUE(all.equal(
      quantile_tern(mtcars$mpg, 0.25),
      unname(stats::quantile(mtcars$mpg, 0.25))
    ))
  )
})

test_that("every rendered cell matches tern cell for cell", {
  skip_if_not_installed("tern")
  d <- data.frame(AVAL = mtcars$mpg, ARM = "A")
  ours <- summary_table(
    d, vars = "AVAL", by = "ARM",
    stats = c("mean_sd", "mean_ci", "median", "median_ci", "q1_q3", "min_max")
  )
  lyt <- rtables::basic_table()
  lyt <- rtables::split_cols_by(lyt, "ARM")
  lyt <- tern::analyze_vars(
    lyt, "AVAL",
    .stats = c("mean_sd", "mean_ci", "median", "median_ci", "quantiles", "range")
  )
  theirs <- rtables::build_table(lyt, d)
  # The rendered strings, not the raw numbers: the formats are half of what
  # "matches old CDEx" means.
  cells <- formatters::matrix_form(theirs)$strings
  cells <- trimws(cells[-seq_len(nrow(cells) - nrow(ours)), 2])
  expect_identical(as.character(ours$A), cells)
})

test_that("the tern-matching stat set reproduces old CDEx's row labels", {
  # The set old CDEx selects (spec_CA_244_0001_app.R), in tern's order.
  out <- summary_table_long(
    mtcars, vars = "mpg",
    stats = c("mean_sd", "mean_ci", "median", "median_ci", "q1_q3", "min_max")
  )
  expect_identical(
    out$.label,
    c("Mean (SD)", "Mean 95% CI", "Median", "Median 95% CI",
      "25% and 75%-ile", "Min - Max")
  )
})
