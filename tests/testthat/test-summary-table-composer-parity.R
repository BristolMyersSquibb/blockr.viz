# `composer` is the standard for clinical table numbers, and this catalog is held
# at composer's vocabulary rather than above it. These tests pin the three places
# that has been got wrong before -- see
# blockr.sandbox/dev/summary-statistics-conventions.md.

test_that("SD renders at one decimal, as composer formats it", {
  # composer's continuous blocks use "{mean:xx.x} ({sd:xx.x})". Ours carried a
  # two-decimal SD, which made an otherwise identical table read as different.
  expect_identical(SUMMARY_STATS_CATALOG$sd$fmt, "{sd:1}")
  expect_identical(SUMMARY_STATS_CATALOG$mean_sd$fmt, "{mean:1} ({sd:1})")
  out <- summary_table(iris, vars = "Sepal.Length", stats = "mean_sd")
  expect_identical(out$Overall[[1]], "5.8 (0.8)")
})

test_that("quartiles use R's default quantile type, as composer does", {
  # composer's registry is q1 = \(x) unname(quantile(x, 0.25, na.rm = TRUE)),
  # i.e. type 7. tern uses type 2 and disagrees in the first decimal; that
  # difference from old CDEx is expected and is not a defect.
  out <- summary_table_long(mtcars, vars = "mpg", stats = "q1_q3")
  expect_equal(out$q1, unname(stats::quantile(mtcars$mpg, 0.25)))
  expect_false(
    isTRUE(all.equal(out$q1, unname(stats::quantile(mtcars$mpg, 0.25, type = 2))))
  )
})

test_that("the catalog carries no statistic composer lacks", {
  # composer's stat registry has no confidence interval, so neither do we: a
  # statistic the standard does not carry is one we do not offer. Adding one
  # here means adding it to composer first.
  expect_false(any(grepl("_ci", names(SUMMARY_STATS_CATALOG))))
  expect_identical(
    names(SUMMARY_STATS_CATALOG),
    c("n", "n_pct", "mean", "sd", "mean_sd", "median", "median_q1_q3",
      "q1_q3", "min_max")
  )
})

test_that("the row labels are composer's", {
  labs <- vapply(SUMMARY_STATS_CATALOG, `[[`, "", "label", USE.NAMES = FALSE)
  expect_true(all(c("Q1, Q3", "Min, Max", "Median (Q1, Q3)") %in% labs))
  expect_false(any(grepl("75%-ile|Min - Max", labs)))
})
