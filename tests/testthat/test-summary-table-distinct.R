# Regression tests for the `na.rm = TRUE` fix in distinct-subject counting.
# n_distinct_in() and compute_denom() must IGNORE NA values in the id column,
# so that an NA subject id is never counted as a distinct subject.

test_that("n_distinct_in() ignores NA in the subject column", {
  df <- data.frame(
    USUBJID = c("S1", "S1", "S2", NA, NA),
    val     = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  # 2 real subjects (S1, S2); the NA rows must NOT add to the count.
  expect_identical(
    blockr.viz:::n_distinct_in(df, "USUBJID"),
    2L
  )
})

test_that("n_distinct_in() falls back to nrow() without a subject var", {
  df <- data.frame(x = c(1, 2, NA, 4))
  expect_identical(blockr.viz:::n_distinct_in(df, NULL), 4L)
  expect_identical(blockr.viz:::n_distinct_in(df, "not_a_col"), 4L)
})

test_that("compute_denom() ignores NA subjects for the overall N", {
  df <- data.frame(
    USUBJID = c("S1", "S1", "S2", NA),
    stringsAsFactors = FALSE
  )

  denom <- blockr.viz:::compute_denom(df, character(), "USUBJID")
  expect_identical(denom$N, 2L)
})

test_that("compute_denom() counts distinct subjects per group", {
  df <- data.frame(
    USUBJID = c("S1", "S1", "S2", "S3"),
    ARM     = c("A",  "A",  "A",  "B"),
    stringsAsFactors = FALSE
  )

  denom <- blockr.viz:::compute_denom(df, "ARM", "USUBJID")
  # Arm A has 2 distinct subjects (S1, S2); Arm B has 1 (S3).
  expect_identical(denom$N[denom$ARM == "A"], 2L)
  expect_identical(denom$N[denom$ARM == "B"], 1L)
})

test_that("summary_table() distinct-subject denominator drops NA ids end-to-end", {
  # 4 rows are MILD/SEV across 3 distinct subjects (S1, S2, S3) plus 2 rows
  # with an NA subject id. The percentage denominator is the distinct-subject
  # count: with the NA dropped N = 3, so MILD = 2/3 = 66.7%. If the NA were
  # (wrongly) counted, N = 4 and MILD would be 50%.
  df <- data.frame(
    USUBJID = c("S1", "S1", "S2", "S3", NA, NA),
    AESEV   = c("MILD", "MILD", "SEV", "MILD", "MILD", "SEV"),
    stringsAsFactors = FALSE
  )

  out <- summary_table(
    df,
    vars        = "AESEV",
    subject_var = "USUBJID"
  )

  expect_s3_class(out, "data.frame")
  mild_pct <- out$pct[out$.label == "MILD"]
  expect_equal(mild_pct, 200 / 3, tolerance = 1e-6)
})
