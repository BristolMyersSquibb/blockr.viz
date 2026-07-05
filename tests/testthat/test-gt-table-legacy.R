# Characterization tests for gt_table()'s legacy long-format branch
# (gt_table_legacy / prepare_table_wide), preserved for back-compat with
# blockr.sandbox's tidy_summary_block / occurrence_summary_block output.
# These pin the branch's CURRENT behavior -- the two producer stat shapes it
# hard-codes (`n (pct)` and `mean (sd)` sprintf formats), the (N=...) column
# labels, and the depth-driven bold/indent styling -- so future refactors
# cannot silently break saved boards. The branch is deprecated (see the
# rlang::warn() nudge), not removed.

# occurrence_summary_block shape: n / N / pct per (label, col_var) cell,
# ordered so each consecutive n_cols rows form one display row.
legacy_npct <- data.frame(
  label   = rep(c("Any AE", "Headache"), each = 2),
  depth   = rep(c(0L, 1L), each = 2),
  col_var = rep(c("Placebo", "Drug"), 2),
  n       = c(10L, 12L, 4L, 6L),
  N       = c(20L, 24L, 20L, 24L),
  pct     = c(50, 50, 20, 25),
  stringsAsFactors = FALSE
)

# tidy_summary_block continuous shape: mean / sd per cell, with the
# denominator N taken from the depth-0 rows' n.
legacy_meansd <- data.frame(
  label   = rep(c("Age", "Weight"), each = 2),
  depth   = rep(c(0L, 1L), each = 2),
  col_var = rep(c("Placebo", "Drug"), 2),
  mean    = c(45.2, 47.8, 71.5, 69.9),
  sd      = c(8.1, 9.3, 12.4, 11.8),
  n       = c(20L, 24L, 20L, 24L),
  stringsAsFactors = FALSE
)

test_that("legacy n/pct shape: prepare_table_wide output is pinned", {
  prep <- prepare_table_wide(legacy_npct)

  expect_false(prep$has_nesting)
  expect_identical(prep$col_names, c("Placebo", "Drug"))
  expect_identical(
    as.data.frame(prep$denom_map),
    data.frame(col_var = c("Placebo", "Drug"), N = c(20L, 24L),
               stringsAsFactors = FALSE),
    ignore_attr = TRUE
  )
  # The hard-coded "n (pct)" cell format: sprintf("%d (%0.1f%%)", n, pct).
  expect_identical(
    as.data.frame(prep$wide),
    data.frame(
      label   = c("Any AE", "Headache"),
      depth   = c(0L, 1L),
      Placebo = c("10 (50.0%)", "4 (20.0%)"),
      Drug    = c("12 (50.0%)", "6 (25.0%)"),
      stringsAsFactors = FALSE
    )
  )
})

test_that("legacy mean/sd shape: prepare_table_wide output is pinned", {
  prep <- prepare_table_wide(legacy_meansd)

  expect_false(prep$has_nesting)
  # Denominators come from the depth-0 rows' n.
  expect_identical(
    as.data.frame(prep$denom_map),
    data.frame(col_var = c("Placebo", "Drug"), N = c(20L, 24L),
               stringsAsFactors = FALSE),
    ignore_attr = TRUE
  )
  # The hard-coded "mean (sd)" cell format: sprintf("%.1f (%.1f)", mean, sd).
  expect_identical(
    as.data.frame(prep$wide),
    data.frame(
      label   = c("Age", "Weight"),
      depth   = c(0L, 1L),
      Placebo = c("45.2 (8.1)", "71.5 (12.4)"),
      Drug    = c("47.8 (9.3)", "69.9 (11.8)"),
      stringsAsFactors = FALSE
    )
  )
})

test_that("legacy render: (N=...) column labels + depth styling are pinned", {
  html <- suppressWarnings(
    as.character(gt::as_raw_html(gt_table_legacy(legacy_npct, title = "AEs")))
  )

  # Denominator column labels.
  expect_match(html, "Placebo (N=20)", fixed = TRUE)
  expect_match(html, "Drug (N=24)", fixed = TRUE)
  # The formatted cells.
  expect_match(html, "10 (50.0%)", fixed = TRUE)
  expect_match(html, "6 (25.0%)", fixed = TRUE)
  # Title.
  expect_match(html, "AEs", fixed = TRUE)

  # depth < max_depth rows are bold; depth == max_depth label cells indent.
  body_rows <- regmatches(
    html, gregexpr("(?s)<tr[^>]*>.*?</tr>", html, perl = TRUE)
  )[[1]]
  any_ae   <- grep("Any AE", body_rows, value = TRUE)
  headache <- grep("Headache", body_rows, value = TRUE)
  expect_match(any_ae, "font-weight: bold")
  expect_match(headache, "text-indent: 20px")
  expect_no_match(headache, "font-weight: bold")
})

test_that("gt_table() still routes label/depth + stat frames to the legacy branch", {
  html <- suppressWarnings(
    as.character(gt::as_raw_html(gt_table(legacy_meansd)))
  )
  expect_match(html, "45.2 (8.1)", fixed = TRUE)
  expect_match(html, "Placebo (N=20)", fixed = TRUE)
})

test_that("legacy branch signals its deprecation", {
  # rlang's once-per-session frequency always fires under testthat, but
  # reset explicitly so the assertion holds when run interactively too.
  rlang::reset_warning_verbosity("blockr.viz_gt_table_legacy")
  expect_warning(
    gt_table_legacy(legacy_npct),
    "legacy long-format input to gt_table\\(\\).*deprecated"
  )
  # A frame without the legacy stat columns still fails with the pinned
  # producer-shape error (reached at render time, not dat_valid).
  bad <- data.frame(label = "x", depth = 0L, col_var = "a",
                    stringsAsFactors = FALSE)
  expect_error(
    suppressWarnings(gt_table_legacy(bad)),
    "stat columns \\(n/N/pct or mean/sd\\)"
  )
})
