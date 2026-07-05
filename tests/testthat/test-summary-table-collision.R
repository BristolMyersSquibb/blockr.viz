# Regression tests for the `.var` row-identity fix in the display pivot.
# Two variables routinely share level labels ("OTHER", "UNKNOWN", "MISSING"
# in pharma data); without a source-variable column in the pivot id the
# rows merged into list-cols and one variable's level row vanished.

test_that("shared level labels across vars keep their own rows (no pivot collision)", {
  d <- data.frame(
    sex  = c("F", "M", "OTHER", "OTHER"),
    race = c("WHITE", "ASIAN", "OTHER", "OTHER"),
    stringsAsFactors = FALSE
  )

  expect_no_warning(wide <- summary_table(d, vars = c("sex", "race")))

  # 2 header rows + 3 sex levels + 3 race levels.
  expect_identical(nrow(wide), 8L)
  expect_false(any(vapply(wide, is.list, logical(1))))
  # The plumbing column never reaches the wide output.
  expect_false(".var" %in% names(wide))

  # Each variable section carries its own OTHER row with its own cell.
  hdr <- which(!is.na(wide$.strong) & wide$.strong)
  expect_identical(wide$.label[hdr], c("sex", "race"))
  sex_rows <- wide$.label[(hdr[1] + 1):(hdr[2] - 1)]
  race_rows <- wide$.label[(hdr[2] + 1):nrow(wide)]
  expect_true("OTHER" %in% sex_rows)
  expect_true("OTHER" %in% race_rows)
  expect_identical(sum(wide$.label == "OTHER"), 2L)
  expect_identical(wide$Overall[wide$.label == "OTHER"], rep("2 (50.0%)", 2))
})

test_that("shared level labels survive a grouped (spread) summary", {
  d <- data.frame(
    sex  = c("F", "M", "OTHER", "OTHER"),
    race = c("WHITE", "ASIAN", "OTHER", "OTHER"),
    grp  = c("A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )

  expect_no_warning(wide <- summary_table(d, vars = c("sex", "race"), by = "grp"))

  expect_identical(nrow(wide), 8L)
  expect_false(any(vapply(wide, is.list, logical(1))))
  expect_false(".var" %in% names(wide))
  # One OTHER row per variable section, each spread across both groups.
  expect_identical(sum(wide$.label == "OTHER"), 2L)
  expect_identical(wide$A[wide$.label == "OTHER"], rep("1 (50.0%)", 2))
  expect_identical(wide$B[wide$.label == "OTHER"], rep("1 (50.0%)", 2))
})

test_that("multi-stat numeric vars no longer collide on shared stat labels", {
  # The docs' own example: mpg and hp both emit "n (%)" / "Median (Q1, Q3)" /
  # "Min, Max" rows, which used to merge into list-cols.
  expect_no_warning(
    wide <- summary_table(
      mtcars,
      vars = c("mpg", "hp"),
      by = "cyl",
      stats = c("n_pct", "median_q1_q3", "min_max"),
      add_overall = TRUE
    )
  )
  expect_identical(nrow(wide), 8L)
  expect_false(any(vapply(wide, is.list, logical(1))))
})

test_that("non-colliding output keeps its shape, order and cells", {
  wide <- summary_table(iris, vars = c("Sepal.Length", "Species"),
                        stats = "mean_sd")

  expect_identical(names(wide), c(".label", ".indent", ".strong", "Overall"))
  expect_identical(
    wide$.label,
    c("Sepal.Length", "Mean (SD)", "Species", "setosa", "versicolor",
      "virginica")
  )
  expect_identical(wide$.indent, c(0L, 1L, 0L, 1L, 1L, 1L))
  expect_identical(wide$Overall[2], "5.8 (0.83)")
  expect_identical(wide$Overall[4], "50 (33.3%)")
})

test_that("an input `.var` column is rejected as block-internal", {
  d <- data.frame(x = 1:3, .var = c("a", "b", "c"), check.names = FALSE)
  expect_error(
    summary_table(d, vars = "x"),
    "block-internal namespace"
  )
})
