# Regression tests for row identity in the display pivot.
# Two variables routinely share level labels ("OTHER", "UNKNOWN", "MISSING"
# in pharma data); without a source-variable column in the pivot id the
# rows merged into list-cols and one variable's level row vanished. Under the
# v2 contract the identity is the `.variable` / `.variable_level` pair, which
# stays in the wide output (it is what a drill filters on); variable headers
# are no longer rows -- the renderer synthesizes them from `.variable_label`
# runs.

test_that("shared level labels across vars keep their own rows (no pivot collision)", {
  d <- data.frame(
    sex  = c("F", "M", "OTHER", "OTHER"),
    race = c("WHITE", "ASIAN", "OTHER", "OTHER"),
    stringsAsFactors = FALSE
  )

  expect_no_warning(wide <- summary_table(d, vars = c("sex", "race")))

  # 3 sex levels + 3 race levels; headers are rendering, not rows.
  expect_identical(nrow(wide), 6L)
  expect_false(any(vapply(wide, is.list, logical(1))))

  # Each variable block carries its own OTHER row with its own cell,
  # distinguished by the machine identity pair.
  expect_identical(wide$.variable_label,
                   c(rep("sex", 3), rep("race", 3)))
  other <- wide$.label == "OTHER"
  expect_identical(sum(other), 2L)
  expect_identical(wide$.variable[other], c("sex", "race"))
  expect_identical(wide$.variable_level[other], c("OTHER", "OTHER"))
  expect_identical(wide$Overall[other], rep("2 (50.0%)", 2))
})

test_that("shared level labels survive a grouped (spread) summary", {
  d <- data.frame(
    sex  = c("F", "M", "OTHER", "OTHER"),
    race = c("WHITE", "ASIAN", "OTHER", "OTHER"),
    grp  = c("A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )

  expect_no_warning(wide <- summary_table(d, vars = c("sex", "race"), by = "grp"))

  expect_identical(nrow(wide), 6L)
  expect_false(any(vapply(wide, is.list, logical(1))))
  # One OTHER row per variable block, each spread across both groups.
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
  # 3 stat rows per variable; the shared labels stay apart via `.variable`.
  expect_identical(nrow(wide), 6L)
  expect_false(any(vapply(wide, is.list, logical(1))))
  expect_identical(sum(wide$.label == "Min, Max"), 2L)
  # Stat rows make no value claim: their `.variable_level` is NA.
  expect_true(all(is.na(wide$.variable_level)))
})

test_that("non-colliding output keeps its shape, order and cells", {
  wide <- summary_table(iris, vars = c("Sepal.Length", "Species"),
                        stats = "mean_sd")

  expect_identical(
    names(wide),
    c(".variable", ".variable_label", ".variable_level", ".label",
      ".indent", "Overall")
  )
  expect_identical(
    wide$.label,
    c("Mean (SD)", "setosa", "versicolor", "virginica")
  )
  expect_identical(wide$.variable_label,
                   c("Sepal.Length", rep("Species", 3)))
  # Absolute indents match what the materialized header rows used to impose.
  expect_identical(wide$.indent, c(1L, 1L, 1L, 1L))
  expect_identical(wide$Overall[1], "5.8 (0.83)")
  expect_identical(wide$Overall[2], "50 (33.3%)")
})

test_that("input columns in the block-internal namespace are rejected", {
  d <- data.frame(x = 1:3, .variable = c("a", "b", "c"), check.names = FALSE)
  expect_error(
    summary_table(d, vars = "x"),
    "block-internal namespace"
  )
  d2 <- data.frame(x = 1:3, .group1_level = c("a", "b", "c"),
                   check.names = FALSE)
  expect_error(
    summary_table(d2, vars = "x"),
    "block-internal namespace"
  )
})
