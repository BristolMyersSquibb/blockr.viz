# The `stats` argument: catalog-key selection for numeric variables,
# legacy "compact"/"expanded" preset mapping, canonical ordering, and
# the n_pct share-of-non-missing semantics.
#
# These exercise the row-shaping contract (raw numeric columns + `.fmt`
# templates + `.group`), which is the internal `summary_table_long()`. The
# public `summary_table()` wraps it with `fmt_to_wide()` and is covered by the
# wide-output test below plus the renderer tests in test-html-table.R.

test_that("legacy presets map to catalog keys", {
  compact <- summary_table_long(mtcars, vars = "mpg", stats = "compact")
  mean_sd <- summary_table_long(mtcars, vars = "mpg", stats = "mean_sd")
  expect_identical(compact, mean_sd)

  expanded <- summary_table_long(mtcars, vars = "mpg", stats = "expanded")
  keys <- summary_table_long(
    mtcars, vars = "mpg",
    stats = c("n", "mean", "sd", "median", "q1_q3", "min_max")
  )
  expect_identical(expanded, keys)
  expect_identical(
    keys$.label,
    c("N", "Mean", "SD", "Median", "Q1, Q3", "Min, Max")
  )
})

test_that("a single stat key emits one un-indented row per variable", {
  out <- summary_table_long(mtcars, vars = "mpg", stats = "median_q1_q3")
  expect_identical(nrow(out), 1L)
  expect_identical(out$.label, "Median (Q1, Q3)")
  expect_identical(out$.indent, 0L)
  expect_identical(out$.fmt, "{median:1} ({q1:1}, {q3:1})")
})

test_that("several stat keys emit one indented row each", {
  out <- summary_table_long(mtcars, vars = "mpg", stats = c("n_pct", "mean_sd"))
  expect_identical(out$.label, c("n (%)", "Mean (SD)"))
  expect_identical(out$.indent, c(1L, 1L))
})

test_that("selection is reordered to canonical catalog order", {
  fwd <- summary_table_long(mtcars, vars = "mpg", stats = c("n", "min_max"))
  rev <- summary_table_long(mtcars, vars = "mpg", stats = c("min_max", "n"))
  expect_identical(fwd, rev)
  expect_identical(fwd$.label, c("N", "Min, Max"))
})

test_that("n_pct is the share of non-missing rows per group", {
  df <- data.frame(
    x = c(1, 2, NA, 4, NA, 6),
    g = rep(c("a", "b"), each = 3)
  )
  out <- summary_table_long(df, vars = "x", by = "g", stats = "n_pct")
  expect_identical(out$.fmt, rep("{n:0} ({pct:1}%)", 2))
  expect_identical(out$n[out$.group == "a"], 2L)
  expect_equal(out$pct[out$.group == "a"], 2 / 3 * 100)
  expect_equal(out$pct[out$.group == "b"], 2 / 3 * 100)
})

test_that("categorical and logical variables are unaffected by `stats`", {
  df <- data.frame(sex = c("F", "M", "F"), flag = c(TRUE, FALSE, TRUE))
  a <- summary_table_long(df, vars = c("sex", "flag"), stats = "mean_sd")
  b <- summary_table_long(df, vars = c("sex", "flag"), stats = c("n", "min_max"))
  expect_identical(a, b)
})

test_that("summary_table() returns the wide annotated df with baked cells", {
  wide <- summary_table(mtcars, vars = "mpg", by = "cyl", stats = "mean_sd")
  # No long-form plumbing columns survive the widen.
  expect_false(any(c(".fmt", ".group", "n", "pct") %in% names(wide)))
  # Row-side structure columns do, plus one formatted column per group level.
  expect_true(".label" %in% names(wide))
  expect_setequal(
    setdiff(names(wide), c(".label", ".indent", ".strong")),
    as.character(sort(unique(mtcars$cyl)))
  )
  # Cells are baked strings at the catalog precision, not raw numbers.
  cell <- wide[[as.character(sort(unique(mtcars$cyl))[1])]][1]
  expect_type(cell, "character")
  expect_match(cell, "^[0-9.]+ \\([0-9.]+\\)$")
})

test_that("unknown or empty stats error with the valid vocabulary", {
  expect_error(
    summary_table(mtcars, vars = "mpg", stats = "bogus"),
    "Unknown `stats` value"
  )
  expect_error(
    summary_table(mtcars, vars = "mpg", stats = character()),
    "at least one `stats` key"
  )
})

test_that("block constructor normalizes legacy stats into state", {
  blk <- new_summary_table_block(vars = "mpg", stats = "expanded")
  shiny::testServer(
    blk$expr_server,
    args = list(data = shiny::reactive(mtcars)),
    {
      expect_identical(
        session$returned$state$stats(),
        c("n", "mean", "sd", "median", "q1_q3", "min_max")
      )
      # A widget edit with legacy-free keys lands filtered + canonical.
      session$setInputs(
        summary_input = list(
          vars = list("mpg"),
          stats = list("min_max", "n_pct", "nonsense")
        )
      )
      expect_identical(
        session$returned$state$stats(),
        c("n_pct", "min_max")
      )
    }
  )
})
