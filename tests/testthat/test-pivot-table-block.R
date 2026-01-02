# Tests for Pivot Table Block

# ==============================================================================
# Constructor Tests
# ==============================================================================

test_that("pivot_table_block constructor creates correct class", {
  blk <- new_pivot_table_block()
  expect_s3_class(blk, c("pivot_table_block", "transform_block", "block"))
})

test_that("pivot_table_block constructor accepts all arguments", {
  blk <- new_pivot_table_block(
    rows = c("Region", "Country"),
    cols = "Category",
    measures = c("Revenue", "Profit"),
    agg_fun = "mean",
    digits = "2"
  )
  expect_s3_class(blk, c("pivot_table_block", "transform_block", "block"))
})

test_that("pivot_table_block constructor with single row and col", {
  blk <- new_pivot_table_block(
    rows = "Region",
    cols = "Category",
    measures = "Revenue"
  )
  expect_s3_class(blk, c("pivot_table_block", "transform_block", "block"))
})

test_that("pivot_table_block constructor with empty dimensions", {
  blk <- new_pivot_table_block(
    rows = character(),
    cols = character(),
    measures = "Revenue"
  )
  expect_s3_class(blk, c("pivot_table_block", "transform_block", "block"))
})

# ==============================================================================
# State Tests
# ==============================================================================

test_that("pivot_table_block state contains correct fields", {
  block <- new_pivot_table_block(
    rows = c("Region"),
    cols = "Category",
    measures = "Revenue",
    agg_fun = "sum",
    digits = "0"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      returned <- session$returned
      expect_type(returned, "list")
      expect_true("state" %in% names(returned))
      expect_true("expr" %in% names(returned))

      # Check state has all expected fields
      state <- returned$state
      expect_true("rows" %in% names(state))
      expect_true("cols" %in% names(state))
      expect_true("measures" %in% names(state))
      expect_true("agg_fun" %in% names(state))
      expect_true("digits" %in% names(state))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("pivot_table_block state values are accessible reactives", {
  block <- new_pivot_table_block(
    rows = c("Region", "Country"),
    cols = "Category",
    measures = c("Revenue", "Profit"),
    agg_fun = "mean",
    digits = "2"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state

      # State should have the expected reactive values (functions)
      expect_true(is.function(state$rows))
      expect_true(is.function(state$cols))
      expect_true(is.function(state$measures))
      expect_true(is.function(state$agg_fun))
      expect_true(is.function(state$digits))

      # agg_fun should match since it's initialized directly
      expect_equal(state$agg_fun(), "mean")
      expect_equal(state$digits(), "2")
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

# ==============================================================================
# Expression Generation Tests (using build_pivot_expr directly)
# ==============================================================================

test_that("build_pivot_expr generates dplyr expression", {
  expr <- blockr.bi:::build_pivot_expr(
    rows = "Region",
    cols = character(),
    measures = "Revenue",
    agg_fun = "sum"
  )
  expr_text <- deparse(expr)

  # Should use dplyr
  expect_true(any(grepl("dplyr", expr_text)))
})

test_that("build_pivot_expr generates summarise for aggregation", {
  expr <- blockr.bi:::build_pivot_expr(
    rows = "Region",
    cols = character(),
    measures = "Revenue",
    agg_fun = "sum"
  )
  expr_text <- deparse(expr)

  expect_true(any(grepl("summarise", expr_text)))
  expect_true(any(grepl("sum", expr_text)))
})

test_that("build_pivot_expr generates pivot_wider when cols specified", {
  expr <- blockr.bi:::build_pivot_expr(
    rows = "Region",
    cols = "Category",
    measures = "Revenue",
    agg_fun = "sum"
  )
  expr_text <- deparse(expr)

  expect_true(any(grepl("pivot_wider", expr_text)))
})

test_that("build_pivot_expr handles count aggregation", {
  expr <- blockr.bi:::build_pivot_expr(
    rows = "Region",
    cols = character(),
    measures = "Revenue",
    agg_fun = "n"
  )
  expr_text <- deparse(expr)

  expect_true(any(grepl("dplyr::n", expr_text)))
})

# ==============================================================================
# Result Tests - Simple Aggregation (No Pivot)
# ==============================================================================

test_that("pivot_table_block returns grouped sum without pivot", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_pivot_expr(
    rows = "Region",
    cols = character(),
    measures = "Revenue",
    agg_fun = "sum"
  )
  result <- eval(expr, list(data = demo_data))

  expect_true(is.data.frame(result))
  expect_true("Region" %in% names(result))
  expect_true("Revenue" %in% names(result))

  # Should have one row per region
  expect_equal(nrow(result), length(unique(demo_data$Region)))

  # Check sum is correct for one region
  expected_sum <- sum(demo_data$Revenue[demo_data$Region == "Western Europe"])
  actual_sum <- result$Revenue[result$Region == "Western Europe"]
  expect_equal(actual_sum, expected_sum)
})

test_that("pivot_table_block returns multiple measures as columns", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_pivot_expr(
    rows = "Region",
    cols = character(),
    measures = c("Revenue", "Profit"),
    agg_fun = "sum"
  )
  result <- eval(expr, list(data = demo_data))

  expect_true(is.data.frame(result))
  expect_true("Region" %in% names(result))
  expect_true("Revenue" %in% names(result))
  expect_true("Profit" %in% names(result))
})

# ==============================================================================
# Result Tests - Pivot to Wide Format
# ==============================================================================

test_that("pivot_table_block pivots single measure by column dimension", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_pivot_expr(
    rows = "Region",
    cols = "Category",
    measures = "Revenue",
    agg_fun = "sum"
  )
  result <- eval(expr, list(data = demo_data))

  expect_true(is.data.frame(result))
  expect_true("Region" %in% names(result))

  # Categories should become column names
  categories <- unique(demo_data$Category)
  for (cat in categories) {
    expect_true(cat %in% names(result), info = paste("Missing column:", cat))
  }
})

test_that("pivot_table_block handles multiple measures with column dimension", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_pivot_expr(
    rows = "Region",
    cols = "Category",
    measures = c("Revenue", "Profit"),
    agg_fun = "sum"
  )
  result <- eval(expr, list(data = demo_data))

  expect_true(is.data.frame(result))
  expect_true("Region" %in% names(result))

  # Should have nested columns with " | " separator
  col_names <- names(result)
  expect_true(any(grepl("\\|", col_names)))
})

# ==============================================================================
# Aggregation Function Tests (using build_pivot_expr directly)
# ==============================================================================

test_that("pivot_table_block sum aggregation works", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_pivot_expr(
    rows = "Region",
    cols = character(),
    measures = "Revenue",
    agg_fun = "sum"
  )
  result <- eval(expr, list(data = demo_data))

  expected_sum <- sum(demo_data$Revenue[demo_data$Region == "Western Europe"])
  actual_sum <- result$Revenue[result$Region == "Western Europe"]
  expect_equal(actual_sum, expected_sum)
})

test_that("pivot_table_block mean aggregation works", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_pivot_expr(
    rows = "Region",
    cols = character(),
    measures = "Revenue",
    agg_fun = "mean"
  )
  result <- eval(expr, list(data = demo_data))

  # Check mean is correct for one region
  expected_mean <- mean(demo_data$Revenue[demo_data$Region == "Western Europe"])
  actual_mean <- result$Revenue[result$Region == "Western Europe"]
  expect_equal(actual_mean, expected_mean, tolerance = 0.01)
})

test_that("pivot_table_block count (n) aggregation works", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_pivot_expr(
    rows = "Region",
    cols = character(),
    measures = "Revenue",
    agg_fun = "n"
  )
  result <- eval(expr, list(data = demo_data))

  expect_true("Count" %in% names(result))

  # Check count is correct for one region
  expected_count <- sum(demo_data$Region == "Western Europe")
  actual_count <- result$Count[result$Region == "Western Europe"]
  expect_equal(actual_count, expected_count)
})

test_that("pivot_table_block min aggregation works", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_pivot_expr(
    rows = "Region",
    cols = character(),
    measures = "Revenue",
    agg_fun = "min"
  )
  result <- eval(expr, list(data = demo_data))

  expected_min <- min(demo_data$Revenue[demo_data$Region == "Western Europe"])
  actual_min <- result$Revenue[result$Region == "Western Europe"]
  expect_equal(actual_min, expected_min)
})

test_that("pivot_table_block max aggregation works", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_pivot_expr(
    rows = "Region",
    cols = character(),
    measures = "Revenue",
    agg_fun = "max"
  )
  result <- eval(expr, list(data = demo_data))

  expected_max <- max(demo_data$Revenue[demo_data$Region == "Western Europe"])
  actual_max <- result$Revenue[result$Region == "Western Europe"]
  expect_equal(actual_max, expected_max)
})

test_that("pivot_table_block median aggregation works", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_pivot_expr(
    rows = "Region",
    cols = character(),
    measures = "Revenue",
    agg_fun = "median"
  )
  result <- eval(expr, list(data = demo_data))

  expected_median <- median(demo_data$Revenue[demo_data$Region == "Western Europe"])
  actual_median <- result$Revenue[result$Region == "Western Europe"]
  expect_equal(actual_median, expected_median)
})

# ==============================================================================
# Digits/Rounding Tests
# ==============================================================================

test_that("pivot_table_block respects digits parameter", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_pivot_expr(
    rows = "Region",
    cols = character(),
    measures = "Revenue",
    agg_fun = "mean",
    digits = 0
  )
  result <- eval(expr, list(data = demo_data))

  # Values should be rounded to 0 decimal places (whole numbers)
  expect_true(all(result$Revenue == round(result$Revenue, 0)))
})

test_that("pivot_table_block NULL digits means no rounding", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_pivot_expr(
    rows = "Region",
    cols = character(),
    measures = "Revenue",
    agg_fun = "mean",
    digits = NULL
  )
  result <- eval(expr, list(data = demo_data))

  expect_true(is.data.frame(result))
  expect_true("Revenue" %in% names(result))
})

# ==============================================================================
# Multiple Row Dimensions Tests
# ==============================================================================

test_that("pivot_table_block handles multiple row dimensions", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_pivot_expr(
    rows = c("Region", "Category"),
    cols = character(),
    measures = "Revenue",
    agg_fun = "sum"
  )
  result <- eval(expr, list(data = demo_data))

  expect_true("Region" %in% names(result))
  expect_true("Category" %in% names(result))
  expect_true("Revenue" %in% names(result))

  # Should have rows for each combination
  expected_rows <- nrow(unique(demo_data[, c("Region", "Category")]))
  expect_equal(nrow(result), expected_rows)
})

# ==============================================================================
# Edge Case Tests
# ==============================================================================

test_that("pivot_table_block handles no grouping (total only)", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_pivot_expr(
    rows = character(),
    cols = character(),
    measures = "Revenue",
    agg_fun = "sum"
  )
  result <- eval(expr, list(data = demo_data))

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
  expect_equal(result$Revenue, sum(demo_data$Revenue))
})

test_that("pivot_table_block handles data with NA values", {
  test_data <- data.frame(
    group = c("A", "A", "B", "B"),
    value = c(10, NA, 20, 30)
  )

  expr <- blockr.bi:::build_pivot_expr(
    rows = "group",
    cols = character(),
    measures = "value",
    agg_fun = "sum"
  )
  result <- eval(expr, list(data = test_data))

  # Should handle NAs (na.rm = TRUE)
  expect_equal(result$value[result$group == "A"], 10)
  expect_equal(result$value[result$group == "B"], 50)
})

test_that("pivot_table_block handles single row data", {
  test_data <- data.frame(group = "A", value = 42)

  expr <- blockr.bi:::build_pivot_expr(
    rows = "group",
    cols = character(),
    measures = "value",
    agg_fun = "sum"
  )
  result <- eval(expr, list(data = test_data))

  expect_equal(nrow(result), 1)
  expect_equal(result$value, 42)
})

# ==============================================================================
# Block Server Integration Tests
# ==============================================================================

test_that("pivot_table_block server initializes without error", {
  block <- new_pivot_table_block()
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # The block should initialize without error
      expect_type(session$returned, "list")
      expect_true("state" %in% names(session$returned))
      expect_true("expr" %in% names(session$returned))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

# ==============================================================================
# UI Tests
# ==============================================================================

test_that("pivot_table_block UI contains expected elements", {
  blk <- new_pivot_table_block(
    rows = "Region",
    cols = "Category",
    measures = "Revenue"
  )

  ui_output <- blk$expr_ui("test_id")

  expect_s3_class(ui_output, "shiny.tag.list")
  ui_text <- as.character(ui_output)

  # Should contain selectize inputs
  expect_true(grepl("selectize", ui_text, ignore.case = TRUE))

  # Should have rows, cols, measures inputs
  expect_true(grepl("rows", ui_text))
  expect_true(grepl("cols", ui_text))
  expect_true(grepl("measures", ui_text))

  # Should have aggregation selector
  expect_true(grepl("agg_fun", ui_text))

  # Should have digits input
  expect_true(grepl("digits", ui_text))
})
