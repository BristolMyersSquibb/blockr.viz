# Tests for Aggregate Block

# ==============================================================================
# Constructor Tests
# ==============================================================================

test_that("aggregate_block constructor creates correct class", {
  blk <- new_aggregate_block()
  expect_s3_class(blk, c("aggregate_block", "transform_block", "block"))
})

test_that("aggregate_block constructor accepts all arguments", {
  blk <- new_aggregate_block(
    drill_down = c("Region", "Category"),
    values = c("Revenue", "Profit"),
    agg_fun = "mean"
  )
  expect_s3_class(blk, c("aggregate_block", "transform_block", "block"))
})

test_that("aggregate_block constructor with single drill_down", {
  blk <- new_aggregate_block(drill_down = "Region")
  expect_s3_class(blk, c("aggregate_block", "transform_block", "block"))
})

test_that("aggregate_block constructor with empty drill_down", {
  blk <- new_aggregate_block(drill_down = character())
  expect_s3_class(blk, c("aggregate_block", "transform_block", "block"))
})

# ==============================================================================
# State Tests
# ==============================================================================

test_that("aggregate_block state contains correct fields", {
  block <- new_aggregate_block(
    drill_down = c("Region"),
    values = c("Revenue"),
    agg_fun = "sum"
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
      expect_true("drill_down" %in% names(state))
      expect_true("values" %in% names(state))
      expect_true("agg_fun" %in% names(state))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("aggregate_block state values are accessible", {
  block <- new_aggregate_block(
    drill_down = c("Region", "Category"),
    values = c("Revenue", "Profit"),
    agg_fun = "mean"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state

      # State should have the expected reactive values
      expect_true(shiny::is.reactivevalues(state) || is.list(state))

      # agg_fun should match since it's set from constructor directly
      expect_equal(state$agg_fun(), "mean")
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

# ==============================================================================
# Expression Generation Tests (using build_aggregate_expr directly)
# ==============================================================================

test_that("build_aggregate_expr generates dplyr expression with drill_down", {
  expr <- blockr.bi:::build_aggregate_expr(
    drill_down = "Region",
    values = "Revenue",
    agg_fun = "sum"
  )
  expr_text <- deparse(expr)

  expect_true(any(grepl("dplyr", expr_text)))
  expect_true(any(grepl("group_by", expr_text)))
  expect_true(any(grepl("summarise", expr_text)))
  expect_true(any(grepl("Count", expr_text)))
})

test_that("build_aggregate_expr generates expression without drill_down", {
  expr <- blockr.bi:::build_aggregate_expr(
    drill_down = character(),
    values = "Revenue",
    agg_fun = "sum"
  )
  expr_text <- deparse(expr)

  expect_true(any(grepl("summarise", expr_text)))
  expect_true(any(grepl("Count", expr_text)))
})

test_that("build_aggregate_expr handles count aggregation", {
  expr <- blockr.bi:::build_aggregate_expr(
    drill_down = "Region",
    values = "Revenue",
    agg_fun = "n"
  )
  expr_text <- deparse(expr)

  expect_true(any(grepl("dplyr::n", expr_text)))
})

# ==============================================================================
# Result Tests (testing expression evaluation directly)
# ==============================================================================

test_that("aggregate_block expression evaluates correctly for sum", {
  demo_data <- bi_demo_data()

  # Test the expression directly
 expr <- blockr.bi:::build_aggregate_expr(
    drill_down = "Region",
    values = "Revenue",
    agg_fun = "sum"
  )
  result <- eval(expr, list(data = demo_data))

  expect_true(is.data.frame(result))
  expect_true("Region" %in% names(result))
  expect_true("Revenue" %in% names(result))
  expect_true("Count" %in% names(result))

  # Should have one row per region
  expect_equal(nrow(result), length(unique(demo_data$Region)))

  # Check sum is correct
  expected_sum <- sum(demo_data$Revenue[demo_data$Region == "Western Europe"])
  actual_sum <- result$Revenue[result$Region == "Western Europe"]
  expect_equal(actual_sum, expected_sum)
})

test_that("aggregate_block expression includes correct Count", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_aggregate_expr(
    drill_down = "Region",
    values = "Revenue",
    agg_fun = "sum"
  )
  result <- eval(expr, list(data = demo_data))

  expected_count <- sum(demo_data$Region == "Western Europe")
  actual_count <- result$Count[result$Region == "Western Europe"]
  expect_equal(actual_count, expected_count)
})

test_that("aggregate_block expression handles multiple values", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_aggregate_expr(
    drill_down = "Region",
    values = c("Revenue", "Profit"),
    agg_fun = "sum"
  )
  result <- eval(expr, list(data = demo_data))

  expect_true("Revenue" %in% names(result))
  expect_true("Profit" %in% names(result))
})

test_that("aggregate_block expression handles multiple drill_down columns", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_aggregate_expr(
    drill_down = c("Region", "Category"),
    values = "Revenue",
    agg_fun = "sum"
  )
  result <- eval(expr, list(data = demo_data))

  expect_true("Region" %in% names(result))
  expect_true("Category" %in% names(result))

  # Should have rows for each combination
  expected_rows <- nrow(unique(demo_data[, c("Region", "Category")]))
  expect_equal(nrow(result), expected_rows)
})

# ==============================================================================
# Aggregation Function Tests (testing expression evaluation directly)
# ==============================================================================

test_that("aggregate_block mean aggregation works", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_aggregate_expr(
    drill_down = "Region",
    values = "Revenue",
    agg_fun = "mean"
  )
  result <- eval(expr, list(data = demo_data))

  expected_mean <- mean(demo_data$Revenue[demo_data$Region == "Western Europe"])
  actual_mean <- result$Revenue[result$Region == "Western Europe"]
  expect_equal(actual_mean, expected_mean, tolerance = 0.01)
})

test_that("aggregate_block count (n) aggregation works", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_aggregate_expr(
    drill_down = "Region",
    values = "Revenue",
    agg_fun = "n"
  )
  result <- eval(expr, list(data = demo_data))

  # With n(), only Count column is returned
  expect_true("Count" %in% names(result))
  expect_true("Region" %in% names(result))

  expected_count <- sum(demo_data$Region == "Western Europe")
  actual_count <- result$Count[result$Region == "Western Europe"]
  expect_equal(actual_count, expected_count)
})

test_that("aggregate_block min aggregation works", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_aggregate_expr(
    drill_down = "Region",
    values = "Revenue",
    agg_fun = "min"
  )
  result <- eval(expr, list(data = demo_data))

  expected_min <- min(demo_data$Revenue[demo_data$Region == "Western Europe"])
  actual_min <- result$Revenue[result$Region == "Western Europe"]
  expect_equal(actual_min, expected_min)
})

test_that("aggregate_block max aggregation works", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_aggregate_expr(
    drill_down = "Region",
    values = "Revenue",
    agg_fun = "max"
  )
  result <- eval(expr, list(data = demo_data))

  expected_max <- max(demo_data$Revenue[demo_data$Region == "Western Europe"])
  actual_max <- result$Revenue[result$Region == "Western Europe"]
  expect_equal(actual_max, expected_max)
})

test_that("aggregate_block median aggregation works", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_aggregate_expr(
    drill_down = "Region",
    values = "Revenue",
    agg_fun = "median"
  )
  result <- eval(expr, list(data = demo_data))

  expected_median <- median(demo_data$Revenue[demo_data$Region == "Western Europe"])
  actual_median <- result$Revenue[result$Region == "Western Europe"]
  expect_equal(actual_median, expected_median)
})

# ==============================================================================
# Edge Case Tests (testing expression evaluation directly)
# ==============================================================================

test_that("aggregate_block handles no drill_down (total only)", {
  demo_data <- bi_demo_data()

  expr <- blockr.bi:::build_aggregate_expr(
    drill_down = character(),
    values = "Revenue",
    agg_fun = "sum"
  )
  result <- eval(expr, list(data = demo_data))

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
  expect_true("Revenue" %in% names(result))
  expect_true("Count" %in% names(result))
  expect_equal(result$Revenue, sum(demo_data$Revenue))
})

test_that("aggregate_block handles data with NA values", {
  test_data <- data.frame(
    group = c("A", "A", "B", "B"),
    value = c(10, NA, 20, 30)
  )

  expr <- blockr.bi:::build_aggregate_expr(
    drill_down = "group",
    values = "value",
    agg_fun = "sum"
  )
  result <- eval(expr, list(data = test_data))

  # Should handle NAs (na.rm = TRUE)
  expect_equal(result$value[result$group == "A"], 10)
  expect_equal(result$value[result$group == "B"], 50)
})

test_that("aggregate_block handles single row data", {
  test_data <- data.frame(group = "A", value = 42)

  expr <- blockr.bi:::build_aggregate_expr(
    drill_down = "group",
    values = "value",
    agg_fun = "sum"
  )
  result <- eval(expr, list(data = test_data))

  expect_equal(nrow(result), 1)
  expect_equal(result$value, 42)
  expect_equal(result$Count, 1)
})

# ==============================================================================
# Block Server Integration Tests
# ==============================================================================

test_that("aggregate_block server initializes without error", {
  block <- new_aggregate_block()
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

test_that("aggregate_block UI contains expected elements", {
  blk <- new_aggregate_block(
    drill_down = "Region",
    values = "Revenue"
  )

  ui_output <- blk$expr_ui("test_id")

  expect_s3_class(ui_output, "shiny.tag.list")
  ui_text <- as.character(ui_output)

  # Should contain selectize inputs
  expect_true(grepl("selectize", ui_text, ignore.case = TRUE))

  # Should have drill_down and values inputs
  expect_true(grepl("drill_down", ui_text))
  expect_true(grepl("values", ui_text))

  # Should have aggregation selector
  expect_true(grepl("agg_fun", ui_text))
})
