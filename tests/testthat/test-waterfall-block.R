# Tests for Waterfall Block

# Sample data for testing
waterfall_test_data <- function() {
  data.frame(
    Category = rep(c("North", "South", "West"), each = 4),
    Revenue = c(100, 120, 80, 90, 95, 105, 85, 100, 110, 90, 100, 80),
    Gross_Profit = c(70, 85, 55, 62, 65, 72, 58, 68, 76, 62, 68, 54),
    Net_Income = c(45, 55, 35, 40, 42, 47, 37, 44, 49, 40, 44, 35)
  )
}

# ==============================================================================
# Constructor Tests
# ==============================================================================

test_that("waterfall_block constructor creates correct class", {
  blk <- new_waterfall_block()
  expect_s3_class(blk, c("waterfall_block", "transform_block", "block"))
})

test_that("waterfall_block constructor accepts all arguments", {
  blk <- new_waterfall_block(
    measures = c("Revenue", "Gross_Profit", "Net_Income"),
    colors = list(increase = "#00ff00", decrease = "#ff0000", total = "#0000ff")
  )
  expect_s3_class(blk, c("waterfall_block", "transform_block", "block"))
})

test_that("waterfall_block constructor with empty measures", {
  blk <- new_waterfall_block(measures = character())
  expect_s3_class(blk, c("waterfall_block", "transform_block", "block"))
})

# ==============================================================================
# State Tests - Verify arguments are properly stored and retrievable
# ==============================================================================

test_that("waterfall_block state contains correct fields", {
  block <- new_waterfall_block(
    measures = c("Revenue", "Gross_Profit", "Net_Income")
  )
  test_data <- waterfall_test_data()

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
      expect_true("measures" %in% names(state))
      expect_true("colors" %in% names(state))
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("waterfall_block state values match constructor arguments", {
  block <- new_waterfall_block(
    measures = c("Revenue", "Gross_Profit", "Net_Income"),
    colors = list(increase = "#00ff00", decrease = "#ff0000", total = "#0000ff")
  )
  test_data <- waterfall_test_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state

      # State reactives should return the values passed to constructor
      expect_equal(state$measures(), c("Revenue", "Gross_Profit", "Net_Income"))
      expect_equal(state$colors()$increase, "#00ff00")
      expect_equal(state$colors()$decrease, "#ff0000")
      expect_equal(state$colors()$total, "#0000ff")
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

# ==============================================================================
# Expression Generation Tests
# ==============================================================================

test_that("waterfall_block generates correct expression", {
  block <- new_waterfall_block(
    measures = c("Revenue", "Gross_Profit", "Net_Income")
  )
  test_data <- waterfall_test_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      expr <- session$returned$expr()
      expr_text <- deparse(expr)

      expect_true(any(grepl("dplyr::summarise", expr_text)))
      expect_true(any(grepl("Revenue", expr_text)))
      expect_true(any(grepl("Gross_Profit", expr_text)))
      expect_true(any(grepl("Net_Income", expr_text)))
      expect_true(any(grepl("sum", expr_text)))
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

# ==============================================================================
# Result Tests - Verify output data is correct
# ==============================================================================

test_that("waterfall_block returns correct aggregated result", {
  block <- new_waterfall_block(
    measures = c("Revenue", "Gross_Profit", "Net_Income")
  )
  test_data <- waterfall_test_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 1)
      expect_true("Revenue" %in% names(result))
      expect_true("Gross_Profit" %in% names(result))
      expect_true("Net_Income" %in% names(result))

      # Check the sums are correct
      expect_equal(result$Revenue, sum(test_data$Revenue))
      expect_equal(result$Gross_Profit, sum(test_data$Gross_Profit))
      expect_equal(result$Net_Income, sum(test_data$Net_Income))
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

# ==============================================================================
# Auto-detection Tests
# ==============================================================================

test_that("waterfall_block auto-detects numeric columns when measures is empty", {
  block <- new_waterfall_block(measures = character())
  test_data <- waterfall_test_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Should auto-select some numeric columns
      state <- session$returned$state
      measures <- state$measures()
      expect_true(length(measures) >= 2)  # Need at least 2 for waterfall
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("waterfall_block auto-selects first N numeric columns", {
  block <- new_waterfall_block(measures = character())
  test_data <- waterfall_test_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state
      measures <- state$measures()

      # Should select the first 3 numeric columns
      numeric_cols <- names(test_data)[vapply(test_data, is.numeric, logical(1))]
      expect_equal(measures, utils::head(numeric_cols, 3))
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

# ==============================================================================
# Edge Case Tests
# ==============================================================================

test_that("waterfall_block handles data with NA values", {
  block <- new_waterfall_block(
    measures = c("Revenue", "Net_Income")
  )
  test_data <- data.frame(
    Revenue = c(100, 200, NA, 300),
    Net_Income = c(50, 80, NA, 120)
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Should sum non-NA values (na.rm = TRUE)
      expect_equal(result$Revenue, 600)
      expect_equal(result$Net_Income, 250)
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("waterfall_block handles single-row data", {
  block <- new_waterfall_block(
    measures = c("Revenue", "Net_Income")
  )
  test_data <- data.frame(
    Revenue = 100,
    Net_Income = 60
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_equal(result$Revenue, 100)
      expect_equal(result$Net_Income, 60)
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

# ==============================================================================
# UI Tests
# ==============================================================================

test_that("waterfall_block UI contains expected elements", {
  blk <- new_waterfall_block(
    measures = c("Revenue", "Net_Income")
  )

  ui_output <- blk$expr_ui("test_id")

  # UI returns a tagList which inherits from shiny.tag.list
  expect_true(inherits(ui_output, "shiny.tag") || inherits(ui_output, "shiny.tag.list"))
  ui_text <- as.character(ui_output)

  # Should contain selectize input for measures
  expect_true(grepl("selectize", ui_text, ignore.case = TRUE))

  # Should contain measures input
  expect_true(grepl("measures", ui_text))
})

# ==============================================================================
# Reactive Updates Tests
# ==============================================================================

test_that("waterfall_block updates when measures input changes", {
  block <- new_waterfall_block(
    measures = c("Revenue", "Gross_Profit", "Net_Income")
  )
  test_data <- waterfall_test_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Initial state should have 3 measures
      state <- session$returned$state
      expect_length(state$measures(), 3)

      # Change measures via input
      session$setInputs(`expr-measures` = c("Revenue", "Net_Income"))
      session$flushReact()

      # Should now have 2 measures
      expect_length(state$measures(), 2)
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

# ==============================================================================
# Waterfall Data Helper Tests
# ==============================================================================

test_that("build_waterfall_data creates correct structure", {
  data <- data.frame(
    Revenue = 100,
    Gross_Profit = 70,
    Net_Income = 45
  )
  measures <- c("Revenue", "Gross_Profit", "Net_Income")

  wf <- blockr.bi:::build_waterfall_data(data, measures)

  expect_true(is.data.frame(wf))
  expect_equal(nrow(wf), 3)
  expect_true("step" %in% names(wf))
  expect_true("total" %in% names(wf))
  expect_true("delta" %in% names(wf))
  expect_true("helper" %in% names(wf))
  expect_true("positive" %in% names(wf))
  expect_true("negative" %in% names(wf))
})

test_that("build_waterfall_data calculates correct deltas for decreasing values", {
  data <- data.frame(
    Revenue = 100,
    Gross_Profit = 70,
    Net_Income = 45
  )
  measures <- c("Revenue", "Gross_Profit", "Net_Income")

  wf <- blockr.bi:::build_waterfall_data(data, measures)

  # First delta is the initial value
  expect_equal(wf$delta[1], 100)
  # Second delta is the decrease from 100 to 70
  expect_equal(wf$delta[2], -30)
  # Third delta is the decrease from 70 to 45
  expect_equal(wf$delta[3], -25)
})

test_that("build_waterfall_data handles increasing values", {
  data <- data.frame(
    Base = 100,
    Plus_Add = 130,
    Total = 145
  )
  measures <- c("Base", "Plus_Add", "Total")

  wf <- blockr.bi:::build_waterfall_data(data, measures)

  # First delta is the initial value
  expect_equal(wf$delta[1], 100)
  # Second delta is the increase from 100 to 130
  expect_equal(wf$delta[2], 30)
  # Third delta is the increase from 130 to 145
  expect_equal(wf$delta[3], 15)

  # Positive values should be in the positive column for middle bars
  expect_equal(wf$positive[2], 30)
  # Last bar shows delta, same as middle bars
  expect_equal(wf$positive[3], 15)
  expect_equal(wf$helper[3], 130)
})

test_that("build_waterfall_data handles mixed increases and decreases", {
  data <- data.frame(
    Start = 100,
    After_Add = 150,
    After_Sub = 120,
    Final = 110
  )
  measures <- c("Start", "After_Add", "After_Sub", "Final")

  wf <- blockr.bi:::build_waterfall_data(data, measures)

  expect_equal(wf$delta[1], 100)
  expect_equal(wf$delta[2], 50)   # Increase
  expect_equal(wf$delta[3], -30)  # Decrease
  expect_equal(wf$delta[4], -10)  # Decrease

  # Second bar (increase) should have positive value
  expect_equal(wf$positive[2], 50)
  expect_equal(wf$negative[2], 0)

  # Third bar (decrease) should have negative value
  expect_equal(wf$positive[3], 0)
  expect_equal(wf$negative[3], 30)
})

# ==============================================================================
# Colors Parameter Tests
# ==============================================================================

test_that("waterfall_block uses default colors", {
  blk <- new_waterfall_block()

  # Default colors should be set
  expect_equal(attr(blk, "colors")$increase, "#009E73")
  expect_equal(attr(blk, "colors")$decrease, "#dc2626")
  expect_equal(attr(blk, "colors")$total, "#bbbbbb")
})

test_that("waterfall_block uses custom colors", {
  blk <- new_waterfall_block(
    colors = list(increase = "#00ff00", decrease = "#ff0000", total = "#0000ff")
  )

  expect_equal(attr(blk, "colors")$increase, "#00ff00")
  expect_equal(attr(blk, "colors")$decrease, "#ff0000")
  expect_equal(attr(blk, "colors")$total, "#0000ff")
})
