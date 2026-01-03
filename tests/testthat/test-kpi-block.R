# Tests for KPI Block

# ==============================================================================
# Constructor Tests
# ==============================================================================

test_that("kpi_block constructor creates correct class", {
  blk <- new_kpi_block()
  expect_s3_class(blk, c("kpi_block", "transform_block", "block"))
})

test_that("kpi_block constructor accepts all arguments", {
  blk <- new_kpi_block(
    measures = c("Revenue", "Profit"),
    agg_fun = "mean",
    prefix = "$",
    suffix = "M",
    digits = "2"
  )
  expect_s3_class(blk, c("kpi_block", "transform_block", "block"))
})

test_that("kpi_block constructor with single measure", {
  blk <- new_kpi_block(measures = "Revenue")
  expect_s3_class(blk, c("kpi_block", "transform_block", "block"))
})

test_that("kpi_block constructor with empty measures", {
  blk <- new_kpi_block(measures = character())
  expect_s3_class(blk, c("kpi_block", "transform_block", "block"))
})

# ==============================================================================
# State Tests - Verify arguments are properly stored and retrievable
# ==============================================================================

test_that("kpi_block state contains correct fields", {
  block <- new_kpi_block(
    measures = c("Revenue", "Profit"),
    agg_fun = "sum",
    prefix = "$",
    suffix = "",
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
      expect_true("measures" %in% names(state))
      expect_true("agg_fun" %in% names(state))
      expect_true("prefix" %in% names(state))
      expect_true("suffix" %in% names(state))
      expect_true("digits" %in% names(state))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("kpi_block state values match constructor arguments", {
  block <- new_kpi_block(
    measures = c("Revenue", "Profit"),
    agg_fun = "mean",
    prefix = "$",
    suffix = "M",
    digits = "2"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state

      # State reactives should return the values passed to constructor
      expect_equal(state$measures(), c("Revenue", "Profit"))
      expect_equal(state$agg_fun(), "mean")
      expect_equal(state$prefix(), "$")
      expect_equal(state$suffix(), "M")
      expect_equal(state$digits(), "2")
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

# ==============================================================================
# Expression Generation Tests
# ==============================================================================

test_that("kpi_block generates correct expression for sum", {
  block <- new_kpi_block(
    measures = c("Revenue"),
    agg_fun = "sum",
    digits = "0"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      expr <- session$returned$expr()
      expr_text <- deparse(expr)

      expect_true(any(grepl("dplyr::summarise", expr_text)))
      expect_true(any(grepl("sum", expr_text)))
      expect_true(any(grepl("Revenue", expr_text)))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("kpi_block generates correct expression for mean", {
  block <- new_kpi_block(
    measures = c("Revenue"),
    agg_fun = "mean",
    digits = "2"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      expr <- session$returned$expr()
      expr_text <- deparse(expr)

      expect_true(any(grepl("dplyr::summarise", expr_text)))
      expect_true(any(grepl("mean", expr_text)))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("kpi_block generates correct expression for multiple measures", {
  block <- new_kpi_block(
    measures = c("Revenue", "Profit", "Transactions"),
    agg_fun = "sum",
    digits = "0"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      expr <- session$returned$expr()
      expr_text <- deparse(expr)

      expect_true(any(grepl("Revenue", expr_text)))
      expect_true(any(grepl("Profit", expr_text)))
      expect_true(any(grepl("Transactions", expr_text)))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("kpi_block generates correct expression for count (n)", {
  block <- new_kpi_block(
    measures = c("Revenue"),
    agg_fun = "n",
    digits = "0"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      expr <- session$returned$expr()
      expr_text <- deparse(expr)

      expect_true(any(grepl("dplyr::n", expr_text)))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

# ==============================================================================
# Result Tests - Verify output data is correct
# ==============================================================================

test_that("kpi_block returns correct result for single measure sum", {
  block <- new_kpi_block(
    measures = c("Revenue"),
    agg_fun = "sum",
    digits = "0"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 1)
      expect_true("Revenue" %in% names(result))

      # Check the sum is correct
      expected_sum <- sum(demo_data$Revenue)
      expect_equal(result$Revenue, expected_sum)
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("kpi_block returns correct result for multiple measures", {
  block <- new_kpi_block(
    measures = c("Revenue", "Profit"),
    agg_fun = "sum",
    digits = "0"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 1)
      expect_equal(ncol(result), 2)
      expect_true("Revenue" %in% names(result))
      expect_true("Profit" %in% names(result))

      # Check the sums are correct
      expect_equal(result$Revenue, sum(demo_data$Revenue))
      expect_equal(result$Profit, sum(demo_data$Profit))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("kpi_block returns correct result for mean aggregation", {
  block <- new_kpi_block(
    measures = c("Revenue"),
    agg_fun = "mean",
    digits = "2"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 1)

      # Check the mean is correct (within rounding)
      expected_mean <- round(mean(demo_data$Revenue), 2)
      expect_equal(result$Revenue, expected_mean)
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("kpi_block digits parameter affects rounding", {
  block <- new_kpi_block(
    measures = c("Revenue"),
    agg_fun = "mean",
    digits = "0"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # With 0 digits, should be a whole number
      expected_mean <- round(mean(demo_data$Revenue), 0)
      expect_equal(result$Revenue, expected_mean)
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

# ==============================================================================
# Aggregation Function Tests
# ==============================================================================

test_that("kpi_block median aggregation works", {
  block <- new_kpi_block(
    measures = c("Revenue"),
    agg_fun = "median",
    digits = "0"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expected_median <- round(median(demo_data$Revenue), 0)
      expect_equal(result$Revenue, expected_median)
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("kpi_block min aggregation works", {
  block <- new_kpi_block(
    measures = c("Revenue"),
    agg_fun = "min",
    digits = "0"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expected_min <- round(min(demo_data$Revenue), 0)
      expect_equal(result$Revenue, expected_min)
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("kpi_block max aggregation works", {
  block <- new_kpi_block(
    measures = c("Revenue"),
    agg_fun = "max",
    digits = "0"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expected_max <- round(max(demo_data$Revenue), 0)
      expect_equal(result$Revenue, expected_max)
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

# ==============================================================================
# Auto-detection Tests
# ==============================================================================

test_that("kpi_block auto-detects numeric columns when measures is empty", {
  block <- new_kpi_block(measures = character())
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Should auto-select some numeric columns
      state <- session$returned$state
      measures <- state$measures()
      expect_true(length(measures) > 0)
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

# ==============================================================================
# Edge Case Tests
# ==============================================================================

test_that("kpi_block handles data with NA values", {
  block <- new_kpi_block(
    measures = c("value"),
    agg_fun = "sum"
  )
  test_data <- data.frame(
    value = c(10, 20, NA, 30, NA)
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Should sum non-NA values (na.rm = TRUE)
      expect_equal(result$value, 60)
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("kpi_block handles single-row data", {
  block <- new_kpi_block(
    measures = c("value"),
    agg_fun = "sum"
  )
  test_data <- data.frame(value = 42)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_equal(result$value, 42)
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("kpi_block handles all-NA column", {
  block <- new_kpi_block(
    measures = c("value"),
    agg_fun = "sum"
  )
  test_data <- data.frame(value = c(NA_real_, NA_real_, NA_real_))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # sum of all NAs with na.rm=TRUE is 0
      expect_equal(result$value, 0)
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

# ==============================================================================
# UI Tests
# ==============================================================================

test_that("kpi_block UI contains expected elements", {
  blk <- new_kpi_block(
    measures = c("Revenue"),
    prefix = "$"
  )

  ui_output <- blk$expr_ui("test_id")

  # UI returns a tagList which inherits from shiny.tag.list
  expect_true(inherits(ui_output, "shiny.tag") || inherits(ui_output, "shiny.tag.list"))
  ui_text <- as.character(ui_output)

  # Should contain selectize input for measures
  expect_true(grepl("selectize", ui_text, ignore.case = TRUE))

  # Should contain aggregation selector
  expect_true(grepl("agg_fun", ui_text))

  # Should contain prefix input
  expect_true(grepl("prefix", ui_text))

  # Should contain suffix input
  expect_true(grepl("suffix", ui_text))

  # Should contain digits input
  expect_true(grepl("digits", ui_text))
})

# ==============================================================================
# Reactive Updates Tests
# ==============================================================================

test_that("kpi_block updates when inputs change", {
  block <- new_kpi_block(
    measures = c("Revenue"),
    agg_fun = "sum"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Initial result
      result1 <- session$returned$result()
      expect_equal(result1$Revenue, sum(demo_data$Revenue))

      # Change aggregation to mean via input
      session$setInputs(`expr-agg_fun` = "mean")
      session$flushReact()

      result2 <- session$returned$result()
      # Result should now be mean
      expect_equal(result2$Revenue, round(mean(demo_data$Revenue), 0))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

# ==============================================================================
# Titles and Subtitles Tests
# ==============================================================================

test_that("kpi_block constructor accepts titles parameter", {
  blk <- new_kpi_block(
    measures = c("Revenue", "Profit"),
    titles = c(Revenue = "Total Revenue", Profit = "Net Profit")
  )
  expect_s3_class(blk, c("kpi_block", "transform_block", "block"))
})

test_that("kpi_block constructor accepts subtitles parameter", {
  blk <- new_kpi_block(
    measures = c("Revenue", "Profit"),
    subtitles = c(Revenue = "Year to date", Profit = "After taxes")
  )
  expect_s3_class(blk, c("kpi_block", "transform_block", "block"))
})

test_that("kpi_block constructor accepts both titles and subtitles", {
  blk <- new_kpi_block(
    measures = c("Revenue", "Profit"),
    titles = c(Revenue = "Total Revenue"),
    subtitles = c(Revenue = "Year to date", Profit = "After taxes")
  )
  expect_s3_class(blk, c("kpi_block", "transform_block", "block"))
})

test_that("kpi_block state contains titles and subtitles fields", {
  block <- new_kpi_block(
    measures = c("Revenue", "Profit"),
    titles = c(Revenue = "Total Revenue"),
    subtitles = c(Revenue = "Year to date")
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state
      expect_true("titles" %in% names(state))
      expect_true("subtitles" %in% names(state))
      expect_true(is.function(state$titles))
      expect_true(is.function(state$subtitles))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("kpi_block state titles match constructor arguments", {
  block <- new_kpi_block(
    measures = c("Revenue", "Profit"),
    titles = c(Revenue = "Total Revenue", Profit = "Net Profit")
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state
      titles <- state$titles()

      expect_equal(titles[["Revenue"]], "Total Revenue")
      expect_equal(titles[["Profit"]], "Net Profit")
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("kpi_block state subtitles match constructor arguments", {
  block <- new_kpi_block(
    measures = c("Revenue", "Profit"),
    subtitles = c(Revenue = "Year to date", Profit = "After taxes")
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state
      subtitles <- state$subtitles()

      expect_equal(subtitles[["Revenue"]], "Year to date")
      expect_equal(subtitles[["Profit"]], "After taxes")
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("kpi_block titles update when dynamic inputs change", {
  block <- new_kpi_block(
    measures = c("Revenue"),
    titles = NULL
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Initial state - no titles
      state <- session$returned$state
      initial_titles <- state$titles()
      expect_true(is.null(initial_titles) || length(initial_titles) == 0)

      # Set title via dynamic input
      session$setInputs(`expr-title_Revenue` = "Custom Title")
      session$flushReact()

      # Title should be updated
      updated_titles <- state$titles()
      expect_equal(updated_titles[["Revenue"]], "Custom Title")
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("kpi_block subtitles update when dynamic inputs change", {
  block <- new_kpi_block(
    measures = c("Revenue"),
    subtitles = NULL
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Initial state - no subtitles
      state <- session$returned$state
      initial_subtitles <- state$subtitles()
      expect_true(is.null(initial_subtitles) || length(initial_subtitles) == 0)

      # Set subtitle via dynamic input
      session$setInputs(`expr-subtitle_Revenue` = "Custom Subtitle")
      session$flushReact()

      # Subtitle should be updated
      updated_subtitles <- state$subtitles()
      expect_equal(updated_subtitles[["Revenue"]], "Custom Subtitle")
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("kpi_block hidden JSON inputs are updated with titles", {
  block <- new_kpi_block(
    measures = c("Revenue"),
    titles = c(Revenue = "Total Revenue")
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Hidden textInputs are updated via updateTextInput
      # We verify the state is set correctly (which drives the hidden input updates)
      state <- session$returned$state
      expect_equal(state$titles()[["Revenue"]], "Total Revenue")
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})
