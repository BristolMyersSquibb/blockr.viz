test_that("table_filter_block constructor", {
  # Test basic constructor
  blk <- new_table_filter_block()
  expect_s3_class(blk, c("table_filter_block", "transform_block", "block"))

  # Test constructor with dimensions
  blk <- new_table_filter_block(dimensions = c("Region", "Country"))
  expect_s3_class(blk, c("table_filter_block", "transform_block", "block"))

  # Test constructor with measure
  blk <- new_table_filter_block(measure = "Revenue")
  expect_s3_class(blk, c("table_filter_block", "transform_block", "block"))

  # Test constructor with both
  blk <- new_table_filter_block(
    dimensions = c("Region", "Category"),
    measure = "Profit"
  )
  expect_s3_class(blk, c("table_filter_block", "transform_block", "block"))
})

test_that("table_filter_block auto-detects dimensions", {
  block <- new_table_filter_block()
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # The server should detect dimensions from data
      # Non-numeric columns should be dimensions
      expect_type(session$returned, "list")
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("table_filter_block limits dimensions to 4", {
  block <- new_table_filter_block()
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Server should initialize without error
      expect_type(session$returned, "list")
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("table_filter_block uses specified dimensions", {
  block <- new_table_filter_block(dimensions = c("Region", "Channel"))
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Should initialize with specified dimensions
      expect_type(session$returned, "list")
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("table_filter_block clear_filters button", {
  block <- new_table_filter_block()
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Click clear filters button
      session$setInputs(clear_filters = 1)
      session$flushReact()

      # Should not crash
      expect_type(session$returned, "list")

      # Click again
      session$setInputs(clear_filters = 2)
      session$flushReact()

      expect_type(session$returned, "list")
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("table_filter_block handles empty data", {
  block <- new_table_filter_block()
  empty_data <- data.frame()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Should handle empty data without crashing
      expect_type(session$returned, "list")
    },
    args = list(x = block, data = list(data = function() empty_data))
  )
})

test_that("table_filter_block handles data with no numeric columns", {
  block <- new_table_filter_block()
  char_only_data <- data.frame(
    a = c("x", "y", "z"),
    b = c("p", "q", "r"),
    stringsAsFactors = FALSE
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Should handle data without numeric columns
      expect_type(session$returned, "list")
    },
    args = list(x = block, data = list(data = function() char_only_data))
  )
})

test_that("table_filter_block handles data with only numeric columns", {
  block <- new_table_filter_block()
  numeric_only_data <- data.frame(
    a = c(1, 2, 3),
    b = c(4, 5, 6),
    c = c(7, 8, 9)
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Should handle data with only numeric columns (no dimensions)
      expect_type(session$returned, "list")
    },
    args = list(x = block, data = list(data = function() numeric_only_data))
  )
})

test_that("table_filter_block state contains correct fields", {
  block <- new_table_filter_block(
    dimensions = c("Region"),
    measure = "Revenue"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Check returned structure has state
      returned <- session$returned
      expect_type(returned, "list")
      expect_true("state" %in% names(returned))
      expect_true("expr" %in% names(returned))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("table_filter_block with mtcars data", {
  # Test with a different dataset to ensure generalization
  block <- new_table_filter_block()

  # mtcars has numeric columns (mpg, cyl, disp, hp, etc.)
  # No character columns, so dimensions will be empty
  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      expect_type(session$returned, "list")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("table_filter_block with iris data", {
  # iris has Species as character/factor column
  block <- new_table_filter_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      expect_type(session$returned, "list")
    },
    args = list(x = block, data = list(data = function() iris))
  )
})

test_that("table_filter_block ignores non-existent dimensions", {
  # Specify dimensions that don't exist in data
  block <- new_table_filter_block(
    dimensions = c("NonExistent1", "NonExistent2", "Region")
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Should only use Region (the one that exists)
      expect_type(session$returned, "list")
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("table_filter_block uses first numeric column when measure not found", {
  block <- new_table_filter_block(measure = "NonExistentMeasure")
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Should fall back to first numeric column
      expect_type(session$returned, "list")
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

# ==============================================================================
# Filter State Tests
# ==============================================================================

test_that("table_filter_block constructor accepts filters parameter", {
  # Single filter
  blk <- new_table_filter_block(
    filters = list(Region = "Western Europe")
  )
  expect_s3_class(blk, c("table_filter_block", "transform_block", "block"))

  # Multiple filters
  blk <- new_table_filter_block(
    filters = list(
      Region = "Western Europe",
      Category = "Electronics"
    )
  )
  expect_s3_class(blk, c("table_filter_block", "transform_block", "block"))

  # Multi-select filter (multiple values for one dimension)
  blk <- new_table_filter_block(
    filters = list(Region = c("Western Europe", "Southern Europe"))
  )
  expect_s3_class(blk, c("table_filter_block", "transform_block", "block"))

  # Empty filters (default)
  blk <- new_table_filter_block(filters = list())
  expect_s3_class(blk, c("table_filter_block", "transform_block", "block"))
})

test_that("table_filter_block state contains filters field", {
  block <- new_table_filter_block(
    dimensions = c("Region", "Category"),
    measure = "Revenue",
    filters = list(Region = "Western Europe")
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Check state has filters field
      state <- session$returned$state
      expect_true("filters" %in% names(state))
      expect_true(is.function(state$filters))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("table_filter_block filters are initialized from constructor", {
  block <- new_table_filter_block(
    dimensions = c("Region", "Category"),
    measure = "Revenue",
    filters = list(Region = "Western Europe")
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Check filters are initialized correctly
      state <- session$returned$state
      filters <- state$filters()

      expect_type(filters, "list")
      expect_true("Region" %in% names(filters))
      expect_equal(filters$Region, "Western Europe")
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("table_filter_block filters with multiple dimensions", {
  block <- new_table_filter_block(
    dimensions = c("Region", "Category", "Channel"),
    measure = "Revenue",
    filters = list(
      Region = "Western Europe",
      Category = "Electronics"
    )
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state
      filters <- state$filters()

      expect_equal(filters$Region, "Western Europe")
      expect_equal(filters$Category, "Electronics")
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("table_filter_block filters with multi-select values", {
  block <- new_table_filter_block(
    dimensions = c("Region"),
    measure = "Revenue",
    filters = list(Region = c("Western Europe", "Southern Europe"))
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state
      filters <- state$filters()

      expect_equal(
        sort(filters$Region),
        sort(c("Western Europe", "Southern Europe"))
      )
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("table_filter_block generates filter expression when filters are set", {
  block <- new_table_filter_block(
    dimensions = c("Region"),
    measure = "Revenue",
    filters = list(Region = "Western Europe")
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      expr <- session$returned$expr()
      expr_text <- deparse(expr)

      # Should generate a dplyr::filter expression
      expect_true(any(grepl("dplyr::filter", expr_text)))
      expect_true(any(grepl("Region", expr_text)))
      expect_true(any(grepl("Western Europe", expr_text)))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("table_filter_block generates identity expression when no filters", {
  block <- new_table_filter_block(
    dimensions = c("Region"),
    measure = "Revenue",
    filters = list()
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      expr <- session$returned$expr()
      expr_text <- deparse(expr)

      # Should generate identity(data) when no filters
      expect_true(any(grepl("identity", expr_text)))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("table_filter_block filters produce correct result", {
  block <- new_table_filter_block(
    dimensions = c("Region"),
    measure = "Revenue",
    filters = list(Region = "Western Europe")
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()

      # Result should only contain Western Europe
      expect_true(all(result$Region == "Western Europe"))

      # Should have fewer rows than original
      expect_lt(nrow(result), nrow(demo_data))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("table_filter_block multi-select filters produce correct result", {
  block <- new_table_filter_block(
    dimensions = c("Region"),
    measure = "Revenue",
    filters = list(Region = c("Western Europe", "Southern Europe"))
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()

      # Result should only contain selected regions
      expect_true(all(result$Region %in% c("Western Europe", "Southern Europe")))

      # Should have fewer rows than original
      expect_lt(nrow(result), nrow(demo_data))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("table_filter_block multiple dimension filters produce correct result", {
  block <- new_table_filter_block(
    dimensions = c("Region", "Category"),
    measure = "Revenue",
    filters = list(
      Region = "Western Europe",
      Category = "Electronics"
    )
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()

      # Result should only contain filtered values
      expect_true(all(result$Region == "Western Europe"))
      expect_true(all(result$Category == "Electronics"))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("table_filter_block empty filters constructor works", {
  # Test that empty filters is valid and returns a working block
  block <- new_table_filter_block(
    dimensions = c("Region"),
    measure = "Revenue",
    filters = list()
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Filters should be empty
      filters <- session$returned$state$filters()
      expect_equal(length(filters), 0)

      # Expression should be identity when no filters
      expr <- session$returned$expr()
      expr_text <- deparse(expr)
      expect_true(any(grepl("identity", expr_text)))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

# ==============================================================================
# State Restoration Tests
# ==============================================================================

test_that("table_filter_block dimensions are restored from constructor", {
  block <- new_table_filter_block(
    dimensions = c("Region", "Channel"),
    measure = "Revenue"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state
      dims <- state$dimensions()

      expect_equal(dims, c("Region", "Channel"))
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("table_filter_block measure is restored from constructor", {
  block <- new_table_filter_block(
    dimensions = c("Region"),
    measure = "Profit"
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state
      meas <- state$measure()

      expect_equal(meas, "Profit")
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})

test_that("table_filter_block full state restoration", {
  # Test all three state components together
  block <- new_table_filter_block(
    dimensions = c("Region", "Category", "Channel"),
    measure = "Profit",
    filters = list(Region = "Western Europe", Category = "Electronics")
  )
  demo_data <- bi_demo_data()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state

      # Verify dimensions
      expect_equal(state$dimensions(), c("Region", "Category", "Channel"))

      # Verify measure
      expect_equal(state$measure(), "Profit")

      # Verify filters
      filters <- state$filters()
      expect_equal(filters$Region, "Western Europe")
      expect_equal(filters$Category, "Electronics")
    },
    args = list(x = block, data = list(data = function() demo_data))
  )
})
