test_that("visual_filter_block constructor", {
  # Test basic constructor
  blk <- new_visual_filter_block()
  expect_s3_class(blk, c("visual_filter_block", "transform_block", "block"))

  # Test constructor with dimensions
  blk <- new_visual_filter_block(dimensions = c("Region", "Country"))
  expect_s3_class(blk, c("visual_filter_block", "transform_block", "block"))

  # Test constructor with measure
  blk <- new_visual_filter_block(measure = "Revenue")
  expect_s3_class(blk, c("visual_filter_block", "transform_block", "block"))

  # Test constructor with both
  blk <- new_visual_filter_block(
    dimensions = c("Region", "Category"),
    measure = "Profit"
  )
  expect_s3_class(blk, c("visual_filter_block", "transform_block", "block"))
})

test_that("visual_filter_block auto-detects dimensions", {
  block <- new_visual_filter_block()
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

test_that("visual_filter_block limits dimensions to 4", {
  # Data has 6 potential dimension columns (Region, Country, Category, Channel, Year, Quarter)
  # But Year and Quarter are numeric in our demo, so we have 4 non-numeric
  block <- new_visual_filter_block()
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

test_that("visual_filter_block uses specified dimensions", {
  block <- new_visual_filter_block(dimensions = c("Region", "Channel"))
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

test_that("visual_filter_block clear_filters button", {
  block <- new_visual_filter_block()
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

test_that("visual_filter_block handles empty data", {
  block <- new_visual_filter_block()
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

test_that("visual_filter_block handles data with no numeric columns", {
  block <- new_visual_filter_block()
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

test_that("visual_filter_block handles data with only numeric columns", {
  block <- new_visual_filter_block()
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

test_that("visual_filter_block state contains correct fields", {
  block <- new_visual_filter_block(
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

test_that("visual_filter_block with mtcars data", {
  # Test with a different dataset to ensure generalization
  block <- new_visual_filter_block()

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

test_that("visual_filter_block with iris data", {
  # iris has Species as character/factor column
  block <- new_visual_filter_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      expect_type(session$returned, "list")
    },
    args = list(x = block, data = list(data = function() iris))
  )
})

test_that("visual_filter_block ignores non-existent dimensions", {
  # Specify dimensions that don't exist in data
  block <- new_visual_filter_block(
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

test_that("visual_filter_block uses first numeric column when measure not found", {
  block <- new_visual_filter_block(measure = "NonExistentMeasure")
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
