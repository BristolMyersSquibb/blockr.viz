# type = "histogram": R bins x, the browser draws the bins as touching bars,
# and a click on a bin filters x to its range.

hist_df <- function() {
  data.frame(v = c(1, 2, 2, 3, 7, 8, 9, 15), g = rep(c("a", "b"), 4),
             stringsAsFactors = FALSE)
}

test_that("bins are readable breaks, closed on the left", {
  b <- chart_bins(c(0, 4.9, 5, 10), 2)
  expect_equal(levels(b$bin), c("0–5", "5–10"))
  expect_equal(as.character(b$bin), c("0–5", "0–5", "5–10",
                                      "5–10"))
  # the last bin includes its upper end
  expect_true(b$edges$last[[2L]])
  expect_null(chart_bin_cond("v", "x", NULL))
})

test_that("a histogram ships its bins, every one of them, as a bar's group", {
  blk <- new_chart_block(type = "histogram", x = "v", bins = 3)
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      sent <- list()
      root <- session$rootScope()
      root$sendCustomMessage <- function(type, message) {
        if (identical(type, "drilldown-data")) sent[[length(sent) + 1L]] <<- message
      }
      session$flushReact()
      cfg <- sent[[length(sent)]]$config
      expect_equal(cfg$chart_type, "histogram")
      expect_equal(cfg$group, "v_bin")
      expect_equal(cfg$value, ".count")
      expect_equal(cfg$func, "count")
      expect_equal(cfg$sort_by, "data")
      expect_equal(cfg$orientation, "vertical")
      expect_equal(session$returned$state$x(), "v")
      expect_equal(session$returned$state$bins(), 3)
    },
    args = list(x = blk, data = list(data = hist_df))
  )
})

test_that("a click on a bin filters x to the bin's range", {
  blk <- new_chart_block(type = "histogram", x = "v", bins = 3,
                         drill = "auto")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      lab <- levels(chart_bins(hist_df()$v, 3)$bin)[[1L]]
      session$makeScope("expr")$setInputs(drilldown_block_action = list(
        action = "filter", filter_type = "categorical",
        filters = list(v_bin = list(lab)), nonce = 1
      ))
      session$flushReact()
      res <- session$returned$result()
      expect_false("v_bin" %in% names(res))
      expect_true(nrow(res) > 0L)
      expect_true(all(res$v < 5))
    },
    args = list(x = blk, data = list(data = hist_df))
  )
})

test_that("the gear's bins edit reaches state, and x survives the echo", {
  blk <- new_chart_block(type = "histogram", x = "v")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      session$makeScope("expr")$setInputs(drilldown_block_action = list(
        action = "config", chart_type = "histogram", group = "v_bin",
        value = ".count", func = "count", x = "v", y = "", bins = "10",
        orientation = "vertical", sort_by = "data", sort_dir = "asc"
      ))
      session$flushReact()
      st <- session$returned$state
      expect_equal(st$bins(), 10)
      expect_equal(st$x(), "v")
      expect_null(st$y())
      # the bins' own order is not written over the bar's sort
      expect_null(st$sort_by())
    },
    args = list(x = blk, data = list(data = hist_df))
  )
})
