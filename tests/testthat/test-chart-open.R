test_that("a gantt's open column takes a block and reaches the browser", {
  d <- data.frame(s = 1, e = 2, t = "a", o = TRUE, z = 1)
  v <- list(type = "gantt", x = "s", xend = "e", y = "t", open = "o")
  expect_true("o" %in% chart_needed_cols(v, d))
  blk <- new_chart_block(type = "gantt", x = "s", xend = "e", y = "t",
                         open = "o")
  expect_s3_class(blk, "chart_block")
})
