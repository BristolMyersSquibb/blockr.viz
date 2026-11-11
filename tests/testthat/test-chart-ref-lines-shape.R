test_that("ref_lines and shape take a block and their columns reach the browser", {
  d <- data.frame(day = 1:2, val = c(3, 4), LLN = 1, ULN = 5, grade = "G1",
                  unused = 0)
  v <- list(type = "line", x = "day", y = "val", ref_lines = c("LLN", "ULN"),
            shape = "grade")
  cols <- chart_needed_cols(v, d)
  expect_true(all(c("LLN", "ULN", "grade") %in% cols))
  expect_false("unused" %in% cols)
  # A listed column the data lacks is not asked for.
  v$ref_lines <- c("LLN", "absent")
  expect_false("absent" %in% chart_needed_cols(v, d))

  blk <- new_chart_block(type = "line", x = "day", y = "val",
                         ref_lines = c("LLN", "ULN"),
                         ref_line_colors = list(LLN = "#4DAF4A"),
                         shape = "grade")
  expect_s3_class(blk, "chart_block")
})

test_that("ref_line_colors is a named list of colors", {
  spec <- viz_block_spec("chart")
  ok <- viz_check(spec, list(ref_line_colors = list(LLN = "#4DAF4A",
                                                   ULN = "red")))
  expect_identical(ok$ref_line_colors, list(LLN = "#4DAF4A", ULN = "red"))
  expect_error(viz_check(spec, list(ref_line_colors = list("red"))))
  expect_null(viz_check(spec, list())$ref_line_colors)
})
