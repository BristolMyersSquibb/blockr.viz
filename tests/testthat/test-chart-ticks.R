json <- function(x) jsonlite::fromJSON(x, simplifyVector = TRUE)

ticks <- function(x) {
  viz_check_value(viz_arg("ticks", null = "auto"), x, "y_ticks")
}

test_that("one number is a step", {
  expect_identical(ticks(10), 10)
  expect_identical(ticks(2.5), 2.5)
  expect_identical(ticks("25"), 25)
  expect_identical(ticks("every 25"), 25)
  expect_null(ticks(NULL))
  expect_null(ticks(NA))
})

test_that("a date step takes a count and a unit", {
  expect_identical(ticks("7 days"), "7 days")
  expect_identical(ticks("1 day"), "1 day")
  expect_identical(ticks("month"), "1 month")
  expect_identical(ticks("Months"), "1 month")
  expect_identical(ticks("every 2 weeks"), "2 weeks")
  expect_identical(ticks("3 quarter"), "3 quarters")
})

test_that("two values or more are the ticks, sorted", {
  expect_identical(ticks(c(50, 0, 25, 100)), c(0, 25, 50, 100))
  expect_identical(ticks(c("2024-03-01", "2024-01-01")),
                   c("2024-01-01", "2024-03-01"))
  expect_identical(ticks(as.Date(c("2024-01-01", "2024-02-01"))),
                   c("2024-01-01", "2024-02-01"))
  expect_identical(ticks(json("[0, 25, 50]")), c(0, 25, 50))
  expect_identical(ticks(json("[\"0\", \"25\"]")), c(0, 25))
})

test_that("ticks refuse what is not a step or a list", {
  expect_error(ticks(0), "above 0")
  expect_error(ticks(-5), "above 0")
  expect_error(ticks("fortnight"), "step such as")
  expect_error(ticks("1.5 days"), "whole days")
  expect_error(ticks(c(1, 1)), "two values or more")
  expect_error(ticks(c("a", "b")), "dates")
  expect_error(ticks(c(1, Inf)), "finite")
})

test_that("ticks go to the browser as a value or an array", {
  wire <- function(x) {
    jsonlite::toJSON(wire_value(spec_type("ticks"), x), auto_unbox = TRUE)
  }
  expect_identical(as.character(wire(25)), "25")
  expect_identical(as.character(wire("7 days")), "\"7 days\"")
  expect_identical(as.character(wire(c(0, 50))), "[0,50]")
  expect_null(wire_value(spec_type("ticks"), NULL))
})

test_that("the chart block takes x_ticks and y_ticks", {
  b <- new_chart_block(type = "line", x = "t", y = "v",
                       x_ticks = "7 days", y_ticks = 25)
  expect_s3_class(b, "chart_block")
  expect_error(new_chart_block(type = "line", y_ticks = -1), "above 0")
  spec <- viz_block_spec("chart")
  expect_identical(spec$args$y_ticks$type$name, "ticks")
  expect_identical(spec$args$y_ticks$gear$control, "ticks")
  expect_identical(spec$args$x_ticks$applies, spec$args$x_limits$applies)
  expect_identical(spec$args$y_ticks$applies, spec$args$y_limits$applies)
})

test_that("the sentence prints ticks as every or at", {
  expect_identical(slot_ticks_text(25), "every 25")
  expect_identical(slot_ticks_text("7 days"), "every 7 days")
  expect_identical(slot_ticks_text("1 month"), "every month")
  expect_identical(slot_ticks_text(c(0, 25, 50)), "at 0, 25, 50")
  expect_null(slot_ticks_text(NULL))
  spec <- viz_block_spec("chart")
  v <- viz_slot_values(spec, list(y_ticks = 25, x_ticks = NULL))
  expect_identical(v$y_ticks, "every 25")
  expect_null(v$x_ticks)
})
