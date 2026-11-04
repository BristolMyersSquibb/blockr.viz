json <- function(txt) jsonlite::fromJSON(txt, simplifyVector = FALSE)

limits <- function(x) {
  viz_check_value(viz_arg("limits", null = "auto"), x, "y_limits")
}

test_that("limits take two numbers, NA at an end for auto", {
  expect_identical(limits(c(0, 120)), c(0, 120))
  expect_identical(limits(c(NA, 120)), c(NA, 120))
  expect_identical(limits(c(0L, NA)), c(0, NA))
  expect_null(limits(NULL))
  expect_null(limits(c(NA, NA)))
})

test_that("limits read the JSON the gear sends", {
  expect_identical(limits(json("[null, 120]")), c(NA, 120))
  expect_identical(limits(json("[\"0\", \"5\"]")), c(0, 5))
  expect_null(limits(json("[null, null]")))
})

test_that("date limits are ISO text", {
  expect_identical(limits(as.Date(c("2020-01-01", NA))),
                   c("2020-01-01", NA))
  expect_identical(limits(c("2020-01-01", "2020-06-30")),
                   c("2020-01-01", "2020-06-30"))
})

test_that("limits refuse a wrong length, infinity and a reversed pair", {
  expect_error(limits(1), "two values")
  expect_error(limits(c(1, 2, 3)), "two values")
  expect_error(limits(c(-Inf, 2)), "finite or NA")
  expect_error(limits(c(5, 1)), "from below to")
  expect_error(limits(c(1, 1)), "from below to")
})

test_that("limits go to the browser with null for auto", {
  w <- wire_value(spec_type("limits"), c(NA, 120))
  expect_identical(jsonlite::toJSON(w, auto_unbox = TRUE, null = "null"),
                   jsonlite::toJSON(list(NULL, 120), auto_unbox = TRUE,
                                    null = "null"))
  expect_null(wire_value(spec_type("limits"), NULL))
})

test_that("the chart block takes x_limits and y_limits", {
  b <- new_chart_block(type = "line", x = "t", y = "v",
                       x_limits = c("2020-02-01", NA), y_limits = c(0, NA))
  expect_s3_class(b, "chart_block")
  expect_error(new_chart_block(type = "line", y_limits = c(5, 1)),
               "from below to")
  spec <- viz_block_spec("chart")
  expect_identical(spec$args$x_limits$type$name, "limits")
  expect_null(spec$args$y_limits$gear)
  expect_true("boxplot" %in% spec$args$y_limits$applies)
  expect_false("boxplot" %in% spec$args$x_limits$applies)
})
