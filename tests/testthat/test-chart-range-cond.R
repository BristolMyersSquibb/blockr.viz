# The emitted condition of a chart's range selection (chart_range_cond()).

test_that("a point click reads as == on both axes", {
  rng <- list(x_col = "num", x_range = c(4, 4), y_col = "num2",
              y_range = c(1, 1))
  expect_identical(chart_range_cond(rng),
                   quote(.data[["num"]] == 4 & .data[["num2"]] == 1))
})

test_that("a brush keeps between(), even with equal ends on one axis", {
  expect_identical(
    chart_range_cond(list(x_col = "num", x_range = c(4, 4))),
    quote(dplyr::between(.data[["num"]], 4, 4))
  )
  expect_identical(
    chart_range_cond(list(x_col = "num", x_range = c(2, 8), y_col = "num2",
                          y_range = c(1, 1))),
    quote(dplyr::between(.data[["num"]], 2, 8) &
            dplyr::between(.data[["num2"]], 1, 1))
  )
})

test_that("a point on a date axis keeps the date literal", {
  d <- data.frame(day = as.Date("2024-01-08"), num = 2)
  # the browser sends a time axis value as milliseconds since 1970
  ms <- as.numeric(as.Date("2024-01-08")) * 86400000
  rng <- list(x_col = "day", x_range = c(ms, ms), y_col = "num",
              y_range = c(2, 2))
  expect_identical(
    chart_range_cond(rng, d),
    quote(.data[["day"]] == as.Date("2024-01-08") & .data[["num"]] == 2)
  )
})
