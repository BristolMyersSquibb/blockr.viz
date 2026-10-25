# A Date or date-time column on a line or scatter's x is a time axis: the
# column metadata says so, and a brush or point click taken there (in ms)
# filters as a date.

test_that("dd_col_meta() marks date and date-time columns", {
  d <- data.frame(
    d = as.Date("2020-01-01") + 0:1,
    t = as.POSIXct("2020-01-01 10:00:00", tz = "UTC") + 0:1,
    v = 1:2,
    s = c("a", "b")
  )
  meta <- dd_col_meta(d)
  by <- stats::setNames(meta, vapply(meta, `[[`, "", "name"))
  expect_equal(by$d$temporal, "date")
  expect_equal(by$t$temporal, "datetime")
  expect_null(by$v$temporal)
  expect_null(by$s$temporal)
  # The type the gear's pickers read does not change.
  expect_equal(by$d$type, "categorical")
})

test_that("chart_time_literal() turns browser ms into a date literal", {
  ms <- as.numeric(as.POSIXct("2020-03-01", tz = "UTC")) * 1000
  expect_identical(chart_time_literal(ms, as.Date("2020-01-01")),
                   quote(as.Date("2020-03-01")))
  ms_t <- as.numeric(as.POSIXct("2020-03-01 12:30:00", tz = "UTC")) * 1000
  expect_identical(
    chart_time_literal(ms_t, as.POSIXct("2020-01-01", tz = "Europe/Zurich")),
    quote(as.POSIXct("2020-03-01 12:30:00", tz = "Europe/Zurich"))
  )
  expect_identical(chart_time_literal(5, 1:3), 5)
  expect_identical(chart_time_literal("a", as.Date("2020-01-01")), "a")
})

test_that("a range taken on a Date axis filters the rows in it", {
  d <- data.frame(day = as.Date("2020-01-01") + 0:9, v = 1:10)
  ms <- function(x) as.numeric(as.POSIXct(x, tz = "UTC")) * 1000
  rng <- list(x_col = "day", x_range = c(ms("2020-01-03"), ms("2020-01-05")))
  cond <- chart_range_cond(rng, d)
  out <- dplyr::filter(d, !!cond)
  expect_equal(out$v, 3:5)
  expect_match(paste(deparse(cond), collapse = ""), "as.Date(\"2020-01-03\")",
               fixed = TRUE)
  # Without the data the condition is the plain numeric one, as before.
  expect_match(paste(deparse(chart_range_cond(rng)), collapse = ""),
               "between")
})
