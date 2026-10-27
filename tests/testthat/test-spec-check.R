json <- function(txt) jsonlite::fromJSON(txt, simplifyVector = FALSE)

check <- function(type, x, ...) {
  viz_check_value(viz_arg(type, ...), x, "a")
}

test_that("column takes a name, or an `also` keyword", {
  expect_identical(check("column", "cyl", null = "none"), "cyl")
  expect_null(check("column", NULL, null = "none"))
  expect_null(check("column", "", null = "none"))
  expect_identical(check("column", "auto", also = "auto", null = "off"),
                   "auto")
  expect_error(check("column", c("a", "b"), null = "none"),
               "`a` must be a single string")
  expect_error(check("column", 1, null = "none"), "single string, not 1")
})

test_that("a NULL for a non-null argument gives the default", {
  expect_identical(check("enum", NULL, default = "bar",
                         values = c("bar", "line")), "bar")
  expect_error(check("string", NULL), "`a` cannot be NULL")
})

test_that("enum and enums check their values", {
  expect_error(check("enum", "pie", default = "bar", values = c("bar", "line")),
               "`a` must be one of \"bar\", \"line\", not \"pie\"")
  expect_identical(check("enums", json('["a","b"]'), values = c("a", "b"),
                         null = "none"), c("a", "b"))
  expect_null(check("enums", list(), values = "a", null = "none"))
  expect_error(check("enums", c("a", "z"), values = "a", null = "none"),
               "not \"z\"")
})

test_that("JSON arrays and plain vectors give the same value", {
  expect_identical(check("columns", json('["x","y"]'), null = "none"),
                   c("x", "y"))
  expect_identical(check("columns", c("x", "y"), null = "none"), c("x", "y"))
  expect_identical(check("columns", json('["x"]'), null = "none"), "x")
  expect_null(check("columns", json("[]"), null = "none"))
  expect_error(check("columns", json('["x", null]'), null = "none"),
               "without NA")
  expect_identical(check("columns", factor("x"), null = "none"), "x")
})

test_that("numbers and integers respect bounds", {
  expect_identical(check("number", 2L, default = 1, bounds = bounds(0.5, 3)),
                   2)
  expect_error(check("number", 4, default = 1, bounds = bounds(0.5, 3)),
               "at least 0.5 and at most 3, not 4")
  expect_error(check("number", 0, default = 45,
                     bounds = bounds(0, min_open = TRUE)), "above 0")
  expect_error(check("number", Inf, default = 1), "finite")
  expect_identical(check("integer", 12, default = 12L, bounds = bounds(1)),
                   12L)
  expect_error(check("integer", 1.5, default = 1L), "whole number")
  expect_error(check("number", "3", default = 1), "single finite number")
})

test_that("bool is TRUE or FALSE", {
  expect_true(check("bool", json("true"), default = FALSE))
  expect_error(check("bool", "on", default = FALSE), "TRUE or FALSE")
  expect_error(check("bool", NA, default = FALSE), "TRUE or FALSE")
})

test_that("numbers keep their names as labels", {
  expect_identical(check("numbers", c(Target = 4, 6), null = "none"),
                   c(Target = 4, 6))
  expect_identical(
    check("numbers", json('[{"value":4,"label":"Target"},{"value":6}]'),
          null = "none"),
    c(Target = 4, 6)
  )
  expect_identical(check("numbers", json("[4, 6]"), null = "none"), c(4, 6))
  expect_null(check("numbers", list(), null = "none"))
  expect_error(check("numbers", c(1, NA), null = "none"), "finite")
})

test_that("values are numbers when they all are, else text", {
  x <- viz_arg("column", null = "none")
  s <- viz_spec("chart", x = x,
                v = viz_arg(type_values(of = "x"), null = "none"))
  v <- function(val) viz_check_value(s$args$v, val, "v")
  expect_identical(v(json("[1, 2.5]")), c(1, 2.5))
  expect_identical(v(json('["a", "b"]')), c("a", "b"))
  expect_identical(v(as.Date("2024-01-02")), "2024-01-02")
  expect_identical(v(c(Listed = "2024-01-02")), c(Listed = "2024-01-02"))
  expect_error(v(list(1, list(2))), "single values")
})

test_that("format takes a keyword or decimals from 0 to 6", {
  f <- function(x) check("format", x, default = "number")
  expect_identical(f("compact"), "compact")
  expect_identical(f(2), 2L)
  expect_identical(f("2"), 2L)
  expect_error(f(7), "from 0 to 6, not 7")
  expect_error(f("pretty"), "not \"pretty\"")
})

test_that("records are checked field by field", {
  ranges <- viz_arg(type_records(name = "string", from = "string",
                                 to = "string"), null = "none")
  x <- json('[{"name":"Recession","from":"2008-01-01","to":"2009-06-30"}]')
  out <- viz_check_value(ranges, x, "x_ranges")
  expect_identical(out[[1L]]$name, "Recession")
  expect_error(viz_check_value(ranges, json('[{"start":"x"}]'), "x_ranges"),
               "has no field `start`")
  expect_null(viz_check_value(ranges, list(), "x_ranges"))
  hl <- viz_arg(type_record(column = "column",
                            values = type_values(of = "column")),
                null = "none")
  out <- viz_check_value(hl, json('{"column":"country","values":["CH"]}'),
                         "highlight")
  expect_identical(out, list(column = "country", values = "CH"))
  expect_identical(viz_check_value(hl, list(column = "c"), "highlight"),
                   list(column = "c", values = NULL))
  expect_error(viz_check_value(hl, list("c"), "highlight"), "named list")
})

test_that("map takes any named list", {
  m <- viz_arg("map", default = list())
  expect_identical(viz_check_value(m, list(site = "A"), "script_values"),
                   list(site = "A"))
  expect_identical(viz_check_value(m, list(), "script_values"), list())
  expect_error(viz_check_value(m, list(1), "script_values"), "named list")
})

test_that("a spec refuses a default that fails its type", {
  expect_error(
    viz_spec("chart", n = viz_arg("integer", default = 0L, bounds = bounds(1))),
    "`n` must be at least 1"
  )
})

spec_abc <- function() {
  viz_spec(
    "chart",
    type = viz_arg("enum", default = "bar", values = c("bar", "line")),
    color = viz_arg("column", null = "no color"),
    bins = viz_arg("integer", default = 30L, bounds = bounds(1, 200))
  )
}

test_that("viz_check() fills defaults and names unknown arguments", {
  s <- spec_abc()
  out <- viz_check(s, list(color = "cyl"))
  expect_identical(out, list(type = "bar", color = "cyl", bins = 30L))
  expect_error(viz_check(s, list(colour = "cyl")),
               "unknown argument `colour`. Did you mean `color`?")
  expect_error(viz_check(s, list(zzzzzz = 1)), "unknown argument `zzzzzz`.$")
  expect_error(viz_check(s, list(type = "pie")), "`type` must be one of")
})

test_that("viz_heal() resets bad values and drops unknown names", {
  s <- spec_abc()
  expect_warning(
    out <- viz_heal(s, list(type = "radar", color = list(), old = 1)),
    "Restoring a chart: reset `type` to the default; dropped `old`."
  )
  expect_identical(out, list(type = "bar", color = NULL, bins = 30L))
  expect_silent(viz_heal(s, list(type = "line")))
})
