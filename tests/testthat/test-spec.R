small_spec <- function() {
  viz_spec(
    "chart",
    type = viz_arg("enum", default = "bar", values = c("bar", "line"),
                   gear = gear("Chart type", "tiles", "Chart type"),
                   slot = TRUE),
    x = viz_arg("column", null = "unset",
                gear = gear("Mapping", "column", "X"), slot = TRUE),
    facet = viz_arg("column", null = "one panel"),
    facet_cols = viz_arg("integer", default = opt("test.viz.facet_cols"),
                         bounds = bounds(1), null = "fit the width",
                         gear = gear("Facets", "select", "Panel columns",
                                     when = "facet")),
    x_lines = viz_arg(type_values(of = "x"), null = "none"),
    x_ranges = viz_arg(type_records(name = "string", from = "string",
                                    to = "string"), null = "none"),
    connect = viz_arg("enum", default = "straight",
                      values = c("straight", "monotone"),
                      applies = "line")
  )
}

test_that("viz_spec() collects named arguments", {
  s <- small_spec()
  expect_s3_class(s, "viz_spec")
  expect_identical(s$block, "chart")
  expect_named(s$args, c("type", "x", "facet", "facet_cols", "x_lines",
                         "x_ranges", "connect"))
  expect_identical(s$args$x$type$name, "column")
  expect_identical(s$args$x_lines$type$of, "x")
  expect_named(s$args$x_ranges$type$fields, c("name", "from", "to"))
})

test_that("an unnamed list of arguments is spliced in", {
  shared <- list(title = viz_arg("template", null = "auto"),
                 download = viz_arg("bool", default = TRUE))
  s <- viz_spec("table", layout = viz_arg("enum", default = "records",
                                          values = "records"), shared)
  expect_named(s$args, c("layout", "title", "download"))
})

test_that("viz_spec() refuses broken specs", {
  a <- viz_arg("string")
  expect_error(viz_spec("chart", a), "must be a list of named")
  expect_error(viz_spec("chart", x = a, x = a), "names x twice")
  expect_error(viz_spec("chart", x = viz_arg(type_values(of = "y"))),
               "unknown argument `y`")
  expect_error(
    viz_spec("chart", x = viz_arg("integer", gear = gear("F", "number", "N",
                                                          when = "facet"))),
    "unknown argument `facet`"
  )
  expect_error(viz_spec("chart", x = list(type = "string")), "not a viz_arg")
})

test_that("viz_arg() checks its fields", {
  expect_error(viz_arg("colour"), "Unknown argument type")
  expect_error(viz_arg("values"), "use type_values")
  expect_error(viz_arg("enum"), "need it")
  expect_error(viz_arg("string", values = "a"), "need it")
  expect_error(viz_arg("enum", values = character()), "non-empty")
  expect_error(viz_arg("string", bounds = bounds(0)), "number and integer")
  expect_error(viz_arg("string", also = "auto"), "column arguments")
  expect_error(viz_arg("string", null = TRUE), "FALSE or a text")
  expect_error(viz_arg("string", gear = list()), "gear()")
  expect_error(viz_arg("string", applies = 1), "applies")
  expect_error(viz_arg("string", slot = NA))
  expect_s3_class(viz_arg("column", also = "auto", null = "off"), "viz_arg")
})

test_that("gear() knows its controls", {
  g <- gear("Look", "segmented", "Legend", labels = c("Bottom", "Top"))
  expect_s3_class(g, "viz_gear")
  expect_identical(g$control, "segmented")
  expect_error(gear("Look", "dropdown", "Legend"), "Unknown gear control")
})

test_that("record types need named fields", {
  expect_error(type_record(), "named fields")
  expect_error(type_records("string"), "named fields")
  expect_error(type_record(a = "bogus"), "Unknown argument type")
  r <- type_record(column = "column", values = type_values(of = "column"))
  expect_identical(r$fields$values$of, "column")
})

test_that("a values field must name a field or argument in the spec", {
  expect_error(
    viz_spec("chart", h = viz_arg(type_record(values = type_values("col")))),
    "unknown argument `col`"
  )
})

test_that("opt() defaults are read when the block is built", {
  s <- small_spec()
  withr::local_options(test.viz.facet_cols = NULL)
  expect_null(spec_default(s$args$facet_cols))
  withr::local_options(test.viz.facet_cols = 3L)
  expect_identical(spec_default(s$args$facet_cols), 3L)
  expect_identical(spec_default(s$args$type), "bar")
})

test_that("applies takes a list or except()", {
  s <- small_spec()
  expect_true(spec_applies(s$args$x, "pie"))
  expect_true(spec_applies(s$args$connect, "line"))
  expect_false(spec_applies(s$args$connect, "bar"))
  e <- viz_arg("column", applies = except("pie", "treemap"))
  expect_false(spec_applies(e, "pie"))
  expect_true(spec_applies(e, "bar"))
})
