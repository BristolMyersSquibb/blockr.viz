small_spec <- function() {
  viz_spec(
    "chart",
    type = viz_arg("enum", default = "bar", values = c("bar", "line"),
                   gear = gear("Chart type", "tiles", "Chart type",
                               labels = c("Bar", "Line")),
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

check_fails <- function(spec, pattern) {
  expect_error(viz_spec_check(spec), pattern)
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
  expect_identical(viz_spec_check(s), s)
})

test_that("an unnamed list of arguments is spliced in", {
  shared <- list(title = viz_arg("template", null = "auto"),
                 download = viz_arg("bool", default = TRUE))
  s <- viz_spec("table", layout = viz_arg("enum", default = "records",
                                          values = "records"), shared)
  expect_named(s$args, c("layout", "title", "download"))
})

test_that("building a spec checks nothing", {
  s <- viz_spec("chart", a = viz_arg("colour"), a = viz_arg("enum"))
  expect_s3_class(s, "viz_spec")
})

test_that("viz_spec_check() lists every problem", {
  a <- viz_arg("string")
  check_fails(viz_spec("chart", a), "every argument must be named")
  check_fails(viz_spec("chart", x = a, x = a), "`x` is named twice")
  check_fails(viz_spec("chart", x = viz_arg(type_values(of = "y"))),
              "`x` refers to unknown argument `y`")
  check_fails(
    viz_spec("chart", x = viz_arg("integer", default = 1L,
                                  gear = gear("F", "number", "N",
                                              when = "facet"))),
    "refers to unknown argument `facet`"
  )
  check_fails(viz_spec("chart", x = list(type = "string")), "not a viz_arg")
  check_fails(viz_spec("chart", a = viz_arg("colour"), b = viz_arg("enum")),
              "`a` has unknown type \"colour\"; `b` needs `values`")
})

test_that("viz_spec_check() checks argument fields", {
  bad <- list(
    "needs `values`" = viz_arg("string", values = "a"),
    "non-empty character" = viz_arg("enum", values = character()),
    "not a number or integer" = viz_arg("string", bounds = bounds(0)),
    "not a column type" = viz_arg("string", also = "auto"),
    "neither FALSE nor a text" = viz_arg("string", null = TRUE),
    "not from gear" = viz_arg("string", gear = list()),
    "not a character vector or except" = viz_arg("string", applies = 1),
    "not TRUE or FALSE" = viz_arg("string", slot = NA),
    "unknown gear control" = viz_arg("string",
                                     gear = gear("L", "dropdown", "X")),
    "2 gear labels for 3 values" = viz_arg(
      "enum", default = "a", values = c("a", "b", "c"),
      gear = gear("L", "select", "X", labels = c("A", "B"))
    ),
    "cannot be a title slot" = viz_arg("map", default = list(), slot = TRUE),
    "no default and no `null`" = viz_arg("column", required = TRUE,
                                         null = "x"),
    "default that fails" = viz_arg("integer", default = 0L,
                                   bounds = bounds(1)),
    "example that fails" = viz_arg("enum", default = "a", values = "a",
                                   example = "z")
  )
  for (p in names(bad)) {
    check_fails(viz_spec("chart", a = bad[[p]]), p)
  }
})

test_that("bool gear labels are the TRUE and FALSE labels", {
  s <- viz_spec("chart", a = viz_arg("bool", default = FALSE,
                                     gear = gear("L", "segmented", "A",
                                                 labels = c("On", "Off"))))
  expect_identical(viz_spec_check(s), s)
  s$args$a$gear$labels <- "On"
  check_fails(s, "1 gear labels for 2 values")
})

test_that("record fields are checked inside their record", {
  check_fails(viz_spec("chart", h = viz_arg(type_record(), null = "x")),
              "without named fields")
  check_fails(
    viz_spec("chart", h = viz_arg(type_record(a = "bogus"), null = "x")),
    "`h` has field `a` has unknown type"
  )
  check_fails(
    viz_spec("chart", h = viz_arg(type_record(v = type_values("col")),
                                  null = "x")),
    "`v` refers to unknown argument `col`"
  )
  ok <- viz_spec("chart", h = viz_arg(
    type_record(column = "column", values = type_values(of = "column")),
    null = "x"
  ))
  expect_identical(viz_spec_check(ok), ok)
})

test_that("viz_block_spec() builds and checks a block's spec once", {
  local_mocked_bindings(VIZ_BLOCK_SPECS = list(demo = small_spec))
  spec_cache$demo <- NULL
  withr::defer(spec_cache$demo <- NULL)
  expect_identical(viz_block_spec("demo"), small_spec())
  expect_true(!is.null(spec_cache$demo))
  expect_error(viz_block_spec("nope"), "No spec for block `nope`")
})

test_that("opt() defaults are read when the block is built", {
  s <- small_spec()
  withr::local_options(test.viz.facet_cols = NULL)
  expect_null(spec_default(s$args$facet_cols))
  withr::local_options(test.viz.facet_cols = 3L)
  expect_identical(spec_default(s$args$facet_cols), 3L)
  expect_identical(spec_default(s$args$type), "bar")
})
