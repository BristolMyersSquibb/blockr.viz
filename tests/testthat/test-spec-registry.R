reg_spec <- function() {
  viz_spec(
    "chart",
    type = viz_arg("enum", default = "bar", values = c("bar", "line"),
                   slot = TRUE, describe = "Chart type.", example = "line"),
    x = viz_arg("column", null = "unset", slot = TRUE, describe = "X.",
                example = "cyl"),
    bins = viz_arg("integer", default = 30L, describe = "Bins."),
    tooltip = viz_arg("columns", null = "none", describe = "Tooltip."),
    y_lines = viz_arg("numbers", null = "none", slot = TRUE),
    x_lines = viz_arg(type_values(of = "x"), null = "none", slot = TRUE),
    highlight = viz_arg(type_record(column = "column",
                                    values = type_values(of = "column")),
                        null = "none"),
    x_ranges = viz_arg(type_records(name = "string", from = "string",
                                    to = "string"), null = "none"),
    script = viz_arg("string", null = "none", ai = FALSE)
  )
}

test_that("the registry lists the ai arguments with core types", {
  r <- viz_registry_args(reg_spec())
  expect_true(blockr.core::is_arg_specs(r))
  expect_named(r, c("type", "x", "bins", "tooltip", "y_lines", "x_lines",
                    "highlight", "x_ranges"))
  expect_identical(blockr.core::arg_spec_description(r$type), "Chart type.")
  expect_identical(blockr.core::arg_spec_example(r$x), "cyl")
  expect_identical(blockr.core::arg_spec_type(r$type)$enum,
                   I(c("bar", "line")))
  expect_identical(blockr.core::arg_spec_type(r$tooltip)$type, "array")
  expect_identical(blockr.core::arg_spec_type(r$bins)$type, "integer")
  expect_null(blockr.core::arg_spec_type(r$x_lines))
  hl <- blockr.core::arg_spec_type(r$highlight)
  expect_identical(hl$type, "object")
  expect_named(hl$properties, "column")
  expect_identical(blockr.core::arg_spec_type(r$x_ranges)$items$type,
                   "object")
})

test_that("required record fields are required in the schema", {
  s <- viz_spec("table", shadings = viz_arg(
    type_records(spec = viz_spec("shading",
                                 col = viz_arg("column", required = TRUE),
                                 low = viz_arg("number", null = "min"))),
    null = "none"
  ))
  t <- blockr.core::arg_spec_type(viz_registry_args(s)$shadings)
  expect_identical(t$items$required, I("col"))
})

test_that("records_by has no schema type, only its description", {
  s <- viz_spec("table", summaries = viz_arg(
    type_records_by("type", list(a = viz_spec("a", n = viz_arg("bool",
                                                               default = TRUE)))),
    default = list(), describe = "Summaries."
  ))
  r <- viz_registry_args(s)
  expect_null(blockr.core::arg_spec_type(r$summaries))
  expect_identical(blockr.core::arg_spec_description(r$summaries),
                   "Summaries.")
})

test_that("slots print single values as they are and lists joined", {
  s <- reg_spec()
  v <- viz_check(s, list(x = "cyl", y_lines = c(Target = 4, 6.5),
                         x_lines = c("2024-01-02", "2024-02-01")))
  out <- viz_slot_values(s, v)
  expect_named(out, c("type", "x", "y_lines", "x_lines"))
  expect_identical(out$type, "bar")
  expect_identical(out$x, "cyl")
  expect_identical(out$y_lines, "4, 6.5")
  expect_identical(out$x_lines, "2024-01-02, 2024-02-01")
  expect_null(viz_slot_values(s, viz_check(s, list()))$y_lines)
})
