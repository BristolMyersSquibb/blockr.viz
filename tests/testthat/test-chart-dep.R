# The chart block's UI loads the JS core and the specs, then the chart
# scripts in the order inst/js/chart/scripts.txt lists them, view.js last.

test_that("the chart dependency lists the core, then every chart script", {
  ui <- new_chart_block()$expr_ui("blk")
  deps <- htmltools::findDependencies(ui)
  names <- vapply(deps, `[[`, character(1), "name")
  core <- deps[[match("blockr-viz-core", names)]]
  chart <- deps[[match("chart-js", names)]]
  expect_lt(match("blockr-viz-core", names), match("chart-js", names))
  expect_identical(core$script[[1]], "core/namespace.js")
  expect_true(all(c("spec/chart.js", "core/gear/engine.js") %in% core$script))
  expect_identical(chart$script[-(1:2)], file.path("chart", js_script_list("chart")))
  expect_identical(utils::tail(chart$script, 1), "chart/view.js")
  for (d in list(core, chart)) {
    expect_true(all(file.exists(file.path(d$src$file, d$script))))
  }
})
