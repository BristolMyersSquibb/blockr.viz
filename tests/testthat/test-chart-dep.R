# The chart block's UI loads the chart scripts in the order
# inst/js/chart/scripts.txt lists them, after the theme register.

test_that("the chart dependency lists every chart script, binding last", {
  ui <- new_chart_block()$expr_ui("blk")
  deps <- htmltools::findDependencies(ui)
  names <- vapply(deps, `[[`, character(1), "name")
  chart <- deps[[match("chart-js", names)]]
  expect_identical(chart$script[[1]], "drilldown-theme-register.js")
  expect_identical(chart$script[-1], chart_scripts())
  expect_identical(utils::tail(chart$script, 1), "chart/binding.js")
  expect_true(all(file.exists(file.path(chart$src$file, chart$script))))
})
