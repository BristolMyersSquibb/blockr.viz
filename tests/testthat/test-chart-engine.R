# options(blockr.viz.chart_engine = "v2") swaps chart.js for the v2 scripts in
# the chart block's UI. One engine per page: both register the same binding.

chart_ui <- function() {
  blk <- new_chart_block()
  blk$expr_ui("blk")
}

dep_names <- function(ui) {
  vapply(htmltools::findDependencies(ui), `[[`, character(1), "name")
}

container_class <- function(ui) {
  html <- as.character(htmltools::renderTags(ui)$html)
  regmatches(html, regexpr('id="blk-drilldown_block" class="[^"]*"', html))
}

test_that("v1 is the default engine", {
  withr::local_options(blockr.viz.chart_engine = NULL)
  expect_identical(chart_engine(), "v1")
  ui <- chart_ui()
  expect_true("chart-js" %in% dep_names(ui))
  expect_false("chart-v2-js" %in% dep_names(ui))
  expect_identical(
    container_class(ui),
    'id="blk-drilldown_block" class="drilldown-chart-container"'
  )
})

test_that("the option loads the v2 scripts instead of chart.js", {
  withr::local_options(blockr.viz.chart_engine = "v2")
  expect_identical(chart_engine(), "v2")
  ui <- chart_ui()
  deps <- htmltools::findDependencies(ui)
  names <- dep_names(ui)
  expect_true("chart-v2-js" %in% names)
  expect_false("chart-js" %in% names)
  v2 <- deps[[match("chart-v2-js", names)]]
  expect_identical(v2$script[[1]], "drilldown-theme-register.js")
  expect_identical(v2$script[-1], chart_v2_scripts())
  expect_identical(utils::tail(v2$script, 1), "chart-v2/binding.js")
  expect_true(all(file.exists(file.path(v2$src$file, v2$script))))
  expect_match(container_class(ui), "drilldown-chart-container dd-engine-v2")
})

test_that("an unknown engine falls back to v1", {
  withr::local_options(blockr.viz.chart_engine = "v3")
  expect_identical(chart_engine(), "v1")
})
