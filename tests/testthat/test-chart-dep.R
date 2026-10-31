# The chart block's UI loads the JS core and the specs, then the chart
# scripts in the order inst/js/chart/scripts.txt lists them, view.js last.

test_that("the chart dependency lists the core, then every chart script", {
  ui <- new_chart_block()$expr_ui("blk")
  deps <- htmltools::findDependencies(ui)
  names <- vapply(deps, `[[`, character(1), "name")
  core <- deps[[match("blockr-viz-core", names)]]
  chart <- deps[[match("chart-js", names)]]
  expect_lt(match("blockr-viz-core", names), match("chart-js", names))
  expect_identical(core$script[[1L]], "js/core/namespace.js")
  expect_true(all(c("js/spec/chart.js", "js/core/gear/engine.js") %in%
                    core$script))
  # The frame's and the gear's styles come with the core.
  expect_identical(core$stylesheet, c("css/frame.css", "css/gear.css"))
  expect_identical(chart$script,
                   c("drilldown-theme-register.js",
                     file.path("chart", js_script_list("chart"))))
  # The busy cue loads with the core: every view's output answers it.
  expect_true("js/core/busy.js" %in% core$script)
  expect_identical(utils::tail(chart$script, 1), "chart/view.js")
  for (d in list(core, chart)) {
    files <- file.path(d$src$file, c(d$script, d$stylesheet))
    expect_true(all(file.exists(files)))
  }
})

test_that("a table-only page gets the core, not the chart's styles", {
  deps <- htmltools::findDependencies(table_view_dep())
  core <- deps[[match("blockr-viz-core", vapply(deps, `[[`, "", "name"))]]
  expect_true("js/core/busy.js" %in% core$script)
  expect_false("chart-css" %in% vapply(deps, `[[`, "", "name"))
})

test_that("the table's cache number covers its stylesheets", {
  deps <- htmltools::findDependencies(table_view_dep())
  tab <- deps[[match("blockr-viz-table", vapply(deps, `[[`, "", "name"))]]
  own <- setdiff(tab$script, "js/vendor/snapdom.js")
  expect_true("css/table.css" %in% tab$stylesheet)
  expect_identical(tab$version, js_version(c(own, tab$stylesheet), "."))
  expect_false(identical(tab$version, js_version(own, ".")))
})
