dep_scripts <- function(tags) {
  deps <- htmltools::resolveDependencies(htmltools::findDependencies(tags))
  unlist(lapply(deps, function(d) {
    s <- d$script
    if (is.list(s)) vapply(s, function(x) x$src %||% x, "") else s
  }))
}

test_that("the busy cue comes with the core, so both blocks have it", {
  expect_true("core/busy.js" %in% viz_core_dep()$script)
  expect_true("core/busy.js" %in% dep_scripts(table_view_dep()))
  expect_true("core/busy.js" %in% dep_scripts(drilldown_chart_dep()))
  expect_true(file.exists(system.file("js", "core", "busy.js",
                                      package = "blockr.viz")))
})
