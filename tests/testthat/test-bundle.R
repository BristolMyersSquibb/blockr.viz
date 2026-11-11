# The bundles the page loads (inst/js/dist, inst/css/dist) are written by
# tools/bundle.js. The first line of each holds an md5 of its sources and the
# second their names; a bundle whose line no longer matches the sources is
# stale: run `node tools/bundle.js` and commit the result.

# The md5 tools/bundle.js writes: of "// <name>\n" and the text without
# carriage returns, for every source in order.
bundle_hash <- function(dir, files) {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  con <- file(tmp, "wb")
  for (f in files) {
    raw <- readBin(file.path(dir, f), "raw", file.size(file.path(dir, f)))
    writeBin(c(charToRaw(paste0("// ", f, "\n")), raw[raw != as.raw(13L)]), con)
  }
  close(con)
  unname(tools::md5sum(tmp))
}

# The sources of each bundle, as the dev_js dependencies list them.
bundle_sources <- function() {
  core <- c(file.path("core", js_script_list("core")), "spec/chart.js", "spec/table.js")
  list(
    "js/dist/core.js" = core,
    "js/dist/chart.js" = c("drilldown-theme-register.js",
                           file.path("chart", js_script_list("chart"))),
    "js/dist/table.js" = file.path("table", js_script_list("table")),
    "css/dist/core.css" = css_list("core"),
    "css/dist/table.css" = css_list("table")
  )
}

test_that("every bundle is built from the current sources", {
  inst <- system.file(package = "blockr.viz")
  for (out in names(bundle_sources())) {
    files <- bundle_sources()[[out]]
    dir <- file.path(inst, dirname(dirname(out)))
    head <- readLines(file.path(inst, out), n = 2L, warn = FALSE)
    names_line <- sub("^(//|/\\*) sources: (.*?)( \\*/)?$", "\\2", head[[2L]])
    expect_identical(strsplit(names_line, " ", fixed = TRUE)[[1L]], files,
                     label = paste(out, "sources"))
    expect_true(grepl(bundle_hash(dir, files), head[[1L]], fixed = TRUE),
                label = paste(out, "is current (run `node tools/bundle.js`)"))
  }
})

test_that("the page gets the bundles; dev_js the single sources", {
  deps <- function() {
    d <- htmltools::findDependencies(new_chart_block()$expr_ui("blk"))
    stats::setNames(d, vapply(d, `[[`, "", "name"))
  }
  b <- deps()
  expect_identical(b[["blockr-viz-core"]]$script, "js/dist/core.js")
  expect_identical(b[["blockr-viz-core"]]$stylesheet, "css/dist/core.css")
  expect_identical(b[["chart-js"]]$script, "dist/chart.js")
  expect_identical(b[["blockr-viz-table"]]$script, "js/dist/table.js")
  expect_identical(b[["blockr-viz-table"]]$stylesheet, "css/dist/table.css")
  withr::local_options(blockr.viz.dev_js = TRUE)
  s <- deps()
  expect_identical(s[["blockr-viz-core"]]$script[[1L]], "js/core/namespace.js")
  expect_identical(s[["blockr-viz-table"]]$stylesheet,
                   file.path("css", css_list("table")))
  own <- c("blockr-viz-core", "chart-js", "chart-css", "blockr-viz-table")
  for (d in c(b[own], s[own])) {
    expect_true(all(file.exists(file.path(d$src$file, c(d$script, d$stylesheet)))))
  }
})

test_that("snapdom and the ECharts themes are not loaded with the page", {
  scripts <- unlist(lapply(
    htmltools::findDependencies(new_chart_block()$expr_ui("blk")),
    `[[`, "script"
  ))
  expect_false(any(grepl("snapdom", scripts)))
  expect_false(any(grepl("themes|dark\\.js|vintage\\.js", scripts)))
  # snapdom is served with the core, where core/lazy.js fetches it.
  core <- viz_core_dep()
  expect_true(file.exists(file.path(core$src$file, "js/vendor/snapdom.js")))
})

test_that("a theme with a file is named by its served path", {
  dep <- drilldown_echarts_themes_dep()
  skip_if(is.null(dep))
  expect_null(dep$script)
  expect_identical(echart_theme_src("dark"), "echarts4r-themes-6.0.0/dark.js")
  expect_true(file.exists(file.path(dep$src$file, "purple-passion.js")))
  expect_null(echart_theme_src("default"))
  expect_null(echart_theme_src("blockr"))
  expect_null(echart_theme_src("../../etc/passwd"))
  expect_null(echart_theme_src(NULL))
})
