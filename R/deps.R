# The JS core (inst/js/core/: the Blockr.viz namespace, Scope, the shared
# output binding, the frame, the gear engine, titles, downloads and capture),
# the generated specs and the frame's and the gear's styles, as one
# dependency. Block views load after it. The page gets the bundles
# tools/bundle.js writes (js/dist/core.js, css/dist/core.css);
# `options(blockr.viz.dev_js = TRUE)` lists the sources instead.
viz_core_dep <- memoise_js(function(dev) {
  script <- if (dev) {
    file.path("js", c(file.path("core", js_script_list("core")),
                      "spec/chart.js", "spec/table.js"))
  } else {
    "js/dist/core.js"
  }
  css <- if (dev) file.path("css", css_list("core")) else "css/dist/core.css"
  htmltools::htmlDependency(
    name = "blockr-viz-core",
    version = js_version(c(script, css), "."),
    # The whole package directory is served, so the core can fetch
    # js/vendor/snapdom.js when a picture is first taken (core/lazy.js).
    src = system.file(package = "blockr.viz"),
    script = script,
    stylesheet = css
  )
})

# The package version with a number taken from the files' contents, so a
# browser never serves a changed script or stylesheet from its cache.
js_version <- function(script, dir = "js") {
  sums <- tools::md5sum(system.file(dir, script, package = "blockr.viz"))
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(unname(sums), tmp)
  paste0(utils::packageVersion("blockr.viz"), ".",
         strtoi(substr(unname(tools::md5sum(tmp)), 1L, 6L), 16L))
}

# The scripts a folder of inst/js lists in its scripts.txt, in load order.
# The node test harness reads the same lists.
js_script_list <- function(dir) {
  lines <- trimws(readLines(
    system.file("js", dir, "scripts.txt", package = "blockr.viz"),
    warn = FALSE
  ))
  lines[nzchar(lines) & !startsWith(lines, "#")]
}

# The stylesheets inst/css/styles.txt lists for one bundle, in load order.
# tools/bundle.js reads the same file.
css_list <- function(bundle) {
  lines <- trimws(readLines(
    system.file("css", "styles.txt", package = "blockr.viz"),
    warn = FALSE
  ))
  rows <- strsplit(lines[nzchar(lines) & !startsWith(lines, "#")], "[[:space:]]+")
  row <- Filter(function(r) identical(r[[1L]], bundle), rows)
  stopifnot(length(row) == 1L)
  row[[1L]][-1L]
}

# ECharts, from echarts4r's dependency.
viz_echarts_dep <- memoise0(function() {
  w <- echarts4r::e_charts(height = 0)
  htmltools::findDependencies(w)
})
