# The rewritten JS core (inst/js/core/: the Blockr.viz namespace, Scope, the
# shared output binding, the gear engine, titles, downloads and capture), the
# busy cue every view's output answers (busy-cue.js, R/kit-output.R) and the
# generated specs, as one dependency. Block views load after it.

#' HTML dependency for the blockr.viz JS core and the block specs
#' @noRd
viz_core_dep <- memoise0(function() {
  script <- c("busy-cue.js", file.path("core", js_script_list("core")),
              "spec/chart.js", "spec/table.js")
  htmltools::htmlDependency(
    name = "blockr-viz-core",
    version = js_version(script),
    src = system.file("js", package = "blockr.viz"),
    script = script
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

#' ECharts, from echarts4r's dependency
#' @noRd
viz_echarts_dep <- memoise0(function() {
  w <- echarts4r::e_charts(height = 0)
  htmltools::findDependencies(w)
})
