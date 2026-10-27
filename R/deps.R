# The rewritten JS core (inst/js/core/: the Blockr.viz namespace, Scope, the
# shared output binding, the gear engine, titles, downloads and capture) and
# the generated specs, as one dependency. Block views load after it.

#' HTML dependency for the blockr.viz JS core and the block specs
#' @noRd
viz_core_dep <- memoise0(function() {
  script <- c(file.path("core", js_script_list("core")), "spec/chart.js", "spec/table.js")
  htmltools::htmlDependency(
    name = "blockr-viz-core",
    version = js_version(script),
    src = system.file("js", package = "blockr.viz"),
    script = script
  )
})

# The package version with a number taken from the scripts' contents, so a
# browser never serves a changed script from its cache.
js_version <- function(script) {
  sums <- tools::md5sum(system.file("js", script, package = "blockr.viz"))
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
