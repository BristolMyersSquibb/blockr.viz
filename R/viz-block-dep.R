# Process-level cache for the block htmlDependency builders. Each builder takes
# no data-dependent args and returns the same object for the life of the R
# process (packageVersion()/system.file() cannot change while the app runs), yet
# was re-running disk I/O (read.dcf + system.file) on every render and every
# block construction -- ~95% of dt_chrome() and of dplyr block construction (see
# dev/block-build-cost-findings.md). htmlDependency objects are immutable value
# lists that htmltools already shares across sessions, so a process-level cache
# shared across Shiny sessions is correct. `dep_cached()` is called at RUNTIME
# (not when the builder is defined), so collation order does not matter; the
# exists() check caches NULL returns too (e.g. missing echarts themes).
.dep_cache <- new.env(parent = emptyenv())

#' @noRd
dep_cached <- function(key, build) {
  if (!exists(key, envir = .dep_cache, inherits = FALSE)) {
    assign(key, build(), envir = .dep_cache)
  }
  get(key, envir = .dep_cache, inherits = FALSE)
}

#' @noRd
viz_block_css_dep <- function() dep_cached("viz_block_css_dep", function() {
  htmltools::htmlDependency(
    name = "viz-block-css",
    version = utils::packageVersion("blockr.viz"),
    src = system.file("css", package = "blockr.viz"),
    stylesheet = "viz-block.css"
  )
})

#' Settings band + checkbox (design-system pilot; see blockr.ui/dev/
#' gear-panel-proposals.html and boolean-controls-proposals.html). The gear
#' expands an in-flow full-width band instead of a floating popover; on/off
#' options render as checkboxes. settings-band.js must load before
#' drilldown-config.js / the block scripts (they use Blockr.checkbox).
#' @noRd
settings_band_dep <- function() dep_cached("settings_band_dep", function() {
  htmltools::htmlDependency(
    name = "blockr-viz-settings-band",
    version = paste0(utils::packageVersion("blockr.viz"), ".2"),
    src = system.file(package = "blockr.viz"),
    script = "js/settings-band.js",
    stylesheet = "css/settings-band.css"
  )
})

#' The shared drilldown JS — the aggregation vocabulary (drilldown-agg.js)
#' and the gear-popover config engine (drilldown-config.js) consumed by the
#' chart, table AND tile. ONE dependency with ONE version counter, so an
#' engine edit is a single bump and a stale copy can never shadow a fresh
#' one on a mixed dashboard (the former three-copies-in-three-deps setup
#' required bumping chart-js, blockr-viz-table and tile-block in lockstep).
#' Must load before the block scripts (they read Blockr.DrilldownAgg /
#' Blockr.DrilldownConfig) and after settings-band.js (the engine uses
#' Blockr.checkbox).
#'
#' Bump the version suffix on EVERY drilldown-agg.js / drilldown-config.js
#' edit (version-pinned asset cache).
#' @noRd
drilldown_shared_dep <- function() dep_cached("drilldown_shared_dep", function() {
  htmltools::htmlDependency(
    name = "blockr-viz-drilldown-shared",
    version = paste0(utils::packageVersion("blockr.viz"), ".10"),
    src = system.file("js", package = "blockr.viz"),
    script = c("drilldown-agg.js", "drilldown-config.js")
  )
})

#' Ensure echarts is available via echarts4r's dependency
#' @noRd
viz_echarts_dep <- function() dep_cached("viz_echarts_dep", function() {
  w <- echarts4r::e_charts(height = 0)
  htmltools::findDependencies(w)
})
