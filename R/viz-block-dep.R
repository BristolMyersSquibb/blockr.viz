#' @noRd
viz_block_css_dep <- function() {
  htmltools::htmlDependency(
    name = "viz-block-css",
    version = utils::packageVersion("blockr.viz"),
    src = system.file("css", package = "blockr.viz"),
    stylesheet = "viz-block.css"
  )
}

#' Settings band + checkbox (design-system pilot; see blockr.ui/dev/
#' gear-panel-proposals.html and boolean-controls-proposals.html). The gear
#' expands an in-flow full-width band instead of a floating popover; on/off
#' options render as checkboxes. settings-band.js must load before
#' drilldown-config.js / the block scripts (they use Blockr.checkbox).
#' @noRd
settings_band_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-viz-settings-band",
    version = paste0(utils::packageVersion("blockr.viz"), ".1"),
    src = system.file(package = "blockr.viz"),
    script = "js/settings-band.js",
    stylesheet = "css/settings-band.css"
  )
}

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
drilldown_shared_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-viz-drilldown-shared",
    version = paste0(utils::packageVersion("blockr.viz"), ".8"),
    src = system.file("js", package = "blockr.viz"),
    script = c("drilldown-agg.js", "drilldown-config.js")
  )
}

#' Ensure echarts is available via echarts4r's dependency
#' @noRd
viz_echarts_dep <- function() {
  w <- echarts4r::e_charts(height = 0)
  htmltools::findDependencies(w)
}
