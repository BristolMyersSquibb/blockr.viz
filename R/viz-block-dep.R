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

#' Ensure echarts is available via echarts4r's dependency
#' @noRd
viz_echarts_dep <- function() {
  w <- echarts4r::e_charts(height = 0)
  htmltools::findDependencies(w)
}
