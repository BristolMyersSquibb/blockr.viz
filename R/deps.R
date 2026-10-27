#' The shared drilldown JS
#'
#' The aggregation vocabulary (drilldown-agg.js) and the gear config engine
#' (drilldown-config.js), used by the chart and the table. Loads before the
#' block scripts, which read Blockr.DrilldownAgg and Blockr.DrilldownConfig,
#' and after blockr.ui::controls_dep(), whose Blockr.checkbox and
#' Blockr.Select the engine uses.
#'
#' Bump the version suffix on every edit to these scripts (the asset cache is
#' keyed on the version).
#' @noRd
drilldown_shared_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "blockr-viz-drilldown-shared",
    version = paste0(utils::packageVersion("blockr.viz"), ".30"),
    src = system.file("js", package = "blockr.viz"),
    script = c("drilldown-agg.js", "drilldown-config.js", "capture-pages.js",
               "busy-cue.js")
  )
})

#' ECharts, from echarts4r's dependency
#' @noRd
viz_echarts_dep <- memoise0(function() {
  w <- echarts4r::e_charts(height = 0)
  htmltools::findDependencies(w)
})
