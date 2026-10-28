# The chart block's dependencies: blockr.ui's controls, the ECharts themes,
# the JS core and specs, then the scripts in `inst/js/chart/scripts.txt`.
drilldown_chart_dep <- memoise0(function() {
  htmltools::tagList(
    blockr.ui::controls_dep(),
    drilldown_echarts_themes_dep(),
    viz_core_dep(),
    chart_js_dep(),
    chart_css_dep()
  )
})

chart_js_dep <- memoise0(function() {
  script <- c("busy-cue.js", "drilldown-theme-register.js",
              file.path("chart", js_script_list("chart")))
  htmltools::htmlDependency(
    name = "chart-js",
    version = js_version(script),
    src = system.file("js", package = "blockr.viz"),
    script = script
  )
})

# chart.css, shared with the table block, versioned by its contents.
chart_css_dep <- memoise0(function() {
  htmltools::tagList(
    tooltip_card_dep(),
    htmltools::htmlDependency(
      name = "chart-css",
      version = js_version("chart.css", "css"),
      src = system.file("css", package = "blockr.viz"),
      stylesheet = "chart.css"
    )
  )
})

#' HTML dependency for the data tooltip card
#'
#' The stylesheet behind every blockr.viz data tooltip: a headline with a
#' colour swatch, then rows with the name on the left and the value on the
#' right (`dd-tt-head`, `dd-tt-sw`, `dd-tt-row`, `dd-tt-label`,
#' `dd-tt-value`, `dd-tt-meta`, `dd-tt-sep`). Another package's tooltips can
#' attach it and use the same classes.
#'
#' @return An [htmltools::htmlDependency()].
#' @export
tooltip_card_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "blockr-viz-tooltip",
    version = js_version("tooltip.css", "css"),
    src = system.file("css", package = "blockr.viz"),
    stylesheet = "tooltip.css"
  )
})

# The echarts themes bundled in echarts4r, loaded so that
# `echarts.init(el, name)` resolves the name.
drilldown_echarts_themes_dep <- memoise0(function() {
  theme_dir <- system.file(
    "htmlwidgets/lib/echarts-6.0.0/themes", package = "echarts4r"
  )
  scripts <- c(
    "dark.js", "vintage.js", "westeros.js", "essos.js", "wonderland.js",
    "walden.js", "chalk.js", "infographic.js", "macarons.js", "roma.js",
    "shine.js", "purple-passion.js"
  )
  scripts <- scripts[file.exists(file.path(theme_dir, scripts))]
  if (!length(scripts)) return(NULL)

  htmltools::htmlDependency(
    name = "echarts4r-themes",
    version = "6.0.0",
    src = theme_dir,
    script = scripts
  )
})
