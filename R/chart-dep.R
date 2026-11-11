# The chart block's dependencies: blockr.ui's controls, the folder of
# ECharts themes, the JS core and specs, then the scripts in
# `inst/js/chart/scripts.txt` (bundled in js/dist/chart.js).
drilldown_chart_dep <- memoise_js(function(dev) {
  htmltools::tagList(
    blockr.ui::controls_dep(),
    drilldown_echarts_themes_dep(),
    viz_core_dep(),
    chart_js_dep(),
    chart_css_dep(),
    # The data view under the chart is drawn by the table view.
    table_view_dep()
  )
})

chart_js_dep <- memoise_js(function(dev) {
  script <- if (dev) {
    c("drilldown-theme-register.js", file.path("chart", js_script_list("chart")))
  } else {
    "dist/chart.js"
  }
  htmltools::htmlDependency(
    name = "chart-js",
    version = js_version(script),
    src = system.file("js", package = "blockr.viz"),
    script = script
  )
})

# chart.css, the chart's own styles, and the tooltip card. The version comes
# from the file's contents.
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
#' right (`vz-tt-head`, `vz-tt-sw`, `vz-tt-row`, `vz-tt-label`,
#' `vz-tt-value`, `vz-tt-meta`, `vz-tt-sep`). Another package's tooltips can
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

# The echarts themes bundled in echarts4r, as a served folder with no
# script: the chart view loads the one the board names
# (`derived$theme_src`, echart_theme_src()) before it draws, so that
# `echarts.init(el, name)` resolves the name.
drilldown_echarts_themes_dep <- memoise0(function() {
  theme_dir <- echart_theme_dir()
  if (!nzchar(theme_dir)) return(NULL)
  htmltools::htmlDependency(
    name = "echarts4r-themes",
    version = "6.0.0",
    src = theme_dir,
    script = NULL,
    all_files = TRUE
  )
})

echart_theme_dir <- function() {
  system.file("htmlwidgets/lib/echarts-6.0.0/themes", package = "echarts4r")
}

# The themes the board option offers that come as files
# (new_drilldown_theme_option()); "default" is ECharts' own and the JS
# registers "blockr" itself.
ECHART_THEME_FILES <- c(
  "dark", "vintage", "westeros", "essos", "wonderland", "walden", "chalk",
  "infographic", "macarons", "roma", "shine", "purple-passion"
)

# Where the page finds a theme's script, relative to the page, as Shiny
# serves the theme dependency; NULL for a theme with no file.
echart_theme_src <- function(theme) {
  if (!is.character(theme) || length(theme) != 1L ||
        !theme %in% ECHART_THEME_FILES) {
    return(NULL)
  }
  dep <- drilldown_echarts_themes_dep()
  file <- paste0(theme, ".js")
  if (is.null(dep) || !file.exists(file.path(dep$src$file, file))) {
    return(NULL)
  }
  paste0(dep$name, "-", dep$version, "/", file)
}
