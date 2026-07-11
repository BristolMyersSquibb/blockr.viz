#' HTML dependencies for the chart block
#' @noRd
drilldown_chart_dep <- memoise0(function() {
  htmltools::tagList(
    # Reuse blockr.dplyr's shared CSS (gear, popover, rows) and select component
    htmltools::htmlDependency(
      name = "blockr-blocks-css",
      version = paste0(utils::packageVersion("blockr.dplyr"), ".3"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = c("blockr-blocks.css", "blockr-select.css")
    ),
    htmltools::htmlDependency(
      name = "blockr-select-js",
      version = paste0(utils::packageVersion("blockr.dplyr"), ".3"),
      src = system.file("js", package = "blockr.dplyr"),
      script = c("blockr-core.js", "blockr-select.js")
    ),
    settings_band_dep(),
    drilldown_echarts_themes_dep(),
    # The shared aggregation vocabulary + gear engine (one dep, one version —
    # see drilldown_shared_dep()). Listed BEFORE chart-js: chart.js reads
    # Blockr.DrilldownAgg and Blockr.DrilldownConfig at load.
    drilldown_shared_dep(),
    htmltools::htmlDependency(
      name = "chart-js",
      version = paste0(utils::packageVersion("blockr.viz"), ".64"),
      src = system.file("js", package = "blockr.viz"),
      script = c("drilldown-theme-register.js", "chart.js")
    ),
    htmltools::htmlDependency(
      name = "chart-css",
      version = paste0(utils::packageVersion("blockr.viz"), ".34"),
      src = system.file("css", package = "blockr.viz"),
      stylesheet = "chart.css"
    )
  )
})

# Bundled echarts theme files (dark, vintage, westeros, ...) live inside the
# echarts4r package. The block calls `echarts.init(el, name)` directly, so
# each theme's JS must be loaded in the page for the name to resolve.
drilldown_echarts_themes_dep <- memoise0(function() {
  theme_dir <- system.file(
    "htmlwidgets/lib/echarts-6.0.0/themes", package = "echarts4r"
  )
  if (!nzchar(theme_dir)) return(NULL)

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
