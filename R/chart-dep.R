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
      version = paste0(utils::packageVersion("blockr.viz"), ".89"),
      src = system.file("js", package = "blockr.viz"),
      script = c("drilldown-theme-register.js", "chart.js")
    ),
    chart_css_dep()
  )
})

# chart.css, shared by the chart block and the table block (which reuses the
# chart's gear/popover styling). ONE definition so the cache-busting suffix
# cannot drift between the two: htmltools dedupes same-name dependencies by
# highest version, so a table-only page with a stale copy of this dep would
# serve chart.css under an old version string and hit the browser cache.
# Suffix bumped when inst/css/chart.css changes.
chart_css_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "chart-css",
    version = paste0(utils::packageVersion("blockr.viz"), ".35"),
    src = system.file("css", package = "blockr.viz"),
    stylesheet = "chart.css"
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
