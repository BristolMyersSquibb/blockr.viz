#' HTML dependencies for the chart block
#'
#' The design system's tokens and shared controls (blockr.ui), the ECharts
#' themes, the shared aggregation vocabulary and gear engine (they load
#' before the chart scripts, which read Blockr.DrilldownAgg and
#' Blockr.DrilldownConfig at load), then the chart scripts listed in
#' `inst/js/chart/scripts.txt`.
#' @noRd
drilldown_chart_dep <- memoise0(function() {
  htmltools::tagList(
    blockr.ui::controls_dep(),
    drilldown_echarts_themes_dep(),
    drilldown_shared_dep(),
    htmltools::htmlDependency(
      name = "chart-js",
      version = paste0(utils::packageVersion("blockr.viz"), ".136"),
      src = system.file("js", package = "blockr.viz"),
      script = c("drilldown-theme-register.js", chart_scripts())
    ),
    chart_css_dep()
  )
})

# The chart scripts in load order, relative to inst/js. The node test harness
# reads the same list.
chart_scripts <- function() {
  lines <- trimws(readLines(
    system.file("js", "chart", "scripts.txt", package = "blockr.viz"),
    warn = FALSE
  ))
  file.path("chart", lines[nzchar(lines) & !startsWith(lines, "#")])
}

# chart.css, shared by the chart block and the table block (which reuses the
# chart's gear/popover styling). ONE definition so the cache-busting suffix
# cannot drift between the two: htmltools dedupes same-name dependencies by
# highest version, so a table-only page with a stale copy of this dep would
# serve chart.css under an old version string and hit the browser cache.
# Suffix bumped when inst/css/chart.css changes.
chart_css_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "chart-css",
    version = paste0(utils::packageVersion("blockr.viz"), ".47"),
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
