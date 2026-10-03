#' The chart engine the chart block's UI loads
#'
#' `options(blockr.viz.chart_engine = "v2")` loads the rewrite in
#' `inst/js/chart-v2/` instead of `chart.js`; anything else is v1. Read when
#' the UI is built, so one app loads one engine: both register the same
#' input binding and message handlers.
#' @noRd
chart_engine <- function() {
  engine <- getOption("blockr.viz.chart_engine", "v1")
  if (identical(engine, "v2")) "v2" else "v1"
}

#' HTML dependencies for the chart block
#' @noRd
drilldown_chart_dep <- memoise0(function() {
  htmltools::tagList(
    # The design system's tokens and shared controls (Select, checkbox,
    # gear tray, segmented control, tooltip), from blockr.ui.
    blockr.ui::controls_dep(),
    drilldown_echarts_themes_dep(),
    # The shared aggregation vocabulary + gear engine (one dep, one version —
    # see drilldown_shared_dep()). Listed BEFORE chart-js: chart.js reads
    # Blockr.DrilldownAgg and Blockr.DrilldownConfig at load.
    drilldown_shared_dep(),
    htmltools::htmlDependency(
      name = "chart-js",
      version = paste0(utils::packageVersion("blockr.viz"), ".131"),
      src = system.file("js", package = "blockr.viz"),
      script = c("drilldown-theme-register.js", "chart.js")
    ),
    chart_css_dep()
  )
})

# The v2 engine: the same shared parts as v1, then the scripts listed in
# inst/js/chart-v2/scripts.txt instead of chart.js. A dependency of its own
# name, so a page never serves both under one name.
drilldown_chart_v2_dep <- memoise0(function() {
  htmltools::tagList(
    blockr.ui::controls_dep(),
    drilldown_echarts_themes_dep(),
    drilldown_shared_dep(),
    htmltools::htmlDependency(
      name = "chart-v2-js",
      version = paste0(utils::packageVersion("blockr.viz"), ".2"),
      src = system.file("js", package = "blockr.viz"),
      script = c("drilldown-theme-register.js", chart_v2_scripts())
    ),
    chart_css_dep()
  )
})

# The v2 scripts in load order, relative to inst/js. The node test harness
# reads the same list.
chart_v2_scripts <- function() {
  lines <- trimws(readLines(
    system.file("js", "chart-v2", "scripts.txt", package = "blockr.viz"),
    warn = FALSE
  ))
  file.path("chart-v2", lines[nzchar(lines) & !startsWith(lines, "#")])
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
    version = paste0(utils::packageVersion("blockr.viz"), ".44"),
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
