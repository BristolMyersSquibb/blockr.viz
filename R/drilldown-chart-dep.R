#' HTML dependencies for the drilldown chart block
#' @noRd
drilldown_chart_dep <- function() {
  htmltools::tagList(
    # Reuse blockr.dplyr's shared CSS (gear, popover, rows) and select component
    htmltools::htmlDependency(
      name = "blockr-blocks-css",
      version = paste0(utils::packageVersion("blockr.dplyr"), ".2"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = c("blockr-blocks.css", "blockr-select.css")
    ),
    htmltools::htmlDependency(
      name = "blockr-select-js",
      version = paste0(utils::packageVersion("blockr.dplyr"), ".2"),
      src = system.file("js", package = "blockr.dplyr"),
      script = c("blockr-core.js", "blockr-select.js")
    ),
    drilldown_echarts_themes_dep(),
    htmltools::htmlDependency(
      name = "drilldown-chart-js",
      version = paste0(utils::packageVersion("blockr.bi"), ".31"),
      src = system.file("js", package = "blockr.bi"),
      # drilldown-config.js (the shared gear-popover engine) must load BEFORE
      # drilldown-chart.js, which references Blockr.DrilldownConfig.
      script = c("drilldown-theme-register.js", "drilldown-config.js",
                 "drilldown-chart.js")
    ),
    htmltools::htmlDependency(
      name = "drilldown-chart-css",
      version = paste0(utils::packageVersion("blockr.bi"), ".24"),
      src = system.file("css", package = "blockr.bi"),
      stylesheet = "drilldown-chart.css"
    )
  )
}

# Bundled echarts theme files (dark, vintage, westeros, ...) live inside the
# echarts4r package. The block calls `echarts.init(el, name)` directly, so
# each theme's JS must be loaded in the page for the name to resolve.
drilldown_echarts_themes_dep <- function() {
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
}
