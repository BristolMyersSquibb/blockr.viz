#' @noRd
viz_block_css_dep <- function() {
  htmltools::htmlDependency(
    name = "viz-block-css",
    version = utils::packageVersion("blockr.bi"),
    src = system.file("css", package = "blockr.bi"),
    stylesheet = "viz-block.css"
  )
}

#' Ensure echarts is available via echarts4r's dependency
#' @noRd
viz_echarts_dep <- function() {
  w <- echarts4r::e_charts(height = 0)
  htmltools::findDependencies(w)
}
