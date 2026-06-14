.onLoad <- function(libname, pkgname) {
  # nocov start

  shiny::addResourcePath(
    "blockr-viz-js",
    system.file("js", package = pkgname)
  )
  shiny::addResourcePath(
    "blockr-viz-css",
    system.file("css", package = pkgname)
  )

  register_viz_blocks()
  echart_theme_blockr_viz()
  register_drilldown_ai_effect()

  invisible(NULL)
} # nocov end
