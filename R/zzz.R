.onLoad <- function(libname, pkgname) {
  # nocov start

  shiny::addResourcePath(
    "blockr-bi-js",
    system.file("js", package = pkgname)
  )
  shiny::addResourcePath(
    "blockr-bi-css",
    system.file("css", package = pkgname)
  )

  register_bi_blocks()
  echart_theme_blockr_bi()

  invisible(NULL)
} # nocov end
