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

  viz_input_handlers(lapply(names(VIZ_BLOCK_SPECS), viz_block_spec))
  register_viz_blocks()
  register_assistant_effect()
  viz_register_exporters()

  invisible(NULL)
} # nocov end
