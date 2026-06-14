#' blockr.viz: Visualization and render blocks for blockr
#'
#' The render layer for blockr dashboards. It provides the interactive
#' renderers ([new_chart_block()], [new_table_block()], [new_tile_block()]),
#' the [new_summary_table_block()] shaper, and the static
#' [new_gt_table_block()] renderer. See `vignette("blockr-viz")` for the
#' shaper / renderer architecture.
#'
#' @importFrom rlang %||%
#' @importFrom blockr.dplyr blockr_core_js_dep blockr_blocks_css_dep
#'   blockr_select_dep
#' @keywords internal
"_PACKAGE"

# NSE column names used inside dplyr pipelines built in the chart / table /
# tile server closures — declared so R CMD check does not flag them as
# undefined global variables.
utils::globalVariables(c(
  ".", "val", "xv", "yv", "xlo", "xhi", "ylo", "yhi"
))
