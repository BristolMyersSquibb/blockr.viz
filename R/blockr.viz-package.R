#' blockr.viz: Visualization and render blocks for blockr
#'
#' The render layer for blockr dashboards. It provides the interactive
#' renderers ([new_chart_block()] and [new_table_block()], which also draws
#' KPI cards),
#' and the [new_summary_table_block()] shaper. See `vignette("blockr-viz")`
#' for the shaper / renderer architecture.
#'
#' @importFrom rlang %||%
#' @keywords internal
"_PACKAGE"

# NSE column names used inside dplyr pipelines built in the chart and table
# server closures -- declared so R CMD check does not flag them as
# undefined global variables.
utils::globalVariables(c(
  ".", "val", "xv", "yv", "xlo", "xhi", "ylo", "yhi",
  # bbquote() placeholders in chart_filter_call()
  "data", ".."
))
