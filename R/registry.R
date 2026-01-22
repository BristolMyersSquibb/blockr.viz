#' Register BI Blocks
#'
#' Registers the BI blocks with blockr.
#'
#' @export
#' @importFrom blockr.core register_blocks
register_bi_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_visual_filter_block",
      "new_aggregate_block",
      "new_pivot_table_block",
      "new_kpi_block",
      "new_waterfall_block"
    ),
    name = c(
      "Visual Filter",
      "Aggregate",
      "Pivot Table",
      "KPI",
      "Waterfall"
    ),
    description = c(
      "Interactive visual filter with clickable bar charts",
      "Group by drill down columns and summarize value columns",
      "Flexible pivot table with row and column dimensions",
      "Display a single key performance indicator",
      "Waterfall/bridge chart for sequential value progression"
    ),
    category = c(
      "transform",
      "transform",
      "transform",
      "transform",
      "transform"
    ),
    icon = c(
      "bar-chart",
      "grid-3x3",
      "table",
      "speedometer2",
      "bar-chart-steps"
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
