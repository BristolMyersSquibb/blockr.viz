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
      "new_pivot_table_block"
    ),
    name = c(
      "Visual Filter",
      "Aggregate",
      "Pivot Table"
    ),
    description = c(
      "Interactive visual filter with clickable bar charts",
      "Group by drill down columns and summarize value columns",
      "Flexible pivot table with row and column dimensions"
    ),
    category = c(
      "transform",
      "transform",
      "transform"
    ),
    icon = c(
      "bar-chart",
      "grid-3x3",
      "table"
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
