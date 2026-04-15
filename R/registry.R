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
      "new_pivot_table_block",
      "new_summary_table_block",
      "new_gt_table_block",
      "new_html_table_block",
      "new_kpi_block",
      "new_waterfall_block"
    ),
    name = c(
      "Visual Filter",
      "Pivot Table",
      "Summary Table",
      "gt Table",
      "HTML Table",
      "KPI",
      "Waterfall"
    ),
    description = c(
      "Interactive visual filter with clickable bar charts",
      "Flexible pivot table with row and column dimensions (X by Y x Z pattern)",
      "Wide, display-shaped multi-variable summary (list of variables by Y pattern). Successor to tidy_summary_block.",
      "Render wide-format tables (from pivot_table or summary_table) as styled gt tables. Also supports legacy long-format input from tidy_summary_block.",
      "Dashboard-native HTML renderer for wide-format tables: collapsible sections, sticky headers, multi-level column spanners.",
      "Display a single key performance indicator",
      "Waterfall/bridge chart for sequential value progression"
    ),
    category = c(
      "transform",
      "transform",
      "transform",
      "table",
      "table",
      "transform",
      "transform"
    ),
    icon = c(
      "bar-chart",
      "table",
      "calculator",
      "table",
      "table",
      "speedometer2",
      "bar-chart-steps"
    ),
    arguments = list(
      NULL,                         # visual_filter_block (not in MCP universe)
      pivot_table_arguments(),
      summary_table_arguments(),
      gt_table_arguments(),
      NULL,                         # html_table_block (not in MCP universe)
      NULL,                         # kpi_block (not in MCP universe)
      NULL                          # waterfall_block (not in MCP universe)
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
