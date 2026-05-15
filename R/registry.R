#' Register BI Blocks
#'
#' Registers the BI blocks with blockr.
#'
#' @export
#' @importFrom blockr.core register_blocks
register_bi_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_bi_filter_block",
      "new_visual_filter_block",
      "new_pivot_table_block",
      "new_summary_table_block",
      "new_gt_table_block",
      "new_html_table_block",
      "new_tile_block",
      "new_kpi_block",
      "new_waterfall_block",
      "new_drilldown_chart_block",
      "new_drilldown_table_block"
    ),
    name = c(
      "Filter",
      "Visual Filter",
      "Pivot Table",
      "Summary Table",
      "gt Table",
      "HTML Table",
      "Tile",
      "KPI",
      "Waterfall",
      "Drill-Down Chart",
      "Drill-Down Table"
    ),
    description = c(
      "Minimal value filter for a data frame. Columns hidden behind the gear; per-column single vs multi select.",
      "Interactive visual filter with clickable bar charts",
      "Flexible pivot table with row and column dimensions (X by Y x Z pattern)",
      "Wide, display-shaped multi-variable summary (list of variables by Y pattern). Successor to tidy_summary_block.",
      "Render wide-format tables (from pivot_table or summary_table) as styled gt tables. Also supports legacy long-format input from tidy_summary_block.",
      "Dashboard-native HTML renderer for wide-format tables: collapsible sections, sticky headers, multi-level column spanners.",
      "Dashboard tiles: big numbers, sparklines, progress rings. ggplot-style aesthetic mapping. Successor to kpi_block.",
      "Display one or more key performance indicators as prominent numbers.",
      "Waterfall/bridge chart for sequential value progression",
      "Configurable chart with click-to-filter drill-down",
      "Interactive table (sticky header, sort, search) with optional cell coloring and click-to-filter drill-down"
    ),
    category = c(
      "transform",
      "transform",
      "transform",
      "transform",
      "table",
      "table",
      "transform",
      "transform",
      "transform",
      "plot",
      "table"
    ),
    icon = c(
      "filter",
      "bar-chart",
      "table",
      "calculator",
      "table",
      "table",
      "speedometer2",
      "speedometer2",
      "bar-chart-steps",
      "funnel",
      "table"
    ),
    arguments = list(
      NULL,                         # filter_block (not in MCP universe)
      NULL,                         # visual_filter_block (not in MCP universe)
      pivot_table_arguments(),
      summary_table_arguments(),
      gt_table_arguments(),
      NULL,                         # html_table_block (not in MCP universe)
      NULL,                         # tile_block (not in MCP universe)
      NULL,                         # kpi_block (not in MCP universe)
      NULL,                         # waterfall_block (not in MCP universe)
      drilldown_chart_arguments(),
      NULL                          # drilldown_table_block (not in MCP universe)
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
