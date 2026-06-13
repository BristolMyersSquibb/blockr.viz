#' Register BI Blocks
#'
#' Registers the BI blocks with blockr.
#'
#' @export
#' @importFrom blockr.core register_blocks
register_bi_blocks <- function() {
  # NOTE: deprecated blocks are UNREGISTERED, not tagged — their constructors
  # stay exported (so serialized boards still deserialize via get0()), but they
  # are removed from this list so the picker and the MCP/AI universe stop
  # offering them. Currently deprecated & unregistered:
  #   - new_kpi_block      -> superseded by new_tile_block (showcase = "number")
  #   - new_pivot_table_block -> = summarize + pivot_wider; no production caller
  #   - new_waterfall_block -> folded into new_chart_block as chart_type =
  #       "waterfall" (bar + baseline = "cumulative")
  # See dev/table-and-chart-architecture.md.
  blockr.core::register_blocks(
    c(
      "new_summary_table_block",
      "new_gt_table_block",
      "new_html_table_block",
      "new_tile_block",
      "new_chart_block",
      "new_table_block"
    ),
    name = c(
      "Summary Table",
      "gt Table",
      "HTML Table",
      "Tile",
      "Chart",
      "Table"
    ),
    description = c(
      "Wide, display-shaped multi-variable summary (list of variables by Y pattern). Successor to tidy_summary_block.",
      "Render wide-format tables (from summary_table) as styled gt tables. Also supports legacy long-format input from tidy_summary_block.",
      "Dashboard-native HTML renderer for wide-format tables: collapsible sections, sticky headers, multi-level column spanners.",
      "Dashboard tiles: big numbers, sparklines, progress rings. ggplot-style aesthetic mapping. Successor to kpi_block.",
      "Configurable chart with click-to-filter drill-down",
      "Interactive table (sticky header, sort, search) with optional cell coloring and click-to-filter drill-down"
    ),
    category = c(
      "transform",
      "table",
      "table",
      "transform",
      "plot",
      "table"
    ),
    icon = c(
      "calculator",
      "table",
      "table",
      "speedometer2",
      "funnel",
      "table"
    ),
    arguments = list(
      summary_table_arguments(),
      gt_table_arguments(),
      NULL,                         # html_table_block (not in MCP universe)
      NULL,                         # tile_block (not in MCP universe)
      chart_arguments(),
      table_arguments()
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
