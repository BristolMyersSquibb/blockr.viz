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
  #   - new_gt_table_block  -> superseded by new_table_block, which now renders
  #       structured Table-1 summaries interactively. Constructor kept (static
  #       gt / board compat); the static-gt adapter's home (bi vs blockr.gt) is
  #       still open.
  #   - new_html_table_block -> folded into new_table_block (table_block reuses
  #       the html builders for flat + structured input). Constructor kept for
  #       board compat.
  # See dev/table-and-chart-architecture.md.
  blockr.core::register_blocks(
    c(
      "new_summary_table_block",
      "new_tile_block",
      "new_chart_block",
      "new_table_block"
    ),
    name = c(
      "Summary Table",
      "Tile",
      "Chart",
      "Table"
    ),
    description = c(
      "Wide, display-shaped multi-variable summary (list of variables by Y pattern). Successor to tidy_summary_block.",
      "Dashboard tiles: big numbers, sparklines, progress rings. ggplot-style aesthetic mapping. Successor to kpi_block.",
      "Configurable chart with click-to-filter drill-down",
      "Interactive table (sticky header, sort, search) with optional cell coloring and click-to-filter drill-down"
    ),
    category = c(
      "transform",
      "transform",
      "plot",
      "table"
    ),
    icon = c(
      "calculator",
      "speedometer2",
      "funnel",
      "table"
    ),
    arguments = list(
      summary_table_arguments(),
      NULL,                         # tile_block (not in MCP universe)
      chart_arguments(),
      table_arguments()
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
