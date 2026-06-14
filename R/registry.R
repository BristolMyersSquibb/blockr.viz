#' Register BI Blocks
#'
#' Registers the BI blocks with blockr.
#'
#' @export
#' @importFrom blockr.core register_blocks
register_bi_blocks <- function() {
  # Removed outright (2026-06-14) — superseded, with no compat shim kept since
  # blockr.bi is being retired in favour of blockr.viz (a conscious upgrade):
  #   - new_kpi_block          -> new_tile_block
  #   - new_pivot_table_block  -> summarize + tidyr::pivot_wider (a composed
  #                               reshape, not a bespoke block)
  #   - new_waterfall_block    -> new_chart_block(chart_type = "waterfall")
  #   - new_html_table_block / new_drilldown_table_block -> new_table_block
  #     (renders flat + structured input via the html_table() builders)
  #   - new_drilldown_chart_block -> new_chart_block
  # See dev/table-and-chart-architecture.md.
  blockr.core::register_blocks(
    c(
      "new_summary_table_block",
      "new_gt_table_block",
      "new_tile_block",
      "new_chart_block",
      "new_table_block"
    ),
    name = c(
      "Summary Table",
      "gt Table",
      "Tile",
      "Chart",
      "Table"
    ),
    description = c(
      "Wide, display-shaped multi-variable summary (list of variables by Y pattern). Successor to tidy_summary_block.",
      "Render wide-format tables (from summary_table) as styled gt tables — static / print / CSR output.",
      "Scorecard of bold KPI numbers — cards or an aligned matrix, with deltas / fills / status pills and click-to-filter drill. A pure renderer (shape upstream).",
      "Configurable chart with click-to-filter drill-down",
      "Interactive table (sticky header, sort, search) with optional cell coloring and click-to-filter drill-down"
    ),
    category = c(
      "transform",
      "table",
      "transform",
      "plot",
      "table"
    ),
    icon = c(
      "calculator",
      "table",
      "speedometer2",
      "funnel",
      "table"
    ),
    arguments = list(
      summary_table_arguments(),
      gt_table_arguments(),
      tile_arguments(),
      chart_arguments(),
      table_arguments()
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
