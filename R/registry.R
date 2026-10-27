#' Register blockr.viz blocks
#'
#' Registers the blockr.viz render and shaper blocks with blockr so they
#' appear in the block-adder and the assistant block universe.
#'
#' @return Invisibly, the result of [blockr.core::register_blocks()].
#' @examplesIf interactive()
#' register_viz_blocks()
#' @export
#' @importFrom blockr.core register_blocks new_arg_specs new_arg_spec
#'   arg_string arg_number arg_integer arg_boolean arg_enum arg_array arg_object
register_viz_blocks <- function() {
  # Removed outright (2026-06-14) -- superseded, with no compat shim kept since
  # blockr.bi was renamed to blockr.viz (a conscious upgrade):
  #   - new_kpi_block          -> new_tile_block, which went in its turn
  #                               (2026-10): new_table_block(look = "cards")
  #   - new_pivot_table_block  -> summarize + tidyr::pivot_wider (a composed
  #                               reshape, not a bespoke block)
  #   - new_waterfall_block    -> new_chart_block(chart_type = "waterfall")
  #   - new_html_table_block / new_drilldown_table_block -> new_table_block
  #     (renders flat + structured input via the html_table() builders)
  #   - new_drilldown_chart_block -> new_chart_block
  #   - new_gt_table_block     -> new_table_block (2026-10, table rewrite):
  #                               the constructor stays for saved boards,
  #                               off the block list
  # See dev/table-and-chart-architecture.md.
  blockr.core::register_blocks(
    c(
      "new_chart_block",
      "new_table_block"
    ),
    name = c(
      "Chart",
      "Table"
    ),
    description = c(
      "Configurable chart with click-to-filter drill-down",
      "Interactive table: the input rows (cell colouring, finished tables from R code) or one row per group with a column per summary (count/mean bars, box / point-range distributions, swimlanes, sparklines, text stats), or a Table 1 with one row per statistic of each variable and a column per group, with search, sort and click-to-filter drill-down. Drawn as a table, a compact matrix, or KPI cards (one card per summary: its number in large type and its glyph)"
    ),
    # Categories come from blockr.core::suggested_categories() (a fixed
    # vocabulary the pickers group by; anything else warns as discouraged).
    # The two interactive renderers (chart / table) are display blocks
    # (dev/table-and-chart-architecture.md): the chart is a visualization
    # ("plot"), the table tabular display ("table"). summary_table stays
    # "transform": it is a shaper whose output feeds the renderers.
    category = c(
      "plot",
      "table"
    ),
    icon = c(
      "funnel",
      "table"
    ),
    arguments = list(
      viz_registry_args(viz_block_spec("chart")),
      table_arguments()
    ),
    guidance = c(
      chart_guidance(),
      table_guidance()
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
