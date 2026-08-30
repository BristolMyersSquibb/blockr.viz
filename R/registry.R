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
  # Registered separately at the end (own arg specs + guidance in its file):
  # the picker, a shaper like summary_table.
  on.exit(register_picker_block())
  # Removed outright (2026-06-14) -- superseded, with no compat shim kept since
  # blockr.bi was renamed to blockr.viz (a conscious upgrade):
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
      "new_table_block",
      "new_summarize_table_block",
      "new_heatmap_block"
    ),
    name = c(
      # "Variable summary", not "Summary Table": its rows are VARIABLES
      # (the display-shaped Table-1 producer), and the name must not
      # collide with the summarize table, whose rows are groups.
      "Variable summary (Table 1)",
      "gt Table",
      "Tile",
      "Chart",
      "Table",
      "Summarize table",
      "Heatmap"
    ),
    description = c(
      "Wide, display-shaped multi-variable summary (list of variables by Y pattern). Successor to tidy_summary_block.",
      "Render wide-format tables (from summary_table) as styled gt tables \u2014 static / print / CSR output.",
      "Scorecard of bold KPI numbers \u2014 cards or an aligned matrix, with deltas / fills / status pills and click-to-filter drill. A pure renderer (shape upstream).",
      "Configurable chart with click-to-filter drill-down",
      "Interactive table (sticky header, sort, search) with optional cell coloring and click-to-filter drill-down",
      "Grouped summary table with graphical cells \u2014 an ordered list of summary columns (count/mean bars, box / point-range distributions, interval swimlanes, sparklines, text stats, group facts) over one grouping, with facet, search, sort and click-to-filter drill-down",
      "Row x column matrix from long event rows \u2014 cell shows the event count, paint encodes the worst level of a severity column; top-N column cap, group rail, click-to-filter drill (the AE heatmap form)"
    ),
    # Categories come from blockr.core::suggested_categories() (a fixed
    # vocabulary the pickers group by; anything else warns as discouraged).
    # The three interactive renderers (tile / chart / table) are display
    # blocks (dev/table-and-chart-architecture.md): the tile and chart are
    # visualizations ("plot"), the table is tabular display ("table") -- the
    # tile is NOT a transform (it renders, its data output is a passthrough
    # filter). summary_table stays "transform": it is a shaper whose output
    # feeds the renderers.
    category = c(
      "transform",
      "table",
      "plot",
      "plot",
      "table",
      "table",
      "plot"
    ),
    icon = c(
      "calculator",
      "table",
      "speedometer2",
      "funnel",
      "table",
      "bar-chart-steps",
      "grid-3x3"
    ),
    arguments = list(
      summary_table_arguments(),
      gt_table_arguments(),
      tile_arguments(),
      chart_arguments(),
      table_arguments(),
      rank_arguments(),
      heatmap_arguments()
    ),
    guidance = c(
      summary_table_guidance(),
      gt_table_guidance(),
      tile_guidance(),
      chart_guidance(),
      table_guidance(),
      rank_guidance(),
      heatmap_guidance()
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
