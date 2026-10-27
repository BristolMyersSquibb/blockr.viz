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
