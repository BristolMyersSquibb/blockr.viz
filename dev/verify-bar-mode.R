# Verification harness for bar_mode (stacked / grouped / percent).
# Three color-split bar charts, one per mode, on a plain board so outputs
# render headless (dock suspends inactive panels). Serve on 3838.
options(shiny.port = 3838L, shiny.host = "0.0.0.0")
options(blockr.tabular_display = blockr.ui::html_table_display)

pkgload::load_all("blockr.core", quiet = TRUE)
pkgload::load_all("blockr.viz", quiet = TRUE)

stopifnot(requireNamespace("safetyData", quietly = TRUE))
adsl <- safetyData::adam_adsl

board <- new_board(
  blocks = c(
    data = new_static_block(adsl, block_name = "ADSL"),
    stacked = new_chart_block(
      chart_type = "bar", group = "ARM", color = "SEX",
      metric = ".count", agg_fn = "count", bar_mode = "stacked",
      orientation = "vertical", block_name = "Stacked"),
    grouped = new_chart_block(
      chart_type = "bar", group = "ARM", color = "SEX",
      metric = ".count", agg_fn = "count", bar_mode = "grouped",
      orientation = "vertical", block_name = "Grouped"),
    percent = new_chart_block(
      chart_type = "bar", group = "ARM", color = "SEX",
      metric = ".count", agg_fn = "count", bar_mode = "percent",
      orientation = "vertical", block_name = "Percent (100%)")
  ),
  links = links(
    from = c("data", "data", "data"),
    to   = c("stacked", "grouped", "percent")
  )
)

serve(board)
