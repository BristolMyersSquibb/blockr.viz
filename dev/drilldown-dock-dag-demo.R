# Chart -> table drill demo — a bar chart aggregates subjects per SITEID
# (coloured by arm); clicking a bar emits a categorical filter that flows
# downstream to the table, which then shows only the drilled rows.
#
# Run from the workspace root (works inside or outside the dev container):
#   Rscript blockr.viz/dev/drilldown-dock-dag-demo.R
# open the local URL serve() prints (or uncomment the options line to pin 3838).

options(blockr.html_table_preview = TRUE)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.viz")

stopifnot(requireNamespace("safetyData", quietly = TRUE))
adsl <- safetyData::adam_adsl

board <- new_dock_board(
  blocks = c(
    data = new_static_block(adsl),
    chart = new_chart_block(group = "SITEID", color = "TRT01P",
                            chart_type = "bar", agg_fn = "count",
                            drill = "SITEID"),
    tbl = new_table_block(rowname = "USUBJID",
                          values = c("AGE", "BMIBL", "WEIGHTBL"),
                          drill = "USUBJID")
  ),
  links = links(
    from = c("data", "chart"),
    to   = c("chart", "tbl")
  ),
  extensions = list(blockr.dag::new_dag_extension())
)

# options(shiny.port = 3838L, shiny.host = "0.0.0.0")  # uncomment to pin
serve(board)
