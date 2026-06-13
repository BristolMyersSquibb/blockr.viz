# Drilldown dock+dag demo
#
# Simple workflow to verify the rewritten drilldown blocks end-to-end:
#
#   [data: ADSL] --> [drilldown_chart] --> [drilldown_table]
#
# The chart aggregates (count of subjects per SITEID, coloured by TRT01P) and
# emits a categorical drill filter when a bar is clicked; that filter flows
# downstream to the table, which then shows only the drilled rows. Both blocks
# share the rewritten DrilldownConfig gear-popover engine.
#
# Served on a blockr.dock board with the blockr.dag extension.
#
# From /workspace:
#   Rscript blockr.bi/dev/drilldown-dock-dag-demo.R
# then open http://127.0.0.1:3838/

pkgload::load_all("blockr.core", quiet = TRUE)
pkgload::load_all("blockr.ui",   quiet = TRUE)
pkgload::load_all("blockr.dplyr", quiet = TRUE)
pkgload::load_all("blockr.dock", quiet = TRUE)
pkgload::load_all("blockr.dag",  quiet = TRUE)
pkgload::load_all("blockr.bi",   quiet = TRUE)

stopifnot(requireNamespace("safetyData", quietly = TRUE))
adsl <- safetyData::adam_adsl

board <- new_dock_board(
  blocks = c(
    data = new_static_block(adsl),

    chart = new_chart_block(
      group      = "SITEID",
      color      = "TRT01P",
      chart_type = "bar",
      agg_fn     = "count",
      drill      = "SITEID"
    ),

    tbl = new_table_block(
      label_col  = "USUBJID",
      value_cols = c("AGE", "BMIBL", "WEIGHTBL"),
      drill      = "USUBJID"
    )
  ),
  links = links(
    from = c("data", "chart"),
    to   = c("chart", "tbl")
  ),
  extensions = new_dock_extensions(list(
    new_dag_extension()
  ))
)

# options(shiny.port = 3838, shiny.host = "0.0.0.0", shiny.launch.browser = FALSE)
shiny::runApp(serve(board, "drilldown-dock-dag"))
