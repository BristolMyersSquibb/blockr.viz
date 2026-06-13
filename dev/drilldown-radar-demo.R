# Radar demo for the drilldown chart (aggregated family).
#
# Radar mapping: `group` levels = the spokes, one shape per `color` level,
# each vertex = agg_fn(metric) for that (group, color) cell. Clicking a
# shape (with drill on) filters downstream on its color value.
#
# safetyData ADaM ADLBC: mean lab value by visit, one shape per arm.
#
# From /workspace:
#   Rscript blockr.bi/dev/drilldown-radar-demo.R
# then open http://127.0.0.1:3838/

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.bi")

stopifnot(requireNamespace("safetyData", quietly = TRUE))
adsl <- safetyData::adam_adsl

board <- new_dock_board(
  blocks = c(
    data = new_static_block(data = adsl),
    radar = new_chart_block(
      chart_type = "radar",
      group      = "RACE",
      color      = "TRT01P",
      metric     = "AGE",
      agg_fn     = "mean",
      drill      = "auto"
    ),
    table = new_table_block()
  ),
  links = links(
    from = c("data", "radar"),
    to   = c("radar", "table")
  ),
  extensions = list(blockr.dag::new_dag_extension())
)

shiny::runApp(serve(board), port = 3838, host = "0.0.0.0")
