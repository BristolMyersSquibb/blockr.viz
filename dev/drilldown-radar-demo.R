# Radar chart demo — `group` levels are the spokes, one shape per `color`
# level, each vertex = agg_fn(metric) for that (group, color) cell. With drill
# on, clicking a shape filters the downstream table on its value.
#
# Run from the workspace root (works inside or outside the dev container):
#   Rscript blockr.bi/dev/drilldown-radar-demo.R
# open the local URL serve() prints (or uncomment the options line to pin 3838).

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.bi")

stopifnot(requireNamespace("safetyData", quietly = TRUE))
adsl <- safetyData::adam_adsl

board <- new_dock_board(
  blocks = c(
    data = new_static_block(adsl),
    radar = new_chart_block(chart_type = "radar", group = "RACE",
                            color = "TRT01P", metric = "AGE", agg_fn = "mean",
                            drill = "auto"),
    table = new_table_block()
  ),
  links = links(
    from = c("data", "radar"),
    to   = c("radar", "table")
  ),
  extensions = list(blockr.dag::new_dag_extension())
)

# options(shiny.port = 3838L, shiny.host = "0.0.0.0")  # uncomment to pin
serve(board)
