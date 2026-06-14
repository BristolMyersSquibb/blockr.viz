# Chart waterfall demo — waterfall is a BAR option (the `baseline` toggle), not
# a separate chart_type. Open the chart's gear: the type picker shows
# Bar | Pie | Treemap, and with Bar selected a "Bars: Standard | Waterfall"
# toggle. Picking Waterfall draws the cumulative bridge.
#
# Run from the workspace root (works inside or outside the dev container):
#   Rscript blockr.bi/dev/chart-baseline-demo.R
# open the local URL serve() prints (or uncomment the options line to pin 3838).

options(blockr.html_table_preview = TRUE)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.bi")

board <- new_dock_board(
  blocks = c(
    data = new_dataset_block("BOD"),
    bar = new_chart_block(chart_type = "bar", group = "Time",
                          metric = "demand", agg_fn = "sum")
  ),
  links = links(
    from = "data",
    to   = "bar"
  ),
  extensions = list(blockr.dag::new_dag_extension())
)

# options(shiny.port = 3838L, shiny.host = "0.0.0.0")  # uncomment to pin
serve(board)
