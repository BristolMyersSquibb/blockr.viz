# Verify: waterfall is now a BAR option (the `baseline` toggle), not a separate
# chart_type. Starts as a plain bar; open the chart's gear -> the type picker
# should show Bar | Pie | Treemap (no Waterfall), and with Bar selected a
# "Bars: Standard | Waterfall" toggle. Picking Waterfall -> cumulative bridge.
#   cd /workspace && Rscript blockr.bi/dev/chart-baseline-demo.R > /tmp/cb.log 2>&1 &
.libPaths("/workspace/blockr.dev/.devcontainer/.library")
suppressMessages({ library(blockr.core); pkgload::load_all("blockr.bi", quiet = TRUE) })
register_bi_blocks()

board <- new_board(
  blocks = c(
    data = new_dataset_block("BOD"),
    bar = new_chart_block(chart_type = "bar", group = "Time",
                          metric = "demand", agg_fn = "sum")
  ),
  links = c(new_link("data", "bar", "data"))
)
options(shiny.port = 3838, shiny.host = "0.0.0.0", shiny.launch.browser = FALSE)
shiny::runApp(serve(board))
