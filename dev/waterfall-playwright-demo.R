# Waterfall visual check — uses new_dataset_block (the data path that renders
# cleanly) on the built-in BOD dataset, mapping the waterfall onto its real
# columns (Time -> step axis, demand -> value). Proves the bar+cumulative
# baseline rendering; not a semantic P&L.
#   cd /workspace/blockr.bi && Rscript dev/waterfall-playwright-demo.R > /tmp/wf.log 2>&1 &

suppressMessages({
  library(blockr.core)
  pkgload::load_all("blockr.bi", quiet = TRUE)
})
register_bi_blocks()

board <- new_board(
  blocks = c(
    data = new_dataset_block("BOD"),
    wf = new_chart_block(
      chart_type = "waterfall",
      group      = "Time",
      metric     = "demand",
      agg_fn     = "sum"
    )
  ),
  links = c(new_link("data", "wf", "data"))
)

options(shiny.launch.browser = FALSE)
shiny::runApp(serve(board))
