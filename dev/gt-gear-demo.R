# gt block gear-settings styling check: dataset -> summary_table -> gt_table.
# The gt block's panel shows its config form (Title/Subtitle/NA/toggles).
#   cd /workspace && Rscript blockr.bi/dev/gt-gear-demo.R > /tmp/gt.log 2>&1 &
.libPaths("/workspace/blockr.dev/.devcontainer/.library")
suppressMessages({ library(blockr.core); pkgload::load_all("/workspace/blockr.bi", quiet = TRUE) })
register_bi_blocks()

board <- new_board(
  blocks = c(
    data = new_dataset_block("CO2"),
    summ = new_summary_table_block(state = list(
      vars = list("uptake", "Type"), by = list("Treatment"), add_overall = TRUE)),
    gt = new_gt_table_block(title = "Demographics")
  ),
  links = c(new_link("data", "summ", "data"), new_link("summ", "gt", "data"))
)
options(shiny.port = 3838, shiny.host = "0.0.0.0", shiny.launch.browser = FALSE)
shiny::runApp(serve(board))
