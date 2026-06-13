# Table-1 structured-render check: dataset -> summary_table -> table.
# Verifies table_block now renders section nesting / indents / spanners
# (Phase 1 fold). Serve on 3838:
#   cd /workspace && Rscript blockr.bi/dev/table1-playwright-demo.R > /tmp/t1.log 2>&1 &
.libPaths("/workspace/blockr.dev/.devcontainer/.library")
suppressMessages({
  library(blockr.core)
  pkgload::load_all("/workspace/blockr.bi", quiet = TRUE)
})
register_bi_blocks()

board <- new_board(
  blocks = c(
    data = new_dataset_block("CO2"),  # Type/Treatment factors + uptake numeric
    summ = new_summary_table_block(state = list(
      vars = list("uptake", "Type"),
      by   = list("Treatment"),
      add_overall = TRUE
    )),
    tbl  = new_table_block()
  ),
  links = c(
    new_link("data", "summ", "data"),
    new_link("summ", "tbl",  "data")
  )
)

options(shiny.port = 3838, shiny.host = "0.0.0.0", shiny.launch.browser = FALSE)
shiny::runApp(serve(board))
