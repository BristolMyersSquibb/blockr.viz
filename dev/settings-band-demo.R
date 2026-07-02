# Settings-band + checkbox pilot demo (design-system: blockr.ui/dev/
# gear-panel-proposals.html variant B + boolean-controls-proposals.html).
# One plain board with the four blockr.viz blocks: chart, table, tile,
# summary table. Click any gear: it expands an in-flow, full-width settings
# band (standard controls; on/off options are checkboxes).
#
# Rscript dev/settings-band-demo.R  (serves on PORT, default 3838)

pkgload::load_all("/workspace/blockr.viz", quiet = TRUE)

port <- as.integer(Sys.getenv("PORT", "3838"))

board <- blockr.core::new_board(
  blocks = c(
    data    = blockr.core::new_dataset_block("penguins", package = "palmerpenguins"),
    chart   = new_chart_block(),
    table   = new_table_block(),
    tile    = new_tile_block(),
    summary = new_summary_table_block()
  ),
  links = c(
    blockr.core::new_link("data", "chart", "data"),
    blockr.core::new_link("data", "table", "data"),
    blockr.core::new_link("data", "tile", "data"),
    blockr.core::new_link("data", "summary", "data")
  )
)

app <- blockr.core::serve(board)
shiny::runApp(app, port = port, host = "0.0.0.0")
