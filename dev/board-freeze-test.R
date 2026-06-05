# Real-board test for the family-switch filter path:
#   data -> drilldown chart (click/brush filters) -> downstream table.
# The table consumes the chart's FILTERED output via a link — the real path a
# board uses (unlike serve.block, which only previews one block's result).
# From /workspace:  Rscript blockr.bi/dev/board-freeze-test.R  -> http://127.0.0.1:3838/
pkgload::load_all("/workspace/blockr.core", quiet = TRUE)
pkgload::load_all("/workspace/blockr.bi", quiet = TRUE)

df <- mtcars
df$cyl  <- factor(df$cyl)
df$gear <- factor(df$gear)

board <- new_board(
  blocks = c(
    data  = new_static_block(df),
    chart = new_drilldown_chart_block(chart_type = "bar", group = "cyl", drill = "cyl"),
    tbl   = new_drilldown_table_block()
  ),
  links = links(
    new_link(from = "data",  to = "chart"),
    new_link(from = "chart", to = "tbl")
  )
)

shiny::runApp(serve(board), port = 3838, host = "0.0.0.0", launch.browser = FALSE)
