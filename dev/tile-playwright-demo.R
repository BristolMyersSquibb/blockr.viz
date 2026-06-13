# Tile (KPI renderer) visual + drill check. Serve on 3838:
#   cd /workspace && Rscript blockr.bi/dev/tile-playwright-demo.R > /tmp/tile.log 2>&1 &
#
# Covers: cards with delta / fill / pill, the grouped matrix (table layout),
# and a drill=TRUE tile whose click filters a downstream table.
.libPaths("/workspace/blockr.dev/.devcontainer/.library")
suppressMessages({
  library(blockr.core)
  pkgload::load_all("/workspace/blockr.bi", quiet = TRUE)
})
register_bi_blocks()

d <- tile_demo_data()

board <- new_board(
  blocks = c(
    # --- card styles on the scorecard frame -------------------------------
    sc1  = new_static_block(d$scorecard),
    t_delta = new_tile_block(value = "value", measure = "metric",
                             secondary = "delta", style = "delta",
                             good_when = "up", format = "compact",
                             layout = "cards"),
    sc2  = new_static_block(d$scorecard),
    t_fill = new_tile_block(value = "value", measure = "metric",
                            secondary = "progress", style = "fill",
                            layout = "cards"),
    sc3  = new_static_block(d$scorecard),
    t_pill = new_tile_block(value = "value", measure = "metric",
                            secondary = "status", style = "pill",
                            layout = "cards"),

    # --- grouped matrix (table layout) ------------------------------------
    rg1  = new_static_block(d$regions),
    t_mtx = new_tile_block(value = c("revenue", "conversion", "orders"),
                           by = "region", layout = "table"),

    # --- drill: clicking a region card filters the downstream table -------
    rg2   = new_static_block(d$regions),
    t_drill = new_tile_block(value = "revenue", by = "region",
                             layout = "cards", drill = TRUE),
    downstream = new_table_block()
  ),
  links = c(
    new_link("sc1", "t_delta", "data"),
    new_link("sc2", "t_fill",  "data"),
    new_link("sc3", "t_pill",  "data"),
    new_link("rg1", "t_mtx",   "data"),
    new_link("rg2", "t_drill", "data"),
    new_link("t_drill", "downstream", "data")
  )
)

options(shiny.port = 3838, shiny.host = "0.0.0.0", shiny.launch.browser = FALSE)
shiny::runApp(serve(board))
