# Tile (KPI renderer) demo — cards (delta / fill / pill), a grouped matrix, and
# a drill tile whose click filters a downstream table.
#
# Run from the workspace root (works inside or outside the dev container):
#   Rscript blockr.viz/dev/tile-playwright-demo.R
# open the local URL serve() prints (or uncomment the options line to pin 3838).

options(blockr.html_table_preview = TRUE)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.viz")

d <- tile_demo_data()

board <- new_dock_board(
  blocks = c(
    sc1 = new_static_block(d$scorecard),
    t_delta = new_tile_block(value = "value", measure = "metric",
                             secondary = "delta", style = "delta",
                             good_when = "up", format = "number",
                             layout = "cards"),
    sc2 = new_static_block(d$scorecard),
    t_fill = new_tile_block(value = "value", measure = "metric",
                            secondary = "progress", style = "fill",
                            layout = "cards"),
    sc3 = new_static_block(d$scorecard),
    t_pill = new_tile_block(value = "value", measure = "metric",
                            secondary = "status", style = "pill",
                            layout = "cards"),
    rg1 = new_static_block(d$regions),
    t_mtx = new_tile_block(value = c("revenue", "conversion", "orders"),
                           by = "region", layout = "table"),
    rg2 = new_static_block(d$regions),
    t_drill = new_tile_block(value = "orders", by = "region", unit = "orders",
                             layout = "cards", drill = TRUE),
    downstream = new_table_block()
  ),
  links = links(
    from = c("sc1", "sc2", "sc3", "rg1", "rg2", "t_drill"),
    to   = c("t_delta", "t_fill", "t_pill", "t_mtx", "t_drill", "downstream")
  ),
  extensions = list(blockr.dag::new_dag_extension())
)

# options(shiny.port = 3838L, shiny.host = "0.0.0.0")  # uncomment to pin
serve(board)
