# Tile-block feature catalog — proper blockr app.
#
# 12 scenes, each a (static_block, tile_block) pair wired through the
# dock+DAG board. Same coverage as dev/tile_showcase.R but built with
# real blockr blocks so you can poke at the gear panel, swap aesthetics,
# toggle showcase pills, etc.
#
#   Rscript /workspace/blockr.bi/dev/tile_showcase_app.R
#
# Dock shows scenes as tabs along the top; click a tab to focus that
# scene's block. Use the "+" tab to spin up new boards from the catalog.

pkgload::load_all("blockr.core", quiet = TRUE)
pkgload::load_all("blockr.dock", quiet = TRUE)
pkgload::load_all("blockr.dag",  quiet = TRUE)
pkgload::load_all("blockr.bi",   quiet = TRUE)

register_bi_blocks()

dd <- tile_demo_data()

# --- helpers ---------------------------------------------------------

tile <- function(showcase = "number", aesthetics = list(),
                 stats = list(), color = list()) {
  new_tile_block(
    showcase = showcase,
    state = list(aesthetics = aesthetics, stats = stats, color = color)
  )
}

# --- scenes ----------------------------------------------------------
# Each scene gets its own data block (so the DAG stays readable) plus
# one tile block. The data sources reuse three tibbles from
# tile_demo_data(): transactions, time_series, kpis_with_goals.

board <- new_dock_board(
  blocks = c(
    # 01 — Zero-config number tiles
    s01_data  = new_static_block(dd$transactions),
    s01_tiles = tile("number",
      aesthetics = list(value = c("revenue", "orders", "conversion"))),

    # 02 — Multi-stat
    s02_data  = new_static_block(dd$transactions),
    s02_tiles = tile("number",
      aesthetics = list(value = "revenue"),
      stats = list(value = c("mean", "sum", "min", "max"))),

    # 03 — Scorecard region × segment
    s03_data  = new_static_block(dd$transactions),
    s03_tiles = tile("number",
      aesthetics = list(value = c("revenue", "orders"),
                        rows = "region", cols = "segment"),
      stats = list(value = "sum")),

    # 04 — Sparklines
    s04_data  = new_static_block(dd$time_series),
    s04_tiles = tile("spark",
      aesthetics = list(value = "price",
                        spark_value = "price",
                        spark_x = "date",
                        cols = "ticker"),
      stats = list(value = "last")),

    # 05 — Progress rings
    s05_data  = new_static_block(dd$kpis_with_goals),
    s05_tiles = tile("progress",
      aesthetics = list(value = "value", max = "target",
                        label = "metric", status = "status")),

    # 06 — List-in-card (no color)
    s06_data  = new_static_block(dd$kpis_with_goals),
    s06_tiles = tile("number",
      aesthetics = list(value = "value", label = "metric",
                        target = "target", status = "status")),

    # 07 — List-in-card · status (tint)
    s07_data  = new_static_block(dd$kpis_with_goals),
    s07_tiles = tile("number",
      aesthetics = list(value = "value", label = "metric",
                        target = "target", status = "status"),
      color = list(by = "status", intensity = "tint")),

    # 08 — List-in-card · status (solid)
    s08_data  = new_static_block(dd$kpis_with_goals),
    s08_tiles = tile("number",
      aesthetics = list(value = "value", label = "metric",
                        target = "target", status = "status"),
      color = list(by = "status", intensity = "solid")),

    # 09 — List-in-card · status (border)
    s09_data  = new_static_block(dd$kpis_with_goals),
    s09_tiles = tile("number",
      aesthetics = list(value = "value", label = "metric",
                        target = "target", status = "status"),
      color = list(by = "status", intensity = "border")),

    # 10 — Number tiles · color by measure (tint)
    s10_data  = new_static_block(dd$transactions),
    s10_tiles = tile("number",
      aesthetics = list(value = c("revenue", "orders", "conversion")),
      color = list(by = "measure", intensity = "tint")),

    # 11 — Scorecard · color by region (solid)
    s11_data  = new_static_block(dd$transactions),
    s11_tiles = tile("number",
      aesthetics = list(value = c("revenue", "orders"), rows = "region"),
      stats = list(value = "sum"),
      color = list(by = "region", intensity = "solid")),

    # 12 — Sparklines · color by ticker (border)
    s12_data  = new_static_block(dd$time_series),
    s12_tiles = tile("spark",
      aesthetics = list(value = "price",
                        spark_value = "price",
                        spark_x = "date",
                        cols = "ticker"),
      stats = list(value = "last"),
      color = list(by = "ticker", intensity = "border"))
  ),
  links = links(
    from = paste0("s", sprintf("%02d", 1:12), "_data"),
    to   = paste0("s", sprintf("%02d", 1:12), "_tiles")
  ),
  extensions = new_dock_extensions(list(
    new_dag_extension()
  ))
)

serve(board, "tile-showcase")
