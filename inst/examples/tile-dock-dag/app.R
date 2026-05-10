# tile-dock-dag/app.R — Tile block in a full dock+DAG board.
#
# Pre-seeds four pipelines (number tiles, scorecard, sparklines,
# progress rings). The dock UI gives you the block sidebar, gear
# offcanvas, DAG panel, etc.
#
#   Rscript /workspace/blockr.bi/inst/examples/tile-dock-dag/app.R
#
# then open http://localhost:3840 .

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.react")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.bi")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.extra")

register_bi_blocks()  # makes new_tile_block appear in the block-adder

dd <- tile_demo_data()

board <- new_dock_board(
  blocks = c(
    # --- Pipeline A: number tiles (block auto-aggregates) -----------
    tx_data = new_static_block(dd$transactions),
    tx_tiles = new_tile_block(
      showcase = "number",
      state = list(
        aesthetics = list(value = c("revenue", "orders", "conversion")),
        stats = list(value = "mean")
      )
    ),

    # --- Pipeline B: scorecard (region x segment) -------------------
    tx_scorecard = new_tile_block(
      showcase = "number",
      state = list(
        aesthetics = list(
          value = c("revenue", "orders"),
          rows = "region",
          cols = "segment"
        ),
        stats = list(value = "sum")
      )
    ),

    # --- Pipeline C: sparklines from time series --------------------
    ts_data = new_static_block(dd$time_series),
    ts_tiles = new_tile_block(
      showcase = "spark",
      state = list(
        aesthetics = list(
          value = "price", spark_value = "price",
          spark_x = "date", cols = "ticker"
        ),
        stats = list(value = "last")
      )
    ),

    # --- Pipeline D: progress rings from KPI frame ------------------
    kpi_data = new_static_block(dd$kpis_with_goals),
    kpi_tiles = new_tile_block(
      showcase = "progress",
      state = list(
        aesthetics = list(
          value = "value", max = "target",
          label = "metric", status = "status"
        )
      )
    ),

    # --- Pipeline E: list-in-card (multiple metrics in one tile) ----
    # Same KPI frame, number showcase + label aesthetic, no row/col
    # facet → one card with one row per metric (label · value / target
    # · status pill). Color the rows by status (tint).
    kpi_list_tiles = new_tile_block(
      showcase = "number",
      state = list(
        aesthetics = list(
          value = "value", label = "metric",
          target = "target", status = "status"
        ),
        color = list(by = "status", intensity = "tint")
      )
    ),

    # --- Pipeline F: solid scorecard tiles colored by region --------
    tx_colored = new_tile_block(
      showcase = "number",
      state = list(
        aesthetics = list(
          value = c("revenue", "orders"),
          rows = "region"
        ),
        stats = list(value = "sum"),
        color = list(by = "region", intensity = "solid")
      )
    )
  ),
  links = links(
    from = c("tx_data", "tx_data",  "ts_data",  "kpi_data",  "kpi_data",        "tx_data"),
    to   = c("tx_tiles", "tx_scorecard", "ts_tiles", "kpi_tiles", "kpi_list_tiles", "tx_colored")
  ),
  extensions = new_dock_extensions(list(
    new_dag_extension()
  ))
)

serve(board, "tile-dock-dag")
