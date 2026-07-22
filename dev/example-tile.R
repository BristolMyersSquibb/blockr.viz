# Tile capabilities tour — one renderer, every scorecard style.
#
# `new_tile_block()` is the dashboard scorecard renderer: it turns a tidy frame
# into bold KPI numbers, as cards or an aligned matrix, with a secondary
# indicator (delta / progress bar / status pill), per-measure number formatting,
# and optional click-to-filter drill. A pure renderer — it shapes the display,
# not the data (aggregate upstream). Each dock VIEW (the layout switcher,
# top-right) isolates one capability; the left "Workflow" canvas shows the wiring.
#
# Views
#   1. Delta cards   big numbers with an up/down delta vs target (good_when
#                    colours the arrow: up = green, down = red)
#   2. Progress      the same numbers with a fill bar (secondary = progress)
#   3. Status pills   a coloured status pill per card (ok / warn)
#   4. Matrix        several measures x groups as one aligned grid (layout=table)
#   5. Formats       compact (1.2M / 38.4K), percent (62%), and a free-text unit
#   6. Drill         a per-region tile whose click filters a downstream table
#   7. Aggregation   RAW rows reduced in-block: group + metrics -> per-group cards
#   8. Grand totals   metrics with no group -> a single row of headline cards
#
# Run from the workspace root (inside or outside the dev container):
#   Rscript blockr.viz/dev/example-tile.R
# open the local URL serve() prints (or uncomment the options line to pin 3838).

options(blockr.tabular_display = blockr.ui::html_table_display)
options(blockr.dock_is_locked = FALSE)
# options(shiny.port = 3838L, shiny.host = "0.0.0.0")  # uncomment to pin

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.viz")

d <- tile_demo_data()   # $scorecard, $regions (display-shaped), $transactions (raw)

board <- new_dock_board(
  blocks = c(
    sc = new_static_block(d$scorecard, block_name = "Scorecard data"),
    rg = new_static_block(d$regions, block_name = "Regions data"),
    tx = new_static_block(d$transactions, block_name = "Transactions (raw rows)"),

    # 1. DELTA CARDS — value with a +/- delta vs target; good_when="up" paints
    #    a rise green and a fall red.
    t_delta = new_tile_block(
      value = "value", measure = "metric", secondary = "delta",
      style = "delta", good_when = "up", format = "compact", layout = "cards",
      block_name = "KPIs with delta vs target"),

    # 2. PROGRESS — the same cards with a fill bar driven by `progress`.
    t_fill = new_tile_block(
      value = "value", measure = "metric", secondary = "progress",
      style = "fill", format = "compact", layout = "cards",
      block_name = "KPIs with progress fill"),

    # 3. STATUS PILLS — a coloured pill per card from a status column.
    t_pill = new_tile_block(
      value = "value", measure = "metric", secondary = "status",
      style = "pill", format = "compact", layout = "cards",
      block_name = "KPIs with status pill"),

    # 4. MATRIX — several measures across groups in one aligned grid.
    t_matrix = new_tile_block(
      value = c("revenue", "conversion", "orders"), group = "region",
      layout = "table", block_name = "Measures x region (matrix)"),

    # 5. FORMATS — compact numbers, a percent, and a free-text unit, side by side.
    t_compact = new_tile_block(
      value = "value", measure = "metric", format = "compact", layout = "cards",
      block_name = "Compact (1.2M / 38.4K)"),
    t_percent = new_tile_block(
      value = "progress", measure = "metric", format = "percent",
      layout = "cards", block_name = "Percent (62%)"),
    t_unit = new_tile_block(
      value = "orders", group = "region", format = "number", unit = "orders",
      layout = "cards", block_name = "Number + unit"),

    # 6. DRILL — clicking a region card emits a filter the table applies.
    t_drill = new_tile_block(
      value = "orders", group = "region", unit = "orders", layout = "cards",
      drill = TRUE, block_name = "Orders by region (drill)"),
    downstream = new_table_block(block_name = "Drilled region (downstream)"),

    # 7. AGGREGATION — RAW transaction rows reduced IN-BLOCK: group by region,
    #    then sum(revenue) + count. Each metric is a card; one cluster per
    #    region. A card click still drills to that region's raw rows.
    t_agg = new_tile_block(
      value = "revenue", group = "region",
      metrics = list(
        list(agg_fn = "sum",   cols = list("revenue")),
        list(agg_fn = "count", cols = list())
      ),
      format = "compact", drill = TRUE,
      block_name = "Per-region totals (in-block aggregation)"),
    agg_down = new_table_block(block_name = "Drilled region rows (downstream)"),

    # 8. GRAND TOTALS — metrics with NO group: the whole frame reduces to one
    #    row of headline cards (count, mean revenue, sum orders).
    t_totals = new_tile_block(
      value = "revenue",
      metrics = list(
        list(agg_fn = "count",  cols = list()),
        list(agg_fn = "mean",   cols = list("revenue")),
        list(agg_fn = "sum",    cols = list("orders"))
      ),
      format = "compact", block_name = "Grand totals (no group)")
  ),
  links = links(
    from = c("sc", "sc", "sc", "rg", "sc", "sc", "rg", "rg", "t_drill",
             "tx", "t_agg", "tx"),
    to   = c("t_delta", "t_fill", "t_pill", "t_matrix", "t_compact",
             "t_percent", "t_unit", "t_drill", "downstream",
             "t_agg", "agg_down", "t_totals")
  ),
  layouts = list(
    delta    = dock_layout("t_delta", name = "1. Delta cards"),
    progress = dock_layout("t_fill", name = "2. Progress"),
    pills    = dock_layout("t_pill", name = "3. Status pills"),
    matrix   = dock_layout("t_matrix", name = "4. Matrix"),
    formats  = dock_layout("t_compact", "t_percent", "t_unit",
                           name = "5. Formats"),
    drill    = dock_layout("t_drill", "downstream", name = "6. Drill"),
    agg      = dock_layout("t_agg", "agg_down", name = "7. Aggregation"),
    totals   = dock_layout("t_totals", name = "8. Grand totals")
  ),
  options = dock_board_options(),
  active = "delta",
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
