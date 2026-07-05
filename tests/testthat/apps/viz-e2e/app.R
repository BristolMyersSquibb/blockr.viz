library(blockr.core)
library(blockr.viz)

# A small, deterministic frame: one categorical key with repeated values to
# drill on, two clean numeric columns to render data bars / heatmaps over.
viz_data <- data.frame(
  region  = c("North", "North", "South", "South", "East", "West"),
  product = c("A", "B", "A", "B", "A", "A"),
  revenue = c(100, 50, 80, 40, 60, 30),
  profit  = c(10, 5, 8, 4, 6, 3),
  growth  = c(0.12, -0.04, 0.20, -0.10, 0.08, 0.00), # deltas for the tile
  # Unique high-precision values: displayed rounded (digits = 2), so a drill
  # on this column only works if the click emits the RAW value (data-raw).
  ratio   = c(0.123456, 0.234567, 0.345678, 0.456789, 0.567891, 0.678912),
  stringsAsFactors = FALSE
)

serve(
  new_board(
    blocks = c(
      data = new_static_block(data = viz_data),
      # Click-to-filter drill source: clicking a row emits a categorical
      # filter on `region`; the block re-filters its own (downstream) output.
      table = new_table_block(
        rowname = "region",
        values  = c("revenue", "profit"),
        drill   = "region"
      ),
      # Cell-visual renderer: data bars over the numeric columns. Starts with
      # no drill so the config round-trip test can switch it on.
      bars = new_table_block(
        rowname    = "region",
        values     = c("revenue", "profit"),
        cell_color = drilldown_table_color("bar")
      ),
      # Cell-visual renderer: diverging heatmap (solid cell backgrounds).
      heat = new_table_block(
        rowname    = "region",
        values     = c("revenue", "profit"),
        cell_color = drilldown_table_color("diverging")
      ),
      # Aggregated bar chart, click-to-filter drill on the group.
      chart = new_chart_block(
        chart_type = "bar",
        group      = "region",
        value     = "revenue",
        func     = "sum",
        drill      = "region"
      ),
      # Aggregated pie: clicking a slice drills on the group.
      chart_pie = new_chart_block(
        chart_type = "pie",
        group      = "region",
        value     = "revenue",
        func     = "sum",
        drill      = "region"
      ),
      # Individual scatter split by region: clicking a point drills its region.
      chart_scatter = new_chart_block(
        chart_type = "scatter",
        x          = "revenue",
        y          = "profit",
        color      = "region",
        drill      = "region"
      ),
      # Brushable scatter (numeric x, no series): a rect brush is a geometric
      # x/y range filter.
      chart_brush = new_chart_block(
        chart_type = "scatter",
        x          = "revenue",
        y          = "profit",
        drill      = "auto"
      ),
      # Dedicated chart for the gear-popover engine test (mutating its
      # chart_type must not disturb the other charts' drill tests).
      chart_cfg = new_chart_block(
        chart_type = "bar",
        group      = "region",
        value     = "revenue",
        func     = "sum",
        drill      = "region"
      ),
      # KPI tile matrix, click-to-filter drill on the group.
      tile = new_tile_block(
        value = "revenue",
        group = "region",
        drill = TRUE
      ),
      # Tile in table (matrix) layout with a delta-styled secondary: covers
      # the cell-coloring render and a real matrix-row drill click.
      tile_x = new_tile_block(
        value     = "revenue",
        group     = "region",
        secondary = "growth",
        style     = "delta",
        layout    = "table",
        drill     = TRUE
      ),
      # Numeric-column drill: the display rounds ratio to 2 digits, the
      # click must filter on the raw value (drill value fidelity).
      table_num = new_table_block(
        rowname = "region",
        values  = c("ratio", "revenue"),
        drill   = "ratio",
        digits  = 2L
      ),
      # "Restored board" fixtures: filter state passed through the ctor is
      # exactly what a board restore does — these must come up already
      # filtered, with the active highlight and the status footer.
      table_restored = new_table_block(
        rowname       = "region",
        values        = c("revenue", "profit"),
        drill         = "region",
        filter_column = "region",
        filter_values = list("North")
      ),
      tile_restored = new_tile_block(
        value         = "revenue",
        group         = "region",
        drill         = TRUE,
        filter_column = "region",
        filter_values = list("South")
      ),
      chart_restored = new_chart_block(
        chart_type    = "bar",
        group         = "region",
        value         = "revenue",
        func          = "sum",
        drill         = "region",
        filter_column = "region",
        filter_values = list("North")
      ),
      # A genuine DOWNSTREAM consumer of the drill table: its result is the
      # table's filtered output flowing through a real board link.
      table_ds = new_head_block(n = 100L),
      # Display-shaped summary + a gt render of a plain frame.
      summary = new_summary_table_block(
        vars = c("revenue", "profit"), by = "region"
      ),
      gt = new_gt_table_block()
    ),
    links = c(
      new_link("data", "table", "data"),
      new_link("data", "bars", "data"),
      new_link("data", "heat", "data"),
      new_link("data", "chart", "data"),
      new_link("data", "chart_pie", "data"),
      new_link("data", "chart_scatter", "data"),
      new_link("data", "chart_brush", "data"),
      new_link("data", "chart_cfg", "data"),
      new_link("data", "tile", "data"),
      new_link("data", "tile_x", "data"),
      new_link("data", "table_num", "data"),
      new_link("data", "table_restored", "data"),
      new_link("data", "tile_restored", "data"),
      new_link("data", "chart_restored", "data"),
      new_link("table", "table_ds", "data"),
      new_link("data", "summary", "data"),
      new_link("data", "gt", "data")
    )
  ),
  id = "board"
)
