library(blockr.core)
library(blockr.viz)

# A small, deterministic frame: one categorical key with repeated values to
# drill on, two clean numeric columns to render data bars / heatmaps over.
viz_data <- data.frame(
  region  = c("North", "North", "South", "South", "East", "West"),
  product = c("A", "B", "A", "B", "A", "A"),
  revenue = c(100, 50, 80, 40, 60, 30),
  profit  = c(10, 5, 8, 4, 6, 3),
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
        metric     = "revenue",
        agg_fn     = "sum",
        drill      = "region"
      ),
      # KPI tile matrix, click-to-filter drill on the group.
      tile = new_tile_block(
        value = "revenue",
        by    = "region",
        drill = TRUE
      ),
      # Display-shaped summary + a gt render of a plain frame.
      summary = new_summary_table_block(
        state = list(vars = c("revenue", "profit"), by = "region")
      ),
      gt = new_gt_table_block()
    ),
    links = c(
      new_link("data", "table", "data"),
      new_link("data", "bars", "data"),
      new_link("data", "heat", "data"),
      new_link("data", "chart", "data"),
      new_link("data", "tile", "data"),
      new_link("data", "summary", "data"),
      new_link("data", "gt", "data")
    )
  ),
  id = "board"
)
