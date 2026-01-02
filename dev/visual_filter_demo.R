# Visual Filter Demo
#
# Demonstrates the interactive visual filter block with the bi_demo_data()
# dataset. Click on bars to filter, see filtered data flow downstream.
#
# Features:
# - Click on bars to filter across all charts
# - Clear filters button to reset
# - Filtered data flows to downstream blocks
# - Auto-detects dimensions and measures from data

library(blockr)
library(blockr.dag)
library(blockr.bi)

# ============================================================================
# Visual filter with aggregate output
# ============================================================================
#
# Uses bi_demo_data() which contains European sales data with:
# - Dimensions: Region, Country, Category, Channel
# - Measures: Revenue, Quantity, Profit, Transactions
#
# The visual filter returns filtered data, which flows to the aggregate block.

run_app(
  blocks = c(
    # Data source block using the demo dataset
    demo_data = new_static_block(bi_demo_data()),

    # Visual filter - click bars to filter
    visual_filter = new_visual_filter_block(),

    # Aggregate filtered data
    summary = new_aggregate_block(
      drill_down = c("Region", "Country"),
      values = c("Revenue", "Profit", "Transactions"),
      agg_fun = "sum"
    )
  ),
  links = c(
    new_link("demo_data", "visual_filter", "data"),
    new_link("visual_filter", "summary", "data")
  ),
  extensions = list(new_dag_extension())
)
