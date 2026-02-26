# Visual Filter Demo
#
# Demonstrates the visual filter block (echarts4r-based).
#
# Features:
# - Clickable bar charts for each dimension
# - Crossfilter: clicking on one chart filters all others
# - Clear filters button to reset

library(blockr)
library(blockr.dag)
library(blockr.bi)

# ============================================================================
# Visual Filter with Pivot Table Summary
# ============================================================================
#
# Uses bi_demo_data() which contains European sales data with:
# - Dimensions: Region, Country, Category, Channel
# - Measures: Revenue, Quantity, Profit, Transactions

run_app(
  blocks = c(
    # Data source block using the demo dataset
    demo_data = new_static_block(bi_demo_data()),

    # Visual filter - click bars to filter
    visual_filter = new_visual_filter_block(),

    # Pivot table for visual filter output
    summary = new_pivot_table_block(
      rows = c("Region", "Country"),
      measures = c("Revenue", "Profit", "Transactions"),
      agg_fun = "sum"
    )
  ),
  links = c(
    new_link("demo_data", "visual_filter", "data"),
    new_link("visual_filter", "summary", "data")
  ),
  extensions = list(new_dag_extension())
)
