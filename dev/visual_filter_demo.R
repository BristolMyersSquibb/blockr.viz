# Filter Block Comparison Demo
#
# Compares the table filter block (new, reactable-based) with
# the visual filter block (original, echarts4r-based).
#
# Features of table filter:
# - Sortable, searchable tables for each dimension
# - Click on rows to filter across all tables (crossfilter)
# - Inline bar charts show relative values
# - Clear filters button to reset

library(blockr)
library(blockr.dag)
library(blockr.bi)

# ============================================================================
# Compare: Table Filter vs Visual Filter
# ============================================================================
#
# Uses bi_demo_data() which contains European sales data with:
# - Dimensions: Region, Country, Category, Channel
# - Measures: Revenue, Quantity, Profit, Transactions

run_app(
  blocks = c(
    # Data source block using the demo dataset
    demo_data = new_static_block(bi_demo_data()),

    # NEW: Table filter - click rows to filter
    table_filter = new_table_filter_block(),

    # ORIGINAL: Visual filter - click bars to filter (for comparison)
    visual_filter = new_visual_filter_block(),

    # Pivot table for table filter output
    summary1 = new_pivot_table_block(
      rows = c("Region", "Country"),
      measures = c("Revenue", "Profit", "Transactions"),
      agg_fun = "sum"
    ),

    # Pivot table for visual filter output
    summary2 = new_pivot_table_block(
      rows = c("Region", "Country"),
      measures = c("Revenue", "Profit", "Transactions"),
      agg_fun = "sum"
    )
  ),
  links = c(
    new_link("demo_data", "table_filter", "data"),
    new_link("demo_data", "visual_filter", "data"),
    new_link("table_filter", "summary1", "data"),
    new_link("visual_filter", "summary2", "data")
  ),
  extensions = list(new_dag_extension())
)
