# Pivot Table Demo
#
# Demonstrates the flexible pivot table block for slice & dice analysis.
# Map dimensions to rows/columns, and the block automatically aggregates.
#
# Features:
# - Map any dimension to rows or columns
# - Multiple dimensions per axis (combined with " | ")
# - Unmapped dimensions are aggregated over
# - Choose measure and aggregation function

library(blockr)
library(blockr.dag)
library(blockr.bi)

# ============================================================================
# Example 1: Simple pivot - Region x Category
# ============================================================================
#
# Shows revenue by Region (rows) and Category (columns)

run_app(
  blocks = c(
    demo_data = new_static_block(bi_demo_data()),

    pivot = new_pivot_table_block(
      rows = "Region",
      cols = "Category",
      measure = "Revenue",
      agg_fun = "sum"
    )
  ),
  links = c(
    new_link("demo_data", "pivot", "data")
  ),
  extensions = list(new_dag_extension())
)


# ============================================================================
# Example 2: Multi-level rows - Region > Country
# ============================================================================
#
# Shows revenue by Region and Country (hierarchical rows) x Category

if (FALSE) {
  run_app(
    blocks = c(
      demo_data = new_static_block(bi_demo_data()),

      pivot = new_pivot_table_block(
        rows = c("Region", "Country"),
        cols = "Category",
        measure = "Revenue",
        agg_fun = "sum"
      )
    ),
    links = c(
      new_link("demo_data", "pivot", "data")
    ),
    extensions = list(new_dag_extension())
  )
}


# ============================================================================
# Example 3: Combined with Visual Filter
# ============================================================================
#
# Use visual filter to select data, then pivot the filtered results

if (FALSE) {
  run_app(
    blocks = c(
      demo_data = new_static_block(bi_demo_data()),

      # Visual filter for interactive selection
      filter = new_visual_filter_block(
        dimensions = c("Region", "Channel", "Year"),
        measure = "Revenue"
      ),

      # Pivot filtered data
      pivot = new_pivot_table_block(
        rows = "Country",
        cols = "Category",
        measure = "Revenue",
        agg_fun = "sum"
      )
    ),
    links = c(
      new_link("demo_data", "filter", "data"),
      new_link("filter", "pivot", "data")
    ),
    extensions = list(new_dag_extension())
  )
}
