# BI Dashboard Demo
#
# Demonstrates all blockr.bi blocks working together:
# - KPI block for multiple headline numbers (with auto-colors)
# - Visual filter for interactive filtering
# - Pivot table for detailed analysis
#
# The visual filter filters all downstream blocks (KPIs + pivot table)

library(blockr)
library(blockr.dag)
library(blockr.io)
library(blockr.bi)

# ============================================================================
# Full BI Dashboard
# ============================================================================
#
# Layout:
#   [Data] --> [Visual Filter] --> [KPIs: Revenue, Profit, Transactions]
#                              --> [Pivot Table]
#
# Click on the visual filter charts to filter everything!

run_app(
  blocks = c(
    # Data source - read from CSV in inst/extdata
    data = new_read_block(
      path = system.file("extdata", "bi_demo_data.csv", package = "blockr.bi")
    ),

    # Visual filter - click bars to filter all downstream blocks
    filter = new_visual_filter_block(
      dimensions = c("Region", "Category", "Channel", "Year"),
      measure = "Revenue"
    ),

    # KPI block showing multiple headline numbers with auto-colors
    kpis = new_kpi_block(
      measures = c("Revenue", "Profit", "Transactions"),
      prefix = "$",
      digits = "0"
    ),

    # Pivot table for detailed breakdown
    pivot = new_pivot_table_block(
      rows = c("Region", "Country"),
      cols = "Category",
      measures = c("Revenue", "Profit"),
      digits = "0"
    )
  ),
  links = c(
    # Visual filter receives data
    new_link("data", "filter", "data"),

    # KPIs and pivot receive filtered data
    new_link("filter", "kpis", "data"),
    new_link("filter", "pivot", "data")
  ),
  extensions = list(new_dag_extension())
)
