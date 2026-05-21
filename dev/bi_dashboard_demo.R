# BI Dashboard Demo
#
# Demonstrates blockr.bi blocks working together:
# - KPI block for multiple headline numbers (with auto-colors)
# - Pivot table for detailed analysis

library(blockr)
library(blockr.dag)
library(blockr.io)
library(blockr.bi)

# ============================================================================
# Full BI Dashboard
# ============================================================================
#
# Layout:
#   [Data] --> [KPIs: Revenue, Profit, Transactions]
#          --> [Pivot Table]

run_app(
  blocks = c(
    # Data source - read from CSV in inst/extdata
    data = new_read_block(
      path = system.file("extdata", "bi_demo_data.csv", package = "blockr.bi")
    ),

    # KPI block showing multiple headline numbers with auto-colors
    kpis = new_kpi_block(
      measures = c("Revenue", "Profit", "Transactions"),
      digits = "0",
      subtitles = c(
        Revenue = "Total revenue this year",
        Profit = "Net profit margin",
        Transactions = "Completed transactions"
      )
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
    new_link("data", "kpis", "data"),
    new_link("data", "pivot", "data")
  ),
  extensions = list(new_dag_extension())
)
