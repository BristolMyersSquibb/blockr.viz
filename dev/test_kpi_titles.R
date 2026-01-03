# Test KPI titles and subtitles
# Run this to verify titles/subtitles work in the UI

library(blockr)
library(blockr.dag)
library(blockr.io)
library(blockr.bi)

# Test 1: KPI block with predefined titles and subtitles
run_app(
  blocks = c(
    data = new_read_block(
      path = system.file("extdata", "bi_demo_data.csv", package = "blockr.bi")
    ),
    kpis = new_kpi_block(
      measures = c("Revenue", "Profit", "Transactions"),
      digits = "0",
      titles = c(
        Revenue = "Total Revenue",
        Profit = "Net Profit",
        Transactions = "Count"
      ),
      subtitles = c(
        Revenue = "Year to date",
        Profit = "After taxes",
        Transactions = "All channels"
      )
    )
  ),
  links = c(
    new_link("data", "kpis", "data")
  ),
  extensions = list(new_dag_extension())
)
