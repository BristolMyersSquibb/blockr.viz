# blockr.bi

Business Intelligence Blocks for [blockr](https://github.com/cynkra/blockr).

## Installation

```r
# install.packages("pak")
pak::pak("cynkra/blockr.bi")
```

## Blocks

### Visual Filter Block

Interactive crossfilter-style charts for filtering data. Click on bars to filter.

```r
new_visual_filter_block(
  dimensions = c("Region", "Category", "Channel"),
  measure = "Revenue"
)
```

### KPI Block

Display key performance indicators as prominent numbers with colored labels.

```r
new_kpi_block(
  measures = c("Revenue", "Profit", "Transactions"),
  titles = c(Revenue = "Total Revenue", Profit = "Net Profit"),
  subtitles = c(Revenue = "Year to date")
)
```

### Pivot Table Block

Create pivot tables with rows, columns, and measures.
```r
new_pivot_table_block(
  rows = c("Region", "Country"),
  cols = "Category",
  measures = c("Revenue", "Profit")
)
```

## Demo

```r
library(blockr)
library(blockr.dag)
library(blockr.io)
library(blockr.bi)

run_app(
 blocks = c(
    data = new_read_block(
      path = system.file("extdata", "bi_demo_data.csv", package = "blockr.bi")
    ),
    filter = new_visual_filter_block(
      dimensions = c("Region", "Category", "Channel", "Year"),
      measure = "Revenue"
    ),
    kpis = new_kpi_block(
      measures = c("Revenue", "Profit", "Transactions"),
      subtitles = c(
        Revenue = "Total revenue this year",
        Profit = "Net profit margin",
        Transactions = "Completed transactions"
      )
    ),
    pivot = new_pivot_table_block(
      rows = c("Region", "Country"),
      cols = "Category",
      measures = c("Revenue", "Profit")
    )
  ),
  links = c(
    new_link("data", "filter", "data"),
    new_link("filter", "kpis", "data"),
    new_link("filter", "pivot", "data")
  ),
  extensions = list(new_dag_extension())
)
```
