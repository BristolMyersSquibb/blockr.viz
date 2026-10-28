# blockr.viz

Chart and table blocks for [blockr](https://github.com/BristolMyersSquibb/blockr.core)
boards.

| Block | What it shows |
|---|---|
| `new_chart_block()` | An interactive chart (ECharts): bar, histogram, waterfall, pie, treemap, box, point range, scatter, line, area, band and timeline. Aggregated types count or summarize the rows per category themselves. |
| `new_table_block()` | An interactive table, in three layouts: the input's rows, one row per group with a column per summary, or a Table 1 with one row per statistic of each variable. Each drawn as a table, a compact matrix or cards. |

A click on either block can filter the blocks downstream (`drill`). Both
download as Excel, a web page, an image or PowerPoint. Settings live in a
gear on the block; titles can name the block's settings, which the reader
changes by clicking them.

## Installation

```r
# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.viz")
```

## A board

A bar chart of mean mileage per number of gears filters the table below it
when a bar is clicked.

```r
library(blockr.core)
library(blockr.dock)
library(blockr.dag)
library(blockr.viz)

cars <- transform(mtcars, gear = factor(gear), name = rownames(mtcars))

board <- new_dock_board(
  blocks = c(
    data = new_static_block(cars),
    chart = new_chart_block(
      type = "bar", x = "gear", y = "mpg", func = "mean", drill = "auto"
    ),
    table = new_table_block(rowname = "name", columns = c("mpg", "hp", "wt"))
  ),
  links = links(from = c("data", "chart"), to = c("chart", "table")),
  extensions = list(new_dag_extension())
)

serve(board)
```

`vignette("chart")` and `vignette("table")` go through the arguments.
