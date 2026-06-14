# blockr.viz

The render layer for blockr dashboards.

blockr.viz draws tidy, rectangular data: interactive `chart`, `table`, and
`tile` blocks (built on ECharts and HTML widgets), a static `gt` table
renderer, and a `summary_table` shaper. Renderers never compute or reshape —
they take an already-shaped data frame and turn it into pixels, with optional
cell coloring and opt-in click-to-filter drill-down.

## Installation

```r
# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.viz")
```

## Blocks

| Block | Role | What it does |
|---|---|---|
| `new_chart_block()` | Renderer | Configurable ECharts chart (bar, line, scatter, pie, treemap, boxplot, radar, gantt, waterfall). |
| `new_table_block()` | Renderer | Interactive HTML table — sticky header, sort, search, optional cell coloring. Renders flat *and* structured ("Table 1") input. |
| `new_tile_block()` | Renderer | Scorecard of bold KPI numbers — cards or an aligned matrix, with deltas / fills / status pills. |
| `new_gt_table_block()` | Renderer (static) | Renders a wide display table as a styled `gt` table for print / report / CSR output. |
| `new_summary_table_block()` | Shaper | Wide, display-shaped multi-variable summary ("list of variables by Y"); emits a tidy `.fmt` frame for any renderer. |

Drill-down (click a mark / row to emit a downstream filter) is an opt-in
feature of `chart` and `table` — off by default; set the `drill` argument to a
column name to enable it.

Call `register_viz_blocks()` once to register all five with the block-adder and
the assistant block universe.

## How it fits together

blockr.viz pairs with [blockr.dock](https://github.com/BristolMyersSquibb/blockr.dock):
**blockr.dock arranges** the blocks into a dashboard layout, **blockr.viz draws**
the data inside them. See `vignette("blockr-viz")` for the shaper / renderer
model and the five-layer pipeline.

## Demo

A bar chart aggregates rows per category and, on click, filters the table
below it. blockr.dock provides the dashboard layout; `new_dag_extension()` adds
the workflow editor.

```r
library(blockr.core)
library(blockr.dock)
library(blockr.dag)
library(blockr.viz)

register_viz_blocks()

cars <- transform(mtcars, gear = factor(gear), name = rownames(mtcars))

board <- new_dock_board(
  blocks = c(
    data  = new_static_block(cars),
    chart = new_chart_block(
      group = "gear", chart_type = "bar",
      agg_fn = "count", drill = "gear"
    ),
    tbl   = new_table_block(
      rowname = "name",
      values  = c("mpg", "hp", "wt")
    )
  ),
  links = links(
    from = c("data", "chart"),
    to   = c("chart", "tbl")
  ),
  extensions = list(new_dag_extension())
)

serve(board)
```
