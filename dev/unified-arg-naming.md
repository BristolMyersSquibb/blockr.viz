# Shared arguments: chart and table

`new_chart_block()` and `new_table_block()` share these argument names. A
shared name means the same thing in both blocks, takes the same values and has
the same default, unless the table below says otherwise. Checked against
`fix/chart-gear-gaps` (chart) and `feat/table-rewrite` (table), 2026-10-26.

The table's `summaries` entries reuse chart names too; they are listed in the
second table.

## Block arguments

| Argument | Meaning | Values | Default | Where they differ |
|---|---|---|---|---|
| `id` | A column counted distinct instead of rows (a subject id) | column name | `NULL` (rows) | Chart: the band and the N labels (`show_n`). Table: the variables layout; in the groups layout a summary counts subjects with `func = "count_distinct"`. |
| `pct_of` | The base of a percentage | chart `"facet"`, `"category"`, `"color"`, `"total"`; table `"facet"`, `"total"` | `"facet"` | `"facet"` and `"total"` mean the same. The table has no category or colour base. Chart: the base of `func = "pct_distinct"`; table: of a count's percentage. |
| `missing` | Missing values of a category, colour or facet column (and of a Table 1 variable) | `"level"`, `"drop"` | `"level"` | `"level"` shows them as "(Missing)"; a click on that row or bar gives the rows with no value. `"drop"`: the chart keeps the rows in the panel (they still count toward `pct_distinct`); the table gives them no row. The table's records layout keeps every row and only names the header. |
| `format` | How numbers print | `"number"`, `"compact"` (1.2K, 3.4M), `"percent"` (a fraction x100 with %), a whole number of decimals 0 to 6 | `"number"` | Chart: the value axis, tooltips and value labels. Table: the records layout's cells, cards and downloads; the groups and variables layouts format their cells themselves. `"number"`: chart smart decimals, table up to two decimals. A number of decimals: the chart prints exactly that many (`2`: 4.30), the table drops trailing zeros (4.3). |
| `unit` | Free-text unit of the numbers ("CHF") | text | `""` | Chart: the axis title once, and after each value in tooltips and value labels. Table: the headers of the numeric columns once ("Revenue (CHF)"), and after the number on a card. |
| `sort_by` | Order of the categories (chart) or rows (table) | `"value"`, `"data"` (the data's order), `"alpha"` (alphabetical), a column name (each category's minimum of it) | chart by type (`"value"` for bar, pie, treemap; `"data"` for boxplot, pointrange; `"onset"` for gantt); table `"value"` | Chart also `"onset"` (gantt). Table also a summary's name or a facet level. The table reads its old `"label"` as `"alpha"`. |
| `sort_dir` | Direction of `sort_by` | `"asc"`, `"desc"` | chart by type; table `"desc"` | |
| `value_labels` | Print each value beside its mark | `TRUE`, `FALSE` | chart `FALSE`, table `TRUE` | Different on purpose: a bar chart reads off its axis, a table's bars have no axis to read from. |
| `color` | A column whose levels get the board's colours | column name | `NULL` | Different on purpose. Chart: the colour split of the marks. Table: the row tint in the records layout; the split lives on each summary (`summaries[[i]]$color`). |
| `drill` | Turns the click on and says what it filters on | `NULL`/`""` off, `"auto"`, a column name | `NULL` | Table also `"source"`: a structured or grouped table hands on the records behind the clicked row. In the table's groups layout any value turns it on and the click filters on the row's groups. |
| `drill_target`, `drill_table` | Beta. Send the click to a filter block (and the table in its dm) instead of filtering this block's output | block id or `"auto"`; table name | `""` | |
| `download` | Offer the block for download | `TRUE`, `FALSE` | `TRUE` | Chart: image, HTML, PowerPoint and the plotted numbers. Table: xlsx, html, pptx. |
| `title`, `subtitle`, `caption` | Text above and below | `NULL` the input's own label, `""` none, else a template resolved against the data | `NULL`; `caption` `getOption("blockr.viz.default_caption")` | |
| `script`, `script_values` | Prepare step: plain R on the incoming rows, and the values of its controls | R code; named list | `NULL`, `list()` | The table runs it in the groups layout. |
| `selection` | Internal: what the last click selected, saved with the board | | `NULL` | Same role, each block's own shape. |

## Summary entries (table) and chart roles

| Key | Meaning | Values | Where they differ |
|---|---|---|---|
| `func` | Aggregation | `"count"`, `"count_distinct"`, `"sum"`, `"mean"`, `"median"`, `"min"`, `"max"`, `"identity"` | The chart also has `"pct_distinct"`. |
| `color` | Splits the mark into the levels of a column | column name | Same meaning, per summary in the table. |
| `position` | How a colour split is laid out | `"stack"`, `"dodge"`, `"fill"` | |
| `facet` | One more panel (chart) or column (table) per level | column name | Chart: small multiples. Table: the summary's column repeats per level on one scale. |
| `summary` | A distribution's inner range (boxplot body, point range interval); a text cell's statistic | `"median_q1_q3"`, `"mean_sd"`, `"mean_2sd"`, `"mean_se"`, `"p5_p95"`, `"p10_p90"`, `"min_max"` | The table adds `"mean_ci95"` (a t-quantile, R only) and `"none"` (no inner range). Default: chart `"median_q1_q3"` (boxplot) or `"mean_se"` (pointrange); table `"median_q1_q3"`. |
| `whiskers` | A distribution's outer range | `"tukey"` or any `summary` value | The table adds `"mean_ci95"` and `"none"`. Default `"tukey"`; the table's point range has none. |
| `ref_lo`, `ref_hi` | A reference range | column name | Table (`pair`): also a number. |
| `x`, `xend` | Start and end of a segment | column name | Chart: the axes. Table: `pair` (dumbbell), `spans` (swimlane), `series` (`x` only). |
| `label` | A column printed with the mark | column name | Chart: the on-mark text. Table (`spans`): the event headlining each segment's tooltip. |

## Old names still read

- Table `sort_by = "label"` -> `"alpha"`.
- Table `digits` -> `format`; the gear's old `digits` key too.
- Table summaries written with the summarize table's keys (`stat`, `inner`,
  `outer`, `from` / `to`, `lo` / `hi`, `fields`): see `lane_public_summary()`.
- Table blocks saved before the rewrite: `table_legacy_args()`.
- Chart names before blockr.viz 0.3.0: `R/chart-args.R`.
