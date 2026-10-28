# The table block in R: a map

Which file does what, and how a state becomes the payload the browser
draws. The JS side is `inst/js/table/` (its view reads the TableModel of
`inst/js/table/table.d.ts`).

## From state to payload

```
new_table_block(...)                     R/table-block.R
  legacy_ctor_args()  old names          R/legacy-restore.R, R/legacy.R
  viz_check()         the spec           R/spec-table.R, R/spec-table-summaries.R
  table_normalize()   cross-argument     R/table-normalize.R
        |
table_server()                           R/table-server.R
  viz_block_server()  the kit: state, settings, clicks, payload output
  table_sources()     the input after the prepare script (R/table-prepare.R)
  one part per layout, the current layout's part reads the data:
    records    table_records_part()      R/table-records.R
    groups     table_groups_part()       R/table-groups.R
    variables  table_variables_part()    R/table-variables.R
        |
part$view()  ->  table_payload()  ->  viz_payload()  (R/kit-output.R)
  payload.data     the TableModel (only sent again when it changes)
  payload.columns  the input's columns, for the gear's pickers
  payload.derived  titles, empty state, receipt, downloads, script values
```

The state holds the spec's argument names (`layout`, `by`, `summaries`,
`drill`, ...). Each layout's model reads them through `R/table-model.R`
(`table_model_records()`, `table_model_groups()`,
`table_model_variables()`), which are pure functions of state and data.

A part also gives `expr(sel)` (the emitted filter, R/table-expr.R),
`claims(sel)` and `receipt(sel)` (what a click claims and how it reads,
R/table-drill.R) and `exhibit(tt)` (what the downloads write,
R/table-exhibit.R and R/table-download.R).

## The groups layout

One row per level of `by`, one column per summary entry. Three steps, each
with its own name prefix:

1. `groups_prepare()` (R/groups-prepare.R): the input as one row per
   group with every summary's numbers in columns, sorted and cut to
   `top_n`, plus one plan entry per column. Entries are normalized under
   the spec's keys first (R/groups-entries.R). The numbers come from
   R/groups-build.R (per type), R/groups-custom.R (custom functions),
   R/groups-stats.R (box and point-range statistics), R/groups-tile.R,
   R/groups-aggregate.R and R/groups-sort.R; the plan entries from
   R/groups-plan.R and their scales from R/groups-domains.R.
2. `groups_cells()` (R/groups-cells*.R, prefix `cells_`): the drawing
   geometry of each column from those rows: bar widths, box and range
   positions, swimlane segments, sparkline paths, tooltips, value labels.
3. `table_groups_view()` (R/table-groups-view.R, R/table-groups-cols.R):
   the TableModel. Each cell model column becomes a TableCol: its text,
   its mark (the cell model's fields as JSON arrays) and its axis; the head
   rows, row claims and nesting come from the prepared model.

The prepared rows and the cell model are two steps on purpose: the rows
are the numbers, which the downloads (`summarize_export_df()`) and the
cards also read; the cell model is the geometry, which the view and the
picture export (R/summarize-paint.R) read.

## The records and variables layouts

Records: `table_model_records()` builds `dt_flat_build()`'s cell model
(R/table-model-records.R): display strings, paint from the `shadings` rules
(R/table-model-shading.R), number formats (R/table-model-format.R), drill
raw values and widths. `table_records_view()` turns it into the TableModel.
A nested or structured frame (dotted columns, `table_is_structured()`) goes
through R/table-structured*.R instead. `look = "cards"` draws cards
(R/table-cards.R).

Variables: a Table 1 from `summary_table()` (R/summary-table.R), drawn as a
structured frame (R/table-variables.R).

## Shared by every layout

| file | holds |
|---|---|
| R/table-block.R | the constructor, its UI and the assistant's text about the block |
| R/table-server.R | the server, the sources, titles and the payload |
| R/table-model.R | each layout's model from state and data |
| R/table-prepare.R | the prepare script run on the input |
| R/table-normalize.R | rules between arguments |
| R/table-drill.R | click claims, receipts and drill filters |
| R/table-expr.R | the emitted filter |
| R/table-aggregate.R | the records aggregation, also used by the chart |
| R/table-model-format.R, R/table-model-shading.R | number formats, shading |
| R/table-dep.R | the view's scripts and stylesheets |
| R/legacy*.R | boards and code from earlier blockr.viz (delete with them) |

## Tests

The recordings (`tests/testthat/test-table-golden.R`, helpers
`helper-table-rewrite.R`, `helper-table-golden.R`) replay the reference
block's scenarios against the new one. `test-table-groups-view.R` checks the
groups TableModel against the recorded payloads. `test-groups-*.R` test the
groups engine's steps, `test-legacy-*.R` the restore paths.
