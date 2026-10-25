# Table block rewrite: requirements

2026-10-25. Mockup with the interactive block, the heatmaps and all tables
below: `_scratch/blockx/table-mockup/index.html` in the workspace. Argument
names follow the chart block review, `_scratch/blockx/viz-mockup/arg-names.md`.
Background: `dev/table-and-chart-architecture.md`, the annotated data frame
spec (`_blockr.design/open/annotated-data-frame/`) and the summarize table
spec (`_blockr.design/open/summarize-table/`).

## Goal

One block, `new_table_block()`, for every table blockr.viz draws. It replaces:

| Today | In the new block |
|---|---|
| `new_table_block()` (flat table, in-block aggregation, annotated frames) | layout `"records"`, pass-through |
| `new_summarize_table_block()` (one row per group, glyph cells) | layout `"groups"` |
| `new_summary_table_block()` (Table 1, one row per variable) | layout `"variables"` |
| `new_heatmap_block()` (matrix engine behind the AE heatmap) | layout `"groups"` + `compact = TRUE` |
| `new_gt_table_block()` | retired; gt can become a download format |

Written in R first. blockx follows through the blockx-ready contract.

## Why one block

The group-by summary and Table 1 hold the same numbers. Every cell is one
group level, one variable and one statistic; the group-by summary puts group
levels on rows, Table 1 puts them on columns. Behind both is the long ARD
shape that {cards} produces and a SQL `GROUP BY` returns. So `by` and
`summaries` mean the same in both layouts, and one switch picks the
orientation.

The flat table adds one job the summary layouts lack: showing a frame built
elsewhere. One block can do that too: input that already is a finished table
is shown as it is.

## Layouts and inputs

- **Records.** Input rows as they are. Row name column, optional `by` for
  collapsible row groups, cell colour, sort, search, drill. Also the target
  for a tidy summary from dplyr: `by = "species", rowname = "island"` nests it.
- **Groups.** One row per level of `by`, one column per summary, text or glyph
  cells.
- **Variables.** One row per statistic or level, one column per level of `by`.
- **Finished table (pass-through).** Input is a gtsummary object, a composer
  object or a frame with annotated columns (`.variable`, `.label`, ...). The
  block converts it (the `as_annotated_df()` methods run inside the block) and
  shows it as it is. Layout and summaries are hidden behind a badge.

Output, as for chart and tile: the input rows, filtered by the last drill
click. Downloads (xlsx, html, pptx) carry the table as shown. Table 1 as a data
output for another block is given up; its only consumer was `gt_table`.

Class stays `table_block`. blockr.outline's export checks
`inherits(blk, c("table_block", ...))` (`R/export.R:558`).

## Requirements

### Contract

1. Inside the block every finished table is an annotated frame (format v2):
   gtsummary and composer objects are converted to it, and the downloads read
   it. User code never builds it.
2. Numbers stay numbers. Rounding and composites such as "13 (18.8%)" are
   render choices, so Excel gets numbers and sorting works.
   `summary_table()` writes character cells today.
3. Column labels and the footnotes of a converted table survive dplyr,
   serialization and the Parquet / DuckDB step in blockx. Title and caption
   are block arguments and do not travel with the data.

### The block

4. One block, `new_table_block()`, class `table_block`, with `layout` =
   `"records"`, `"groups"` or `"variables"`, and pass-through for gtsummary,
   composer and annotated frames.
5. Argument names match the chart block (table below), including `script` /
   `script_values`, so a prepare snippet works in both blocks.
6. One statistics vocabulary across chart, tile and table: the chart's `func`
   and `summary` values. Today there are two (`AGG_FNS` and the
   `summary_table` stat keys).
7. Denominators are explicit: `id` for distinct counts, `pct_of`, `missing`,
   N in the column header.
8. A compact matrix look (`compact = TRUE`) in every layout, good enough to
   replace the heatmap block: the AE heatmap at 184 × 25 and a correlation
   matrix read without x overflow.
9. Nested categoricals (species → island, SOC → PT) give collapsible nested
   rows, sorted by frequency within each level, with an optional top N and
   "Other". Group headers look the same in every layout: bold title, N muted
   beside it, chevron left of the stub edge (as since `c79f5fb`).
10. No domain words in the block. Clinical terms come from column labels and
    board options, so the same block works on penguins and ADSL.
11. Old boards load: `new_summarize_table_block()` and
    `new_summary_table_block()` forward; legacy argument names go through
    `blockr_deser.table_block()`. The devmaster board's uses (below) are the
    acceptance test.

### Code in front of the block

12. Code in front of a table block (an R block, the `script` prepare step, the
    assistant's code) uses existing packages only: dplyr, gtsummary, {cards},
    composer. No blockr function, no dotted columns written by hand. It runs
    and can be verified without blockr.
13. Presentation lives in the table block's arguments: `by` and `rowname` nest
    a tidy frame, `title` and `caption` label it. The assistant learns the
    block's arguments, not a column convention.

### blockx

14. blockx hosts the new table widget under the block type `table` and uses
    it on pages. Today it keys the summarize widget on `summarize_table`
    (`web/src/blockr/registry.ts`, `shared/blocks.ts`).
15. The block follows the blockx-ready contract: summaries as IR (DuckDB
    `GROUP BY`), layout as a shared pure function, R stays the source of
    truth.

## Arguments

```r
new_table_block(
  layout        = "records",    # "records", "groups", "variables"
  by            = character(),  # groups: one row per level; variables: one
                                # column per level; records: collapsible row
                                # groups. Outer first, at most 2.
  rowname       = NULL,         # records: stub column
  summaries     = list(),       # groups, variables: one entry per column
  id            = NULL,         # count distinct values of this column
  pct_of        = "by",         # denominator of percentages: "by", "total"
  missing       = "level",      # missing categories: "level", "drop"
  total         = FALSE,        # total row (groups) or column (variables)
  shadings      = list(),       # cell colour rules: list(list(mode, cols, domain))
  format        = "number",     # default number format, as chart and tile
  compact       = FALSE,        # square cells, rotated titles, blank empties
  value_labels  = TRUE,
  facet_layout  = "by_summary",
  sort_by       = "value",
  sort_dir      = "desc",
  top_n         = NULL,
  drill         = NULL,
  drill_target  = "",
  drill_table   = "",
  search        = TRUE,
  sortable      = TRUE,
  collapsible   = TRUE,
  max_height    = NULL,
  download      = FALSE,
  title         = NULL,
  subtitle      = NULL,
  caption       = getOption("blockr.viz.default_caption"),
  script        = NULL,         # prepare step: plain R on the incoming rows
  script_values = list(),
  selection     = NULL,         # internal: what the last click selected
  ...
)

# one entry of `summaries`
list(
  type     = "dist",     # simple, dist, share, field, pair, series, spans, expr
  col      = "body_mass",
  func     = NULL,       # simple: count, count_distinct, sum, mean, ...
  summary  = "mean_sd",  # dist: mean_sd, median_q1_q3, ...; several in "variables"
  whiskers = NULL,
  show     = "box",      # number, bar, dot, box, pointrange, text, tile, ...
  fill     = NULL,       # tile: column whose worst level paints the cell
  name     = "Body mass (g)",
  color    = NULL, position = "stack",
  facet    = NULL,
  format   = NULL, unit = NULL,
  ref_lo   = NULL, ref_hi = NULL,
  x        = NULL, xend = NULL,
  label    = NULL, tooltip = NULL
)
```

### Same concept, same name

| Concept | Chart (after review) | Tile (today) | New table | Today in table / summarize / Table 1 |
|---|---|---|---|---|
| kind of display | `type` | `layout` | `layout` | three blocks |
| grouping column | `x` / `y` | `group` | `by` | `group` / `by` / `by` |
| value column | `y` | `value` | `summaries$col` | `value` / `col` / `vars` |
| aggregation | `func` | `summaries$func` | `summaries$func` | `func` |
| distribution statistic | `summary` | | `summaries$summary` | `stat` / `stats` |
| colour split | `color` | `color` | `summaries$color` | `color` |
| split mode | `position` (stack, dodge, fill) | | `summaries$position` | `bar_mode` (stacked, grouped, percent) |
| small multiples | `facet` | | `summaries$facet`, `facet_layout` | same |
| individual id | `id` | | `id` | `id_var` |
| percent denominator | `pct_of` | | `pct_of` | |
| missing categories | `missing` | | `missing` | |
| reference range | `ref_lo`, `ref_hi` | | `summaries$ref_lo`, `$ref_hi` | `lo`, `hi` (pair) |
| segment | `x`, `xend` | | `summaries$x`, `$xend` | `from`, `to` (pair) |
| tooltip columns | `tooltip` | | `summaries$tooltip` | `fields` (spans) |
| paint by another column | | | `summaries$fill` | `color` (heatmap), `source` (shading) |
| matrix look | | | `compact` | `rotate_titles`; always on (heatmap) |
| number format | `format`, `unit` | `format`, `unit` | `format`, `summaries$format`, `$unit` | `digits` |
| drill | `drill`, `drill_target`, `drill_table` | `ctrl_*` | as chart | `ctrl_*` |
| click state | `selection` | `filter_*` | `selection` | `filter_*` |
| prepare step | `script`, `script_values` | | same | `script`, `values` |
| text | `title`, `subtitle`, `caption` | `caption` | same | same |

### Old boards

As for the chart: the constructor has no deprecated formals. A
`blockr_deser.table_block()` method renames keys of boards saved before the
CRAN release, from one internal table (`table_legacy_names`), and folds the
`filter_*` fields into `selection`. `new_summarize_table_block()` and
`new_summary_table_block()` stay as forwarding constructors and leave the
block list.

| Old block | Old argument | New |
|---|---|---|
| table | `cell_color`, `row_color`, `color` | `shadings` |
| table | `group`, `value`, `summaries` (in-block aggregation) | `layout = "groups"`, `by`, `summaries` |
| table | `digits` | `format` |
| table | `excel_download`, `html_download`, `pptx_download`; `rotate_titles` | `download`; `compact` |
| summarize | `group`, `value`, `func`, `parent`, block-level `color` / `facet` / `bar_mode` | one `simple` summary; `parent` becomes the outer `by`; `bar_mode` becomes `position` |
| summarize | `id_var`; `values` | `id`; `script_values` |
| summarize | summary keys `stat`, `lo`, `hi`, `from`, `to`, `fields` | `summary`, `ref_lo`, `ref_hi`, `x`, `xend`, `tooltip` |
| summarize | `cols`, `fields`, `axis`, `bar_width` | to check one by one |
| summary_table | `vars` + `stats` | one summary per variable: `dist` with `summary` for numeric, `share` for categorical |
| summary_table | `sections`, `nest_hierarchies` | nested `share`: `col = c("AESOC", "AEDECOD")` |
| summary_table | `add_overall`, `overall_label` | `total` (label "Total") |
| summary_table | `id_var`; `indent_details` | `id`; dropped (always indented) |
| all | `ctrl_target`, `ctrl_table`, `filter_*` | `drill_target`, `drill_table`, `selection` |

## Heatmaps

The heatmap block folds into the table. Its matrix look is `compact = TRUE`:
square cells with a 28px floor, rotated and sticky column titles, empty cells
left blank. Groups use the same collapsible header rows as every other
layout; the grey rail of the August 2026 version does not come back.

AE heatmap, one row per subject grouped by arm, one column per term, count
shown, painted by worst severity:

```r
new_table_block(
  layout    = "groups",
  by        = c("ACTARM", "USUBJID"),
  summaries = list(list(
    type  = "simple", func = "count",
    facet = "AEDECOD",
    show  = "tile",
    fill  = "AESEV"
  )),
  compact   = TRUE,
  sort_by   = "value", sort_dir = "desc",
  drill     = "USUBJID",
  script    = "
top_n <- 25  #| number(min = 1, max = 200)
top <- dplyr::count(data, AEDECOD, sort = TRUE)
dplyr::filter(data, AEDECOD %in% utils::head(top$AEDECOD, top_n))"
)
```

The prepare script keeps only the top-N cap, as `ae_heatmap_script()` does
today. Counting and the worst level are the block's own summary, so each cell
keeps its subject and term. The ConMed heatmap is the same call with
`facet = "CMDECOD"` and no `fill`: cells are then painted by count.

Correlation matrix (`correlate` output, one row per variable):

```r
new_table_block(
  layout   = "records",
  rowname  = "var",
  shadings = list(list(mode = "diverging", domain = c(-1, 1))),
  compact  = TRUE
)
```

The renderer takes over from the heatmap: the compact display, `show = "tile"`
with `fill` (worst level of an ordered factor in its own order, numeric grades
numerically, a character column sorted), facet columns ordered by count that
stay usable at 25 and more columns, the status line
(`184 × 25 of 230 AE Term`), and the pptx download that splits a long matrix
over several slides.

Afterwards these go: `new_heatmap_block()`, `heatmap_matrix()`,
`R/heatmap-block.R`, `R/heatmap-html.R`, `inst/js/heatmap-block.js`,
`inst/css/heatmap-block.css` and the hook at `R/html-table.R:1749`.

### `blockr.pharma::new_ae_heatmap_block()`

Not needed as a block after the rewrite. Its three reasons today: it is the
registered surface (the engine is not registered, and an unregistered block
in a saved board breaks the dock on restore), it carries AE defaults, and
saved boards name it as their constructor. The table block is registered,
and the defaults become ordinary arguments in the board. What remains is old
boards.

So it shrinks to a forwarding function: it returns
`new_table_block(layout = "groups", ...)` and stamps `new_table_block` from
blockr.viz as the constructor, the reverse of what it stamps today. An old
board loads through it and saves as `new_table_block` on its next save. Once
the prod boards are re-saved, delete it.

Goes with it: the AE heatmap tests in blockr.pharma, the deploy check in
`blockr.sandbox/deploy-simple.R:948-970`, the load-order note in the devmaster
board's `_boot.R`, and the `ae_heatmap_block` example in blockr.sandbox's
`clinical-figures` skill.

## Devmaster board (blockr.sandbox)

Acceptance test for loading and look. Its clinical tables are composer blocks
and do not change.

| Block | Today | New |
|---|---|---|
| correlation heatmap (`explore_cor_tbl`) | `new_table_block(rowname = "var", cell_color = drilldown_table_color("diverging", domain = c(-1, 1)))` | `layout = "records"`, `rowname = "var"`, `shadings = list(list(mode = "diverging", domain = c(-1, 1)))`, `compact = TRUE` |
| baseline to peak (`lab_pair`) | `new_summarize_table_block(by = "USUBJID", summaries = list(field, pair), ..., ctrl_target = "pt_drill")` | `layout = "groups"`, same `by` and summaries (pair keys renamed), `drill_target = "pt_drill"` |
| AE heatmap, ConMed heatmap | `new_ae_heatmap_block(...)` | `new_table_block()` calls as above |
| checks | `checks/figure-catalogue.R` names the id `summarize_table_block` | update the id |

Prod master and study boards were not checked. They load through the
forwarding constructors.

## Open decisions

1. Grouping column: `by` or `group`. Proposal `by`: blockr.dplyr's summarize
   has it on CRAN and dplyr uses `.by`. The tile renamed `by` to `group` in
   June and would rename back.
2. Distribution statistic key: `summary` (the chart's name) inside each entry
   of `summaries`. The alternative is `stat` in both blocks, which means a
   rename in the chart review too.
3. Glyphs in layout "variables": text only, or a glyph per cell where a row
   shares a scale.
4. Composite cells: character in the frame, or numbers plus a template (R2).
5. How column labels and footnotes reach blockx (R3): attributes in Parquet
   metadata, or a small sidecar with the result.
6. Accept a {cards} ARD as input and lay it out with `layout` without
   recomputing. Cards keeps statistics in a list column, which needs a check
   for the Parquet step.
7. Names for the matrix pieces: `compact` and `fill`.
8. `digits` to `format`: enough, or does a table need fixed decimals per
   column?

## Implementation phases

Branch `feat/table-rewrite`. The chart block's CRAN branch (`feat/chart-cran`)
edits `R/legacy-deser.R`, `NAMESPACE` and `DESCRIPTION`; the table keeps its
legacy handling in its own files (`R/table-args.R`) to keep the merge small.

1. **One constructor, two engines.** `new_table_block()` with the new
   signature and a `layout` state. Its server runs the current table server
   (records) and the current summarize server (groups) as sub-modules, on a
   shared set of reactiveVals for the cross-cutting settings (title,
   subtitle, caption, download, search, sortable, drill target). Only the
   active engine sees data; the other is parked. Old names in code and in
   saved boards translate through `table_legacy_args()`. In-block aggregation
   of the old table (`group` + `summaries` of `{func, cols}`) restores as
   layout "groups" with one `simple` summary per column.
2. **Layout in the gear.** "Rows are" in both gears; switching swaps the
   engine and keeps the shared settings.
3. **Variables layout.** `summary_table()` computed from the `summaries`
   list, drawn by the records engine's structured path. gtsummary input
   through `as_annotated_df()`.
4. **Heatmap.** `show = "tile"` with `fill`, facet over 25+ levels ordered
   by count (the summarize facet caps at 15 today, `LANE_MAX_LEVELS`), and
   `compact = TRUE` (rotated headers, square cells) in the groups engine.
   AE heatmap on top of it; heatmap engine deleted.
5. **Vocabulary.** Summary keys renamed (`stat` -> `summary`, `lo`/`hi` ->
   `ref_lo`/`ref_hi`, `from`/`to` -> `x`/`xend`, `fields` -> `tooltip`,
   `bar_mode` -> `position`), `digits` -> `format`, `pct_of`, `missing`.
   Registry, argument specs, guidance, `gt_table` retired.
6. **One renderer.** Merge `table.js` and `summarize-table.js`.
7. **blockx.**

### Status (2026-10-26)

Phases 1 to 3 are done on `feat/table-rewrite`. Deviations from the sketch
above:

- The records layout keeps a `columns` argument (the old `value`).
- `...` sits right after `layout`, so old names cannot partially match new
  ones (`value` would match `value_labels`).
- `digits` stays until phase 5.
- The variables layout takes the Table 1 settings as their own arguments
  (`stats`, `sections`, `total`, `total_label`, `nest`); its variables and
  grouping are the shared `summaries`, `by` and `id`. A categorical variable
  is stored as a count split by that variable (`simple` with `color`), the
  closest existing summary type to the planned `share`.
- The variables layout's settings are the former summary table widget,
  shown above the table; the table is drawn by the records engine.
- The variables layout's data output is the drilled Table 1 frame, as when a
  summary table block fed a table block (`drill = "source"` resolves it to
  source rows). The records and groups layouts output the drilled input.
- `new_summary_table_block()` forwards to `layout = "variables"` and left the
  registry.

R tests: all pass except `test-summarize-push` and `test-write-exhibit`,
which fail on `main` too. JS tests: 449 of 450, the same one fails on
`main`.

### Class migration (resolved)

Resolved by BristolMyersSquibb/blockr.core#379 (PR #384, on `main`): a
restored block only has to be a block. Needs a blockr.core build that
includes it; the version number was not bumped (still 0.1.4).

#### Earlier analysis


blockr.core restores a saved block only if the rebuilt block has exactly the
saved class vector (`blockr_deser.list`, `blockr.core/R/utils-serdes.R:295`).
A board saved with a summarize table (`summarize_table_block`,
`lane_chart_block`, `rank_block`) or an AE heatmap (`ae_heatmap_block`,
`heatmap_block`) therefore cannot come back as a `table_block`. Two ways:

- Keep the old class on blocks restored through the old constructors, for
  good. The forwarders stay, and every `table_block` method is registered
  for those classes too. No change outside blockr.viz.
- Let blockr.core accept a declared class migration (for example, a
  deserializer method marks its result as migrated and the class check then
  only requires a block). Old boards then save as `table_block` and the
  forwarders can go later. Needs a blockr.core change.
