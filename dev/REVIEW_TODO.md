# blockr.bi review todo

Captured 2026-05-07 from a Claude Code review pass. Not for today — pick up after
the presentation. Items are roughly ordered by impact / how cheap they are to fix.

## Concrete bugs

- [ ] **`gt_table_arguments()` is out of sync with the constructor.**
  Advertises `indent_stat` (doesn't exist on `new_gt_table_block()`) and omits
  `na_rep` (which does exist). MCP server will mislead the AI.
  - `R/block-arguments.R:117-147` vs `R/gt-table-block.R:363-368`
- [ ] **Dead parameter `stub_is_sortable <- FALSE`** in `R/html-table-block.R:64`.
- [ ] **`n_distinct` missing `na.rm = TRUE`** at `R/tile-block-expr.R:187` —
  inconsistent with neighbouring stats, will silently mis-count under NAs.
- [ ] **Unchecked column access** in `R/waterfall-block.R:184-185` and
  `R/drilldown-chart-block.R:117-128`: `data[[measures[i]]]` returns NA silently
  if an upstream block renames the column. Validate in `dat_valid()` instead.

## Migration / deprecation story

- [ ] **`lifecycle` is in Imports but unused.** Pick a path:
  - mark `kpi_block` deprecated with `lifecycle::deprecate_soft()` (registry
    already calls `tile_block` its successor), OR
  - drop "successor" language from the registry descriptions.
- [ ] **`gt_table()` legacy long-format branch** (`R/gt-table-block.R:195-337`):
  the "deprecation window" comment has no timeline and no test. Either commit to
  the deprecation or test/document the path.
- [ ] **Three filter blocks** (`bi_filter`, `visual_filter`, `drilldown_chart`)
  with no decision matrix. README only documents `visual_filter`. Either retire
  one or write a "when to use which" guide.
- [ ] **Two demo datasets** (`bi_demo_data`, `tile_demo_data`) without docs
  explaining which block consumes which.

## Shape contract for table-shapers ↔ table-renderers

- [ ] Replace the column-name sniff (`R/gt-table-block.R:47-48`,
  `R/html-table-block.R:50`) with an explicit attribute or `.blockr_table_format`
  S3 class that `pivot_table` / `summary_table` tag and renderers validate.

## Heavy files / copy-paste

- [ ] **`html-table-block.R` (923 lines)** inlines a ~620-line JS template + 175
  lines of CSS as R strings. Move to `inst/js/` and `inst/css/`.
- [ ] **`summary-table.R` (893)** — `summary_table()` (L95-273) and
  `compute_hierarchy_run()` (L788-890) duplicate stat logic; share helpers.
- [ ] **`kpi-block.R` (609)** and **`waterfall-block.R` (397)** share ~100 lines
  of state-mgmt boilerplate. Consider a `new_aggregation_block_base()` factory.
- [ ] **`visual-filter-block.R` (643)** mixes echarts config, dimension
  detection, and UI; split the echarts piece into a helper module.

## Shiny / JS coupling

- [ ] **Custom-message handlers aren't namespaced by instance.**
  `drilldown-chart-block.R` registers a global handler — second instance
  overwrites the first. Filter-block does this correctly via `ns()` in the
  payload; use it as the template.
- [ ] **Theme registry is process-global** (`R/drilldown-theme.R:5-6`).
  Collisions if two boards register different themes under the same name, or
  if `blockr.echarts` and `blockr.bi` both load.
- [ ] **State re-init on rebind**: `kpi`, `waterfall`, `drilldown_chart`
  overwrite restored state with constructor defaults when re-bound in a DAG.
- [ ] A few `observeEvent`s lack `bindEvent` / `ignoreInit` — minor, but
  inconsistent with the rest of the codebase.

## Tests

- [ ] No snapshot tests for `gt_table` / `html_table` output — add some.
- [ ] No end-to-end "click bar → downstream filter applies" test.
- [ ] `dat_valid()` error paths aren't exercised anywhere.
- [ ] Two `drilldown-chart` tests `skip_if_not_installed("blockr.insurance")` —
  the click-to-filter path is effectively untested in CI. Move to a tagged
  integration suite or stub the dep.

## Suggested order when you come back to it

1. `gt_table_arguments` fix (real MCP-visible bug, smallest fix).
2. Lifecycle decision (deprecate kpi + legacy gt path, or drop the language).
3. Decide the three-filter-blocks story; update README.
4. Move html-table JS/CSS out to `inst/`.
5. Shape-contract attribute + renderer-side validator.
6. Then the snapshot + end-to-end tests.
