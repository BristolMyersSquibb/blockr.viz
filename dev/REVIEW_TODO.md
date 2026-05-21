# blockr.bi review todo

Captured 2026-05-07 from a Claude Code review pass. Not for today — pick up after
the presentation. Items are roughly ordered by impact / how cheap they are to fix.

## Cleanup pass — 2026-05-21

Walk-through during the pharma MCP demo prep surfaced these items. Several
overlap with sections below; treat this as the decision queue.

- [ ] **`pivot_table_block` — deprecate or delete.** No production caller
  (only `dev/pivot_table_demo.R`, `dev/bi_dashboard_demo.R`, and its own
  tests). Today: dropped from the MCP `block_universe` (so the AI no longer
  recommends it) and fixed its broken headless-eval `state` contract so
  direct R callers still work. Next step: either add `lifecycle::deprecate_soft()`
  to `new_pivot_table_block()` and keep it as a low-priority fallback, or
  delete the source + tests + JS + dev demos. `summary_table_block` already
  covers the "X by Y × Z" pattern as `vars=[X], by=[Y]` in real dashboards.
- [ ] **`visual_filter_block` already removed locally.** `git status` shows
  `R/visual-filter-block.R`, `man/new_visual_filter_block.Rd`, and
  `tests/testthat/test-visual-filter-block.R` deleted; `dev/visual_filter_demo.R`
  gone too. Commit the removal as a standalone change with a short rationale
  so the deletion is auditable.
- [ ] **kpi_block ↔ tile_block — finish the migration.** `tile_block` is
  registered as "successor to kpi_block" (`R/registry.R:36`) but `kpi_block`
  is still active and still exposed in the MCP universe. Decide: do we drop
  `kpi_block` now (and migrate any inst/examples) or keep both during a
  formal `lifecycle::deprecate_soft()` window? Related: existing item in
  *Migration / deprecation story* below.
- [ ] **`html_table_block` vs `gt_table_block` — pick one default.** Both
  render the same wide-tibble contract. `gt_table_block` is the pharma SAP
  default (publication-quality static tables); `html_table_block` is the
  dashboard-native renderer (collapsible sections, sticky headers). Either
  document the split clearly (when to use which) or fold one into the other.
  Today the MCP universe only recommends gt; html_table is effectively
  dormant from the AI side.
- [ ] **`bi_demo_data` is stale.** Predates the wide-tibble contract;
  several demo scripts still seed it. Either regenerate it against the
  current `summary_table` / `pivot_table` output shape, or replace usages
  with `tile_demo_data` / `safetyData::adam_*` and delete `bi_demo_data`.
  Overlaps with the existing *Two demo datasets* item below.
- [ ] **Rename blockr.bi → blockr.dashboard?** Open question. "bi" reads as
  business intelligence, but the actual scope is dashboard primitives (KPI
  tiles, drilldown charts/tables, wide-table renderers). Worth deciding
  before the package is widely advertised. If yes: blast radius is the
  package name, NAMESPACE, all dev demos, MCP `block_universe` `package`
  fields, blockr.docs cross-references, install instructions, and any apps
  that `library(blockr.bi)`.

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
- [ ] **Filter story** is `bi_filter` (defunct stub pointing to
  `blockr.dm::new_value_filter_block`) + `drilldown_chart`. Document when to use
  which (or fold into a single guide).
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

## Shiny / JS coupling

- [ ] **Custom-message handlers aren't namespaced by instance.**
  `drilldown-chart-block.R` registers a global handler — second instance
  overwrites the first. Use `ns()` in the payload so each instance only reacts
  to its own messages.
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
