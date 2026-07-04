# Unified argument naming: chart, tile, table

Target vocabulary for the three drilldown display blocks (`new_chart_block`,
`new_tile_block`, `new_table_block`) so the same concept carries the same
argument name everywhere, and aligned with `blockr.dplyr::new_summarize_block`.

## Naming rule

- A **role** is singular, even when its type binds several columns. Precedent:
  the table's `group` is already an array (`["SEX","ARM"]`) yet named `group`.
  So `value`, `group`, `color`, `x`, `y` stay singular regardless of arity.
- **Plural only for a genuine list of objects.** `summaries` is a list of
  `{func, cols}` specs, so it earns the plural. It is the only plural in the
  surface.

## The two data concepts (kept apart)

1. **`value`** — raw column(s) shown as-is, no computation. The tile's big
   number, the table body, and a boxplot's measured variable. A boxplot has no
   aggregation, so its value lives here, not in `summaries`.
2. **`summaries`** — computed aggregations, a list of `{func, cols}`. Bar
   heights, table aggregate rows, tile aggregate cards.

## Final target table

| Concept | Chart | Tile | Table | dplyr |
|---|---|---|---|---|
| Group / cluster by | `group` | `group` | `group` | `by` (leave) |
| Computed summaries `[{func, cols}]` | n/a (removed — see below) | **`summaries`** | **`summaries`** | `summaries` |
| Aggregation function | **`func`** (was `agg_fn`) | **`func`** (entry) | **`func`** (entry) | `func` |
| The value column | **`value`** (was `metric`) | **`value`** | **`value`** (was `values`) | n/a |
| Position axes | `x` / `y` / `xend` | n/a | n/a | n/a |
| Aesthetics | `color` / `facet` / `series` / `label` | n/a | n/a | n/a |
| Label / name column | `label` (on-mark) | `name` (was `measure`) | `rowname` (kept: the row-label stub) | n/a |
| Drill | `drill` | `drill` | `drill` | n/a |

## Renames (old -> new)

| Block | Old | New | In use (deprecate)? |
|---|---|---|---|
| chart | `metric` | `value` | yes (`#9`) -> **alias** |
| chart | `agg_fn` (top-level) | `func` | yes (`#9`) -> **alias** |
| chart | `metrics` | `summaries` | no (new today) -> clean |
| chart | entry field `agg_fn` | `func` | no (new) -> clean |
| tile | `metrics` | `summaries` | no (new) -> clean |
| tile | entry field `agg_fn` | `func` | no (new) -> clean |
| table | `metrics` | `summaries` | no (new) -> clean |
| table | entry field `agg_fn` | `func` | no (new) -> clean |
| table | `values` | `value` | done — clean rename, no deprecation (unused block); done surgically to avoid the `filter_values` / `msg$values` collision |

Entry shape: `{agg_fn, cols}` -> `{func, cols}` (`cols` unchanged).

Not renamed this pass (only the *fold* is deferred): the chart keeps the flat
`value` + `func` pair AND the `summaries` list, exactly as it kept `metric` +
`agg_fn` alongside `metrics`. Collapsing the flat pair into `summaries` is the
separate deferred step.

## Scope

**This pass (mechanical, coherent):**
- `metrics` -> `summaries`, `agg_fn` -> `func`, table `values` -> `value`.
- Constructor args, saved-state keys, JS object fields, JSON edges, AI argument
  descriptions.
- Back-compat shim in each constructor: still accept the old keys and map them,
  so saved boards restore.

**Deferred (needs its own design / confirmation):**
- Fold the chart's flat `metric` + `func` into `summaries` (a plain count bar
  becomes `summaries=[{func:"count", cols:[]}]`; the gear needs a friendly
  single-row default first).
- tile `measure` -> `name` (DONE: `measure` collides with `metric`/`value`; its
  gear label is already "Name"). Only the arg + config/role/state surface was
  renamed; the internal `cells$measure` dimension and the `tk_normalize` /
  `tile_html` `measure` params (which build it) were left as-is -- "measure" =
  a KPI is legitimate builder vocabulary, not user-facing.
- table `rowname` -> KEPT. It is the row-label stub column, a genuinely
  different role from tile's KPI-name column, so it does not fold into `name`.
- Align dplyr's `by` with `group` (left as a known dplyr-ism).
- Align the dplyr entry shape further (`type`/`name`/`expr` variants).

## Implementation plan

Order matters: rename the internal canonical first, keep every reader tolerant
of both spellings, flip writers to the new spelling, then update the docs/AI
surface. At no point is the tree left broken, because readers accept both.

1. **Shared JS vocabulary** (`inst/js/drilldown-agg.js`): the role key becomes
   `func` (was `agg_fn`); `reconcileMetric` and helpers read `cfg.func`. This is
   the source of truth the chart/table/tile JS all import.
2. **JS consumers** (`chart.js`, `table.js`, `tile-block.js`,
   `drilldown-config.js`): read a summary entry's function as
   `m.func ?? m.agg_fn`; read the list as `cfg.summaries ?? cfg.metrics`; emit
   only the new names. Rename the top-level chart `agg_fn` reads to `func`.
3. **R constructors + state** (`chart-block.R`, `table-block.R`,
   `tile-block.R`): add `summaries`/`func`/`value` formals; keep `metrics`/
   `agg_fn`/`values` as deprecated formals that map onto them (see Deprecation).
   State keys, `dd_parse_metrics`, `dd_metrics_json`, `block_ctor_inputs` all
   speak the new names; parsers still read the old.
4. **AI argument surface** (`chart-arguments.R`, `table-block.R` /
   `tile-block.R` arg descriptions, `block-arguments.R`): rename the documented
   args, add a one-line "(was `metrics`)" note so the model tolerates old boards.
5. **Tests + man/**: update fixtures and roxygen; regenerate NAMESPACE/Rd.

## Deprecation plan

Only two args are established and renamed: the **chart's `metric` and
`agg_fn`** (both from `#9`, in real boards). Everything else here is either
brand-new (today's `metrics`/`summaries`, entry `func`) or in an unused block
(the drilldown table's `values`), so it renames with no compat.

- **Chart only: accept-old, canonicalize to new.** `new_chart_block` keeps
  `metric` and `agg_fn` as deprecated formals; when supplied they map onto
  `value` / `func` and emit one deprecation warning. The block's saved state,
  server reactives, and the config it ships to JS all speak `value` / `func`,
  so the JS never sees the old names (no JS-side fallback needed).
- **Removal window.** The two chart aliases stay for one minor release cycle,
  then are deleted. Removal = delete every site tagged below.
- **Grep marker.** Every back-compat site is tagged with the literal token
  `ARG-RENAME` in a comment that links here. To find all cleanup sites:
  `grep -rn "ARG-RENAME" blockr.viz/R blockr.viz/inst`.

## Cleanup checklist (remove at end of deprecation window)

Each item is a site tagged `ARG-RENAME`; deleting all of them completes the
rename. Keep this list in sync as sites are added.

- [ ] `chart-block.R` (single `ARG-RENAME` tag) — the `.dep <- list(...)` block
      that maps the deprecated `metric`/`agg_fn` onto `value`/`func`. This is the
      ONLY compat site: `grep -rn "ARG-RENAME" R/ inst/` returns just this one.

## Status (implemented)

Done and green (402 tests pass, 0 fail): `metric`->`value`, `agg_fn`->`func`,
`metrics`->`summaries` across chart / table / tile (R args + state + server
contract + JS config keys + `data-dt-*`/`data-tk-*` attrs + AI arg descriptions
+ shared `drilldown-agg.js`/`drilldown-config.js`). Chart `metric`/`agg_fn`
accepted as deprecated aliases (warn once, map to new). Boxplot color-split
re-verified through the renamed keys.

Also done: table `values`->`value` (clean, no deprecation, done surgically
around the `filter_values`/`msg$values` collision).

Also done: tile `measure`->`name` (clean, no deprecation; config/role/state
surface only, internal `cells$measure` kept). table `rowname` kept as-is (a
distinct role, the row-label stub).

Also done: `summaries` REMOVED from the chart. A chart aggregates a single
`value` + `func`; showing several measures is a table/tile idea (many columns /
cards) and on a chart is expressed via `color`/`series` from the data shape
(pivot longer upstream, like the line-chart rule). So the chart no longer
exposes `summaries` -- the constructor arg, AI arg, gear repeatable-list
control, R<->JS round-trip, and state key are all gone. The chart always renders
through the single-value path (`_aggregate`, reads `value`/`func`). The dormant
multi-series JS (`_metricPlan` / `_aggregateMulti` / `_normalizeMetrics` /
`_metricWords`, now never triggered because `summaries` is always empty) is left
in place; ripping it out is a safe follow-up best done with browser
verification. `summaries` stays on table and tile.

Deferred: none (the earlier "fold flat value+func into summaries" is moot --
we went the other way and removed chart summaries). Internal identifier
names left as-is where not part
of the serialized contract (`metricsList`, `_metricPlan`, `_reconcileMetric`
method, `metrics_set`/`aggMetrics` markers) — cosmetic, safe to tidy later.
