# Argument naming: chart, tile, table

Shared aggregation vocabulary for `new_chart_block` / `new_tile_block` /
`new_table_block`, aligned with `blockr.dplyr::new_summarize_block`.

**Rule:** role names are singular even when they take several columns
(`value`, `group`, `color`, `name`). Plural only for a list of objects —
`summaries` (a list of `{func, cols}`) is the only one.

**Two concepts:** `value` = a raw column shown as-is (tile number, table body,
boxplot variable). `summaries` = computed aggregations, each `{func, cols}`
(`func` ∈ count / count_distinct / mean / median / sum / min / max).

| Concept | Chart | Tile | Table |
|---|---|---|---|
| Group by | `group` | `group` | `group` |
| Raw value column | `value` | `value` | `value` |
| Aggregation fn | `func` | `func` (in `summaries`) | `func` (in `summaries`) |
| Aggregations | — | `summaries` | `summaries` |
| Axes / aesthetics | `x`/`y`/`xend`, `color`/`facet`/`series`/`label` | — | — |
| Name column | `label` (on-mark) | `name` | `rowname` (row stub) |
| Drill | `drill` | `drill` | `drill` |

- A chart takes no `summaries` — it aggregates one `value` + `func`; several
  measures come from `color`/`series` on reshaped data.
- tile `name` (KPI-name column) and table `rowname` (row stub) are distinct roles.
- Only chart `metric`/`agg_fn` remain as deprecated aliases (→ `value`/`func`,
  warn once). One back-compat site: `grep -rn "ARG-RENAME" R/ inst/`. Remove
  after one release.
