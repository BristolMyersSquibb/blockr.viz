# Table & chart blocks: the shaper / renderer architecture

Captured 2026-06-13 from a design session. This is the **reference model** for the
table/chart block cleanup; the action items at the bottom feed
[`REVIEW_TODO.md`](REVIEW_TODO.md). Companion AI-tuning notes live in
`blockr.ai/dev/harness-prompting-lessons.md`.

The motivating realization: blockr.bi had grown several overlapping "table" blocks
(`pivot_table`, `summary_table`, `gt_table`, `html_table`, `drilldown_table`) and a
parallel chart/tile/kpi set, with names that described *features* rather than
*roles*. This note defines the roles so every block has one obvious place.

---

## TL;DR — the five layers

Every block in a dashboard pipeline is one of five kinds. Data flows left to right;
**a renderer only ever sees a tidy/rectangular data frame.**

| Layer | Job | Output | Blocks |
|---|---|---|---|
| **Shape** | compute / summarise | **tidy df** | `describe`, `correlate`, `frequencies`, `survival`, `padjust`, + dplyr verbs (`summarize`, `filter`, …) |
| **Reshape** | tidy → display grid (wide, *on purpose*) | wide df | dplyr `pivot_wider` |
| **Fit** | fit a statistical model | **model object** | `model` |
| **Adapt** | model → tidy | tidy df | `broom` (tidy / glance / augment) |
| **Render** | df → pixels | UI | `table`, `chart`, `tile`, `gt`, `ggplot` |

Two invariants make the system predictable:

1. **Verbs compute (tidy out); nouns render.** If a block's name is a verb it
   returns a tidy data frame you can keep transforming. If it's a bare noun it draws.
2. **Renderers only see tidy/rectangular data.** The *only* non-df intermediate in
   the whole system is a fitted model, and it is fenced off — it exists solely
   between `model` and `broom`. A coefficient table is `model → broom → table`; no
   renderer ever touches a fit.

---

## Render layer — the noun blocks

### Interactive | static pairs
Each output medium has an **interactive** renderer (dashboard-native) and a
**static** renderer (print / CSR / report):

| Medium | Interactive | Static |
|---|---|---|
| table | **`table`** | `gt` |
| chart | **`chart`** (ECharts) | `ggplot` (blockr.ggplot) |
| number/spark/ring | **`tile`** | — |

Pick by destination: dashboards use `table` / `chart` / `tile`; print/CSR uses `gt`
/ `ggplot`. The static renderers conceptually belong with their toolkit packages
(`blockr.gt`, `blockr.ggplot`), not in blockr.bi — blockr.bi owns the *interactive*
renderers.

### Drill-down is a feature, not an identity
`table` and `chart` were `drilldown_table` / `drilldown_chart`. But `drill=` is one
opt-in capability (click-to-filter, **off by default**) — naming the whole block
after it overweighted a feature. These are simply *the* table and chart blocks; you
reach for them for almost every table/chart in a dashboard. **Renamed 2026-06-13**
(see "Done" below). The drill *feature* keeps its `drilldown-*` internals
(`drilldown-config.js`, `drilldown-theme.R`, `filter_*` transport) — only the block
identity changed.

### `table` is the universal interactive renderer
`html_table` and `table` are both interactive HTML tables with disjoint features
(`html_table` understands section/spanner structure; `table` has color + drill). The
plan: **fold `html_table`'s section/spanner/collapse rendering into `table`** so one
renderer handles flat *and* structured input, then retire `html_table`. ("drilldown
= universal renderer", deepen a few renderers over adding specialized ones.)

### `tile` supersedes `kpi`
`tile`'s `showcase = "number"` covers everything `kpi` does (big numbers), plus
`"spark"` and `"progress"`. `kpi` is **deprecated** (done — see below). Note their
APIs differ (`kpi`: flat `measures/agg_fun/…`; `tile`: `showcase` + `state`), so
`kpi` *cannot* forward to `tile` — existing kpi blocks keep rendering as kpi via the
kept constructor; new work uses tile.

### Deprecation policy: **unregister, don't tag**
When a block is deprecated it is **removed from `register_bi_blocks()` and the MCP
universe** (so the picker and the AI stop offering it), but its **constructor stays
exported** with a `lifecycle::deprecate_soft()` nudge. blockr.core deserializes a
saved block by **constructor name** (`get0()`), not via the registry, so existing
boards keep loading. No "[Deprecated]-but-still-in-the-picker" half-measure.

### `waterfall` — a **baseline mode on the bar chart**, not a new chart type
`waterfall` today is a **specialized ECharts chart mislabeled as a transform** — it
aggregates the chosen `measures`, computes the cumulative bridge, and renders via a
*separate* echarts4r htmlwidget (the universal `chart` instead ships data as JSON to
`inst/js/drilldown-chart.js`). It is to be folded into `chart`, but the right framing
is **not** "another `chart_type`":

**A waterfall is a bar chart.** Same contract (`group` + `metric` + `agg_fn`), same
cartesian axis, same factor-based ordering, same drill. The *only* difference is the
bars' **baseline**: a plain bar starts at 0; a waterfall bar starts where the previous
one ended (the running cumulative). So it is a **mode of bar**, not a sibling of it:

```
 BAR  baseline = 0            WATERFALL  baseline = running cumulative
                                     ┌────┐
       ┌────┐                        │+500│╌╌┐
  ┌────┤    │ ┌────┐                 │Rev │  │╌╌┐ ┌────┐
  │Rev │Cost│ │Prof│                 │    │  │Cost│ │300 │
  0────┴────┴─┴────┴─            0───┴────┴──┴────┴─┴────┴─
  (every bar starts at 0)        (each bar starts where the last ended)
```

Litmus test for "new `chart_type` vs option on an existing one": **does it change the
coordinate system?** Radar earned its own type (polar spokes). Waterfall is the *same*
cartesian bars with a shifted baseline → it's a **bar option**.

Design:
- **Model**: `bar` + `baseline = "zero" | "cumulative"` (+ sign-coloring: green up /
  red down — cosmetic). More general than P&L: cumulative bars are reusable.
- **Totals/subtotals** = a **per-bar baseline override** (a `type` column with
  `"relative"` / `"total"`; a `total` bar resets its baseline to 0). This is ECharts'
  native model and rides through `pivot_longer` as just another column — the one bit
  of info beyond `group + metric`.
- **Data contract** = long: `group` = step axis, `metric` = value, exactly like every
  other aggregated chart. Wide P&L data (`Revenue|Costs|Profit` as columns) is
  `pivot_longer`'d to `(step, value)` first — a composable reshape, consistent with
  "reshape upstream". Step **order** is the step column's factor levels (standard
  categorical ordering, not waterfall-special); the bar renderer must honor data order
  for this mode (don't value-sort).
- **Implementation**: add the `baseline` mode to the existing **bar** branch in
  `drilldown-chart.js` (compute running cumulative, honor per-bar total reset,
  sign-color). No new data contract, no new axis logic, no second rendering stack —
  much smaller than a bespoke waterfall renderer. In the UI, "Waterfall" can still be a
  named `chart_type` choice, but it's *sugar* for `bar + baseline="cumulative"`.

Keep `new_waterfall_block` registered until the bar baseline-mode lands (no replacement
before then); then deprecate it like the others (unregister, keep constructor).

### Renderer responsibilities (and the boundary)
A renderer **draws** and **formats**; it does **not** compute or reshape.
- ✅ draw structure (sections / spanners / indents), format numbers (decimals),
  color, drill, sort/search.
- ❌ aggregate, reshape, run statistics.

The one bounded exception worth naming: a chart self-aggregates because the visual
encoding *is* the aggregation (a bar = sum-of-group). That asymmetry is correct —
charts take raw data + an aesthetic mapping; tables take an already-shaped table.

The pharma `"13 (18.8%)"` cell (n + pct) is the lone thing on the compute/draw
boundary; it is resolved by the **`.fmt` convention** below — the renderer formats,
the shaper stays tidy numbers.

### The `.fmt` convention (cell formatting, resolved)
Cell formatting rides in a **hidden dotted column `.fmt`**, exactly like `.indent` /
`.label` / `.section_*` — the numbers stay in plain numeric columns (`dplyr`-able),
and `.fmt` is a per-row **template that names the columns to combine**:

```
 .fmt = "{estimate} ({std.error})"      -> "0.05 (0.01)"
 .fmt = "{n} ({pct}%)"                  -> "38 (54%)"
 .fmt = "{mean} ({sd})"                 -> "65.2 (8.1)"
 .fmt = "{median} ({q1}, {q3})"         -> "12 (8, 19)"
 .fmt = "{or} ({conf.low}–{conf.high})" -> "1.8 (1.2–2.7)"
```

The renderer is **pure interpolation**: glue the named columns into the literal
template — no codebook of named formats, no domain knowledge, no per-stat branching.

Three properties make this the resolution of the n(%) question:
1. **Defaults first.** A renderer column-default formats a raw tidy frame (e.g. a
   `broom` table) with **zero** annotation: `p.value`→3 dp/stars, `estimate`→2 dp,
   etc. `.fmt` is the **override**, needed only when formatting varies **per row** (a
   mixed Table 1: some rows `n (%)`, some `mean (sd)`).
2. **Rounding stays out of `.fmt`** — column defaults or a `digits` hint own it. Keep
   `.fmt` to "names + punctuation"; don't grow a `sprintf` mini-language.
3. **Format-then-spread** for grouped tables. The cell's numbers are on the same row,
   so combine **per row first**, then a *plain* `pivot_wider(names_from = group,
   values_from = <formatted cell>)`. Order matters: format before spreading (spreading
   raw `n`/`pct` first forces messy column-pairing). Both steps are render-time, so the
   shaper's output stays the tidy numbers.

**One structure for every table.** Table 1, regression (`broom`), survival, and
frequency tables are the *same* display: rows (`.section`/`.label`/`.indent`) ×
optional spread dimension (arm / model / subgroup) × composite cells (`.fmt`). One
`table` renderer + this convention covers all of them, fed by tidy shapers. Because
the numbers stay tidy, the *same* frame renders as combined cells **or** as separate
columns — a render choice, not baked in.

---

## Shape layer — the verb blocks

### Tidy by default
Shapers **always emit tidy data frames** — no display strings baked in. This is the
property that makes every shaper's output `dplyr`-able and re-renderable. Today
`summary_table` violates this: it `sprintf`s every cell to `"13 (18.8%)"` /
`"mean (sd)"` and **discards the raw numbers it already computed** (`stats_df` carries
`n`, `pct`, `mean`, `sd`, … and throws them away). The display formatting moves to
the renderer; the shaper emits the numbers. No `format` toggle — tidy is the only mode.

### The "earns a block" test
A computation earns its *own* block only if it is **bounded** (a finite, UI-able
config) **and** has **no clean primitive path** (no single dplyr/tidyr verb). Otherwise
compose existing blocks, or use `function_block` (the escape hatch for arbitrary,
unbounded transforms — *not* a substitute for a real block when the config is bounded).

| Candidate | Clean primitive path? | Verdict |
|---|---|---|
| `correlate` | no — `cor()` returns a matrix, no verb | **own block** |
| `describe` (mixed numeric+categorical+logical, by group) | no — no single verb does mixed-type describe | **own block** |
| `pivot_table` | **yes** — `summarize` → `pivot_wider` | **demote/drop** (pure convenience macro) |
| correlation heatmap | = `correlate` → `table(cell_color)` | compose, no new block |

### Per-block decisions
- **`describe`** — the rich descriptive shaper (numeric stats + categorical n/pct +
  logical flags, by group, **tidy out**). This is `summary_table`'s compute core,
  separated from its formatting. Keep `summary_table` as the name unless we go to a
  verb (`describe`); either way it stops pre-formatting and emits tidy.
- **`descriptives`** (blockr.stats) — **retire.** Numeric-only, ungrouped, tidy; its
  own author notes it ≈ `dplyr::summarise(across(where(is.numeric), …))`. Fully
  subsumed by the rich `describe`. Stats users use `describe`/`summary_table`.
- **`frequencies`** (blockr.stats) — fold into `describe` later (it already does
  categorical counts); or keep as the dedicated 1-/2-way count shaper. Low priority.
- **`pivot_table`** (blockr.bi) — **demote/drop.** = `summarize` + `pivot_wider`,
  both clean dplyr verbs; adds no unique capability, only an Excel-pivot UX.
  `REVIEW_TODO.md` already flags it as having no production caller. A crosstab is
  `summarize → pivot_wider → table`; the AI can compose that from one prompt.
- **`correlate`** — **new block** (the one genuine gap). df → tidy correlation matrix
  (`var` column + numeric columns; `method` arg). Rendered by `table(cell_color)` for
  the heatmap. Replaces the inline `transform="correlation"` currently in `table`
  (drop `transform`/`cor_method` from the renderer → keep it pure). Home: likely
  blockr.stats. NB: `new_correlation_matrix_block` is referenced in demos but **never
  defined** — remove those dead references.

### Fit / adapt (the modeling excursion)
`model` returns a **fitted model object**, not a df — verified against the source
(its roxygen: "broom adapter … turns it into tidy frames"). `broom` is the bridge
back to tidy df-land. This is the sanctioned out-of-df-and-back detour; `model` is a
*fitter*, not a shaper, and keeps its name. (`survival` already returns tidy;
`padjust` is df→df; `stat_test` / `effect_size` return types unverified — check
before classifying.)

---

## Naming system

- **verb → compute/shape, tidy out**: `describe`, `correlate`, `frequencies`,
  `model` (fit), `test`, …
- **bare noun → render**: `chart`, `table`, `tile`, `gt`, `ggplot`.
- **reshape** is just dplyr `pivot_wider` — no bespoke bi block.

So a pipeline reads as roles: `describe → table` ("describe these vars, show as a
table"), `correlate → table` (heatmap), `model → broom → table` (coefficients),
`summarize → pivot_wider → chart` (crosstab plotted).

### Renderer arg consistency (chart vs table)
- **Inherently different** (and correct): chart maps *aesthetics* (`x`, `y`, `group`,
  `series`, `facet`, `label`); table maps *roles* (`rowname`, `values`, `cell_color`).
  You can't put `x`/`y` on a table.
- **Shared cross-cutting concepts share names** (good, already true): `drill`,
  `digits`, the `filter_*` runtime transport.
- **`color` (chart) vs `cell_color` (table)** — deliberately distinct: chart `color`
  is a *categorical* series→hue mapping; table `cell_color` is a *numeric*
  value→background scale. Different operations, so different names — document the why
  so it doesn't read as an inconsistency.
- Internal renderer fn uses `label_col`/`value_cols`; the block uses `rowname`/
  `values`. **Unify on `rowname`/`values`** across both layers.

---

## Done (in the working tree, 2026-06-13, uncommitted)

- **Renderer rename**: `new_drilldown_chart_block → new_chart_block` (class
  `chart_block`, "Chart"); `new_drilldown_table_block → new_table_block` (class
  `table_block`, "Table"); `*_arguments()` and `config_effect.*` methods follow the
  class. Deprecated forwarding aliases keep old serialized boards loading (blockr.core
  resolves a saved block by **constructor name** via `get0()`, verified against
  `blockr_deser.block`). blockr.mcp `block-universe` keys updated.
- **blockr.input** `new_table_block` (CRUD) → `new_table_crud_block` to free the name.
- **`summary_table` AI args** tuned to 5/5 (all 9 `state` fields exposed,
  sections-nesting + `id_var` gating). See `blockr.ai/dev/summary-table-eval-live.R`.

---

## Open decisions

1. `gt` — keep a thin convention-aware adapter in bi, or move gt rendering entirely
   to `blockr.gt`? (Its only live consumer was `blockr.csr`, which is unused — see
   below.)
2. `summary_table` name — keep, or rename to the verb `describe`?
3. Verify `stat_test` / `effect_size` return types before placing them in the layers.

*(Resolved: `n (%)` cell formatting → the `.fmt` hidden-column convention above.)*

## Migration / TODO

- [ ] Make `summary_table` emit tidy numbers (stop discarding `stats_df`); move
      `n (%)` / `mean (sd)` formatting to the renderer.
- [ ] Add `correlate` block (df → tidy matrix); drop `transform`/`cor_method` from
      `table`; remove dead `new_correlation_matrix_block` references.
- [ ] Retire `descriptives` (blockr.stats); point users at `describe`/`summary_table`.
- [ ] Fold `html_table` section/spanner rendering into `table`; retire `html_table`.
- [x] Deprecate `kpi` — unregistered + `lifecycle::deprecate_soft → new_tile_block`,
      removed from MCP universe, constructor kept. *(2026-06-13)* Still TODO: migrate
      live boards (unibas, insurance, deploy apps) from kpi to tile.
- [x] Deprecate `pivot_table` — unregistered + `lifecycle::deprecate_soft`, constructor
      kept (already out of MCP universe). *(2026-06-13)*
- [ ] Fold `waterfall` into `chart` as a **bar baseline mode** (`baseline =
      "cumulative"` + per-bar `total` override + sign colors) in `drilldown-chart.js`,
      *not* a new chart_type. "Waterfall" stays a named UI choice = sugar for
      `bar + baseline="cumulative"`. Keep `new_waterfall_block` registered until then.
- [ ] Move/clarify `gt` ownership (blockr.gt vs bi adapter).
- [ ] Unify `rowname`/`values` naming across block + internal renderer.

## Related

`blockr.csr` (the unused CSR example dashboard) was the last live consumer of
`gt_table` + `html_table`; it's an archive candidate, which is what unblocks the
renderer convergence above. See the workspace memory note `project_blockr_csr_unused`.
