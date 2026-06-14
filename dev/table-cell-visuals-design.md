# Table cell visuals: heatmap shading + data bars (design note)

Package: **blockr.viz** (renamed from blockr.viz). Captured 2026-06-14 from a design
session (Christoph). **Planned feature — design only, not implemented.** Scope it
AFTER the in-flight table-block / `.bi`→`.viz` cleanup lands, because it touches the
same files (`R/table-block.R`, `inst/js/table.js`, the gear popover). Companion docs:
`table-and-chart-architecture.md` (block roles) and the value-coloring code that
already exists in the table block.

---

## What exists today

The table block already does **numeric cell shading** (heatmap / correlation):

- `cell_color` spec via `drilldown_table_color(type, domain, palette)`.
- Domain inferred by **pooling all numeric columns** into one range
  (`table-block.R` ~`:99-111`), then a closure from `dt_color_fun()` (~`:431-471`)
  maps each value → `list(bg, fg)`, emitted as inline `style="background:…;color:…"`
  per `<td>` on the **vectorized single-`HTML()` fast path** (the ~170× perf work —
  do not regress this).
- Gear: `color_mode` segmented toggle (`off` / `diverging` / `sequential`) in
  `TABLE_ROLES` / `TABLE_SECTIONS`.

Pooled-domain is *correct* for a correlation matrix (one unit, one scale) but wrong
for the new cases below.

## What we want to add

A **data-bar** rendering: an in-cell horizontal bar proportional to the value, for
the "patients with most adverse events" style table — preferred over an ECharts bar
because the table is sortable / searchable. Same value→visual mapping as shading,
different output.

### 1. Bar is a fourth mode, on the same pipeline

Add `bar` to the existing mode toggle: `off / sequential / diverging / bar`. It
reuses the domain inference + per-cell numeric path. **Emit a CSS gradient string,
NOT DOM nodes:**

```
background: linear-gradient(90deg, <bar> 0 X%, transparent X%);   /* X = (v-lo)/(hi-lo)*100 */
```

This keeps the vectorized single-`HTML()` render. (The crossfilter block in
blockr.dm builds the bar from `track`+`fill` divs — fine for its small categorical
widget, but div-per-cell would reintroduce the renderTags walk the table perf work
removed. Borrow crossfilter's *decisions*, not its DOM.)

### 2. Negatives / baseline — follow crossfilter: NO center baseline

crossfilter (`inst/js/crossfilter-block.js` ~`:809,836`) deliberately normalizes on
**absolute magnitude** — single left-anchored fill, no mirrored center baseline:

```js
const maxVal = sorted.reduce((m, d) => Math.max(m, Math.abs(gv(d))), 0);
fill.style.width = `${(Math.abs(count) / maxVal) * 100}%`;
```

Do the same. Fixed fill color at reduced opacity (crossfilter uses
`--blockr-color-primary` @ 0.6). Skip center-baseline unless a real signed-value case
demands it.

### 3. Column scope — ONE select, empty = all numeric

This is the key interaction decision.

- The mode (bar/color) has **one column multi-select**.
- **Empty select = ALL numeric columns.** Its placeholder must read
  **"All numeric columns"** so it's discoverable that empty = everything (a blank
  control reads as "nothing", which is the wrong mental model).
- **Picking ≥1 column restricts to those.**

Correlation heatmap = `diverging`, leave the select empty → zero column-fiddling for
the common case. AE bar = `bar`, pick the one count column.

This collapses the earlier "separate scope toggle" idea into the select itself, and
matches blockr.dplyr's `[]` → `dplyr::everything()` convention (select_block).

### 4. Normalization falls out of scope — don't add a knob

- Empty (all numeric) + diverging/heatmap → **shared scale** (one pooled domain —
  what a correlation matrix needs; the current behavior).
- Picked columns + bar → **per-column scale** (each column its own max — what bars
  need; a count column and a percent column must not share a domain).

Derive this from mode + scope. Do **not** expose "per-column vs shared" as a setting.

### 4b. Center-at-0 for diverging — fix it, belongs HERE

This is a property of the **diverging color scale**, owned by the renderer — do NOT
push it into the `correlate` shape block (renderer must not know it's correlation;
the fix benefits any signed diverging measure: deltas, z-scores, log-fold-change).

Current `dt_color_fun()` (`:446`):

```r
mid <- if (lo < 0 && hi > 0) 0 else (lo + hi) / 2
```

centers at 0 **only if the pooled domain straddles 0**, and never symmetrizes the
domain. Both break a correlation matrix:

- all-positive corrs (e.g. 0.2–1.0 incl. the 1.0 diagonal) → `mid = 0.6`, white in
  the wrong place;
- asymmetric range (e.g. −0.3 to 1.0) → centers at 0 but the negative side never
  reaches full saturation, so ±0.3 don't read as equal-and-opposite.

**Make diverging intrinsically centered:** `mid = 0` (default) AND domain symmetric
around it (`[-m, +m]`, `m = max(abs(range))`; or fixed `[-1, 1]`). This is the
*definition* of diverging, **not a new knob** — keeps "normalization derives from
scope". Two small touches: symmetrize the domain when `type=="diverging"` in the
inference step (~`:99-111`), and default `mid = 0`. (Could expose an explicit
non-zero `center` in the spec later; default 0.)

**Two different "centers" — don't conflate:** the diverging *color midpoint* (yes,
= 0) is NOT the *bar baseline* (no — bars are left-anchored abs-magnitude, §2).
Sequential has no center (plain lo→hi ramp).

## Two build-time rules that are easy to get wrong

These are the difference between "self-healing under upstream data changes" and
"board frozen, redo everything":

1. **Empty stores a rule, not a snapshot.** When the select is empty, persist only
   "mode is on" — store NO column names. Resolve "all numeric" at render time against
   whatever data arrives. Then an upstream schema change (added/dropped/renamed
   numeric column) re-derives automatically, nothing to redo. Round-trips through the
   constructor as `scope="all_numeric"` (see the state-restore-via-ctor rule).

2. **Picked names must FAIL SOFT, never wedge.** A stored column name is the only
   brittle part, and it's opt-in. If the name goes stale upstream: skip that column
   (render it plain), keep the rest of the table working. It must NOT error and must
   NOT freeze the block — i.e. the color/columns field has to be in
   `allow_empty_state`, and a stale/empty value treated as "no shading". (blockr.core
   gotcha: clearing a field NOT in `allow_empty_state` silently wedges the block AND
   everything downstream — that would turn a rename into exactly the "redo everything"
   pain.)

## Deferred / promote-later

- An explicit **"select all" affordance inside** `Blockr.Select.multi()` is NOT
  needed for this — the empty-=-all default makes the common case skip the picker
  entirely. If we ever want a one-click "fill all" within picked mode, that belongs
  in the shared primitive (blockr.dplyr, loaded via `blockr_select_dep()`), promoted
  after proving the UX in blockr.viz. Note: the primitive currently exposes
  `getValue/setOptions/updateOptions` but no obvious selection setter — confirm
  before relying on programmatic select-all.

## Final gear shape

```
mode:    off | sequential | diverging | bar
columns: <multi-select, placeholder "All numeric columns">   (empty = all numeric)
```

One control set does heatmap shading, sequential shading, and data bars.
