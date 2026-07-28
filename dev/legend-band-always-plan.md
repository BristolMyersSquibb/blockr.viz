# Plan: make the HTML legend band the ONLY legend

Status: **APPLIED 2026-07-28** on branch `feat/legend-band-always` (worktree
`.claude/worktrees/legend-band`), rebased onto `3abd126`. Net −205 lines in
`inst/js/chart.js`. Kept as the record of why, plus the four places reality
differed from the plan (see "What changed on contact" at the bottom).

Verified: `dev/verify-legend-band.R` (5 families x faceted/unfaceted) driven by
`dev/probe-legend-band.R`, `probe-legend-resize.R`, `probe-legend-export.R`;
R suite 3043 pass / 0 fail.

Every edit below is anchored on a **unique code string**, not a line number.

---

## Decision

Today the HTML band (`.dd-legend-band`, `_updateLegendBand()`) draws only when
there is more than one facet panel. All three render paths carry the same rule:

```js
const sharedLegend = !singleFacet;
```

in `_renderAggregated`, `_renderIndividual` and `_renderTimeline`. One panel
gets the native ECharts legend drawn inside the canvas at `bottom: 0`.

**Make the band unconditional.** The native legend is worse on its merits (it
is canvas pixels: not selectable, not themeable by CSS, not focusable) and it
drags a large amount of layout machinery behind it that exists *only* because
a canvas cannot reflow:

- `_legendRows()` — predicts chip wrap by measuring text on a scratch canvas
  against ECharts' internal horizontal-legend constants (25px chip + 5px gap +
  10px itemGap + a deliberate 2px per-item slack bias).
- `__legendFit` + `_refitLegend()` — the prediction is made at build width, so
  a dock resize needs a second corrective pass.
- The `grid.bottom` reservation is **coupled** to the rotated-x-label gutter:
  `_refitXLabels` mutates `lf.base += delta` so the two patches compose.
- `_radarLayout()` shrinks the polygon radius by half the extra legend rows.
- `_withLegendTitle()` + `_legendTitleSeries()` — an empty carrier series
  appended purely so ECharts does not drop a legend entry matching no series,
  all so the legend can have a *title*.

`flex-wrap: wrap` replaces the lot.

### No overflow policy is needed

`_render()` hard-stops before any family dispatch when the colour column has
more than 15 distinct levels:

```js
const MAX_COLOR_LEVELS = 15;
```

with an explanatory empty state (and, for individual/timeline, a hint to use
`series` for the high-cardinality split). The band can therefore never receive
more than 15 chips. No `max-height`, no scroll, no `+N more`. Fifteen chips at
11px wrap to a few rows at worst and CSS does that for free.

Corollary: **the timeline's scroll policy is already dead code that lies.**
Its comment claims

> The legend is scroll-type, so high cardinality (AETERM with 200+ values)
> scrolls instead of being suppressed.

That cannot happen — the 15-level guard fires first. `type: 'scroll'`, the
`MAX_ROWS = 4` cliff in `_legendRows()` and the whole `scroll` return branch
all go.

### Out of scope

Pie, treemap and waterfall have no legend in either mode (pie/treemap are
self-labelling, waterfall/`cumulative` is a single running bridge). They are
excluded in the band item derivation today and stay excluded.

---

## Conflict surface with the y-axis work

Expect **textual** conflicts, not semantic ones. The y-axis work will touch
`_yGutter`, `valAxis` / `nameGap` / `nameTextStyle`, and `grid.left`. This plan
touches `grid.bottom` and the bottom term of `__panelH` — different properties,
but frequently **inside the same object literal** in:

- `_buildAggregatedOption` (the big `return { … }`)
- `_buildDistribution` (same)
- the timeline `const option = { … }`
- `_refitXLabels` (shared with `__xFit`, which is x not y — should be clear)

So: let their change land, re-grep each anchor below, then apply. Do not try to
merge two in-flight edits to the same literal.

---

## Phase A — flip the flag (small, testable, revertible)

Goal: band always draws, native legend never does. No deletions yet. This is a
working checkpoint; verify it before touching Phase B.

**A1. Force the flag at the three render paths.** Each site currently reads:

```js
      // Faceted: one shared HTML legend under the grid instead of a repeated
      // per-panel one (see _updateLegendBand).
      const sharedLegend = !singleFacet;
```

(the comment wording differs slightly between `_renderIndividual` and
`_renderTimeline` — grep `const sharedLegend = !singleFacet` for all three).

Set `const sharedLegend = true;` and rewrite the comment to state the standing
policy: *the legend is always the HTML band; panels keep a hidden legend
component whose selection model the band drives.*

`singleFacet` stays — it is also used for `dd-chart-grid-single`.

**A2. Derive band items in the single-panel case too.**

- `_renderAggregated`: drop the `if (sharedLegend) {` wrapper around the
  `bandItems` derivation (keep the inner `ct !== 'pie' && ct !== 'treemap' &&
  this._baselineMode() !== 'cumulative' && colors.length` test — that is the
  real exclusion).
- `_renderIndividual`: `if (sharedLegend && showLegend) {` → `if (showLegend) {`.
- `_renderTimeline`: `this._updateLegendBand((sharedLegend && color &&
  colorLevels.length)` → drop the `sharedLegend &&` conjunct.

**A3. Radar heading carrier.** In `_buildRadar`, this line runs regardless of
`nativeLegend` and pushes a null-valued shape so the heading chip binds:

```js
      if (legTitle) data.push({ name: legTitle, value: groups.map(() => null) });
```

With A1, `legTitle` is always `null` (it is already gated on `nativeLegend`), so
this becomes inert. Leave it for Phase B.

### Verify Phase A before continuing

`dev/verify-legend-title.R` is already exactly the right harness — one block per
legend-building family (bar, boxplot, radar, scatter, gantt), with a labelled
colour column and an unlabelled one. Add a faceted twin of one block so both
modes are on screen at once.

- Bump `Version` in `DESCRIPTION` (0.2.30 → 0.2.31) **and** the chart-js dep
  counter in `R/chart-dep.R` (`paste0(utils::packageVersion("blockr.viz"),
  ".115")` → `.116`) — the URL must change or the browser serves cached JS.
- All packages `load_all()`'d ⇒ `inst/js` source is served, no reinstall.
- Port 3838.
- Check per family, faceted and not: chips present, colours match the marks,
  heading names the variable (label when set, bare column name when not), a
  chip click hides that level in **all** panels, plot area gained the reclaimed
  bottom row, and the PNG export (gear header button) contains the band.

---

## Phase B — delete the dead machinery

Only after Phase A is verified. Each item is a straight deletion plus collapsing
a ternary to its `false` branch.

**B1. `_legendRows()`** — delete the method. Call sites (all guarded by
`nativeLegend ? … : { extra: 0, scroll: false }`) collapse to `extra = 0`,
`scroll = false`, so every `+ leg.extra` addend in `grid.bottom` and `__panelH`
disappears, as does every `...(leg.scroll ? { type: 'scroll' } : {})`.

**B2. `__legendFit` / `_refitLegend()`** — delete the method, the `__legendFit`
key from all three builder return objects, the move-onto-instance step in
`_renderAggregated`:

```js
        /** @type {any} */ (chart).__legendFit = anyOption.__legendFit;
        delete anyOption.__legendFit;
```

the equivalent `(chart).__legendFit = legendItems ? …` in `_renderIndividual`,
and the `this._refitLegend(c);` call in `_resizeCharts` (fix the neighbouring
comment about handing `_refitLegend` a corrected base).

**B3. The gutter coupling in `_refitXLabels`.** Delete:

```js
      const lf = chart.__legendFit;
      if (lf) lf.base += delta;
```

Keep the `delta` computation itself — it still drives `grid.bottom` and the slot
height. Re-read the surrounding comment block ("composes with `_refitLegend`,
which patches the same `grid.bottom` from its own base") and rewrite it: after
this change `_refitXLabels` is the *only* writer of `grid.bottom` at resize
time, which is the simplification worth recording.

**B4. `_withLegendTitle()` / `_legendTitleSeries()`** — delete both methods and
all call sites: `legTitle` / `legItems` / `legendItems` / `legendData` locals in
`_buildAggregatedOption`, `_buildRadar`, `_buildDistribution`,
`_renderIndividual`, `_renderTimeline`, plus the `...this._legendTitleSeries(…)`
spreads in each `series:` array and the radar `data.push` from A3.

**Keep `_legendTitleName()`** — `_updateLegendBand` uses it for the band's own
heading.

**B5. `legend:` in each option** collapses from the `nativeLegend ? {show:true…}
: (legendOn ? {show:false, data:…} : undefined)` ternary to the hidden form
alone. **The hidden legend component must stay** — its selection model is what
`_legendApply`'s `legendSelect`/`legendUnSelect` dispatches drive. Dropping it
would silently break every chip toggle.

**B6. `grid.bottom` / height constants** collapse to their non-native branch:

| site | now | after |
|---|---|---|
| `_buildAggregatedOption` vertical | `(nativeLegend ? 55 : 40) + 26 + xlab.bottom` | `40 + 26 + xlab.bottom` |
| `_buildAggregatedOption` horizontal | `(nativeLegend ? 55 : 20) + 26` | `20 + 26` |
| `_buildDistribution` | `46 + (nativeLegend ? 29 : 0) + …` | `46 + …` |
| `_renderIndividual` `__legendFit.base` | `(nativeLegend ? 78 : 52) + xlab.bottom` | folds away with B2 |
| `_renderTimeline` grid | `bottom: nativeLegend ? 78 : 48` | `bottom: 48` |
| `_renderTimeline` height | `heightExtra = (color && colorLevels.length > 0 && !sharedLegend) ? 100 : 80` | `heightExtra = 80` |

**B7. `_radarLayout()`** — with `extra` always 0 and `showLegend` always false it
returns a constant. Delete the method and inline `{ radius: '62%', center:
['50%', '50%'] }` in `_buildRadar`. (Note the centre moves from `'46%'` to
`'50%'`: the polygon was riding high to clear an in-canvas legend that no longer
exists. This is the one visible geometry change in the plan — eyeball it.)

**B8. `nativeLegend` / `legendOn`** — every `const nativeLegend = … &&
!sharedLegend;` is now constant `false`; delete each and fold. `legendOn` /
`showLegend` survive where they still gate the hidden component and the band
items. Then delete the `sharedLegend` parameter from `_buildAggregatedOption`,
`_buildDistribution` and `_buildRadar` (signatures **and** JSDoc `@param`), and
the argument at each call.

**B9. Keep the redundant `legendCardinality <= 15` test** in `_renderIndividual`
— it duplicates the global guard, but it is cheap and local; removing it makes
this diff depend on a guard three thousand lines away.

---

## B-item that needs thought, not mechanical deletion

**The `legendselectchanged` handler.** It has two branches:

1. A guard skipping the heading chip (`icon: 'none'` first entry) and
   re-selecting it so a click on the title toggles nothing. This is **dead**
   after B4 — the hidden legend has no heading chip. Delete it.
2. The colour-level fan-out, for when the legend shows colour levels distinct
   from the series split.

Branch 2 is **not** obviously dead, and this is the one place to be careful.
`chart.dispatchAction({ type: 'legendUnSelect' })` from `_legendApply` *does*
fire `legendselectchanged`, so the handler still runs — it would fan out again
over the already-resolved series names. That is idempotent (unselecting an
already-unselected series is a no-op) and is exactly what faceted mode does
today, so behaviour is unchanged either way. Decide by reading, not by
assuming: if nothing else can reach the hidden legend's selection model, the
handler can go entirely; if anything else can, keep branch 2. Do not delete it
just because branch 1 died.

---

## Final verification

Re-run `dev/verify-legend-title.R` across all five families, faceted and
unfaceted, plus:

- **Resize**: narrow the dock panel until the chips wrap to 2–3 rows. The plot
  must not move (the band is outside the canvas now) — this is the case that
  needed `_refitLegend` and is the headline win.
- **Rotated x labels**: a long-category bar chart, resized. Confirms B3 did not
  break the gutter delta.
- **Radar**: confirm the centre change (B7) looks right, not high or clipped.
- **Toggle persistence**: toggle a level off, push new data (change an upstream
  filter). The off state must survive — `_legendOff` is keyed on
  `col + '|' + levels`.
- **PNG export** for a single-panel chart: the band is composited from DOM
  geometry (`_downloadImage` reads `legendEl.getBoundingClientRect()`), a path
  that previously only ran for faceted charts.
- `_updateLegendBand(null)` on the empty/teardown path still clears the band.

---

## What changed on contact

Four ways the applied change differed from the plan above. Recorded because
each was a wrong assumption in the plan, not a discovery during the work.

1. **The builder signatures had grown a `sharedMax` parameter.** The concurrent
   y-axis work (`c39c97a feat(chart): facet panels share one scale by default`)
   added it to `_buildAggregatedOption` and `_buildRadar` between the plan
   being written and applied. Dropping `sharedLegend` meant editing signatures
   the plan quoted in a now-stale form. Re-grepping the anchors before editing
   is what caught it — as the conflict-surface section predicted.

2. **`legendselectchanged` was fully dead, not partly.** The plan said to
   delete the heading-chip branch and *decide* on the colour-level fan-out
   branch. Reading it out: `_legendApply` is the only caller of
   `dispatchAction({type: 'legendSelect'/'legendUnSelect'})` in the file, and
   it already resolves `slot.seriesByColorByVal` itself before dispatching. With
   the legend hidden nothing is clickable, so the event could only be triggered
   by the fan-out it was supposed to perform. The whole handler went.
   `slot.seriesByColorByVal` stays; `_legendApply` is now its only reader.

3. **Phase A was one edit, not three.** Setting `sharedLegend = true` made the
   existing `if (sharedLegend)` / `if (sharedLegend && showLegend)` guards pass
   on their own, so the band-item derivation needed no changes to work — those
   guards were then removed as dead in Phase B. The two-phase split paid off:
   Phase A was verifiable in isolation and Phase B was provably render-neutral
   (the screenshots are identical).

4. **The radar centre change had already landed in Phase A.** `_radarLayout`'s
   `showLegend` argument was `nativeLegend`, which Phase A already forced to
   false — so `'46%'` became `'50%'` there, not in Phase B. Confirmed visually
   before the method was deleted.

## Verification notes for next time

- **Dock tabs are lazy.** A board built with stacked panels renders only the
  fronted one, so a headless probe reads `bandsInDom: 10, bandsVisible: 0` and
  looks like a bug in the feature. `verify-legend-band.R` uses `grids =
  list(... dock_grid("bar", "box", ...))` to TILE the charts so every panel
  renders. See [[infra_devcontainer_shiny_bg_reaped]].
- **Launch + drive + kill must be ONE Bash call** in this container; a
  backgrounded Shiny app is reaped when the call returns.
- **Playwright MCP is usually locked** by a concurrent session. These probes use
  `chromote` directly, which launches its own browser with its own profile.
- **`pkill -f <pattern>` matches its own shell.** If the pattern appears in the
  command line you are running (it does, if you also launch the script there),
  pkill kills the call. Use a distinct port instead.
