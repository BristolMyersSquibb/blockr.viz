# JavaScript tests for the chart block

`inst/js/chart.js` runs in node with happy-dom, loaded the way the page loads
it: blockr.ui's controls, `drilldown-agg.js`, `drilldown-config.js`,
`capture-pages.js`, `drilldown-theme-register.js`, `chart.js`. A chart is
created by the input binding's `initialize` and drawn by the `drilldown-data`
message handler. echarts is a fake that records what the chart asks of it
(`harness.js` lists everything that is stubbed).

## Running

```sh
npm ci
npm test                      # everything, about 70 s
SKIP_SCALING=1 npm test       # snapshots only, about 7 s
node --test tests/js/chart-aggregated.test.js
```

blockr.ui's scripts come from the installed package (found with `Rscript`).
`BLOCKR_UI_JS=/path/to/blockr.ui/inst/assets/js` uses a source tree instead,
which is what CI does.

## Snapshots

Each case in `cases.js` draws one chart config against
`fixtures/chart-data.json` and is compared with
`__snapshots__/<case>.json`: every `setOption`, `dispatchAction` and `resize`
per facet panel, what the option's formatters print for sample inputs
(`probes`), and the text outside the canvas (titles, legend band, facet
labels, footer, empty states). Numbers are rounded to 6 significant digits
and functions are written as `"[fn]"`.

A missing snapshot fails. After an intended change:

```sh
UPDATE_SNAPSHOTS=1 npm test
```

and review the diff of `__snapshots__/` before committing it.

The fixture is built by `fixtures/make-fixtures.R` with the package's own
payload builders (`dd_col_meta()`, `chart_data_json()`,
`compute_band_series()`, `compute_smoother_series()`). Rebuild it with
`Rscript tests/js/fixtures/make-fixtures.R` from the package root, then
update the snapshots.

## Interactions

The `interactions-*.test.js` files record everything the chart does besides
drawing, as the reference for a rewrite of `chart.js`: clicks and drill,
brush, the hover pickers, the gear, the sentence words, state restore,
lifecycle, the messages R sends, and export. Snapshots are in
`__snapshots__/interactions/` and update the same way as the drawing ones.
`interact.js` holds the helpers: a chart opened on a virtual clock
(`advance(ms)` runs timers, `performance.now()` follows it), click params
built the way echarts builds them, and the step runner that records, per
step, the inputs sent, the echarts calls (a full redraw is reduced to its
series types), the footer and the selection.

`CONTRACT.md` lists every input the chart sends to R and every message it
handles, with the snapshots that pin each one.

## Scaling

`scaling.test.js` times `setData` at n and 4n patients (five rows each,
`synth.js`), median of three runs after a warm-up. Linear work takes about
4 times as long, quadratic work about 16 times; the test fails above 8
(`t(4n) / t(n) > 8`).

The cases with a level per patient (series, group, categorical x) are the
ones that used to be quadratic, before chart.js read rows through its
`RowIndex`. `SCALING_ONLY="<case name>"` runs one case, and
`CHART_JS=<path>` times another copy of `chart.js`, for trying a change out
before making it.

## The v2 engine

`inst/js/chart-v2/` is the rewrite of `chart.js`, built beside it one chart
family at a time (spec: blockr.design `open/chart-block-v2`). The harness
loads either engine:

```sh
npm run test:v2               # every test above, against v2
CHART_ENGINE=v2 node --require ./tests/js/v2-preload.js --test tests/js/chart-timeline.test.js
```

Under v2 the snapshots are still v1's, and v2 has to reproduce them.
`v2-differences.json` lists the ones it does not, each with the IDs of the
decisions that explain the difference (D1-D7, B1-B7) and a line of why; what
v2 draws instead is pinned in `__snapshots__/v2/`. An unlisted difference
fails, and so does a listed one that v2 now reproduces.
`UPDATE_SNAPSHOTS=1 npm run test:v2` rewrites `__snapshots__/v2/` only.

`v2-pending.js` names the tests of families v2 does not draw yet; the
preload (`v2-preload.js`) skips them. Each build step removes its family's
entries. `V2_RUN_PENDING=1` runs them anyway.

`tests/js/v2/` holds the node tests of v2's pure parts (the index, keys,
models and options, loaded into a vm context without a DOM), a comparison of
the v2 binding with v1's, and checks of the two lists above. `npm test` runs
them.
