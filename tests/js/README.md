# JavaScript tests for the chart block

The chart scripts in `inst/js/chart/` run in node with happy-dom, loaded
the way the page loads them: blockr.ui's controls, `drilldown-agg.js`,
`drilldown-config.js`, `capture-pages.js`, `busy-cue.js`,
`drilldown-theme-register.js`, then the chart scripts in the order
`scripts.txt` lists them. A chart is
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
drawing: clicks and drill,
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
ones that used to be quadratic, before the chart read rows through its
`RowIndex`. `SCALING_ONLY="<case name>"` runs one case.

## Unit tests

`tests/js/chart/` holds the node tests of the chart's pure parts (the index,
keys, models and options, loaded into a vm context without a DOM), the
binding's message handling and a few behaviours too narrow for a scenario.
`npm test` runs them.

## Summarize table

`summarize-table.test.js` drives the summarize table block's script
(`inst/js/rank-table.js`) the way the page does: the chrome the block's UI
renders once, then the payloads its server pushes. Both come from the block's
R code, recorded by `fixtures/make-summarize-fixtures.R` under
`shiny::testServer()`, so a class, attribute or message name that R writes and
the script does not read fails here. Rebuild the fixture after changing the
block's server, payload or chrome:

```sh
Rscript tests/js/fixtures/make-summarize-fixtures.R
```
