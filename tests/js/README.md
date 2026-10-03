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
npm test                      # everything, about 25 s
SKIP_SCALING=1 npm test       # snapshots only, about 3 s
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

## Scaling

`scaling.test.js` times `setData` at n and 4n patients (five rows each,
`synth.js`), median of three runs after a warm-up. Linear work takes about
4 times as long, quadratic work about 16 times; the test fails above 8
(`t(4n) / t(n) > 8`).

The four known quadratic cases are marked `todo`: they print their ratio but
do not fail the run. `SCALING_STRICT=1` runs them as ordinary tests.
`SCALING_ONLY="<case name>"` runs one case, and `CHART_JS=<path>` times
another copy of `chart.js`, for trying a change out before making it.
