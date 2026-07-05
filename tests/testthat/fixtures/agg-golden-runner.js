// Standalone node runner for the golden aggregation cross-test
// (tests/testthat/test-agg-golden.R). Loads inst/js/drilldown-agg.js (path =
// argv[2]) exactly as shipped, feeds it {rows, config} JSON from stdin and
// writes Blockr.DrilldownAgg.aggregate(rows, config) as JSON to stdout, so
// testthat can compare the JS engine's numbers against the R engine's.
//
// drilldown-agg.js attaches to `window` (browser convention); alias it to
// globalThis so the file loads untouched outside a browser.
'use strict';

globalThis.window = globalThis;
require(process.argv[2]);

const chunks = [];
process.stdin.on('data', (c) => chunks.push(c));
process.stdin.on('end', () => {
  const input = JSON.parse(Buffer.concat(chunks).toString('utf8'));
  const out = globalThis.window.DrilldownAgg.aggregate(input.rows, input.config);
  process.stdout.write(JSON.stringify(out));
});
