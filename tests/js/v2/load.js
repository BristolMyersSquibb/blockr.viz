/* Load v2's pure scripts into a plain node vm context: no DOM, no ECharts,
 * no Shiny. Returns the namespace they fill (Blockr.chart).
 *
 * The scripts are the ones the page loads, in scripts.txt order, up to the
 * first that needs a browser. roles.js needs drilldown-agg.js before it;
 * the aggregated model and option need both (the aggregation engine, the
 * statistic names), so `{ roles: true }` loads them.
 */
'use strict';

const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');

const JS_DIR = path.join(__dirname, '..', '..', '..', 'inst', 'js');
const PURE = ['common.js', 'chart-index.js', 'chart-keys.js', 'axes-labels.js',
              'model-common.js', 'model-timeline.js', 'model-aggregated.js',
              'model-individual.js', 'option-timeline.js', 'option-bar.js', 'option-radial.js',
              'option-distribution.js', 'option-aggregated.js', 'option-points.js',
              'option-band.js', 'option-individual.js'];

/** @param {{ roles?: boolean }} [opts] */
function loadPure(opts = {}) {
  const ctx = vm.createContext({ console });
  ctx.window = ctx;
  const run = (f) => vm.runInContext(fs.readFileSync(path.join(JS_DIR, f), 'utf8'), ctx,
                                     { filename: f });
  for (const f of PURE) run(path.join('chart-v2', f));
  if (opts.roles) {
    run('drilldown-agg.js');
    run(path.join('chart-v2', 'roles.js'));
  }
  return ctx.Blockr.chart;
}

/** Plain JSON copy, so values from the vm realm compare with deepStrictEqual. */
const plain = (x) => JSON.parse(JSON.stringify(x, (k, v) =>
  (Object.prototype.toString.call(v) === '[object Map]' ? Object.fromEntries(v) : v)));

/** A text measure for option builds: 7 px per character, as the harness. */
const measure = (s) => 7 * String(s).length;

module.exports = { loadPure, plain, measure, JS_DIR };
