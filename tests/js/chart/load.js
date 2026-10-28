/* Load the chart's pure scripts into a plain node vm context: no DOM, no ECharts,
 * no Shiny. Returns the namespace they fill (Blockr.viz.chart).
 *
 * The scripts are the core namespace and helpers, then the chart's in
 * scripts.txt order up to the first that needs a browser, plus the
 * generated spec.
 */
'use strict';

const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');

const JS_DIR = path.join(__dirname, '..', '..', '..', 'inst', 'js');
// The chart's scripts in scripts.txt order, up to the gear host; the rest
// need a browser.
const SCRIPTS = fs.readFileSync(path.join(JS_DIR, 'chart', 'scripts.txt'), 'utf8')
  .split('\n').map((l) => l.trim()).filter((l) => l && !l.startsWith('#'));
const PURE = SCRIPTS.slice(0, SCRIPTS.indexOf('gear.js') + 1);

/** The whole namespace, Blockr.viz, with the core helpers the chart uses. */
function loadViz() {
  const ctx = vm.createContext({ console });
  ctx.window = ctx;
  const run = (f) => vm.runInContext(fs.readFileSync(path.join(JS_DIR, f), 'utf8'), ctx,
                                     { filename: path.join(JS_DIR, f) });
  for (const f of ['namespace.js', 'util.js', 'tooltip.js']) run(path.join('core', f));
  for (const f of PURE) run(path.join('chart', f));
  run(path.join('spec', 'chart.js'));
  return ctx.Blockr.viz;
}

function loadPure() {
  return loadViz().chart;
}

/** Plain JSON copy, so values from the vm realm compare with deepStrictEqual. */
const plain = (x) => JSON.parse(JSON.stringify(x, (k, v) =>
  (Object.prototype.toString.call(v) === '[object Map]' ? Object.fromEntries(v) : v)));

/** A text measure for option builds: 7 px per character, as the harness. */
const measure = (s) => 7 * String(s).length;

module.exports = { loadPure, loadViz, plain, measure, JS_DIR };
