/* Scaling: setData at n and 2n patients (five rows each), median of three
 * runs after one warm-up. Linear work doubles; the test fails when the time
 * more than triples, which is where quadratic work shows.
 *
 * SKIP_SCALING=1 skips the file. SCALING_STRICT=1 runs the known-quadratic
 * cases as ordinary tests (they fail today). SCALING_ONLY=<name> runs one
 * case. CHART_JS=<path> (see harness.js) times another copy of chart.js.
 */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const h = require('./harness');
const { synth } = require('./synth');

const MAX_RATIO = 3;
const RUNS = 3;

const median = (xs) => xs.slice().sort((a, b) => a - b)[Math.floor(xs.length / 2)];

/** Median setData ms at `n` patients, after one warm-up run. */
function measure(env, config, n) {
  const d = synth(n);
  const run = () => h.timeDraw(env, { columns: d.columns, data: d.data, config: { ...config } });
  run();
  const ts = [];
  for (let i = 0; i < RUNS; i++) ts.push(run());
  return median(ts);
}

// n: patients in the small run, picked so it takes roughly 50-300 ms here.
const CASES = [
  { name: 'bar, 200 terms by arm', n: 12000,
    config: { chart_type: 'bar', group: 'TERM', color: 'ARM', drill: 'auto' } },
  { name: 'bar, faceted by arm', n: 12000,
    config: { chart_type: 'bar', group: 'TERM', facet: 'ARM', drill: 'auto' } },
  { name: 'pie', n: 15000,
    config: { chart_type: 'pie', group: 'GRADE', drill: 'auto' } },
  { name: 'boxplot by visit', n: 8000,
    config: { chart_type: 'boxplot', group: 'AVISIT', value: 'AVAL', drill: 'auto' } },
  { name: 'scatter', n: 15000,
    config: { chart_type: 'scatter', x: 'ADY', y: 'AVAL', drill: 'auto' } },
  { name: 'line, faceted by arm', n: 8000,
    config: { chart_type: 'line', x: 'ADY', y: 'AVAL', facet: 'ARM', color: 'GRADE',
              drill: 'auto' } },
  { name: 'gantt, lane per patient, faceted by arm', n: 8000,
    config: { chart_type: 'gantt', x: 'ASTDY', xend: 'AENDY', y: 'USUBJID', facet: 'ARM',
              sort_by: 'onset', drill: 'auto' } },
  // Known quadratic: a scan of every row per series / group level.
  { name: 'scatter, series = patient', n: 700,
    todo: 'rows.filter() per series level in _renderIndividual',
    config: { chart_type: 'scatter', x: 'ADY', y: 'AVAL', series: 'USUBJID', drill: 'auto' } },
  { name: 'scatter, series = patient, colour = arm', n: 500,
    todo: 'rows.filter() and data.find() per series level in _renderIndividual',
    config: { chart_type: 'scatter', x: 'ADY', y: 'AVAL', series: 'USUBJID', color: 'ARM',
              drill: 'auto' } },
  { name: 'boxplot, group = patient, colour = arm', n: 600,
    todo: 'rowsFor() filters the facet rows per (group, level) in _buildDistribution',
    config: { chart_type: 'boxplot', group: 'USUBJID', color: 'ARM', value: 'AVAL',
              drill: 'auto' } },
  { name: 'bar, group = patient, colour = grade', n: 4000,
    todo: 'facetData.find() per (group, colour) in _buildAggregatedOption',
    config: { chart_type: 'bar', group: 'USUBJID', color: 'GRADE', drill: 'auto' } }
];

const skip = process.env.SKIP_SCALING === '1';
const strict = process.env.SCALING_STRICT === '1';
const only = process.env.SCALING_ONLY;

for (const cs of CASES) {
  if (only && cs.name !== only) continue;
  const opts = skip ? { skip: 'SKIP_SCALING=1' }
    : (cs.todo && !strict ? { todo: cs.todo } : {});
  test(`scaling: ${cs.name}`, opts, (t) => {
    const env = h.createEnv({ record: 'none' });
    try {
      const t1 = measure(env, cs.config, cs.n);
      const t2 = measure(env, cs.config, 2 * cs.n);
      const ratio = t2 / t1;
      const line = `n=${cs.n}: ${t1.toFixed(1)} ms, 2n: ${t2.toFixed(1)} ms, ratio ${ratio.toFixed(2)}`;
      t.diagnostic(line);
      assert.ok(ratio <= MAX_RATIO, `t(2n)/t(n) = ${ratio.toFixed(2)} > ${MAX_RATIO} (${line})`);
    } finally {
      env.close();
    }
  });
}

module.exports = { measure, CASES, MAX_RATIO };
