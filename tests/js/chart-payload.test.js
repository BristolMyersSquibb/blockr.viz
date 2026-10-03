/* The payload forms setData accepts draw the same chart. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const h = require('./harness');
const { F, aggregated, individual, timeline, other } = require('./cases');
const { caseSnapshot, snapshotTests } = require('./run-case');

snapshotTests(other);

const byName = (n) => [...aggregated, ...individual, ...timeline].find((c) => c.name === n);
const PICKS = ['bar-color-stacked', 'boxplot-color-outliers', 'scatter-series-color',
               'line-facet-counts', 'gantt-patient-facet-counts'];

for (const name of PICKS) {
  test(`${name}: dictionary-encoded columns draw the same`, () => {
    const cs = byName(name);
    assert.deepStrictEqual(caseSnapshot({ ...cs, dict: true }), caseSnapshot(cs));
  });

  test(`${name}: a JSON string payload draws the same`, () => {
    const cs = byName(name);
    assert.deepStrictEqual(caseSnapshot({ ...cs, data: JSON.stringify(F.data) }),
                           caseSnapshot(cs));
  });
}

test('an unchanged data_rev keeps the parsed rows', () => {
  const env = h.createEnv();
  const { block, id } = h.mount(env);
  const config = byName('bar-group').config;
  h.send(env, id, { columns: F.columns, data: F.data, config, dataRev: 7 });
  const rows = block.data;
  assert.strictEqual(rows.length, 60);
  h.send(env, id, { columns: F.columns, data: F.data, config, dataRev: 7 });
  assert.strictEqual(block.data, rows);
  h.send(env, id, { columns: F.columns, data: F.data, config, dataRev: 8 });
  assert.notStrictEqual(block.data, rows);
  env.close();
});
