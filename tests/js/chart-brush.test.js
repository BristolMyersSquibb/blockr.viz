/* The scatter brush on a large cloud of stacked points: 150,000 rows on 100
 * distinct (x, y) positions, all of them brushed. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const h = require('./harness');

const N = 150000;
const SIDE = 10;  // SIDE x SIDE distinct (x, y) positions

function cloud() {
  const X = new Array(N), Y = new Array(N), ID = new Array(N);
  for (let i = 0; i < N; i++) {
    X[i] = i % SIDE;
    Y[i] = Math.floor(i / SIDE) % SIDE;
    ID[i] = 'R' + i;
  }
  const columns = [
    { name: 'X', class: 'number', n_unique: SIDE },
    { name: 'Y', class: 'number', n_unique: SIDE },
    { name: 'ID', class: 'string', n_unique: N }
  ];
  return { columns, data: { X, Y, ID } };
}

/** Draw the scatter, then brush every point of its one series. */
function brushAll(drill) {
  const env = h.createEnv({ record: 'none' });
  const m = h.mount(env);
  const d = cloud();
  h.send(env, m.id, { columns: d.columns, data: d.data,
                      config: { type: 'scatter', x: 'X', y: 'Y', drill } });
  const chart = m.block._slots[0].chart;
  const pts = chart.getOption().series[0].data;
  assert.strictEqual(pts.length, N);
  chart.trigger('brushSelected', {
    batch: [{ selected: [{ seriesIndex: 0, dataIndex: Array.from({ length: N }, (_, i) => i) }] }]
  });
  const actions = env.inputs().filter((i) => /_select:/.test(i.name)).map((i) => i.value.selection);
  env.close();
  return actions;
}

test('brush with a drill column: every row once in the drill payload', () => {
  const actions = brushAll('ID');
  assert.strictEqual(actions.length, 1);
  const a = actions[0];
  assert.strictEqual(a.type, 'categorical');
  const values = a.filters.ID;
  assert.strictEqual(values.length, N);
  assert.strictEqual(new Set(values).size, N);
});

test('brush without a drill column: a range over every brushed point', () => {
  const actions = brushAll('auto');
  assert.strictEqual(actions.length, 1);
  assert.deepStrictEqual(actions[0], {
    type: 'range', x: 'X', y: 'Y', x_range: [0, SIDE - 1], y_range: [0, SIDE - 1]
  });
});

/** A small scatter: `n` points on the diagonal, one ID each. */
function line(n) {
  const X = [], Y = [], ID = [];
  for (let i = 0; i < n; i++) { X.push(i); Y.push(i); ID.push('S' + i); }
  const columns = [
    { name: 'X', class: 'number', n_unique: n },
    { name: 'Y', class: 'number', n_unique: n },
    { name: 'ID', class: 'string', n_unique: n }
  ];
  return { columns, data: { X, Y, ID } };
}

test('a redraw keeps a brushed selection; the reader can still clear it', () => {
  const env = h.createEnv({ record: 'none' });
  const m = h.mount(env);
  const config = { type: 'scatter', x: 'X', y: 'Y', drill: 'ID', drill_target: 'f1' };
  const d = line(10);
  h.send(env, m.id, { columns: d.columns, data: d.data, config });
  const chart = m.block._slots[0].chart;
  const sent = () => env.inputs().filter((i) => /_select:/.test(i.name))
    .map((i) => i.value.selection);
  chart.trigger('brush', { areas: [{ brushType: 'rect', coordRange: [[0, 4], [0, 4]] }] });
  chart.trigger('brushSelected', {
    batch: [{ selected: [{ seriesIndex: 0, dataIndex: [0, 1, 2, 3, 4] }] }]
  });
  assert.strictEqual(sent().length, 1);
  assert.deepStrictEqual(sent()[0].filters.ID, ['S0', 'S1', 'S2', 'S3', 'S4']);
  // New rows upstream redraw the chart, and ECharts reports the redrawn
  // brush as empty before the rectangle is put back.
  const d2 = line(8);
  h.send(env, m.id, { columns: d2.columns, data: d2.data, config, dataRev: 2 });
  chart.trigger('brushSelected', { batch: [{ selected: [] }] });
  chart.trigger('brush', { areas: [] });
  assert.strictEqual(sent().length, 1);
  // Later, an empty brush is the reader's and clears.
  env.flushTimers();
  chart.trigger('brushSelected', { batch: [{ selected: [] }] });
  assert.strictEqual(sent().length, 2);
  assert.strictEqual(sent()[1], null);
  env.close();
});
