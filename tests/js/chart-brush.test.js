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
    { name: 'X', type: 'numeric', n_unique: SIDE },
    { name: 'Y', type: 'numeric', n_unique: SIDE },
    { name: 'ID', type: 'categorical', n_unique: N }
  ];
  return { columns, data: { X, Y, ID } };
}

/** Draw the scatter, then brush every point of its one series. */
function brushAll(drill) {
  const env = h.createEnv({ record: 'none' });
  const { block, id } = h.mount(env);
  const d = cloud();
  h.send(env, id, { columns: d.columns, data: d.data,
                    config: { chart_type: 'scatter', x: 'X', y: 'Y', drill } });
  const chart = block._slots[0].chart;
  const pts = chart.getOption().series[0].data;
  assert.strictEqual(pts.length, N);
  /** @type {any[][]} */
  const emitted = [];
  const emit = block._emitDrill.bind(block);
  block._emitDrill = (rows) => { emitted.push(rows); emit(rows); };
  chart.trigger('brushSelected', {
    batch: [{ selected: [{ seriesIndex: 0, dataIndex: Array.from({ length: N }, (_, i) => i) }] }]
  });
  const actions = env.inputs().filter((i) => /_action$/.test(i.name)).map((i) => i.value);
  env.close();
  return { emitted, actions };
}

test('brush with a drill column: every row once in the drill payload', () => {
  const { emitted, actions } = brushAll('ID');
  assert.strictEqual(emitted.length, 1);
  assert.strictEqual(emitted[0].length, N);
  assert.strictEqual(new Set(emitted[0]).size, N);
  assert.strictEqual(actions.length, 1);
  const a = actions[0];
  assert.strictEqual(a.filter_type, 'categorical');
  // v1 sends {column, values}; v2 the named list {filters: {ID: [...]}} (D2).
  const values = a.filters ? a.filters.ID : a.values;
  if (!a.filters) assert.strictEqual(a.column, 'ID');
  assert.strictEqual(values.length, N);
  assert.strictEqual(new Set(values).size, N);
});

test('brush without a drill column: a range over every brushed point', () => {
  const { emitted, actions } = brushAll('auto');
  assert.strictEqual(emitted.length, 0);
  assert.strictEqual(actions.length, 1);
  assert.deepStrictEqual(actions[0], {
    action: 'filter', filter_type: 'range', x_col: 'X', y_col: 'Y',
    x_range: [0, SIDE - 1], y_range: [0, SIDE - 1]
  });
});
