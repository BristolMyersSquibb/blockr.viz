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

/** A brush area as ECharts reports it: pixels in `range`. */
const AREA = { brushType: 'rect', range: [[10, 60], [20, 80]], coordRange: [[0, 4], [0, 4]] };

/** Draw the scatter, then brush every point of its one series; with a drill
 * target the drag ends too. */
function brushAll(drill, target) {
  const env = h.createEnv({ record: 'none' });
  const m = h.mount(env);
  const d = cloud();
  h.send(env, m.id, { columns: d.columns, data: d.data,
                      config: { type: 'scatter', x: 'X', y: 'Y', drill,
                                ...(target ? { drill_target: target } : {}) } });
  const chart = m.block._slots[0].chart;
  const pts = chart.getOption().series[0].data;
  assert.strictEqual(pts.length, N);
  chart.trigger('brushSelected', {
    batch: [{ selected: [{ seriesIndex: 0, dataIndex: Array.from({ length: N }, (_, i) => i) }] }]
  });
  if (target) {
    chart.trigger('brushEnd', { areas: [AREA] });
    env.flushTimers();
  }
  const actions = env.inputs().filter((i) => /_select:/.test(i.name)).map((i) => i.value.selection);
  env.close();
  return actions;
}

test('brush with a drill column and target: every row once in the drill payload', () => {
  const actions = brushAll('ID', 'f1');
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

test('a drill brush sends once, at the end of the drag, and leaves only a trace', () => {
  const env = h.createEnv({ record: 'none' });
  const m = h.mount(env);
  const config = { type: 'scatter', x: 'X', y: 'Y', drill: 'ID', drill_target: 'f1' };
  const d = line(10);
  h.send(env, m.id, { columns: d.columns, data: d.data, config });
  const slot = m.block._slots[0];
  const chart = slot.chart;
  const sent = () => env.inputs().filter((i) => /_select:/.test(i.name))
    .map((i) => i.value.selection);
  // ECharts brushes the outside points itself while the reader drags.
  assert.strictEqual([].concat(chart.getOption().brush)[0].outOfBrush.opacity, 0.25);
  // The drag grows: nothing is sent while it lasts.
  chart.trigger('brush', { areas: [AREA] });
  chart.trigger('brushSelected', { batch: [{ selected: [{ seriesIndex: 0, dataIndex: [0, 1] }] }] });
  chart.trigger('brushSelected', {
    batch: [{ selected: [{ seriesIndex: 0, dataIndex: [0, 1, 2, 3, 4] }] }]
  });
  assert.strictEqual(sent().length, 0);
  chart.trigger('brushEnd', { areas: [AREA] });
  env.flushTimers(1);
  assert.strictEqual(sent().length, 1);
  assert.deepStrictEqual(sent()[0].filters.ID, ['S0', 'S1', 'S2', 'S3', 'S4']);
  // No latch: the footer shows the receipt, and a trace sits on the rectangle.
  assert.strictEqual(m.block.selection.current, null);
  assert.match(m.block.frame.statusEl.textContent, /^Drilled down to /);
  const tr = slot.chartDiv.querySelector('.vz-chart-flash');
  assert.ok(tr);
  assert.deepStrictEqual([tr.style.left, tr.style.top, tr.style.width, tr.style.height],
                         ['10px', '20px', '50px', '60px']);
  // The rectangle's clear takes nothing back: the target owns the cohort.
  chart.trigger('brush', { areas: [] });
  chart.trigger('brushSelected', { batch: [{ selected: [] }] });
  env.flushTimers();
  assert.strictEqual(sent().length, 1);
  env.close();
});

test('a redraw keeps a brushed selection; the reader can still clear it', () => {
  const env = h.createEnv({ record: 'none' });
  const m = h.mount(env);
  const config = { type: 'scatter', x: 'X', y: 'Y', drill: 'ID' };
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
  // Without a target the chart filters itself to the points in the
  // rectangle, not to every row of their IDs.
  assert.deepStrictEqual(sent()[0], { type: 'range', x: 'X', y: 'Y', x_range: [0, 4], y_range: [0, 4] });
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

test('with drill off a brush peeks: the data view gets it, the selection does not', () => {
  const env = h.createEnv({ record: 'none' });
  const m = h.mount(env);
  const d = line(5);
  h.send(env, m.id, { columns: d.columns, data: d.data,
                      config: { type: 'scatter', x: 'X', y: 'Y' } });
  const chart = m.block._slots[0].chart;
  const brush = (idx) => chart.trigger('brushSelected', {
    batch: [{ selected: [{ seriesIndex: 0, dataIndex: idx }] }]
  });
  brush([1, 2]);
  brush([1, 2]);
  const peeks = () => env.inputs().filter((i) => /_peek:/.test(i.name)).map((i) => i.value.selection);
  assert.strictEqual(env.inputs().filter((i) => /_select:/.test(i.name)).length, 0);
  assert.deepStrictEqual(peeks(), [
    { type: 'range', x: 'X', y: 'Y', x_range: [1, 2], y_range: [1, 2] }
  ]);
  chart.trigger('brush', { areas: [] });
  env.flushTimers();
  assert.deepStrictEqual(peeks().slice(-1), [null]);
  env.close();
});
