/* ECharts sends an empty brushSelected after the brush cursor is armed,
 * which every draw of a brushable scatter does. Taken for the reader
 * clearing the brush, it would drop a restored range on load and send R a
 * clear. The chart clears on an empty selection only a filter a brush made;
 * the reader's own clear is a `brush` event with no areas. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const I = require('../interact');

const RANGE = {
  chart_type: 'scatter', x: 'ADY', y: 'AVAL', drill: 'auto', filter_type: 'range',
  filter_range: { x_col: 'ADY', y_col: 'AVAL', x_range: [1, 30], y_range: [25, 32] }
};
const armed = (c) => c.trigger('brushSelected', { batch: [{ areas: [], selected: [] }] });

/** Run `steps` on a fresh chart; the inputs sent and the footer after. */
const run = (cfg, steps) => {
  const c = I.open(cfg);
  try {
    const m = c.mark();
    for (const s of steps) s(c);
    return { inputs: c.since(m).inputs, footer: c.footer(), filter: c.block._filter };
  } finally {
    c.close();
  }
};

test('a restored range survives the brush cursor being armed', () => {
  const r = run(RANGE, [armed]);
  assert.deepStrictEqual(r.inputs, []);
  assert.strictEqual(r.filter.type, 'range');
  assert.match(JSON.stringify(r.footer), /to 30/);
});

test('a point click survives the brush cursor being armed', () => {
  const r = run({ chart_type: 'scatter', x: 'ADY', y: 'AVAL', drill: 'auto' }, [
    (c) => c.click(0, 0, 0), (c) => c.advance(200), armed
  ]);
  assert.strictEqual(r.inputs.length, 1);
  assert.strictEqual(r.filter.type, 'range');
});

test('an empty selection still clears a brush, and a brush clear any filter', () => {
  const brushed = (c) => c.trigger('brushSelected',
    { batch: [{ selected: [{ seriesIndex: 0, dataIndex: [0, 1] }] }] });
  const a = run({ chart_type: 'scatter', x: 'ADY', y: 'AVAL', drill: 'auto' }, [brushed, armed]);
  assert.strictEqual(a.filter, null);
  assert.strictEqual(a.inputs.length, 2);
  const b = run(RANGE, [(c) => c.trigger('brush', { areas: [] })]);
  assert.strictEqual(b.filter, null);
  assert.strictEqual(b.inputs.length, 1);
});
