/* The memo the chart's models and options run through (chart/memo.js): a
 * result is reused while the config fields the computation read hold the
 * same values, and recomputed when one of them, or another input, changes. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure } = require('./load');

const C = loadPure();

/** A memo over a computation that reads `x`, and `y` only when `x` is 'both'. */
const setup = () => {
  const memo = new C.ReadMemo();
  let runs = 0;
  const compute = (cfg) => {
    runs++;
    return cfg.x === 'both' ? cfg.x + cfg.y : cfg.x + cfg.derived.z;
  };
  return { memo, compute, runs: () => runs };
};

test('a field the computation did not read changes nothing', () => {
  const { memo, compute, runs } = setup();
  assert.strictEqual(memo.update({ x: 'a', y: 1, derived: { z: 2 } }, '', compute), true);
  assert.strictEqual(memo.value, 'a2');
  assert.strictEqual(memo.update({ x: 'a', y: 9, derived: { z: 2, w: 5 } }, '', compute), false);
  assert.strictEqual(runs(), 1);
  assert.deepStrictEqual([...memo.reads], ['derived.z', 'x']);
});

test('a field it read, in the config or in derived, recomputes', () => {
  const { memo, compute, runs } = setup();
  memo.update({ x: 'a', derived: { z: 2 } }, '', compute);
  memo.update({ x: 'a', derived: { z: 3 } }, '', compute);
  assert.strictEqual(memo.value, 'a3');
  memo.update({ x: 'b', derived: { z: 3 } }, '', compute);
  assert.strictEqual(memo.value, 'b3');
  assert.strictEqual(runs(), 3);
});

test('another path through the code reads other fields, and keys on them', () => {
  const { memo, compute, runs } = setup();
  memo.update({ x: 'a', y: 1, derived: { z: 2 } }, '', compute);
  memo.update({ x: 'both', y: 1, derived: { z: 2 } }, '', compute);
  assert.deepStrictEqual([...memo.reads], ['x', 'y']);
  memo.update({ x: 'both', y: 2, derived: { z: 2 } }, '', compute);
  assert.strictEqual(memo.value, 'both2');
  memo.update({ x: 'both', y: 2, derived: { z: 7 } }, '', compute);
  assert.strictEqual(runs(), 3, 'derived.z is no longer read');
});

test('the other inputs are part of the key', () => {
  const { memo, compute, runs } = setup();
  memo.update({ x: 'a', derived: { z: 2 } }, 'rows 1', compute);
  memo.update({ x: 'a', derived: { z: 2 } }, 'rows 2', compute);
  assert.strictEqual(runs(), 2);
});

test('the model and option read through a recording config', () => {
  const memo = new C.ReadMemo();
  const cfg = { type: 'bar', color: null, derived: { palette: ['#000000'], band: null } };
  memo.update(cfg, '', (c) => C.paletteOf(c));
  assert.deepStrictEqual([...memo.reads], ['derived.palette']);
});
