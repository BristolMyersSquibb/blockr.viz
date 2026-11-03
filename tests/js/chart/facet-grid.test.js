/* A panel grid (facet_row): `facet` levels across, `facet_row` levels down,
 * strips on top and on the right, an empty cell for a combination without
 * rows, y shared within a row. Without facet_row the panels wrap as before. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const h = require('../harness');
const { F, BASE } = require('../cases');
const { loadPure, plain } = require('./load');

const C = loadPure();

const LINE = { ...BASE, type: 'line', x: 'AVISITN', y: 'AVAL', series: 'USUBJID', facet: 'ARM' };

// The fixture's rows, without one ARM x SEX combination, so the grid has an
// empty cell.
const rowsOf = (d) => d.ARM.map((_, i) => Object.fromEntries(Object.keys(d).map((k) => [k, d[k][i]])));
const colsOf = (rows) => Object.fromEntries(Object.keys(rows[0]).map((k) => [k, rows.map((r) => r[k])]));
const ALL = rowsOf(F.data);
// A missing level is a level of the grid, '' in the key, "(missing)" on its strip.
const levelsOf = (col) => [...new Set(ALL.map((r) => (r[col] == null ? '' : String(r[col]))))].sort();
const ARMS = levelsOf('ARM');
const SEXES = levelsOf('SEX');
const strip = (lv) => (lv === '' ? '(missing)' : lv);
const GAP = { ARM: ARMS[ARMS.length - 1], SEX: SEXES[SEXES.length - 1] };
const DATA = colsOf(ALL.filter((r) => !(r.ARM === GAP.ARM && r.SEX === GAP.SEX)));

const drawGrid = (cfg) => h.draw({ ...LINE, facet_row: 'SEX', ...cfg }, DATA, F.columns,
                                  { width: 900, keepOpen: true });

test('a grid key splits back into the row and the column column', () => {
  const g = { row: 'SEX', col: 'ARM' };
  const key = C.model.gridKey({ SEX: 'F', ARM: 'Placebo' }, g);
  assert.deepStrictEqual(plain(C.model.gridLevels(key)), { row: 'F', col: 'Placebo' });
  assert.deepStrictEqual(plain(C.model.facetKeys({ AVAL: 1 }, C.model.GRID_COL, g, key)),
                         { AVAL: 1, SEX: 'F', ARM: 'Placebo' });
  // Outside a grid the facet key is as before.
  assert.deepStrictEqual(plain(C.model.facetKeys({}, 'ARM', null, 'Placebo')), { ARM: 'Placebo' });
  assert.deepStrictEqual(plain(C.model.facetKeys({}, 'ARM', null, '__all__')), {});
});

test('settings: facet_row makes a grid over facet', () => {
  const s = C.model.settings({ type: 'line', facet: 'ARM', facet_row: 'SEX' }, F.columns);
  assert.strictEqual(s.facet, C.model.GRID_COL);
  assert.deepStrictEqual(plain(s.facet_grid), { row: 'SEX', col: 'ARM' });
  const w = C.model.settings({ type: 'line', facet: 'ARM' }, F.columns);
  assert.strictEqual(w.facet, 'ARM');
  assert.strictEqual(w.facet_grid, null);
});

test('without facet_row the panels wrap exactly as before', () => {
  const a = h.draw({ ...LINE }, F.data, F.columns, { width: 900 });
  const b = h.draw({ ...LINE, facet_row: null }, F.data, F.columns, { width: 900 });
  assert.strictEqual(a.error, null);
  assert.deepStrictEqual(b.dom, a.dom);
  assert.deepStrictEqual(b.options, a.options);
  assert.strictEqual(a.dom.slots.length, ARMS.length);
});

test('the grid puts each panel at its row and column, strips on top and right', () => {
  const r = drawGrid({});
  try {
    assert.strictEqual(r.error, null);
    const grid = r.block.chartGrid;
    assert.ok(grid.classList.contains('vz-chart-grid--matrix'));
    assert.strictEqual(grid.style.gridTemplateColumns, `repeat(${ARMS.length}, minmax(0, 1fr)) auto`);
    const strips = (cls) => Array.from(grid.querySelectorAll('.' + cls)).map((e) => ({
      text: e.textContent, row: e.style.gridRow, col: e.style.gridColumn }));
    assert.deepStrictEqual(strips('vz-chart-strip--col'),
                           ARMS.map((a, j) => ({ text: strip(a), row: '1', col: String(j + 1) })));
    assert.deepStrictEqual(strips('vz-chart-strip--row'), SEXES.map((s, i) =>
      ({ text: strip(s), row: String(i + 2), col: String(ARMS.length + 1) })));
    // One panel per combination with rows; the gap's cell holds none.
    const cells = r.block._slots.map((s) => s.container.style.gridRow + '/' + s.container.style.gridColumn);
    const lv = (v) => (v == null ? '' : String(v));
    const combos = new Set(rowsOf(DATA).map((x) => lv(x.SEX) + '|' + lv(x.ARM)));
    assert.ok(combos.size < ARMS.length * SEXES.length);
    assert.strictEqual(cells.length, combos.size);
    assert.strictEqual(new Set(cells).size, cells.length);
    const gap = (SEXES.indexOf(GAP.SEX) + 2) + '/' + (ARMS.indexOf(GAP.ARM) + 1);
    assert.ok(!cells.includes(gap));
    // No per-panel label: the strips name the panels.
    assert.ok(r.dom.slots.every((s) => s.label === null));
  } finally {
    r.env.close();
  }
});

// The y domain each panel was patched to share, as "min:max", or null.
const yShared = (r) => r.block._slots.map((s) => {
  const patch = s.chart.log.filter((e) => e.op === 'setOption' && e.option && e.option.yAxis &&
    e.option.yAxis[0] && e.option.yAxis[0].min != null && Object.keys(e.option).length === 1);
  const last = patch[patch.length - 1];
  return last ? last.option.yAxis[0].min + ':' + last.option.yAxis[0].max : null;
});

test('free_y in a grid shares y within each row, not across rows', () => {
  const r = drawGrid({ facet_scales: 'free_y' });
  try {
    const y = yShared(r);
    const byRow = new Map();
    r.block._slots.forEach((s, i) => {
      if (!byRow.has(s.gridRow)) byRow.set(s.gridRow, new Set());
      byRow.get(s.gridRow).add(y[i]);
    });
    for (const doms of byRow.values()) assert.strictEqual(doms.size, 1);
    assert.ok(y.every((d) => d !== null));
  } finally {
    r.env.close();
  }
});

test('fixed in a grid shares one y across every panel', () => {
  const r = drawGrid({ facet_scales: 'fixed' });
  try {
    assert.strictEqual(new Set(yShared(r)).size, 1);
  } finally {
    r.env.close();
  }
});

// The domain each panel was patched to share on `key`, as "min:max", or null.
const shared = (r, key) => r.block._slots.map((s) => {
  const patch = s.chart.log.filter((e) => e.op === 'setOption' && e.option && e.option[key] &&
    e.option[key][0] && e.option[key][0].min != null && Object.keys(e.option).length === 1);
  const last = patch[patch.length - 1];
  return last ? last.option[key][0].min + ':' + last.option[key][0].max : null;
});

test('horizontal bars in a grid: free_y shares the value axis (x) within a row', () => {
  const r = h.draw({ ...BASE, type: 'bar', category: 'AETERM', func: 'count', orientation: 'horizontal',
                     facet: 'ARM', facet_row: 'SEX', facet_scales: 'free_y' }, DATA, F.columns,
                   { width: 900, keepOpen: true });
  try {
    assert.strictEqual(r.error, null);
    const x = shared(r, 'xAxis');
    assert.ok(x.every((d) => d !== null));
    const byRow = new Map();
    r.block._slots.forEach((s, i) => {
      if (!byRow.has(s.gridRow)) byRow.set(s.gridRow, new Set());
      byRow.get(s.gridRow).add(x[i]);
    });
    for (const doms of byRow.values()) assert.strictEqual(doms.size, 1);
    // The categories stay shared: every panel lists the same ones.
    const cats = r.block._slots.map((s) => JSON.stringify(s.chart.getOption().yAxis[0].data));
    assert.strictEqual(new Set(cats).size, 1);
  } finally {
    r.env.close();
  }
});
