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
// A line drops a row missing a facet level, in a grid as in a wrap.
const missing = (v) => v == null || v === '';
const levelsOf = (col) => [...new Set(ALL.filter((r) => !missing(r[col])).map((r) => String(r[col])))].sort();
const ARMS = levelsOf('ARM');
const SEXES = levelsOf('SEX');
const strip = (lv) => lv;
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
  // A wrap keeps its panel for a missing level, as before.
  const wrapped = new Set(ALL.map((x) => (missing(x.ARM) ? '' : String(x.ARM))));
  assert.strictEqual(a.dom.slots.length, wrapped.size);
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
    const combos = new Set(rowsOf(DATA).filter((x) => !missing(x.SEX) && !missing(x.ARM))
      .map((x) => x.SEX + '|' + x.ARM));
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

// The rows of `ALL` with `col` missing in the first `n`, as columns.
const withMissing = (col, n) => colsOf(ALL.map((r, i) => (i < n ? { ...r, [col]: null } : r)));

test('a missing level: a line drops the row, a bar keeps it as "(missing)", as in a wrap', () => {
  const data = withMissing('SEX', 6);
  const line = h.draw({ ...LINE, facet_row: 'SEX' }, data, F.columns, { width: 900, keepOpen: true });
  const bar = h.draw({ ...BASE, type: 'bar', category: 'AETERM', func: 'count', facet: 'ARM',
                       facet_row: 'SEX' }, data, F.columns, { width: 900, keepOpen: true });
  const drop = h.draw({ ...BASE, type: 'bar', category: 'AETERM', func: 'count', facet: 'ARM',
                        facet_row: 'SEX', missing: 'drop' }, data, F.columns, { width: 900, keepOpen: true });
  const rowStrips = (r) => Array.from(r.block.chartGrid.querySelectorAll('.vz-chart-strip--row'))
    .map((e) => e.textContent);
  try {
    assert.strictEqual(line.error, null);
    assert.deepStrictEqual(rowStrips(line), SEXES);
    assert.ok(line.block._slots.every((s) => s.gridRow != null && s.gridRow >= 0));
    assert.deepStrictEqual(rowStrips(bar), ['(missing)', ...SEXES]);
    assert.deepStrictEqual(rowStrips(drop), SEXES);
  } finally {
    line.env.close(); bar.env.close(); drop.env.close();
  }
});

test('show_n counts the rows on the strips', () => {
  const r = drawGrid({ show_n: 'facet' });
  try {
    const rows = rowsOf(DATA).filter((x) => !missing(x.SEX) && !missing(x.ARM));
    const n = (col, lv) => rows.filter((x) => String(x[col]) === lv).length;
    const text = (cls) => Array.from(r.block.chartGrid.querySelectorAll('.' + cls)).map((e) => e.textContent);
    assert.deepStrictEqual(text('vz-chart-strip--col'), ARMS.map((a) => `${a} (${n('ARM', a)})`));
    assert.deepStrictEqual(text('vz-chart-strip--row'), SEXES.map((x) => `${x} (${n('SEX', x)})`));
  } finally {
    r.env.close();
  }
});

// The axes a panel was told to leave unlabelled, as "xy", "x", "y" or "".
const hidden = (s) => {
  const o = s.outerHidden;
  return o ? (o.x ? 'x' : '') + (o.y ? 'y' : '') : '';
};

test('a grid labels its outer axes only: y on the first column, x on the lowest row', () => {
  const r = drawGrid({ facet_scales: 'free_y' });
  try {
    const slots = r.block._slots;
    const firstCol = new Map(), lastRow = new Map();
    for (const s of slots) {
      firstCol.set(s.gridRow, Math.min(firstCol.get(s.gridRow) ?? Infinity, s.gridCol));
      lastRow.set(s.gridCol, Math.max(lastRow.get(s.gridCol) ?? -Infinity, s.gridRow));
    }
    for (const s of slots) {
      const want = (s.gridRow !== lastRow.get(s.gridCol) ? 'x' : '') + (s.gridCol !== firstCol.get(s.gridRow) ? 'y' : '');
      assert.strictEqual(hidden(s), want, `panel ${s.gridRow}/${s.gridCol}`);
      const off = s.chart.log.filter((e) => e.op === 'setOption' && e.option &&
        ((e.option.xAxis && e.option.xAxis[0].axisLabel && e.option.xAxis[0].axisLabel.show === false) ||
         (e.option.yAxis && e.option.yAxis[0].axisLabel && e.option.yAxis[0].axisLabel.show === false)));
      assert.strictEqual(off.length > 0, want !== '');
    }
    assert.ok(slots.some((s) => hidden(s) !== ''));
  } finally {
    r.env.close();
  }
});

test('horizontal bars, free: categories (y) differ by column, so every panel keeps its y labels', () => {
  const r = h.draw({ ...BASE, type: 'bar', category: 'AETERM', func: 'count', orientation: 'horizontal',
                     facet: 'ARM', facet_row: 'SEX', facet_scales: 'free' }, DATA, F.columns,
                   { width: 900, keepOpen: true });
  try {
    assert.ok(r.block._slots.every((s) => !hidden(s).includes('y')));
    // Within a column every panel lists the column's categories.
    const byCol = new Map();
    for (const s of r.block._slots) {
      if (!byCol.has(s.gridCol)) byCol.set(s.gridCol, new Set());
      byCol.get(s.gridCol).add(JSON.stringify(s.chart.getOption().yAxis[0].data));
    }
    for (const cats of byCol.values()) assert.strictEqual(cats.size, 1);
  } finally {
    r.env.close();
  }
});

test('without facet_row no axis is unlabelled', () => {
  const r = h.draw({ ...LINE }, F.data, F.columns, { width: 900, keepOpen: true });
  try {
    assert.ok(r.block._slots.every((s) => s.outerHidden === null));
  } finally {
    r.env.close();
  }
});

test('a shared value axis lands on a nice step', () => {
  assert.deepStrictEqual(plain(C.axes.niceShared(0, 51)), [0, 60, 10]);
  assert.deepStrictEqual(plain(C.axes.niceShared(0, 50)), [0, 50, 10]);
  assert.deepStrictEqual(plain(C.axes.niceShared(-5, 42)), [-10, 50, 10]);
  assert.deepStrictEqual(plain(C.axes.niceShared(0.1, 0.9)), [0, 1, 0.2]);
});

test('the gear words panel scales by row and column in a grid', () => {
  const host = C.gear.host({ derived: {}, columns: F.columns });
  const named = host.choices('facet_scales', { facet: 'ARM', facet_row: 'SEX' });
  assert.deepStrictEqual(plain(named).map((c) => c.label),
                         ['Shared across panels', 'Free per row and column', 'Free per row']);
  assert.strictEqual(host.choices('facet_scales', { facet: 'ARM' }), null);
});

test('a panel without x labels gives back their gutter: smaller bottom margin, shorter panel', () => {
  const r = h.draw({ ...BASE, type: 'bar', category: 'AETERM', func: 'count', facet: 'ARM',
                     facet_row: 'SEX' }, DATA, F.columns, { width: 900, keepOpen: true });
  try {
    const slots = r.block._slots;
    const inner = slots.filter((s) => s.outerHidden && s.outerHidden.x);
    const outer = slots.filter((s) => !(s.outerHidden && s.outerHidden.x));
    assert.ok(inner.length && outer.length);
    for (const s of inner) {
      const g = s.chart.log.filter((e) => e.op === 'setOption' && e.option && e.option.grid &&
                                   e.option.grid.bottom === 10);
      assert.strictEqual(g.length, 1);
    }
    const px = (s) => parseFloat(s.chartDiv.style.height);
    assert.ok(Math.max(...inner.map(px)) < Math.min(...outer.map(px)));
  } finally {
    r.env.close();
  }
});
