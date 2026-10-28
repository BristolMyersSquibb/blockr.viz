/* `label` on the aggregated marks (bar, pie, treemap) and the spacing of
 * names written on the marks (point labels, the names of scatter clouds). */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure, measure } = require('./chart/load');

const C = loadPure({ roles: true });

const COLUMNS = [
  { name: 'G', class: 'string' }, { name: 'K', class: 'string' },
  { name: 'L', class: 'string' }, { name: 'V', class: 'number' }
];
// One label per G, except c, whose rows disagree.
const ROWS = [
  { G: 'a', K: 'x', L: 'Alpha', V: 4 }, { G: 'a', K: 'y', L: 'Alpha', V: 2 },
  { G: 'b', K: 'x', L: 'Beta', V: 3 }, { G: 'b', K: 'y', L: null, V: 5 },
  { G: 'c', K: 'x', L: 'C1', V: 1 }, { G: 'c', K: 'y', L: 'C2', V: 6 }
];
const plain = (v) => JSON.parse(JSON.stringify(v));

// A bar lies horizontal: its category G is y, its measure V is x. The other
// types take G as x and V as y.
const agg = (cfg) => {
  const bar = cfg.type === 'bar';
  const c = C.model.settings({ drill: 'auto', func: 'sum', sort_by: 'alpha', sort_dir: 'asc',
                               label: 'L', ...(bar ? { y: 'G', x: 'V', orientation: 'horizontal' }
                                                    : { x: 'G', y: 'V' }), ...cfg }, {}, COLUMNS);
  const m = C.model.aggregated({ rows: ROWS, ix: new C.RowIndex(), columns: COLUMNS, cfg: c });
  const o = C.option.aggregated(m, { cfg: c, columns: COLUMNS, ink: C.INK_DEFAULT, theme: null,
                                     measure, widths: [600] });
  return { m, opt: o.panels[0].option };
};
const fmt = (s, i, value) => s.label.formatter({ value, dataIndex: i });

test('a bar writes its label at its end, where its rows agree on one', () => {
  const { opt } = agg({ type: 'bar' });
  const s = opt.series[0];
  assert.strictEqual(s.label.show, true);
  assert.strictEqual(s.label.position, 'outside');
  assert.deepStrictEqual(['a', 'b', 'c'].map((g, i) => fmt(s, i, s.data[i])),
                         ['Alpha', 'Beta', '']);
  // Headroom past the longest bar, as value labels get.
  const valAxis = opt.xAxis.type === 'value' ? opt.xAxis : opt.yAxis;
  assert.deepStrictEqual(plain(valAxis.boundaryGap), [0, '12%']);
});

test('with values on, the label comes first', () => {
  const { opt } = agg({ type: 'bar', value_labels: true });
  const s = opt.series[0];
  assert.strictEqual(fmt(s, 0, 6), 'Alpha · 6');
  assert.strictEqual(fmt(s, 2, 7), '7');
});

test('a grouped bar labels each bar by its own cell', () => {
  const { opt } = agg({ type: 'bar', color: 'K', position: 'dodge' });
  const [x, y] = opt.series;
  assert.deepStrictEqual([0, 1, 2].map((i) => fmt(x, i, 1)), ['Alpha', 'Beta', 'C1']);
  assert.deepStrictEqual([0, 1, 2].map((i) => fmt(y, i, 1)), ['Alpha', '', 'C2']);
});

test('a stack writes the labels inside its segments, and keeps its totals', () => {
  const { opt } = agg({ type: 'bar', color: 'K', orientation: 'horizontal',
                        value_labels: true });
  const lab = opt.series.find((s) => s.id === '__label__');
  assert.ok(lab, 'no label series');
  assert.strictEqual(lab.silent, true);
  const texts = plain(lab.data).map((d) => [d[1], d[2]]);
  assert.ok(texts.some((d) => d[0] === 'a' && d[1] === 'Alpha'));
  assert.ok(!texts.some((d) => d[0] === 'b' && d[1] == null));
  // The total still rides the outermost segment.
  const y = opt.series.find((s) => s.name === 'y');
  assert.ok(y.data.some((d) => d && d.label && d.label.show && /\d/.test(d.label.formatter)));
});

test('a waterfall and a boxplot draw no label', () => {
  assert.strictEqual(agg({ type: 'waterfall' }).m.markLabels, null);
  assert.strictEqual(agg({ type: 'boxplot' }).m.markLabels, null);
});

test('pie and treemap write the label in place of the slice name', () => {
  const pie = agg({ type: 'pie' }).opt.series[0];
  const byName = Object.fromEntries(pie.data.map((d) => [d.name, d]));
  assert.strictEqual(pie.label.formatter({ name: 'a', data: byName.a }), 'Alpha');
  // c's rows disagree: its name stays.
  assert.strictEqual(pie.label.formatter({ name: 'c', data: byName.c }), 'c');
  const tm = agg({ type: 'treemap' }).opt.series[0];
  const tb = Object.fromEntries(tm.data.map((d) => [d.name, d]));
  assert.strictEqual(tm.label.formatter({ name: 'b', value: 8, data: tb.b }), 'Beta\n8');
});

// -- spacing ------------------------------------------------------------------

const P = C.option.pointParts;
const rectAt = (x, y) => ({ x, y, width: 0, height: 0 });

test('names that would cover each other move apart a line, across series', () => {
  const lay = P.labelSpacer('inside', 0);
  const lab = (x, y) => ({ x, y: y - 7, width: 40, height: 14 });
  // Two clouds whose centres sit 4px apart, each name in its own series.
  const a = lay({ seriesIndex: 3, dataIndex: 0, rect: rectAt(100, 100), labelRect: lab(80, 100) });
  const b = lay({ seriesIndex: 4, dataIndex: 0, rect: rectAt(105, 104), labelRect: lab(85, 104) });
  assert.strictEqual(a.dy, 0);
  assert.ok(Math.abs(b.dy) >= 10, `moved ${b.dy}`);
  // A new layout pass (a resize) starts again from the first name.
  const a2 = lay({ seriesIndex: 3, dataIndex: 0, rect: rectAt(100, 100), labelRect: lab(80, 100) });
  assert.strictEqual(a2.dy, 0);
  // Names far apart stay put.
  const c = lay({ seriesIndex: 4, dataIndex: 0, rect: rectAt(300, 300), labelRect: lab(280, 300) });
  assert.strictEqual(c.dy, 0);
});

test('scatter cloud names share one spacer', () => {
  const rows = [];
  for (let i = 0; i < 10; i++) {
    rows.push({ X: i, Y: i, K: 'one' }, { X: i + 0.1, Y: i + 0.1, K: 'two' });
  }
  const cols = [{ name: 'X', class: 'number' }, { name: 'Y', class: 'number' },
                { name: 'K', class: 'string' }];
  const c = C.model.settings({ type: 'scatter', x: 'X', y: 'Y', color: 'K', legend: 'marks',
                               drill: 'auto' }, {}, cols);
  const m = C.model.individual({ rows, ix: new C.RowIndex(), columns: cols, cfg: c });
  const opt = C.option.individual(m, { cfg: c, columns: cols, ink: C.INK_DEFAULT, theme: null,
                                       measure, widths: [600] }).panels[0].option;
  const names = opt.series.filter((s) => String(s.id || '').startsWith('__legend_'));
  assert.strictEqual(names.length, 2);
  assert.strictEqual(typeof names[0].labelLayout, 'function');
  assert.strictEqual(names[0].labelLayout, names[1].labelLayout);
});

test('a group name keeps clear of a point label placed before it', () => {
  const room = { placed: [], lastS: -1, lastD: -1 };
  const labels = P.labelSpacer('right', 5, room);
  const names = P.labelSpacer('inside', 0, room);
  const lab = (x, y) => ({ x, y: y - 7, width: 40, height: 14 });
  assert.strictEqual(labels({ seriesIndex: 2, dataIndex: 0, rect: rectAt(100, 100),
                              labelRect: lab(105, 100) }).dy, 0);
  const n = names({ seriesIndex: 3, dataIndex: 0, rect: rectAt(110, 102), labelRect: lab(95, 102) });
  assert.ok(Math.abs(n.dy) >= 10, `moved ${n.dy}`);
});
