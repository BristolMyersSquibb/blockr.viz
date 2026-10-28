/* `highlight`: one column and some of its values. Marks holding a
 * highlighted row keep their colour, the rest are grey and drawn first;
 * the palette and the legend cover the highlighted levels only. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure, plain, measure } = require('./load');

const C = loadPure({ roles: true });
// R's derived: none in these tests.
const derived = {};
const MUTED = C.mutedMark(C.INK_DEFAULT);
const [BLUE, ORANGE] = C.BLOCKR_PALETTE;

const cat = (name, extra) => ({ name, class: 'string', ...extra });
const num = (name) => ({ name, class: 'number' });

// Countries, their region and two periods: the shape of a relative PPP chart.
const COLUMNS = [cat('country'), cat('region'), cat('period'), num('x'), num('y')];
const ROWS = [
  { country: 'CH', region: 'Europe', period: '30y', x: -1.9, y: -1.5 },
  { country: 'CH', region: 'Europe', period: '5y', x: -2.5, y: -2.0 },
  { country: 'DE', region: 'Europe', period: '30y', x: 0.1, y: 0.3 },
  { country: 'DE', region: 'Europe', period: '5y', x: 1.0, y: 0.8 },
  { country: 'JP', region: 'Asia', period: '30y', x: -2.0, y: 1.0 },
  { country: 'JP', region: 'Asia', period: '5y', x: 0.5, y: 4.0 },
  { country: 'BR', region: 'Americas', period: '30y', x: 6.0, y: 5.5 },
  { country: 'BR', region: 'Americas', period: '5y', x: 4.0, y: 3.0 }
];

const look = (c) => ({ cfg: c, derived, columns: COLUMNS, ink: C.INK_DEFAULT, theme: null, measure,
                       widths: [600, 600] });

// `highlight` is a column and some of its values: H('country', 'CH').
const H = (col, ...values) => ({ [col]: values.map((value) => ({ value, label: null })) });

const bar = (cfg) => {
  const c = C.model.settings({ type: 'bar', category: 'country', value: 'x', orientation: 'horizontal',
                               func: 'sum', drill: 'auto', sort_by: 'alpha', sort_dir: 'asc',
                               ...cfg }, COLUMNS);
  const m = C.model.aggregated({ rows: ROWS, ix: new C.RowIndex(), columns: COLUMNS, cfg: c, derived });
  const o = C.option.aggregated(m, look(c));
  return { m, c, opt: o.panels[0].option };
};

const points = (cfg) => {
  const c = C.model.settings({ type: 'scatter', x: 'x', y: 'y', drill: 'auto', ...cfg }, COLUMNS);
  const m = C.model.individual({ rows: ROWS, ix: new C.RowIndex(), columns: COLUMNS, cfg: c, derived });
  const o = C.option.individual(m, look(c));
  return { m, c, opt: o.panels[0].option };
};

const colourOf = (d, s) => (d && typeof d === 'object' && d.itemStyle && d.itemStyle.color) ||
  s.itemStyle.color;

test('highlightOf reads the column and its values, on the types that draw it', () => {
  const of = (type, highlight) => C.highlightOf(C.model.settings({ type, highlight }, COLUMNS));
  assert.deepStrictEqual(plain(of('bar', H('country', 'CH'))), { country: ['CH'] });
  assert.deepStrictEqual(plain(of('line', H('country', 'CH'))), { country: ['CH'] });
  assert.strictEqual(of('bar', H('country')), null);
  assert.strictEqual(of('bar', {}), null);
  assert.strictEqual(of('pie', H('country', 'CH')), null);
});

test('the muted grey sits between the muted text and the surface', () => {
  assert.strictEqual(MUTED, '#c4c7cc');
  assert.strictEqual(C.mutedMark({ muted: 'rgb(138, 148, 163)', surface: '#191e26' }), '#464d58');
});

test('a bar: the highlighted bar keeps its colour, the others are grey', () => {
  const { opt } = bar({ highlight: H('country', 'CH') });
  const s = opt.series[0];
  const cats = plain(opt.yAxis.data);
  const byCat = Object.fromEntries(cats.map((g, i) => [g, colourOf(s.data[i], s)]));
  assert.deepStrictEqual(byCat, { BR: MUTED, CH: BLUE, DE: MUTED, JP: MUTED });
  // Its name on the axis is semibold.
  assert.strictEqual(opt.yAxis.axisLabel.formatter('CH', 0), '{hl|CH}');
  assert.strictEqual(opt.yAxis.axisLabel.formatter('DE', 1), 'DE');
  assert.strictEqual(opt.yAxis.axisLabel.rich.hl.fontWeight, 600);
});

test('a bar highlights by a column it does not map, through its rows', () => {
  const { opt } = bar({ highlight: H('region', 'Europe') });
  const s = opt.series[0];
  const cats = plain(opt.yAxis.data);
  const lit = cats.filter((g, i) => colourOf(s.data[i], s) !== MUTED);
  assert.deepStrictEqual(lit, ['CH', 'DE']);
});

test('a colour split: the palette and the legend cover the highlighted levels', () => {
  const { m, c, opt } = bar({ color: 'region', highlight: H('country', 'JP') });
  const asia = opt.series.find((s) => s.name === 'Asia');
  const europe = opt.series.find((s) => s.name === 'Europe');
  // Asia is the only highlighted level, so it takes the palette's first colour.
  assert.strictEqual(asia.itemStyle.color, BLUE);
  assert.strictEqual(europe.itemStyle.color, MUTED);
  assert.deepStrictEqual(plain(C.option.aggregatedLegend(m, c, derived).items), [{ name: 'Asia', color: BLUE }]);
});

test('without a highlight nothing changes', () => {
  const a = bar({ color: 'region' }).opt;
  const b = bar({ color: 'region', highlight: H('country') }).opt;
  assert.deepStrictEqual(plain(a.series.map((s) => s.itemStyle.color)),
                         plain(b.series.map((s) => s.itemStyle.color)));
});

test('a scatter: muted points first, then the highlighted ones, named', () => {
  const { m, opt } = points({ highlight: H('country', 'CH') });
  const marks = opt.series.filter((s) => s.type === 'scatter' && s.id == null);
  assert.deepStrictEqual(plain(marks.map((s) => [s.itemStyle.color, s.data.length, !!s.blockrMuted])),
                         [[MUTED, 6, true], [BLUE, 2, false]]);
  const lab = opt.series.find((s) => s.id === '__label__');
  assert.deepStrictEqual(plain(lab.data.map((d) => d[2])), ['CH', 'CH']);
  assert.strictEqual(lab.label.fontWeight, 600);
  assert.ok(m.panels[0].series[0].muted);
});

test('a scatter: a value on many points is not written on each', () => {
  const { opt } = points({ highlight: H('region', 'Europe') });
  assert.strictEqual(opt.series.find((s) => s.id === '__label__'), undefined);
});

test('a scatter coloured by another column keeps that colour on the highlighted points', () => {
  const { m, opt } = points({ color: 'period', highlight: H('country', 'CH') });
  const marks = opt.series.filter((s) => s.type === 'scatter' && s.id == null);
  // Both periods' muted points, then CH's 30y and 5y point.
  assert.deepStrictEqual(plain(marks.map((s) => [s.name, s.itemStyle.color])),
                         [['30y', MUTED], ['5y', MUTED], ['30y', BLUE], ['5y', ORANGE]]);
  assert.deepStrictEqual(plain(m.legend.items),
                         [{ name: '30y', color: BLUE }, { name: '5y', color: ORANGE }]);
});

test('a line: muted lines first, the highlighted one named at its end', () => {
  const { opt } = points({ type: 'line', x: 'x', y: 'y', series: 'country',
                           highlight: H('country', 'CH') });
  const lines = opt.series.filter((s) => s.type === 'line' && s.id == null && !s.silent);
  const last = lines[lines.length - 1];
  assert.strictEqual(last.name, 'CH');
  assert.strictEqual(last.itemStyle.color, BLUE);
  assert.strictEqual(last.endLabel.show, true);
  assert.ok(lines.slice(0, -1).every((s) => s.itemStyle.color === MUTED && !s.endLabel));
  assert.ok(opt.grid.right > 5);
});

test('a drill selection dims each part of a split level by its own points', () => {
  const { m, opt } = points({ color: 'period', highlight: H('country', 'CH') });
  const filters = { region: ['Asia'] };
  const lit = (keys, rows) => C.keys.markLit(keys, rows, filters);
  const patch = plain(C.option.individualPatch(m, m.panels[0], opt.series, lit, false, filters));
  const marks = opt.series.map((s, i) => [s, patch.series[i]])
    .filter(([s]) => s.type === 'scatter' && s.id == null);
  // Each part's data is its own points, so nothing shifts between them.
  for (const [s, p] of marks) assert.strictEqual(p.data.length, s.data.length);
  // CH is in Europe: its highlighted points dim.
  const ch = marks.filter(([s]) => !s.blockrMuted).map(([, p]) => p.data[0].itemStyle.opacity);
  assert.deepStrictEqual(plain(ch), [0.25, 0.25]);
});
