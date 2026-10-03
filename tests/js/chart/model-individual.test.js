/* model-individual.js: series, colours, legend, facets, the line cap,
 * clicks (D6, D7, D3, missing keys), the brush and the hover picker's
 * interpolation, from small frames. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure, plain } = require('./load');

const C = loadPure({ roles: true });

const cat = (name, extra) => ({ name, type: 'categorical', ...extra });
const num = (name, extra) => ({ name, type: 'numeric', ...extra });
const COLUMNS = [cat('ID'), cat('ARM', { label: 'Arm' }), cat('SEX'), cat('VIS', { levels: ['Base', 'W2', 'W4'] }),
                 num('DAY', { label: 'Study Day' }), num('VAL'), num('LO'), num('HI')];
const ROWS = [
  { ID: 'S01', ARM: 'B', SEX: 'M', VIS: 'W2', DAY: 14, VAL: 5, LO: 4, HI: 6 },
  { ID: 'S01', ARM: 'B', SEX: 'M', VIS: 'Base', DAY: 1, VAL: 3, LO: 2, HI: 4 },
  { ID: 'S02', ARM: 'A', SEX: 'F', VIS: 'Base', DAY: 2, VAL: 7, LO: 6, HI: 8 },
  { ID: 'S02', ARM: 'A', SEX: 'F', VIS: 'W4', DAY: 28, VAL: 9, LO: null, HI: 10 },
  { ID: 'S03', ARM: null, SEX: 'F', VIS: 'W2', DAY: 15, VAL: 6, LO: 5, HI: 7 },
  { ID: null, ARM: 'A', SEX: null, VIS: 'W4', DAY: 30, VAL: null, LO: 1, HI: 2 }
];
const BASE = { chart_type: 'scatter', x: 'DAY', y: 'VAL', drill: 'auto' };

const model = (cfg, rows = ROWS) => C.model.individual({ rows, ix: new C.RowIndex(),
                                                         columns: COLUMNS, cfg: { ...BASE, ...cfg } });

test('an unsplit scatter: one series of the rows with an x and a y', () => {
  const m = model({});
  assert.strictEqual(m.family, 'individual');
  assert.strictEqual(m.panels.length, 1);
  const s = m.panels[0].series;
  assert.strictEqual(s.length, 1);
  assert.strictEqual(s[0].level, undefined);
  assert.deepStrictEqual(plain(s[0].pts), [[14, 5], [1, 3], [2, 7], [28, 9], [15, 6]]);
  assert.strictEqual(s[0].rows.length, 5);
  assert.strictEqual(s[0].color, C.BLOCKR_PALETTE[0]);
  assert.strictEqual(m.legend, null);
});

test('without x or y the model is empty', () => {
  assert.match(model({ x: '' }).empty, /Select X and Y columns/);
});

test('tooltip fields ride on the points, less the ones the tooltip shows', () => {
  const m = model({ color: 'ARM', tt_fields: ['SEX', 'DAY', 'ARM', 'null'] });
  assert.deepStrictEqual(plain(m.ttFields), ['SEX']);
  assert.deepStrictEqual(plain(m.panels[0].series[1].pts[0]), [2, 7, 'F']);
});

test('a colour split: a level per value, a missing colour is the "" level', () => {
  const m = model({ color: 'ARM' });
  assert.deepStrictEqual(plain(m.levels), ['', 'A', 'B']);
  assert.deepStrictEqual(plain(m.panels[0].series.map((s) => s.level)), ['', 'A', 'B']);
  assert.deepStrictEqual(plain(m.panels[0].series[0].pts), [[15, 6]]);
  assert.deepStrictEqual(plain(m.levelColors), plain(C.BLOCKR_PALETTE.slice(0, 3)));
  assert.deepStrictEqual(plain(m.legend), { col: 'ARM', items: [
    { name: '', color: C.BLOCKR_PALETTE[0] }, { name: 'A', color: C.BLOCKR_PALETTE[1] },
    { name: 'B', color: C.BLOCKR_PALETTE[2] }] });
  assert.strictEqual(m.byColor, false);
});

test('a series apart from the colour: a series takes its colour value\'s colour', () => {
  const m = model({ series: 'ID', color: 'ARM',
                    scales: { var: 'ARM', color: { B: '#123456' } } });
  // '' (the missing ID) has colour A; S01 B (scale); S02 A; S03 missing.
  assert.deepStrictEqual(plain(m.levels), ['', 'S01', 'S02', 'S03']);
  const P = C.BLOCKR_PALETTE;
  assert.deepStrictEqual(plain(m.levelColors), [P[0], '#123456', P[0], P[1]]);
  assert.strictEqual(m.byColor, true);
  // Chips: the colour's values, sorted; scale first, then the cycled colour.
  assert.deepStrictEqual(plain(m.legend.items), [
    { name: '', color: P[1] }, { name: 'A', color: P[0] }, { name: 'B', color: '#123456' }]);
});

test('categorical x: factor levels order the axis, a line runs in that order', () => {
  const m = model({ chart_type: 'line', x: 'VIS', series: 'ID' });
  assert.strictEqual(m.x.type, 'category');
  assert.deepStrictEqual(plain(m.x.cats), ['Base', 'W2', 'W4']);
  const s01 = m.panels[0].series.find((s) => s.level === 'S01');
  assert.deepStrictEqual(plain(s01.pts), [['Base', 3], ['W2', 5]]);
  // The rows line up with the points; the data-order rows stay for error bars.
  assert.deepStrictEqual(plain(s01.rows.map((r) => r.VIS)), ['Base', 'W2']);
  assert.deepStrictEqual(plain(s01.src.map((r) => r.VIS)), ['W2', 'Base']);
});

test('a numeric line is drawn in x order, a scatter in row order', () => {
  assert.deepStrictEqual(plain(model({ chart_type: 'line' }).panels[0].series[0].pts.map((p) => p[0])),
                         [1, 2, 14, 15, 28]);
  assert.deepStrictEqual(plain(model({}).panels[0].series[0].pts.map((p) => p[0])),
                         [14, 1, 2, 28, 15]);
});

test('facets: sorted levels, a panel each; the missing facet\'s panel is empty', () => {
  const m = model({ facet: 'SEX', count_on: 'facet', count_col: 'ID' });
  assert.deepStrictEqual(plain(m.panels.map((p) => p.facet)), ['', 'F', 'M']);
    // No patient has a missing sex, so that strip has no count.
  assert.deepStrictEqual(plain(m.panels.map((p) => p.label)), ['', 'F (2)', 'M (1)']);
  assert.deepStrictEqual(plain(m.panels.map((p) => p.rows.length)), [0, 3, 2]);
  assert.strictEqual(m.single, false);
});

test('a line with more than 1000 series draws the first 1000 and says so', () => {
  const rows = [];
  for (let i = 0; i < 1003; i++) rows.push({ ID: 'P' + String(i).padStart(4, '0'), DAY: 1, VAL: i });
  const m = model({ chart_type: 'line', series: 'ID' }, rows);
  assert.strictEqual(m.levels.length, 1000);
  assert.strictEqual(m.seriesCount, 1000);
  assert.match(m.capMessage, /^Showing first 1000 of 1003 series \(ID\)/);
  assert.strictEqual(m.lineOpacity, 0.15);
  assert.strictEqual(m.symbol, 'none');
  // A scatter is not capped.
  assert.strictEqual(model({ series: 'ID' }, rows).levels.length, 1003);
});

test('a band keeps rows for clicks and draws no row series', () => {
  const m = model({ chart_type: 'band', color: 'ARM', facet: 'SEX' });
  assert.deepStrictEqual(plain(m.panels.map((p) => p.series.length)), [0, 0, 0]);
  assert.strictEqual(m.panels[1].rows.length, 3);
});

// -- Clicks ------------------------------------------------------------------

const click = (cfg, params, facet = '__all__', focusName) => {
  const m = model(cfg);
  const c = { ...BASE, ...cfg };
  return plain(C.model.individualClick(m, c, C.model.panelOf(m, facet), params, focusName));
};

test('D6: a point selects its series; colour alone does not widen it', () => {
  assert.deepStrictEqual(click({ series: 'ID', color: 'ARM' }, { seriesName: 'S02', value: [2, 7] }),
                         { filters: { ID: ['S02'] } });
  assert.deepStrictEqual(click({ color: 'ARM' }, { seriesName: 'A', value: [2, 7] }),
                         { range: { x_col: 'DAY', y_col: 'VAL', x_range: [2, 2], y_range: [7, 7] } });
  assert.deepStrictEqual(click({}, { value: [14, 5] }),
                         { range: { x_col: 'DAY', y_col: 'VAL', x_range: [14, 14], y_range: [5, 5] } });
});

test('a missing series is a key too, sent as null; the facet joins it (D3)', () => {
  assert.deepStrictEqual(click({ series: 'ID' }, { seriesName: '', value: [30, null] }),
                         { filters: { ID: [null] } });
  assert.deepStrictEqual(click({ series: 'ID', facet: 'SEX' }, { seriesName: 'S02', value: [2, 7] }, 'F'),
                         { filters: { ID: ['S02'], SEX: ['F'] } });
});

test('an explicit drill column: its values in the rows under the point', () => {
  assert.deepStrictEqual(click({ color: 'ARM', drill: 'ID' }, { seriesName: 'A', value: [2, 7] }),
                         { filters: { ID: ['S02'] } });
  // No row there in that series: nothing is sent.
  assert.strictEqual(click({ color: 'ARM', drill: 'ID' }, { seriesName: 'B', value: [2, 7] }), null);
});

test('a line: the hovered line before the hit test; drill off sends nothing', () => {
  const cfg = { chart_type: 'line', series: 'ID' };
  assert.deepStrictEqual(click(cfg, { seriesName: 'S03', value: [15, 6] }, '__all__', 'S01'),
                         { filters: { ID: ['S01'] } });
  assert.deepStrictEqual(click(cfg, { seriesName: 'S03', value: [15, 6] }),
                         { filters: { ID: ['S03'] } });
  assert.deepStrictEqual(click({ ...cfg, drill: 'ARM' }, { seriesName: 'S02', value: [2, 7] }),
                         { filters: { ARM: ['A'] } });
  assert.strictEqual(click({ ...cfg, drill: '' }, { seriesName: 'S03', value: [15, 6] }), null);
  // An unsplit line's point is a range, as a scatter's.
  assert.ok(click({ chart_type: 'line' }, { value: [15, 6] }).range);
});

test('D7: a coloured band selects its level; an uncoloured band is not clickable', () => {
  assert.deepStrictEqual(click({ chart_type: 'band', color: 'ARM' }, { seriesName: 'A', value: [0, 5] }),
                         { filters: { ARM: ['A'] } });
  assert.deepStrictEqual(click({ chart_type: 'band', color: 'ARM', facet: 'SEX' },
                               { seriesName: 'A', value: [0, 5] }, 'F'),
                         { filters: { ARM: ['A'], SEX: ['F'] } });
  assert.deepStrictEqual(click({ chart_type: 'band', color: 'ARM', drill: 'ID' },
                               { seriesName: 'A', value: [0, 5] }),
                         { filters: { ID: ['S02'] } });
  assert.strictEqual(click({ chart_type: 'band' }, { seriesName: undefined, value: [0, 5] }), null);
});

test('the keys of a line or band level: its level and the facet', () => {
  const m = model({ chart_type: 'line', series: 'ID', facet: 'SEX' });
  assert.deepStrictEqual(plain(C.model.individualKeys(m, 'F', 'S02')), { ID: 'S02', SEX: 'F' });
  assert.deepStrictEqual(plain(C.model.individualKeys(m, '', '')), { ID: '', SEX: '' });
  assert.deepStrictEqual(plain(C.model.individualKeys(model({}), '__all__', null)), {});
});

// -- Brush --------------------------------------------------------------------

test('a brush: the points\' x and y; with a drill column their rows, each (x, y) once', () => {
  const rows = [{ X: 1, Y: 5, ID: 'a' }, { X: 1, Y: 5, ID: 'b' }, { X: 2, Y: 6, ID: 'c' }];
  const cols = [num('X'), num('Y'), cat('ID')];
  const cfg = { chart_type: 'scatter', x: 'X', y: 'Y', drill: 'ID' };
  const m = C.model.individual({ rows, ix: new C.RowIndex(), columns: cols, cfg });
  const series = [{ data: m.panels[0].series[0].pts }];
  const sel = (idx) => idx.map((dataIndex) => ({ seriesIndex: 0, dataIndex }));
  const r = C.model.individualBrush(m, cfg, m.panels[0], series, sel([0, 1, 2, 9999, -1]));
  assert.deepStrictEqual(plain(r.xVals), [1, 1, 2]);
  assert.deepStrictEqual(plain(r.rows.map((x) => x.ID)), ['a', 'b', 'c']);
  assert.strictEqual(r.drill, 'ID');
  const none = C.model.individualBrush(m, { ...cfg, drill: 'auto' }, m.panels[0], series, sel([9999]));
  assert.deepStrictEqual(plain(none), { xVals: [], yVals: [], rows: [], drill: null, levels: [] });
});

// D9: a brush takes the brushed points, never their colour; D10: in a facet
// panel the panel's key rides with it.
const brushOf = (cfg, idx, facet = '__all__') => {
  const c = { ...BASE, ...cfg };
  const m = model(cfg);
  const panel = C.model.panelOf(m, facet);
  const series = panel.series.map((s) => ({ name: s.level, data: s.pts }));
  const sel = idx.map(([seriesIndex, dataIndex]) => ({ seriesIndex, dataIndex }));
  return plain(C.model.brushSelection(m, c, panel,
    C.model.individualBrush(m, c, panel, series, sel)));
};

test('D9: a brush in a coloured scatter is its range, not the colour levels', () => {
  const r = brushOf({ color: 'ARM' }, [[1, 0], [2, 0]]);
  assert.ok(r.range);
  assert.strictEqual(r.filters, undefined);
  assert.deepStrictEqual(r.range.x_col, 'DAY');
});

test('D9: with a series, a brush takes the series of the brushed points', () => {
  const r = brushOf({ series: 'ID', color: 'ARM' }, [[1, 0], [2, 0]]);
  assert.deepStrictEqual(r, { filters: { ID: ['S01', 'S02'] } });
});

test('D9: an explicit drill column still reads the rows at the brushed points', () => {
  assert.deepStrictEqual(brushOf({ color: 'ARM', drill: 'ID' }, [[1, 0]]).filters, { ID: ['S02'] });
});

test('D10: a brush and a point in a facet panel carry the panel', () => {
  const r = brushOf({ facet: 'SEX' }, [[0, 0]], 'F');
  assert.deepStrictEqual(r.filters, { SEX: ['F'] });
  assert.ok(r.range);
  const p = click({ facet: 'SEX' }, { value: [2, 7] }, 'F');
  assert.deepStrictEqual(p, { range: { x_col: 'DAY', y_col: 'VAL', x_range: [2, 2], y_range: [7, 7] },
                              filters: { SEX: ['F'] } });
});

test('interpYAtX: between points, null outside; a category x by its position', () => {
  const data = [[0, 10], [10, 20], [20, 0]];
  assert.strictEqual(C.model.interpYAtX(data, 5, null), 15);
  assert.strictEqual(C.model.interpYAtX(data, 15, null), 10);
  assert.strictEqual(C.model.interpYAtX(data, 25, null), null);
  const order = new Map([['Base', 0], ['W2', 1]]);
  assert.strictEqual(C.model.interpYAtX([['Base', 2], ['W2', 4]], 0.5, order), 3);
});

test('the model reads only the config fields it lists', () => {
  const configs = [
    {}, { color: 'ARM', facet: 'SEX', count_on: 'both', count_col: 'ID', tt_fields: ['ID'] },
    { chart_type: 'line', series: 'ID', color: 'ARM', scales: { var: 'ARM', color: {} } },
    { chart_type: 'band', color: 'ARM', palette: ['#000000'] }, { x: 'VIS', chart_type: 'line' }
  ];
  for (const cfg of configs) {
    const read = new Set();
    const spy = new Proxy({ ...BASE, ...cfg }, { get(t, k) { read.add(k); return t[k]; } });
    C.model.individual({ rows: ROWS, ix: new C.RowIndex(), columns: COLUMNS, cfg: spy });
    const extra = [...read].filter((k) => typeof k === 'string' &&
                                          !C.model.individual.FIELDS.includes(k));
    assert.deepStrictEqual(extra, [], `${JSON.stringify(cfg)} reads ${extra}`);
  }
});
