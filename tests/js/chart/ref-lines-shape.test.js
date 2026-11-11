/* `ref_lines` (reference lines read from columns, per panel) and `shape`
 * (a point's marker from a column) on the individual option
 * (option-individual.js, option-points.js, option-domains.js), the legend's
 * shape chips (legend.js) and the gear rows. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure, plain, measure } = require('./load');
const D = require('./drive');

const C = loadPure({ roles: true });

const cat = (name, extra) => ({ name, class: 'string', ...extra });
const num = (name, extra) => ({ name, class: 'number', ...extra });
const COLUMNS = [cat('SUBJ'), cat('PARAM'), cat('GR', { levels: ['GRADE 0', 'GRADE 1', 'GRADE 2'] }),
                 num('DAY'), num('VAL'), num('LLN'), num('ULN', { label: 'Upper limit' }), num('NONE')];
const ROWS = [
  { SUBJ: 'S1', PARAM: 'ALT', GR: 'GRADE 1', DAY: 1, VAL: 20, LLN: 10, ULN: 40, NONE: null },
  { SUBJ: 'S1', PARAM: 'ALT', GR: 'GRADE 0', DAY: 2, VAL: 25, LLN: 10, ULN: 42, NONE: null },
  { SUBJ: 'S2', PARAM: 'ALT', GR: null, DAY: 1, VAL: 30, LLN: 10, ULN: 40, NONE: null },
  { SUBJ: 'S2', PARAM: 'ALT', GR: 'GRADE 2', DAY: 2, VAL: 35, LLN: 10, ULN: 42, NONE: null },
  { SUBJ: 'S1', PARAM: 'CRP', GR: 'GRADE 0', DAY: 1, VAL: 1, LLN: null, ULN: 5, NONE: null },
  { SUBJ: 'S1', PARAM: 'CRP', GR: 'GRADE 0', DAY: 2, VAL: 3, LLN: null, ULN: 5, NONE: null }
];
const BASE = { type: 'line', x: 'DAY', y: 'VAL', series: 'SUBJ', drill: 'auto' };

const build = (cfg, rows = ROWS, look = {}) => {
  const c = C.model.settings({ ...BASE, ...cfg }, COLUMNS);
  const m = C.model.individual({ rows, ix: new C.RowIndex(), columns: COLUMNS, cfg: c, derived: {} });
  const o = C.option.individual(m, { cfg: c, derived: {}, columns: COLUMNS, ink: C.INK_DEFAULT, theme: null,
                                     measure, widths: [600, 600, 600], ...look });
  return { m, o, c };
};
const refLines = (opt) => {
  const s = opt.series.find((x) => x.markLine);
  return s ? s.markLine.data.filter((d) => d.yAxis != null && typeof d.label.formatter === 'function') : [];
};

// -- ref_lines -----------------------------------------------------------------

test('ref_lines: a line per column at its mean among the panel\'s rows, per facet panel', () => {
  const { o } = build({ facet: 'PARAM', ref_lines: ['LLN', 'ULN', 'NONE', 'ABSENT'] });
  const [alt, crp] = o.panels.map((p) => refLines(p.option));
  // ALT: LLN 10, ULN the mean of 40, 42, 40, 42. NONE is all missing, ABSENT not in the data.
  assert.deepStrictEqual(plain(alt.map((d) => d.yAxis)), [10, 41]);
  // CRP has no LLN value: only ULN.
  assert.deepStrictEqual(plain(crp.map((d) => d.yAxis)), [5]);
});

test('ref_lines: labelled with the column label, else its name; dashed, in their colour', () => {
  const { o } = build({ ref_lines: ['LLN', 'ULN'], ref_line_colors: { LLN: '#4DAF4A' } });
  const [lln, uln] = refLines(o.panels[0].option);
  assert.strictEqual(lln.label.formatter(), 'LLN');
  assert.strictEqual(uln.label.formatter(), 'Upper limit');
  assert.strictEqual(lln.lineStyle.type, 'dashed');
  assert.strictEqual(lln.lineStyle.color, '#4DAF4A');
  // Without an entry: the reference line colour value_lines use.
  assert.strictEqual(uln.lineStyle.color, C.INK_DEFAULT.danger);
});

test('ref_lines: the panel\'s y axis takes in a line beyond its data', () => {
  const { o } = build({ facet: 'PARAM', ref_lines: ['ULN'] });
  const crp = o.panels[1].option.yAxis;
  const y = Array.isArray(crp) ? crp[0] : crp;
  // Data 1..3, ULN at 5: the top moves a step past the line; the bottom stays auto.
  assert.strictEqual(y.max({ min: 1, max: 3 }), 5.5);
  assert.strictEqual(y.min({ min: 1, max: 3 }), undefined);
  // A line inside the data leaves both ends auto.
  assert.strictEqual(y.max({ min: 0, max: 10 }), undefined);
  // Without ref_lines the axis is ECharts' own.
  const none = build({ facet: 'PARAM' }).o.panels[1].option.yAxis;
  assert.strictEqual((Array.isArray(none) ? none[0] : none).max, undefined);
});

test('ref_lines draw nothing on a type they do not apply to, or without a value', () => {
  assert.deepStrictEqual(refLines(build({ ref_lines: ['NONE'] }).o.panels[0].option), []);
  const bar = C.option.domainParts.panelRefLines({ type: 'bar', ref_lines: ['ULN'] }, { rows: ROWS });
  assert.deepStrictEqual(plain(bar), []);
});

// -- shape ---------------------------------------------------------------------

const shapeSeries = (opt) => opt.series.filter((s) => /^__shape_/.test(String(s.id || '')));

test('shape on a line: an overlay per marker over the points, in each line\'s colour', () => {
  const { o, m } = build({ shape: 'GR' });
  // Factor levels in their order.
  assert.deepStrictEqual(plain(m.shape.levels), ['GRADE 0', 'GRADE 1', 'GRADE 2']);
  const opt = o.panels[0].option;
  const sh = shapeSeries(opt);
  assert.deepStrictEqual(plain(sh.map((s) => s.symbol)), ['circle', 'triangle', 'diamond']);
  assert.ok(sh.every((s) => s.silent && s.blockrShape));
  // A missing grade is a circle: GRADE 0 x3 (CRP and ALT) + the missing one.
  assert.strictEqual(sh[0].data.length, 4);
  assert.deepStrictEqual(plain(sh[1].data.map((d) => d.value)), [[1, 20]]);
  const s1 = opt.series.find((s) => s.name === 'S1' && s.type === 'line');
  assert.strictEqual(sh[1].data[0].itemStyle.color, s1.itemStyle.color);
  // The line itself draws no marker under them.
  assert.strictEqual(s1.symbol, 'none');
});

test('shape keeps the points of a crowd of lines visible', () => {
  const rows = [];
  for (let i = 0; i < 60; i++) {
    for (const d of [1, 2]) rows.push({ SUBJ: 'S' + i, PARAM: 'ALT', GR: d === 1 ? 'GRADE 0' : 'GRADE 1', DAY: d, VAL: i + d });
  }
  const { m, o } = build({ shape: 'GR' }, rows);
  assert.strictEqual(m.symbol, 'none');
  assert.ok(m.crowd);
  const sh = shapeSeries(o.panels[0].option);
  assert.deepStrictEqual(plain(sh.map((s) => [s.symbol, s.data.length])), [['circle', 60], ['triangle', 60]]);
  assert.ok(sh.every((s) => s.symbolSize > 0));
});

test('shape on a scatter: each point\'s own marker, no overlay', () => {
  const { o } = build({ type: 'scatter', series: null, shape: 'GR' });
  const opt = o.panels[0].option;
  assert.strictEqual(shapeSeries(opt).length, 0);
  const s = opt.series[0];
  const syms = s.data.map((_, i) => s.symbol(null, { dataIndex: i }));
  assert.deepStrictEqual(plain(syms), ['triangle', 'circle', 'circle', 'diamond', 'circle', 'circle']);
});

test('shape: the legend lists the levels with their markers, after the colour chips', () => {
  const { m } = build({ shape: 'GR', color: 'PARAM', series: 'SUBJ' });
  const spec = C.option.individualLegend(m);
  assert.strictEqual(spec.col, 'PARAM');
  assert.deepStrictEqual(plain(spec.shapes), {
    col: 'GR',
    items: [{ name: 'GRADE 0', symbol: 'circle' }, { name: 'GRADE 1', symbol: 'triangle' },
            { name: 'GRADE 2', symbol: 'diamond' }]
  });
  // Without a colour the band still names the shapes.
  const alone = C.option.individualLegend(build({ shape: 'GR' }).m);
  assert.strictEqual(alone.col, null);
  assert.strictEqual(alone.shapes.items.length, 3);
});

test('shape: a selection fades the markers of the lines it leaves out', () => {
  const { m, o } = build({ shape: 'GR' });
  const panel = m.panels[0];
  const drawn = o.panels[0].option.series;
  const lit = (keys) => keys.SUBJ === 'S1';
  const p = C.option.individualPatch(m, panel, drawn, lit, false, { SUBJ: ['S1'] });
  const i = drawn.findIndex((s) => s.id === '__shape_0');
  const ops = p.series[i].data.map((d) => (d.itemStyle.opacity == null ? 1 : d.itemStyle.opacity));
  const lvs = drawn[i].data.map((d) => d.lv);
  lvs.forEach((lv, j) => assert.strictEqual(ops[j] === 1, lv === 'S1', lv));
});

// -- the band and the gear -----------------------------------------------------

test('the legend band draws a shape group: the title, a marker per level', () => {
  const c = D.open({ type: 'line', x: 'DAY', y: 'VAL', series: 'SUBJ', color: 'PARAM', shape: 'GR',
                     drill: 'auto' }, { data: ROWS, columns: COLUMNS });
  const el = c.block.legend.el;
  assert.strictEqual(el.hidden, false);
  const titles = Array.from(el.querySelectorAll('.vz-chart-legend-title'), (t) => t.textContent);
  assert.deepStrictEqual(titles, ['PARAM', 'GR']);
  const chips = Array.from(el.querySelectorAll('.vz-chart-legend-chip--shape'));
  assert.deepStrictEqual(chips.map((x) => x.dataset.shape), ['circle', 'triangle', 'diamond']);
  assert.deepStrictEqual(chips.map((x) => x.textContent), ['GRADE 0', 'GRADE 1', 'GRADE 2']);
  assert.ok(chips.every((x) => x.querySelector('svg.vz-chart-legend-shape path')));
  // The colour chips still select; the shape chips are no buttons.
  assert.strictEqual(el.querySelectorAll('button.vz-chart-legend-chip').length, 2);
  c.close();
});

test('the gear has rows for shape (once set) and ref_lines on a line, not ref_line_colors', () => {
  const c = D.open({ type: 'line', x: 'DAY', y: 'VAL', shape: 'GR', drill: 'auto' },
                   { data: ROWS, columns: COLUMNS });
  const rows = Array.from(c.block.frame.tray.band.querySelectorAll('.vz-row[data-role]'),
                          (r) => r.getAttribute('data-role'));
  assert.ok(rows.includes('shape'));
  assert.ok(rows.includes('ref_lines'));
  assert.ok(!rows.includes('ref_line_colors'));
  c.close();
});
