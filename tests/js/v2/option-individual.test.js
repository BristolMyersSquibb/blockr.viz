/* v2 option-individual.js, option-points.js and option-band.js: the
 * ECharts option per panel of a scatter, line and band, the slot state it
 * carries, the legend chips and the highlight patch (D5). */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure, plain, measure } = require('./load');

const C = loadPure({ roles: true });

const cat = (name, extra) => ({ name, type: 'categorical', ...extra });
const num = (name, extra) => ({ name, type: 'numeric', ...extra });
const COLUMNS = [cat('ID'), cat('ARM', { label: 'Arm' }), cat('SEX'), cat('VIS', { levels: ['Base', 'W2'] }),
                 num('DAY', { label: 'Study Day' }), num('VAL'), num('LO'), num('HI')];
const ROWS = [
  { ID: 'S01', ARM: 'B', SEX: 'M', VIS: 'Base', DAY: 1, VAL: 3, LO: 2, HI: 4 },
  { ID: 'S01', ARM: 'B', SEX: 'M', VIS: 'W2', DAY: 14, VAL: 5, LO: 4, HI: 6 },
  { ID: 'S02', ARM: 'A', SEX: 'F', VIS: 'Base', DAY: 2, VAL: 7, LO: 6, HI: 8 },
  { ID: 'S02', ARM: 'A', SEX: 'F', VIS: 'W2', DAY: 15, VAL: 9, LO: null, HI: 10 }
];
const BASE = { chart_type: 'scatter', x: 'DAY', y: 'VAL', drill: 'auto' };

const build = (cfg, look = {}) => {
  const c = { ...BASE, ...cfg };
  const m = C.model.individual({ rows: ROWS, ix: new C.RowIndex(), columns: COLUMNS, cfg: c });
  const o = C.option.individual(m, { cfg: c, columns: COLUMNS, ink: C.INK_DEFAULT, theme: null,
                                     measure, widths: [600, 600, 600], ...look });
  return { m, o, c, po: o.panels[0], opt: o.panels[0].option };
};
const types = (opt) => plain(opt.series.map((s) => [s.type, s.name == null ? null : s.name]));

test('a scatter: a series per level, titled axes, the brush armed', () => {
  const { po, opt } = build({ color: 'ARM' });
  assert.deepStrictEqual(types(opt), [['scatter', 'A'], ['scatter', 'B']]);
  assert.strictEqual(opt.xAxis.name, 'Study Day');
  assert.strictEqual(opt.yAxis.name, 'VAL');
  assert.strictEqual(opt.series[0].symbolSize, 6);
  assert.deepStrictEqual(plain(opt.toolbox.feature), { brush: { type: ['rect', 'lineX', 'clear'] } });
  assert.strictEqual(opt.brush.yAxisIndex, 0);
  assert.strictEqual(po.brushable, true);
  assert.strictEqual(po.zoomable, false);
  assert.strictEqual(po.focus, null);
  assert.strictEqual(po.height, '400px');
});

test('a scatter with series is clicked, not brushed', () => {
  const { po, opt } = build({ series: 'ID' });
  assert.strictEqual(po.brushable, false);
  assert.strictEqual(opt.toolbox, undefined);
});

test('a line: the zoom toolbox, the veil\'s axes, the hover overlay, a picker for a crowd', () => {
  const { po, opt } = build({ chart_type: 'line', series: 'ID', connect: 'step-end' });
  assert.deepStrictEqual(types(opt), [['line', 'S01'], ['line', 'S02'], ['custom', null],
                                      ['line', '__focus__'], ['scatter', '__focus_dots__']]);
  assert.strictEqual(opt.series[0].step, 'end');
  assert.strictEqual(opt.series[0].smooth, undefined);
  assert.strictEqual(opt.xAxis.length, 2);
  assert.strictEqual(opt.xAxis[1].show, false);
  assert.ok(opt.toolbox.feature.dataZoom);
  assert.strictEqual(po.zoomable, true);
  assert.strictEqual(po.focus.series, opt.series);
  assert.strictEqual(po.focus.stepMode, 'end');
  // One line: nothing to pick out.
  assert.strictEqual(build({ chart_type: 'line' }).po.focus, null);
});

test('the line tooltip follows the hovered line, and says nothing away from every line', () => {
  const { po, opt } = build({ chart_type: 'line', series: 'ID' });
  const ps = [{ seriesType: 'line', seriesIndex: 0, seriesName: 'S01', value: [1, 3], color: '#111111' },
              { seriesType: 'line', seriesIndex: 1, seriesName: 'S02', value: [1, 7], color: '#222222' }];
  assert.strictEqual(opt.tooltip.formatter(ps), '');
  po.hover.si = 1;
  const html = opt.tooltip.formatter(ps);
  assert.match(html, /Study Day: 1/);
  assert.match(html, /S02/);
  assert.doesNotMatch(html, /S01/);
});

test('the point tooltip: the level, x and y by title, then the tooltip fields', () => {
  const { opt } = build({ color: 'ARM', tt_fields: ['SEX'] });
  const html = opt.tooltip.formatter({ seriesName: 'A', value: [2, 7, 'F'], color: '#123456' });
  for (const s of ['A', 'Study Day', '>2<', 'VAL', '>7<', 'Arm', 'SEX', '>F<']) {
    assert.ok(html.includes(s), `${s} in ${html}`);
  }
});

test('error bars in data order, the smoother from R\'s series of the panel', () => {
  const smoother_series = { F: { __all__: { x: [0, 10, 20], y: [1, 2, 3] } },
                            M: { __all__: { x: [0, 10], y: [5, NaN] } } };
  const { o } = build({ facet: 'SEX', lo: 'LO', hi: 'HI', smoother: 'lm', smoother_series });
  const [f, m] = o.panels.map((p) => p.option);
  assert.deepStrictEqual(types(f), [['scatter', null], ['line', 'fit'], ['custom', 'errbar']]);
  assert.deepStrictEqual(plain(f.series[1].data), [[0, 1], [10, 2], [20, 3]]);
  // A row without LO has no bar.
  assert.deepStrictEqual(plain(f.series[2].data), [[2, 6, 8]]);
  // M's fit has one finite point: no line.
  assert.deepStrictEqual(types(m), [['scatter', null], ['custom', 'errbar']]);
});

test('the identity line: one rounded domain on both axes, on the markLine', () => {
  const { opt } = build({ identity_line: 'on' });
  assert.strictEqual(opt.xAxis.min, 0);
  assert.strictEqual(opt.xAxis.max, 16);
  assert.strictEqual(opt.yAxis.min, 0);
  assert.strictEqual(opt.yAxis.max, 16);
  const d = opt.series[0].markLine.data;
  assert.deepStrictEqual(plain(d[d.length - 1].map((e) => e.coord)), [[0, 0], [16, 16]]);
  // Not on a line chart.
  assert.strictEqual(build({ chart_type: 'line', identity_line: 'on' }).opt.xAxis[0].min, undefined);
});

test('helper lines on x and y share the markLine of the first series', () => {
  const { opt } = build({ x_lines: [5], value_lines: [4, 8] });
  const d = plain(opt.series[0].markLine.data);
  assert.deepStrictEqual(d.map((e) => e.xAxis ?? e.yAxis), [5, 4, 8]);
});

test('categorical x: the labels fit the panel width; turned labels grow the canvas', () => {
  const wide = build({ chart_type: 'line', x: 'VIS', series: 'ID' });
  assert.deepStrictEqual(plain(wide.opt.xAxis[0].data), ['Base', 'W2']);
  assert.strictEqual(wide.po.xFit.inset, 71);
  assert.strictEqual(wide.po.height, '400px');
  const narrow = build({ chart_type: 'line', x: 'VIS', series: 'ID' }, { widths: [100] });
  assert.ok(narrow.po.panelH > 400);
  assert.strictEqual(narrow.opt.xAxis[0].axisLabel.rotate, 90);
});

const BAND = {
  __all__: {
    A: { x: [0, 10, 20], center: [5, 6, 7], lo: [4, 5, 6], hi: [6, 7, 8], olo: [3, 4, 5],
         ohi: [7, 8, 9], out_x: [10], out_y: [12] },
    B: { x: [0, 10, 20], center: [1, 2, null], lo: [0, null, 1], hi: [2, 3, 2] }
  }
};

test('a coloured band: per level a hidden ribbon, a halo and the centre line', () => {
  const { po, opt } = build({ chart_type: 'band', color: 'ARM', band_series: BAND });
  // B's ribbon breaks at the missing lo into runs of one: no ribbon.
  assert.deepStrictEqual(types(opt), [['custom', 'A'], ['line', 'A'], ['line', 'A'],
                                      ['line', 'B'], ['line', 'B']]);
  assert.deepStrictEqual(plain(opt.series[0].data), []);
  assert.deepStrictEqual(plain(po.band.centerIdx), { A: 2, B: 4 });
  assert.deepStrictEqual(plain(po.band.ribbonIdx), { A: { idx: 0, item: [0, 6] } });
  assert.strictEqual(po.band.total, 5);
  assert.strictEqual(opt.tooltip.trigger, 'item');
  assert.strictEqual(po.brushable, false);
});

test('one band level: the outer ribbon, the outliers, a y axis fitted to them', () => {
  const band_series = { __all__: { A: BAND.__all__.A } };
  const { po, opt } = build({ chart_type: 'band', color: 'ARM', band_series, box_points: 'outliers' });
  assert.deepStrictEqual(types(opt).map((t) => t[0]), ['custom', 'custom', 'scatter', 'line', 'line']);
  assert.strictEqual(po.band, null);
  assert.ok(opt.yAxis.max >= 12);
});

test('band reference limits: inside the frame, or pinned to its edge', () => {
  const band_series = { __all__: { __all__: BAND.__all__.A } };
  const refs = { hi: { col: 'HI', value: 8.5, lo: 8, hi: 9, n_distinct: 2 },
                 lo: { col: 'LO', value: -50, lo: -50, hi: -50, n_distinct: 1 } };
  const { opt } = build({ chart_type: 'band', band_series, band_refs: refs });
  const ml = opt.series.find((s) => s.markLine).markLine.data;
  assert.deepStrictEqual(plain(ml.map((d) => d.label.formatter)), ['HI 8.5 (8–9)', 'LO -50 ↓ off scale']);
  assert.strictEqual(ml[1].yAxis, opt.yAxis.min);
});

test('a band reads its facet panel\'s own series', () => {
  const band_series = { F: { __all__: BAND.__all__.A }, M: { __all__: BAND.__all__.B } };
  const { o } = build({ chart_type: 'band', facet: 'SEX', band_series });
  const centre = (p) => p.option.series[p.option.series.length - 1].data[0];
  assert.deepStrictEqual(plain(o.panels.map(centre)), [[0, 5], [0, 1]]);
});

test('the legend: the colour levels; by colour value with a separate series', () => {
  assert.deepStrictEqual(plain(C.option.individualLegend(build({ color: 'ARM' }).m)).items.map((i) => i.name),
                         ['A', 'B']);
  const { m, opt, po } = build({ series: 'ID', color: 'ARM' });
  assert.deepStrictEqual(plain(C.option.individualLegend(m).items.map((i) => i.name)), ['A', 'B']);
  // An empty series per chip, for the legend to bind to.
  assert.deepStrictEqual(types(opt).slice(-2), [['scatter', 'A'], ['scatter', 'B']]);
  assert.deepStrictEqual(plain(po.seriesByColorByVal), { A: ['S02'], B: ['S01'] });
  assert.strictEqual(C.option.individualLegend(build({ series: 'ID' }).m), null);
});

test('drill off: no emphasis, no hand cursor; a theme drops the transparent background', () => {
  const { opt } = build({ drill: '' });
  assert.strictEqual(opt.series[0].cursor, 'default');
  assert.strictEqual(opt.backgroundColor, 'transparent');
  assert.strictEqual(build({}, { theme: 'dark' }).opt.backgroundColor, undefined);
});

// -- Highlight (D5) ----------------------------------------------------------

const patch = (cfg, filters, wasDimmed = false) => {
  const { m, opt } = build(cfg);
  const lit = filters ? (keys, rows) => C.keys.markLit(keys, rows, filters) : null;
  return plain(C.option.individualPatch(m, m.panels[0], opt.series, lit, wasDimmed, filters));
};

test('D5: no filter is an empty patch, unless something was dimmed', () => {
  assert.deepStrictEqual(patch({ chart_type: 'line', series: 'ID' }, null),
                         { series: [{}, {}, {}, {}, {}], dimmed: false });
  const undo = patch({ chart_type: 'line', series: 'ID' }, null, true);
  assert.deepStrictEqual(undo.series[0], { lineStyle: { opacity: 1 }, itemStyle: { opacity: 1 } });
  assert.deepStrictEqual(undo.series.slice(2), [{}, {}, {}]);
});

test('D5: a filter on the series dims the other lines', () => {
  const p = patch({ chart_type: 'line', series: 'ID' }, { ID: ['S02'] });
  assert.deepStrictEqual(p.series.slice(0, 2), [
    { lineStyle: { opacity: 0.15 }, itemStyle: { opacity: 0.15 } },
    { lineStyle: { opacity: 1 }, itemStyle: { opacity: 1 } }]);
  assert.strictEqual(p.dimmed, true);
});

test('D5: a filter on a column the points do not carry lights each point by its row', () => {
  const p = patch({ color: 'ARM' }, { ID: ['S02'] });
  assert.deepStrictEqual(p.series[1], { data: [{ value: [1, 3], itemStyle: { opacity: 0.15 } },
                                               { value: [14, 5], itemStyle: { opacity: 0.15 } }],
                                        itemStyle: { opacity: 1 } });
  assert.deepStrictEqual(p.series[0].data[0].itemStyle, { opacity: 1 });
  // A filter on the colour lights whole series, the points plain again.
  const q = patch({ color: 'ARM' }, { ARM: ['A'] });
  assert.deepStrictEqual(q.series[1], { data: [[1, 3], [14, 5]], itemStyle: { opacity: 0.15 } });
});

test('D5: a band level dims with its halo and ribbon', () => {
  const p = patch({ chart_type: 'band', color: 'ARM', band_series: BAND }, { ARM: ['A'] });
  assert.deepStrictEqual(p.series.map((s) => (s.lineStyle || s.itemStyle).opacity),
                         [1, 1, 1, 0.15, 0.15]);
});

test('the option reads only the config fields it and the model list', () => {
  const configs = [
    {}, { color: 'ARM', facet: 'SEX', lo: 'LO', hi: 'HI', x_lines: [1], value_lines: [2] },
    { chart_type: 'line', series: 'ID', connect: 'straight', line_width_mult: 2 },
    { chart_type: 'line', x: 'VIS', color: 'ARM', dot_size_mult: 2 },
    { smoother: 'lm', smoother_series: {}, identity_line: 'on', drill: '' },
    { chart_type: 'band', color: 'ARM', band_series: BAND, box_points: 'outliers',
      band_refs: { hi: { col: 'HI', value: 7 } } }
  ];
  const allowed = new Set([...C.model.individual.FIELDS, ...C.option.individual.FIELDS]);
  for (const cfg of configs) {
    const c = { ...BASE, ...cfg };
    const m = C.model.individual({ rows: ROWS, ix: new C.RowIndex(), columns: COLUMNS, cfg: c });
    const read = new Set();
    const spy = new Proxy(c, { get(t, k) { read.add(k); return t[k]; } });
    C.option.individual(m, { cfg: spy, columns: COLUMNS, ink: C.INK_DEFAULT, theme: null,
                             measure, widths: [600, 600, 600] });
    const extra = [...read].filter((k) => typeof k === 'string' && !allowed.has(k));
    assert.deepStrictEqual(extra, [], `${JSON.stringify(cfg)} reads ${extra}`);
  }
});
