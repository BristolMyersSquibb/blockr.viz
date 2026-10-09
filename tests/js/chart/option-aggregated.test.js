/* option-aggregated.js and its builders: the ECharts option per panel,
 * the legend chips and the highlight patch (D5), from small models. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure, plain, measure } = require('./load');

const C = loadPure({ roles: true });

const cat = (name, extra) => ({ name, type: 'categorical', ...extra });
const num = (name) => ({ name, type: 'numeric' });
const COLUMNS = [cat('T', { label: 'Term' }), cat('ID'), cat('ARM'), cat('SEV'), num('V')];
const ROWS = [
  { T: 'Rash', ID: 'S01', ARM: 'B', SEV: 'MILD', V: 4 },
  { T: 'Nausea', ID: 'S01', ARM: 'B', SEV: 'SEVERE', V: -2 },
  { T: 'Rash', ID: 'S02', ARM: 'A', SEV: 'SEVERE', V: 1 },
  { T: 'Rash', ID: 'S03', ARM: 'A', SEV: 'MILD', V: 6 },
  { T: 'Fever', ID: 'S03', ARM: 'A', SEV: 'MILD', V: 3 }
];
const BASE = { chart_type: 'bar', group: 'T', value: '.count', func: 'count', drill: 'auto',
               sort_by: 'value', sort_dir: 'desc' };

const build = (cfg, look = {}, rows = ROWS) => {
  const c = { ...BASE, ...cfg };
  const m = C.model.aggregated({ rows, ix: new C.RowIndex(), columns: COLUMNS, cfg: c });
  const o = C.option.aggregated(m, { cfg: c, columns: COLUMNS, ink: C.INK_DEFAULT, theme: null,
                                     measure, widths: [600, 600], ...look });
  return { m, o, c, opt: o.panels[0].option };
};
const values = (s) => plain(s.data.map((d) => (d && typeof d === 'object' ? d.value : d)));

test('a plain bar: one series in group order, horizontal, 28 px a row', () => {
  const { o, opt } = build({});
  assert.strictEqual(opt.series.length, 1);
  assert.deepStrictEqual(plain(opt.yAxis.data), ['Rash', 'Nausea', 'Fever']);
  assert.deepStrictEqual(values(opt.series[0]), [3, 1, 1]);
  assert.strictEqual(opt.yAxis.inverse, true);
  assert.strictEqual(opt.xAxis.name, 'Count');
  assert.strictEqual(o.panels[0].height, 30 + 3 * 28 + 46 + 'px');
  assert.strictEqual(o.panels[0].xFit, null);
});

test('a stacked colour split: a series per level, the outer segment rounded', () => {
  const { opt } = build({ color: 'SEV' });
  assert.deepStrictEqual(plain(opt.series.map((s) => [s.name, s.stack])),
                         [['MILD', 'stack'], ['SEVERE', 'stack']]);
  assert.deepStrictEqual(values(opt.series[0]), [2, null, 1]);
  // Rash's outer segment is SEVERE; Fever has MILD only.
  assert.deepStrictEqual(plain(opt.series[1].data[0].itemStyle.borderRadius), [0, 2, 2, 0]);
  assert.deepStrictEqual(plain(opt.series[0].data[2].itemStyle.borderRadius), [0, 2, 2, 0]);
  assert.strictEqual(opt.series[0].data[0], 2);
  assert.deepStrictEqual(plain(opt.legend), { show: false, data: ['MILD', 'SEVERE'] });
});

test('percent: shares of the group total, the raw value kept, a 0..1 axis', () => {
  const { opt } = build({ color: 'SEV', bar_mode: 'percent' });
  assert.deepStrictEqual(plain(opt.series[0].data[0]), { value: 2 / 3, raw: 2 });
  assert.strictEqual(opt.xAxis.max, 1);
  assert.strictEqual(opt.xAxis.name, '% of group total');
  assert.strictEqual(opt.xAxis.axisLabel.formatter(0.25), '25%');
  // A split bar's card lists every level at the category, the hovered one marked.
  const tip = opt.tooltip.formatter({ name: 'Rash', dataIndex: 0, seriesName: 'MILD' });
  assert.match(tip, /is-hit.*67% \(2\)/);
  assert.match(tip, /is-dim.*SEVERE/);
});

test('vertical: categories on x, labels fitted to the width, the canvas grows', () => {
  const long = ROWS.map((r) => ({ ...r, T: r.T + ' with a long name to wrap' }));
  const { o, opt } = build({ orientation: 'vertical' }, { widths: [200] }, long);
  assert.strictEqual(opt.xAxis.type, 'category');
  const fit = o.panels[0].xFit;
  assert.ok(fit && fit.gutter > 0);
  assert.strictEqual(o.panels[0].panelH, 350 + fit.gutter);
  assert.strictEqual(opt.grid.bottom, 66 + fit.gutter);
  const wide = build({ orientation: 'vertical' }, { widths: [2000] });
  assert.strictEqual(wide.o.panels[0].xFit.gutter, 0);
});

test('value labels: a stack labelled once with its total, headroom on the axis', () => {
  const { opt } = build({ color: 'SEV', value_labels: 'on' });
  assert.strictEqual(opt.series[0].label.show, false);
  assert.deepStrictEqual(plain(opt.series[1].data[0].label), { show: true, formatter: '3' });
  assert.deepStrictEqual(plain(opt.xAxis.boundaryGap), [0, '12%']);
});

test('value lines: a markLine on the first series and an axis widened past the line', () => {
  const { opt } = build({ value_lines: [10] });
  assert.strictEqual(opt.series[0].markLine.data[0].xAxis, 10);
  assert.strictEqual(typeof opt.xAxis.max, 'function');
  assert.strictEqual(opt.xAxis.max({ min: 0, max: 3 }), 12);
});

test('waterfall: a floating delta per step, totals restate the running sum', () => {
  const { opt } = build({ chart_type: 'waterfall', value: 'V', func: 'sum', sort_by: 'data',
                          waterfall_totals: ['Fever'] });
  assert.deepStrictEqual(plain(opt.xAxis.data), ['Rash', 'Nausea', 'Fever']);
  assert.deepStrictEqual(values(opt.series[0]), [0, 9, 0]);
  assert.deepStrictEqual(values(opt.series[1]), [11, 2, 9]);
  assert.deepStrictEqual(plain(opt.series[1].data.map((d) => d.itemStyle.color)),
                         ['#009E73', '#dc2626', '#bbbbbb']);
  assert.strictEqual(opt.legend.show, false);
});

test('pie and treemap: a slice per group with a value, the board scale colours it', () => {
  const pie = build({ chart_type: 'pie', scales: { var: 'T', color: { Rash: '#123456' } } }).opt;
  assert.deepStrictEqual(plain(pie.series[0].data.map((d) => [d.name, d.value, d.itemStyle.color])),
                         [['Rash', 3, '#123456'], ['Nausea', 1, '#D55E00'], ['Fever', 1, '#F0E442']]);
  const tm = build({ chart_type: 'treemap', value: 'V', func: 'sum' }).opt;
  // Nausea sums to -2 and drops out.
  assert.deepStrictEqual(plain(tm.series[0].data.map((d) => d.name)), ['Rash', 'Fever']);
});

test('radar: a shape per colour level on spokes sharing one max', () => {
  const { opt } = build({ chart_type: 'radar', color: 'SEV', facet: 'ARM' });
  assert.deepStrictEqual(plain(opt.radar.indicator.map((i) => [i.name, i.max])),
                         [['Rash', 1], ['Nausea', 1], ['Fever', 1]]);
  assert.deepStrictEqual(plain(opt.series[0].data.map((d) => [d.name, d.value])),
                         [['MILD', [1, 0, 1]], ['SEVERE', [1, 0, 0]]]);
});

test('boxplot: five numbers per slot, a NaN box for an empty one, outliers beside', () => {
  const rows = [1, 2, 3, 4, 100].map((V) => ({ T: 'a', V })).concat([{ T: 'b', V: null }]);
  const { opt } = build({ chart_type: 'boxplot', value: 'V', func: 'mean', sort_by: 'data',
                          sort_dir: 'asc', box_points: 'outliers' }, {}, rows);
  assert.deepStrictEqual(plain(opt.xAxis.data), ['a', 'b']);
  assert.deepStrictEqual(plain(opt.series[0].data[0]), { value: [1, 2, 3, 4, 7], n: 5 });
  assert.deepStrictEqual(plain(opt.series[0].data[1].value), [null, null, null, null, null]);
  assert.strictEqual(opt.series[0].data[1].n, 0);
  assert.deepStrictEqual(plain(opt.series[1].data), [[0, 100]]);
  assert.strictEqual(opt.yAxis.scale, true);
});

test('point range: an interval under a centre dot, the slot as the dot name', () => {
  const { opt } = build({ chart_type: 'pointrange', value: 'V', func: 'mean', color: 'SEV',
                          sort_by: 'data', sort_dir: 'asc', orientation: 'horizontal' });
  assert.deepStrictEqual(plain(opt.series.map((s) => [s.type, s.name])),
                         [['custom', 'MILD'], ['scatter', 'MILD'], ['custom', 'SEVERE'],
                          ['scatter', 'SEVERE']]);
  assert.strictEqual(opt.series[1].data[0].name, 'Rash\u0000MILD');
  assert.strictEqual(opt.yAxis.axisLabel.formatter('Rash\u0000MILD'), 'Rash');
});

test('a distribution without a numeric value draws nothing in its panel', () => {
  const { o } = build({ chart_type: 'boxplot', value: '' });
  assert.strictEqual(o.panels[0].option, null);
  assert.strictEqual(o.panels[0].height, '350px');
});

test('legend chips: colour levels; none for pie, treemap and waterfall', () => {
  const legend = (cfg) => plain(C.option.aggregatedLegend(build(cfg).m, { ...BASE, ...cfg }));
  assert.deepStrictEqual(legend({ color: 'SEV' }), { col: 'SEV', items: [
    { name: 'MILD', color: '#0072B2' }, { name: 'SEVERE', color: '#D55E00' }] });
  assert.strictEqual(legend({ chart_type: 'pie', color: 'SEV' }), null);
  assert.strictEqual(legend({ chart_type: 'waterfall', color: 'SEV' }), null);
  assert.strictEqual(legend({}), null);
});

// -- D5 ------------------------------------------------------------------------

const patch = (cfg, filters, panel = 0) => {
  const { m, opt, c } = build(cfg);
  const ix = new C.RowIndex();
  const lit = filters ? (keys) => C.keys.markLit(keys,
    () => C.model.rowsUnder(ROWS, ix, keys), filters) : null;
  return plain(C.option.aggregatedPatch(m, m.panels[panel], c, opt, lit, false));
};
const opacities = (p) => p.series.map((s) => s.data.map((d) => d.itemStyle.opacity));

test('D5: a segment filter lights that segment, a group filter the whole bar', () => {
  assert.deepStrictEqual(opacities(patch({ color: 'SEV' }, { T: ['Rash'], SEV: ['MILD'] })),
                         [[1, 0.15, 0.15], [0.15, 0.15, 0.15]]);
  // Fever has no SEVERE cell; its empty slot is lit with the group.
  assert.deepStrictEqual(opacities(patch({ color: 'SEV' }, { T: ['Rash', 'Fever'] })),
                         [[1, 0.15, 1], [1, 0.15, 1]]);
  // No filter: every mark at full strength.
  assert.deepStrictEqual(opacities(patch({ color: 'SEV' }, null)), [[1, 1, 1], [1, 1, 1]]);
});

test('D5: a filter on a column the chart does not draw lights the marks holding it', () => {
  assert.deepStrictEqual(opacities(patch({}, { ID: ['S03'] })), [[1, 0.15, 1]]);
});

test('D5: a facet key lights its own panel only', () => {
  const f = { T: ['Rash'], ARM: ['B'] };
  assert.deepStrictEqual(opacities(patch({ facet: 'ARM' }, f, 0)), [[0.15, 0.15, 0.15]]);
  assert.deepStrictEqual(opacities(patch({ facet: 'ARM' }, f, 1)), [[1, 0.15, 0.15]]);
});

test('D5: point ranges dim too, and stay untouched while nothing is filtered', () => {
  const cfg = { chart_type: 'pointrange', value: 'V', func: 'mean', sort_by: 'data',
                sort_dir: 'asc' };
  const p = patch(cfg, { T: ['Nausea'] });
  assert.deepStrictEqual(p.series.map((s) => s.data.map((d) => d.itemStyle.opacity)),
                         [[0.15, 1, 0.15], [0.15, 1, 0.15]]);
  assert.strictEqual(p.dimmed, true);
  assert.deepStrictEqual(patch(cfg, null).series, [{}, {}]);
});

test('the option reads only the config fields the model and the option list', () => {
  const configs = [
    {}, { color: 'SEV', bar_mode: 'percent', value_labels: 'on', facet: 'ARM' },
    { color: 'SEV', bar_mode: 'grouped', orientation: 'vertical', value_lines: [2] },
    { func: 'pct_distinct', value: 'ID', pct_of: ['color'], color: 'SEV' },
    { chart_type: 'waterfall', value: 'V', func: 'sum', waterfall_totals: ['Fever'],
      value_labels: 'on', count_on: 'axis' },
    { chart_type: 'pie' }, { chart_type: 'treemap' }, { chart_type: 'radar', color: 'SEV' },
    { chart_type: 'boxplot', value: 'V', func: 'mean', color: 'SEV', box_points: 'outliers',
      count_on: 'axis', count_col: 'ID' },
    { chart_type: 'pointrange', value: 'V', func: 'mean', connect_centers: 'on',
      orientation: 'horizontal', summary: 'mean_sd' },
    { drill: '', palette: ['#000000'] }
  ];
  const allowed = new Set([...C.model.aggregated.FIELDS, ...C.option.aggregated.FIELDS]);
  for (const cfg of configs) {
    const c = { ...BASE, ...cfg };
    const m = C.model.aggregated({ rows: ROWS, ix: new C.RowIndex(), columns: COLUMNS, cfg: c });
    const read = new Set();
    const spy = new Proxy(c, { get(t, k) { read.add(k); return t[k]; } });
    C.option.aggregated(m, { cfg: spy, columns: COLUMNS, ink: C.INK_DEFAULT, theme: null,
                             measure, widths: [600, 600] });
    const extra = [...read].filter((k) => typeof k === 'string' && !allowed.has(k));
    assert.deepStrictEqual(extra, [], `${JSON.stringify(cfg)} reads ${extra}`);
  }
});
