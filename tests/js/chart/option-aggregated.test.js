/* option-aggregated.js and its builders: the ECharts option per panel,
 * the legend chips and the highlight patch (D5), from small models. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure, plain, measure } = require('./load');

const C = loadPure({ roles: true });

const cat = (name, extra) => ({ name, class: 'string', ...extra });
const num = (name) => ({ name, class: 'number' });
const COLUMNS = [cat('T', { label: 'Term' }), cat('ID'), cat('ARM'), cat('SEV'), num('V')];
const ROWS = [
  { T: 'Rash', ID: 'S01', ARM: 'B', SEV: 'MILD', V: 4 },
  { T: 'Nausea', ID: 'S01', ARM: 'B', SEV: 'SEVERE', V: -2 },
  { T: 'Rash', ID: 'S02', ARM: 'A', SEV: 'SEVERE', V: 1 },
  { T: 'Rash', ID: 'S03', ARM: 'A', SEV: 'MILD', V: 6 },
  { T: 'Fever', ID: 'S03', ARM: 'A', SEV: 'MILD', V: 3 }
];
// A horizontal bar of row counts per term. A case that draws something else
// states its own category, value and orientation. `derived` is R's derived object.
const BASE = { type: 'bar', category: 'T', orientation: 'horizontal', drill: 'auto',
               sort_by: 'value', sort_dir: 'desc' };
const VERTICAL = { orientation: 'vertical' };

/** The settings of `cfg` and R's derived, which `cfg` may carry. */
const settings = (cfg, columns = COLUMNS) => {
  const { derived = {}, ...config } = { ...BASE, ...cfg };
  return { c: C.model.settings(config, columns), derived };
};

const build = (cfg, look = {}, rows = ROWS) => {
  const { c, derived } = settings(cfg);
  const m = C.model.aggregated({ rows, ix: new C.RowIndex(), columns: COLUMNS, cfg: c, derived });
  const o = C.option.aggregated(m, { cfg: c, derived, columns: COLUMNS, ink: C.INK_DEFAULT,
                                     theme: null, measure, widths: [600, 600], ...look });
  return { m, o, c, derived, opt: o.panels[0].option };
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
  const { opt } = build({ color: 'SEV', position: 'fill' });
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
  const { o, opt } = build(VERTICAL, { widths: [200] }, long);
  assert.strictEqual(opt.xAxis.type, 'category');
  const fit = o.panels[0].xFit;
  assert.ok(fit && fit.gutter > 0);
  assert.strictEqual(o.panels[0].panelH, 350 + fit.gutter);
  assert.strictEqual(opt.grid.bottom, 66 + fit.gutter);
  const wide = build(VERTICAL, { widths: [2000] });
  assert.strictEqual(wide.o.panels[0].xFit.gutter, 0);
});

test('value labels: a stack labelled once with its total, headroom on the axis', () => {
  const { opt } = build({ color: 'SEV', value_labels: true });
  assert.strictEqual(opt.series[0].label.show, false);
  assert.deepStrictEqual(plain(opt.series[1].data[0].label), { show: true, formatter: '3' });
  assert.deepStrictEqual(plain(opt.xAxis.boundaryGap), [0, '12%']);
});

test('value lines: a markLine on the first series and an axis widened past the line', () => {
  const { opt } = build({ value_lines: [{ value: 10, label: null }] });
  assert.strictEqual(opt.series[0].markLine.data[0].xAxis, 10);
  assert.strictEqual(typeof opt.xAxis.max, 'function');
  assert.strictEqual(opt.xAxis.max({ min: 0, max: 3 }), 12);
});

test('waterfall: a floating delta per step, totals restate the running sum', () => {
  const { opt } = build({ type: 'waterfall', category: 'T', value: 'V', orientation: null, func: 'sum',
                          sort_by: 'data', waterfall_totals: [{ value: 'Fever', label: null }] });
  assert.deepStrictEqual(plain(opt.xAxis.data), ['Rash', 'Nausea', 'Fever']);
  assert.deepStrictEqual(values(opt.series[0]), [0, 9, 0]);
  assert.deepStrictEqual(values(opt.series[1]), [11, 2, 9]);
  assert.deepStrictEqual(plain(opt.series[1].data.map((d) => d.itemStyle.color)),
                         ['#009E73', '#dc2626', '#bbbbbb']);
  assert.strictEqual(opt.legend.show, false);
});

test('pie and treemap: a slice per group with a value, the board scale colours it', () => {
  const pie = build({ type: 'pie', category: 'T', value: null, orientation: null,
                      derived: { scales: { var: 'T', color: { Rash: '#123456' } } } }).opt;
  assert.deepStrictEqual(plain(pie.series[0].data.map((d) => [d.name, d.value, d.itemStyle.color])),
                         [['Rash', 3, '#123456'], ['Nausea', 1, '#E69F00'], ['Fever', 1, '#009E73']]);
  const tm = build({ type: 'treemap', category: 'T', value: 'V', orientation: null, func: 'sum' }).opt;
  // Nausea sums to -2 and drops out.
  assert.deepStrictEqual(plain(tm.series[0].data.map((d) => d.name)), ['Rash', 'Fever']);
});

test('boxplot: five numbers per slot, a NaN box for an empty one, outliers beside', () => {
  const rows = [1, 2, 3, 4, 100].map((V) => ({ T: 'a', V })).concat([{ T: 'b', V: null }]);
  const { opt } = build({ type: 'boxplot', category: 'T', value: 'V', orientation: 'vertical', sort_by: 'data',
                          sort_dir: 'asc', box_points: 'outliers' }, {}, rows);
  assert.deepStrictEqual(plain(opt.xAxis.data), ['a', 'b']);
  assert.strictEqual(opt.series[0].type, 'custom');
  assert.deepStrictEqual(plain(opt.series[0].data[0]), { value: [0, 1, 2, 3, 4, 7], n: 5, name: 'a' });
  assert.deepStrictEqual(plain(opt.series[0].data[1].value), [1, null, null, null, null, null]);
  assert.strictEqual(opt.series[0].data[1].n, 0);
  assert.deepStrictEqual(plain(opt.series[1].data), [[0, 100]]);
  assert.strictEqual(opt.yAxis.scale, true);
});

test('point range: an interval under a centre dot, the slot as the dot name', () => {
  const { opt } = build({ type: 'pointrange', category: 'T', value: 'V', color: 'SEV',
                          sort_by: 'data', sort_dir: 'asc', orientation: 'horizontal' });
  assert.deepStrictEqual(plain(opt.series.map((s) => [s.type, s.name])),
                         [['custom', 'MILD'], ['scatter', 'MILD'], ['custom', 'SEVERE'],
                          ['scatter', 'SEVERE']]);
  assert.strictEqual(opt.series[1].data[0].name, 'Rash\u0000MILD');
  assert.strictEqual(opt.yAxis.axisLabel.formatter('Rash\u0000MILD'), 'Rash');
});

test('a distribution without a numeric value draws nothing in its panel', () => {
  const { o } = build({ type: 'boxplot', category: 'T', value: null, orientation: 'vertical' });
  assert.strictEqual(o.panels[0].option, null);
  assert.strictEqual(o.panels[0].height, '350px');
});

test('legend chips: colour levels; none for pie, treemap and waterfall', () => {
  const legend = (cfg) => {
    const { m, c, derived } = build(cfg);
    return plain(C.option.aggregatedLegend(m, c, derived));
  };
  assert.deepStrictEqual(legend({ color: 'SEV' }), { col: 'SEV', items: [
    { name: 'MILD', color: '#0072B2' }, { name: 'SEVERE', color: '#E69F00' }] });
  assert.strictEqual(legend({ type: 'pie', category: 'T', value: null, orientation: null, color: 'SEV' }), null);
  assert.strictEqual(legend({ type: 'waterfall', category: 'T', value: null, orientation: null, color: 'SEV' }), null);
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
                         [[1, 0.25, 0.25], [0.25, 0.25, 0.25]]);
  // Fever has no SEVERE cell; its empty slot is lit with the group.
  assert.deepStrictEqual(opacities(patch({ color: 'SEV' }, { T: ['Rash', 'Fever'] })),
                         [[1, 0.25, 1], [1, 0.25, 1]]);
  // No filter: every mark at full strength.
  assert.deepStrictEqual(opacities(patch({ color: 'SEV' }, null)), [[1, 1, 1], [1, 1, 1]]);
});

test('D5: a filter on a column the chart does not draw lights the marks holding it', () => {
  assert.deepStrictEqual(opacities(patch({}, { ID: ['S03'] })), [[1, 0.25, 1]]);
});

test('D5: a facet key lights its own panel only', () => {
  const f = { T: ['Rash'], ARM: ['B'] };
  assert.deepStrictEqual(opacities(patch({ facet: 'ARM' }, f, 0)), [[0.25, 0.25, 0.25]]);
  assert.deepStrictEqual(opacities(patch({ facet: 'ARM' }, f, 1)), [[1, 0.25, 0.25]]);
});

test('D5: point ranges dim too, and stay untouched while nothing is filtered', () => {
  const cfg = { type: 'pointrange', category: 'T', value: 'V', orientation: 'horizontal', sort_by: 'data', sort_dir: 'asc' };
  const p = patch(cfg, { T: ['Nausea'] });
  assert.deepStrictEqual(p.series.map((s) => s.data.map((d) => d.itemStyle.opacity)),
                         [[0.25, 1, 0.25], [0.25, 1, 0.25]]);
  assert.strictEqual(p.dimmed, true);
  assert.deepStrictEqual(patch(cfg, null).series, [{}, {}]);
});

// -- format / unit (format.js) -------------------------------------------------

test('format and unit: compact ticks, the unit in the title, values with it', () => {
  const f = C.valueFormat({ format: 'compact', unit: 'CHF' });
  assert.strictEqual(f(1234567), '1.2M CHF');
  assert.strictEqual(f(950), '950 CHF');
  assert.strictEqual(C.valueFormat({ format: 'percent' })(0.256), '25.6%');
  assert.strictEqual(C.valueFormat({ format: 'number' }), null);
  assert.strictEqual(C.valueFormat({ unit: 'CHF' }, { unit: false }), null);

  const { opt } = build({ value: 'V', func: 'sum', format: 'compact', unit: 'CHF' });
  // A column without a label: the axis is named by the unit alone.
  assert.strictEqual(opt.xAxis.name, 'CHF');
  assert.strictEqual(opt.xAxis.axisLabel.formatter(2500), '2.5K');
});

test('the axis name: label and unit, the unit alone, none when the title says it', () => {
  const titled = (text) => ({ titles: { title: { text } } });
  assert.strictEqual(C.axisName('Term', 'T', { unit: '%' }, {}), 'Term (%)');
  assert.strictEqual(C.axisName('gdp_2026', 'gdp_2026', { unit: '%' }, {}), '%');
  assert.strictEqual(C.axisName('gdp_2026', 'gdp_2026', {}, {}), 'gdp_2026');
  assert.strictEqual(C.axisName('gdp_2026', 'gdp_2026', { unit: '%' }, titled('Real GDP growth 2026, %')), '');
  assert.strictEqual(C.axisName('Count', '.count', {}, titled('Adverse events')), 'Count');
});

test('value labels print each value on its own; a line at 0 has no label', () => {
  const { opt } = build({ value: 'V', func: 'sum', value_labels: true,
                         value_lines: [{ value: 0, label: null }, { value: 2, label: null }] });
  const lbl = opt.series[0].label.formatter;
  assert.strictEqual(lbl({ value: 1.5 }), '1.5');
  assert.strictEqual(lbl({ value: 2.14 }), '2.14');
  assert.strictEqual(lbl({ value: 4.3587 }), '4.36');
  const lines = opt.series[0].markLine.data;
  assert.strictEqual(lines[0].label.show, false);
  assert.strictEqual(lines[1].label.show, true);
});

test('x_lines and x_ranges on a bar: on the categories, either way round', () => {
  const columns = [{ name: 'WEEK', class: 'date' }, num('V')];
  const rows = ['2026-10-05', '2026-10-12', '2026-10-19', '2026-10-26']
    .map((d, i) => ({ WEEK: d, V: i + 1 }));
  const run = (cfg) => {
    const { orientation, ...rest } = cfg;
    const { c, derived } = settings({ category: 'WEEK', value: 'V', orientation, func: 'sum', sort_by: 'data', ...rest },
                                    columns);
    const m = C.model.aggregated({ rows, ix: new C.RowIndex(), columns, cfg: c, derived });
    return C.option.aggregated(m, { cfg: c, derived, columns, ink: C.INK_DEFAULT, theme: null,
                                    measure, widths: [600] }).panels[0].option;
  };
  const cfg = {
    x_lines: [{ value: '2026-10-21', label: 'Listed' }, { value: '2026-10-12', label: null },
              { value: '2027-01-01', label: null }],
    x_ranges: { Promo: [{ value: '2026-09-01', label: null }, { value: '2026-10-14', label: null }] },
    value_lines: [{ value: 2, label: null }]
  };
  const v = run({ ...cfg, orientation: 'vertical' });
  // The value line stays; a date inside a week marks that week's bar; past
  // the axis draws nothing; unnamed on its own bar, no label.
  assert.deepStrictEqual(plain(v.series[0].markLine.data.map((d) => [d.xAxis ?? null, d.yAxis ?? null])),
                         [[null, 2], ['2026-10-19', null], ['2026-10-12', null]]);
  assert.strictEqual(v.series[0].markLine.data[1].label.formatter, 'Listed');
  assert.strictEqual(v.series[0].markLine.data[2].label.show, false);
  // The range starts before the axis: it shades from the oldest bar. The
  // weeks run newest first (sort_dir desc), the range in axis order.
  assert.deepStrictEqual(plain(v.series[0].markArea.data),
                         [[{ name: 'Promo', xAxis: '2026-10-12' }, { xAxis: '2026-10-05' }]]);
  const h = run({ ...cfg, orientation: 'horizontal' });
  assert.strictEqual(h.series[0].markLine.data[1].yAxis, '2026-10-19');
  assert.strictEqual(h.series[0].markArea.data[0][1].yAxis, '2026-10-05');
});
