/* v2 option-timeline.js: the ECharts option per panel, from a model. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure, plain, measure } = require('./load');

const C = loadPure();

const COLUMNS = [
  { name: 'T', type: 'categorical', label: 'Term' }, { name: 'SEV', type: 'categorical' },
  { name: 'ID', type: 'categorical' }, { name: 'S', type: 'numeric', label: 'Start' },
  { name: 'E', type: 'numeric' }, { name: 'V', type: 'categorical' }
];
const ROWS = [
  { T: 'Rash', SEV: 'MILD', ID: 'S01', S: 4, E: 9, V: 'Week 1' },
  { T: 'Nausea', SEV: 'SEVERE', ID: 'S02', S: 2, E: null, V: 'Week 2' },
  { T: 'Rash', SEV: 'SEVERE', ID: 'S02', S: 1, E: 3, V: 'Week 2' }
];
const BASE = { chart_type: 'gantt', x: 'S', xend: 'E', y: 'T', drill: 'auto' };

const build = (cfg = {}, look = {}) => {
  const c = { ...BASE, ...cfg };
  const m = C.model.timeline({ rows: ROWS, ix: new C.RowIndex(), columns: COLUMNS, cfg: c });
  const o = C.option.timeline(m, { cfg: c, columns: COLUMNS, ink: C.INK_DEFAULT, theme: null,
                                   measure, ...look });
  return { m, o, opt: o.panels[0].option };
};

test('one custom series without colour, the lanes on an inverted category y', () => {
  const { o, opt } = build();
  assert.strictEqual(o.panels.length, 1);
  assert.strictEqual(opt.series.length, 1);
  assert.strictEqual(opt.series[0].type, 'custom');
  assert.deepStrictEqual(plain(opt.series[0].encode), { x: [0, 1], y: 2 });
  assert.deepStrictEqual(plain(opt.series[0].data.map((d) => d.value[3])), ['Rash', 'Nausea', 'Rash']);
  assert.deepStrictEqual(plain(opt.yAxis.data), ['Rash', 'Nausea']);
  assert.strictEqual(opt.yAxis.inverse, true);
  assert.strictEqual(opt.xAxis.type, 'value');
  assert.strictEqual(opt.xAxis.name, 'Start');
  assert.strictEqual(opt.backgroundColor, 'transparent');
  assert.deepStrictEqual(plain(opt.legend), { show: false });
  assert.deepStrictEqual(plain(opt.color), plain(C.BLOCKR_PALETTE));
});

test('a series per colour level; a board scale colours the levels', () => {
  const { opt } = build({ color: 'SEV' });
  assert.deepStrictEqual(plain(opt.series.map((s) => [s.name, s.data.length])),
                         [['MILD', 1], ['SEVERE', 2]]);
  assert.deepStrictEqual(plain(opt.legend), { show: false, data: ['MILD', 'SEVERE'] });
  const scaled = build({ color: 'SEV', scales: { var: 'SEV', color: { SEVERE: '#aa0000' } } });
  assert.deepStrictEqual(plain(scaled.opt.color), ['#0072B2', '#aa0000']);
  const legend = C.option.timelineLegend(scaled.m, { scales: { var: 'SEV', color: { SEVERE: '#aa0000' } } });
  assert.deepStrictEqual(plain(legend), { col: 'SEV', items: [
    { name: 'MILD', color: '#0072B2' }, { name: 'SEVERE', color: '#aa0000' }] });
  assert.strictEqual(C.option.timelineLegend(build().m, {}), null);
});

test('panel height: 28 px a lane plus 80, at least 200, capped', () => {
  assert.strictEqual(build().o.panels[0].height, '200px');
  const rows = Array.from({ length: 300 }, (_, i) => ({ T: 'L' + i, S: i, E: i + 1 }));
  const cfg = { ...BASE };
  const m = C.model.timeline({ rows, ix: new C.RowIndex(), columns: COLUMNS, cfg });
  const o = C.option.timeline(m, { cfg, columns: COLUMNS, ink: C.INK_DEFAULT, theme: null, measure });
  assert.strictEqual(o.panels[0].height, (Math.min(C.PANEL_H_CAP, 300 * 28) + 80) + 'px');
});

test('the label gutter is measured, with lane counts in it', () => {
  // "Nausea" is 42 px at 7 px a character, plus 10 slack.
  assert.deepStrictEqual(plain(build().opt.grid), { left: 67, right: 10, top: 20, bottom: 48 });
  const { opt } = build({ count_on: 'axis' });
  assert.strictEqual(opt.yAxis.axisLabel.formatter('Rash'), 'Rash (2)');
  assert.strictEqual(opt.grid.left, 7 * 'Nausea (1)'.length + 10 + 15);
});

test('the tooltip: the lane, the interval and its length, the colour, extra fields', () => {
  const { opt } = build({ color: 'SEV', tt_fields: ['ID'] });
  const tip = opt.tooltip.formatter;
  const bar = tip({ value: [4, 9, 0, 'Rash', 'MILD', '', '', 'Rash', ['S01']] });
  assert.match(bar, /<span>Rash<\/span>/);
  assert.match(bar, /Start<\/span><span class="dd-tt-value">4</);
  assert.match(bar, /E<\/span><span class="dd-tt-value">9 \(5\)</);
  assert.match(bar, /SEV<\/span><span class="dd-tt-value">MILD</);
  assert.match(bar, /ID<\/span><span class="dd-tt-value">S01</);
  // A single day names one date.
  const dot = tip({ value: [2, 2, 1, 'Nausea', '', '', '', 'Nausea', ['S02']] });
  assert.match(dot, /Start<\/span><span class="dd-tt-value">2</);
  assert.doesNotMatch(dot, /\(0\)/);
  // HTML in a value is escaped.
  assert.match(tip({ value: [1, 1, 0, '<b>', '', '', '', '', 0] }), /&lt;b&gt;/);
});

test('a categorical x names its categories in the tooltip', () => {
  const { opt } = build({ x: 'V', xend: null });
  assert.deepStrictEqual(plain(opt.xAxis.data), ['Week 1', 'Week 2']);
  assert.match(opt.tooltip.formatter({ value: [1, 1, 0, 'Rash', '', '', '', 'Rash', 0] }),
               /Week 2/);
});

test('drill off: no emphasis and no hand cursor; a theme drops the transparent background', () => {
  const { opt } = build({ drill: '' });
  assert.deepStrictEqual(plain(opt.series[0].emphasis), { disabled: true });
  assert.strictEqual(opt.series[0].cursor, 'default');
  assert.strictEqual(build({}, { theme: 'dark' }).opt.backgroundColor, undefined);
});

test('the tooltip card takes the ink', () => {
  const ink = { ...C.INK_DEFAULT, raised: '#222222', text: '#eeeeee' };
  const { opt } = build({}, { ink });
  assert.strictEqual(opt.tooltip.backgroundColor, '#222222');
  assert.strictEqual(opt.tooltip.textStyle.color, '#eeeeee');
});

// -- D5 ------------------------------------------------------------------------

const timelinePatch = (cfg, filters, wasDimmed = false) => {
  const c = { ...BASE, ...cfg };
  const m = C.model.timeline({ rows: ROWS, ix: new C.RowIndex(), columns: COLUMNS, cfg: c });
  const lit = filters ? (keys, rows) => C.keys.markLit(keys, rows, filters) : null;
  return plain(C.option.timelinePatch(m, m.panels[0], c, lit, wasDimmed));
};
const barOpacity = (p) => p.series.map((s) => s.data.map((d) => [d.value[3], d.itemStyle.opacity]));

test('D5: a lane filter dims the other lanes; the colour is not a key', () => {
  const p = timelinePatch({ color: 'SEV' }, { T: ['Rash'] });
  assert.deepStrictEqual(barOpacity(p), [[['Rash', 1]], [['Nausea', 0.15], ['Rash', 1]]]);
  assert.strictEqual(p.dimmed, true);
  // A filter naming the colour too is a filter on a column the mark does
  // not carry: the event's own row decides.
  assert.deepStrictEqual(barOpacity(timelinePatch({ color: 'SEV' }, { T: ['Rash'], SEV: ['MILD'] })),
                         [[['Rash', 1]], [['Nausea', 0.15], ['Rash', 0.15]]]);
});

test('D5: a filter on another column lights the events holding it', () => {
  assert.deepStrictEqual(barOpacity(timelinePatch({ drill: 'ID' }, { ID: ['S02'] })),
                         [[['Rash', 0.15], ['Nausea', 1], ['Rash', 1]]]);
});

test('D5: no filter is an empty patch, unless bars were dimmed before', () => {
  assert.deepStrictEqual(timelinePatch({ color: 'SEV' }, null), { series: [{}, {}], dimmed: false });
  assert.deepStrictEqual(barOpacity(timelinePatch({}, null, true)),
                         [[['Rash', 1], ['Nausea', 1], ['Rash', 1]]]);
});
