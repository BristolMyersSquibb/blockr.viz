/* y_scale / x_scale = "log": the ticks, which types and axes take it, values
 * at or below zero left out and counted, intervals cut at the floor, shared
 * facet domains, the identity line and the band. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure, plain, measure } = require('./load');

const C = loadPure({ roles: true });

const cat = (name) => ({ name, type: 'categorical' });
const num = (name) => ({ name, type: 'numeric' });
const COLUMNS = [cat('CTRY'), cat('REG'), num('YEAR'), num('GDP'), num('LIFE'), num('LO'), num('HI')];
const ROWS = [
  { CTRY: 'CH', REG: 'EU', YEAR: 2000, GDP: 60000, LIFE: 80, LO: 50000, HI: 70000 },
  { CTRY: 'CH', REG: 'EU', YEAR: 2010, GDP: 80000, LIFE: 82, LO: 70000, HI: 90000 },
  { CTRY: 'IN', REG: 'AS', YEAR: 2000, GDP: 450, LIFE: 62, LO: -100, HI: 900 },
  { CTRY: 'IN', REG: 'AS', YEAR: 2010, GDP: 1300, LIFE: 66, LO: 1000, HI: 1600 },
  { CTRY: 'XX', REG: 'AS', YEAR: 2000, GDP: 0, LIFE: 50, LO: null, HI: null },
  { CTRY: 'XX', REG: 'AS', YEAR: 2010, GDP: -5, LIFE: 51, LO: null, HI: null }
];
const BASE = { chart_type: 'scatter', x: 'GDP', y: 'LIFE' };

const build = (cfg, rows = ROWS) => {
  const c = { ...BASE, ...cfg };
  const m = C.model.individual({ rows, ix: new C.RowIndex(), columns: COLUMNS, cfg: c });
  const o = C.option.individual(m, { cfg: c, columns: COLUMNS, ink: C.INK_DEFAULT, theme: null,
                                     measure, widths: [600, 600, 600] });
  return { m, o, opt: o.panels[0].option };
};
const ax = (a) => (Array.isArray(a) ? a[0] : a);

test('log ticks: 1-2-5 up to three decades, powers of ten beyond, ends on ticks', () => {
  const t = plain(C.axes.logTicks(190, 88000));
  assert.deepStrictEqual([t.min, t.max], [100, 100000]);
  assert.deepStrictEqual(t.ticks.slice(0, 4), [100, 200, 500, 1000]);
  assert.deepStrictEqual(plain(C.axes.logTicks(3, 9)).ticks, [3, 5, 7, 10]);
  assert.deepStrictEqual(plain(C.axes.logTicks(0.5, 2e5)).ticks, [0.1, 1, 10, 100, 1000, 10000, 100000, 1000000]);
  // One value: a step either side.
  assert.deepStrictEqual(plain(C.axes.logTicks(5, 5)), { min: 3, max: 7, ticks: [3, 5, 7] });
  assert.strictEqual(C.axes.logTicks(0, 10), null);
  assert.strictEqual(C.axes.logTicks(NaN, 10), null);
});

test('which charts take a log axis', () => {
  const on = (chart_type) => plain(C.logAxes({ chart_type, y_scale: 'log', x_scale: 'log' }));
  assert.deepStrictEqual(on('scatter'), { x: true, y: true });
  assert.deepStrictEqual(on('line'), { x: false, y: true });
  assert.deepStrictEqual(on('band'), { x: false, y: true });
  for (const t of ['bar', 'area', 'histogram', 'boxplot', 'gantt']) assert.deepStrictEqual(on(t), { x: false, y: false });
  assert.deepStrictEqual(plain(C.logAxes({ chart_type: 'scatter' })), { x: false, y: false });
});

test('a log x axis: its own ticks, rows at or below zero left out and counted', () => {
  const { m, o, opt } = build({ x_scale: 'log' });
  assert.strictEqual(m.logDropped, 2);
  const x = ax(opt.xAxis);
  assert.strictEqual(x.type, 'log');
  assert.strictEqual(x.logBase, 10);
  assert.strictEqual(x.scale, undefined);
  assert.deepStrictEqual([x.min, x.max], [200, 100000]);
  assert.deepStrictEqual(plain(x.axisLabel.customValues), plain(x.axisTick.customValues));
  assert.strictEqual(ax(opt.yAxis).type, 'value');
  const drawn = opt.series.filter((s) => s.type === 'scatter').flatMap((s) => s.data);
  assert.strictEqual(drawn.length, 4);
  assert.ok(drawn.every((p) => (Array.isArray(p) ? p[0] : p.value[0]) > 0));
  assert.strictEqual(o.labelNote, 'Log scale: 2 values at or below zero not shown.');
});

test('linear axes draw every row and say nothing', () => {
  const { m, o, opt } = build({});
  assert.strictEqual(m.logDropped, 0);
  assert.strictEqual(ax(opt.xAxis).type, 'value');
  assert.strictEqual(o.labelNote, null);
});

test('a line takes y_scale and ignores x_scale; an area ignores both', () => {
  const line = build({ chart_type: 'line', x: 'YEAR', y: 'GDP', series: 'CTRY', y_scale: 'log', x_scale: 'log' });
  assert.strictEqual(ax(line.opt.yAxis).type, 'log');
  assert.strictEqual(ax(line.opt.xAxis).type, 'value');
  assert.strictEqual(line.m.logDropped, 2);
  // The hover picks lines in logs.
  assert.strictEqual(line.o.panels[0].focus.yLog, true);
  const area = build({ chart_type: 'area', x: 'YEAR', y: 'GDP', y_scale: 'log' });
  assert.strictEqual(ax(area.opt.yAxis).type, 'value');
  assert.strictEqual(area.m.logDropped, 0);
});

test('a guide line at or below zero is left out and counted', () => {
  const { o, opt } = build({ chart_type: 'line', x: 'YEAR', y: 'GDP', series: 'CTRY', y_scale: 'log',
                             value_lines: [0, 1000] });
  const ml = opt.series.find((s) => s.markLine).markLine.data;
  assert.strictEqual(ml.length, 1);
  assert.strictEqual(o.labelNote, 'Log scale: 3 values at or below zero not shown.');
});

test('an interval end at or below zero: the low end is cut at the floor', () => {
  const { opt } = build({ chart_type: 'scatter', x: 'YEAR', y: 'GDP', y_scale: 'log', lo: 'LO', hi: 'HI' });
  const y = ax(opt.yAxis);
  const bars = opt.series.find((s) => s.type === 'custom');
  const lows = bars.data.map((d) => d[1]);
  assert.ok(lows.includes(y.min));
  assert.ok(lows.every((v) => v > 0));
});

test('shared facet scales share one log domain; free scales do not', () => {
  const cfg = { x_scale: 'log', facet: 'REG' };
  const fixed = build(cfg).o.panels.map((p) => [ax(p.option.xAxis).min, ax(p.option.xAxis).max]);
  assert.deepStrictEqual(plain(fixed[0]), plain(fixed[1]));
  const free = build({ ...cfg, facet_scales: 'free' }).o.panels.map((p) => ax(p.option.xAxis).min);
  assert.notStrictEqual(free[0], free[1]);
  // free_y keeps x shared.
  const fy = build({ ...cfg, facet_scales: 'free_y' }).o.panels.map((p) => ax(p.option.xAxis).min);
  assert.strictEqual(fy[0], fy[1]);
});

test('the identity line: on log-log over one domain, gone when the scales differ', () => {
  const both = build({ x: 'LO', y: 'HI', x_scale: 'log', y_scale: 'log', identity_line: 'on' });
  const x = ax(both.opt.xAxis), y = ax(both.opt.yAxis);
  assert.deepStrictEqual([x.min, x.max], [y.min, y.max]);
  assert.ok(both.opt.series.some((s) => s.markLine));
  const mixed = build({ x: 'LO', y: 'HI', x_scale: 'log', identity_line: 'on' });
  assert.ok(!mixed.opt.series.some((s) => s.markLine));
});

test('a log band: a low edge at zero sits on the floor, an outlier there is counted', () => {
  const band_series = { __all__: { __all__: {
    x: [2000, 2005, 2010], center: [5, 8, 20], lo: [0, 4, 10], hi: [9, 15, 40],
    olo: [-2, 2, 6], ohi: [12, 20, 60], out_x: [2005, 2010], out_y: [0, 90]
  } } };
  const { o, opt } = build({ chart_type: 'band', x: 'YEAR', y: 'GDP', y_scale: 'log', band_series,
                             box_points: 'outliers' });
  const y = ax(opt.yAxis);
  assert.strictEqual(y.type, 'log');
  assert.ok(y.min > 0 && y.max >= 90);
  const outl = opt.series.find((s) => s.type === 'scatter');
  assert.deepStrictEqual(plain(outl.data.map((p) => p[1])), [90]);
  // The model leaves band rows alone; the outlier at zero is the one counted.
  assert.strictEqual(o.labelNote, 'Log scale: 1 value at or below zero not shown.');
});
