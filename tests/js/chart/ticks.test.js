/* Axis ticks: the value read and written (core/ticks.js), what the option
 * carries before the draw and the ticks set after it (chart/ticks.js), the
 * zoom, the gear row and the sentence's words. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadViz, plain } = require('./load');
const D = require('./drive');

const VIZ = loadViz();
const T = VIZ.ticks;
const ZO = VIZ.chart.zoomOption;
const DAY = 864e5;

const LINE = { type: 'line', x: 'ADY', y: 'AVAL', drill: 'auto' };

test('a value reads as a step, a list or auto, and prints as the sentence does', () => {
  assert.deepStrictEqual(plain(T.parse(25)), { form: 'every', n: 25, unit: null });
  assert.deepStrictEqual(plain(T.parse('7 days')), { form: 'every', n: 7, unit: 'day' });
  assert.deepStrictEqual(plain(T.parse('month')), { form: 'every', n: 1, unit: 'month' });
  assert.deepStrictEqual(plain(T.parse([0, 25, 50])), { form: 'at', list: [0, 25, 50] });
  assert.strictEqual(T.parse(null), null);
  assert.strictEqual(T.parse(0), null);
  assert.strictEqual(T.text(25), 'every 25');
  assert.strictEqual(T.text('7 days'), 'every 7 days');
  assert.strictEqual(T.text('1 month'), 'every month');
  assert.strictEqual(T.text([0, 50]), 'at 0, 50');
  assert.strictEqual(T.text(null), 'auto');
  assert.strictEqual(T.step(2, 'week'), '2 weeks');
  assert.strictEqual(T.step(1, 'week'), '1 week');
  assert.strictEqual(T.step(10, null), 10);
});

test('typed text becomes a value', () => {
  assert.strictEqual(T.fromText('25'), 25);
  assert.strictEqual(T.fromText('every 7 days'), '7 days');
  assert.strictEqual(T.fromText('Month'), '1 month');
  assert.deepStrictEqual(plain(T.fromText('at 0, 25, 50')), [0, 25, 50]);
  assert.deepStrictEqual(plain(T.fromText('−50, −20, 0')), [-50, -20, 0]);
  assert.deepStrictEqual(plain(T.fromText('2024-01-01, 2024-02-01')), ['2024-01-01', '2024-02-01']);
  assert.strictEqual(T.fromText('auto'), null);
  assert.strictEqual(T.fromText(''), null);
});

test('a step lands on its multiples; a list on its values in view', () => {
  assert.deepStrictEqual(plain(T.values(25, 3, 96, 'value')), [25, 50, 75]);
  assert.deepStrictEqual(plain(T.values(25, 0, 100, 'value')), [0, 25, 50, 75, 100]);
  assert.deepStrictEqual(plain(T.values(0.1, 0, 0.3, 'value')), [0, 0.1, 0.2, 0.3]);
  assert.deepStrictEqual(plain(T.values([0, 50, 75, 200], 0, 100, 'value')), [0, 50, 75]);
  assert.strictEqual(T.values([50, 200], 0, 100, 'value'), null, 'one tick in view: auto');
  assert.strictEqual(T.values(1, 0, 1000, 'value'), null, 'too many: auto');
  assert.strictEqual(T.values(10, 1, 1000, 'log'), null, 'no step on a log axis');
  assert.deepStrictEqual(plain(T.values([1, 10, 100], 1, 1000, 'log')), [1, 10, 100]);
  assert.deepStrictEqual(plain(T.values([-1, 10, 100], -5, 1000, 'log')), [10, 100]);
  assert.deepStrictEqual(plain(T.roundOut(25, 3, 96)), [0, 100]);
  assert.strictEqual(T.roundOut('7 days', 3, 96), null);
  assert.strictEqual(T.roundOut([0, 50], 3, 96), null);
});

test('a date step counts days and weeks from the start, months from the first', () => {
  const jan3 = Date.UTC(2024, 0, 3);
  assert.deepStrictEqual(plain(T.values('7 days', jan3, jan3 + 20 * DAY, 'time')),
                         [jan3, jan3 + 7 * DAY, jan3 + 14 * DAY]);
  assert.deepStrictEqual(plain(T.values('1 week', jan3, jan3 + 7 * DAY, 'time')), [jan3, jan3 + 7 * DAY]);
  assert.deepStrictEqual(plain(T.values(2, jan3, jan3 + 4 * DAY, 'time')),
                         [jan3, jan3 + 2 * DAY, jan3 + 4 * DAY], 'a bare number is days');
  assert.deepStrictEqual(plain(T.values('month', jan3, Date.UTC(2024, 3, 2), 'time')),
                         [Date.UTC(2024, 1, 1), Date.UTC(2024, 2, 1), Date.UTC(2024, 3, 1)]);
  assert.deepStrictEqual(plain(T.values('quarter', Date.UTC(2024, 1, 10), Date.UTC(2025, 0, 2), 'time')),
                         [Date.UTC(2024, 3, 1), Date.UTC(2024, 6, 1), Date.UTC(2024, 9, 1), Date.UTC(2025, 0, 1)]);
  assert.deepStrictEqual(plain(T.values('year', Date.UTC(2023, 5, 1), Date.UTC(2025, 5, 1), 'time')),
                         [Date.UTC(2024, 0, 1), Date.UTC(2025, 0, 1)]);
  assert.deepStrictEqual(plain(T.values(['2024-01-01', '2024-03-01'], Date.UTC(2023, 11, 1),
                                        Date.UTC(2024, 5, 1), 'time')),
                         [Date.UTC(2024, 0, 1), Date.UTC(2024, 2, 1)]);
});

test('a form picked starts from the ticks drawn now', () => {
  assert.strictEqual(T.start('every', [0, 20, 40], 'value'), 20);
  assert.strictEqual(T.start('every', [], 'value'), 1);
  assert.deepStrictEqual(plain(T.start('at', [0, 20, 40], 'value')), [0, 20, 40]);
  assert.strictEqual(T.start('every', [0, 7 * DAY], 'time'), '1 week');
  assert.strictEqual(T.start('every', [0, 3 * DAY], 'time'), '3 days');
  assert.strictEqual(T.start('every', [0, 31 * DAY], 'time'), '1 month');
  assert.deepStrictEqual(plain(T.start('at', [Date.UTC(2024, 0, 1), Date.UTC(2024, 1, 1)], 'time')),
                         ['2024-01-01', '2024-02-01']);
  assert.strictEqual(T.note(10, true), T.LOG_NOTE);
  assert.strictEqual(T.note([1, 10], true), null);
  assert.strictEqual(T.note([5], false), T.LIST_NOTE);
});

test('before the draw: a step rounds the auto ends out, the limits stay', () => {
  const d = ZO.decorate({ xAxis: { type: 'value', scale: true }, yAxis: { type: 'value', scale: true } },
                        { type: 'line', y_ticks: 25 });
  assert.strictEqual(d.option.yAxis.min({ min: 3, max: 96 }), 0);
  assert.strictEqual(d.option.yAxis.max({ min: 3, max: 96 }), 100);
  assert.strictEqual(d.option.xAxis.min, undefined, 'x has no setting');
  const bar = ZO.decorate({ xAxis: { type: 'category' }, yAxis: { type: 'value' } },
                          { type: 'bar', y_ticks: 25 });
  assert.strictEqual(bar.option.yAxis.min({ min: 30, max: 96 }), 0, 'a bar keeps zero in');
  const lim = ZO.decorate({ xAxis: { type: 'value', scale: true }, yAxis: { type: 'value', scale: true } },
                          { type: 'line', y_ticks: 25, y_limits: [3, null] });
  assert.strictEqual(lim.option.yAxis.min, 3, 'a limit is not rounded');
  assert.strictEqual(lim.option.yAxis.max({ min: 3, max: 96 }), 100);
  const time = ZO.decorate({ xAxis: { type: 'time' }, yAxis: { type: 'value' } },
                           { type: 'line', x_ticks: '7 days' });
  assert.strictEqual(time.option.xAxis.min({ min: 1000, max: 5000 }), 1000, 'a week step starts at the first date');
  const list = ZO.decorate({ xAxis: { type: 'value', scale: true }, yAxis: { type: 'value', scale: true } },
                           { type: 'line', y_ticks: [0, 50] });
  assert.strictEqual(list.option.yAxis.min, undefined, 'a list leaves the range alone');
  const box = ZO.decorate({ xAxis: { type: 'category' }, yAxis: { type: 'value' } },
                          { type: 'boxplot', x_ticks: 10 });
  assert.strictEqual(box.option.yAxis.min, undefined, 'a box plot takes no x ticks');
});

/** The custom tick values on a drawn axis. */
const custom = (c, key) => {
  const ax = c.chart().getOption()[key][0];
  return [ax.axisLabel && ax.axisLabel.customValues, ax.axisTick && ax.axisTick.customValues];
};

test('after the draw: the axis takes its ticks over its extent', () => {
  // The fake ECharts draws y over 20 to 42 and ignores the rounded ends.
  const c = D.open({ ...LINE, y_ticks: 5 });
  const [labels, ticks] = custom(c, 'yAxis');
  assert.deepStrictEqual(plain(labels), [20, 25, 30, 35, 40], 'on the multiples of 5');
  assert.deepStrictEqual(plain(ticks), plain(labels), 'ticks and labels agree');
  assert.strictEqual(custom(c, 'xAxis')[0], undefined, 'x draws its own');
  c.send({ ...LINE, y_ticks: [0, 30, 40] });
  assert.deepStrictEqual(plain(custom(c, 'yAxis')[0]), [30, 40], 'a list, inside the extent');
  c.send({ ...LINE, y_ticks: null });
  assert.ok(custom(c, 'yAxis')[0] == null, 'auto again');
  c.close();
});

test('zoomed in past the ticks, the axis draws its own until the reset', () => {
  const c = D.open({ ...LINE, y_ticks: 5 });
  const slot = c.block._slots[0];
  const vals = slot.ticks.yAxis.vals;
  c.env.win.Blockr.viz.chart.ticks.zoomed(slot, { xAxis: null, yAxis: [21, 24] });
  assert.ok(custom(c, 'yAxis')[0] == null, 'no tick in the window: auto');
  c.env.win.Blockr.viz.chart.ticks.zoomed(slot, { yAxis: [vals[0], vals[vals.length - 1]] });
  assert.deepStrictEqual(plain(custom(c, 'yAxis')[0]), plain(vals), 'the ticks come back');
  // An axis with ticks of its own (a log axis's 1-2-5) gets those back, not none.
  slot.ticks.yAxis.own = [22, 33];
  c.env.win.Blockr.viz.chart.ticks.zoomed(slot, { xAxis: null, yAxis: [21, 24] });
  assert.deepStrictEqual(plain(custom(c, 'yAxis')[0]), [22, 33], 'the axis keeps its own ticks');
  c.close();
});

test('the sentence draws a word per part; a form picked starts from the drawn ticks', () => {
  const c = D.open({ ...LINE, x_ticks: '7 days', y_ticks: 25 });
  const g = c.block.gear;
  const y = g.slot('y_ticks');
  assert.strictEqual(y.kind, 'words');
  assert.deepStrictEqual(plain(y.words.map((w) => w.text)), ['every', '25']);
  y.words[1].how.set(10);
  assert.deepStrictEqual(plain(c.sets().pop()), { y_ticks: 10 });
  y.words[0].how.set('at');
  assert.ok(Array.isArray(c.sets().pop().y_ticks), 'At sends a list');
  y.words[0].how.set('auto');
  assert.deepStrictEqual(plain(c.sets().pop()), { y_ticks: null });
  const x = g.slot('x_ticks');
  assert.deepStrictEqual(plain(x.words.map((w) => w.text)), ['every', '7', 'days']);
  x.words[2].how.set('week');
  assert.deepStrictEqual(plain(c.sets().pop()), { x_ticks: '7 weeks' });
  c.close();
});

test('on a log axis the sentence offers no step', () => {
  const c = D.open({ type: 'scatter', x: 'ADY', y: 'AVAL', y_transform: 'log10', y_ticks: [1, 10] });
  const y = c.block.gear.slot('y_ticks');
  assert.deepStrictEqual(plain(y.words[0].how.options.map((o) => o.value)), ['auto', 'at']);
  c.close();
});

test('the gear row: Auto | Every | At and the fields of the form', () => {
  const c = D.open({ ...LINE, y_ticks: 25 });
  c.block.frame.tray.set(true);
  const row = () => c.block.frame.tray.band.querySelector('.vz-row[data-role="y_ticks"]');
  assert.ok(row(), 'the row shows');
  const segs = Array.from(row().querySelectorAll('.blockr-segmented__seg'), (b) => b.textContent);
  assert.deepStrictEqual(segs, ['Auto', 'Every', 'At']);
  const step = row().querySelector('.vz-ticks-step');
  assert.strictEqual(step.value, '25');
  step.value = '10';
  step.dispatchEvent(new c.env.win.KeyboardEvent('keydown', { key: 'Enter' }));
  assert.deepStrictEqual(plain(c.sets().pop()), { y_ticks: 10 });
  row().querySelectorAll('.blockr-segmented__seg')[2].click();
  assert.ok(Array.isArray(c.sets().pop().y_ticks), 'At sends a list');
  c.close();
});

test('a date step shows its unit in the gear row', () => {
  const c = D.open({ ...LINE, x_ticks: '7 days' });
  c.block.frame.tray.set(true);
  const row = c.block.frame.tray.band.querySelector('.vz-row[data-role="x_ticks"]');
  assert.strictEqual(row.querySelector('.vz-ticks-step').value, '7');
  assert.ok(row.querySelector('.vz-ticks-unit'), 'the unit pick shows');
  c.close();
});
