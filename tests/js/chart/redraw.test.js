/* What survives a redraw from a new payload (the zoom window: zoom.test.js): a brush
 * filter, a drill receipt at its own age; and the pointer: the line under
 * the cursor takes the focus, and the brush cursor being armed does not
 * clear what a click or a restored range selected. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const D = require('./drive');

const LINE = { type: 'line', x: 'ADY', y: 'AVAL', series: 'USUBJID', drill: 'auto' };
const SCATTER = { type: 'scatter', x: 'ADY', y: 'AVAL', drill: 'auto' };
const armed = (c) => c.trigger('brushSelected', { batch: [{ areas: [], selected: [] }] });

test('a brush filter and a drill receipt survive a new payload', () => {
  const c = D.open(SCATTER);
  c.trigger('brushSelected', { batch: [{ selected: [{ seriesIndex: 0, dataIndex: [0, 1] }] }] });
  const before = c.footer();
  c.send({ ...SCATTER, point_size: 2 }, { data: null });
  assert.strictEqual(c.footer(), before);
  c.close();

  const t = D.open({ type: 'bar', category: 'AETERM', orientation: 'horizontal', drill: 'auto',
                     drill_target: 'auto' });
  t.clickWhere(0, D.at('Nausea'));
  t.advance(1000);
  t.send({ ...t.config, value_labels: true }, { data: null });
  const rec = t.block.frame.statusEl.querySelector('.vz-status-receipt');
  assert.ok(rec, 'the receipt is still there');
  assert.match(rec.style.animation, /-?\d+(\.\d+)?ms both$/);
  assert.ok(rec.style.animation.includes(' 400ms both'), 'resumed at its own age: ' + rec.style.animation);
  t.advance(4000);
  assert.strictEqual(t.block.frame.statusEl.querySelector('.vz-status-receipt'), null);
  t.close();
});

test('the line under the cursor takes the focus; leaving the plot drops it', () => {
  const c = D.open(LINE);
  const slot = c.block._slots[0];
  const series = c.chart().getOption().series;
  const si = series.findIndex((s) => s.type === 'line' && s.name && (s.data || []).length > 1);
  const [x, y] = [].concat(series[si].data[0].value || series[si].data[0]);
  c.zr('mousemove', { offsetX: x, offsetY: y });
  assert.notStrictEqual(slot.focusSi, null, 'a line has the focus');
  // In a crowd of lines the hovered line is drawn over a veil.
  const overlay = (id) => c.chart().getOption().series.find((s) => s.id === id);
  assert.ok(overlay('__focus__').data.length > 1, 'the hovered line on top');
  assert.strictEqual(overlay('__scrim__').data.length, 1, 'the veil over the rest');
  // A dot at each of its points, in its colour, ringed in the surface colour.
  const dots = overlay('__focus_dots__');
  assert.strictEqual(dots.data.length, overlay('__focus__').data.length);
  assert.strictEqual(dots.symbolSize, 8);
  assert.strictEqual(dots.itemStyle.borderWidth, 2);
  assert.strictEqual(dots.itemStyle.color, overlay('__focus__').lineStyle.color);
  c.zr('globalout', {});
  assert.strictEqual(slot.hover.si, null);
  assert.strictEqual(overlay('__focus__').data.length, 0);
  assert.strictEqual(overlay('__scrim__').data.length, 0);
  assert.strictEqual(overlay('__focus_dots__').data.length, 0);
  c.close();
});

test('a colour split of a few lines: hover draws nothing', () => {
  const c = D.open({ ...LINE, series: null, color: 'ARM' });
  const before = c.chart().log.length;
  c.zr('mousemove', { offsetX: 10, offsetY: 10 });
  assert.strictEqual(c.block._slots[0].focus, null);
  assert.strictEqual(c.chart().log.length, before, 'nothing redrawn');
  c.close();
});

test('the brush cursor being armed keeps a point click', () => {
  const c = D.open(SCATTER);
  c.click(0, 0, 0);
  c.advance(200);
  armed(c);
  assert.strictEqual(c.selections().length, 1);
  assert.strictEqual(c.block.selection.current.type, 'range');
  c.close();
});

test('an empty selection still clears a brush; a brush clear clears any filter', () => {
  const c = D.open(SCATTER);
  c.trigger('brushSelected', { batch: [{ selected: [{ seriesIndex: 0, dataIndex: [0, 1] }] }] });
  armed(c);
  assert.strictEqual(c.block.selection.current, null);
  assert.strictEqual(c.selections().length, 2);
  c.close();
});

// A click's payload changes the selection, the download counts and the data
// view, none of which the chart draws: no setOption with notMerge.
test('a payload that only moves the selection updates in place', () => {
  const c = D.open(LINE);
  const redraws = () => c.chart().log.filter((e) => e.op === 'setOption' && e.notMerge).length;
  const base = c.config;
  const before = redraws();
  c.clickWhere(0, () => true);
  c.advance(300);
  const sel = c.selections()[0];
  assert.ok(sel, 'the click sent a selection');
  const dl = (count) => ({ downloads: [{ id: 'dl', label: 'Rows', ext: 'csv', section: 'Data', count }] });
  c.send({ ...base, selection: sel }, { data: null, derived: dl('5 of 40') });
  assert.strictEqual(redraws(), before, 'not redrawn');
  assert.strictEqual(JSON.stringify(c.block.selection.current.filters), JSON.stringify(sel.filters));
  assert.strictEqual(c.block.derived.downloads[0].count, '5 of 40');
  // R clears it: the marks are lit again, still without a redraw.
  c.send({ ...base, selection: null }, { data: null, derived: dl('40 rows') });
  assert.strictEqual(redraws(), before);
  assert.strictEqual(c.block.selection.current, null);
  // A setting the chart draws still redraws.
  c.send({ ...base, line_width: 3 }, { data: null, derived: dl('40 rows') });
  assert.ok(redraws() > before, 'a drawn setting redraws');
  c.close();
});

// A payload whose changed field the option does not read finds the memoised
// option: the chart is not set again.
test('a setting the option does not read leaves the chart alone', () => {
  const c = D.open(LINE);
  const sets = () => c.chart().log.filter((e) => e.op === 'setOption').length;
  c.advance(300);
  const before = sets();
  c.send({ ...c.config, title: 'ALT' }, { data: null });
  assert.strictEqual(sets(), before, 'no setOption');
  c.send({ ...c.config, line_width: 3 }, { data: null });
  assert.ok(sets() > before, 'a drawn setting still sets the option');
  c.close();
});

// The settle resize 300 ms after a draw: once per chart, then only when the
// canvas differs from its box.
test('the settle resize runs on the first draw and when the size differs', () => {
  const c = D.open(LINE);
  const resizes = () => c.chart().log.filter((e) => e.op === 'resize').length;
  c.advance(300);
  assert.strictEqual(resizes(), 1, 'first draw');
  c.send({ ...c.config, line_width: 3 }, { data: null });
  c.advance(300);
  assert.strictEqual(resizes(), 1, 'same size: no resize');
  const chart = c.chart();
  const w = chart.getWidth;
  chart.getWidth = () => w() - 10;
  c.send({ ...c.config, line_width: 4 }, { data: null });
  c.advance(300);
  assert.strictEqual(resizes(), 2, 'size differs: resize');
  c.close();
});

// An aggregated draw ends in a highlight patch; with nothing selected it is
// empty and is not sent.
test('no empty highlight patch after a draw with no selection', () => {
  const c = D.open({ type: 'bar', category: 'AETERM', orientation: 'horizontal', drill: 'auto' });
  const empty = c.chart().log.filter((e) => e.op === 'setOption' && !e.notMerge &&
    e.option && Array.isArray(e.option.series) && e.option.series.every((s) => !Object.keys(s).length));
  assert.strictEqual(empty.length, 0);
  c.close();
});
