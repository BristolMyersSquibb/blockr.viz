/* What survives a redraw from a new payload: a line's zoom window, a brush
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

test('a line zoom window survives a new payload; a reset forgets it', () => {
  const c = D.open(LINE);
  const slot = () => c.block._slots[0];
  c.trigger('datazoom', { batch: [{ startValue: 10, endValue: 60, start: 10, end: 70 }] });
  assert.strictEqual(slot().zoom.startValue, 10);
  assert.strictEqual(slot().resetBtn.hidden, false);
  c.send({ ...LINE, color: 'ARM' }, { data: null });
  assert.strictEqual(slot().zoom.endValue, 60, 'kept across the redraw');
  c.trigger('datazoom', { start: 0, end: 100 });
  assert.strictEqual(slot().zoom, null);
  assert.strictEqual(slot().resetBtn.hidden, true);
  c.close();
});

test('a brush filter and a drill receipt survive a new payload', () => {
  const c = D.open(SCATTER);
  c.trigger('brushSelected', { batch: [{ selected: [{ seriesIndex: 0, dataIndex: [0, 1] }] }] });
  const before = c.footer();
  c.send({ ...SCATTER, point_size: 2 }, { data: null });
  assert.strictEqual(c.footer(), before);
  c.close();

  const t = D.open({ type: 'bar', y: 'AETERM', orientation: 'horizontal', drill: 'auto',
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
  c.zr('globalout', {});
  assert.strictEqual(slot.hover.si, null);
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
