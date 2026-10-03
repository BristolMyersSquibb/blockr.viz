/* axes-labels.js: the x label ladder, the thinned-label note and the
 * helper lines on a value axis. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure, plain, measure } = require('./load');

const C = loadPure();
const A = C.axes;
const ink = C.INK_DEFAULT;

test('x labels: flat when they fit, wrapped flat, then turned', () => {
  const flat = A.xAxisLabels(['a', 'b'], 400, false, measure, ink);
  assert.strictEqual(flat.bottom, 0);
  assert.strictEqual(flat.axisLabel.rotate, 0);
  // Two words that fit their slot on two lines.
  const wrapped = A.xAxisLabels(['alpha beta', 'gamma delta'], 2 * 60 + 16, false, measure, ink);
  assert.strictEqual(wrapped.axisLabel.rotate, 0);
  assert.strictEqual(wrapped.bottom, A.LABEL_LINE_H);
  assert.strictEqual(wrapped.axisLabel.formatter('alpha beta'), 'alpha\nbeta');
  // A word too wide for its slot turns the labels.
  const turned = A.xAxisLabels(['abcdefghijklmnop', 'b'], 2 * 40 + 16, false, measure, ink);
  assert.strictEqual(turned.axisLabel.rotate, 90);
  assert.strictEqual(turned.axisLabel.interval, 0);
  assert.strictEqual(turned.bottom, 16 * 7 + 8);
  assert.notStrictEqual(turned.key, flat.key);
});

test('x labels: only an ordered axis thins turned labels', () => {
  const labels = Array.from({ length: 40 }, (_, i) => 'Visit number ' + i);
  assert.strictEqual(A.xAxisLabels(labels, 316, false, measure, ink).axisLabel.interval, 0);
  assert.ok(A.xAxisLabels(labels, 316, true, measure, ink).axisLabel.interval > 0);
});

test('wrapLabel breaks a word wider than the line', () => {
  assert.deepStrictEqual(plain(A.wrapLabel(measure, 'abcdef', 21)),
                         { lines: ['abc', 'def'], hard: true });
});

test('a note when a category axis has fewer pixels than labels', () => {
  assert.strictEqual(A.thinnedLabelNote(4000, 100, 'AETERM'), null);
  assert.match(A.thinnedLabelNote(4000, 500, 'AETERM'),
               /^Axis labels thinned: 285 of 500 AETERM labelled/);
});

test('value lines: a markLine on the first series, the axis widened past it', () => {
  const series = [{}, {}];
  const axis = {};
  A.addValueLines(series, axis, false, true, [50, 'x'], ink);
  assert.strictEqual(series[0].markLine.data.length, 1);
  assert.strictEqual(series[0].markLine.data[0].yAxis, 0.5);
  assert.strictEqual(series[0].markLine.data[0].label.formatter, '50%');
  assert.strictEqual(series[1].markLine, undefined);
  // The max is the data's, or the line's rounded out to a nice step.
  assert.strictEqual(axis.max({ min: 0, max: 1 }), 1);
  assert.strictEqual(axis.max({ min: 0, max: 0.4 }), 0.6);
  const none = {};
  A.addValueLines([{}], none, false, false, [], ink);
  assert.strictEqual(none.max, undefined);
});
