/* Brush: what a brushSelected / brush event sends for scatter (the brushable
 * family member), with and without a colour split, a facet and a drill
 * column, and how a click guards against the brush clear that follows it.
 * chart-brush.test.js covers the large-cloud case. */
'use strict';

const test = require('node:test');
const I = require('./interact');

const LAB = { x: 'ADY', y: 'AVAL', drill: 'auto' };

/** brushSelected over the given [seriesIndex, [dataIndex...]] pairs. */
const brush = (pairs, slot = 0) => [
  `brushSelected ${JSON.stringify(pairs)}${slot ? ' in slot ' + slot : ''}`,
  (c) => c.trigger('brushSelected', {
    batch: [{ selected: pairs.map(([seriesIndex, dataIndex]) => ({ seriesIndex, dataIndex })) }]
  }, slot)
];
const brushEmpty = (slot = 0) => ['brushSelected with nothing selected',
  (c) => c.trigger('brushSelected', { batch: [{ selected: [] }] }, slot)];
const brushNoSelected = () => ['brushSelected whose batch has no selected list',
  (c) => c.trigger('brushSelected', { batch: [{}] })];
const brushNoBatch = () => ['brushSelected with an empty batch',
  (c) => c.trigger('brushSelected', { batch: [] })];
const brushCleared = () => ['brush event with no areas', (c) => c.trigger('brush', { areas: [] })];
const brushArea = () => ['brush event with an area',
  (c) => c.trigger('brush', { areas: [{ brushType: 'rect' }] })];
const clickFirst = () => ['click series 0 datum 0', (c) => c.click(0, 0, 0)];
const wait = (ms) => [`advance ${ms} ms`, (c) => c.advance(ms)];
const reset = () => ['Reset', (c) => c.reset()];

const scenario = (name, config, steps, opts) =>
  test(name, () => I.snap('brush-' + name, I.runSteps(config, steps, opts)));

scenario('scatter-geometric', { chart_type: 'scatter', ...LAB }, [
  brush([[0, [0, 1, 2, 3]]]),
  brushEmpty(),
  brush([[0, [5, 9]]]),
  brushCleared(),
  brush([[0, [5, 9]]]),
  brushNoSelected(),
  brush([[0, [7]]]),
  reset()
]);

scenario('scatter-ignored-events', { chart_type: 'scatter', ...LAB }, [
  brushNoBatch(),
  brushArea(),
  // Indices past the end of the series are skipped.
  brush([[0, [9999]]]),
  brush([[0, [-1, 2]]])
]);

scenario('scatter-color', { chart_type: 'scatter', ...LAB, color: 'ARM' }, [
  brush([[1, [0, 1]], [3, [0]]]),
  brushEmpty()
]);

scenario('scatter-facet', { chart_type: 'scatter', ...LAB, color: 'AESEV', facet: 'ARM' }, [
  brush([[1, [0, 1, 2]], [3, [0]]], 2),
  brush([[2, [0]]], 1)
]);

scenario('scatter-drill-column', { chart_type: 'scatter', ...LAB, color: 'ARM',
                                   drill: 'USUBJID' }, [
  brush([[1, [0, 1, 2]], [2, [0]]])
]);

scenario('scatter-drill-off', { chart_type: 'scatter', ...LAB, drill: '' }, [
  brush([[0, [0, 1]]]), brushEmpty(), brushCleared()
]);

// A brush is not a claim: with a ctrl_target it still latches like a
// latched-mode brush.
scenario('scatter-transient', { chart_type: 'scatter', ...LAB, color: 'ARM',
                                ctrl_target: 'auto' }, [
  brush([[1, [0, 1]]]),
  brushEmpty()
]);

// The click guard: a click arms a 150 ms window in which brush clears are
// ignored.
scenario('scatter-click-guard', { chart_type: 'scatter', ...LAB }, [
  clickFirst(),
  brushEmpty(),
  brushCleared(),
  wait(150),
  brushCleared()
]);

// Line charts are not armed for brushing, but the handler is attached; an
// event that reaches it filters on x only.
scenario('line-event', { chart_type: 'line', ...LAB }, [
  brush([[0, [0, 3]]]),
  brushEmpty()
]);

// Two points at the same (x, y): their rows are taken once.
scenario('scatter-duplicate-points', { chart_type: 'scatter', x: 'X', y: 'Y', drill: 'ID' }, [
  brush([[0, [0, 1, 2]]])
], {
  columns: [{ name: 'X', type: 'numeric', n_unique: 2 }, { name: 'Y', type: 'numeric', n_unique: 2 },
            { name: 'ID', type: 'categorical', n_unique: 4 }],
  data: { X: [1, 1, 2, 2], Y: [5, 5, 6, 6], ID: ['a', 'b', 'c', 'd'] }
});
