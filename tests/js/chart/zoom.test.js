/* The drag zoom and the axis limits: what the option carries
 * (zoom-option.js) and what a drag, the links and the Select / Zoom switch
 * do (zoom.js). */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure, plain } = require('./load');
const D = require('./drive');

const NS = loadPure();
const ZO = NS.zoomOption;

const LINE = { type: 'line', x: 'ADY', y: 'AVAL', drill: 'auto' };
const SCATTER = { type: 'scatter', x: 'ADY', y: 'AVAL', drill: 'auto' };
const BOX = { type: 'boxplot', category: 'ARM', value: 'AVAL', orientation: 'vertical' };

test('every continuous axis zooms; the aggregated types only their value axis', () => {
  const line = ZO.decorate({ xAxis: { type: 'value' }, yAxis: { type: 'value' }, series: [] },
                           { type: 'line' });
  assert.deepStrictEqual(plain(line.roles), { xAxis: 'x', yAxis: 'y' });
  assert.deepStrictEqual(plain(line.option.dataZoom.map((z) => [z.id, z.filterMode])),
                         [['vz-zoom-x', 'filter'], ['vz-zoom-y', 'none']]);
  const bar = ZO.decorate({ xAxis: { type: 'value' }, yAxis: { type: 'category' } }, { type: 'bar' });
  assert.deepStrictEqual(plain(bar.roles), { xAxis: 'y', yAxis: null });
  const box = ZO.decorate({ xAxis: {}, yAxis: {}, series: [{ type: 'custom' }] }, { type: 'boxplot' });
  assert.deepStrictEqual(plain(box.roles), { xAxis: null, yAxis: 'y' });
  assert.strictEqual(box.option.series[0].clip, true, 'a box is clipped');
  const gantt = ZO.decorate({ xAxis: { type: 'time' }, yAxis: { type: 'category' } }, { type: 'gantt' });
  assert.deepStrictEqual(plain(gantt.roles), { xAxis: 'x', yAxis: null });
  const pie = { series: [] };
  assert.strictEqual(ZO.decorate(pie, { type: 'pie' }).option, pie);
  const band = ZO.decorate({ xAxis: { type: 'value' }, yAxis: {} }, { type: 'band' });
  assert.strictEqual(band.option.dataZoom[0].filterMode, 'none', 'a band is not filtered');
});

test('limits become the axis min and max, on the value axis whichever way it lies', () => {
  const opt = { xAxis: [{ type: 'time' }, { show: false }], yAxis: { type: 'value' } };
  const d = ZO.decorate(opt, { type: 'line', x_limits: ['2020-01-01', null], y_limits: [0, 120] });
  assert.strictEqual(d.option.xAxis[0].min, Date.UTC(2020, 0, 1));
  assert.strictEqual(d.option.xAxis[0].max, undefined);
  assert.strictEqual(d.option.xAxis[1].show, false, 'the veil axis is kept');
  assert.deepStrictEqual([d.option.yAxis.min, d.option.yAxis.max], [0, 120]);
  assert.strictEqual(opt.yAxis.min, undefined, 'the option passed in is unchanged');
  const bar = ZO.decorate({ xAxis: { type: 'value' }, yAxis: { type: 'category' } },
                          { type: 'bar', y_limits: [10, null], x_limits: [1, 2] });
  assert.strictEqual(bar.option.xAxis.min, 10);
  assert.strictEqual(bar.option.yAxis.min, undefined);
  const log = ZO.decorate({ xAxis: { type: 'value' }, yAxis: { type: 'log' } },
                          { type: 'scatter', y_limits: [0, 50] });
  assert.deepStrictEqual([log.option.yAxis.min, log.option.yAxis.max], [undefined, 50],
                         'a log axis ignores a limit at zero');
  assert.deepStrictEqual(plain(ZO.ends({ type: 'line', y_limits: ['1', 'x'] }, 'y', 'value')), [1, null]);
});

/** The chart with a 400 x 300 plot at the origin; the fake converts pixels to equal values. */
const open = (cfg) => {
  const c = D.open(cfg);
  const grid = () => {
    const ch = c.chart();
    if (ch.__grid) return;
    const orig = ch.getModel;
    ch.getModel = () => {
      const m = orig();
      return { getComponent: (k, i) => (k === 'grid'
        ? { coordinateSystem: { getRect: () => ({ x: 0, y: 0, width: 400, height: 300 }) } }
        : m.getComponent(k, i)) };
    };
    ch.__grid = true;
  };
  grid();
  c.drag = (a, b) => {
    grid();
    c.zr('mousedown', { offsetX: a[0], offsetY: a[1] });
    c.zr('mousemove', { offsetX: b[0], offsetY: b[1] });
    c.zr('mouseup', { offsetX: b[0], offsetY: b[1] });
  };
  c.slot = () => c.block._slots[0];
  c.zooms = () => c.chart().log.filter((x) => x.op === 'dispatchAction' && x.action.type === 'dataZoom');
  return c;
};

test('a zoom window gets tidy ends', () => {
  const c = D.open(LINE);
  const Z = c.env.win.Blockr.viz.chart.zoom;
  assert.deepStrictEqual(plain(Z.tidy([87.86, 124.3], 'value')), [87, 125]);
  assert.deepStrictEqual(plain(Z.tidy([0.1234, 0.1567], 'value')), [0.123, 0.157]);
  assert.deepStrictEqual(plain(Z.tidy([5, 5], 'value')), [5, 5]);
  const day = 864e5;
  assert.deepStrictEqual(plain(Z.tidy([day * 10.5, day * 20.2], 'time')), [day * 10, day * 21]);
  assert.deepStrictEqual(plain(Z.tidy([1000, 2000], 'time')), [1000, 2000]);
  c.close();
});

test('a drag box zooms x and y; a flat drag x only, refitting y', () => {
  const c = open(LINE);
  c.drag([10, 20], [110, 120]);
  assert.deepStrictEqual(plain(c.slot().zoom), { xAxis: [10, 110], yAxis: [20, 120] });
  assert.deepStrictEqual(plain(c.zooms().pop().action.batch), [
    { dataZoomId: 'vz-zoom-x', startValue: 10, endValue: 110 },
    { dataZoomId: 'vz-zoom-y', startValue: 20, endValue: 120 }]);
  assert.strictEqual(c.slot().resetBtn.hidden, false);
  assert.strictEqual(c.slot().keepBtn.hidden, false);
  c.drag([30, 50], [80, 53]);
  assert.deepStrictEqual(plain(c.slot().zoom), { xAxis: [30, 80], yAxis: null });
  const n = c.zooms().length;
  c.drag([30, 50], [33, 52]);
  assert.strictEqual(c.zooms().length, n, 'a short drag zooms nothing');
  c.zr('mousedown', { offsetX: 500, offsetY: 10 });
  c.zr('mouseup', { offsetX: 100, offsetY: 100 });
  assert.strictEqual(c.zooms().length, n, 'a drag from outside the plot zooms nothing');
  c.close();
});

test('the window survives a new payload; the link and a double-click reset it', () => {
  const c = open(LINE);
  c.drag([10, 20], [110, 120]);
  c.send({ ...LINE, line_width: 2 }, { data: null });
  assert.deepStrictEqual(plain(c.slot().zoom.xAxis), [10, 110], 'kept across the redraw');
  c.slot().resetBtn.click();
  assert.strictEqual(c.slot().zoom, null);
  assert.strictEqual(c.slot().resetBtn.hidden, true);
  assert.deepStrictEqual(plain(c.zooms().pop().action.batch[0]), { dataZoomId: 'vz-zoom-x', start: 0, end: 100 });
  c.drag([10, 20], [110, 120]);
  c.zr('dblclick');
  assert.strictEqual(c.slot().zoom, null);
  c.drag([10, 20], [110, 120]);
  c.send({ ...BOX }, { data: null });
  assert.strictEqual(c.slot().zoom, null, 'a type with other axes forgets it');
  c.close();
});

test('a box plot zooms its value axis only', () => {
  const c = open(BOX);
  c.drag([10, 20], [110, 120]);
  assert.deepStrictEqual(plain(c.slot().zoom), { yAxis: [20, 120] });
  c.close();
});

test('a click that ends a drag is no click', () => {
  const c = open({ ...LINE, series: 'USUBJID' });
  c.drag([10, 20], [110, 120]);
  c.click(0, 1, 0);
  c.advance(400);
  assert.strictEqual(c.selections().length, 0);
  c.close();
});

test('a scatter drag brushes until the switch gives it to the zoom', () => {
  const c = open(SCATTER);
  const mode = c.slot().modeEl;
  assert.ok(mode && !mode.hidden, 'the switch shows over a brushable scatter');
  const [sel, zoom] = mode.querySelectorAll('button');
  assert.ok(sel.classList.contains('is-on'));
  c.drag([10, 20], [110, 120]);
  assert.strictEqual(c.slot().zoom, null, 'Select: the drag is the brush');
  zoom.click();
  assert.ok(zoom.classList.contains('is-on'));
  const cursor = c.chart().log.filter((x) => x.op === 'dispatchAction' && x.action.key === 'brush').pop();
  assert.strictEqual(cursor.action.brushOption.brushType, false, 'the brush is released');
  c.drag([10, 20], [110, 120]);
  assert.deepStrictEqual(plain(c.slot().zoom), { xAxis: [10, 110], yAxis: [20, 120] });
  c.send({ ...SCATTER, point_size: 2 }, { data: null });
  assert.strictEqual(c.block.zoomOn, true, 'the mode survives a redraw');
  sel.click();
  assert.strictEqual(c.block.zoomOn, false);
  c.close();
  const line = open(LINE);
  assert.ok(!line.slot().modeEl, 'a line has no switch');
  line.close();
});

test('Keep as limits writes the window into the limits and spends the zoom', () => {
  const c = open(LINE);
  c.drag([10, 20], [110, 120]);
  c.slot().keepBtn.click();
  const set = c.sets().pop();
  assert.deepStrictEqual(Object.keys(set).sort(), ['x_limits', 'y_limits']);
  assert.ok(set.x_limits.every(Number.isFinite));
  assert.strictEqual(c.slot().zoom, null);
  c.close();
});

test('Clear limits shows while limits are set, on the first panel, and empties them', () => {
  const c = open({ ...BOX, y_limits: [20, null] });
  assert.strictEqual(c.slot().clearBtn.hidden, false);
  assert.strictEqual(c.slot().resetBtn.hidden, true, 'no zoom, no Reset zoom');
  c.slot().clearBtn.click();
  assert.deepStrictEqual(plain(c.sets().pop()), { y_limits: null });
  c.send({ ...BOX, y_limits: null }, { data: null });
  assert.strictEqual(c.slot().clearBtn.hidden, true);
  c.send({ ...BOX, x_limits: [1, 2] }, { data: null });
  assert.strictEqual(c.slot().clearBtn.hidden, true, 'a box plot takes no x limits');
  c.close();
  const f = open({ ...LINE, facet: 'ARM', x_limits: [0, null], y_limits: [null, 50] });
  assert.strictEqual(f.block._slots[0].clearBtn.hidden, false);
  assert.ok(f.block._slots.slice(1).every((s) => s.clearBtn.hidden), 'one link, on the first panel');
  f.block._slots[0].clearBtn.click();
  assert.deepStrictEqual(plain(f.sets().pop()), { x_limits: null, y_limits: null });
  f.close();
});
