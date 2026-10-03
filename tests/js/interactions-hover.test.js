/* Hover pickers: chart.js's own nearest-line (line charts) and
 * nearest-band-level (band charts) logic, and the overlay patches they
 * drive. The fake echarts converts pixels to data one to one, so a pixel
 * position below is also a data position. */
'use strict';

const test = require('node:test');
const I = require('./interact');

const LAB = { x: 'ADY', y: 'AVAL', drill: 'auto' };

const XS = [-5, 0, 1, 2, 3, 15, 16, 29, 30, 57, 58, 85, 86, 120];
const YS = [10, 21.37, 22, 25, 27.37, 28.37, 30, 33, 45, 80];

/** The picker's answer at every (x, y) of the grid, as series names. */
function lineGrid(c, slot = 0, xs = XS, ys = YS) {
  const s = c.slot(slot);
  const names = s.focus ? s.focus.series.map((x) => x.name) : [];
  const out = {};
  for (const y of ys) {
    out['y=' + y] = xs.map((x) => {
      const si = c.block._nearestLineSeries(s, s.chart, x, y);
      return si == null ? null : names[si];
    });
  }
  return { xs, rows: out };
}

function bandGrid(c, slot = 0, xs = XS, ys = YS) {
  const s = c.slot(slot);
  const out = {};
  for (const y of ys) {
    out['y=' + y] = xs.map((x) => c.block._nearestBandLevel(s, s.chart, x, y));
  }
  return { xs, rows: out };
}

const move = (x, y, slot = 0) => [`mousemove (${x}, ${y})${slot ? ' in slot ' + slot : ''}`,
  (c) => c.zr('mousemove', { offsetX: x, offsetY: y }, slot)];
const out = (slot = 0) => ['globalout', (c) => c.zr('globalout', {}, slot)];

/** Steps plus each slot's focus state after each. */
const hoverRun = (config, steps, opts = {}) => I.runSteps(config, steps, {
  ...opts,
  extra: (c) => ({
    focus: c.block._slots.map((s) => ({
      focusSi: s.focusSi ?? null, hoverSi: s.hover ? s.hover.si ?? null : null,
      bandFocus: s.bandFocus ?? null
    }))
  })
});

test('line-series: picker grid', () => {
  const c = I.open({ chart_type: 'line', ...LAB, series: 'USUBJID' });
  try {
    I.snap('hover-line-series-grid', {
      focus: !!c.slot().focus, xAxisType: c.slot().focus.xAxisType,
      grid: lineGrid(c)
    });
  } finally { c.close(); }
});

test('line-categorical-x: picker grid (x is the category index)', () => {
  const c = I.open({ chart_type: 'line', x: 'AVISIT', y: 'AVAL', series: 'USUBJID',
                     drill: 'auto' }, { width: 300 });
  try {
    I.snap('hover-line-categorical-grid', {
      xAxisType: c.slot().focus.xAxisType,
      xOrder: c.slot().focus.xOrder ? Array.from(c.slot().focus.xOrder.entries()) : null,
      grid: lineGrid(c, 0, [-1, 0, 0.5, 1, 2, 3, 4, 5], YS)
    });
  } finally { c.close(); }
});

test('line-color-only: picker grid', () => {
  const c = I.open({ chart_type: 'line', ...LAB, color: 'SEX' });
  try {
    I.snap('hover-line-color-grid', { grid: lineGrid(c) });
  } finally { c.close(); }
});

test('single line, scatter and band: no line focus', () => {
  const kinds = {
    line: { chart_type: 'line', ...LAB },
    scatter: { chart_type: 'scatter', ...LAB, color: 'ARM' },
    scatterSeries: { chart_type: 'scatter', ...LAB, series: 'USUBJID' },
    band: { chart_type: 'band', ...LAB, color: 'ARM', band_series: I.F.band_series.color,
            whiskers: 'p10_p90' }
  };
  const res = {};
  for (const [k, cfg] of Object.entries(kinds)) {
    const c = I.open(cfg);
    const m = c.mark();
    c.zr('mousemove', { offsetX: 2, offsetY: 21.37 });
    c.zr('globalout', {});
    const s = c.slot();
    res[k] = {
      focus: !!s.focus, band: !!s.band,
      picked: c.block._nearestLineSeries(s, s.chart, 2, 21.37),
      calls: c.since(m).calls
    };
    c.close();
  }
  I.snap('hover-no-focus', res);
});

test('line-series: mousemove promotes, moves, demotes', () => {
  const r = hoverRun({ chart_type: 'line', ...LAB, series: 'USUBJID', color: 'ARM' }, [
    move(2, 21.37),
    // Same line again: no patch.
    move(2.5, 22),
    move(3, 28.37),
    // Far from every line: demote.
    move(40, 80),
    move(40, 80),
    move(1, 27.37),
    out(),
    out()
  ]);
  I.snap('hover-line-promote', r);
});

test('line-facet: each panel picks its own lines', () => {
  const c = I.open({ chart_type: 'line', ...LAB, series: 'USUBJID', facet: 'SEX' });
  try {
    I.snap('hover-line-facet-grid', {
      panels: c.block._slots.map((s, i) => ({ facet: s.facetVal, grid: lineGrid(c, i) }))
    });
  } finally { c.close(); }
});

test('band-color: level picker grid', () => {
  const c = I.open({ chart_type: 'band', ...LAB, color: 'ARM',
                     band_series: I.F.band_series.color, whiskers: 'p10_p90' });
  try {
    I.snap('hover-band-grid', { band: c.slot().band, grid: bandGrid(c) });
  } finally { c.close(); }
});

test('band-color: mousemove reveals the nearest ribbon', () => {
  const r = hoverRun({ chart_type: 'band', ...LAB, color: 'ARM',
                       band_series: I.F.band_series.color, whiskers: 'p10_p90' }, [
    move(0, 29.87),
    move(10, 29.9),
    move(0, 26.3),
    move(0, 200),
    out(),
    out()
  ]);
  I.snap('hover-band-focus', r);
});

test('band-facet: no colour split, no ribbon picker', () => {
  const r = hoverRun({ chart_type: 'band', ...LAB, facet: 'SEX', box_points: 'outliers',
                       band_series: I.F.band_series.facet, band_refs: I.F.band_refs,
                       whiskers: 'p10_p90' }, [move(0, 26.24), out()]);
  I.snap('hover-band-facet', r);
});
