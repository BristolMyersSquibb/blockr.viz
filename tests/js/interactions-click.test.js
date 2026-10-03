/* Clicks and drill: what a click on each kind of mark sends to R
 * (`<id>_action`), what it leaves on the chart (highlight patches, the
 * footer, the drill signal), and what a second click, another click and
 * Reset do. Latched drill and transient drill (`ctrl_target` set).
 *
 * Each scenario is a list of steps; the snapshot holds, per step, the new
 * inputs, the new echarts calls, the footer and the selection state. */
'use strict';

const test = require('node:test');
const I = require('./interact');

const AE = { group: 'AETERM', drill: 'auto' };
const LAB = { x: 'ADY', y: 'AVAL', drill: 'auto' };
const GANTT = { chart_type: 'gantt', x: 'ASTDY', xend: 'AENDY', y: 'AETERM', drill: 'auto' };

/** Where a datum sits: by category / datum name, and optionally series. */
const at = (name, seriesName) => (p) => String(p.name) === name &&
  (seriesName === undefined || p.seriesName === seriesName);
const series = (seriesName) => (p) => p.seriesName === seriesName;

// Steps. Each is [label, (ctx) => void].
const click = (pred, slot = 0, extra) => [`click ${describe(pred)}${slot ? ' in slot ' + slot : ''}`,
  (c) => c.clickWhere(slot, pred, extra)];
const clickAt = (slot, si, di, extra) => [`click series ${si} datum ${di} in slot ${slot}`,
  (c) => c.click(slot, si, di, extra)];
const reset = () => ['Reset', (c) => c.reset()];
// Reset, where the chart shows one.
const resetIfShown = () => ['Reset, if shown', (c) => {
  if (c.block.statusEl.querySelector('.dd-status-reset')) c.reset();
}];
const wait = (ms) => [`advance ${ms} ms`, (c) => c.advance(ms)];
const describe = (pred) => pred.label || 'mark';
const named = (label, pred) => Object.assign(pred, { label });

/** A flash target: the bar's own box, as zrender reports it. */
const box = { event: { offsetX: 40, offsetY: 30, target: I.rectTarget(10, 20, 120, 18, [0, 2, 2, 0]) } };
/** A click with no box to trace (a sector, a polygon, a point). */
const ring = { event: { offsetX: 55, offsetY: 66, target: { shape: { cx: 1, cy: 1, r: 5 } } } };

const scenario = (name, config, steps, opts) =>
  test(name, () => I.snap('click-' + name, I.runSteps(config, steps, opts)));

// -- aggregated --------------------------------------------------------------

const nausea = named('Nausea', at('Nausea'));
const headache = named('Headache', at('Headache'));
const rash = named('Rash', at('Rash'));

scenario('bar-latched', { chart_type: 'bar', ...AE }, [
  click(nausea), click(nausea), click(headache), click(rash), reset()
]);

scenario('bar-color', { chart_type: 'bar', ...AE, color: 'AESEV' }, [
  click(named('Rash / MODERATE', at('Rash', 'MODERATE'))),
  // Another colour segment of the same bar (the missing-severity level):
  // the same group, so it toggles off.
  click(named('Rash / (missing)', at('Rash', ''))),
  click(named('Headache / MILD', at('Headache', 'MILD'))),
  reset()
]);

scenario('bar-grouped-vertical', { chart_type: 'bar', ...AE, color: 'AESEV', bar_mode: 'grouped',
                                   orientation: 'vertical' }, [
  click(named('Rash / MODERATE', at('Rash', 'MODERATE')))
]);

scenario('bar-facet-drill-column', { chart_type: 'bar', group: 'AETERM', facet: 'ARM',
                                     drill: 'USUBJID' }, [
  click(nausea, 1),
  // Same group in another panel: the selection is the group alone, so this
  // toggles off rather than selecting the other panel's bar.
  click(nausea, 2),
  click(headache, 2),
  reset()
]);

scenario('bar-drill-off', { chart_type: 'bar', group: 'AETERM', color: 'AESEV', drill: '' }, [
  click(named('Headache / MILD', at('Headache', 'MILD')))
]);

scenario('bar-transient', { chart_type: 'bar', ...AE, ctrl_target: 'auto' }, [
  click(nausea, 0, box),
  wait(1200),
  // Same mark again: sent again with a new nonce, no toggle.
  click(nausea, 0, box),
  wait(2000),
  wait(1200),
  click(headache, 0, ring)
]);

scenario('bar-transient-drill-off', { chart_type: 'bar', group: 'AETERM', drill: '',
                                      ctrl_target: 'auto' }, [
  click(nausea, 0, box)
]);

scenario('bar-transient-drill-column', { chart_type: 'bar', group: 'AETERM', drill: 'USUBJID',
                                         ctrl_target: 'flt' }, [
  click(nausea, 0, box)
]);

scenario('waterfall', { chart_type: 'waterfall', group: 'AVISIT', value: 'CHG', func: 'sum',
                        drill: 'auto' }, [
  click(named('Week 2 / delta', at('Week 2', 'delta'))),
  // The invisible base under the same step is the same group.
  click(named('Week 2 / base', at('Week 2', 'base'))),
  click(named('Week 8 / delta', at('Week 8', 'delta'))),
  reset()
]);

scenario('pie', { chart_type: 'pie', group: 'AESEV', drill: 'auto' }, [
  click(named('SEVERE', at('SEVERE'))), click(named('SEVERE', at('SEVERE'))),
  click(named('MILD', at('MILD'))), reset()
]);

scenario('pie-facet', { chart_type: 'pie', group: 'AESEV', facet: 'SEX', drill: 'auto' }, [
  click(named('MILD', at('MILD')), 0), click(named('MILD', at('MILD')), 1),
  click(named('MODERATE', at('MODERATE')), 1)
]);

scenario('pie-transient', { chart_type: 'pie', group: 'AESEV', drill: 'auto',
                            ctrl_target: 'auto' }, [
  click(named('MILD', at('MILD')), 0, ring)
]);

scenario('treemap', { chart_type: 'treemap', group: 'AETERM', value: 'AVAL', func: 'sum',
                      drill: 'auto' }, [
  click(headache), click(nausea), reset(), click(nausea), click(nausea)
]);

scenario('radar-color', { chart_type: 'radar', group: 'AVISIT', color: 'ARM', value: 'AVAL',
                          func: 'mean', drill: 'auto' }, [
  click(named('Placebo', at('Placebo'))), click(named('Low Dose', at('Low Dose'))), reset()
]);

scenario('radar-nocolor', { chart_type: 'radar', group: 'AETERM', drill: 'auto' }, [
  clickAt(0, 0, 0)
]);

scenario('radar-drill-column', { chart_type: 'radar', group: 'AVISIT', color: 'ARM',
                                 drill: 'USUBJID' }, [
  click(named('Placebo', at('Placebo')))
]);

scenario('radar-facet', { chart_type: 'radar', group: 'AVISIT', color: 'AESEV', facet: 'SEX',
                          drill: 'auto' }, [
  click(named('MILD', at('MILD')), 1)
]);

const week = (w, lvl) => named(lvl ? `${w} / ${lvl}` : w,
  (p) => String(p.name) === (lvl ? `${w}\u0000${lvl}` : w));

scenario('boxplot', { chart_type: 'boxplot', group: 'AVISIT', value: 'AVAL', drill: 'auto' }, [
  click(week('Week 2')), click(week('Week 4')), reset(), click(week('Week 4')),
  click(week('Week 4'))
]);

scenario('boxplot-color', { chart_type: 'boxplot', group: 'AVISIT', color: 'ARM', value: 'AVAL',
                            box_points: 'outliers', drill: 'auto' }, [
  click(week('Week 2', 'Low Dose')),
  // The other arm's box in the same visit is the same group: toggles off.
  click(week('Week 2', 'Placebo')),
  click(week('Week 8', 'High Dose'))
]);

scenario('boxplot-facet', { chart_type: 'boxplot', group: 'AVISIT', color: 'ARM', facet: 'SEX',
                            value: 'AVAL', drill: 'USUBJID' }, [
  click(week('Week 2', 'Low Dose'), 0)
]);

scenario('pointrange', { chart_type: 'pointrange', group: 'AVISIT', value: 'AVAL',
                         drill: 'auto' }, [
  // The interval (custom series) and the centre dot (scatter) of one group.
  click(named('Week 4 interval', (p) => p.seriesType === 'custom' && p.name === 'Week 4')),
  click(named('Week 4 centre', (p) => p.seriesType === 'scatter' && p.name === 'Week 4')),
  click(named('Baseline interval', (p) => p.seriesType === 'custom' && p.name === 'Baseline'))
]);

scenario('pointrange-color', { chart_type: 'pointrange', group: 'AVISIT', color: 'ARM',
                               value: 'AVAL', drill: 'auto' }, [
  click(named('first centre dot', (p) => p.seriesType === 'scatter'))
]);

// -- individual --------------------------------------------------------------

const point = (x, y) => named(`point (${x}, ${y})`, (p) => Array.isArray(p.value) &&
  p.value[0] === x && p.value[1] === y);
const anyPoint = (seriesName) => named(`first point of ${seriesName}`, series(seriesName));

scenario('scatter-geometric', { chart_type: 'scatter', ...LAB }, [
  click(point(15, 29.24)),
  wait(150),
  click(point(15, 29.24)),
  click(point(2, 21.37)),
  reset()
]);

scenario('scatter-color', { chart_type: 'scatter', ...LAB, color: 'ARM' }, [
  click(anyPoint('Placebo')),
  click(anyPoint('Placebo')),
  click(anyPoint('High Dose')),
  reset()
]);

scenario('scatter-series', { chart_type: 'scatter', ...LAB, series: 'USUBJID' }, [
  click(anyPoint('S03'))
]);

scenario('scatter-facet-drill-column', { chart_type: 'scatter', ...LAB, color: 'ARM',
                                         facet: 'SEX', drill: 'USUBJID' }, [
  click(anyPoint('Placebo'), 1),
  click(anyPoint('Low Dose'), 0)
]);

scenario('scatter-drill-off', { chart_type: 'scatter', ...LAB, color: 'ARM', drill: '' }, [
  click(anyPoint('Placebo'))
]);

scenario('scatter-transient', { chart_type: 'scatter', ...LAB, color: 'ARM',
                                ctrl_target: 'auto' }, [
  click(anyPoint('Placebo'), 0, ring), click(anyPoint('Placebo'), 0, ring)
]);

// No drill column: a geometric click has no claim to send, so it latches a
// local range filter even with a ctrl_target.
scenario('scatter-transient-geometric', { chart_type: 'scatter', ...LAB,
                                          ctrl_target: 'auto' }, [
  click(point(15, 29.24), 0, ring)
]);

// A point in a facet panel: the panel's key goes with the point, and the
// points outside it dim.
scenario('scatter-facet-geometric', { chart_type: 'scatter', ...LAB, facet: 'ARM' }, [
  click(named('first point', () => true), 1),
  reset()
]);

scenario('scatter-non-series', { chart_type: 'scatter', ...LAB }, [
  ['click a markLine (componentType markLine)', (c) =>
    c.trigger('click', { componentType: 'markLine', seriesIndex: 0, value: 5 })]
]);

const lineOf = (s) => named(`line ${s}`, series(s));

scenario('line-series', { chart_type: 'line', ...LAB, series: 'USUBJID' }, [
  // The click is held 275 ms (a double-click resets the zoom instead).
  click(lineOf('S03')),
  wait(274),
  wait(1),
  click(lineOf('S03')),
  wait(275),
  click(lineOf('S05')),
  wait(275),
  reset()
]);

scenario('line-dblclick-cancels', { chart_type: 'line', ...LAB, series: 'USUBJID' }, [
  click(lineOf('S03')),
  ['zr dblclick', (c) => c.zr('dblclick')],
  wait(500)
]);

scenario('line-hover-wins', { chart_type: 'line', ...LAB, series: 'USUBJID', color: 'ARM' }, [
  // The cursor sits on S02 (x = 2, y = 21.37 in pixel = data space), and
  // echarts' hit test reports S03: the hovered line is the one drilled.
  ['mousemove onto S02', (c) => c.zr('mousemove', { offsetX: 2, offsetY: 21.37 })],
  click(lineOf('S03')),
  wait(275)
]);

scenario('line-color-only', { chart_type: 'line', ...LAB, color: 'SEX' }, [
  click(lineOf('F')), wait(275)
]);

scenario('line-no-split', { chart_type: 'line', ...LAB }, [
  click(point(15, 29.24)), wait(275)
]);

scenario('line-transient', { chart_type: 'line', ...LAB, series: 'USUBJID',
                             ctrl_target: 'auto' }, [
  click(lineOf('S03'), 0, ring), wait(275)
]);

scenario('band-color', { chart_type: 'band', ...LAB, color: 'ARM',
                         band_series: I.F.band_series.color, whiskers: 'p10_p90' }, [
  click(named('High Dose centre line', (p) => p.seriesType === 'line' &&
                p.seriesName === 'High Dose'))
]);

scenario('band-plain', { chart_type: 'band', ...LAB, band_series: I.F.band_series.plain,
                         whiskers: 'p10_p90' }, [
  click(named('centre line', (p) => p.seriesType === 'line'))
]);

// -- timeline ----------------------------------------------------------------

const lane = (term) => named(`bar in lane ${term}`, (p) => Array.isArray(p.value) &&
  p.value[3] === term);

scenario('gantt', { ...GANTT }, [
  click(lane('Rash')),
  // No toggle on a gantt: the same lane again stays selected and resends.
  click(lane('Rash')),
  click(lane('Nausea')),
  reset()
]);

scenario('gantt-drill-column', { ...GANTT, drill: 'USUBJID' }, [
  click(lane('Rash'))
]);

scenario('gantt-drill-off', { ...GANTT, drill: '' }, [
  click(lane('Rash'))
]);

scenario('gantt-facet', { ...GANTT, y: 'USUBJID', facet: 'ARM' }, [
  click(named('first bar', () => true), 1)
]);

// An explicit drill column in a facet panel: only the drill column is sent,
// the clicked event's own value of it.
scenario('gantt-facet-drill-column', { ...GANTT, y: 'USUBJID', facet: 'ARM', drill: 'AETERM' }, [
  click(named('first bar', () => true), 1)
]);

scenario('gantt-transient', { ...GANTT, ctrl_target: 'auto' }, [
  click(lane('Rash'), 0, box), wait(3100)
]);

scenario('gantt-non-series', { ...GANTT }, [
  ['click a non-series component', (c) =>
    c.trigger('click', { componentType: 'markLine', value: [1, 2, 3, 'Rash'] })]
]);

// -- legend band (faceted charts) --------------------------------------------

const chip = (name) => [`legend chip ${name}`, (c) => {
  const ch = Array.from(c.block.legendEl.querySelectorAll('.dd-legend-chip'))
    .find((x) => x.textContent === name);
  if (!ch) throw new Error(`no chip ${name}`);
  ch.click();
}];
const echo = (extra) => [`R echoes the config${extra ? ' ' + JSON.stringify(extra) : ''}`,
  (c) => c.echo(extra)];

scenario('legend-bar-facet', { chart_type: 'bar', ...AE, color: 'AESEV', facet: 'ARM' }, [
  chip('MILD'), chip('SEVERE'), echo(),
  chip('MILD'),
  // A different level list resets the toggles.
  echo({ color: 'SEX' })
]);

scenario('legend-line-series-color-facet', { chart_type: 'line', ...LAB, series: 'USUBJID',
                                             color: 'ARM', facet: 'SEX' }, [
  chip('Placebo'), chip('Placebo')
]);

// A chip on a chart without a facet filters its level, also with an
// explicit drill column, with a ctrl_target, and not at all with drill off.
// The chips marked as the filter's level.
const chipsOn = { extra: (c) => ({ legendOn: Array.from(
  c.block.legendEl.querySelectorAll('.dd-legend-chip-on'), (x) => x.textContent) }) };

scenario('legend-scatter-color', { chart_type: 'scatter', ...LAB, color: 'ARM' }, [
  chip('Placebo'), chip('Placebo'), chip('High Dose'), resetIfShown()
], chipsOn);

scenario('legend-bar-drill-column', { chart_type: 'bar', group: 'AETERM', color: 'AESEV',
                                      drill: 'USUBJID' }, [
  chip('SEVERE')
], chipsOn);

scenario('legend-bar-transient', { chart_type: 'bar', ...AE, color: 'AESEV',
                                   ctrl_target: 'auto' }, [
  chip('MILD'), wait(3100)
], chipsOn);

scenario('legend-gantt-drill-off', { ...GANTT, color: 'AESEV', drill: '' }, [
  chip('MILD')
], chipsOn);
