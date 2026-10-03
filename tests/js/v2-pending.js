/* Tests v2 skips by name until it draws their chart family.
 *
 * CHART_ENGINE=v2 runs every test in tests/js against v2; the ones listed
 * here are skipped, keyed "<test file>: <test name>" (the drawing snapshots
 * are all registered by run-case.js). Each build step removes its family's
 * entries. A test of a built family must match v1 or have its difference
 * listed in v2-differences.json.
 *
 * V2_RUN_PENDING=1 runs them anyway, to see which already pass.
 */
'use strict';

module.exports = {
  // Step 2c: scatter, line, band, and the gear drive that switches through
  // scatter.
  individual: [
    // Switches the type through scatter.
    'interactions-gear.test.js: drive: type-switch',
    'interactions-click.test.js: legend-line-series-color-facet',
    'chart-brush.test.js: brush with a drill column: every row once in the drill payload',
    'chart-brush.test.js: brush without a drill column: a range over every brushed point',
    'run-case.js: scatter-defaults',
    'run-case.js: scatter',
    'run-case.js: scatter-color',
    'run-case.js: scatter-series',
    'run-case.js: scatter-series-color',
    'run-case.js: scatter-facet-fixed',
    'run-case.js: scatter-facet-free-y',
    'run-case.js: scatter-identity',
    'run-case.js: scatter-smoother',
    'run-case.js: scatter-smoother-color',
    'run-case.js: scatter-smoother-facet',
    'run-case.js: scatter-errorbars-refs',
    'run-case.js: scatter-tt-fields',
    'run-case.js: scatter-categorical-x',
    'run-case.js: scatter-drill-off',
    'run-case.js: line',
    'run-case.js: line-series-color',
    'run-case.js: line-categorical-x',
    'run-case.js: line-facet-counts',
    'run-case.js: line-single-step-errorbars',
    'run-case.js: line-color-only',
    'run-case.js: band',
    'run-case.js: band-color',
    'run-case.js: band-facet-refs',
    'run-case.js: band-note',
    'chart-payload.test.js: scatter-series-color: dictionary-encoded columns draw the same',
    'chart-payload.test.js: scatter-series-color: a JSON string payload draws the same',
    'chart-payload.test.js: line-facet-counts: dictionary-encoded columns draw the same',
    'chart-payload.test.js: line-facet-counts: a JSON string payload draws the same',
    'interactions-brush.test.js: scatter-geometric',
    'interactions-brush.test.js: scatter-ignored-events',
    'interactions-brush.test.js: scatter-color',
    'interactions-brush.test.js: scatter-facet',
    'interactions-brush.test.js: scatter-drill-column',
    'interactions-brush.test.js: scatter-drill-off',
    'interactions-brush.test.js: scatter-transient',
    'interactions-brush.test.js: scatter-click-guard',
    'interactions-brush.test.js: line-event',
    'interactions-brush.test.js: scatter-duplicate-points',
    'interactions-click.test.js: scatter-geometric',
    'interactions-click.test.js: scatter-color',
    'interactions-click.test.js: scatter-series',
    'interactions-click.test.js: scatter-facet-drill-column',
    'interactions-click.test.js: scatter-drill-off',
    'interactions-click.test.js: scatter-transient',
    'interactions-click.test.js: scatter-transient-geometric',
    'interactions-click.test.js: scatter-non-series',
    'interactions-click.test.js: line-series',
    'interactions-click.test.js: line-dblclick-cancels',
    'interactions-click.test.js: line-hover-wins',
    'interactions-click.test.js: line-color-only',
    'interactions-click.test.js: line-no-split',
    'interactions-click.test.js: line-transient',
    'interactions-click.test.js: band-color',
    'interactions-click.test.js: band-plain',
    'interactions-gear.test.js: outline: scatter',
    'interactions-gear.test.js: outline: scatter-facet',
    'interactions-gear.test.js: outline: line',
    'interactions-gear.test.js: outline: band',
    'interactions-gear.test.js: drive: scatter',
    'interactions-gear.test.js: drive: line',
    'interactions-gear.test.js: drive: band',
    'interactions-hover.test.js: line-series: picker grid',
    'interactions-hover.test.js: line-categorical-x: picker grid (x is the category index)',
    'interactions-hover.test.js: line-color-only: picker grid',
    'interactions-hover.test.js: single line, scatter and band: no line focus',
    'interactions-hover.test.js: line-series: mousemove promotes, moves, demotes',
    'interactions-hover.test.js: line-facet: each panel picks its own lines',
    'interactions-hover.test.js: band-color: level picker grid',
    'interactions-hover.test.js: band-color: mousemove reveals the nearest ribbon',
    'interactions-hover.test.js: band-facet: no colour split, no ribbon picker',
    'interactions-restore.test.js: scatter',
    'interactions-restore.test.js: line',
    'interactions-restore.test.js: line zoom window survives a re-render, a type switch forgets it',
    'interactions-restore.test.js: a brush filter survives a re-render',
    'scaling.test.js: scaling: scatter',
    'scaling.test.js: scaling: line, faceted by arm',
    'scaling.test.js: scaling: scatter, series = patient',
    'scaling.test.js: scaling: scatter, series = patient, colour = arm',
    'scaling.test.js: scaling: scatter, series = patient, faceted by arm',
    'scaling.test.js: scaling: line, categorical x with a level per patient'
  ]
};
