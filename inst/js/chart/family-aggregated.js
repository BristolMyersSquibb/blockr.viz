// @ts-check
/**
 * Chart block: the aggregated family (bar, histogram, waterfall, pie,
 * treemap, box plot, point range) as the view uses it. Its model and option
 * are in model-aggregated.js and option-aggregated.js.
 *
 * A click on a bar segment, slice, tile, box or point range selects the
 * mark's keys, or with a drill column that column's values in the rows
 * under it. Every draw ends with the highlight patch.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** @type {ChartFamily} */
  const aggregated = {
    name: 'aggregated',
    model: NS.model.aggregated,
    option: NS.option.aggregated,
    legend: NS.option.aggregatedLegend,
    widths: true,
    alwaysHighlight: true,

    // The handler reads the view's live state: an instance outlives many draws.
    attach(view, slot) {
      slot.chart.on('click', (/** @type {any} */ params) => {
        const cfg = view.config;
        if (NS.drillState(cfg) === 'off') return;
        const m = view._memo.model;
        if (!m || m.family !== 'aggregated') return;
        const at = NS.model.aggregatedMarkAt(m, cfg, params);
        if (!at) return;
        const keys = NS.model.aggregatedKeys(cfg, slot.facetVal, at.group, at.level);
        const filters = NS.model.aggregatedClick(cfg, keys,
          () => NS.model.rowsUnder(view.data, view._ix, keys));
        if (filters) view.selection.click(slot, params, filters);
      });
    },

    // A range selection only exists on the individual family.
    patch: (c) => NS.option.aggregatedPatch(c.m, c.panel, c.cfg, c.current,
                                            c.range ? null : c.lit, c.dimmed)
  };

  NS.families = Object.assign(NS.families || {}, { aggregated });
})();
