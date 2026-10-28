// @ts-check
/**
 * Chart block: the timeline family (gantt) as the view uses it. Its model
 * and option are in model-timeline.js and option-timeline.js. A click on a
 * bar selects its lane, or with a drill column that column's values.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** @type {ChartFamily} */
  const timeline = {
    name: 'timeline',
    model: NS.model.timeline,
    option: NS.option.timeline,
    legend: NS.option.timelineLegend,
    widths: false,
    alwaysHighlight: false,

    attach(view, slot) {
      slot.chart.on('click', (/** @type {any} */ params) => {
        if (NS.drillState(view.settings) === 'off') return;
        if (params.componentType !== 'series' || !params.value) return;
        const filters = NS.model.timelineClick(view.settings, slot.facetVal, params.value);
        if (filters) view.selection.click(slot, params, filters);
      });
    },

    patch: (c) => NS.option.timelinePatch(c.m, c.panel, c.cfg, c.range ? null : c.lit, c.dimmed)
  };

  NS.families = Object.assign(NS.families || {}, { timeline });
})();
