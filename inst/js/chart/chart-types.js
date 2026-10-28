// @ts-check
/**
 * Chart block: the chart types and the family that draws each. A family
 * shares a model and an option builder (family-*.js): aggregated types
 * summarise rows into marks, the timeline draws a lane per level, the
 * individual types draw a mark per row.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  const AGGREGATED_TYPES = ['bar', 'histogram', 'waterfall', 'pie', 'treemap', 'boxplot', 'pointrange'];
  // Box and point range summarize raw values per group in the browser.
  const DISTRIBUTION_TYPES = ['boxplot', 'pointrange'];
  // An area is a line with its area filled.
  /** @param {any} t */
  const isLineType = (t) => t === 'line' || t === 'area';

  /** @param {any} type @returns {'aggregated' | 'timeline' | 'individual'} */
  const familyOf = (type) => AGGREGATED_TYPES.includes(type) ? 'aggregated'
    : type === 'gantt' ? 'timeline' : 'individual';

  /** Panel scales: free_y only exists where the value axis is y.
   * @param {Record<string, any>} cfg @returns {'fixed' | 'free' | 'free_y'} */
  const facetScales = (cfg) =>
    (cfg.facet_scales === 'free_y' && familyOf(cfg.type) !== 'individual' ? 'free' : cfg.facet_scales);

  /** The axes drawn on a log scale: `y_transform` on a scatter, line or band, `x_transform`
   * on a scatter (the model also needs a numeric axis).
   * @param {Record<string, any>} cfg @returns {{ x: boolean, y: boolean }} */
  const logAxes = (cfg) => {
    const t = cfg.type;
    return {
      y: cfg.y_transform === 'log10' && (t === 'scatter' || t === 'line' || t === 'band'),
      x: cfg.x_transform === 'log10' && t === 'scatter'
    };
  };

  Object.assign(NS, { DISTRIBUTION_TYPES, familyOf, isLineType, facetScales, logAxes });
})();
