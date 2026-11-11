// @ts-check
/**
 * Chart block: the names of the aggregations and statistics. R aggregates
 * (R/chart-cells.R); the browser only words what it draws.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** One word per aggregation, for composed labels ("Mean AGE").
   * @type {Record<string, string>} */
  const AGG_WORDS = {
    count: 'Count', count_distinct: 'Distinct', mean: 'Mean',
    median: 'Median', sum: 'Sum', min: 'Min', max: 'Max'
  };

  // The words of a `summary` / `whiskers` statistic in a tooltip. An unknown
  // statistic reads as Tukey's whiskers.
  /** @type {Array<{value: string, center: string, range: string}>} */
  const STAT_META = [
    { value: 'tukey', center: '', range: '1.5×IQR' },
    { value: 'median_q1_q3', center: 'Median', range: 'Q1–Q3' },
    { value: 'mean_sd', center: 'Mean', range: '±1 SD' },
    { value: 'mean_2sd', center: 'Mean', range: '±2 SD' },
    { value: 'mean_se', center: 'Mean', range: '±1 SE' },
    { value: 'mean_ci95', center: 'Mean', range: '95% CI' },
    { value: 'p5_p95', center: 'Median', range: 'P5–P95' },
    { value: 'p10_p90', center: 'Median', range: 'P10–P90' },
    { value: 'min_max', center: 'Median', range: 'Min–Max' }
  ];
  /** @param {string} stat */
  const statMeta = (stat) => STAT_META.find((s) => s.value === stat) || STAT_META[0];

  NS.agg = { AGG_WORDS, statMeta };
})();
