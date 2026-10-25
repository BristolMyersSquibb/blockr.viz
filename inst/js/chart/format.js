// @ts-check
/**
 * Chart block: the number format of a chart's values (`format`, `unit`),
 * the same vocabulary as the tile block. "number" prints separators and
 * smart decimals, "compact" 1.2K / 3.4M / 5.6B, "percent" a fraction x100
 * with "%". `unit` is free text printed after the number ("CHF").
 *
 * A chart with format "number" and no unit keeps its own formatting:
 * valueFormat() then returns null and the callers fall back to it.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});

  /** @param {number} v */
  const compact = (v) => {
    const a = Math.abs(v);
    /** @param {number} d @param {string} s */
    const short = (d, s) => String(Math.round((v / d) * 10) / 10) + s;
    if (a >= 1e9) return short(1e9, 'B');
    if (a >= 1e6) return short(1e6, 'M');
    if (a >= 1e3) return short(1e3, 'K');
    return String(NS.ddNum(v));
  };

  /**
   * The formatter for a chart's values, or null to keep the chart's own.
   * `unit: false` leaves the unit off (axis ticks: the axis title carries
   * it once).
   * @param {Record<string, any>} cfg @param {{ unit?: boolean }} [opts]
   * @returns {((v: any) => string) | null}
   */
  const valueFormat = (cfg, opts = {}) => {
    const format = cfg.format || 'number';
    const unit = cfg.unit ? String(cfg.unit) : '';
    const withUnit = opts.unit !== false && unit !== '';
    if (format === 'number' && !withUnit) return null;
    /** @type {(v: number) => string} */
    const base = format === 'percent'
      // `|| 0` folds a negative zero.
      ? (v) => (Math.round(v * 1000) / 10 || 0) + '%'
      : (format === 'compact' ? compact : (v) => String(NS.ddNum(v)));
    return (v) => {
      const n = Number(v);
      if (v == null || !isFinite(n)) return '';
      return base(n) + (withUnit ? ' ' + unit : '');
    };
  };

  /** An axis title with the unit, "Sum of amount (CHF)". @param {string} title @param {Record<string, any>} cfg */
  const unitTitle = (title, cfg) => {
    const unit = cfg.unit ? String(cfg.unit) : '';
    if (!unit) return title;
    return title ? title + ' (' + unit + ')' : unit;
  };

  NS.valueFormat = valueFormat;
  NS.unitTitle = unitTitle;
})();
