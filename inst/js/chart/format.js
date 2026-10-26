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
   * The decimals that print every value as ddNum() would, at most 4: one
   * count for a set of labels, so 1.7 next to 2.14 reads 1.70.
   * @param {number[]} values @returns {number}
   */
  const sharedDigits = (values) => {
    let d = 0;
    for (const v of values) {
      if (!Number.isFinite(v) || Number.isInteger(v)) continue;
      const s = String(Number(v.toPrecision(6)));
      if (s.includes('e')) continue;
      const i = s.indexOf('.');
      if (i >= 0) d = Math.max(d, s.length - i - 1);
    }
    return Math.min(d, 4);
  };

  /** @param {number} v @param {number} digits */
  const fixedNum = (v, digits) =>
    (v || 0).toLocaleString(undefined, { minimumFractionDigits: digits, maximumFractionDigits: digits });

  /**
   * The formatter for a chart's values, or null to keep the chart's own.
   * `unit: false` leaves the unit off (axis ticks: the axis title carries
   * it once).
   * `digits`: "number" prints that many decimals (sharedDigits()), and
   * then always returns a formatter.
   * @param {Record<string, any>} cfg @param {{ unit?: boolean, digits?: number }} [opts]
   * @returns {((v: any) => string) | null}
   */
  const valueFormat = (cfg, opts = {}) => {
    const format = cfg.format || 'number';
    const unit = cfg.unit ? String(cfg.unit) : '';
    const withUnit = opts.unit !== false && unit !== '';
    const digits = opts.digits;
    if (format === 'number' && !withUnit && digits == null) return null;
    /** @type {(v: number) => string} */
    const base = format === 'percent'
      // `|| 0` folds a negative zero.
      ? (v) => (Math.round(v * 1000) / 10 || 0) + '%'
      : format === 'compact' ? compact
        : digits != null ? (v) => fixedNum(v, digits) : (v) => String(NS.ddNum(v));
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

  /**
   * An axis name. `title` is the column's label, or its name when it has
   * none (axes.axisTitle); `col` the column. With a label: the label and
   * the unit, "Growth (%)". Without: the unit alone when there is one, else
   * the column name. Empty when the chart's title already carries it
   * ("Real GDP growth 2026, %" names the "%" axis).
   * @param {string} title @param {any} col @param {Record<string, any>} cfg
   * @param {{ unit?: boolean }} [opts]
   */
  const axisName = (title, col, cfg, opts = {}) => {
    const unit = opts.unit !== false && cfg.unit ? String(cfg.unit) : '';
    const labelled = !!title && title !== col;
    const name = labelled ? unitTitle(title, unit ? cfg : {})
      : (unit || title || '');
    const head = String(cfg.title_resolved || '').toLowerCase();
    if (name && head.includes(name.toLowerCase())) return '';
    return name;
  };

  NS.valueFormat = valueFormat;
  NS.sharedDigits = sharedDigits;
  NS.unitTitle = unitTitle;
  NS.axisName = axisName;
})();
