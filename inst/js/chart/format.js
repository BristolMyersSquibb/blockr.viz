// @ts-check
/**
 * Chart block: the number format of a chart's values (`format`, `unit`),
 * the same vocabulary as the tile block. "number" prints separators and
 * up to two decimals (NS.ddNum), "compact" 1.2K / 3.4M / 5.6B, "percent" a
 * fraction x100 with "%", a whole number (0 to 6, or its text) exactly that
 * many decimals. Each prints as on the table block. `unit` is free text printed after the number ("CHF").
 *
 * A chart with format "number" and no unit keeps its own formatting:
 * valueFormat() then returns null and the callers fall back to it.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});

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

  /** @param {number} v @param {number} digits */
  const fixedNum = (v, digits) =>
    (v || 0).toLocaleString(undefined, { minimumFractionDigits: digits, maximumFractionDigits: digits });

  /**
   * The decimals a `format` fixes (2 or "2"), or null.
   * @param {any} format @returns {number | null}
   */
  const fixedDigits = (format) => {
    if (format == null || format === '' || typeof format === 'boolean') return null;
    const d = Number(format);
    return Number.isInteger(d) && d >= 0 && d <= 6 ? d : null;
  };

  /**
   * The formatter for a chart's values, or null to keep the chart's own.
   * `unit: false` leaves the unit off (axis ticks: the axis title carries
   * it once).
   * @param {Record<string, any>} cfg @param {{ unit?: boolean }} [opts]
   * @returns {((v: any) => string) | null}
   */
  const valueFormat = (cfg, opts = {}) => {
    const fixed = fixedDigits(cfg.format);
    const format = fixed != null ? 'number' : (cfg.format || 'number');
    const unit = cfg.unit ? String(cfg.unit) : '';
    const withUnit = opts.unit !== false && unit !== '';
    if (format === 'number' && !withUnit && fixed == null) return null;
    /** @type {(v: number) => string} */
    const base = format === 'percent'
      // `|| 0` folds a negative zero.
      ? (v) => (Math.round(v * 1000) / 10 || 0) + '%'
      : format === 'compact' ? compact
        : fixed != null ? (v) => fixedNum(v, fixed) : (v) => String(NS.ddNum(v));
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
    const t = cfg.derived && cfg.derived.titles && cfg.derived.titles.title;
    const head = String((t && t.text) || '').toLowerCase();
    if (name && head.includes(name.toLowerCase())) return '';
    return name;
  };

  NS.valueFormat = valueFormat;
  NS.fixedDigits = fixedDigits;
  NS.unitTitle = unitTitle;
  NS.axisName = axisName;
})();
