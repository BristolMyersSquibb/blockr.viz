// @ts-check
/**
 * Chart block: a mark's keys and the filters made of them.
 *
 * A mark carries its keys, the column values that define it ({AETERM:
 * "Rash"}, plus the facet column inside a facet panel). A click sends them
 * as a filter {col: [values]}, the footer names the filter in column labels,
 * and a restored filter lights the marks that hold filtered rows. A missing
 * value ('' or null) is a key too, sent as null, which R matches with
 * is.na().
 *
 * A selection has the shape R reads (R/kit-input.R), in memory as on the
 * wire: `{type: 'categorical', filters}`, or for a point or a brush on
 * numeric axes `{type: 'range', x, y, x_range, y_range}`, with the panel's
 * key in `filters` when it was taken in a facet panel.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** @typedef {Record<string, any[]>} Filters */

  /** Is a key value missing? The aggregation keys it as ''. @param {any} v */
  const isMissing = (v) => v == null || v === '';

  /** A filter from a mark's keys: one value per column, null when missing.
   * @param {Record<string, any>} keys @returns {Filters} */
  const fromKeys = (keys) => {
    /** @type {Filters} */
    const out = {};
    for (const col of Object.keys(keys)) {
      const v = keys[col];
      out[col] = [isMissing(v) ? null : String(v)];
    }
    return out;
  };

  /**
   * A filter on `col` with the distinct non-missing values of `rows`, or null.
   * @param {string} col @param {any[]} rows @returns {Filters | null}
   */
  const fromRows = (col, rows) => {
    const vals = [...new Set(
      (rows || []).map((r) => (r ? r[col] : null)).filter((v) => v != null)
    )].map(String);
    return vals.length ? { [col]: vals } : null;
  };

  /**
   * A filter as R sends it back: {col: [values]}; a scalar is one value.
   * Null when no column has a value.
   * @param {any} obj @returns {Filters | null}
   */
  const clean = (obj) => {
    if (!obj || typeof obj !== 'object' || Array.isArray(obj)) return null;
    /** @type {Filters} */
    const out = {};
    for (const col of Object.keys(obj)) {
      const v = obj[col];
      const vals = (Array.isArray(v) ? v : [v]).map((x) => (isMissing(x) ? null : String(x)));
      if (vals.length) out[col] = vals;
    }
    return Object.keys(out).length ? out : null;
  };

  /**
   * A range selection on columns `x` and `y`. `y` goes with `y_range`, and
   * the panel's key in `filters` only when there is one.
   * @param {string} x @param {string | null} y @param {any[]} xRange
   * @param {any[] | null} yRange @param {Filters | null} [filters]
   * @returns {ChartRangeSelection}
   */
  const range = (x, y, xRange, yRange, filters) => {
    /** @type {ChartRangeSelection} */
    const sel = { type: 'range', x, y: yRange ? y : null, x_range: xRange, y_range: yRange };
    if (filters && Object.keys(filters).length) sel.filters = filters;
    return sel;
  };

  /**
   * The selection a saved one restores, cleaned, or null for none or one
   * the chart cannot show.
   * @param {any} sel @returns {ChartSelection | null}
   */
  const restore = (sel) => {
    if (!sel || typeof sel !== 'object') return null;
    if (sel.type === 'range') {
      if (!sel.x || !Array.isArray(sel.x_range)) return null;
      const yr = Array.isArray(sel.y_range) && sel.y_range.length ? sel.y_range : null;
      return range(sel.x, sel.y || null, sel.x_range, yr, clean(sel.filters));
    }
    const filters = clean(sel.filters);
    return filters ? { type: 'categorical', filters } : null;
  };

  // Above this many values a column is named with its count, not its values.
  const MAX_LISTED = 3;

  /** The words for a filter, in column labels: "Reported Term = Rash", "Severity = (missing)",
   * "Patient, 214 values". @param {Filters} filters @param {VizColumn[]} columns @returns {string}
   */
  const describe = (filters, columns) => Object.keys(filters).map((col) => {
    const label = NS.axes.axisTitle(columns, col);
    const vals = filters[col];
    if (vals.length > MAX_LISTED) return label + ', ' + vals.length + ' values';
    return label + ' = ' + vals.map((v) => (isMissing(v) ? '(missing)' : v)).join(', ');
  }).join(', ');

  /** Decimals that show a number to three significant digits; none for a whole number.
   * @param {number} v */
  const decimalsOf = (v) => {
    if (!Number.isFinite(v) || Number.isInteger(v)) return 0;
    const s = String(Number(v.toPrecision(3)));
    if (/e/i.test(s)) return 0;
    const dot = s.indexOf('.');
    return dot < 0 ? 0 : s.length - dot - 1;
  };

  /** A range in words: "2 to 86", "21.4 to 30.0" (both ends at the same decimals), "= 15" for a
   * single value. Dates on a time axis. @param {any[]} r @param {string} [axisType] */
  const rangeWords = (r, axisType) => {
    const [lo, hi] = r;
    /** @param {any} v @param {number} d */
    const fmt = (v, d) => {
      if (axisType === 'time') return NS.axes.fmtXVal(v, 'time', null);
      if (typeof v !== 'number' || !Number.isFinite(v)) return String(v);
      return v.toLocaleString(undefined, { minimumFractionDigits: d, maximumFractionDigits: d });
    };
    const d = Math.max(decimalsOf(Number(lo)), decimalsOf(Number(hi)));
    if (lo === hi) return '= ' + fmt(lo, decimalsOf(Number(lo)));
    return fmt(lo, d) + ' to ' + fmt(hi, d);
  };

  /**
   * The words for a range filter: "Study Day 2 to 86, Value 21.4 to 30.0";
   * a point reads "Study Day = 15, Value = 29.2". The facet key of a range
   * taken in a panel follows: ", Arm = Placebo".
   * @param {ChartRangeSelection} sel
   * @param {VizColumn[]} columns @param {string} [xAxisType]
   */
  const describeRange = (sel, columns, xAxisType) => {
    const parts = [NS.axes.axisTitle(columns, sel.x) + ' ' + rangeWords(sel.x_range, xAxisType)];
    if (sel.y_range && sel.y) {
      parts.push(NS.axes.axisTitle(columns, sel.y) + ' ' + rangeWords(sel.y_range));
    }
    if (sel.filters && Object.keys(sel.filters).length) parts.push(describe(sel.filters, columns));
    return parts.join(', ');
  };

  /** A value as a filter compares it: missing folds to ''. @param {any} v */
  const keyOf = (v) => (isMissing(v) ? '' : String(v));

  // Each filtered column's values as a set, made once per filter.
  /** @type {WeakMap<Filters, Record<string, Set<string>>>} */
  const valueSets = new WeakMap();
  /** @param {Filters} filters @returns {Record<string, Set<string>>} */
  const setsOf = (filters) => {
    let s = valueSets.get(filters);
    if (!s) {
      s = {};
      for (const col of Object.keys(filters)) s[col] = new Set(filters[col].map(keyOf));
      valueSets.set(filters, s);
    }
    return s;
  };

  /** Does a row satisfy every column of the filter? A null in the filter takes the rows where the
   * column is missing. @param {any} row @param {Filters} filters */
  const rowMatches = (row, filters) => {
    const sets = setsOf(filters);
    for (const col of Object.keys(sets)) {
      if (!sets[col].has(keyOf(row[col]))) return false;
    }
    return true;
  };

  /**
   * Is a value within [lo, hi]? Numbers compare as numbers with a little
   * slack; anything else as text (ISO dates sort that way).
   * @param {any} v @param {any[] | null} r
   */
  const within = (v, r) => {
    if (!r) return true;
    const [lo, hi] = r;
    if (typeof lo === 'number' && typeof hi === 'number') {
      const n = Number(v);
      const eps = 1e-12 * Math.max(1, Math.abs(lo), Math.abs(hi));
      return n >= lo - eps && n <= hi + eps;
    }
    const s = String(v);
    return s >= String(lo) && s <= String(hi);
  };

  /**
   * Is a drawn point ([x, y, ...]) inside a range selection?
   * @param {any[]} p @param {{ x_range: any[], y_range: any[] | null }} sel
   */
  const pointInRange = (p, sel) =>
    within(p[0], sel.x_range) && within(p[1], sel.y_range);

  /**
   * Is a mark lit under a filter? When every filtered column is one of the
   * mark's keys, the keys decide; otherwise the mark is lit when one of its
   * rows passes the filter. `rows` may be a function, called only if needed.
   * @param {Record<string, any>} keys @param {any[] | (() => any[])} rows
   * @param {Filters} filters
   */
  const markLit = (keys, rows, filters) => {
    const sets = setsOf(filters);
    const cols = Object.keys(sets);
    if (cols.every((c) => c in keys)) {
      return cols.every((c) => sets[c].has(keyOf(keys[c])));
    }
    const rs = typeof rows === 'function' ? rows() : rows;
    for (const r of rs || []) if (rowMatches(r, filters)) return true;
    return false;
  };

  NS.keys = { isMissing, fromKeys, fromRows, range, restore,
              describe, describeRange, rowMatches, pointInRange, markLit,
              MAX_LISTED };
})();
