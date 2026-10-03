// @ts-check
/**
 * Chart block v2: a mark's keys and the filters made of them.
 *
 * Every mark carries its keys, the column values that define it
 * ({AETERM: "Rash"}, plus the facet column inside a facet panel). One rule
 * then serves every chart type:
 *
 *   - a click sends the clicked mark's keys as a filter (D2, D3), in the
 *     shape R's dd_ctrl_claims() already takes: {col: [values]};
 *   - the footer names that filter in column labels (D4);
 *   - a restored filter lights the marks that hold filtered rows (D5).
 *
 * A filter here is a plain object {col: [values]}; the values are the
 * strings R compares against. A missing value is a key too: a mark keyed
 * on a missing value ('' or null) sends that column with null, which R
 * matches with is.na() (and the empty string, as the aggregation folds
 * both).
 *
 * A range filter (a point or a brush on numeric axes) is
 * {x_col, y_col, x_range, y_range}, as v1 sends it. In a facet panel it
 * carries the panel's key in the same `filters` field a click uses (D10).
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});

  /** @typedef {Record<string, any[]>} Filters */

  /** Is a key value missing? The aggregation keys a missing value as ''.
   *  @param {any} v */
  const isMissing = (v) => v == null || v === '';

  /**
   * A filter from a mark's keys: one value per column, null for a missing
   * value.
   * @param {Record<string, any>} keys @returns {Filters}
   */
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
   * A filter on `col` with the distinct non-missing values `rows` hold, or
   * null when they hold none.
   * @param {string} col @param {any[]} rows @returns {Filters | null}
   */
  const fromRows = (col, rows) => {
    const vals = [...new Set(
      (rows || []).map((r) => (r ? r[col] : null)).filter((v) => v != null)
    )].map(String);
    return vals.length ? { [col]: vals } : null;
  };

  /**
   * A filter as R sends it back: {col: [values]}, a null value for a
   * missing one. A scalar is one value. Null when no column has a value.
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
   * The categorical filter a saved board restores: `filters`, else v1's
   * filter_column + filter_values. Null for a saved range, and without a
   * column or values.
   * @param {Record<string, any>} cfg @returns {Filters | null}
   */
  const restored = (cfg) => {
    if (cfg.filter_type === 'range') return null;
    const f = clean(cfg.filters);
    if (f) return f;
    const col = cfg.filter_column;
    const vals = cfg.filter_values;
    if (!col || !Array.isArray(vals) || !vals.length) return null;
    return { [col]: vals.slice() };
  };

  /**
   * The range filter a saved board restores, with the facet key it was
   * taken in (D10, D11), or null.
   * @param {Record<string, any>} cfg
   * @returns {{ range: { x_col: string, y_col: string | null, x_range: any[],
   *                      y_range: any[] | null }, filters: Filters | null } | null}
   */
  const restoredRange = (cfg) => {
    const r = cfg.filter_range;
    if (cfg.filter_type !== 'range' || !r || !r.x_col || !Array.isArray(r.x_range)) return null;
    const yr = Array.isArray(r.y_range) && r.y_range.length ? r.y_range : null;
    return {
      range: { x_col: r.x_col, y_col: yr ? (r.y_col || null) : null, x_range: r.x_range, y_range: yr },
      filters: clean(cfg.filters)
    };
  };

  /** The filter message for a click. @param {Filters} filters @param {number} nonce */
  const filterMessage = (filters, nonce) => ({
    action: 'filter', filter_type: 'categorical', filters, nonce
  });

  /**
   * The message for a range: v1's fields, plus the facet key in `filters`
   * when the range was taken in a facet panel (D10).
   * @param {{ x_col: any, y_col: any, x_range: any, y_range: any }} range
   * @param {Filters | null} [filters]
   */
  const rangeMessage = (range, filters) => {
    /** @type {Record<string, any>} */
    const msg = {
      action: 'filter', filter_type: 'range',
      x_col: range.x_col, y_col: range.y_range ? range.y_col : null,
      x_range: range.x_range, y_range: range.y_range
    };
    if (filters && Object.keys(filters).length) msg.filters = filters;
    return msg;
  };

  /**
   * The message that clears a filter. It names the type of the filter it
   * clears (B1); every other field is as v1 sends it.
   * @param {'categorical' | 'range' | 'point'} type
   */
  const clearMessage = (type) => ({
    action: 'filter', filter_type: type,
    column: null, values: null,
    x_col: null, y_col: null, x_range: null, y_range: null
  });

  // Above this many values a column is named with its count, not its values.
  const MAX_LISTED = 3;

  // How the footer names a missing value.
  const MISSING_WORD = '(missing)';

  /**
   * The words for a filter, in column labels (D4): "Reported Term = Rash",
   * "Reported Term = Rash, Severity = MODERATE", "Severity = (missing)",
   * "Patient, 214 values".
   * @param {Filters} filters @param {VizColumn[]} columns @returns {string}
   */
  const describe = (filters, columns) => Object.keys(filters).map((col) => {
    const label = NS.axes.axisTitle(columns, col);
    const vals = filters[col];
    if (vals.length > MAX_LISTED) return label + ', ' + vals.length + ' values';
    return label + ' = ' + vals.map((v) => (isMissing(v) ? MISSING_WORD : v)).join(', ');
  }).join(', ');

  /**
   * Decimals that show a number to three significant digits; none for a
   * whole number.
   * @param {number} v
   */
  const decimalsOf = (v) => {
    if (!Number.isFinite(v) || Number.isInteger(v)) return 0;
    const s = String(Number(v.toPrecision(3)));
    if (/e/i.test(s)) return 0;
    const dot = s.indexOf('.');
    return dot < 0 ? 0 : s.length - dot - 1;
  };

  /**
   * One end of a range, or both: "2 to 86", "21.4 to 30.0" (both ends at
   * the same decimals), "= 15" for a single value. Dates on a time axis,
   * categories as they are.
   * @param {any[]} r @param {string} [axisType]
   */
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
   * The words for a range filter (D4): "Study Day 2 to 86, Value 21.4 to
   * 30.0"; a point reads "Study Day = 15, Value = 29.2". The facet key of
   * a range taken in a panel follows: ", Arm = Placebo" (D10).
   * @param {{ x_col: string, y_col: string | null, x_range: any[],
   *           y_range: any[] | null }} range
   * @param {VizColumn[]} columns @param {string} [xAxisType]
   * @param {Filters | null} [filters]
   */
  const describeRange = (range, columns, xAxisType, filters) => {
    const parts = [NS.axes.axisTitle(columns, range.x_col) + ' ' +
                   rangeWords(range.x_range, xAxisType)];
    if (range.y_range && range.y_col) {
      parts.push(NS.axes.axisTitle(columns, range.y_col) + ' ' + rangeWords(range.y_range));
    }
    if (filters && Object.keys(filters).length) parts.push(describe(filters, columns));
    return parts.join(', ');
  };

  /** A value as a filter compares it: missing folds to ''. @param {any} v */
  const keyOf = (v) => (isMissing(v) ? '' : String(v));

  // The values of each filtered column as a set of strings, made once per
  // filter: a highlight asks for every mark.
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

  /**
   * Does a row satisfy every column of the filter? A null in the filter
   * takes the rows where the column is missing.
   * @param {any} row @param {Filters} filters
   */
  const rowMatches = (row, filters) => {
    const sets = setsOf(filters);
    for (const col of Object.keys(sets)) {
      if (!sets[col].has(keyOf(row[col]))) return false;
    }
    return true;
  };

  /**
   * Is a value within [lo, hi]? Numbers compare as numbers, with a little
   * slack for a value that came back from R as text; anything else as
   * text (ISO dates sort that way).
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
   * Is a drawn point ([x, y, ...]) inside a range filter (D11)?
   * @param {any[]} p @param {{ x_range: any[], y_range: any[] | null }} range
   */
  const pointInRange = (p, range) =>
    within(p[0], range.x_range) && within(p[1], range.y_range);

  /**
   * Is a mark lit under a filter (D5)? When every filtered column is one of
   * the mark's keys, the keys decide; otherwise the mark is lit when one of
   * its rows passes the filter (patient S01 on a chart of terms lights every
   * term S01 has). `rows` may be a function, called only when needed.
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

  /**
   * The v1 view of a filter, for code that reads one column and its values:
   * the column (several: their names) and the value (several: the array; on
   * several columns: the filter itself).
   * @param {Filters | null} filters
   * @returns {{ column: any, selected: any }}
   */
  const selectionView = (filters) => {
    if (!filters) return { column: null, selected: null };
    const cols = Object.keys(filters);
    if (cols.length === 1) {
      const v = filters[cols[0]];
      return { column: cols[0], selected: v.length === 1 ? v[0] : v };
    }
    return { column: cols, selected: filters };
  };

  NS.keys = { isMissing, fromKeys, fromRows, clean, restored, restoredRange, filterMessage,
              rangeMessage, clearMessage, describe, describeRange, rangeWords, rowMatches,
              pointInRange, markLit, selectionView, MAX_LISTED, MISSING_WORD };
})();
