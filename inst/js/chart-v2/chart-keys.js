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
   * The filter a saved board restores: filter_column + filter_values, as
   * R sends them. Null without a column or without values.
   * @param {Record<string, any>} cfg @returns {Filters | null}
   */
  const restored = (cfg) => {
    const col = cfg.filter_column;
    const vals = cfg.filter_values;
    if (!col || !Array.isArray(vals) || !vals.length) return null;
    return { [col]: vals.slice() };
  };

  /** The filter message for a click. @param {Filters} filters @param {number} nonce */
  const filterMessage = (filters, nonce) => ({
    action: 'filter', filter_type: 'categorical', filters, nonce
  });

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

  NS.keys = { isMissing, fromKeys, fromRows, restored, filterMessage, clearMessage, describe,
              rowMatches, markLit, selectionView, MAX_LISTED,
              MISSING_WORD };
})();
