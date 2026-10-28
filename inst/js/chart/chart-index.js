// @ts-check
/**
 * Chart block: rows and the lookups over them.
 *
 * rowsFromPayload() turns the column object R sends into rows. RowIndex
 * buckets rows by a column once and answers every later lookup from a Map,
 * which is what keeps a chart with a level per patient linear. labelCounts()
 * is the "(n)" next to an axis or facet label.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});

  /** @typedef {'nz' | 'raw'} KeyMode */
  // Shared by every miss.
  const NO_ROWS = /** @type {any[]} */ (/** @type {unknown} */ (Object.freeze([])));

  // Lookups over row arrays, memoized on the rows array itself. Callers
  // share the results and must not mutate them. A key is the column value as
  // a string: 'nz' is String(v ?? '') (null joins the '' level), 'raw' is
  // String(v) (null is "null").
  class RowIndex {
    constructor() {
      /** @type {WeakMap<any[], Map<string, any>>} */
      this._memo = new WeakMap();
    }

    /** @param {any[]} rows @param {string} key @param {() => any} build */
    _cached(rows, key, build) {
      let m = this._memo.get(rows);
      if (!m) { m = new Map(); this._memo.set(rows, m); }
      if (!m.has(key)) m.set(key, build());
      return m.get(key);
    }

    /** @param {KeyMode} mode @returns {(v: any) => string} */
    static keyFn(mode) {
      return mode === 'nz' ? (v) => String(v ?? '') : (v) => String(v);
    }

    /** Rows by key of `col`, in row order; Map order is first-seen key order. @param {any[]} rows
     * @param {string} col @param {KeyMode} mode @returns {Map<string, any[]>} */
    buckets(rows, col, mode) {
      return this._cached(rows, 'b\u0000' + mode + '\u0000' + col, () => {
        const key = RowIndex.keyFn(mode);
        /** @type {Map<string, any[]>} */
        const out = new Map();
        for (const r of rows) {
          const k = key(r[col]);
          const b = out.get(k);
          if (b) b.push(r); else out.set(k, [r]);
        }
        return out;
      });
    }

    /** Rows by key of `colA`, then of `colB` (nested, so no two pairs collide). @param {any[]} rows
     * @param {string} colA @param {KeyMode} modeA @param {string} colB @param {KeyMode} modeB
     * @returns {Map<string, Map<string, any[]>>} */
    buckets2(rows, colA, modeA, colB, modeB) {
      return this._cached(rows,
        'b2\u0000' + modeA + '\u0000' + colA + '\u0000' + modeB + '\u0000' + colB, () => {
          const keyA = RowIndex.keyFn(modeA), keyB = RowIndex.keyFn(modeB);
          /** @type {Map<string, Map<string, any[]>>} */
          const out = new Map();
          for (const r of rows) {
            const a = keyA(r[colA]);
            let inner = out.get(a);
            if (!inner) { inner = new Map(); out.set(a, inner); }
            const b = keyB(r[colB]);
            const bucket = inner.get(b);
            if (bucket) bucket.push(r); else inner.set(b, [r]);
          }
          return out;
        });
    }

    /** The rows whose `col` keys to `k`. @param {any[]} rows @param {string} col
     * @param {KeyMode} mode @param {string} k @returns {any[]} */
    get(rows, col, mode, k) {
      return this.buckets(rows, col, mode).get(k) || NO_ROWS;
    }

    /** The rows keyed `a` on `colA` and `b` on `colB`. @param {any[]} rows @param {string} colA
     * @param {KeyMode} modeA @param {string} colB @param {KeyMode} modeB @param {string} a
     * @param {string} b @returns {any[]} */
    get2(rows, colA, modeA, colB, modeB, a, b) {
      const inner = this.buckets2(rows, colA, modeA, colB, modeB).get(a);
      return (inner && inner.get(b)) || NO_ROWS;
    }

    /** Distinct keys of `col`, first-seen order. @param {any[]} rows @param {string} col
     * @param {KeyMode} mode @returns {string[]} */
    levels(rows, col, mode) {
      return this._cached(rows, 'l\u0000' + mode + '\u0000' + col,
        () => Array.from(this.buckets(rows, col, mode).keys()));
    }
  }

  /** Rows from what R sends: a JSON string, a column object (a column may be dictionary-encoded as
   * {__enc__: 'dict', levels, codes}) or rows already. @param {any} data @returns {any[]} */
  const rowsFromPayload = (data) => {
    if (typeof data === 'string') data = JSON.parse(data);
    if (!data) return [];
    if (Array.isArray(data)) return data;
    const keys = Object.keys(data);
    for (const k of keys) {
      const col = data[k];
      if (col && !Array.isArray(col) && col.__enc__ === 'dict') {
        const levels = col.levels || [];
        const codes = col.codes || [];
        const out = new Array(codes.length);
        for (let i = 0; i < codes.length; i++) out[i] = levels[codes[i]];
        data[k] = out;
      }
    }
    const n = keys.length > 0 ? (Array.isArray(data[keys[0]]) ? data[keys[0]].length : 1) : 0;
    const rows = new Array(n);
    for (let i = 0; i < n; i++) {
      /** @type {Record<string, any>} */
      const row = {};
      for (const k of keys) row[k] = Array.isArray(data[k]) ? data[k][i] : data[k];
      rows[i] = row;
    }
    return rows;
  };

  /**
   * Count per value of `dimCol`: distinct `id` when set, else rows. With
   * func 'identity' it is the id column's first value per group as it is.
   * @param {any[]} rows @param {Record<string, any>} cfg
   * @param {string | null | undefined} dimCol
   * @returns {Map<string, any>}
   */
  const labelCounts = (rows, cfg, dimCol) => {
    const idCol = cfg.id;
    const asIs = !!idCol && cfg.func === 'identity';
    /** @type {Map<string, any>} */
    const out = new Map();
    /** @type {Map<string, Set<any>>} */
    const ids = new Map();
    for (const r of rows) {
      const dv = dimCol ? String(r[dimCol] ?? '') : 'Total';
      if (asIs) {
        if (!out.has(dv) && r[idCol] != null) out.set(dv, r[idCol]);
      } else if (idCol) {
        if (r[idCol] == null) continue;
        let s = ids.get(dv);
        if (!s) { s = new Set(); ids.set(dv, s); }
        s.add(r[idCol]);
        out.set(dv, s.size);
      } else {
        out.set(dv, (out.get(dv) || 0) + 1);
      }
    }
    return out;
  };

  /**
   * " (n)" after a label, from a counts map; a non-integer rounds to 2 places.
   * @param {any} label @param {Map<string, any> | null | undefined} counts
   */
  const withCount = (label, counts) => {
    const s = String(label ?? '');
    if (!counts) return s;
    const v = counts.get(s);
    if (v == null) return s;
    const disp = (typeof v === 'number' && !Number.isInteger(v))
      ? Math.round(v * 100) / 100 : v;
    return s + ' (' + disp + ')';
  };

  /** Does show_n cover `surface` ('axis' | 'facet')?
   *  @param {Record<string, any>} cfg @param {string} surface */
  const countOn = (cfg, surface) => {
    const v = cfg.show_n;
    return v === surface || v === 'both';
  };

  Object.assign(NS, { RowIndex, NO_ROWS, rowsFromPayload, labelCounts, withCount, countOn });
})();
