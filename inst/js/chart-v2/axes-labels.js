// @ts-check
/**
 * Chart block v2: axes and their labels, as plain functions.
 *
 * Axis types and category order from the column metadata, axis titles,
 * the category label gutter (measured with a function the caller passes, so
 * no canvas is needed here), value formatting on an axis, and the shared
 * domain of a facet grid.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});

  /** @param {VizColumn[] | null | undefined} columns @param {any} name */
  const colMeta = (columns, name) =>
    /** @type {any} */ ((columns || []).find((c) => c.name === name));

  /**
   * An axis title: the column's label when it has one, else its name.
   * @param {VizColumn[]} columns @param {any} col @returns {string}
   */
  const axisTitle = (columns, col) => {
    if (!col) return '';
    const c = colMeta(columns, col);
    if (c && c.label && c.label !== c.name) return c.label;
    return col;
  };

  /**
   * 'category' for a categorical column, 'time' for a *DT column holding
   * epoch milliseconds, else 'value'.
   * @param {VizColumn[]} columns @param {any[]} rows @param {any} colName
   * @returns {'category' | 'time' | 'value'}
   */
  const axisTypeFor = (columns, rows, colName) => {
    if (!colName) return 'value';
    const meta = colMeta(columns, colName);
    if (meta && meta.type === 'categorical') return 'category';
    if (/DT$/i.test(colName)) {
      const sample = (rows || []).find((r) => r[colName] != null);
      if (sample && typeof sample[colName] === 'number' && sample[colName] > 1e11) {
        return 'time';
      }
    }
    return 'value';
  };

  /**
   * The categories of a column on an axis: factor levels first (values
   * outside them after, in order of appearance), else by a companion number
   * (AVISIT by AVISITN), else in order of appearance.
   * @param {VizColumn[]} columns @param {any[]} rows @param {string} colName
   * @returns {string[]}
   */
  const orderedCategories = (columns, rows, colName) => {
    const src = rows || [];
    const meta = colMeta(columns, colName);
    if (meta && Array.isArray(meta.levels) && meta.levels.length) {
      const present = new Set(src.map((r) => String(r[colName] ?? '')));
      const out = meta.levels.map(String).filter((/** @type {string} */ l) => present.has(l));
      const inOut = new Set(out);
      for (const k of present) if (!inOut.has(k)) { inOut.add(k); out.push(k); }
      return out;
    }
    /** @type {Record<string, string>} */
    const companions = { AVISIT: 'AVISITN' };
    const companion = companions[colName];
    if (companion && src.length && src[0][companion] !== undefined) {
      /** @type {Record<string, number>} */
      const mins = {};
      for (const r of src) {
        const k = String(r[colName] ?? '');
        const v = Number(r[companion]);
        if (!isNaN(v) && (mins[k] == null || v < mins[k])) mins[k] = v;
      }
      return Object.keys(mins).sort((a, b) => mins[a] - mins[b]);
    }
    const seen = new Set();
    const out = [];
    for (const r of src) {
      const k = String(r[colName] ?? '');
      if (!seen.has(k)) { seen.add(k); out.push(k); }
    }
    return out;
  };

  /**
   * Order levels: by the board scale's order, else the column's factor
   * levels, else alphabetically. Unknown levels sort last, alphabetically.
   * @param {any[]} levels @param {any} scale @param {VizColumn[]} columns
   * @param {string} colName @returns {any[]}
   */
  const orderLevels = (levels, scale, columns, colName) => {
    /** @param {any[]} ref */
    const by = (ref) => {
      /** @type {Map<string, number>} */
      const idx = new Map(ref.map((l, i) => /** @type {[string, number]} */ ([String(l), i])));
      const at = (/** @type {any} */ x) => (idx.has(x) ? /** @type {number} */ (idx.get(x)) : 1e9);
      return levels.slice().sort((a, b) => (at(a) - at(b)) || a.localeCompare(b));
    };
    if (scale && Array.isArray(scale.order) && scale.order.length) return by(scale.order);
    const meta = colMeta(columns, colName);
    if (meta && Array.isArray(meta.levels) && meta.levels.length) return by(meta.levels);
    return levels.slice().sort();
  };

  /**
   * The category label gutter of a horizontal axis, fitted to the widest
   * label up to a cap: the label width, its margin and the grid's left edge.
   * @param {any[]} labels @param {(s: string) => number} measure
   */
  const yGutter = (labels, measure) => {
    const LABEL_CAP = 145;
    // Slack for the fallback font the canvas may measure with before the
    // page font loads; without it short labels truncate.
    const PAD = 10;
    let w = 0;
    for (const l of labels) {
      const tw = measure(String(l ?? ''));
      if (tw > w) w = tw;
      if (w >= LABEL_CAP) break;
    }
    w = Math.min(Math.ceil(w) + PAD, LABEL_CAP);
    return { width: w, margin: w + 5, gridLeft: w + 15 };
  };

  /**
   * An x value for a tooltip: the category on a category axis, a date on a
   * time axis, else a trimmed number.
   * @param {any} v @param {string} xAxisType @param {any[] | null} xCats
   */
  const fmtXVal = (v, xAxisType, xCats) => {
    if (v == null || v === '') return '';
    if (xAxisType === 'category') {
      return (xCats && xCats[v] != null) ? String(xCats[v]) : String(v);
    }
    if (xAxisType === 'time') {
      const d = new Date(Number(v));
      return isNaN(d.getTime()) ? String(v) : d.toISOString().slice(0, 10);
    }
    return NS.ddNum(Number(v));
  };

  /**
   * The span between two x values: days on a time axis, units on a value
   * axis, nothing on a category axis.
   * @param {any} a @param {any} b @param {string} xAxisType
   */
  const durationStr = (a, b, xAxisType) => {
    if (xAxisType === 'category') return '';
    const s = Number(a), e = Number(b);
    if (isNaN(s) || isNaN(e)) return '';
    if (xAxisType === 'time') {
      const days = Math.round((e - s) / 86400000);
      return days + (Math.abs(days) === 1 ? ' day' : ' days');
    }
    return String(Math.round((e - s) * 100) / 100);
  };

  /**
   * One domain over the panels' own (already nice-rounded) extents, or null
   * when fewer than two panels have a numeric one.
   * @param {Array<number[] | null>} extents @returns {number[] | null}
   */
  const sharedDomain = (extents) => {
    let lo = Infinity, hi = -Infinity, n = 0;
    for (const ext of extents) {
      if (!ext || !Number.isFinite(ext[0]) || !Number.isFinite(ext[1])) continue;
      if (ext[0] < lo) lo = ext[0];
      if (ext[1] > hi) hi = ext[1];
      n++;
    }
    return (n >= 2 && lo < hi) ? [lo, hi] : null;
  };

  NS.axes = { colMeta, axisTitle, axisTypeFor, orderedCategories, orderLevels, yGutter,
              fmtXVal, durationStr, sharedDomain };
})();
