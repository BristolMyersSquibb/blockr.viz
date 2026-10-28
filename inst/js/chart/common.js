// @ts-check
/**
 * Chart block: the namespace (Blockr.viz.chart), which every chart file
 * fills, and the small helpers they share: elements, set values, the
 * labelled lists of the spec's wire form, extremes and first positions.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart = {};

  /** @param {any} v */
  const hasVal = (v) => v !== null && v !== undefined && v !== '' && v !== '(none)';

  /** A `numbers` or `values` setting, `[{value, label}]`, or [] when unset.
   * @param {any} v @returns {{ value: any, label: string | null }[]} */
  const labelled = (v) => (Array.isArray(v) ? v : []);

  /** The `x_ranges` setting, `{name: [from, to]}`, as a list of spans.
   * @param {any} r @returns {{ name: string, from: any, to: any }[]} */
  const rangesOf = (r) => (r && typeof r === 'object' ? Object.entries(r) : [])
    .map(([name, span]) => {
      const s = labelled(span);
      return { name, from: s[0] ? s[0].value : null, to: s[1] ? s[1].value : null };
    }).filter((x) => x.from != null && x.to != null);

  // Largest / smallest without spreading into arguments (a RangeError past
  // about 100k values). NaN as soon as one value is NaN, like Math.max.
  /** @param {ArrayLike<any>} xs @param {number} init @param {(v: number, m: number) => boolean} wins */
  const extreme = (xs, init, wins) => {
    let m = init;
    for (let i = 0; i < xs.length; i++) {
      const v = +xs[i];
      if (v !== v) return NaN;
      if (wins(v, m)) m = v;
    }
    return m;
  };
  /** @param {ArrayLike<any>} xs */
  const maxOf = (xs) => extreme(xs, -Infinity, (v, m) => v > m);
  /** @param {ArrayLike<any>} xs */
  const minOf = (xs) => extreme(xs, Infinity, (v, m) => v < m);

  /** Value -> its first position in `arr`, for a lookup made once per row.
   *  @param {any[]} arr @returns {Map<any, number>} */
  const firstIndex = (arr) => {
    const m = new Map();
    for (let i = 0; i < arr.length; i++) if (!m.has(arr[i])) m.set(arr[i], i);
    return m;
  };

  /** A new element with a class, appended to `parent` when given.
   * @param {string} tag @param {string} cls @param {HTMLElement} [parent] */
  const mk = (tag, cls, parent) => {
    const e = document.createElement(tag);
    e.className = cls;
    if (parent) parent.appendChild(e);
    return e;
  };

  // Ceiling for a panel's plot height: a canvas past the browser's area
  // limit renders blank, so past it the lanes squeeze instead.
  const PANEL_H_CAP = 4000;

  Object.assign(NS, { hasVal, labelled, rangesOf, maxOf, minOf, firstIndex, mk, PANEL_H_CAP });
})();
