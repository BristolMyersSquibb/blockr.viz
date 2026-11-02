// @ts-check
/**
 * Sorting and nesting helpers of the table body (dom.js): sort keys from
 * the display texts, the parents a matching row keeps, and the tree sort
 * of an indented list.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /**
   * The first number in a display text: "1,234.5 kg" is 1234.5.
   * @param {unknown} s
   * @returns {number | null}
   */
  function parseNum(s) {
    if (s == null) return null;
    const m = String(s).match(/-?\d[\d,]*(\.\d+)?/);
    return m ? parseFloat(m[0].replace(/,/g, '')) : null;
  }

  /**
   * Sort keys of one column: numbers where both sides parse, else text.
   * @param {TableCol} col
   * @param {number} n
   */
  function sortKeys(col, n) {
    /** @type {(number | null)[]} */
    const num = new Array(n);
    /** @type {string[]} */
    const str = new Array(n);
    for (let i = 0; i < n; i++) {
      const k = col.key ? col.key[i] : col.disp[i];
      const s = k == null ? '' : String(k).trim();
      str[i] = s;
      num[i] = col.key && typeof k === 'number' ? k : parseNum(s);
    }
    return { num, str };
  }

  /**
   * The matching rows and the rows they sit under: an indented row's
   * parents stay with it, so a match keeps its context.
   * @param {number[]} view Rows in data order.
   * @param {number[]} indent
   */
  function withParents(view, indent) {
    const keep = new Set(view);
    for (const i of view) {
      let cur = indent[i];
      for (let j = i - 1; j >= 0 && cur > 0; j--) {
        if (indent[j] < cur) {
          keep.add(j);
          cur = indent[j];
        }
      }
    }
    return Array.from(keep).sort((a, b) => a - b);
  }

  /**
   * Sort an indented list as a tree: siblings by `cmp`, each row followed
   * by its own children, sorted the same way.
   * @param {number[]} rows Rows in data order.
   * @param {number[]} indent
   * @param {(a: number, b: number) => number} cmp
   * @returns {number[]}
   */
  function treeSort(rows, indent, cmp) {
    if (rows.length < 2) return rows.slice();
    const top = Math.min(...rows.map((i) => indent[i]));
    /** @type {number[][]} */
    const units = [];
    for (const i of rows) {
      if (indent[i] === top || !units.length) units.push([i]);
      else units[units.length - 1].push(i);
    }
    units.sort((u, v) => cmp(u[0], v[0]));
    return units.flatMap((u) => [u[0]].concat(treeSort(u.slice(1), indent, cmp)));
  }

  Object.assign(V.table, { parseNum, sort: { sortKeys, withParents, treeSort } });
})();
