// @ts-check
/**
 * The variables layout: a Table 1, one row per statistic or level of each
 * variable, one column per level of `by`. R builds it as a structured table
 * (sections per variable, the statistics indented), which the records view
 * draws; the cards look draws it as a table too.
 *
 * The variables are the `summaries` entries' columns (R:
 * table_summaries_vars()). The gear picks them as one list of columns, and
 * this file turns the pick back into entries: a numeric column a `dist`
 * entry, any other a `simple` count split by it. Entries kept keep every
 * key they had.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const T = V.table;

  /**
   * The variable an entry summarises, as R reads it.
   * @param {Record<string, unknown>} s
   * @returns {string | null}
   */
  function varOf(s) {
    const v = s.type === 'simple' ? (s.color != null ? s.color : s.col)
      : s.type === 'dist' || s.type === 'field' ? s.col : null;
    return typeof v === 'string' && v ? v : null;
  }

  /**
   * The entries for a pick of variables, in the pick's order.
   * @param {Record<string, unknown>[]} summaries
   * @param {string[]} vars
   * @param {VizPayload['columns']} columns
   */
  function withVars(summaries, vars, columns) {
    const numeric = new Set(columns.filter((c) => /^(numeric|integer|double)$/.test(String(c.class)))
      .map((c) => c.name));
    return vars.map((v) => summaries.find((s) => varOf(s) === v) ||
      (numeric.has(v) ? { type: 'dist', col: v } : { type: 'simple', color: v }));
  }

  /**
   * The gear's Variables row: a column picker over the entries.
   * @param {HTMLElement} el
   * @param {TableGearCtx} ctx
   */
  function picker(el, ctx) {
    const C = /** @type {any} */ (V).gear && /** @type {any} */ (V).gear.controls;
    if (!C) return;
    const summaries = /** @type {Record<string, unknown>[]} */ (
      Array.isArray(ctx.values.summaries) ? ctx.values.summaries : []);
    const vars = summaries.map(varOf).filter((v) => v != null);
    const by = /** @type {string[]} */ (Array.isArray(ctx.values.by) ? ctx.values.by : []);
    el.textContent = '';
    C.build(el, {
      key: 'summaries', label: 'Variables', value: vars, values: ctx.values,
      arg: { type: { name: 'columns' }, nullable: false,
             gear: { group: 'Variables', control: 'columns', label: 'Variables' } },
      columns: ctx.columns.filter((c) => !by.includes(c.name)),
      required: false, scope: ctx.scope, host: {}, titles: {},
      labelOf: (/** @type {string} */ k) => k,
      set: (/** @type {unknown} */ v) => ctx.set(withVars(summaries, /** @type {string[]} */ (v || []),
                                                         ctx.columns))
    });
  }

  class Variables extends /** @type {any} */ (T.Records) {
    /** A Table 1 has no card form; it draws as a table. */
    cardsOn() {
      return false;
    }

    /** @returns {TableInfo} */
    info() {
      return { structured: true, finished: false, numbers: false };
    }
  }

  T.layouts.variables = /** @type {TableLayoutClass} */ (/** @type {unknown} */ (Variables));
  T.editors = Object.assign(T.editors || {}, { 'variables.summaries': picker });
  T.variables = { varOf, withVars };
})();
