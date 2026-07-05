// @ts-check
/**
 * DrilldownAgg — the shared aggregation vocabulary for the blockr drilldown
 * renderers (chart, table, tile). One home for the three pieces that used to
 * be inlined in chart.js, so the table and tile render the identical control
 * and behave identically:
 *
 *   AGG_FNS
 *     The aggregation-function select options. MUST mirror the R
 *     `AGG_FNS` / `arg_enum(AGG_FNS)` in block-arguments.R (single source per
 *     side; a drift test guards it).
 *
 *   aggRoles({ multiple })
 *     The `group` + `value` + `func` role-spec triple the DrilldownConfig
 *     engine renders. Hosts spread it into their ROLES dict. `multiple` widens
 *     `group` to a multi-column picker (the table groups by several columns;
 *     the chart and tile group by one). (`value`/`func` were `metric`/`agg_fn`;
 *     see dev/unified-arg-naming.md.)
 *
 *   reconcileValue(cfg, columns)
 *     Keep `value` consistent with `func`: "count" ignores the value (force
 *     the synthetic '.count'); "count_distinct" takes any column but not
 *     '.count'; the numeric aggregations need a numeric column. A value that
 *     no longer fits is emptied — the picker then shows the required-empty
 *     state instead of silently charting a wrong number.
 *
 *   aggregate(rows, {group, color, facet, value, func})
 *     The client-side aggregation ENGINE (the chart's twin of the R
 *     dd_table_aggregate() in R/table-block.R). Pure data -> data, no DOM /
 *     Shiny — it must stay loadable in plain node (the golden cross-test in
 *     tests/testthat/test-agg-golden.R executes this file standalone and
 *     compares the numbers against the R engine).
 *
 * Exposed as Blockr.DrilldownAgg (and window.DrilldownAgg). Must load before
 * chart.js / table.js / tile-block.js.
 */
(() => {
  'use strict';

  const AGG_FNS = [
    { value: 'count', label: 'Count' },
    { value: 'count_distinct', label: 'Count distinct' },
    { value: 'mean', label: 'Mean' },
    { value: 'median', label: 'Median' },
    { value: 'sum', label: 'Sum' },
    { value: 'min', label: 'Min' },
    { value: 'max', label: 'Max' }
  ];

  // One word per aggregation, for composed labels ("Mean AGE", axis titles,
  // tooltips). Single home — the hosts used to duplicate this map.
  /** @type {Record<string, string>} */
  const AGG_WORDS = {
    count: 'Count', count_distinct: 'Distinct', mean: 'Mean',
    median: 'Median', sum: 'Sum', min: 'Min', max: 'Max'
  };

  /** @param {{ multiple?: boolean }} [opts] */
  function aggRoles(opts) {
    const multiple = !!(opts && opts.multiple);
    return {
      group: {
        label: 'Group', kind: multiple ? 'columns' : 'column',
        colType: 'cat', ph: 'category column…',
        // The table's multi-group gates the summaries list, so a change
        // re-renders the gear (reveals/hides the aggregations). The chart's
        // single group gates nothing, so it does not.
        rerender: multiple
      },
      // The value picker follows the aggregation (see reconcileValue): row
      // count ignores the value, count_distinct takes any column, the numeric
      // aggregations need a numeric column. `pairReversed` renders the row as a
      // verb-object phrase — "[func] of [column]" — so the aggregation leads.
      // Config keys `value` / `func` (were `metric` / `agg_fn`); see
      // dev/unified-arg-naming.md.
      value: {
        label: 'Value', kind: 'column', pairedWith: 'func',
        pairReversed: true,
        ph: 'column to aggregate…',
        colType: (/** @type {any} */ cfg) =>
          cfg.func === 'count_distinct' ? 'any'
            : (!cfg.func || cfg.func === 'count') ? 'none' : 'num',
        allowCount: (/** @type {any} */ cfg) =>
          !cfg.func || cfg.func === 'count'
      },
      func: { label: 'Aggregate', kind: 'select', options: AGG_FNS, rerender: true }
    };
  }

  /**
   * Keep `value` consistent with `func` (the aggregation function).
   * @param {any} cfg     the mutable config object (mutated in place)
   * @param {any[]} columns  column metadata [{name, type, ...}]
   */
  function reconcileValue(cfg, columns) {
    if (!cfg.func || cfg.func === 'count') { cfg.value = '.count'; return; }
    if (cfg.func === 'count_distinct') {
      if (cfg.value === '.count') cfg.value = '';
      return;
    }
    const col = (columns || []).find((/** @type {any} */ c) => c.name === cfg.value);
    if (!col || col.type !== 'numeric') cfg.value = '';
  }

  /**
   * Group + aggregate rows. Semantics are aligned to the R engine
   * (dd_table_aggregate / dd_metric_plan in R/table-block.R — the source of
   * truth); the golden cross-test guards the pair:
   *
   *   count           rows in the (facet, group, color) cell.
   *   count_distinct  distinct non-missing values of `value` in the cell
   *                   (R: dplyr::n_distinct(x, na.rm = TRUE)).
   *   sum             sum over usable numeric values; an empty cell sums
   *                   to 0 (R: sum(x, na.rm = TRUE)).
   *   mean/median/    over usable numeric values; a cell with NONE yields
   *   min/max         null — no value, not a fabricated 0 (R: NA). The
   *                   chart renders null as a gap (ECharts native).
   *
   * "Usable" = non-null and coercible to a number, so a single bad cell
   * can't poison a mean/sum into NaN. Missing group KEYS form their own
   * cell: null/undefined stringify to '' here, while R groups them under
   * NA (a labeling difference only — same rows, same numbers).
   *
   * Returns RAW numbers ({facet, group, color, value, n}) — presentation
   * rounding belongs to the consumers (tooltip / label formatters).
   * @param {any[]} rows
   * @param {{group?: string, color?: string, facet?: string,
   *          value?: string, func?: string}} cfg
   */
  function aggregate(rows, cfg) {
    const { group, color, facet, value, func } = cfg || {};
    if (!rows || rows.length === 0) return [];

    /** @type {Record<string, any>} */
    const groups = {};
    for (const row of rows) {
      const gv = group ? String(row[group] ?? '') : 'Total';
      const cv = color ? String(row[color] ?? '') : '__all__';
      const fv = facet ? String(row[facet] ?? '') : '__all__';
      const key = fv + '|||' + gv + '|||' + cv;
      if (!groups[key]) groups[key] = { facet: fv, group: gv, color: cv, values: [], rows: [] };
      groups[key].rows.push(row);
      if (value !== '.count' && value != null && row[value] != null) {
        const n = Number(row[value]);
        if (!Number.isNaN(n)) groups[key].values.push(n);
      }
    }

    const result = [];
    for (const g of Object.values(groups)) {
      // `out` (not `value`) — `value` is the config's aggregated column name,
      // read as r[value] in the count_distinct branch; a local `value` would
      // shadow it and silently count r[undefined] (every group → 0).
      let out;
      if (func === 'count') out = g.rows.length;
      else if (func === 'count_distinct') { const s = new Set(); for (const r of g.rows) { const v = value != null ? r[value] : null; if (v != null && !(typeof v === 'number' && Number.isNaN(v))) s.add(v); } out = s.size; }
      else if (func === 'mean') out = g.values.length ? g.values.reduce((/** @type {number} */ a, /** @type {number} */ b) => a + b, 0) / g.values.length : null;
      else if (func === 'median') { const s = g.values.slice().sort((/** @type {number} */ a, /** @type {number} */ b) => a - b); const m = Math.floor(s.length / 2); out = s.length ? (s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2) : null; }
      else if (func === 'sum') out = g.values.reduce((/** @type {number} */ a, /** @type {number} */ b) => a + b, 0);
      else if (func === 'min') out = g.values.length ? Math.min.apply(null, g.values) : null;
      else if (func === 'max') out = g.values.length ? Math.max.apply(null, g.values) : null;
      // n = rows behind this (group, color) cell, for the tooltip's
      // "n = ..." line (how many observations the mark aggregates).
      result.push({ facet: g.facet, group: g.group, color: g.color, value: out, n: g.rows.length });
    }
    return result;
  }

  const ns = /** @type {any} */ (
    (typeof Blockr !== 'undefined') ? Blockr
      : (window.Blockr = window.Blockr || /** @type {any} */ ({})));
  ns.DrilldownAgg = { AGG_FNS, AGG_WORDS, aggRoles, reconcileValue, aggregate };
  window.DrilldownAgg = ns.DrilldownAgg;
})();
