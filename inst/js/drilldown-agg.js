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
      func: { label: 'Agg', kind: 'select', options: AGG_FNS, rerender: true }
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

  const ns = /** @type {any} */ (
    (typeof Blockr !== 'undefined') ? Blockr
      : (window.Blockr = window.Blockr || {}));
  ns.DrilldownAgg = { AGG_FNS, AGG_WORDS, aggRoles, reconcileValue };
  window.DrilldownAgg = ns.DrilldownAgg;
})();
