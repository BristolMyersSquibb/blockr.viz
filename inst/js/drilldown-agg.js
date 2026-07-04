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
 *     The `group` + `metric` + `agg_fn` role-spec triple the DrilldownConfig
 *     engine renders. Hosts spread it into their ROLES dict. `multiple` widens
 *     `group` to a multi-column picker (the table groups by several columns;
 *     the chart and tile group by one).
 *
 *   reconcileMetric(cfg, columns)
 *     Keep `metric` consistent with `agg_fn`: "count" ignores the metric (force
 *     the synthetic '.count'); "count_distinct" takes any column but not
 *     '.count'; the numeric aggregations need a numeric column. A metric that
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

  /** @param {{ multiple?: boolean }} [opts] */
  function aggRoles(opts) {
    const multiple = !!(opts && opts.multiple);
    return {
      group: {
        label: 'Group', kind: multiple ? 'columns' : 'column',
        colType: 'cat', ph: 'category column…',
        // The table's multi-group gates the metrics list, so a change re-renders
        // the gear (reveals/hides the aggregations). The chart's single group
        // gates nothing, so it does not.
        rerender: multiple
      },
      // The metric picker follows the aggregation (see reconcileMetric): row
      // count ignores the metric, count_distinct takes any column, the numeric
      // aggregations need a numeric column. `pairReversed` renders the row as a
      // verb-object phrase — "[agg] of [column]" — so the aggregation leads.
      metric: {
        label: 'Aggregate', kind: 'column', pairedWith: 'agg_fn',
        pairReversed: true,
        ph: 'column to aggregate…',
        colType: (/** @type {any} */ cfg) =>
          cfg.agg_fn === 'count_distinct' ? 'any'
            : (!cfg.agg_fn || cfg.agg_fn === 'count') ? 'none' : 'num',
        allowCount: (/** @type {any} */ cfg) =>
          !cfg.agg_fn || cfg.agg_fn === 'count'
      },
      agg_fn: { label: 'Agg', kind: 'select', options: AGG_FNS, rerender: true }
    };
  }

  /**
   * @param {any} cfg     the mutable config object (mutated in place)
   * @param {any[]} columns  column metadata [{name, type, ...}]
   */
  function reconcileMetric(cfg, columns) {
    if (!cfg.agg_fn || cfg.agg_fn === 'count') { cfg.metric = '.count'; return; }
    if (cfg.agg_fn === 'count_distinct') {
      if (cfg.metric === '.count') cfg.metric = '';
      return;
    }
    const col = (columns || []).find((/** @type {any} */ c) => c.name === cfg.metric);
    if (!col || col.type !== 'numeric') cfg.metric = '';
  }

  const ns = /** @type {any} */ (
    (typeof Blockr !== 'undefined') ? Blockr
      : (window.Blockr = window.Blockr || {}));
  ns.DrilldownAgg = { AGG_FNS, aggRoles, reconcileMetric };
  window.DrilldownAgg = ns.DrilldownAgg;
})();
