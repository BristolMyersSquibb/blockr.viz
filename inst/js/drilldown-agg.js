// @ts-check
/**
 * DrilldownAgg — the aggregation vocabulary of the table's gear:
 *
 *   AGG_FNS
 *     The aggregation-function select options.
 *
 *   AGG_WORDS
 *     One word per aggregation, for composed labels.
 *
 *   aggRoles({ multiple })
 *     The `group` + `value` + `func` role-spec triple the DrilldownConfig
 *     engine renders. Hosts spread it into their ROLES dict. `multiple` widens
 *     `group` to a multi-column picker.
 *
 * Exposed as Blockr.DrilldownAgg (and window.DrilldownAgg). Must load before
 * table.js.
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
  // tooltips).
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
        // Column kinds (mark_column_kinds()): a group is a stratifier, a
        // visit (one bar per AVISIT) or an item dimension (one bar per
        // preferred term) -- the item kinds a colour must never offer.
        kinds: ['group', 'time', 'id'],
        // The table's multi-group gates the summaries list, so a change
        // re-renders the gear (reveals/hides the aggregations).
        rerender: multiple
      },
      // The value picker follows the aggregation: row count ignores the
      // value, count_distinct takes any column, the numeric aggregations need
      // a numeric column. `pairReversed` renders the row as a verb-object
      // phrase — "[func] of [column]" — so the aggregation leads.
      value: {
        label: 'Value', kind: 'column', pairedWith: 'func',
        pairReversed: true,
        ph: 'column to aggregate…',
        colType: (/** @type {any} */ cfg) =>
          cfg.func === 'count_distinct' ? 'any'
            : (!cfg.func || cfg.func === 'count') ? 'none' : 'num',
        allowCount: (/** @type {any} */ cfg) =>
          !cfg.func || cfg.func === 'count',
        // Aggregating a measure. `count_distinct` counts the levels of an id
        // (patients, not records), so ids belong here too -- the colType
        // callback above already opens the picker to any column for it.
        kinds: ['value', 'id']
      },
      func: { label: 'Aggregate', kind: 'select', options: AGG_FNS, rerender: true }
    };
  }

  const ns = /** @type {any} */ (
    (typeof Blockr !== 'undefined') ? Blockr
      : (window.Blockr = window.Blockr || /** @type {any} */ ({})));
  ns.DrilldownAgg = { AGG_FNS, AGG_WORDS, aggRoles };
  window.DrilldownAgg = ns.DrilldownAgg;
})();
