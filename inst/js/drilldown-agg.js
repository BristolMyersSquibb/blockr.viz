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
    // Both count the distinct values of any column, numeric or not, so the
    // value picker must not be narrowed to numerics for either.
    if (cfg.func === 'count_distinct' || cfg.func === 'pct_distinct') {
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
   * cell by default: null/undefined stringify to '' here, while R groups them
   * under NA (a labeling difference only — same rows, same numbers).
   *
   *   na_group        'level' (default, the behaviour above) or 'drop'.
   *                   'drop' removes rows whose cell has no complete address —
   *                   a missing value in ANY mapped role, group, facet or
   *                   color — so they draw no nameless bar, panel or series.
   *                   They are still counted in the population pct_distinct
   *                   divides by, because that is built before any of this.
   *                   The distinction is the whole point: a subject something
   *                   did not happen to is not a category, and is still a
   *                   subject.
   *                   (All three roles, not just group: R's twin has always
   *                   dropped on any of them, since the table passes facet,
   *                   group and color as one grouping. JS checked group alone,
   *                   so the engines disagreed whenever a FACET carried NAs.)
   *   pct_distinct    chart-only. distinct `value` in the cell divided by
   *                   distinct `value` in the surrounding population, as a
   *                   fraction (0..1, like bar_mode 'percent'). Absent from
   *                   AGG_FNS for the same reason as identity: no R twin, so
   *                   it sits out the golden cross-test (see
   *                   test-agg-pct-distinct.R).
   *   pct_of          which roles the denominator is taken WITHIN: any subset
   *                   of 'facet', 'group', 'color'. Default ['facet'].
   *                   A cell only knows the values of the roles it is mapped
   *                   on, so those three are the whole option space -- there
   *                   is nothing else a denominator could be grouped by.
   *
   *                   Why it cannot be inferred: the chart would have to know
   *                   whether a column is a POPULATION split (arm, sex,
   *                   country -- a per-level denominator is meaningful) or an
   *                   EVENT attribute (grade, seriousness -- dividing grade-2
   *                   subjects by subjects-with-grade-2 is circular). Nothing
   *                   in the data says which, so the user says.
   *
   * Returns RAW numbers ({facet, group, color, value, n}) — presentation
   * rounding belongs to the consumers (tooltip / label formatters).
   * @param {any[]} rows
   * @param {{group?: string, color?: string, facet?: string,
   *          value?: string, func?: string}} cfg
   */
  function aggregate(rows, cfg) {
    const { group, color, facet, value, func, na_group, pct_of } = cfg || {};
    if (!rows || rows.length === 0) return [];

    // A key is missing when it stringifies to '' — the same fold the golden
    // cross-test applies to R's NA, so both engines drop the same rows.
    const dropNa = na_group === 'drop';
    const cellCols = [group, facet, color].filter(Boolean);
    const incomplete = (/** @type {any} */ row) =>
      cellCols.some(c => String(row[c] ?? '') === '');
    const usable = (/** @type {any} */ v) =>
      v != null && !(typeof v === 'number' && Number.isNaN(v));

    // The roles the pct_distinct denominator is taken within. Placeholders for
    // an unmapped role MUST match the ones the cells use below ('Total' for
    // group, '__all__' for facet/color), or a cell's key never finds its
    // denominator and every percentage comes out null.
    const pctRoles = (Array.isArray(pct_of) ? pct_of
      : (pct_of ? [pct_of] : ['facet'])).map(String);
    const coords = (/** @type {any} */ row) => ({
      facet: facet ? String(row[facet] ?? '') : '__all__',
      group: group ? String(row[group] ?? '') : 'Total',
      color: color ? String(row[color] ?? '') : '__all__'
    });
    const denomKey = (/** @type {any} */ c) =>
      pctRoles.map(r => c[r] !== undefined ? c[r] : '__all__').join('|||');

    // Denominators, counted over EVERY row including the ones 'drop' removes
    // from the cells. Built before the cells, because that is the point: a
    // subject something did not happen to still belongs to the population.
    /** @type {Record<string, Set<any>>} */
    const denomPop = {};
    if (func === 'pct_distinct') {
      for (const row of rows) {
        const v = value != null ? row[value] : null;
        if (!usable(v)) continue;
        const k = denomKey(coords(row));
        (denomPop[k] || (denomPop[k] = new Set())).add(v);
      }
    }

    /** @type {Record<string, any>} */
    const groups = {};
    for (const row of rows) {
      if (dropNa && incomplete(row)) continue;
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
      else if (func === 'count_distinct') { const s = new Set(); for (const r of g.rows) { const v = value != null ? r[value] : null; if (usable(v)) s.add(v); } out = s.size; }
      // Share of the FACET's distinct values, as a fraction. A facet with no
      // usable value has no denominator, so the cell is null (a gap) rather
      // than a fabricated 0 — same rule as mean/min on an empty cell.
      else if (func === 'pct_distinct') {
        const s = new Set();
        for (const r of g.rows) { const v = value != null ? r[value] : null; if (usable(v)) s.add(v); }
        const pop = denomPop[denomKey(g)];
        const den = pop ? pop.size : 0;
        out = den ? s.size / den : null;
      }
      else if (func === 'mean') out = g.values.length ? g.values.reduce((/** @type {number} */ a, /** @type {number} */ b) => a + b, 0) / g.values.length : null;
      else if (func === 'median') { const s = g.values.slice().sort((/** @type {number} */ a, /** @type {number} */ b) => a - b); const m = Math.floor(s.length / 2); out = s.length ? (s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2) : null; }
      else if (func === 'sum') out = g.values.reduce((/** @type {number} */ a, /** @type {number} */ b) => a + b, 0);
      else if (func === 'min') out = g.values.length ? Math.min.apply(null, g.values) : null;
      else if (func === 'max') out = g.values.length ? Math.max.apply(null, g.values) : null;
      // identity: the value AS-IS — no aggregation. Returns the cell's first
      // usable numeric value; with one row per (group, color) cell (the
      // intended use: precomputed bar heights) that IS the row's value. A cell
      // with no usable value stays null (a gap, like mean/min). Chart-only:
      // offered solely in the chart's `func` picker (chart.js), never in the
      // shared AGG_FNS, so the table/tile and the drift/golden tests are
      // unaffected. Duplicate categories collapse to the first row (documented).
      else if (func === 'identity') out = g.values.length ? g.values[0] : null;
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
