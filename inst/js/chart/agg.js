// @ts-check
/**
 * Chart block: the aggregation engine and the names of the statistics.
 *
 * aggregate() groups rows by facet, category and colour and aggregates the
 * value column per cell. It is the browser's twin of the table model's
 * aggregation in R; tests/testthat/test-agg-golden.R runs this file in node
 * and compares the numbers. Pure: no DOM, no Shiny.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** One word per aggregation, for composed labels ("Mean AGE").
   * @type {Record<string, string>} */
  const AGG_WORDS = {
    count: 'Count', count_distinct: 'Distinct', mean: 'Mean',
    median: 'Median', sum: 'Sum', min: 'Min', max: 'Max'
  };

  // The words of a `summary` / `whiskers` statistic in a tooltip. An unknown
  // statistic reads as Tukey's whiskers.
  /** @type {Array<{value: string, center: string, range: string}>} */
  const STAT_META = [
    { value: 'tukey', center: '', range: '1.5×IQR' },
    { value: 'median_q1_q3', center: 'Median', range: 'Q1–Q3' },
    { value: 'mean_sd', center: 'Mean', range: '±1 SD' },
    { value: 'mean_2sd', center: 'Mean', range: '±2 SD' },
    { value: 'mean_se', center: 'Mean', range: '±1 SE' },
    { value: 'mean_ci95', center: 'Mean', range: '95% CI' },
    { value: 'p5_p95', center: 'Median', range: 'P5–P95' },
    { value: 'p10_p90', center: 'Median', range: 'P10–P90' },
    { value: 'min_max', center: 'Median', range: 'Min–Max' }
  ];
  /** @param {string} stat */
  const statMeta = (stat) => STAT_META.find((s) => s.value === stat) || STAT_META[0];

  // Math.max (dir 1) or Math.min (dir -1) of a non-empty array without NaN,
  // as a loop because Math.max.apply throws past about 100k arguments. A tie
  // between 0 and -0 resolves the way Math.max / Math.min resolve it.
  /** @param {number[]} vals @param {number} dir */
  function extreme(vals, dir) {
    let m = vals[0];
    for (let i = 1; i < vals.length; i++) {
      const v = vals[i];
      if (dir > 0 ? (v > m || (v === 0 && m === 0 && Object.is(m, -0)))
                  : (v < m || (v === 0 && m === 0 && Object.is(v, -0)))) m = v;
    }
    return m;
  }

  /** A value an aggregation can use: not null, not NaN. @param {any} v */
  const usable = (v) => v != null && !(typeof v === 'number' && Number.isNaN(v));

  /**
   * Group and aggregate rows into {facet, group, color, value, n} cells (raw
   * numbers; rounding belongs to the formatters). Aligned to R's
   * dd_table_aggregate in R/table-block.R, which the golden test guards.
   *
   *   count           rows in the cell.
   *   count_distinct  distinct usable values of `value`.
   *   sum             sum of the numeric values; 0 for an empty cell.
   *   mean, median,   over the numeric values; null (a gap, R's NA) when the
   *   min, max        cell has none. A value that is not a number is skipped.
   *   identity        the cell's first numeric value, for precomputed values.
   *   pct_distinct    distinct values in the cell over distinct values in
   *                   the population, 0..1. Chart-only, no R twin.
   *
   * A missing group, facet or colour key is its own cell (''), unless
   * `na_group` is 'drop': then a row missing any of the three is left out
   * of the cells but still counted in the pct_distinct population.
   *
   * `pct_of` names the roles ('facet', 'group', 'color'; default ['facet'])
   * the pct_distinct denominator is taken within. A name that is no role
   * puts every row in one population.
   * @param {any[]} rows
   * @param {{group?: string, color?: string, facet?: string,
   *          value?: string, func?: string, na_group?: string,
   *          pct_of?: string | string[]}} cfg
   */
  function aggregate(rows, cfg) {
    const { group, color, facet, value, func, na_group, pct_of } = cfg || {};
    if (!rows || rows.length === 0) return [];

    const cellCols = /** @type {string[]} */ ([group, facet, color].filter(Boolean));
    const incomplete = (/** @type {any} */ row) =>
      cellCols.some((c) => String(row[c] ?? '') === '');
    // The placeholder of an unmapped role has to be the same in the cells and
    // in the denominators, or a cell never finds its denominator.
    const coords = (/** @type {any} */ row) => ({
      facet: facet ? String(row[facet] ?? '') : '__all__',
      group: group ? String(row[group] ?? '') : 'Total',
      color: color ? String(row[color] ?? '') : '__all__'
    });
    const pctRoles = (Array.isArray(pct_of) ? pct_of
      : (pct_of ? [pct_of] : ['facet'])).map(String);
    const denomKey = (/** @type {any} */ c) =>
      pctRoles.map((r) => (c[r] !== undefined ? c[r] : '__all__')).join('|||');
    /** @param {any[]} rs */
    const distinct = (rs) => {
      const s = new Set();
      for (const r of rs) { const v = value != null ? r[value] : null; if (usable(v)) s.add(v); }
      return s;
    };

    // Counted over every row, the dropped ones too.
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
      if (na_group === 'drop' && incomplete(row)) continue;
      const c = coords(row);
      const key = c.facet + '|||' + c.group + '|||' + c.color;
      if (!groups[key]) groups[key] = { ...c, values: [], rows: [] };
      groups[key].rows.push(row);
      if (value !== '.count' && value != null && row[value] != null) {
        const n = Number(row[value]);
        if (!Number.isNaN(n)) groups[key].values.push(n);
      }
    }

    const sum = (/** @type {number[]} */ vs) => vs.reduce((a, b) => a + b, 0);
    const result = [];
    for (const g of Object.values(groups)) {
      const vs = g.values;
      let out;
      if (func === 'count') out = g.rows.length;
      else if (func === 'count_distinct') out = distinct(g.rows).size;
      else if (func === 'pct_distinct') {
        const pop = denomPop[denomKey(g)];
        out = pop && pop.size ? distinct(g.rows).size / pop.size : null;
      }
      else if (func === 'mean') out = vs.length ? sum(vs) / vs.length : null;
      else if (func === 'median') {
        const s = vs.slice().sort((/** @type {number} */ a, /** @type {number} */ b) => a - b);
        const m = Math.floor(s.length / 2);
        out = s.length ? (s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2) : null;
      }
      else if (func === 'sum') out = sum(vs);
      else if (func === 'min') out = vs.length ? extreme(vs, -1) : null;
      else if (func === 'max') out = vs.length ? extreme(vs, 1) : null;
      else if (func === 'identity') out = vs.length ? vs[0] : null;
      result.push({ facet: g.facet, group: g.group, color: g.color, value: out, n: g.rows.length });
    }
    return result;
  }

  NS.agg = { AGG_WORDS, statMeta, aggregate };
})();
