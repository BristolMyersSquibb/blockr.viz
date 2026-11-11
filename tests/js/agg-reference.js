/* The aggregation an aggregated chart draws, written in JavaScript: the
 * reference the tests hold R's against.
 *
 * R computes what the chart draws (chart_cells() in R/chart-cells.R, with
 * table_aggregate() and chart_dist_stats()). Until 0.3.0 the browser did,
 * with this code. It stays here as an independent second implementation:
 * tests/js/cells.js builds a chart's cells with it, so the snapshot tests
 * draw from numbers R did not make, and tests/testthat/test-chart-cells.R,
 * test-agg-golden.R and test-agg-identity.R compare R with it.
 */
'use strict';

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

const T975 = [12.7062, 4.3027, 3.1824, 2.7764, 2.5706, 2.4469, 2.3646, 2.3060, 2.2622,
  2.2281, 2.2010, 2.1788, 2.1604, 2.1448, 2.1314, 2.1199, 2.1098, 2.1009, 2.0930,
  2.0860, 2.0796, 2.0739, 2.0687, 2.0639, 2.0595, 2.0555, 2.0518, 2.0484, 2.0452, 2.0423];
/** The 97.5% quantile of Student's t, as R's qt(0.975, df): a table to 30
 * degrees of freedom, the Cornish-Fisher series past it.
 * @param {number} df */
const t975 = (df) => {
  if (df <= T975.length) return T975[Math.max(1, Math.round(df)) - 1];
  const z = 1.959964;
  const z3 = z * z * z, z5 = z3 * z * z, z7 = z5 * z * z;
  return z + (z3 + z) / (4 * df) + (5 * z5 + 16 * z3 + 3 * z) / (96 * df * df) +
    (3 * z7 + 19 * z5 + 17 * z3 - 15 * z) / (384 * df * df * df);
};

/**
 * {center, lo, hi} of a statistic over ascending values, or null for none.
 * One implementation for the box body, its whiskers and the point range.
 * sd is the sample sd, 0 for one value.
 * @param {number[]} vals @param {string} stat
 * @returns {{center: number, lo: number, hi: number} | null}
 */
const summarizeStat = (vals, stat) => {
  const n = vals.length;
  if (n === 0) return null;
  const q = (/** @type {number} */ p) => {
    const i = p * (n - 1); const lo = Math.floor(i);
    return lo === i ? vals[lo] : vals[lo] + (vals[lo + 1] - vals[lo]) * (i - lo);
  };
  const mean = vals.reduce((a, b) => a + b, 0) / n;
  const sd = n > 1
    ? Math.sqrt(vals.reduce((a, b) => a + (b - mean) * (b - mean), 0) / (n - 1))
    : 0;
  const se = sd / Math.sqrt(n);
  switch (stat) {
    case 'mean_sd': return { center: mean, lo: mean - sd, hi: mean + sd };
    case 'mean_2sd': return { center: mean, lo: mean - 2 * sd, hi: mean + 2 * sd };
    case 'mean_se': return { center: mean, lo: mean - se, hi: mean + se };
    case 'mean_ci95': {
      const h = n > 1 ? t975(n - 1) * se : 0;
      return { center: mean, lo: mean - h, hi: mean + h };
    }
    case 'p5_p95': return { center: q(0.5), lo: q(0.05), hi: q(0.95) };
    case 'p10_p90': return { center: q(0.5), lo: q(0.1), hi: q(0.9) };
    case 'min_max': return { center: q(0.5), lo: vals[0], hi: vals[n - 1] };
    case 'tukey': {
      // Tukey fences clipped to the data.
      const q1 = q(0.25), q3 = q(0.75), iqr = q3 - q1;
      return { center: q(0.5), lo: Math.max(vals[0], q1 - 1.5 * iqr),
               hi: Math.min(vals[n - 1], q3 + 1.5 * iqr) };
    }
    case 'median_q1_q3':
    default: return { center: q(0.5), lo: q(0.25), hi: q(0.75) };
  }
};

module.exports = { aggregate, summarizeStat };
