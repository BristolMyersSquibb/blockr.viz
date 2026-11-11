/* What R sends a bar, waterfall, pie or treemap in place of its rows: one
 * row per mark (chart_cells() in R/chart-cells.R). This is its twin for the
 * tests, so a case written as rows is drawn the way R feeds the chart.
 * tests/testthat/test-chart-cells.R compares the two on the fixture.
 *
 * The numbers are the engine's in inst/js/chart/agg.js, which the golden
 * test (test-agg-golden.R) ties to R's.
 */
'use strict';

const { loadPure } = require('./chart/load');

const CELL_TYPES = ['bar', 'waterfall', 'pie', 'treemap'];
const FIXED_SORTS = ['value', 'data', 'alpha', 'onset'];
const SEP = '\u001f';

let pure = null;
const aggregate = (rows, cfg) => (pure || (pure = loadPure())).agg.aggregate(rows, cfg);

/** Rows from a payload's data: a JSON string, a column object or rows. */
function rowsOf(data) {
  if (typeof data === 'string') data = JSON.parse(data);
  if (!data) return [];
  if (Array.isArray(data)) return data;
  const cols = {};
  for (const k of Object.keys(data)) {
    const c = data[k];
    cols[k] = c && !Array.isArray(c) && c.__enc__ === 'dict'
      ? c.codes.map((i) => (i == null ? null : c.levels[i])) : c;
  }
  const keys = Object.keys(cols);
  const n = keys.length ? cols[keys[0]].length : 0;
  return Array.from({ length: n }, (_, i) => Object.fromEntries(keys.map((k) => [k, cols[k][i]])));
}

const str = (v) => String(v ?? '');
const list = (x) => (x == null ? [] : [].concat(x)).map(String).filter((c) => c && c !== 'null');

/** The cells of `data` for `config`, as columns; null for a chart that
 * draws rows or has no category. */
function chartCells(data, config, columns) {
  const cfg = config || {};
  const rows = rowsOf(data);
  if (!CELL_TYPES.includes(cfg.type || 'bar') || !rows.length) return null;
  const has = (c) => c != null && c !== '' && Object.prototype.hasOwnProperty.call(rows[0], c);
  if (!has(cfg.category)) return null;
  // A split column counts when it holds a value in some row.
  const split = (c) => (has(c) && rows.some((r) => r[c] != null) &&
    !(columns || []).some((m) => m.name === c && m.all_na) ? c : null);
  const facets = [split(cfg.facet_row), split(cfg.facet)].filter(Boolean);
  const color = split(cfg.color);
  const keys = [...new Set([...facets, cfg.category, color].filter(Boolean))];
  const value = has(cfg.value) ? cfg.value : null;
  const func = value ? (cfg.func || 'count') : 'count';

  const facetOf = (r) => facets.map((c) => str(r[c])).join(SEP);
  const cellKey = (r) => keys.map((c) => str(r[c])).join(SEP + SEP);
  /** @type {Map<string, any[]>} */
  const byCell = new Map();
  for (const r of rows) {
    const k = cellKey(r);
    if (!byCell.has(k)) byCell.set(k, []);
    byCell.get(k).push(r);
  }
  const cells = [...byCell.values()];

  const out = {};
  for (const c of keys) out[c] = cells.map((rs) => rs[0][c] ?? null);
  out['.n'] = cells.map((rs) => rs.length);
  if (func !== 'count') {
    const of = cfg.percent_of === 'category' ? 'group' : cfg.percent_of;
    const tagged = rows.map((r) => ({ ...r, '.f': facetOf(r) }));
    const agg = aggregate(tagged, {
      group: cfg.category, color: color || undefined, facet: facets.length ? '.f' : undefined,
      value, func, na_group: 'level', pct_of: of || undefined
    });
    const at = new Map(agg.map((a) => [a.facet + SEP + a.group + SEP + a.color, a.value]));
    out['.value'] = cells.map((rs) => at.get(
      (facets.length ? facetOf(rs[0]) : '__all__') + SEP + str(rs[0][cfg.category]) + SEP +
      (color ? str(rs[0][color]) : '__all__')) ?? null);
  }
  if (cfg.missing === 'drop') {
    const drop = cells.map((rs) => keys.some((c) => str(rs[0][c]) === ''));
    if (drop.some(Boolean)) out['.drop'] = drop;
  }
  if (has(cfg.sort_by) && !FIXED_SORTS.includes(cfg.sort_by)) {
    out['.sort'] = cells.map((rs) => {
      const vs = rs.map((r) => Number(r[cfg.sort_by])).filter((v) => !Number.isNaN(v));
      return vs.length ? Math.min(...vs) : null;
    });
  }
  for (const f of list(cfg.tooltip).filter(has)) {
    out['.tt.' + f] = cells.map((rs) => rs[0][f] ?? null);
  }
  const distinct = (rs, c, keep) => [...new Set(rs.map((r) => r[c]).filter(keep))];
  if (has(cfg.label)) {
    out['.label'] = cells.map((rs) => distinct(rs, cfg.label,
      (v) => v != null && v !== '' && v !== 'NA').map(String).slice(0, 2));
  }
  const hl = cfg.highlight && typeof cfg.highlight === 'object' ? Object.keys(cfg.highlight)[0] : null;
  if (has(hl) && !keys.includes(hl)) {
    const vals = new Set([].concat(cfg.highlight[hl])
      .map((x) => (x && typeof x === 'object' ? x.value : x))
      .filter((x) => x != null && x !== '').map(String));
    if (vals.size) out['.hl'] = cells.map((rs) => rs.some((r) => vals.has(str(r[hl]))));
  }
  const counted = ['axis', 'facet', 'both'].includes(cfg.show_n);
  const sets = [counted ? cfg.id : null, cfg.drill !== 'auto' ? cfg.drill : null]
    .filter((c) => has(c) && !keys.includes(c));
  for (const c of new Set(sets)) out[c] = cells.map((rs) => distinct(rs, c, (v) => v != null));
  return out;
}

module.exports = { chartCells, rowsOf };
