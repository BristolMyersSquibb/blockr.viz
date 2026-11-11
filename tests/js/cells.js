/* What R sends an aggregated chart in place of its rows: one row per mark
 * (chart_cells() in R/chart-cells.R). This is its twin for the tests, so a
 * case written as rows is drawn the way R feeds the chart.
 * tests/testthat/test-chart-cells.R compares the two.
 *
 * The numbers come from agg-reference.js, not from R.
 */
'use strict';

const { aggregate, summarizeStat } = require('./agg-reference');

const CELL_TYPES = ['bar', 'waterfall', 'pie', 'treemap', 'boxplot', 'pointrange', 'histogram'];
const DIST_TYPES = ['boxplot', 'pointrange'];
const FIXED_SORTS = ['value', 'data', 'alpha', 'onset'];
const SEP = '\u001f';

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

/** The cells of `data` for `config`, as columns: none ({}) without rows or
 * a category, null for a chart that draws rows. */
function chartCells(data, config, columns) {
  const cfg = config || {};
  const rows = rowsOf(data);
  const type = cfg.type || 'bar';
  if (!CELL_TYPES.includes(type)) return null;
  if (!rows.length) return {};
  const has = (c) => c != null && c !== '' && Object.prototype.hasOwnProperty.call(rows[0], c);
  const hist = type === 'histogram', dist = DIST_TYPES.includes(type);
  const category = hist ? (cfg.x ? cfg.x + '_bin' : null) : cfg.category;
  if (!has(category)) return {};
  // A split column counts when it holds a value in some row.
  const split = (c) => (has(c) && rows.some((r) => r[c] != null) &&
    !(columns || []).some((m) => m.name === c && m.all_na) ? c : null);
  const facets = [split(cfg.facet_row), split(cfg.facet)].filter(Boolean);
  const color = split(cfg.color);
  const keys = [...new Set([...facets, category, color].filter(Boolean))];
  const value = !hist && has(cfg.value) && (dist || (cfg.func || 'count') !== 'count')
    ? cfg.value : null;
  const func = !value ? 'count' : dist ? 'mean' : cfg.func;

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
      group: category, color: color || undefined, facet: facets.length ? '.f' : undefined,
      value, func, na_group: 'level', pct_of: of || undefined
    });
    const at = new Map(agg.map((a) => [a.facet + SEP + a.group + SEP + a.color, a.value]));
    out['.value'] = cells.map((rs) => at.get(
      (facets.length ? facetOf(rs[0]) : '__all__') + SEP + str(rs[0][category]) + SEP +
      (color ? str(rs[0][color]) : '__all__')) ?? null);
  }
  if (dist && value) {
    const box = type === 'boxplot';
    const summary = cfg.summary || (box ? 'median_q1_q3' : 'mean_se');
    const stats = cells.map((rs) => {
      const held = rs.filter((r) => r[value] != null);
      const raw = held.map((r) => Number(r[value])).filter((v) => !Number.isNaN(v));
      const vals = raw.slice().sort((a, b) => a - b);
      const body = summarizeStat(vals, summary);
      const whisker = box && body ? summarizeStat(vals, cfg.whiskers || 'tukey') : null;
      return { n: vals.length, body, whisker,
               ids: new Set(held.map((r) => r[cfg.id]).filter((v) => v != null)).size,
               out: whisker ? raw.filter((v) => v < whisker.lo || v > whisker.hi) : [] };
    });
    const at = (part, k) => stats.map((s) => (s[part] ? s[part][k] : null));
    out['.vn'] = stats.map((s) => s.n);
    if (has(cfg.id)) out['.count'] = stats.map((s) => s.ids);
    out['.lo'] = at('body', 'lo');
    out['.center'] = at('body', 'center');
    out['.hi'] = at('body', 'hi');
    if (box) {
      out['.wlo'] = at('whisker', 'lo');
      out['.whi'] = at('whisker', 'hi');
      if (cfg.box_points === 'outliers') out['.out'] = stats.map((s) => s.out);
    }
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

/**
 * The rows a model test hands the aggregated model: the cells of its rows
 * for the settings `c` (NS.model.settings()), with the grid key the view
 * adds. `C` is the chart namespace.
 */
function cellRows(C, rows, c, columns) {
  const g = c.facet_grid;
  const cfg = { ...c, facet: g ? g.col : c.facet, facet_row: g ? g.row : null };
  const cells = chartCells(rows, cfg, columns);
  const out = cells ? rowsOf(cells) : rows;
  if (g) for (const r of out) r[C.model.GRID_COL] = C.model.gridKey(r, g, C.model.gridKeepsMissing(c));
  return out;
}

module.exports.cellRows = cellRows;
