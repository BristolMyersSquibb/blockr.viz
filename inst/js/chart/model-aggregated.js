// @ts-check
/**
 * Chart block: the aggregated model (bar, waterfall, pie, treemap, and the
 * distribution marks boxplot and point range).
 *
 * Rows and config in, what to draw out: the cells of the aggregation engine
 * (agg.js), the panels (one per facet level), each panel's groups in sort
 * order, the colour levels, the "(n)" counts, and for the distribution marks
 * the category slots with their summaries.
 *
 * A mark is addressed by (group, colour level) inside a panel; its keys are
 * the group, the colour where it splits the mark (bar segments, boxes, point
 * ranges), and the facet in a facet panel.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});
  const model = /** @type {any} */ (NS.model = NS.model || {});

  // Config fields the model reads; a change to any other field reuses it.
  const MODEL_FIELDS = ['type', 'category', 'color', 'facet', 'measure', 'func', 'missing',
    'percent_of', 'sort_by', 'sort_dir', 'facet_scales', 'derived.scales', 'show_n',
    'id', 'tooltip', 'summary', 'whiskers', 'box_points', 'label', 'highlight'];

  // A colour-split distribution slot is `group + SEP + level`: ECharts
  // dedupes a category axis by value, so the bare group would collapse the
  // boxes of one group into one slot.
  const BOX_CAT_SEP = '\u0000';

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

  /**
   * The order of a panel's groups on the category axis. A bridge always
   * keeps the data order (a value sort scrambles the running total). "data"
   * is factor levels, else time or numeric order for a date or number
   * column, else first appearance in the rows. "alpha" is factor levels,
   * else by name. "value" is the total across colour cells. Any other value
   * names a column whose smallest value per group orders it.
   * @param {any[]} cells the panel's aggregate cells @param {any[]} rows
   * @param {VizColumn[]} columns @param {Record<string, any>} cfg
   * @returns {string[]}
   */
  const orderGroups = (cells, rows, columns, cfg) => {
    const groups = [...new Set(cells.map((a) => a.group))];
    const sortBy = cfg.sort_by || 'alpha';
    const sortDir = cfg.sort_dir === 'desc' ? -1 : 1;
    const groupCol = cfg.category;
    const meta = NS.axes.colMeta(columns, groupCol);
    const levels = meta && Array.isArray(meta.levels) && meta.levels.length
      ? meta.levels.map(String) : null;
    // Every bin of a histogram, the empty ones too: a missing bin would
    // close the gap in the distribution it stands for. R's edges list them.
    if (cfg.type === 'histogram') {
      // R's edges, a data frame sent as columns.
      const edges = cfg.derived.hist && cfg.derived.hist.label;
      const bins = Array.isArray(edges) && edges.length ? edges.map(String) : levels;
      if (bins) return bins;
    }
    const levelAt = new Map((levels || []).map((l, i) => [l, i]));

    const dataOrder = () => {
      const present = new Set(groups);
      if (levels) {
        return levels.filter((l) => present.has(l)).concat(groups.filter((g) => !levelAt.has(g)));
      }
      // A date or a number orders by itself, a missing value last. Dates
      // travel as ISO strings, which sort as text.
      if (meta && (meta.temporal || meta.type === 'numeric')) {
        const num = !meta.temporal;
        const key = (/** @type {string} */ g) => (num ? Number(g) : g);
        const missing = (/** @type {string} */ g) => g === '' || (num && isNaN(Number(g)));
        const known = groups.filter((g) => !missing(g));
        known.sort((a, b) => (key(a) < key(b) ? -1 : key(a) > key(b) ? 1 : 0));
        return known.concat(groups.filter(missing));
      }
      const seen = new Set(rows.map((r) => (groupCol ? String(r[groupCol] ?? '') : 'Total')));
      return [...seen].filter((g) => present.has(g)).concat(groups.filter((g) => !seen.has(g)));
    };

    if (cfg.type === 'waterfall') return dataOrder();
    if (sortBy === 'data') {
      const ord = dataOrder();
      return sortDir < 0 ? ord.reverse() : ord;
    }
    if (sortBy === 'alpha') {
      const at = (/** @type {string} */ g) => levelAt.get(g) ?? 1e9;
      return groups.sort((a, b) => ((at(a) - at(b)) || a.localeCompare(b)) * sortDir);
    }
    if (sortBy === 'value') {
      /** @type {Record<string, number>} */
      const totals = {};
      for (const a of cells) totals[a.group] = (totals[a.group] || 0) + (a.value ?? 0);
      return groups.sort((a, b) => ((totals[a] || 0) - (totals[b] || 0)) * sortDir);
    }
    if (!groupCol) return groups.sort((a, b) => a.localeCompare(b) * sortDir);
    /** @type {Record<string, number>} */
    const mins = {};
    for (const r of rows) {
      const g = String(r[groupCol] ?? '');
      const v = Number(r[sortBy]);
      if (!isNaN(v) && (mins[g] == null || v < mins[g])) mins[g] = v;
    }
    return groups.sort((a, b) => {
      const av = mins[a], bv = mins[b];
      if (av == null && bv == null) return a.localeCompare(b) * sortDir;
      if (av == null) return 1;
      if (bv == null) return -1;
      return (av - bv) * sortDir;
    });
  };

  /**
   * The first row's tooltip-field values per group, per facet ('__all__'
   * for all rows).
   * @param {any[]} rows @param {Record<string, any>} cfg @param {string[]} ttFields
   * @returns {Map<string, Record<string, any[]>>}
   */
  const ttRepValues = (rows, cfg, ttFields) => {
    const groupCol = cfg.category, facetCol = cfg.facet;
    /** @type {Map<string, Record<string, any[]>>} */
    const byFacet = new Map([['__all__', {}]]);
    for (const r of rows) {
      const g = groupCol ? String(r[groupCol] ?? '') : 'Total';
      const recs = [byFacet.get('__all__')];
      if (facetCol) {
        const fv = String(r[facetCol] ?? '');
        if (!byFacet.has(fv)) byFacet.set(fv, {});
        recs.push(byFacet.get(fv));
      }
      for (const rec of recs) if (rec && !(g in rec)) rec[g] = ttFields.map((c) => r[c] ?? '');
    }
    return byFacet;
  };

  /**
   * `label` on a bar, slice or tile: per facet, group and colour level, the
   * one value the cell's rows carry; null when they disagree. Pie and
   * treemap draw one mark per group, so `byColor` false keys every row
   * '__all__'.
   * @param {any[]} rows @param {Record<string, any>} cfg @param {boolean} byColor
   * @returns {Map<string, string | null>} keyed facet|||group|||color
   */
  const markLabels = (rows, cfg, byColor) => {
    const col = cfg.label, groupCol = cfg.category, facetCol = cfg.facet;
    const colorCol = byColor ? cfg.color : null;
    /** @type {Map<string, string | null>} */
    const out = new Map();
    for (const r of rows) {
      const v = r[col];
      if (v == null || v === '' || v === 'NA') continue;
      const key = (facetCol ? String(r[facetCol] ?? '') : '__all__') + '|||' +
        (groupCol ? String(r[groupCol] ?? '') : 'Total') + '|||' +
        (colorCol ? String(r[colorCol] ?? '') : '__all__');
      const t = String(v);
      const cur = out.get(key);
      if (cur === undefined) out.set(key, t);
      else if (cur !== null && cur !== t) out.set(key, null);
    }
    return out;
  };

  /**
   * The category slots of a distribution panel and what each one draws: one
   * per group, or per (group, level) when colour splits the marks. An empty
   * (group, level) slot is dropped unless a fixed facet grid needs every
   * slot in every panel.
   * @param {{ rows: any[], ix: any, cfg: Record<string, any>, groups: string[],
   *           levels: string[], facet: string }} p
   */
  const distributionSlots = ({ rows, ix, cfg, groups, levels, facet }) => {
    const groupBy = cfg.category, colorCol = cfg.color, facetCol = cfg.facet, value = cfg.measure;
    const isBox = cfg.type === 'boxplot';
    const bodyStat = cfg.summary || (isBox ? 'median_q1_q3' : 'mean_se');
    const whiskerStat = cfg.whiskers || 'tukey';
    // Keyed 'nz', so the panel of a missing facet value holds its rows.
    const facetRows = (facetCol && facet !== '__all__')
      ? ix.get(rows, facetCol, 'nz', String(facet)) : rows;
    const split = levels.length > 0;
    const keepEmpty = !!facetCol && NS.facetScales(cfg) !== 'free';
    const outliersOn = isBox && cfg.box_points === 'outliers';
    const idCol = cfg.id;

    /** @param {any[]} rs */
    const valsOf = (rs) => rs.map((r) => Number(r[value])).filter((v) => !Number.isNaN(v))
      .sort((a, b) => a - b);
    /** @param {any[]} rs */
    const count = (rs) => {
      if (!idCol) return rs.length;
      const s = new Set();
      for (const r of rs) { const id = r[idCol]; if (id != null) s.add(id); }
      return s.size;
    };

    const slots = [];
    for (const g of groups) {
      for (const lv of (split ? levels : ['__all__'])) {
        const cell = lv === '__all__'
          ? ix.get(facetRows, groupBy, 'raw', g)
          : ix.get2(facetRows, groupBy, 'raw', colorCol, 'nz', g, lv);
        const rs = cell.filter((/** @type {any} */ r) => r[value] != null);
        if (split && !keepEmpty && rs.length === 0) continue;
        const vals = valsOf(rs);
        const body = summarizeStat(vals, bodyStat);
        /** @type {any} */
        const slot = { cat: split ? g + BOX_CAT_SEP + lv : g, group: g, level: lv,
                       n: vals.length, count: count(rs), body, whisker: null, outliers: [] };
        if (isBox && body) {
          slot.whisker = summarizeStat(vals, whiskerStat);
          if (outliersOn) {
            const wlo = slot.whisker.lo, whi = slot.whisker.hi;
            for (const r of rs) {
              const v = Number(r[value]);
              if (!Number.isNaN(v) && (v < wlo || v > whi)) slot.outliers.push(v);
            }
          }
        }
        slots.push(slot);
      }
    }
    return slots;
  };

  /**
   * @typedef {{ facet: string, label: string | null, groups: string[], cells: any[],
   *             axisCounts: Map<string, any> | null, ttRep: Record<string, any[]> | null,
   *             slots: any[] | null }} AggregatedPanel
   * @typedef {{ family: 'aggregated', empty: string | null, type: string,
   *             cumulative: boolean, colors: string[], colorScale: any,
   *             levels: string[], split: boolean, distribution: boolean,
   *             noValue: boolean, ttFields: string[],
   *             facet: string | null, nFacets: number, single: boolean,
   *             ix: any, panels: AggregatedPanel[], histogram: boolean,
   *             markLabels: Map<string, string | null> | null,
   *             hl: Record<string, string[]> | null,
   *             hlLit: ((facet: string, group: string, level: string | null) => boolean) | null,
   *             hlColors: Set<string> | null }} AggregatedModel
   */

  /**
   * @param {{ rows: any[], ix: any, columns: VizColumn[], cfg: Record<string, any> }} input
   * @returns {AggregatedModel}
   */
  const aggregated = ({ rows, ix, columns, cfg }) => {
    // A histogram is a bar over the bins R adds to the rows.
    const ct = cfg.type === 'histogram' ? 'bar' : cfg.type;
    const { category, color, facet, measure, func } = cfg;
    const distribution = NS.DISTRIBUTION_TYPES.includes(ct);
    /** @type {AggregatedModel} */
    const out = {
      family: 'aggregated', empty: null, type: ct, cumulative: cfg.type === 'waterfall',
      colors: [], colorScale: NS.scaleFor(cfg, color), levels: [], split: false,
      distribution, noValue: distribution && (!measure || measure === '.count'),
      ttFields: [], facet: facet || null, nFacets: 1, single: true,
      ix, panels: [], histogram: cfg.type === 'histogram', markLabels: null,
      hl: null, hlLit: null, hlColors: null
    };
    const agg = NS.agg.aggregate(rows, {
      group: category, color, facet, value: measure, func, na_group: cfg.missing,
      pct_of: cfg.percent_of === 'category' ? 'group' : cfg.percent_of
    });
    if (agg.length === 0) {
      out.empty = model.emptyState('No data to chart');
      return out;
    }

    const facets = [...new Set(agg.map((/** @type {any} */ a) => a.facet))].sort();
    out.nFacets = facets.length;
    out.single = facets.length === 1;
    out.colors = NS.axes.orderLevels(
      [...new Set(agg.map((/** @type {any} */ a) => a.color))].filter((c) => c !== '__all__'),
      out.colorScale, columns, color);
    if (distribution && color) {
      out.levels = NS.axes.orderLevels(
        ix.levels(rows, color, 'nz').filter((/** @type {string} */ l) => l !== ''),
        out.colorScale, columns, color);
    }
    out.split = out.levels.length > 0;

    // Fixed scales give every panel the same categories; pie and treemap
    // label their own slices.
    const sharedGroups = (NS.facetScales(cfg) !== 'free' && ct !== 'pie' && ct !== 'treemap')
      ? orderGroups(agg, rows, columns, cfg) : null;

    const isBar = (ct === 'bar' || ct === 'waterfall');
    out.ttFields = (isBar && !out.cumulative) ? model.tooltipFields(cfg) : [];
    // On-mark text: a bar (not a waterfall step or histogram bin), slice or tile.
    if (cfg.label && ((ct === 'bar' && !out.cumulative && !out.histogram) ||
                      ct === 'pie' || ct === 'treemap')) {
      out.markLabels = markLabels(rows, cfg, ct === 'bar');
    }
    const ttRep = out.ttFields.length ? ttRepValues(rows, cfg, out.ttFields) : null;
    const countAxis = isBar && NS.countOn(cfg, 'axis');
    const facetLabel = model.facetLabels(rows, cfg, out.nFacets);

    for (const fv of facets) {
      const cells = ix.get(agg, 'facet', 'raw', fv);
      const groups = sharedGroups || orderGroups(cells, rows, columns, cfg);
      const restrict = !!facet && fv !== '__all__';
      let axisCounts = null;
      if (countAxis) {
        axisCounts = NS.labelCounts(restrict ? ix.get(rows, facet, 'nz', fv) : rows, cfg, category);
      }
      out.panels.push({
        facet: fv,
        label: facetLabel(fv),
        groups, cells, axisCounts,
        ttRep: ttRep ? (ttRep.get(restrict ? String(fv) : '__all__') || {}) : null,
        slots: (distribution && !out.noValue)
          ? distributionSlots({ rows, ix, cfg, groups, levels: out.levels, facet: fv }) : null
      });
    }
    highlightMarks(out, rows, ix, cfg);
    return out;
  };

  /**
   * The highlight: whether a bar (or a segment of one) holds a highlighted
   * row, and with a colour split the colour levels that do, which alone get
   * palette colours. Bars only.
   * @param {AggregatedModel} out @param {any[]} rows @param {any} ix
   * @param {Record<string, any>} cfg
   */
  const highlightMarks = (out, rows, ix, cfg) => {
    const hl = NS.highlightOf(cfg);
    if (!hl || out.cumulative || out.histogram || out.type !== 'bar') return;
    /** @type {Map<string, boolean>} */
    const memo = new Map();
    /** @param {string} fv @param {string} g @param {string | null} lv @returns {boolean} */
    const hlLit = (fv, g, lv) => {
      const k = fv + '\u0000' + g + '\u0000' + (lv == null ? '\u0001' : lv);
      const seen = memo.get(k);
      if (seen !== undefined) return seen;
      const keys = aggregatedKeys(cfg, fv, g, lv);
      const lit = !!NS.keys.markLit(keys, () => rowsUnder(rows, ix, keys), hl);
      memo.set(k, lit);
      return lit;
    };
    out.hl = hl;
    out.hlLit = hlLit;
    if (out.colors.length) {
      /** @type {Set<string>} */
      const lit = new Set();
      for (const p of out.panels) {
        for (const a of p.cells) {
          if (a.value != null && hlLit(p.facet, a.group, a.color)) lit.add(a.color);
        }
      }
      out.hlColors = lit;
    }
  };
  aggregated.FIELDS = MODEL_FIELDS;

  /**
   * The aggregate cell of (group, colour) in a panel: the first, or
   * undefined. Without a colour, the group's first cell.
   * @param {AggregatedModel} m @param {AggregatedPanel} panel @param {string} g @param {string | null} [c]
   */
  const cellOf = (m, panel, g, c) => (c == null
    ? m.ix.get(panel.cells, 'group', 'raw', g)[0]
    : m.ix.get2(panel.cells, 'group', 'raw', 'color', 'raw', g, c)[0]);

  /** The cells of one group in a panel. @param {AggregatedModel} m
   *  @param {AggregatedPanel} panel @param {string} g @returns {any[]} */
  const cellsOf = (m, panel, g) => m.ix.get(panel.cells, 'group', 'raw', g);

  /** The `label` text of a bar, slice or tile, or null.
   *  @param {AggregatedModel} m @param {AggregatedPanel} panel @param {string} g
   *  @param {string | null} [c] @returns {string | null} */
  const labelOf = (m, panel, g, c) => (m.markLabels
    ? (m.markLabels.get(panel.facet + '|||' + g + '|||' + (c == null ? '__all__' : c)) ?? null)
    : null);

  /**
   * The keys of an aggregated mark: the group, the colour where colour
   * splits the mark, and the facet in a facet panel. A missing value is ''.
   * @param {Record<string, any>} cfg @param {any} facetVal
   * @param {string | null} group @param {string | null} level
   * @returns {Record<string, string>}
   */
  const aggregatedKeys = (cfg, facetVal, group, level) => {
    /** @type {Record<string, string>} */
    const keys = {};
    if (group != null) keys[cfg.category] = group;
    if (cfg.color && level != null) keys[cfg.color] = level;
    if (cfg.facet && facetVal != null && facetVal !== '__all__') keys[cfg.facet] = facetVal;
    return keys;
  };

  /**
   * The (group, level) a click landed on, or null when it has no mark. Bars
   * read the colour off the series, a split distribution off its slot.
   * @param {AggregatedModel} m @param {Record<string, any>} cfg @param {any} params
   * @returns {{ group: string | null, level: string | null } | null}
   */
  const aggregatedMarkAt = (m, cfg, params) => {
    if (!params || (params.componentType && params.componentType !== 'series')) return null;
    const g = params.name || (params.value && params.value[0]);
    if (!g) return null;
    if (m.distribution && m.split && typeof g === 'string') {
      const [grp, lv] = g.split(BOX_CAT_SEP);
      return { group: grp, level: lv == null ? null : lv };
    }
    const level = (m.type === 'bar' && !m.cumulative && m.colors.includes(params.seriesName))
      ? String(params.seriesName) : null;
    return { group: String(g), level };
  };

  /**
   * The rows under a mark: the rows whose columns hold the mark's keys.
   * @param {any[]} rows @param {any} ix @param {Record<string, any>} keys
   * @returns {any[]}
   */
  const rowsUnder = (rows, ix, keys) => {
    let cur = rows;
    for (const col of Object.keys(keys)) {
      cur = ix.get(cur, col, 'nz', String(keys[col]));
      if (!cur.length) break;
    }
    return cur;
  };

  /**
   * The filter a click on a mark sends, or null. Auto: the mark's keys. A
   * drill column: its values in the rows under the mark.
   * @param {Record<string, any>} cfg @param {Record<string, string>} keys
   * @param {() => any[]} rows
   * @returns {Record<string, any[]> | null}
   */
  const aggregatedClick = (cfg, keys, rows) => {
    const drill = NS.drillColumn(cfg);
    if (!drill) return null;
    if (cfg.drill !== 'auto') return NS.keys.fromRows(drill, rows());
    return NS.keys.fromKeys(keys);
  };

  Object.assign(model, { aggregated, summarizeStat, cellOf, cellsOf, labelOf, aggregatedKeys,
                         aggregatedMarkAt, aggregatedClick, rowsUnder, BOX_CAT_SEP });
})();
