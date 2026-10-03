// @ts-check
/**
 * Chart block: the aggregated model (bar, waterfall, pie, treemap,
 * radar, and the distribution marks boxplot and point range).
 *
 * Rows and config in, what to draw out: the cells of the shared aggregation
 * engine (drilldown-agg.js), the panels (one per facet level), the groups
 * of each panel in their sort order, the colour levels, the "(n)" counts,
 * and for the distribution marks the category slots with their summaries.
 *
 * A mark is addressed by (group, colour level) inside a panel; its keys are
 * the column values that define it (chart-keys.js): the group, the colour
 * where colour splits the mark (bar segments, boxes, point ranges), the
 * colour alone for a radar shape, and the facet in a facet panel.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});
  const model = /** @type {any} */ (NS.model = NS.model || {});

  // Config fields the model reads; a change to any other field reuses it.
  const MODEL_FIELDS = ['chart_type', 'group', 'color', 'facet', 'value', 'func', 'na_group',
    'pct_of', 'sort_by', 'sort_dir', 'baseline', 'facet_scales', 'scales', 'count_on',
    'count_col', 'tt_fields', 'summary', 'whiskers', 'box_points'];

  // A colour-split distribution slot is `group + SEP + level`: ECharts
  // dedupes a category axis by value, so the bare group would collapse the
  // boxes of one group into one slot.
  const BOX_CAT_SEP = '\u0000';

  const DAgg = () => /** @type {any} */ (B.DrilldownAgg || root.DrilldownAgg);

  /**
   * {center, lo, hi} of a statistic over ascending values, or null for none.
   * One implementation for the box body, its whiskers and the point range,
   * so they cannot disagree. sd is the sample sd, 0 for one value.
   * 'p10_p90' draws the 10th to 90th percentile around the median (B8).
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

  /** Is this a bar drawn as a running bridge? @param {Record<string, any>} cfg */
  const isCumulative = (cfg) => cfg.chart_type === 'waterfall' ||
    (cfg.chart_type === 'bar' && cfg.baseline === 'cumulative');

  /**
   * The order of a panel's groups on the category axis. A bridge keeps the
   * data order always (a value sort scrambles the running total); "data" is
   * factor levels, else first appearance in the rows; "alpha" is factor
   * levels, else by name; "value" is the total across colour cells; any
   * other value names a column whose smallest value per group orders it.
   * @param {any[]} cells the panel's aggregate cells @param {any[]} rows
   * @param {VizColumn[]} columns @param {Record<string, any>} cfg
   * @returns {string[]}
   */
  const orderGroups = (cells, rows, columns, cfg) => {
    const groups = [...new Set(cells.map((a) => a.group))];
    const sortBy = cfg.sort_by || 'alpha';
    const sortDir = cfg.sort_dir === 'desc' ? -1 : 1;
    const groupCol = cfg.group;
    const meta = NS.axes.colMeta(columns, groupCol);
    const levels = meta && Array.isArray(meta.levels) && meta.levels.length ? meta.levels : null;

    const dataOrder = () => {
      if (levels) {
        const idx = new Map(levels.map((/** @type {any} */ l, /** @type {number} */ i) =>
          [String(l), i]));
        const present = new Set(groups);
        return levels.map(String).filter((/** @type {string} */ l) => present.has(l))
          .concat(groups.filter((g) => !idx.has(g)));
      }
      /** @type {string[]} */
      const seen = [];
      const seenSet = new Set();
      for (const r of rows) {
        const g = groupCol ? String(r[groupCol] ?? '') : 'Total';
        if (!seenSet.has(g)) { seenSet.add(g); seen.push(g); }
      }
      const present = new Set(groups);
      return seen.filter((g) => present.has(g)).concat(groups.filter((g) => !seenSet.has(g)));
    };

    if (isCumulative(cfg)) return dataOrder();
    if (sortBy === 'data') {
      const ord = dataOrder();
      return sortDir < 0 ? ord.reverse() : ord;
    }
    if (sortBy === 'alpha') {
      if (levels) {
        /** @type {Map<string, number>} */
        const idx = new Map(levels.map((/** @type {any} */ l, /** @type {number} */ i) =>
          /** @type {[string, number]} */ ([String(l), i])));
        const at = (/** @type {string} */ g) => (idx.has(g) ? /** @type {number} */ (idx.get(g)) : 1e9);
        return groups.sort((a, b) => ((at(a) - at(b)) || a.localeCompare(b)) * sortDir);
      }
      return groups.sort((a, b) => a.localeCompare(b) * sortDir);
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

  /** Tooltip fields of a bar, in order. @param {Record<string, any>} cfg @returns {string[]} */
  const barTtFields = (cfg) => [].concat(cfg.tt_fields || [])
    .map(String).filter((c) => c && c !== 'null' && c !== '(none)');

  /**
   * The first row's tooltip-field values per group, per facet ('__all__'
   * for the whole frame), in one pass.
   * @param {any[]} rows @param {Record<string, any>} cfg @param {string[]} ttFields
   * @returns {Map<string, Record<string, any[]>>}
   */
  const ttRepValues = (rows, cfg, ttFields) => {
    const groupCol = cfg.group, facetCol = cfg.facet;
    /** @type {Map<string, Record<string, any[]>>} */
    const byFacet = new Map();
    /** @type {Record<string, any[]>} */
    const all = {};
    byFacet.set('__all__', all);
    for (const r of rows) {
      const g = groupCol ? String(r[groupCol] ?? '') : 'Total';
      if (!(g in all)) all[g] = ttFields.map((c) => r[c] ?? '');
      if (facetCol) {
        const fv = String(r[facetCol] ?? '');
        let rec = byFacet.get(fv);
        if (!rec) { rec = {}; byFacet.set(fv, rec); }
        if (!(g in rec)) rec[g] = ttFields.map((c) => r[c] ?? '');
      }
    }
    return byFacet;
  };

  /**
   * The category slots of a distribution panel and what each one draws.
   * One slot per group, or per (group, level) when colour splits the
   * marks, group-major. An empty (group, level) slot is dropped unless a
   * fixed facet grid needs every slot in every panel.
   * @param {{ rows: any[], ix: any, cfg: Record<string, any>, groups: string[],
   *           levels: string[], facet: string }} p
   */
  const distributionSlots = ({ rows, ix, cfg, groups, levels, facet }) => {
    const groupBy = cfg.group, colorCol = cfg.color, facetCol = cfg.facet, value = cfg.value;
    const isBox = cfg.chart_type === 'boxplot';
    const bodyStat = cfg.summary || (isBox ? 'median_q1_q3' : 'mean_se');
    const whiskerStat = cfg.whiskers || 'tukey';
    // A facet's rows by the key every family uses, so a panel for a
    // missing facet value holds its rows (B10).
    const facetRows = (facetCol && facet !== '__all__')
      ? ix.get(rows, facetCol, 'nz', String(facet)) : rows;
    const split = levels.length > 0;
    const keepEmpty = !!facetCol && NS.facetScales(cfg) !== 'free';
    const outliersOn = isBox && cfg.box_points === 'outliers';
    const idCol = cfg.count_col;

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
   *             noValue: boolean, sharedMax: number | null, ttFields: string[],
   *             facet: string | null, nFacets: number, single: boolean,
   *             ix: any, panels: AggregatedPanel[] }} AggregatedModel
   */

  /**
   * @param {{ rows: any[], ix: any, columns: VizColumn[], cfg: Record<string, any> }} input
   * @returns {AggregatedModel}
   */
  const aggregated = ({ rows, ix, columns, cfg }) => {
    const ct = cfg.chart_type;
    const { group, color, facet, value, func, na_group, pct_of } = cfg;
    const distribution = NS.DISTRIBUTION_TYPES.includes(ct);
    /** @type {AggregatedModel} */
    const out = {
      family: 'aggregated', empty: null, type: ct, cumulative: isCumulative(cfg),
      colors: [], colorScale: NS.scaleFor(cfg, color), levels: [], split: false,
      distribution, noValue: distribution && (!value || value === '.count'),
      sharedMax: null, ttFields: [], facet: facet || null, nFacets: 1, single: true,
      ix, panels: []
    };
    const agg = DAgg().aggregate(rows, { group, color, facet, value, func, na_group, pct_of });
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

    // One category set in every panel under fixed scales, so a slot is the
    // same category in each; pie and treemap label their own slices.
    const sharedGroups = (NS.facetScales(cfg) !== 'free' && ct !== 'pie' && ct !== 'treemap')
      ? orderGroups(agg, rows, columns, cfg) : null;
    // A radar's spokes share one max; fixed scales widen it to the grid.
    out.sharedMax = (sharedGroups && ct === 'radar')
      ? (Math.max(NS.maxOf(agg.map((/** @type {any} */ a) => a.value ?? 0)), 0) || 1) : null;

    const isBar = (ct === 'bar' || ct === 'waterfall');
    out.ttFields = (isBar && !out.cumulative) ? barTtFields(cfg) : [];
    const ttRep = out.ttFields.length ? ttRepValues(rows, cfg, out.ttFields) : null;
    const countAxis = isBar && NS.countOn(cfg, 'axis');
    const labels = model.facetLabels(rows, cfg, facets);

    for (const fv of facets) {
      const cells = ix.get(agg, 'facet', 'raw', fv);
      const groups = sharedGroups || orderGroups(cells, rows, columns, cfg);
      const restrict = !!facet && fv !== '__all__';
      let axisCounts = null;
      if (countAxis) {
        axisCounts = NS.labelCounts(restrict ? ix.get(rows, facet, 'nz', fv) : rows, cfg, group);
      }
      out.panels.push({
        facet: fv,
        label: (!out.single && fv !== '__all__') ? labels.get(fv) : null,
        groups, cells, axisCounts,
        ttRep: ttRep ? (ttRep.get(restrict ? String(fv) : '__all__') || {}) : null,
        slots: (distribution && !out.noValue)
          ? distributionSlots({ rows, ix, cfg, groups, levels: out.levels, facet: fv }) : null
      });
    }
    return out;
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

  // -- Marks, keys and clicks ---------------------------------------------------

  /**
   * Does colour split this chart's marks into marks of their own?
   * @param {AggregatedModel} m
   */
  const colourSplitsMark = (m) => (m.type === 'bar' && !m.cumulative && m.colors.length > 0) ||
    (m.distribution && m.split);

  /**
   * The keys of an aggregated mark: the group, the colour where colour
   * splits the mark, the colour alone for a radar shape, and the facet in a
   * facet panel. Values are as the aggregation keys them ('' for missing).
   * @param {Record<string, any>} cfg @param {any} facetVal
   * @param {string | null} group @param {string | null} level
   * @returns {Record<string, string>}
   */
  const aggregatedKeys = (cfg, facetVal, group, level) => {
    /** @type {Record<string, string>} */
    const keys = {};
    if (cfg.chart_type === 'radar') {
      if (cfg.color && level != null) keys[cfg.color] = level;
    } else {
      if (group != null) keys[cfg.group] = group;
      if (cfg.color && level != null) keys[cfg.color] = level;
    }
    if (cfg.facet && facetVal != null && facetVal !== '__all__') keys[cfg.facet] = facetVal;
    return keys;
  };

  /**
   * The (group, level) a click landed on, or null when the click has no
   * mark (a radar without colour draws one shape of every row; a datum
   * without a name). Bars read the colour off the series, a split
   * distribution off its slot, a radar shape is its level.
   * @param {AggregatedModel} m @param {Record<string, any>} cfg @param {any} params
   * @returns {{ group: string | null, level: string | null } | null}
   */
  const aggregatedMarkAt = (m, cfg, params) => {
    if (!params || (params.componentType && params.componentType !== 'series')) return null;
    if (cfg.chart_type === 'radar') {
      if (!cfg.color) return null;
      const lv = params.name;
      return (lv == null || lv === '') ? null : { group: null, level: String(lv) };
    }
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
   * The filter a click on a mark sends, or null when it sends nothing.
   * Auto: the mark's keys, a missing value sent as null (a missing value is
   * a key too). An explicit drill column: that column's values in the rows
   * under the mark.
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

  Object.assign(model, { aggregated, summarizeStat, orderGroups, isCumulative, cellOf, cellsOf,
                         colourSplitsMark, aggregatedKeys, aggregatedMarkAt, aggregatedClick,
                         rowsUnder, BOX_CAT_SEP });
})();
