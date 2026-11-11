// @ts-check
/**
 * Chart block: the aggregated model (bar, waterfall, pie, treemap, and the
 * distribution marks boxplot and point range).
 *
 * Rows and config in, what to draw out: the cells, the panels (one per facet
 * level), each panel's groups in sort order, the colour levels, the "(n)"
 * counts, and for the distribution marks the category slots with their
 * summaries.
 *
 * The rows are R's cells, one per mark (R/chart-cells.R): the keys under
 * their column names, the rows each stands for (`.n`), its aggregate
 * (`.value`) and what else its mark draws. Nothing is aggregated here.
 *
 * A mark is addressed by (group, colour level) inside a panel; its keys are
 * the group, the colour where it splits the mark (bar segments, boxes, point
 * ranges), and the facet in a facet panel.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const model = /** @type {any} */ (NS.model = NS.model || {});

  // A colour-split distribution slot is `group + SEP + level`: ECharts
  // dedupes a category axis by value, so the bare group would collapse the
  // boxes of one group into one slot.
  const BOX_CAT_SEP = '\u0000';

  // Whether each panel picks its own categories. In a grid only "free" does:
  // "free_y" frees the value axis per row and keeps the categories shared.
  /** @param {any} cfg @returns {boolean} */
  const freeCategories = (cfg) => (cfg.facet_grid ? cfg.facet_scales === 'free'
    : NS.facetScales(cfg) === 'free');

  /** In a grid, each column's categories over the panels of that column.
   * @param {any[]} agg @param {any[]} rows @param {VizColumn[]} columns
   * @param {any} cfg @param {any} derived @returns {Map<string, string[]>} */
  const columnGroups = (agg, rows, columns, cfg, derived) => {
    const colOf = (/** @type {any} */ key) => (key == null ? null : model.gridLevels(key).col);
    /** @type {Map<string, string[]>} */ const out = new Map();
    for (const c of new Set(agg.map((/** @type {any} */ a) => colOf(a.facet)))) {
      const cells = agg.filter((/** @type {any} */ a) => colOf(a.facet) === c);
      const crows = rows.filter((r) => colOf(r[model.GRID_COL]) === c);
      out.set(/** @type {string} */ (c), orderGroups(cells, crows, columns, cfg, derived));
    }
    return out;
  };

  /**
   * R's cells as the aggregation engine gives them: {facet, group, color,
   * value, n}. A cell R marks `.drop` (a missing key under missing = "drop")
   * is not drawn.
   * @param {any[]} rows @param {Record<string, any>} cfg
   */
  const cellsFromRows = (rows, cfg) => {
    const { category, color, facet, func } = cfg;
    const out = [];
    for (const r of rows) {
      if (r['.drop']) continue;
      out.push({
        facet: facet ? String(r[facet] ?? '') : '__all__',
        group: category ? String(r[category] ?? '') : 'Total',
        color: color ? String(r[color] ?? '') : '__all__',
        value: func === 'count' ? r['.n'] : (r['.value'] ?? null),
        n: r['.n']
      });
    }
    return out;
  };

  /** The order of a panel's groups on the category axis. A bridge always keeps the data order (a
   * value sort scrambles the running total). "data" is factor levels, else time or numeric order
   * for a date or number column, else first appearance in the rows. "alpha" is factor levels, else
   * by name. "value" is the total across colour cells. Any other value names a column whose
   * smallest value per group orders it. @param {any[]} cells the panel's aggregate cells
   * @param {any[]} rows @param {VizColumn[]} columns @param {Record<string, any>} cfg
   * @param {ChartDerived} derived @returns {string[]} */
  const orderGroups = (cells, rows, columns, cfg, derived) => {
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
      const edges = derived.hist && derived.hist.label;
      const bins = Array.isArray(edges) && edges.length ? edges.map(String) : levels;
      if (bins) return bins;
    }
    const levelAt = new Map((levels || []).map((/** @type {string} */ l, /** @type {number} */ i) => [l, i]));

    const dataOrder = () => {
      const present = new Set(groups);
      if (levels) {
        return levels.filter((/** @type {string} */ l) => present.has(l)).concat(groups.filter((g) => !levelAt.has(g)));
      }
      // A date or a number orders by itself, a missing value last. Dates
      // travel as ISO strings, which sort as text.
      if (V.util.isTemporal(meta) || V.util.isNumeric(meta)) {
        const num = !V.util.isTemporal(meta);
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
    // A cell brings the column's smallest value in its rows (`.sort`).
    for (const r of rows) {
      const g = String(r[groupCol] ?? '');
      const v = Number(r['.sort']);
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

  /** The first row's tooltip-field values per group, per facet ('__all__' for all rows).
   * @param {any[]} rows @param {Record<string, any>} cfg @param {string[]} ttFields
   * @returns {Map<string, Record<string, any[]>>} */
  const ttRepValues = (rows, cfg, ttFields) => {
    const groupCol = cfg.category, facetCol = cfg.facet;
    /** @type {Map<string, Record<string, any[]>>} */
    const byFacet = new Map([['__all__', {}]]);
    // A cell brings the fields of its first row, and the cells come in the
    // order of their first rows.
    for (const r of rows) {
      const g = groupCol ? String(r[groupCol] ?? '') : 'Total';
      const recs = [byFacet.get('__all__')];
      if (facetCol) {
        const fv = String(r[facetCol] ?? '');
        if (!byFacet.has(fv)) byFacet.set(fv, {});
        recs.push(byFacet.get(fv));
      }
      for (const rec of recs) if (rec && !(g in rec)) rec[g] = ttFields.map((c) => r['.tt.' + c] ?? '');
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
    const groupCol = cfg.category, facetCol = cfg.facet;
    const colorCol = byColor ? cfg.color : null;
    /** @type {Map<string, string | null>} */
    const out = new Map();
    // A cell brings the distinct values of its rows, two at most.
    for (const r of rows) {
      const key = (facetCol ? String(r[facetCol] ?? '') : '__all__') + '|||' +
        (groupCol ? String(r[groupCol] ?? '') : 'Total') + '|||' +
        (colorCol ? String(r[colorCol] ?? '') : '__all__');
      for (const v of NS.valuesOf(r['.label'])) {
        if (v == null || v === '' || v === 'NA') continue;
        const t = String(v);
        const cur = out.get(key);
        if (cur === undefined) out.set(key, t);
        else if (cur !== null && cur !== t) out.set(key, null);
      }
    }
    return out;
  };

  /**
   * The category slots of a distribution panel and what each one draws: one
   * per group, or per (group, level) when colour splits the marks. An empty
   * (group, level) slot is dropped unless a fixed facet grid needs every
   * slot in every panel. One panel is no grid: a facet column the data does
   * not hold (a board's "Subgroup" before a subgroup is picked) puts every
   * row in one panel, and that panel keeps no empty slot.
   * @param {{ rows: any[], ix: any, cfg: Record<string, any>, groups: string[],
   *           levels: string[], facet: string, panels: number }} p
   */
  const distributionSlots = ({ rows, ix, cfg, groups, levels, facet, panels }) => {
    const groupBy = cfg.category, colorCol = cfg.color, facetCol = cfg.facet;
    const isBox = cfg.type === 'boxplot';
    // Keyed 'nz', so the panel of a missing facet value holds its rows.
    const facetRows = (facetCol && facet !== '__all__')
      ? ix.get(rows, facetCol, 'nz', String(facet)) : rows;
    const split = levels.length > 0;
    const keepEmpty = !!facetCol && panels > 1 && !freeCategories(cfg);
    const outliersOn = isBox && cfg.box_points === 'outliers';
    const idCol = cfg.id;

    const slots = [];
    for (const g of groups) {
      for (const lv of (split ? levels : ['__all__'])) {
        const c = (lv === '__all__'
          ? ix.get(facetRows, groupBy, 'raw', g)
          : ix.get2(facetRows, groupBy, 'raw', colorCol, 'nz', g, lv))[0];
        const n = (c && c['.vn']) || 0;
        if (split && !keepEmpty && n === 0) continue;
        const body = n ? { center: c['.center'], lo: c['.lo'], hi: c['.hi'] } : null;
        slots.push({
          cat: split ? g + BOX_CAT_SEP + lv : g, group: g, level: lv,
          n, count: idCol ? ((c && c['.count']) || 0) : n, body,
          whisker: isBox && body ? { lo: c['.wlo'], hi: c['.whi'] } : null,
          outliers: outliersOn && body ? NS.valuesOf(c['.out'] ?? []) : []
        });
      }
    }
    return slots;
  };

  /**
   * @typedef {{ facet: string, label: string | null, groups: string[], cells: any[],
   *             axisCounts: Map<string, any> | null, ttRep: Record<string, any[]> | null,
   *             slots: any[] | null }} AggregatedPanel
   * @typedef {{ family: 'aggregated', empty: VizEmpty | null, type: string,
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

  /** @param {{ rows: any[], ix: any, columns: VizColumn[], cfg: ChartSettings, derived: ChartDerived }} input
   * @returns {AggregatedModel} */
  const aggregated = ({ rows, ix, columns, cfg, derived }) => {
    // A histogram is a bar over the bins R adds to the rows.
    const ct = cfg.type === 'histogram' ? 'bar' : cfg.type;
    const { category, color, facet, measure } = cfg;
    const distribution = NS.DISTRIBUTION_TYPES.includes(ct);
    /** @type {AggregatedModel} */
    const out = {
      family: 'aggregated', empty: null, type: ct, cumulative: cfg.type === 'waterfall',
      colors: [], colorScale: NS.scaleFor(derived, color), levels: [], split: false,
      distribution, noValue: distribution && (!measure || measure === '.count'),
      ttFields: [], facet: facet || null, nFacets: 1, single: true,
      ix, panels: [], histogram: cfg.type === 'histogram', markLabels: null,
      hl: null, hlLit: null, hlColors: null
    };
    const agg = cellsFromRows(rows, cfg);
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
    const sharedGroups = (!freeCategories(cfg) && ct !== 'pie' && ct !== 'treemap')
      ? orderGroups(agg, rows, columns, cfg, derived) : null;
    // A free grid shares the categories down a column, as the panels of a
    // column share the category axis.
    const colGroups = (!sharedGroups && cfg.facet_grid && ct !== 'pie' && ct !== 'treemap')
      ? columnGroups(agg, rows, columns, cfg, derived) : null;

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
      const groups = sharedGroups || (colGroups && colGroups.get(model.gridLevels(fv).col)) ||
        orderGroups(cells, rows, columns, cfg, derived);
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
          ? distributionSlots({ rows, ix, cfg, groups, levels: out.levels, facet: fv,
                                panels: facets.length }) : null
      });
    }
    highlightMarks(out, rows, ix, cfg);
    return out;
  };

  /** The highlight: whether a bar (or a segment of one) holds a highlighted row, and with a colour
   * split the colour levels that do, which alone get palette colours. Bars only.
   * @param {AggregatedModel} out @param {any[]} rows @param {any} ix
   * @param {Record<string, any>} cfg */
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
      // A highlight on a column the cells do not hold: R marked the cells
      // with a matching row (`.hl`).
      const lit = !Object.keys(hl).every((c) => c in keys)
        ? rowsUnder(rows, ix, keys).some((r) => r['.hl'] === true)
        : !!NS.keys.markLit(keys, () => rowsUnder(rows, ix, keys), hl);
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
    model.facetKeys(keys, cfg.facet, cfg.facet_grid, facetVal);
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

  /** The rows under a mark: the rows whose columns hold the mark's keys. @param {any[]} rows
   * @param {any} ix @param {Record<string, any>} keys @returns {any[]} */
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

  Object.assign(model, { aggregated, cellOf, cellsOf, labelOf, aggregatedKeys,
                         aggregatedMarkAt, aggregatedClick, rowsUnder, BOX_CAT_SEP });
})();
