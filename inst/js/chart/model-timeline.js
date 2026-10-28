// @ts-check
/**
 * Chart block: the timeline (gantt) model.
 *
 * Rows and config in, what to draw out: the panels (one per facet level
 * holding rows), their lanes in sort order, the lane and facet counts, the
 * colour levels, and one mark per row with an x. A mark is a tuple, since a
 * swim-lane chart has a mark per event; ECharts hands it back as
 * `params.value` on a click:
 *
 *   [start, end, lane, term, colour, label, series, drill, tooltip]
 *
 * start and end are x coordinates (a category index on a category x; end is
 * start for an event without one), lane is the lane's position in the
 * panel, term the lane value as a string, colour/label/series/drill the
 * columns' values or '', tooltip the tooltip fields' values or 0.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const model = /** @type {any} */ (NS.model);

  // Config fields the model reads; a change to any other field reuses it.
  const MODEL_FIELDS = ['type', 'x', 'xend', 'y', 'color', 'facet', 'sort_by', 'sort_dir',
    'series', 'label', 'tooltip', 'drill', 'derived.scales', 'show_n', 'id', 'func',
    'facet_scales'];

  /**
   * @typedef {{ facet: string, label: string | null, lanes: string[],
   *             laneCounts: Map<string, any> | null, marks: any[][],
   *             rows: any[] }} TimelinePanel
   * @typedef {{ family: 'timeline', empty: VizEmpty | null,
   *             x: { col: string, type: string, cats: string[] | null },
   *             y: { col: string }, xend: string | null, series: string | null,
   *             label: string | null, color: { col: string, levels: any[], scale: any } | null,
   *             facet: string | null, nFacets: number, single: boolean,
   *             ttFields: string[], panels: TimelinePanel[] }} TimelineModel
   */

  /** @param {{ rows: any[], ix: any, columns: VizColumn[], cfg: Record<string, any> }} input
   * @returns {TimelineModel} */
  const timeline = ({ rows, ix, columns, cfg }) => {
    const { x, xend, y, color, facet, sort_by, series, label } = cfg;
    const ttFields = model.tooltipFields(cfg);
    // The drill column's value rides on every mark.
    const drill = NS.drillColumn(cfg);
    const sortDir = cfg.sort_dir === 'desc' ? -1 : 1;
    /** @type {TimelineModel} */
    const out = {
      family: 'timeline', empty: null,
      x: { col: x, type: 'value', cats: null }, y: { col: y },
      xend: xend || null, series: series || null, label: label ?? null,
      color: null, facet: facet || null, nFacets: 1, single: true, ttFields, panels: []
    };
    if (!x || !y) {
      out.empty = NS.model.emptyState('Select X (start) and Y (term) columns');
      return out;
    }

    const xAxisType = NS.axes.axisTypeFor(columns, rows, x);
    const xCats = xAxisType === 'category' ? NS.axes.orderedCategories(columns, rows, x) : null;
    out.x = { col: x, type: xAxisType, cats: xCats };

    const facets = facet ? ix.levels(rows, facet, 'nz').slice().sort() : ['__all__'];
    out.nFacets = facets.length;
    out.single = facets.length === 1;

    const colorScale = NS.scaleFor(cfg, color);
    if (color) {
      const levels = NS.axes.orderLevels(ix.levels(rows, color, 'nz'), colorScale, columns, color);
      if (levels.length) out.color = { col: color, levels, scale: colorScale };
    }

    // An x value as a coordinate: a category's first position, or the number.
    const xPos = xCats ? NS.firstIndex(xCats) : null;
    /** @param {any} v */
    const xCoord = (v) => (xPos ? (xPos.get(String(v ?? '')) ?? 0) : Number(v));

    // Lanes in sort order: by name for 'alpha', else by the smallest value
    // per lane of x ('onset') or of a column; a lane without one is left out.
    /** @param {any[]} rs @returns {string[]} */
    const sortTerms = (rs) => {
      const sb = sort_by || 'onset';
      if (sb === 'alpha') {
        return [...new Set(rs.map((r) => String(r[y] ?? '')))]
          .sort((a, b) => a.localeCompare(b) * sortDir);
      }
      const sortCol = (sb === 'onset') ? x : sb;
      // A plain object on purpose: its key order (integer-like keys first)
      // is the tie order.
      /** @type {Record<string, number>} */
      const mins = {};
      for (const r of rs) {
        const k = String(r[y] ?? '');
        const v = (xPos && sortCol === x) ? xCoord(r[sortCol]) : Number(r[sortCol]);
        if (!isNaN(v) && (mins[k] == null || v < mins[k])) mins[k] = v;
      }
      return Object.keys(mins).sort((a, b) => (mins[a] - mins[b]) * sortDir);
    };

    const facetLabel = model.facetLabels(rows, cfg, out.nFacets);
    // Fixed scales give every panel the same lanes; free scales keep each its own.
    const sharedTerms = NS.facetScales(cfg) !== 'free' ? sortTerms(rows) : null;
    const countAxis = NS.countOn(cfg, 'axis');

    for (const fv of facets) {
      const prow = fv === '__all__' ? rows : ix.get(rows, facet, 'raw', fv);
      if (!prow.length) continue;
      const terms = sharedTerms || sortTerms(prow);
      /** @type {Map<string, number>} */
      const laneOf = new Map();
      terms.forEach((t, i) => laneOf.set(t, i));

      // One mark per row with an x and a known lane; `markRows` is the row
      // behind each mark, for a filter on a column the mark does not carry.
      const marks = [];
      const markRows = [];
      for (const r of prow) {
        if (r[x] == null) continue;
        const term = String(r[y] ?? '');
        const lane = laneOf.get(term);
        if (lane === undefined) continue;
        const s = xCoord(r[x]);
        const hasEnd = xend && r[xend] != null && !Number.isNaN(Number(r[xend]));
        marks.push([s, hasEnd ? xCoord(r[xend]) : s, lane, term, r[color] ?? '',
                    (label != null ? (r[label] ?? '') : ''),
                    r[series] ?? '',
                    (drill != null ? (r[drill] ?? '') : ''),
                    ttFields.length ? ttFields.map((/** @type {string} */ c) => r[c] ?? '') : 0]);
        markRows.push(r);
      }
      out.panels.push({
        facet: fv,
        label: facetLabel(fv),
        // Events per lane, or distinct ids; a lane has no "as is" value.
        lanes: terms, laneCounts: countAxis ? NS.labelCounts(prow, { id: cfg.id }, y) : null,
        marks, rows: markRows
      });
    }
    return out;
  };
  timeline.FIELDS = MODEL_FIELDS;

  /** The keys of a timeline mark: its lane, and the facet in a facet panel. The colour is no key.
   * @param {Record<string, any>} cfg @param {any} facetVal @param {any[]} mark
   * @returns {Record<string, any>} */
  const timelineKeys = (cfg, facetVal, mark) => {
    /** @type {Record<string, any>} */
    const keys = { [cfg.y]: mark[3] };
    if (cfg.facet && facetVal != null && facetVal !== '__all__') keys[cfg.facet] = facetVal;
    return keys;
  };

  /**
   * The filter a click on a timeline mark sends, or null. Auto: the mark's
   * keys. A drill column: the event's own value of it, none when it has none.
   * @param {Record<string, any>} cfg @param {any} facetVal @param {any[]} mark
   * @returns {Record<string, any[]> | null}
   */
  const timelineClick = (cfg, facetVal, mark) => {
    const drill = NS.drillColumn(cfg);
    if (!drill) return null;
    if (cfg.drill === 'auto') return NS.keys.fromKeys(timelineKeys(cfg, facetVal, mark));
    const dv = mark[7];
    if (dv == null || dv === '') return null;
    return { [drill]: [String(dv)] };
  };

  Object.assign(model, { timeline, timelineKeys, timelineClick });
})();
