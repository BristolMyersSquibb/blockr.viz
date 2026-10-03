// @ts-check
/**
 * Chart block v2: the timeline (gantt) model.
 *
 * Rows and config in, what to draw out: the panels (one per facet level
 * holding rows), the lanes of each panel in their sort order, the lane and
 * facet counts, the colour levels, and one mark per row with an x.
 *
 * A mark is a tuple, because a swim-lane chart has a mark per event and an
 * object per event costs memory the chart does not need:
 *
 *   [start, end, lane, term, colour, label, series, drill, tooltip]
 *
 *   start, end  x coordinates (a category index on a category x); end is
 *               start for an event without an end
 *   lane        the lane's position in the panel
 *   term        the lane value as a string (the y)
 *   colour      the colour column's value, or ''
 *   label       the label column's value, or ''
 *   series      the series column's value, or ''
 *   drill       the drill column's value (what a click filters on), or ''
 *   tooltip     the tooltip fields' values, or 0 without any
 *
 * The option builder hands these to ECharts as they are, so a click gets the
 * tuple back as `params.value`.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});
  const model = NS.model = NS.model || {};

  // Config fields the model reads; a change to any other field reuses it.
  const MODEL_FIELDS = ['chart_type', 'x', 'xend', 'y', 'color', 'facet', 'sort_by', 'sort_dir',
    'series', 'label', 'tt_fields', 'drill', 'scales', 'count_on', 'count_col', 'func',
    'facet_scales'];

  /** @param {Record<string, any>} cfg @returns {string[]} */
  const ttFieldsOf = (cfg) => [].concat(cfg.tt_fields || [])
    .map(String).filter((c) => c && c !== 'null');

  /**
   * @typedef {{ facet: string, label: string | null, lanes: string[],
   *             laneCounts: Map<string, any> | null, marks: any[][],
   *             rows: any[] }} TimelinePanel
   * @typedef {{ family: 'timeline', empty: string | null,
   *             x: { col: string, type: string, cats: string[] | null },
   *             y: { col: string }, xend: string | null, series: string | null,
   *             label: string | null, color: { col: string, levels: any[], scale: any } | null,
   *             facet: string | null, nFacets: number, single: boolean,
   *             ttFields: string[], panels: TimelinePanel[] }} TimelineModel
   */

  /**
   * @param {{ rows: any[], ix: any, columns: VizColumn[], cfg: Record<string, any> }} input
   * @returns {TimelineModel}
   */
  const timeline = ({ rows, ix, columns, cfg }) => {
    const { x, xend, y, color, facet, sort_by, series, label } = cfg;
    const ttFields = ttFieldsOf(cfg);
    // The drill column's value rides on every mark: the lane under auto,
    // or the override's column.
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

    // An x value as a coordinate: the category's first position on a
    // category axis, from a map built once.
    const xPos = xCats ? NS.firstIndex(xCats) : null;
    /** @param {any} v */
    const xCoord = (v) => {
      if (xPos) {
        const i = xPos.get(String(v ?? ''));
        return i === undefined ? 0 : i;
      }
      return Number(v);
    };

    // Lanes in sort order. By onset or a column: the smallest value per
    // lane, ascending; lanes without one are left out. A-Z: by name.
    /** @param {any[]} rs @returns {string[]} */
    const sortTerms = (rs) => {
      const sb = sort_by || 'onset';
      if (sb === 'alpha') {
        return [...new Set(rs.map((r) => String(r[y] ?? '')))]
          .sort((a, b) => a.localeCompare(b) * sortDir);
      }
      const sortCol = (sb === 'onset') ? x : sb;
      // A plain object on purpose: its key order (integer-like keys first)
      // is the tie order v1 drew.
      /** @type {Record<string, number>} */
      const mins = {};
      for (const r of rs) {
        const k = String(r[y] ?? '');
        const v = (xPos && sortCol === x) ? xCoord(r[sortCol]) : Number(r[sortCol]);
        if (!isNaN(v) && (mins[k] == null || v < mins[k])) mins[k] = v;
      }
      return Object.keys(mins).sort((a, b) => (mins[a] - mins[b]) * sortDir);
    };

    const panels = [];
    for (const fv of facets) {
      const prow = fv === '__all__' ? rows : ix.get(rows, facet, 'raw', fv);
      if (prow.length) panels.push({ fv, rows: prow });
    }

    const labels = NS.model.facetLabels(rows, cfg, panels.map((p) => p.fv));
    // Fixed scales give every panel the same lanes in the same order, so a
    // row of the grid is one term; free scales keep each panel's own.
    const sharedTerms = NS.facetScales(cfg) !== 'free' ? sortTerms(rows) : null;
    const countAxis = NS.countOn(cfg, 'axis');

    for (const { fv, rows: prow } of panels) {
      const terms = sharedTerms || sortTerms(prow);
      // Events per lane, or distinct count_col. A lane has no "as is" value,
      // so the identity aggregation of a bar does not apply here.
      const laneCounts = countAxis
        ? NS.labelCounts(prow, { count_col: cfg.count_col }, y) : null;

      /** @type {Map<string, number>} */
      const laneOf = new Map();
      terms.forEach((t, i) => laneOf.set(t, i));

      const marks = [];
      // The source row of each mark, for a filter on a column the mark does
      // not carry (D5).
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
                    ttFields.length ? ttFields.map((c) => r[c] ?? '') : 0]);
        markRows.push(r);
      }
      out.panels.push({
        facet: fv,
        label: (!out.single && fv !== '__all__') ? labels.get(fv) : null,
        lanes: terms, laneCounts, marks, rows: markRows
      });
    }
    return out;
  };
  timeline.FIELDS = MODEL_FIELDS;

  /**
   * The keys of a timeline mark: its lane, and the facet in a facet panel
   * (D3). The colour is an attribute of the event, not a key. A missing
   * lane or facet value is a key too ('').
   * @param {Record<string, any>} cfg @param {any} facetVal @param {any[]} mark
   * @returns {Record<string, any>}
   */
  const timelineKeys = (cfg, facetVal, mark) => {
    /** @type {Record<string, any>} */
    const keys = { [cfg.y]: mark[3] };
    if (cfg.facet && facetVal != null && facetVal !== '__all__') keys[cfg.facet] = facetVal;
    return keys;
  };

  /**
   * The filter a click on a timeline mark sends, or null when it sends
   * nothing. Auto: the mark's keys, a missing lane or facet sent as null.
   * An override column: the clicked event's own value of it, nothing when
   * the event has none.
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

  Object.assign(model, { timeline, timelineKeys, timelineClick, ttFieldsOf });
})();
