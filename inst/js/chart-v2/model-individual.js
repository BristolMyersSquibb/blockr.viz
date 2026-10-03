// @ts-check
/**
 * Chart block v2: the individual model (scatter, line, band).
 *
 * Rows and config in, what to draw out: the axes (type, categories), the
 * series levels (the series column, else the colour column) with their
 * colours, the legend, and per facet panel its rows and, for scatter and
 * line, one series per level holding the rows it draws and their points.
 * A band draws R's windowed statistics (config.band_series), not rows, so
 * its panels carry rows only for clicks and highlights.
 *
 * A point is a tuple [x, y, ...tooltip values]; a series' `rows` line up
 * with its `pts` (a line's sorted by x), so a datum index is a row.
 *
 * A mark's keys: a line or a band level is keyed by its level and the
 * facet; a scatter point by its series (when one is set) and the facet,
 * and otherwise by itself, its x and y (D6). Colour never widens a point.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});
  const model = /** @type {any} */ (NS.model = NS.model || {});

  // Config fields the model reads; a change to any other field reuses it.
  const MODEL_FIELDS = ['chart_type', 'x', 'y', 'color', 'facet', 'series', 'tt_fields',
    'scales', 'palette', 'count_on', 'count_col', 'func'];

  // Line charts: past TRAJ_FULL_MAX series the lines thin and drop their
  // markers, past TRAJ_REDUCED_MAX they thin further, and past the cap the
  // rest are not drawn (the footer says so).
  const TRAJ_FULL_MAX = 50;
  const TRAJ_REDUCED_MAX = 500;
  const TRAJ_HARD_CAP = 1000;

  // A colour column with more levels than this gets no legend.
  const LEGEND_MAX = 15;

  /**
   * @typedef {{ level: string | undefined, color: string, rows: any[], pts: any[][],
   *             src: any[] }} IndSeries
   * @typedef {{ facet: string, label: string | null, rows: any[],
   *             series: IndSeries[] }} IndividualPanel
   * @typedef {{ family: 'individual', empty: string | null, type: string,
   *             isLine: boolean, isScatter: boolean, isBand: boolean,
   *             x: { col: string, type: string, cats: string[] | null,
   *                  order: Map<string, number> | null },
   *             y: { col: string, type: string, cats: string[] | null },
   *             seriesCol: string | null, color: string | null, splitCol: string | null,
   *             facet: string | null, levels: string[], levelColors: string[],
   *             palette: string[], colorScale: any, seriesCount: number,
   *             lineOpacity: number, symbol: string, capMessage: string | null,
   *             ttFields: string[], legend: any, byColor: boolean,
   *             nFacets: number, single: boolean, ix: any,
   *             panels: IndividualPanel[] }} IndividualModel
   */

  /**
   * @param {{ rows: any[], ix: any, columns: VizColumn[], cfg: Record<string, any> }} input
   * @returns {IndividualModel}
   */
  const individual = ({ rows, ix, columns, cfg }) => {
    const ct = cfg.chart_type;
    const { x, y } = cfg;
    const color = cfg.color || null;
    const facet = cfg.facet || null;
    const seriesCol = cfg.series || null;
    // The series is the splitter; without one, a series per colour level.
    const splitCol = seriesCol || color;
    const isLine = ct === 'line';
    const isBand = ct === 'band';
    /** @type {IndividualModel} */
    const out = {
      family: 'individual', empty: null, type: ct,
      isLine, isScatter: ct === 'scatter', isBand,
      x: { col: x, type: 'value', cats: null, order: null },
      y: { col: y, type: 'value', cats: null },
      seriesCol, color, splitCol, facet, levels: [], levelColors: [],
      palette: NS.paletteOf(cfg), colorScale: NS.scaleFor(cfg, color),
      seriesCount: 1, lineOpacity: 1, symbol: 'circle', capMessage: null,
      ttFields: [], legend: null, byColor: false,
      nFacets: 1, single: true, ix, panels: []
    };
    if (!x || !y) {
      out.empty = model.emptyState('Select X and Y columns');
      return out;
    }

    // Tooltip fields, less the ones the tooltip already shows (x is the
    // header, y the value, the split column the series name).
    const shown = new Set([x, y, splitCol].filter(Boolean).map(String));
    out.ttFields = [].concat(cfg.tt_fields || []).map(String)
      .filter((c) => c && c !== 'null' && c !== '(none)' && !shown.has(c));

    const facets = facet ? ix.levels(rows, facet, 'nz').slice().sort() : ['__all__'];
    out.nFacets = facets.length;
    out.single = facets.length === 1;

    /** @type {string[]} */
    let levels = splitCol
      ? NS.axes.orderLevels(ix.levels(rows, splitCol, 'nz'), NS.scaleFor(cfg, splitCol),
                            columns, splitCol)
      : [];
    if (isLine && levels.length > TRAJ_HARD_CAP) {
      out.capMessage = `Showing first ${TRAJ_HARD_CAP} of ${levels.length} series` +
        ' (' + (splitCol || 'series') + ') — filter upstream to narrow.';
      levels = levels.slice(0, TRAJ_HARD_CAP);
    }
    out.levels = levels;
    out.seriesCount = levels.length || 1;
    if (isLine) {
      if (out.seriesCount > TRAJ_REDUCED_MAX) { out.lineOpacity = 0.15; out.symbol = 'none'; }
      else if (out.seriesCount > TRAJ_FULL_MAX) { out.lineOpacity = 0.35; out.symbol = 'none'; }
    }

    const colours = seriesColours(rows, ix, out, cfg);
    out.levelColors = levels.map((lv, i) => colours.of(lv, i));
    legendOf(rows, ix, out, colours.lookup);

    const xType = NS.axes.axisTypeFor(columns, rows, x);
    const yType = NS.axes.axisTypeFor(columns, rows, y);
    const xCats = xType === 'category' ? NS.axes.orderedCategories(columns, rows, x) : null;
    const yCats = yType === 'category' ? NS.axes.orderedCategories(columns, rows, y) : null;
    // A categorical x's axis positions: a line is drawn in axis order, or it
    // zig-zags between visits.
    const xOrder = xCats
      ? new Map(xCats.map((/** @type {any} */ c, /** @type {number} */ i) => [String(c), i]))
      : null;
    out.x = { col: x, type: xType, cats: xCats, order: xOrder };
    out.y = { col: y, type: yType, cats: yCats };

    const encodeX = (/** @type {any} */ v) => xType === 'category' ? String(v ?? '') : Number(v);
    const encodeY = (/** @type {any} */ v) => yType === 'category' ? String(v ?? '') : Number(v);
    const tt = out.ttFields;
    /** @param {any} r */
    const packPt = (r) => {
      const p = [encodeX(r[x]), encodeY(r[y])];
      for (const c of tt) p.push(r[c] ?? '');
      return p;
    };
    const drawn = (/** @type {any} */ r) => r[x] != null && r[y] != null;

    /** @param {string | undefined} level @param {any[]} rs @param {string} clr @returns {IndSeries} */
    const mkSeries = (level, rs, clr) => {
      let pts = rs.map(packPt);
      let srows = rs;
      if (isLine) {
        const order = lineOrder(pts, xType, xOrder);
        if (order) {
          pts = order.map((i) => pts[i]);
          srows = order.map((i) => rs[i]);
        }
      }
      // `src`: the rows in data order, which the error bars keep.
      return { level, color: clr, rows: srows, pts, src: rs };
    };

    const labels = model.facetLabels(rows, cfg, facets);
    for (const fv of facets) {
      // As v1: a facet's rows by the raw value, so the panel of a missing
      // facet value holds none.
      const prow = fv === '__all__' ? rows : ix.get(rows, /** @type {string} */ (facet), 'raw', fv);
      /** @type {IndSeries[]} */
      const series = [];
      if (!isBand) {
        if (!levels.length) {
          series.push(mkSeries(undefined, prow.filter(drawn), out.palette[0]));
        } else {
          levels.forEach((lv, ci) => {
            // Keyed like the levels: a null split value is the '' series.
            series.push(mkSeries(lv, ix.get(prow, splitCol, 'nz', lv).filter(drawn),
                                 out.levelColors[ci]));
          });
        }
      }
      out.panels.push({
        facet: fv,
        label: (!out.single && fv !== '__all__') ? labels.get(fv) : null,
        rows: prow, series
      });
    }
    return out;
  };
  individual.FIELDS = MODEL_FIELDS;

  /**
   * The order a line's points are drawn in: by axis position on a
   * categorical x, by value on a numeric one; null on another category
   * axis (data order). Stable, so equal x keep their row order.
   * @param {any[][]} pts @param {string} xType @param {Map<string, number> | null} xOrder
   * @returns {number[] | null}
   */
  const lineOrder = (pts, xType, xOrder) => {
    const idx = pts.map((_, i) => i);
    if (xOrder) {
      const pos = pts.map((p) => xOrder.get(String(p[0])) ?? 0);
      return idx.sort((a, b) => pos[a] - pos[b]);
    }
    if (xType !== 'category') return idx.sort((a, b) => pts[a][0] - pts[b][0]);
    return null;
  };

  /**
   * The colour of each series level. With a series column apart from the
   * colour column, a series takes the colour of its first row's colour
   * value: the board scale's, else the next palette colour in the order
   * the values come up (cycling, so a level keeps its colour under a
   * filter). Otherwise a level takes the scale's colour or the palette's
   * in level order.
   * @param {any[]} rows @param {any} ix @param {IndividualModel} m @param {Record<string, any>} cfg
   */
  const seriesColours = (rows, ix, m, cfg) => {
    const { palette, colorScale, color, seriesCol } = m;
    /** @type {Map<string, string>} colour value -> its cycled colour */
    const lookup = new Map();
    /** @param {string} level @param {number} index */
    const of = (level, index) => {
      if (!color) return palette[0];
      if (seriesCol && seriesCol !== color) {
        const rep = ix.get(rows, seriesCol, 'nz', level)[0];
        const cv = rep ? String(rep[color] ?? '') : '';
        if (colorScale && colorScale.color && colorScale.color[cv] != null) {
          return colorScale.color[cv];
        }
        if (!lookup.has(cv)) lookup.set(cv, palette[lookup.size % palette.length]);
        return /** @type {string} */ (lookup.get(cv));
      }
      if (colorScale && colorScale.color && colorScale.color[level] != null) {
        return colorScale.color[level];
      }
      return palette[index % palette.length];
    };
    return { of, lookup };
  };

  /**
   * The legend band's chips, when the colour column has a readable number
   * of levels. With a series column apart from the colour, the chips are
   * the colour's values (sorted); the option adds an empty series per chip
   * for the legend to bind to. Fills m.legend and m.byColor.
   * @param {any[]} rows @param {any} ix @param {IndividualModel} m
   * @param {Map<string, string>} lookup
   */
  const legendOf = (rows, ix, m, lookup) => {
    const { color, seriesCol, colorScale, palette } = m;
    if (!color) return;
    const colorLevels = ix.levels(rows, color, 'nz');
    if (!colorLevels.length || colorLevels.length > LEGEND_MAX) return;
    m.byColor = !!seriesCol && seriesCol !== color;
    if (!m.byColor) {
      m.legend = { col: color, items: m.levels.map((lv, i) => ({ name: lv, color: m.levelColors[i] })) };
      return;
    }
    const cbLevels = colorLevels.slice().sort();
    const items = cbLevels.map((/** @type {string} */ lvl, /** @type {number} */ i) => ({
      name: lvl,
      color: (colorScale && colorScale.color && colorScale.color[lvl] != null)
        ? colorScale.color[lvl] : (lookup.get(lvl) || palette[i % palette.length])
    }));
    m.legend = { col: color, items };
  };

  // -- Marks, keys and clicks ---------------------------------------------------

  /**
   * The keys of a line, a band level or a scatter series: the split
   * column's level and the facet in a facet panel. A missing level or
   * facet value is a key too ('').
   * @param {IndividualModel} m @param {any} facetVal @param {any} level
   * @returns {Record<string, any>}
   */
  const individualKeys = (m, facetVal, level) => {
    /** @type {Record<string, any>} */
    const keys = {};
    if (m.splitCol && level != null) keys[m.splitCol] = String(level);
    if (m.facet && facetVal != null && facetVal !== '__all__') keys[m.facet] = facetVal;
    return keys;
  };

  /** The panel a facet value names. @param {IndividualModel} m @param {any} facetVal */
  const panelOf = (m, facetVal) =>
    m.panels.find((p) => p.facet === facetVal) || null;

  /**
   * The rows under a clicked point: the panel's rows at its x and y, in its
   * series when the chart is split.
   * @param {IndividualModel} m @param {IndividualPanel} panel @param {any} seriesName
   * @param {any[]} v the point's value
   */
  const pointRows = (m, panel, seriesName, v) => {
    const pool = (m.splitCol && seriesName != null)
      ? m.ix.get(panel.rows, m.splitCol, 'nz', String(seriesName)) : panel.rows;
    const xs = String(v[0]), ys = String(v[1]);
    return pool.filter((/** @type {any} */ r) =>
      String(r[m.x.col]) === xs && String(r[m.y.col]) === ys);
  };

  /**
   * What a click sends, or null when it sends nothing.
   *
   *   - A band (D7): its level when it is coloured; an uncoloured band is
   *     not clickable.
   *   - A split line: its level, the one the cursor hovers (`focusName`)
   *     before the one ECharts' hit test names.
   *   - A scatter point (D6): its series when one is set, else the point
   *     itself, a zero-width range at its x and y. An unsplit line's point
   *     is a range too.
   *
   * Auto sends the keys (the facet with them, D3; a missing value as null).
   * An explicit drill column sends that column's values in the rows under
   * the mark. Nothing is sent when those rows hold none.
   * @param {IndividualModel} m @param {Record<string, any>} cfg
   * @param {IndividualPanel | null} panel @param {any} params
   * @param {string} [focusName]
   * @returns {{ filters: Record<string, any[]> } | { range: any } | null}
   */
  const individualClick = (m, cfg, panel, params, focusName) => {
    if (!panel || NS.drillState(cfg) === 'off') return null;
    const explicit = cfg.drill !== 'auto';
    /** @param {Record<string, any[]> | null} f */
    const wrap = (f) => (f ? { filters: f } : null);
    /** @param {string} lv */
    const levelRows = (lv) => m.ix.get(panel.rows, m.splitCol, 'nz', lv);

    if (m.isBand) {
      if (!m.splitCol || params.seriesName == null) return null;
      const lv = String(params.seriesName);
      if (explicit) return wrap(NS.keys.fromRows(cfg.drill, levelRows(lv)));
      return wrap(NS.keys.fromKeys(individualKeys(m, panel.facet, lv)));
    }
    const lineName = (m.isLine && m.splitCol)
      ? (focusName !== undefined ? focusName : params.seriesName) : null;
    if (lineName != null) {
      const lv = String(lineName);
      if (explicit) return wrap(NS.keys.fromRows(cfg.drill, levelRows(lv)));
      return wrap(NS.keys.fromKeys(individualKeys(m, panel.facet, lv)));
    }
    const v = params.value;
    if (!Array.isArray(v) || v.length < 2) return null;
    if (explicit) return wrap(NS.keys.fromRows(cfg.drill, pointRows(m, panel, params.seriesName, v)));
    if (m.seriesCol && !m.isLine && params.seriesName != null) {
      return wrap(NS.keys.fromKeys(individualKeys(m, panel.facet, params.seriesName)));
    }
    return withPanel(m, panel,
      { x_col: m.x.col, y_col: m.y.col, x_range: [v[0], v[0]], y_range: [v[1], v[1]] });
  };

  /**
   * A range taken in a panel: in a facet panel it carries the panel's key
   * as `filters`, as a click does (D10).
   * @param {IndividualModel} m @param {IndividualPanel} panel @param {any} range
   * @returns {{ range: any, filters?: Record<string, any[]> }}
   */
  const withPanel = (m, panel, range) => {
    const keys = individualKeys(m, panel.facet, null);
    return Object.keys(keys).length ? { range, filters: NS.keys.fromKeys(keys) } : { range };
  };

  /**
   * What a brush caught: the x and y of every brushed point (a dataIndex
   * is its own series' index; one outside the series is skipped); with an
   * explicit drill column, the panel's rows at those points, each (x, y)
   * taken once however many of its points were caught; with a series
   * column, the series of the brushed points.
   * @param {IndividualModel} m @param {Record<string, any>} cfg
   * @param {IndividualPanel | null} panel @param {any[]} allSeries chart.getOption().series
   * @param {Array<{ seriesIndex: number, dataIndex: number }>} selected
   * @returns {{ xVals: any[], yVals: any[], rows: any[], drill: string | null,
   *             levels: string[] }}
   */
  const individualBrush = (m, cfg, panel, allSeries, selected) => {
    // Colour never widens a brush (D9): only an explicit drill column
    // reads the rows.
    const drill = cfg.drill && cfg.drill !== 'auto' ? String(cfg.drill) : null;
    /** @type {Set<string>} */
    const levels = new Set();
    const xc = cfg.x, yc = cfg.y;
    /** @type {Map<string, any[]> | null} */
    let rowIndex = null;
    if (drill && panel) {
      rowIndex = new Map();
      for (const r of panel.rows) {
        const k = String(r[xc]) + '|||' + String(r[yc]);
        const b = rowIndex.get(k);
        if (b) b.push(r); else rowIndex.set(k, [r]);
      }
    }
    /** @type {any[]} */
    const xVals = [], yVals = [], rows = [];
    const taken = new Set();
    for (const sel of selected) {
      const sData = allSeries[sel.seriesIndex] && allSeries[sel.seriesIndex].data;
      if (!sData || sel.dataIndex < 0 || sel.dataIndex >= sData.length) continue;
      const pt = sData[sel.dataIndex];
      if (!pt) continue;
      const vx = Array.isArray(pt) ? pt[0] : pt.value && pt.value[0];
      const vy = Array.isArray(pt) ? pt[1] : pt.value && pt.value[1];
      if (vx != null) xVals.push(vx);
      if (vy != null) yVals.push(vy);
      if (m.seriesCol) levels.add(String(allSeries[sel.seriesIndex].name ?? ''));
      if (rowIndex) {
        const k = String(vx) + '|||' + String(vy);
        const hit = taken.has(k) ? null : rowIndex.get(k);
        if (hit) {
          taken.add(k);
          for (const r of hit) rows.push(r);
        }
      }
    }
    return { xVals, yVals, rows, drill, levels: [...levels] };
  };

  /**
   * What a brush sends (D9), from what it caught, or null for nothing:
   *
   *   - an explicit drill column: its values in the rows at the points;
   *   - a series column: the series of the brushed points;
   *   - else the points' x (and, off a line, y) extent.
   *
   * Colour never widens it. In a facet panel the series and the range
   * carry the panel's key, as a click does (D3, D10).
   * @param {IndividualModel} m @param {Record<string, any>} cfg
   * @param {IndividualPanel | null} panel
   * @param {{ xVals: any[], yVals: any[], rows: any[], drill: string | null,
   *           levels: string[] }} b
   * @returns {{ filters: Record<string, any[]> } |
   *           { range: any, filters?: Record<string, any[]> } | null}
   */
  const brushSelection = (m, cfg, panel, b) => {
    if (b.drill) {
      const f = NS.keys.fromRows(b.drill, b.rows);
      if (f) return { filters: f };
    }
    const keys = panel ? individualKeys(m, panel.facet, null) : {};
    if (!b.drill && m.seriesCol && b.levels.length) {
      return { filters: { [m.seriesCol]: b.levels.map((lv) => (NS.keys.isMissing(lv) ? null : lv)),
                          ...NS.keys.fromKeys(keys) } };
    }
    if (!b.xVals.length) return null;
    const range = {
      x_col: cfg.x, y_col: cfg.y,
      x_range: [NS.minOf(b.xVals), NS.maxOf(b.xVals)],
      y_range: (!m.isLine && b.yVals.length) ? [NS.minOf(b.yVals), NS.maxOf(b.yVals)] : null
    };
    return Object.keys(keys).length ? { range, filters: NS.keys.fromKeys(keys) } : { range };
  };

  /**
   * y of a line at `x`, interpolated between its points, or null outside
   * its range. On a category x the points' x are categories, compared by
   * their axis position (`xOrder`), which is also what a pixel converts to.
   * @param {any[]} data @param {number} x @param {Map<string, number> | null} xOrder
   * @returns {number | null}
   */
  const interpYAtX = (data, x, xOrder) => {
    const n = data.length;
    if (!n) return null;
    const xat = xOrder
      ? (/** @type {number} */ i) => {
          const v = xOrder.get(String(data[i][0]));
          return v == null ? NaN : v;
        }
      : (/** @type {number} */ i) => data[i][0];
    if (x < xat(0) || x > xat(n - 1)) return null;
    for (let k = 1; k < n; k++) {
      const x0 = xat(k - 1), x1 = xat(k);
      if (x >= x0 && x <= x1) {
        const y0 = data[k - 1][1], y1 = data[k][1];
        return x1 === x0 ? y0 : y0 + (y1 - y0) * (x - x0) / (x1 - x0);
      }
    }
    return null;
  };

  Object.assign(model, { individual, individualKeys, individualClick, individualBrush,
                         brushSelection, pointRows, panelOf, interpYAtX, lineOrder,
                         TRAJ_FULL_MAX, TRAJ_REDUCED_MAX, TRAJ_HARD_CAP });
})();
