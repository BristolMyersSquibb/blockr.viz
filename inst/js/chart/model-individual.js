// @ts-check
/**
 * Chart block: the individual model (scatter, line, band).
 *
 * Rows and config in, what to draw out: the axes, the series levels (the
 * series column, else the colour column) with their colours, the legend,
 * and per facet panel its rows and, for scatter and line, one series per
 * level with the points it draws. A band draws R's windowed statistics
 * (derived.band.series), so its panels carry rows only for clicks and
 * highlights.
 *
 * A point is a tuple [x, y, ...tooltip values]; a series' `rows` line up
 * with its `pts`, so a datum index is a row.
 *
 * A mark's keys: a line or band level is keyed by its level and the facet; a
 * scatter point by its series (when set) and the facet, else by its x and y.
 * Colour never widens a point.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const model = /** @type {any} */ (NS.model = NS.model || {});

  // Line charts: past TRAJ_FULL_MAX series the lines thin and lose their
  // markers, past TRAJ_REDUCED_MAX they thin further, past the cap the rest
  // are not drawn (the footer says so).
  const TRAJ_FULL_MAX = 50;
  const TRAJ_REDUCED_MAX = 500;
  const TRAJ_HARD_CAP = 1000;

  // A colour column with more levels than this gets no legend.
  const LEGEND_MAX = 15;

  /**
   * @typedef {{ level: string | undefined, color: string, rows: any[], pts: any[][],
   *             src: any[], muted?: boolean }} IndSeries
   * @typedef {{ facet: string, label: string | null, rows: any[],
   *             series: IndSeries[] }} IndividualPanel
   * @typedef {{ family: 'individual', empty: VizEmpty | null, type: string,
   *             isLine: boolean, isBand: boolean,
   *             x: { col: string, type: string, cats: string[] | null,
   *                  order: Map<string, number> | null, encode?: (v: any) => any,
   *                  period?: string | null },
   *             y: { col: string, type: string, cats: string[] | null },
   *             seriesCol: string | null, color: string | null, splitCol: string | null,
   *             facet: string | null, levels: string[], levelColors: string[],
   *             palette: string[], colorScale: any, seriesCount: number,
   *             lineOpacity: number, symbol: string, capMessage: string | null,
   *             crowd: boolean, logX: boolean, logY: boolean, logDropped: number,
   *             ttFields: string[], legend: any, byColor: boolean,
   *             nFacets: number, single: boolean, ix: any,
   *             panels: IndividualPanel[], hl: Record<string, string[]> | null }} IndividualModel
   */

  /** The x and y axes (type, categories, how a value is placed) and whether each is logarithmic,
   * set on the model. @param {any[]} rows @param {VizColumn[]} columns @param {any} cfg
   * @param {IndividualModel} out */
  const setAxes = (rows, columns, cfg, out) => {
    const { x, y } = cfg;
    // A Date x is a time axis; a Date y stays a category.
    const xType = NS.axes.axisTypeFor(columns, rows, x, { temporal: true });
    const yType = NS.axes.axisTypeFor(columns, rows, y);
    const xCats = xType === 'category' ? NS.axes.orderedCategories(columns, rows, x) : null;
    const yCats = yType === 'category' ? NS.axes.orderedCategories(columns, rows, y) : null;
    // A line on a categorical x is drawn in axis order.
    const xOrder = xCats
      ? new Map(xCats.map((/** @type {any} */ c, /** @type {number} */ i) => [String(c), i]))
      : null;
    // A date arrives as an ISO string; a time axis places it in ms.
    const encodeX = (/** @type {any} */ v) => xType === 'category' ? String(v ?? '')
      : (xType === 'time' ? NS.axes.timeValue(v) : Number(v));
    out.x = { col: x, type: xType, cats: xCats, order: xOrder, encode: encodeX,
              period: xType === 'time' ? NS.axes.timePeriod(rows, x) : null };
    out.y = { col: y, type: yType, cats: yCats };
    // A row at or below zero on a log axis is not drawn; the footer counts it.
    const logs = NS.logAxes(cfg);
    out.logX = logs.x && xType === 'value';
    out.logY = logs.y && yType === 'value';
  };

  /** The facet panels: per panel its rows and, for scatter and line, one series per level.
   * @param {any[]} rows @param {any} ix @param {any} cfg @param {IndividualModel} out
   * @param {string[]} facets */
  const buildPanels = (rows, ix, cfg, out, facets) => {
    const { isLine, isBand, splitCol, facet, levels } = out;
    const { col: x, type: xType, order: xOrder } = out.x;
    const encodeX = /** @type {(v: any) => any} */ (out.x.encode);
    const y = out.y.col, yType = out.y.type;
    const encodeY = (/** @type {any} */ v) => yType === 'category' ? String(v ?? '') : Number(v);
    /** @param {any} r */
    const packPt = (r) => [encodeX(r[x]), encodeY(r[y]), ...out.ttFields.map((c) => r[c] ?? '')];
    const drawn = (/** @type {any} */ r) => {
      if (r[x] == null || r[y] == null) return false;
      if ((out.logX && !(Number(r[x]) > 0)) || (out.logY && !(Number(r[y]) > 0))) {
        out.logDropped++;
        return false;
      }
      return true;
    };
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
      // `src` keeps the rows in data order, for the error bars.
      return { level, color: clr, rows: srows, pts, src: rs };
    };
    const facetLabel = model.facetLabels(rows, cfg, out.nFacets);
    for (const fv of facets) {
      // Keyed 'raw': the panel of a missing facet value holds none.
      const prow = fv === '__all__' ? rows : ix.get(rows, /** @type {string} */ (facet), 'raw', fv);
      /** @type {IndSeries[]} */
      const series = [];
      if (!isBand) {
        if (!levels.length) {
          series.push(mkSeries(undefined, prow.filter(drawn), out.palette[0]));
        } else {
          // Keyed 'nz' like the levels: a null split value is the '' series.
          levels.forEach((lv, ci) => series.push(
            mkSeries(lv, ix.get(prow, splitCol, 'nz', lv).filter(drawn), out.levelColors[ci])));
        }
      }
      out.panels.push({
        facet: fv,
        label: facetLabel(fv),
        rows: prow, series
      });
    }
  };

  /** @param {{ rows: any[], ix: any, columns: VizColumn[], cfg: ChartSettings, derived: ChartDerived }} input
   * @returns {IndividualModel} */
  const individual = ({ rows, ix, columns, cfg, derived }) => {
    const ct = cfg.type;
    // gate() let only a chart with x and y through.
    const x = /** @type {string} */ (cfg.x), y = /** @type {string} */ (cfg.y);
    // A colour, facet or series column the data does not hold (a board adds
    // its Subgroup column only once a subgroup is picked) is left unset.
    const held = (/** @type {string | null | undefined} */ c) =>
      c && (!columns.length || columns.some((m) => m.name === c)) ? c : null;
    const color = held(cfg.color);
    const facet = held(cfg.facet);
    const seriesCol = held(cfg.series);
    // The series splits; without one, a series per colour level.
    const splitCol = seriesCol || color;
    const isLine = NS.isLineType(ct);
    const isBand = ct === 'band';
    /** @type {IndividualModel} */
    const out = {
      family: 'individual', empty: null, type: ct,
      isLine, isBand,
      x: { col: x, type: 'value', cats: null, order: null },
      y: { col: y, type: 'value', cats: null },
      seriesCol, color, splitCol, facet, levels: [], levelColors: [],
      palette: NS.paletteOf(derived), colorScale: NS.scaleFor(derived, color),
      seriesCount: 1, lineOpacity: 1, symbol: 'circle', capMessage: null, crowd: false,
      logX: false, logY: false, logDropped: 0,
      ttFields: [], legend: null, byColor: false,
      nFacets: 1, single: true, ix, panels: [], hl: null
    };
    if (!x || !y) {
      out.empty = model.emptyState('Select X and Y columns');
      return out;
    }

    // Tooltip fields, less x (the header), y (the value) and the split column.
    out.ttFields = model.tooltipFields(cfg, [x, y, splitCol].filter(Boolean).map(String));

    const facets = facet ? ix.levels(rows, facet, 'nz').slice().sort() : ['__all__'];
    out.nFacets = facets.length;
    out.single = facets.length === 1;

    /** @type {string[]} */
    let levels = splitCol
      ? NS.axes.orderLevels(ix.levels(rows, splitCol, 'nz'), NS.scaleFor(derived, splitCol),
                            columns, splitCol)
      : [];
    if (isLine && levels.length > TRAJ_HARD_CAP) {
      const what = splitCol ? NS.axes.axisTitle(columns, splitCol) : 'series';
      out.capMessage = `Showing first ${TRAJ_HARD_CAP} of ${levels.length} series` +
        ' (' + what + ') — filter upstream to narrow.';
      levels = levels.slice(0, TRAJ_HARD_CAP);
    }
    out.levels = levels;
    out.seriesCount = levels.length || 1;
    if (isLine) {
      if (out.seriesCount > TRAJ_REDUCED_MAX) { out.lineOpacity = 0.15; out.symbol = 'none'; }
      else if (out.seriesCount > TRAJ_FULL_MAX) { out.lineOpacity = 0.35; out.symbol = 'none'; }
    }
    // One line per entity: hover picks the nearest line.
    out.crowd = isLine && !!seriesCol && out.seriesCount > 1;
    const colours = seriesColours(rows, ix, out);
    out.levelColors = levels.map((lv, i) => colours.of(lv, i));
    legendOf(rows, ix, out, colours.legendColor);
    setAxes(rows, columns, cfg, out);
    buildPanels(rows, ix, cfg, out, facets);
    highlightSeries(out, rows, ix, cfg);
    return out;
  };

  /** The highlight on a scatter or line. A line is highlighted when one of its rows holds a
   * highlighted value, a point by its own row. What is not highlighted is `muted` (drawn grey by
   * the option) and comes first, so the highlighted marks draw over it: a scatter series splits
   * into its muted and its highlighted points. With a colour, the palette is counted over the
   * highlighted colour values only, and the legend lists only those. @param {IndividualModel} m
   * @param {any[]} rows @param {any} ix @param {Record<string, any>} cfg */
  const highlightSeries = (m, rows, ix, cfg) => {
    const hl = NS.highlightOf(cfg);
    if (!hl || m.isBand) return;
    m.hl = hl;
    const { color } = m;
    /** @param {any} r */
    const rowLit = (r) => NS.keys.rowMatches(r, hl);
    /** @param {IndSeries} s @param {any} facet */
    const lineLit = (s, facet) => NS.keys.markLit(individualKeys(m, facet, s.level), s.rows, hl);

    /** @type {Map<string, string>} */
    const colourOf = new Map();
    if (color) {
      // The colour values the highlighted marks carry.
      /** @type {Set<string>} */
      const lit = new Set();
      for (const p of m.panels) {
        for (const s of p.series) {
          const lr = m.isLine ? (lineLit(s, p.facet) ? s.rows : []) : s.rows.filter(rowLit);
          for (const r of lr) lit.add(String(r[color] ?? ''));
        }
      }
      const order = m.splitCol === color ? m.levels : ix.levels(rows, color, 'nz').slice().sort();
      const cols = NS.litLevelColors(order, lit, m.colorScale, m.palette);
      /** @type {{ name: string, color: string }[]} */
      const items = [];
      order.forEach((/** @type {string} */ lv, /** @type {number} */ i) => {
        const c = cols[i];
        if (c) { colourOf.set(lv, c); items.push({ name: lv, color: c }); }
      });
      m.legend = items.length && items.length <= LEGEND_MAX ? { col: color, items } : null;
    }
    /** @param {IndSeries} s @param {any[]} rs */
    const colourFor = (s, rs) => {
      if (!color) return s.color;
      const cv = rs.length ? String(rs[0][color] ?? '') : String(s.level ?? '');
      return colourOf.get(cv) || s.color;
    };

    for (const p of m.panels) {
      /** @type {IndSeries[]} */
      const back = [];
      /** @type {IndSeries[]} */
      const front = [];
      for (const s of p.series) {
        if (m.isLine) {
          if (lineLit(s, p.facet)) front.push({ ...s, color: colourFor(s, s.rows) });
          else back.push({ ...s, muted: true });
          continue;
        }
        const on = s.rows.map(rowLit);
        /** @param {boolean} want @returns {IndSeries} */
        const part = (want) => {
          const idx = on.map((v, i) => (v === want ? i : -1)).filter((i) => i >= 0);
          const rs = idx.map((i) => s.rows[i]);
          return { level: s.level, color: s.color, rows: rs,
                   pts: idx.map((i) => s.pts[i]), src: rs, muted: !want };
        };
        const mu = part(false), li = part(true);
        if (mu.rows.length) back.push(mu);
        if (li.rows.length) front.push({ ...li, color: colourFor(s, li.rows) });
      }
      p.series = back.concat(front);
    }
  };

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

  /** The colour of each series level, and of each legend chip. With a series column apart from the
   * colour column, a series takes the colour of its first row's colour value: the board scale's,
   * else the next palette colour in the order the values come up (cycling, so a level keeps its
   * colour under a filter). Otherwise a level takes the scale's colour or the palette's in level
   * order. @param {any[]} rows @param {any} ix @param {IndividualModel} m */
  const seriesColours = (rows, ix, m) => {
    const { palette, colorScale, color, seriesCol } = m;
    /** @param {string} v */
    const fromScale = (v) => (colorScale && colorScale.color && colorScale.color[v] != null
      ? colorScale.color[v] : null);
    /** @type {Map<string, string>} colour value -> its cycled colour */
    const lookup = new Map();
    /** @param {string} level @param {number} index */
    const of = (level, index) => {
      if (!color) return palette[0];
      if (seriesCol && seriesCol !== color) {
        const rep = ix.get(rows, seriesCol, 'nz', level)[0];
        const cv = rep ? String(rep[color] ?? '') : '';
        const scaled = fromScale(cv);
        if (scaled != null) return scaled;
        if (!lookup.has(cv)) lookup.set(cv, palette[lookup.size % palette.length]);
        return /** @type {string} */ (lookup.get(cv));
      }
      return fromScale(level) ?? palette[index % palette.length];
    };
    /** @param {string} level @param {number} index */
    const legendColor = (level, index) =>
      fromScale(level) ?? (lookup.get(level) || palette[index % palette.length]);
    return { of, legendColor };
  };

  /** The legend band's chips, when the colour column has a readable number of levels. With a series
   * column apart from the colour, the chips are the colour's values, sorted; the option adds an
   * empty series per chip for the legend to bind to. Fills m.legend and m.byColor.
   * @param {any[]} rows @param {any} ix @param {IndividualModel} m
   * @param {(level: string, index: number) => string} legendColor */
  const legendOf = (rows, ix, m, legendColor) => {
    const { color, seriesCol } = m;
    if (!color) return;
    const colorLevels = ix.levels(rows, color, 'nz');
    if (!colorLevels.length || colorLevels.length > LEGEND_MAX) return;
    m.byColor = !!seriesCol && seriesCol !== color;
    const items = m.byColor
      ? colorLevels.slice().sort().map((/** @type {string} */ lvl, /** @type {number} */ i) =>
        ({ name: lvl, color: legendColor(lvl, i) }))
      : m.levels.map((lv, i) => ({ name: lv, color: m.levelColors[i] }));
    m.legend = { col: color, items };
  };

  /** The keys of a line, a band level or a scatter series: the split column's level and the facet
   * in a facet panel. @param {IndividualModel} m @param {any} facetVal @param {any} level
   * @returns {Record<string, any>} */
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

  /** The rows under a clicked point: the panel's rows at its x and y, within its series when the
   * chart is split. @param {IndividualModel} m @param {IndividualPanel} panel
   * @param {any} seriesName @param {any[]} v the point's value */
  const pointRows = (m, panel, seriesName, v) => {
    const pool = (m.splitCol && seriesName != null)
      ? m.ix.get(panel.rows, m.splitCol, 'nz', String(seriesName)) : panel.rows;
    const xs = String(v[0]), ys = String(v[1]);
    const enc = m.x.encode || ((/** @type {any} */ x) => x);
    return pool.filter((/** @type {any} */ r) =>
      String(enc(r[m.x.col])) === xs && String(r[m.y.col]) === ys);
  };

  /**
   * What a click sends, or null.
   *
   *   - A band: its level; an uncoloured band is not clickable.
   *   - A split line: its level, the hovered one (`focusName`) before the
   *     one ECharts' hit test names.
   *   - A scatter point: its series when one is set, else the point itself,
   *     a zero-width range at its x and y. An unsplit line's point too.
   *
   * Auto sends the mark's keys (the facet with them). A drill column sends
   * its values in the rows under the mark, nothing when there are none.
   * @param {IndividualModel} m @param {Record<string, any>} cfg
   * @param {IndividualPanel | null} panel @param {any} params
   * @param {string} [focusName]
   * @returns {ChartSelection | null}
   */
  const individualClick = (m, cfg, panel, params, focusName) => {
    if (!panel || NS.drillState(cfg) === 'off') return null;
    const explicit = cfg.drill !== 'auto';
    /** @param {Record<string, any[]> | null} f @returns {ChartSelection | null} */
    const wrap = (f) => (f ? { type: 'categorical', filters: f } : null);

    /** @type {string | null} */
    let level = null;
    if (m.isBand) {
      if (!m.splitCol || params.seriesName == null) return null;
      level = String(params.seriesName);
    } else if (m.isLine && m.splitCol) {
      const name = focusName !== undefined ? focusName : params.seriesName;
      if (name != null) level = String(name);
    }
    if (level != null) {
      return wrap(explicit
        ? NS.keys.fromRows(cfg.drill, m.ix.get(panel.rows, m.splitCol, 'nz', level))
        : NS.keys.fromKeys(individualKeys(m, panel.facet, level)));
    }
    const v = params.value;
    if (!Array.isArray(v) || v.length < 2) return null;
    if (explicit) return wrap(NS.keys.fromRows(cfg.drill, pointRows(m, panel, params.seriesName, v)));
    if (m.seriesCol && !m.isLine && params.seriesName != null) {
      return wrap(NS.keys.fromKeys(individualKeys(m, panel.facet, params.seriesName)));
    }
    return inPanel(m, panel, m.x.col, m.y.col, [v[0], v[0]], [v[1], v[1]]);
  };

  /**
   * A range taken in a panel; in a facet panel it carries the panel's key
   * as `filters`.
   * @param {IndividualModel} m @param {IndividualPanel | null} panel
   * @param {string} x @param {string | null} y @param {any[]} xRange
   * @param {any[] | null} yRange @returns {ChartRangeSelection}
   */
  const inPanel = (m, panel, x, y, xRange, yRange) => {
    const keys = panel ? individualKeys(m, panel.facet, null) : {};
    return NS.keys.range(x, y, xRange, yRange,
                         Object.keys(keys).length ? NS.keys.fromKeys(keys) : null);
  };

  /**
   * What a brush caught: the x and y of every brushed point (a dataIndex is
   * its own series' index; one outside the series is skipped); with a drill
   * column, the panel's rows at those points, each (x, y) taken once; with a
   * series column, the series of the brushed points.
   * @param {IndividualModel} m @param {Record<string, any>} cfg
   * @param {IndividualPanel | null} panel @param {any[]} allSeries chart.getOption().series
   * @param {Array<{ seriesIndex: number, dataIndex: number }>} selected
   * @returns {{ xVals: any[], yVals: any[], rows: any[], drill: string | null,
   *             levels: string[] }}
   */
  const individualBrush = (m, cfg, panel, allSeries, selected) => {
    // Colour never widens a brush: only a drill column reads the rows.
    const drill = cfg.drill && cfg.drill !== 'auto' ? String(cfg.drill) : null;
    /** @type {Set<string>} */
    const levels = new Set();
    const encX = m.x.encode || ((/** @type {any} */ x) => x);
    /** @param {any} x @param {any} y */
    const ptKey = (x, y) => String(x) + '|||' + String(y);
    // The panel's rows by (x, y), for a drill column.
    /** @type {Map<string, any[]>} */
    const rowIndex = new Map();
    if (drill && panel) {
      for (const r of panel.rows) {
        const k = ptKey(encX(r[cfg.x]), r[cfg.y]);
        const b = rowIndex.get(k);
        if (b) b.push(r); else rowIndex.set(k, [r]);
      }
    }
    /** @type {any[]} */
    const xVals = [], yVals = [];
    /** @type {Set<string>} */
    const taken = new Set();
    for (const sel of selected) {
      const drawn = allSeries[sel.seriesIndex];
      // A legend chip or a label series is no point of the data.
      if (drawn && typeof drawn.id === 'string' && /^__(legend|label)_/.test(drawn.id)) continue;
      const sData = drawn && drawn.data;
      if (!sData || sel.dataIndex < 0 || sel.dataIndex >= sData.length) continue;
      const pt = sData[sel.dataIndex];
      if (!pt) continue;
      const vx = Array.isArray(pt) ? pt[0] : pt.value && pt.value[0];
      const vy = Array.isArray(pt) ? pt[1] : pt.value && pt.value[1];
      if (vx != null) xVals.push(vx);
      if (vy != null) yVals.push(vy);
      if (m.seriesCol) levels.add(String(drawn.name ?? ''));
      taken.add(ptKey(vx, vy));
    }
    // Each (x, y) once.
    /** @type {any[]} */
    const rows = [];
    for (const k of taken) for (const r of rowIndex.get(k) || []) rows.push(r);
    return { xVals, yVals, rows, drill, levels: [...levels] };
  };

  /**
   * What a brush sends, from what it caught, or null: a drill column's
   * values in the rows at the points; else the series of the brushed points;
   * else the points' x (and, off a line, y) extent. In a facet panel the
   * series and the range carry the panel's key.
   * @param {IndividualModel} m @param {Record<string, any>} cfg
   * @param {IndividualPanel | null} panel
   * @param {{ xVals: any[], yVals: any[], rows: any[], drill: string | null,
   *           levels: string[] }} b
   * @returns {ChartSelection | null}
   */
  const brushSelection = (m, cfg, panel, b) => {
    if (b.drill) {
      const f = NS.keys.fromRows(b.drill, b.rows);
      if (f) return { type: 'categorical', filters: f };
    }
    const keys = panel ? individualKeys(m, panel.facet, null) : {};
    if (!b.drill && m.seriesCol && b.levels.length) {
      return { type: 'categorical',
               filters: { [m.seriesCol]: b.levels.map((lv) => (NS.keys.isMissing(lv) ? null : lv)),
                          ...NS.keys.fromKeys(keys) } };
    }
    if (!b.xVals.length) return null;
    return inPanel(m, panel, cfg.x, cfg.y, [NS.minOf(b.xVals), NS.maxOf(b.xVals)],
                   (!m.isLine && b.yVals.length) ? [NS.minOf(b.yVals), NS.maxOf(b.yVals)] : null);
  };

  /**
   * y of a line at `x`, interpolated between its points, or null outside
   * its range. On a category x the points are compared by axis position
   * (`xOrder`).
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
                         brushSelection, panelOf, interpYAtX });
})();
