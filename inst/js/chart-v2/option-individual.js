// @ts-check
/**
 * Chart block v2: the individual option (scatter, line, band).
 *
 * The model and how it should look in, one ECharts option per panel out.
 * The series come from option-points.js (scatter, line) or option-band.js;
 * this file adds what every panel shares: helper lines, the identity line
 * and the band's reference limits on one markLine, the empty series the
 * legend binds to, the axes, the tooltip, and the brush or zoom toolbox.
 *
 * A panel result also carries the state the view hands its slot: the
 * hover tracker the line tooltip reads, the line picker's registry
 * (`focus`), the band picker's ribbons (`band`), and whether the panel
 * is brushed or zoomed.
 *
 * Beside it: the legend band's chips and the highlight patch (D5).
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});
  const option = /** @type {any} */ (NS.option = NS.option || {});
  const parts = /** @type {any} */ (option.indParts = option.indParts || {});

  // Config fields the option reads beside the model's.
  const OPTION_FIELDS = ['chart_type', 'drill', 'dot_size_mult', 'line_width_mult', 'connect',
    'smoother', 'smoother_series', 'lo', 'hi', 'x_lines', 'value_lines', 'identity_line',
    'band_series', 'band_refs', 'box_points'];

  // The canvas height, before turned x labels add their gutter.
  const PANEL_H = 400;
  // The grid's left and right margins, which the x labels do not get.
  const X_INSET = 71;
  // The opacity of a mark a filter does not light, as a share of its own.
  const DIM = 0.15;

  // A scatter's brush icons.
  /** @param {any} ink */
  const brushToolbox = (ink) => ({
    show: true, right: 8, top: 4, itemSize: 11,
    feature: { brush: { type: ['rect', 'lineX', 'clear'] } },
    iconStyle: { borderColor: ink.muted }
  });

  // A line chart's drag-to-zoom on x. The toolbox stays visible: ECharts
  // builds the drag controller only for a rendered toolbox.
  /** @param {any} ink */
  const zoomToolbox = (ink) => ({
    show: true, right: 8, top: 4, itemSize: 11,
    feature: {
      dataZoom: {
        // The data x axis only, not the veil's hidden one.
        xAxisIndex: 0, yAxisIndex: false, filterMode: 'filter',
        title: { zoom: 'Zoom to range', back: 'Reset zoom' }
      }
    },
    iconStyle: { borderColor: ink.muted }
  });

  /**
   * A domain rounded outward to a nice step: an explicit min/max turns
   * off ECharts' own rounding.
   * @param {number} lo @param {number} hi
   */
  const niceDomain = (lo, hi) => {
    const span = hi - lo;
    const mag = Math.pow(10, Math.floor(Math.log10(span)));
    const rel = span / mag;
    const step = rel >= 5 ? mag : (rel >= 2 ? mag / 2 : mag / 5);
    return [Math.floor(lo / step) * step, Math.ceil(hi / step) * step];
  };

  /**
   * The identity line (y = x) over one domain shared by both axes, so the
   * diagonal runs corner to corner. Null without two numeric ranges.
   * @param {any[]} series
   */
  const identityDomain = (series) => {
    let xLo = Infinity, xHi = -Infinity, yLo = Infinity, yHi = -Infinity;
    for (const s of series) {
      if (s.type !== 'scatter' || !Array.isArray(s.data)) continue;
      for (const p of s.data) {
        const px = Number(p[0]), py = Number(p[1]);
        if (Number.isFinite(px)) { if (px < xLo) xLo = px; if (px > xHi) xHi = px; }
        if (Number.isFinite(py)) { if (py < yLo) yLo = py; if (py > yHi) yHi = py; }
      }
    }
    const rLo = Math.min(xLo, yLo), rHi = Math.max(xHi, yHi);
    if (!(Number.isFinite(rLo) && Number.isFinite(rHi) && rLo < rHi)) return null;
    return niceDomain(rLo, rHi);
  };

  /**
   * One panel's option and the state its slot gets.
   * @param {any} m @param {any} panel @param {any} look @param {number} width
   */
  const panelOption = (m, panel, look, width) => {
    const { cfg, ink, measure, theme } = look;
    const title = (/** @type {any} */ col) => NS.axes.axisTitle(look.columns, col);
    const ax = { labelColor: ink.muted, fontSize: ink.fontSize, splitLineColor: ink.border };
    const lm = cfg.line_width_mult ?? 1.0;
    const isLine = m.isLine, isBand = m.isBand;
    const xCats = m.x.cats;
    // Categorical x: every label, flat while it fits its slot, else turned
    // and thinned (an ordered axis). Measured before the height: turned
    // labels grow the canvas by their gutter.
    const xlab = xCats ? NS.axes.xAxisLabels(xCats, width - X_INSET, true, look.measure, ink) : null;

    /** @type {any[]} */
    let series;
    let mlTarget = 0;
    let bandMany = false;
    /** @type {any} */
    let band = null;
    /** @type {any} */
    let pts = null;
    if (isBand) {
      const b = parts.band(m, panel, look);
      series = b.series;
      band = b.band;
      bandMany = b.many;
      if (b.markLineIdx >= 0) mlTarget = b.markLineIdx;
    } else {
      pts = parts.points(m, panel, look);
      series = pts.series;
    }

    const ext = (isBand && m.y.type === 'value' && series.length)
      ? parts.bandExtent(cfg, panel, bandMany) : null;

    // Helper lines, the band's limits and the identity line share one
    // markLine (ECharts allows one per series).
    /** @type {any[]} */
    const refData = [];
    const refX = Array.isArray(cfg.x_lines) ? cfg.x_lines : [];
    const refY = Array.isArray(cfg.value_lines) ? cfg.value_lines : [];
    for (const v of refX) refData.push(NS.axes.guideLine(Number(v), true, lm, undefined, false, ink));
    for (const v of refY) refData.push(NS.axes.guideLine(Number(v), false, lm, undefined, false, ink));
    if (isBand) for (const r of parts.bandRefLines(cfg, ext, ink)) refData.push(r);
    const idRaw = cfg.identity_line;
    const identityOn = (idRaw === 'on' || idRaw === true) && !isLine &&
      m.x.type !== 'category' && m.y.type !== 'category';
    const idDom = identityOn ? identityDomain(series) : null;
    if (idDom) {
      refData.push([
        { coord: [idDom[0], idDom[0]],
          lineStyle: { color: ink.muted, type: 'dashed', width: 1.5 * lm } },
        { coord: [idDom[1], idDom[1]] }
      ]);
    }
    if (series.length > 0 && refData.length) {
      series[series[mlTarget] ? mlTarget : 0].markLine = {
        silent: true, symbol: 'none',
        lineStyle: { color: ink.danger, type: 'dashed', width: 1.5 * lm },
        label: { show: false },
        data: refData
      };
    }

    // With a series column apart from the colour, the chips are colour
    // values no series is named after: an empty series per chip gives the
    // legend something to bind to.
    if (m.byColor && m.legend) {
      for (const it of m.legend.items) {
        series.push({
          type: isLine ? 'line' : 'scatter', name: it.name, data: [],
          itemStyle: { color: it.color }, lineStyle: { color: it.color },
          showSymbol: false, silent: true
        });
      }
    }

    /** @type {any} */
    const xAxis = {
      type: m.x.type,
      name: title(m.x.col),
      nameLocation: 'middle',
      nameGap: xlab && xlab.bottom ? xlab.bottom + 16 : 28,
      nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize },
      axisLabel: xlab ? xlab.axisLabel : { color: ax.labelColor, fontSize: ax.fontSize },
      axisLine: { lineStyle: { color: ink.strong } },
      splitLine: { lineStyle: { color: ax.splitLineColor, type: 'dashed' } },
      scale: true
    };
    if (xCats) xAxis.data = xCats;
    if (idDom && m.x.type === 'value') { xAxis.min = idDom[0]; xAxis.max = idDom[1]; }
    /** @type {any} */
    const yAxis = {
      type: m.y.type,
      name: title(m.y.col),
      nameLocation: 'middle',
      nameGap: 46,
      nameRotate: 90,
      nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize },
      axisLabel: { color: ax.labelColor, fontSize: ax.fontSize },
      axisLine: { lineStyle: { color: ink.strong } },
      splitLine: { lineStyle: { color: ax.splitLineColor, type: 'dashed' } },
      scale: true
    };
    if (m.y.cats) yAxis.data = m.y.cats;
    if (idDom && m.y.type === 'value') { yAxis.min = idDom[0]; yAxis.max = idDom[1]; }
    if (ext) { yAxis.min = ext.lo; yAxis.max = ext.hi; }
    // The veil's own 0..1 axes, in the same grid (line charts only).
    const scrimAxis = { type: 'value', min: 0, max: 1, show: false, axisPointer: { show: false } };

    // The brush needs continuous x; a line and a band are clicked, not
    // brushed, and so is a scatter with series (a brush would race the
    // click on a point).
    const brushable = m.x.type !== 'category' && !isLine && !isBand && !m.seriesCol;
    const zoomable = isLine;

    const hover = { si: null };
    /** @type {{ veil?: string }} */
    const scrimState = {};
    if (isLine) for (const s of parts.lineOverlay(scrimState)) series.push(s);
    // The line picker's registry: only for a crowd, a lone line has nothing
    // to pick out.
    const focus = (isLine && m.levels.length > 1) ? Object.assign(scrimState, {
      series, stepMode: pts.stepMode || null, smoothOn: pts.smoothOn,
      xAxisType: m.x.type, xOrder: m.x.order, markerPx: pts.markerPx, focusW: pts.focusW
    }) : null;

    const opt = {
      ...(theme ? {} : { backgroundColor: 'transparent' }),
      textStyle: { fontFamily: ink.face },
      tooltip: isLine ? parts.lineTooltip(m, look, hover) : parts.pointTooltip(m, look),
      // A hidden legend: its selection model is what the chips toggle.
      legend: { show: false },
      grid: { left: 66, right: 5, top: 30, bottom: 52 + (xlab ? xlab.bottom : 0) },
      xAxis: isLine ? [xAxis, scrimAxis] : xAxis,
      yAxis: isLine ? [yAxis, scrimAxis] : yAxis,
      toolbox: brushable ? brushToolbox(ink) : (zoomable ? zoomToolbox(ink) : undefined),
      brush: brushable ? {
        toolbox: ['rect', 'lineX', 'clear'], xAxisIndex: 0, yAxisIndex: 0,
        brushStyle: { color: 'rgba(0, 114, 178, 0.1)', borderColor: 'rgba(0, 114, 178, 0.5)',
                      borderWidth: 1 },
        throttleDelay: 300
      } : undefined,
      series
    };
    const panelH = PANEL_H + (xlab ? xlab.bottom : 0);
    return {
      option: NS.cardTooltip(NS.applyDrillEmphasis(opt, cfg), ink),
      height: panelH + 'px',
      panelH,
      xFit: xlab ? { labels: xCats, inset: X_INSET, decimate: true, nameGap: true,
                     gutter: xlab.bottom, key: xlab.key } : null,
      hover, focus, band, brushable, zoomable
    };
  };

  /**
   * @param {any} m the individual model
   * @param {{ cfg: Record<string, any>, columns: VizColumn[], ink: any, theme: any,
   *           measure: (s: string) => number, widths?: number[] }} look
   */
  const individual = (m, look) => ({
    panels: m.panels.map((/** @type {any} */ panel, /** @type {number} */ i) =>
      panelOption(m, panel, look, (look.widths || [])[i] || 0)),
    // The trajectory cap, in the footer.
    labelNote: m.capMessage
  });
  individual.FIELDS = OPTION_FIELDS;

  /** The legend band's chips. @param {any} m */
  const individualLegend = (m) => (m.legend && m.legend.items.length ? m.legend : null);

  // -- Highlight (D5) ----------------------------------------------------------

  /** Is a series one of the panel's marks, not an overlay? @param {any} s */
  const isMark = (s) => s && s.id == null && !s.silent &&
    (s.type === 'scatter' || s.type === 'line');

  /**
   * The highlight patch for one panel, from the series the panel was drawn
   * with (the option's own, so an unnamed series is still unnamed): a line,
   * a band level or a scatter series is lit when its keys match the
   * filter; on a filter of a column a scatter series does not carry, each
   * point by its own row. A range filter (D11) lights the scatter points
   * inside it, and the lines and band levels with a point inside it; its
   * facet key, in `filters`, lights the panel. Overlays follow their
   * series. Without a filter, and nothing dimmed from before, the patch is
   * empty, as v1's.
   * @param {any} m @param {any} panel @param {any[]} drawn the panel option's series
   * @param {((keys: Record<string, any>, rows: any) => boolean) | null} lit
   * @param {boolean} wasDimmed
   * @param {Record<string, any[]> | null} filters
   * @param {{ x_range: any[], y_range: any[] | null } | null} [range]
   * @returns {{ series: any[], dimmed: boolean }}
   */
  const individualPatch = (m, panel, drawn, lit, wasDimmed, filters, range) => {
    if (!lit && !range && !wasDimmed) return { series: drawn.map(() => ({})), dimmed: false };
    const inRange = range ? (/** @type {any[]} */ p) => NS.keys.pointInRange(p, range) : null;
    const NONE = '\u0000';
    const keyOf = (/** @type {any} */ name) => (name == null ? NONE : String(name));
    /** @type {Map<string, any>} */
    const byLevel = new Map();
    for (const s of panel.series) byLevel.set(keyOf(s.level), s);
    const keyCols = new Set(m.splitCol ? [m.splitCol] : []);
    if (m.facet) keyCols.add(m.facet);
    const byRow = !!lit && !!filters && Object.keys(filters).some((c) => !keyCols.has(c));
    const perPoint = (byRow || !!inRange) && !m.isLine && !m.isBand;
    /** @type {Map<string, boolean>} */
    const levelLit = new Map();
    /** @param {any} name */
    const onLevel = (name) => {
      if (!lit && !inRange) return true;
      const k = keyOf(name);
      if (!levelLit.has(k)) {
        const ms = byLevel.get(k);
        const keys = NS.model.individualKeys(m, panel.facet, name);
        const rows = () => (ms ? ms.rows
          : (m.splitCol && name != null ? m.ix.get(panel.rows, m.splitCol, 'nz', String(name))
            : panel.rows));
        let on = lit ? lit(keys, rows) : true;
        // A line or a band level is lit when one of its points is in range.
        if (on && inRange && ms) on = ms.pts.some(inRange);
        levelLit.set(k, on);
      }
      return /** @type {boolean} */ (levelLit.get(k));
    };
    const series = drawn.map((/** @type {any} */ s) => {
      if (!s || s.id != null) return {};
      // The legend's empty series.
      if (s.silent && s.type !== 'custom' && Array.isArray(s.data) && !s.data.length) return {};
      const ms = byLevel.get(keyOf(s.name));
      if (isMark(s) && s.type === 'scatter' && ms) {
        if (perPoint) {
          let any = false;
          // The panel's key decides before the points do.
          const keyOn = byRow || !lit || lit(NS.model.individualKeys(m, panel.facet, s.name), () => ms.rows);
          const data = ms.pts.map((/** @type {any} */ p, /** @type {number} */ j) => {
            const on = keyOn && (!byRow || NS.keys.rowMatches(ms.rows[j], filters)) &&
              (!inRange || inRange(p));
            if (on) any = true;
            return { value: p, itemStyle: { opacity: on ? 1 : DIM } };
          });
          levelLit.set(keyOf(s.name), any);
          return { data, itemStyle: { opacity: 1 } };
        }
        // The plain points again, in case a filter dimmed them one by one.
        return { data: ms.pts, itemStyle: { opacity: onLevel(s.name) ? 1 : DIM } };
      }
      const on = onLevel(s.name);
      if (isMark(s) && s.type === 'line') {
        const base = m.isBand ? 1 : m.lineOpacity;
        return { lineStyle: { opacity: on ? base : base * DIM },
                 itemStyle: { opacity: on ? 1 : DIM } };
      }
      if (s.type === 'line') {
        // A smoother, or a band's halo.
        const base = m.isBand ? 1 : 0.9;
        return { lineStyle: { opacity: on ? base : base * DIM } };
      }
      return { itemStyle: { opacity: on ? 1 : DIM } };
    });
    return { series, dimmed: !!lit || !!inRange };
  };

  Object.assign(option, { individual, individualLegend, individualPatch });
})();
