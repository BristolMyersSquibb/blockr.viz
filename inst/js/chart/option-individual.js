// @ts-check
/**
 * Chart block: the individual option (scatter, line, band), one ECharts
 * option per panel. The series come from option-points.js or option-band.js;
 * this file adds the helper lines, the identity line and the band's limits
 * on one markLine, the legend's empty series, the axes, the tooltip and the
 * brush or zoom toolbox. A panel result also carries the state its slot
 * gets: the hover tracker, the line picker's registry (`focus`), the band
 * picker's ribbons (`band`) and whether it is brushed or zoomed. Beside it:
 * the legend chips and the highlight patch.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});
  const option = /** @type {any} */ (NS.option = NS.option || {});
  const parts = /** @type {any} */ (option.indParts = option.indParts || {});

  // Config fields the option reads beside the model's.
  const OPTION_FIELDS = ['type', 'drill', 'point_size', 'line_width', 'connect', 'smoother',
    'derived.smoother', 'ymin', 'ymax', 'x_lines', 'y_lines', 'identity_line', 'derived.band',
    'box_points', 'linetype', 'x_ranges', 'legend', 'label', 'format', 'unit', 'position',
    'derived.titles', 'y_transform', 'x_transform', 'facet_scales'];

  // The canvas height, before turned x labels add their gutter.
  const PANEL_H = 400;
  // The grid's left and right margins, which the x labels do not get.
  const X_INSET = 71;
  // The opacity of a mark a filter does not light, as a share of its own.
  const DIM = 0.15;

  // ECharts builds the brush and zoom drag controllers only for a rendered
  // toolbox, so one is rendered with no size, outside the canvas.
  /** @param {any} feature */
  const hiddenToolbox = (feature) =>
    ({ show: true, itemSize: 0, itemGap: 0, right: -40, top: -40, feature });
  const BRUSH_TOOLS = ['rect', 'lineX', 'clear'];
  const brushToolbox = () => hiddenToolbox({ brush: { type: BRUSH_TOOLS } });
  // Drag-to-zoom on the data x axis only, not the veil's hidden one.
  const zoomToolbox = () => hiddenToolbox({
    dataZoom: { xAxisIndex: 0, yAxisIndex: false, filterMode: 'filter' }
  });

  const { identityDomain, pos, logDomains, clampBand } = parts.log;

  /** The settings with the band's series clamped above a log axis's floor.
   * @param {Record<string, any>} cfg @param {number} floor */
  const clampedBand = (cfg, floor) => {
    const band = cfg.derived.band || {};
    return { ...cfg, derived: { ...cfg.derived,
                                band: { ...band, series: clampBand(band.series, floor) } } };
  };

  /** What every part of a panel's option reads: config, colours, the panel's
   * log domains (null on a linear axis) and the measured x labels.
   * @param {any} m @param {any} look @param {number} width */
  const panelContext = (m, look, width) => {
    const { cfg, ink, theme } = look;
    const lx = (look.logDom && look.logDom.x) || null;
    const ly = (look.logDom && look.logDom.y) || null;
    const title = (/** @type {any} */ col) => NS.axes.axisTitle(look.columns, col);
    const ax = { labelColor: ink.muted, fontSize: ink.fontSize, splitLineColor: ink.border };
    const xCats = m.x.cats;
    // Measured before the height: turned labels grow the canvas.
    const xlab = xCats ? NS.axes.xAxisLabels(xCats, width - X_INSET, true, look.measure, ink) : null;
    return { m, look, width, cfg, ink, theme, lx, ly, title, ax, lm: cfg.line_width, xCats, xlab,
             isLine: m.isLine, isBand: m.isBand, isTime: m.x.type === 'time' };
  };

  /** The panel's series from the band or the points part, with what the option
   * needs beside them. @param {any} k @param {any} panel */
  const panelSeries = (k, panel) => {
    const { m, look, cfg, lx, ly, width } = k;
    if (k.isBand) {
      const b = parts.band(m, panel, ly ? { ...look, cfg: clampedBand(cfg, ly.min) } : look);
      return { series: b.series, mlTarget: Math.max(b.markLineIdx, 0), bandMany: b.many,
               band: b.band, pts: null, labelPad: b.labelPad };
    }
    const pts = parts.points(m, panel, (lx || ly)
      ? { ...look, logFloorX: lx ? lx.min : null, logFloorY: ly ? ly.min : null } : look,
      width > X_INSET ? width - X_INSET : 0);
    return { series: pts.series, mlTarget: 0, bandMany: false, band: null, pts,
             labelPad: pts.labelPad };
  };

  /** The helper lines, the band's limits and the identity line for the shared
   * markLine, with the domain both axes then share (`idDom`, null without
   * the identity line). @param {any} k @param {any} ext @param {any[]} series */
  const panelRefData = (k, ext, series) => {
    const { m, look, cfg, ink, lx, ly, lm, isBand, isLine } = k;
    const refData = [];
    // An unnamed line at 0 needs no label: the axis prints its value.
    /** @param {any} v @param {any} nm */
    const refText = (v, nm) => nm || (Number(v) === 0 ? null : undefined);
    NS.labelled(cfg.x_lines).forEach(({ value: v, label: nm }) => {
      if (lx && !pos(v)) return;
      if (m.x.type === 'category') {
        const c = NS.axes.catOf(v, m.x.cats || []);
        if (c == null) return;
        const text = nm || (c === String(v) ? null : String(v));
        refData.push(NS.axes.guideLine(c, true, lm, text, false, ink));
        return;
      }
      const at = m.x.encode ? m.x.encode(v) : Number(v);
      if (!Number.isFinite(at)) return;
      const text = m.x.type === 'time'
        ? (nm || NS.axes.fmtXVal(at, 'time', null, m.x.period === 'time' ? 'time' : null))
        : refText(v, nm);
      refData.push(NS.axes.guideLine(at, true, lm, text, false, ink));
    });
    NS.labelled(cfg.y_lines).forEach(({ value: v, label: nm }) => {
      if (ly && !pos(v)) return;
      refData.push(NS.axes.guideLine(Number(v), false, lm, refText(v, nm), false, ink));
    });
    if (isBand) for (const r of parts.bandRefLines(cfg, ext, ink, look.columns)) refData.push(r);
    // y = x is a diagonal only when both axes use the same scale.
    const identityOn = cfg.identity_line === true && !isLine &&
      m.x.type !== 'category' && m.y.type !== 'category' && !lx === !ly;
    /** @type {any} */
    let idDom = null;
    /** @type {any} */
    let idLog = null;
    if (identityOn && lx && ly) {
      idLog = NS.axes.logTicks(Math.min(lx.min, ly.min), Math.max(lx.max, ly.max));
      if (idLog) idDom = [idLog.min, idLog.max];
    } else if (identityOn) {
      idDom = identityDomain(series);
    }
    if (idDom) {
      refData.push([
        { coord: [idDom[0], idDom[0]],
          lineStyle: { color: ink.muted, type: 'dashed', width: 1.5 * lm } },
        { coord: [idDom[1], idDom[1]] }
      ]);
    }
    return { refData, idDom, idLog };
  };

  /**
   * With a series column apart from the colour, the chips are colour values
   * no series is named after: an empty series per chip for the legend to bind.
   * @param {any} k @param {any[]} series
   */
  const addChipSeries = (k, series) => {
    const { m, isLine } = k;
    if (!(m.byColor && m.legend)) return;
    for (const it of m.legend.items) {
      series.push({
        type: isLine ? 'line' : 'scatter', name: it.name, data: [],
        itemStyle: { color: it.color }, lineStyle: { color: it.color },
        showSymbol: false, silent: true
      });
    }
  };

  /** @param {any} k @param {any} ext @param {any} idDom @param {any} idLog */
  const panelAxes = (k, ext, idDom, idLog) => {
    const { m, cfg, ink, ax, lx, ly, xCats, xlab, isLine, isTime } = k;
    // A line chart reads values off the y grid and dates off the crosshair,
    // so it has no x grid and no y axis line.
    const grid = { lineStyle: { color: ax.splitLineColor, type: 'solid', width: 1 } };
    /** @type {any} */
    const xAxis = {
      type: m.x.type,
      name: NS.axisName(k.title(m.x.col), m.x.col, cfg, { unit: false }),
      nameLocation: 'middle',
      nameGap: xlab && xlab.bottom ? xlab.bottom + 16 : 28,
      nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize },
      axisLabel: xlab ? xlab.axisLabel : {
        color: ax.labelColor, fontSize: ax.fontSize, hideOverlap: true,
        // A date axis labels years, then months, then days as it zooms in.
        ...(isTime ? { formatter: { year: '{yyyy}', month: '{MMM}', day: '{d} {MMM}',
                                    hour: '{HH}:{mm}', minute: '{HH}:{mm}' } } : {})
      },
      axisTick: { show: false },
      axisLine: { lineStyle: { color: ink.strong } },
      splitLine: isLine ? { show: false } : grid,
      scale: true
    };
    if (xCats) xAxis.data = xCats;
    if (idDom && m.x.type === 'value') { xAxis.min = idDom[0]; xAxis.max = idDom[1]; }
    if (lx) NS.axes.logAxis(xAxis, idLog || lx);
    const yFormat = m.y.type === 'value' && NS.valueFormat(cfg, { unit: false });
    /** @type {any} */
    const yAxis = {
      type: m.y.type,
      name: NS.axisName(k.title(m.y.col), m.y.col, cfg),
      nameLocation: 'middle',
      nameGap: 46,
      nameRotate: 90,
      nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize },
      axisLabel: { color: ax.labelColor, fontSize: ax.fontSize,
                   ...(yFormat ? { formatter: yFormat } : {}) },
      axisTick: { show: false },
      axisLine: isLine ? { show: false } : { lineStyle: { color: ink.strong } },
      splitLine: grid,
      scale: true
    };
    if (m.y.cats) yAxis.data = m.y.cats;
    if (idDom && m.y.type === 'value') { yAxis.min = idDom[0]; yAxis.max = idDom[1]; }
    if (ext) { yAxis.min = ext.lo; yAxis.max = ext.hi; }
    if (cfg.type === 'area' && m.y.type === 'value' && yAxis.min == null) {
      yAxis.scale = false;
      yAxis.min = (/** @type {any} */ v) => Math.min(0, v.min);
    }
    if (ly) NS.axes.logAxis(yAxis, idLog || ly);
    const scrimAxis = { type: 'value', min: 0, max: 1, show: false, axisPointer: { show: false } };
    return { xAxis, yAxis, scrimAxis };
  };

  /**
   * One panel's option and the state its slot gets.
   * @param {any} m @param {any} panel @param {any} look @param {number} width
   */
  const panelOption = (m, panel, look, width) => {
    const k = panelContext(m, look, width);
    const { cfg, ink, theme, ly, isLine, isBand, isTime, xlab, xCats } = k;
    const { series, mlTarget, bandMany, band, pts, labelPad } = panelSeries(k, panel);

    // On a log axis the band's frame is the log domain.
    const ext = (isBand && m.y.type === 'value' && series.length)
      ? (ly ? { lo: ly.min, hi: ly.max } : parts.bandExtent(cfg, panel, bandMany)) : null;

    // One markLine per series is all ECharts allows, so they share one.
    const { refData, idDom, idLog } = panelRefData(k, ext, series);
    if (series.length > 0 && refData.length) {
      series[series[mlTarget] ? mlTarget : 0].markLine = {
        silent: true, symbol: 'none',
        lineStyle: { color: ink.danger, type: 'dashed', width: 1.5 * k.lm },
        label: { show: false },
        data: refData
      };
    }
    addChipSeries(k, series);

    const { xAxis, yAxis, scrimAxis } = panelAxes(k, ext, idDom, idLog);

    // A line, a band and a scatter with series are clicked, not brushed (a
    // brush would race the click on a point).
    const brushable = m.x.type !== 'category' && !isLine && !isBand && !m.seriesCol;
    const hover = { si: null };
    /** @type {{ veil?: string }} */
    const scrimState = {};
    if (isLine) for (const s of parts.lineOverlay(scrimState)) series.push(s);
    // The line picker's registry, only for a crowd of lines. A few lines are
    // read off the crosshair's tooltip instead.
    const focus = (isLine && m.crowd) ? Object.assign(scrimState, {
      series, stepMode: pts.stepMode || null, smoothOn: pts.smoothOn,
      xAxisType: m.x.type, xOrder: m.x.order, markerPx: pts.markerPx, focusW: pts.focusW,
      yLog: !!ly
    }) : null;

    const opt = {
      ...(theme ? {} : { backgroundColor: 'transparent' }),
      // Dates are placed and labelled in UTC: R writes them without a zone.
      ...(isTime ? { useUTC: true } : {}),
      textStyle: { fontFamily: ink.face },
      // Only the first draw animates: a merge-patch morphs lines while the
      // dots jump to the new layout.
      animationDurationUpdate: 0,
      tooltip: isLine ? parts.lineTooltip(m, look, hover) : parts.pointTooltip(m, look),
      // Hidden: its selection model is what the chips toggle.
      legend: { show: false },
      grid: { left: 66, right: 5 + labelPad, top: 30,
              bottom: 52 + (xlab ? xlab.bottom : 0) },
      xAxis: isLine ? [xAxis, scrimAxis] : xAxis,
      yAxis: isLine ? [yAxis, scrimAxis] : yAxis,
      toolbox: brushable ? brushToolbox() : (isLine ? zoomToolbox() : undefined),
      brush: brushable ? {
        toolbox: BRUSH_TOOLS, xAxisIndex: 0, yAxisIndex: 0,
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
      hover, focus, band, brushable, zoomable: isLine
    };
  };

  /**
   * @param {any} m the individual model
   * @param {{ cfg: Record<string, any>, columns: VizColumn[], ink: any, theme: any,
   *           measure: (s: string) => number, widths?: number[] }} look
   */
  const individual = (m, look) => {
    const logs = (m.logX || m.logY) ? logDomains(m, look.cfg) : null;
    const dropped = (m.logDropped || 0) + (logs ? logs.dropped : 0);
    const logNote = dropped
      ? `Log scale: ${dropped} value${dropped === 1 ? '' : 's'} at or below zero not shown.` : null;
    return {
      panels: m.panels.map((/** @type {any} */ panel, /** @type {number} */ i) =>
        panelOption(m, panel, logs ? { ...look, logDom: { x: logs.x[i], y: logs.y[i] } } : look,
                    (look.widths || [])[i] || 0)),
      labelNote: [m.capMessage, logNote].filter(Boolean).join(' ') || null
    };
  };
  individual.FIELDS = OPTION_FIELDS;

  /** The legend band's chips. @param {any} m */
  const individualLegend = (m) => (m.legend && m.legend.items.length ? m.legend : null);

  /** Is a series one of the panel's marks, not an overlay? @param {any} s */
  const isMark = (s) => s && s.id == null && !s.silent &&
    (s.type === 'scatter' || s.type === 'line');

  /**
   * The highlight patch for one panel, from the series the panel was drawn
   * with: a line, a band level or a scatter series is lit when its keys match
   * the filter; on a filter of a column a scatter series does not carry, each
   * point by its own row. A range filter lights the scatter points inside it,
   * and the lines and band levels with a point inside it; its facet key, in
   * `filters`, lights the panel. Overlays follow their series. Without a
   * filter, and nothing dimmed from before, the patch is empty.
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
    // A highlight splits a scatter level into muted and highlighted parts
    // (model-individual.js): `byPart` finds the part a drawn series is,
    // `byLevel` the whole level.
    const MUTED = '\u0002';
    /** @type {Map<string, any>} */
    const byPart = new Map();
    /** @type {Map<string, { rows: any[], pts: any[] }>} */
    const byLevel = new Map();
    for (const s of panel.series) {
      byPart.set(keyOf(s.level) + (s.muted ? MUTED : ''), s);
      const k = keyOf(s.level);
      const all = byLevel.get(k);
      if (all) byLevel.set(k, { rows: all.rows.concat(s.rows), pts: all.pts.concat(s.pts) });
      else byLevel.set(k, { rows: s.rows, pts: s.pts });
    }
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
      if (s.silent && s.type !== 'custom' && Array.isArray(s.data) && !s.data.length) return {};
      const ms = byPart.get(keyOf(s.name) + (s.blockrMuted ? MUTED : ''));
      if (isMark(s) && s.type === 'scatter' && ms) {
        if (perPoint) {
          let any = false;
          const keyOn = byRow || !lit || lit(NS.model.individualKeys(m, panel.facet, s.name), () => ms.rows);
          const data = ms.pts.map((/** @type {any} */ p, /** @type {number} */ j) => {
            const on = keyOn && (!byRow || NS.keys.rowMatches(ms.rows[j], filters)) &&
              (!inRange || inRange(p));
            if (on) any = true;
            return { value: p, itemStyle: { opacity: on ? 1 : DIM } };
          });
          // Both parts of a highlighted level answer for it.
          levelLit.set(keyOf(s.name), any || levelLit.get(keyOf(s.name)) === true);
          return { data, itemStyle: { opacity: 1 } };
        }
        return { data: ms.pts, itemStyle: { opacity: onLevel(s.name) ? 1 : DIM } };
      }
      const on = onLevel(s.name);
      if (isMark(s) && s.type === 'line') {
        const base = m.isBand ? 1 : m.lineOpacity;
        return { lineStyle: { opacity: on ? base : base * DIM },
                 itemStyle: { opacity: on ? 1 : DIM } };
      }
      if (s.type === 'line') {
        const base = m.isBand ? 1 : 0.9;
        return { lineStyle: { opacity: on ? base : base * DIM } };
      }
      return { itemStyle: { opacity: on ? 1 : DIM } };
    });
    return { series, dimmed: !!lit || !!inRange };
  };

  Object.assign(option, { individual, individualLegend, individualPatch });
})();
