// @ts-check
/**
 * Chart block: the individual option (scatter, line, band).
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
  // toolbox, so one is rendered, with no size and outside the canvas. The
  // cursors are armed in _individualAfter; a double-click or the panel's
  // Reset zoom link undoes a zoom, and a click off the brush clears it.
  const HIDDEN_TOOLBOX = { show: true, itemSize: 0, itemGap: 0, right: -40, top: -40 };

  // A scatter's brush.
  const brushToolbox = () => ({
    ...HIDDEN_TOOLBOX,
    feature: { brush: { type: ['rect', 'lineX', 'clear'] } }
  });

  // A line chart's drag-to-zoom on x.
  const zoomToolbox = () => ({
    ...HIDDEN_TOOLBOX,
    feature: {
      // The data x axis only, not the veil's hidden one.
      dataZoom: { xAxisIndex: 0, yAxisIndex: false, filterMode: 'filter' }
    }
  });

  const { identityDomain, pos, logDomains, clampBand } = parts.log;

  /** The settings with the band's series clamped above `floor`, for a log
   * axis. @param {Record<string, any>} cfg @param {number} floor */
  const clampedBand = (cfg, floor) => {
    const band = cfg.derived.band || {};
    return { ...cfg, derived: { ...cfg.derived,
                                band: { ...band, series: clampBand(band.series, floor) } } };
  };

  /**
   * What every part of a panel's option reads: the config, the colours, the
   * panel's log domains and the measured x labels.
   * @param {any} m @param {any} look @param {number} width
   */
  const panelContext = (m, look, width) => {
    const { cfg, ink, theme } = look;
    // This panel's log domains, null on a linear axis.
    const lx = (look.logDom && look.logDom.x) || null;
    const ly = (look.logDom && look.logDom.y) || null;
    const title = (/** @type {any} */ col) => NS.axes.axisTitle(look.columns, col);
    const ax = { labelColor: ink.muted, fontSize: ink.fontSize, splitLineColor: ink.border };
    const lm = cfg.line_width ?? 1.0;
    const xCats = m.x.cats;
    // Categorical x: every label, flat while it fits its slot, else turned
    // and thinned (an ordered axis). Measured before the height: turned
    // labels grow the canvas by their gutter.
    const xlab = xCats ? NS.axes.xAxisLabels(xCats, width - X_INSET, true, look.measure, ink) : null;
    return { m, look, width, cfg, ink, theme, lx, ly, title, ax, lm, xCats, xlab,
             isLine: m.isLine, isBand: m.isBand, isTime: m.x.type === 'time' };
  };

  /**
   * The panel's series from the band or the points part, with what the
   * option needs beside them.
   * @param {any} k @param {any} panel
   */
  const panelSeries = (k, panel) => {
    const { m, look, cfg, lx, ly, width } = k;
    let mlTarget = 0;
    let bandMany = false;
    /** @type {any} */
    let band = null;
    /** @type {any} */
    let pts = null;
    // Room on the right for names at the line ends.
    let labelPad = 0;
    /** @type {any[]} */
    let series;
    if (k.isBand) {
      const b = parts.band(m, panel, ly
        ? { ...look, cfg: clampedBand(cfg, ly.min) } : look);
      series = b.series;
      labelPad = b.labelPad;
      band = b.band;
      bandMany = b.many;
      if (b.markLineIdx >= 0) mlTarget = b.markLineIdx;
    } else {
      pts = parts.points(m, panel, (lx || ly)
        ? { ...look, logFloorX: lx ? lx.min : null, logFloorY: ly ? ly.min : null } : look,
        width > X_INSET ? width - X_INSET : 0);
      series = pts.series;
      labelPad = pts.labelPad || 0;
    }
    return { series, mlTarget, bandMany, band, pts, labelPad };
  };

  /**
   * The helper lines, the band's limits and the identity line, ready for the
   * shared markLine, with the domain both axes then share (`idDom`, null when
   * the identity line is off).
   * @param {any} k @param {any} ext @param {any[]} series
   */
  const panelRefData = (k, ext, series) => {
    const { m, look, cfg, ink, lx, ly, lm, isBand, isLine } = k;
    /** @type {any[]} */
    const refData = [];
    // A line's label names it; an unnamed line at 0 needs none: the axis
    // prints its value.
    /** @param {any} v @param {any} nm */
    const refText = (v, nm) => nm || (Number(v) === 0 ? null : undefined);
    // An x line sits where the axis puts x: a date on a time axis, a
    // category on a category axis (as `x_ranges` does).
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
    // y = x is a straight diagonal only when both axes use the same scale.
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
   * With a series column apart from the colour, the chips are colour
   * values no series is named after: an empty series per chip gives the
   * legend something to bind to.
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
    // Gridlines are solid hairlines. A line chart reads values off the y
    // grid and dates off the crosshair, so it has no x grid and no y axis
    // line.
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
        // A date axis labels years, then months, then days as it zooms in;
        // never turned.
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
    /** @type {any} */
    const yAxis = {
      type: m.y.type,
      name: NS.axisName(k.title(m.y.col), m.y.col, cfg),
      nameLocation: 'middle',
      nameGap: 46,
      nameRotate: 90,
      nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize },
      axisLabel: { color: ax.labelColor, fontSize: ax.fontSize,
                   ...(m.y.type === 'value' && NS.valueFormat(cfg, { unit: false })
                     ? { formatter: NS.valueFormat(cfg, { unit: false }) } : {}) },
      axisTick: { show: false },
      axisLine: isLine ? { show: false } : { lineStyle: { color: ink.strong } },
      splitLine: grid,
      scale: true
    };
    if (m.y.cats) yAxis.data = m.y.cats;
    if (idDom && m.y.type === 'value') { yAxis.min = idDom[0]; yAxis.max = idDom[1]; }
    if (ext) { yAxis.min = ext.lo; yAxis.max = ext.hi; }
    // An area is measured from zero.
    if (cfg.type === 'area' && m.y.type === 'value' && yAxis.min == null) {
      yAxis.scale = false;
      yAxis.min = (/** @type {any} */ v) => Math.min(0, v.min);
    }
    if (ly) NS.axes.logAxis(yAxis, idLog || ly);
    // The veil's own 0..1 axes, in the same grid (line charts only).
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

    // On a log axis the band's frame is the log domain, so a reference
    // limit at or below zero is pinned to its foot.
    const ext = (isBand && m.y.type === 'value' && series.length)
      ? (ly ? { lo: ly.min, hi: ly.max } : parts.bandExtent(cfg, panel, bandMany)) : null;

    // Helper lines, the band's limits and the identity line share one
    // markLine (ECharts allows one per series).
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

    // The brush needs continuous x; a line and a band are clicked, not
    // brushed, and so is a scatter with series (a brush would race the
    // click on a point).
    const brushable = m.x.type !== 'category' && !isLine && !isBand && !m.seriesCol;
    const zoomable = isLine;

    const hover = { si: null };
    /** @type {{ veil?: string }} */
    const scrimState = {};
    if (isLine) for (const s of parts.lineOverlay(scrimState)) series.push(s);
    // The line picker's registry: only for a crowd of lines, one per entity.
    // A few lines are read off the crosshair's tooltip instead, so nothing
    // on the plot changes as the cursor moves.
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
      // The first draw animates. Every later change of the same chart (a
      // resize, the x label re-fit that follows it, a new filter) snaps:
      // the re-fit patches the option with a merge, and ECharts morphs the
      // lines on that while the dots on top of them jump to the new layout.
      animationDurationUpdate: 0,
      tooltip: isLine ? parts.lineTooltip(m, look, hover) : parts.pointTooltip(m, look),
      // A hidden legend: its selection model is what the chips toggle.
      legend: { show: false },
      grid: { left: 66, right: 5 + labelPad, top: 30,
              bottom: 52 + (xlab ? xlab.bottom : 0) },
      xAxis: isLine ? [xAxis, scrimAxis] : xAxis,
      yAxis: isLine ? [yAxis, scrimAxis] : yAxis,
      toolbox: brushable ? brushToolbox() : (zoomable ? zoomToolbox() : undefined),
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
  const individual = (m, look) => {
    const logs = (m.logX || m.logY) ? logDomains(m, look.cfg) : null;
    const dropped = (m.logDropped || 0) + (logs ? logs.dropped : 0);
    const logNote = dropped
      ? `Log scale: ${dropped} value${dropped === 1 ? '' : 's'} at or below zero not shown.` : null;
    return {
      panels: m.panels.map((/** @type {any} */ panel, /** @type {number} */ i) =>
        panelOption(m, panel, logs ? { ...look, logDom: { x: logs.x[i], y: logs.y[i] } } : look,
                    (look.widths || [])[i] || 0)),
      // The trajectory cap and what a log axis leaves out, in the footer.
      labelNote: [m.capMessage, logNote].filter(Boolean).join(' ') || null
    };
  };
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
   * empty.
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
    // A highlight splits a scatter level into its muted points and its
    // highlighted ones (model-individual.js highlightSeries): `byPart` finds
    // the part a drawn series is, `byLevel` the whole level.
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
      // The legend's empty series.
      if (s.silent && s.type !== 'custom' && Array.isArray(s.data) && !s.data.length) return {};
      const ms = byPart.get(keyOf(s.name) + (s.blockrMuted ? MUTED : ''));
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
          // Both parts of a highlighted level answer for it.
          levelLit.set(keyOf(s.name), any || levelLit.get(keyOf(s.name)) === true);
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
