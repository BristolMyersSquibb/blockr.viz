// @ts-check
/**
 * Chart block: the individual family (scatter, line, area, band) as the
 * view uses it: clicks, the brush, the zoom and the hover pickers. Its model
 * and option are in model-individual.js and option-individual.js.
 *
 *   - A click selects what the model says the mark is: a level as a
 *     categorical selection, or a scatter point as a zero-width range. A
 *     line click waits 275 ms, so a double-click (the zoom reset) cancels it.
 *   - The scatter brush selects the brushed points' extent, their series, or
 *     an explicit drill column's values in their rows. Points outside the
 *     data select nothing. A range taken in a facet panel carries the panel's
 *     key.
 *   - The hover pickers find the nearest line or band level from the
 *     cursor's position on every move, so a dropped mouseout cannot leave a
 *     line highlighted.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  // How long a click guards against the brush clear that follows it (a line
  // longer), and how long a line click waits for a second click.
  const GUARD_MS = 150;
  const LINE_GUARD_MS = 450;
  const LINE_HOLD_MS = 275;
  // How far (px) the cursor may be from a line, or a band's centre line, to hover it.
  const HOVER_PX = 14;
  const BAND_HOVER_PX = 60;

  /** The band level whose centre line is nearest the cursor vertically (at the cursor's x),
   * or null outside the plot or far from every line.
   * @param {ChartSlot} slot @param {any} chart @param {number} px @param {number} py
   * @returns {string | null} */
  const nearestBandLevel = (slot, chart, px, py) => {
    const b = slot.band;
    if (!b) return null;
    if (!chart.containPixel({ gridIndex: 0 }, [px, py])) return null;
    const opt = chart.getOption();
    const all = (opt && opt.series) || [];
    let best = null, bestD = Infinity;
    for (const lvl of Object.keys(b.centerIdx)) {
      const sv = all[b.centerIdx[lvl]];
      if (!sv || !Array.isArray(sv.data)) continue;
      // The nearest point by x stands in for the line: the grid is dense.
      let py2 = null, dx = Infinity;
      for (const pt of sv.data) {
        if (pt[1] == null) continue;
        const q = chart.convertToPixel({ gridIndex: 0 }, [pt[0], pt[1]]);
        if (!q) continue;
        const d = Math.abs(q[0] - px);
        if (d < dx) { dx = d; py2 = q[1]; }
      }
      if (py2 == null) continue;
      const d = Math.abs(py2 - py);
      if (d < bestD) { bestD = d; best = lvl; }
    }
    return bestD <= BAND_HOVER_PX ? best : null;
  };

  /** Show the focused level's ribbon, empty the others. @param {ChartSlot} slot @param {any} chart */
  const applyBandFocus = (slot, chart) => {
    const b = slot.band;
    if (!b || !b.ribbonIdx) return;
    /** @type {any[]} */ const patch = new Array(b.total);
    for (let i = 0; i < b.total; i++) patch[i] = {};
    for (const lvl of Object.keys(b.ribbonIdx)) {
      const r = b.ribbonIdx[lvl];
      const on = slot.bandFocus != null && String(slot.bandFocus) === String(lvl);
      patch[r.idx] = { data: on ? [r.item] : [] };
    }
    chart.setOption({ series: patch }, { lazyUpdate: true });
  };

  /** The line nearest a cursor pixel (found in data space, then checked on screen), or null
   * when none is within HOVER_PX.
   * @param {ChartSlot} slot @param {any} chart @param {number} px @param {number} py
   * @returns {number | null} */
  const nearestLineSeries = (slot, chart, px, py) => {
    const f = slot.focus;
    if (!f) return null;
    let cd;
    try { cd = chart.convertFromPixel({ xAxisIndex: 0, yAxisIndex: 0 }, [px, py]); } catch (e) { return null; }
    if (!cd || cd[0] == null || cd[1] == null) return null;
    const cx = +cd[0], cy = +cd[1];
    // On a log axis, near is measured in logs, as the screen shows it.
    const yt = f.yLog ? (/** @type {number} */ v) => Math.log10(Math.max(v, 1e-300)) : (/** @type {number} */ v) => v;
    const xOrder = f.xAxisType === 'category' ? f.xOrder : null;
    const series = f.series;
    let bestSi = null, bestY = null, bestX = null, bestDy = Infinity;
    for (let i = 0; i < series.length; i++) {
      const s = series[i];
      if (!s || s.type !== 'line' || !Array.isArray(s.data) || !s.data.length) continue;
      const yv = NS.model.interpYAtX(s.data, cx, xOrder);
      if (yv == null) continue;
      const dy = Math.abs(yt(yv) - yt(cy));
      if (dy < bestDy) { bestDy = dy; bestSi = i; bestY = yv; bestX = s.data[0][0]; }
    }
    if (bestSi == null) return null;
    // Near in data can be far on screen; the series' first x converts on a
    // category axis, where cx is a bare index.
    let bp;
    try { bp = chart.convertToPixel({ seriesIndex: bestSi }, [bestX, bestY]); } catch (e) { return bestSi; }
    if (bp && Math.abs(bp[1] - py) > HOVER_PX) return null;
    return bestSi;
  };

  /** Hover: show the nearest band level's ribbon; note the nearest line.
   * @param {ChartView} view @param {ChartSlot} slot @param {any} chart @param {any} zr */
  const hoverEvents = (view, slot, chart, zr) => {
    zr.on('mousemove', (/** @type {any} */ e) => {
      if (view.settings.type === 'band' && slot.band) {
        const lvl = nearestBandLevel(slot, chart, e.offsetX, e.offsetY);
        if (lvl !== slot.bandFocus) {
          slot.bandFocus = lvl;
          applyBandFocus(slot, chart);
        }
      }
      if (!slot.focus) return;
      // The nearest line is what the tooltip names and a click takes; hover
      // draws nothing.
      const si = nearestLineSeries(slot, chart, e.offsetX, e.offsetY);
      slot.hover.si = si;
      slot.focusSi = si;
    });
    zr.on('globalout', () => {
      if (view.settings.type === 'band' && slot.band && slot.bandFocus != null) {
        slot.bandFocus = null;
        applyBandFocus(slot, chart);
      }
      if (!slot.focus) return;
      slot.hover.si = null;
      slot.focusSi = null;
    });
  };

  /** The zoom: a double-click or the reset button clears it; the window is
   * kept for a redraw.
   * @param {ChartView} view @param {ChartSlot} slot @param {any} chart @param {any} zr */
  const zoomEvents = (view, slot, chart, zr) => {
    // A double-click resets the zoom and cancels the line click it queued.
    zr.on('dblclick', () => {
      if (!NS.isLineType(view.settings.type)) return;
      if (slot.lineClick) {
        slot.lineClick();
        slot.lineClick = null;
      }
      chart.dispatchAction({ type: 'dataZoom', start: 0, end: 100 });
    });
    // A reset arrives without values and at the full range.
    chart.on('datazoom', (/** @type {any} */ e) => {
      const p = (e && e.batch && e.batch[0]) || e || {};
      const reset = p.startValue == null && p.endValue == null &&
        (p.start == null || p.start === 0) && (p.end == null || p.end === 100);
      slot.zoom = reset ? null
        : { start: p.start, end: p.end, startValue: p.startValue, endValue: p.endValue };
      slot.resetBtn.hidden = !slot.zoom;
    });
    // The slot's scope goes with this chart; the next chart registers again.
    /** @type {VizScope} */ (slot.scope).on(slot.resetBtn, 'click', () => {
      if (slot.chart) slot.chart.dispatchAction({ type: 'dataZoom', start: 0, end: 100 });
    });
  };

  /** A click on a mark: what the model says it is, latched or sent.
   * @param {ChartView} view @param {ChartSlot} slot @param {any} params @param {string} [focusName] */
  const clickMark = (view, slot, params, focusName) => {
    const m = view._memo.model;
    if (!m || m.family !== 'individual') return;
    const panel = NS.model.panelOf(m, slot.facetVal);
    const sel = NS.model.individualClick(m, view.settings, panel, params, focusName);
    if (!sel) return;
    if (sel.type === 'categorical') view.selection.click(slot, params, sel.filters);
    else view.selection.latch(sel, false);
  };

  /** @param {ChartView} view @param {ChartSlot} slot @param {any} chart */
  const clickEvents = (view, slot, chart) => {
    chart.on('click', (/** @type {any} */ params) => {
      if (NS.drillState(view.settings) === 'off') return;
      if (params.componentType !== 'series') return;
      const isLine = NS.isLineType(view.settings.type);
      const sel = view.selection;
      // A click in brush mode also fires a brush clear: ignore it for a moment.
      sel.suppressBrushClear = true;
      view.scope.timeout(() => { sel.suppressBrushClear = false; },
                         isLine ? LINE_GUARD_MS : GUARD_MS);
      if (isLine) {
        // The hovered line, taken now, before the cursor drifts.
        const focusName = slot.focus?.series[/** @type {number} */ (slot.focusSi)]?.name;
        if (slot.lineClick) slot.lineClick();
        slot.lineClick = view.scope.timeout(() => {
          slot.lineClick = null;
          clickMark(view, slot, params, focusName);
        }, LINE_HOLD_MS);
        return;
      }
      clickMark(view, slot, params);
    });
  };

  /** What the brush caught, latched; the other panels' brushes cleared.
   * @param {ChartView} view @param {ChartSlot} slot
   * @param {Array<{ seriesIndex: number, dataIndex: number }>} selected */
  const brushed = (view, slot, selected) => {
    const m = view._memo.model;
    if (!m || m.family !== 'individual') return;
    // The other panels' brushes go; their clears are not the reader's.
    const others = view.charts.filter((c) => c !== slot.chart);
    if (others.length) {
      view.selection.suppressBrushClear = true;
      for (const other of others) other.dispatchAction({ type: 'brush', areas: [] });
      view.scope.timeout(() => { view.selection.suppressBrushClear = false; }, GUARD_MS);
    }
    const panel = NS.model.panelOf(m, slot.facetVal);
    const opt = slot.chart.getOption();
    const caught = NS.model.individualBrush(m, view.settings, panel, (opt && opt.series) || [], selected);
    const sel = NS.model.brushSelection(m, view.settings, panel, caught);
    if (!sel) return;
    // The rectangle drawn again after a redraw catches what it caught.
    const key = JSON.stringify(sel);
    if (key === JSON.stringify(view.selection.current)) return;
    slot.brushSel = key;
    view.selection.latch(sel, true);
  };

  /** @param {ChartView} view @param {ChartSlot} slot @param {any} chart */
  const brushEvents = (view, slot, chart) => {
    chart.on('brushSelected', (/** @type {any} */ params) => {
      if (NS.drillState(view.settings) === 'off') return;
      const batch = params.batch && params.batch[0];
      if (!batch) return;
      const selected = (batch.selected || []).flatMap((/** @type {any} */ s) =>
        (s.dataIndex || []).map((/** @type {number} */ di) =>
          ({ seriesIndex: s.seriesIndex, dataIndex: di })));
      // An empty selection clears only a selection a brush made: ECharts
      // also sends one when the cursor is armed on every draw, which would
      // drop a restored range or a point click.
      if (!selected.length) {
        if (view.selection.current && view.selection.byBrush) view.selection.brushCleared();
        return;
      }
      brushed(view, slot, selected);
    });
    chart.on('brush', (/** @type {any} */ params) => {
      const areas = params.areas || [];
      // Kept in data coordinates: a redraw drops the rectangle (after()).
      slot.brushAreas = areas.length ? areas.map((/** @type {any} */ a) => ({
        brushType: a.brushType, coordRange: a.coordRange, xAxisIndex: 0,
        ...(a.brushType === 'rect' ? { yAxisIndex: 0 } : {})
      })) : null;
      if (!areas.length) view.selection.brushCleared();
    });
  };

  /** @type {ChartFamily} */
  const individual = {
    name: 'individual',
    model: NS.model.individual,
    option: NS.option.individual,
    legend: NS.option.individualLegend,
    widths: true,
    alwaysHighlight: false,

    attach(view, slot) {
      const chart = slot.chart;
      const zr = chart.getZr();
      hoverEvents(view, slot, chart, zr);
      zoomEvents(view, slot, chart, zr);
      clickEvents(view, slot, chart);
      brushEvents(view, slot, chart);
    },

    // The series as drawn by this option, unless ECharts holds another set.
    patch(c) {
      const drawn = c.drawn && c.drawn.series && c.drawn.series.length === c.current.series.length
        ? c.drawn.series : c.current.series;
      return NS.option.individualPatch(c.m, c.panel, drawn, c.lit, c.dimmed, c.filters, c.range,
                                       c.ink);
    },

    // The slot takes the panel's hover state and pickers; the hover does not
    // survive a redraw.
    prepare(slot, po) {
      slot.hover = po.hover;
      slot.hover.si = null;
      slot.focusSi = null;
      slot.focus = po.focus;
      slot.band = po.band;
    },

    actions: (slot, po) => !!(po.brushable || slot.brushable || po.zoomable || slot.zoomArmed),

    // The brush cursor on a brushable scatter, the zoom drag on a line, each
    // released when the panel stops being one; the zoom window back; a
    // hovered band level's ribbon.
    after(slot, po, view) {
      const chart = slot.chart;
      /** @param {string} key @param {Record<string, any>} opt */
      const cursor = (key, opt) => chart.dispatchAction({ type: 'takeGlobalCursor', key, ...opt });
      if (po.brushable) {
        cursor('brush', { brushOption: { brushType: 'rect', brushMode: 'single' } });
        // The redraw dropped the rectangle; it stays while its selection does.
        if (slot.brushAreas && slot.brushSel === JSON.stringify(view.selection.current)) {
          chart.dispatchAction({ type: 'brush', areas: slot.brushAreas });
        }
      } else if (slot.brushable) {
        cursor('brush', { brushOption: { brushType: false } });
      }
      slot.brushable = po.brushable;
      if (po.zoomable) cursor('dataZoomSelect', { dataZoomSelectActive: true });
      else if (slot.zoomArmed) cursor('dataZoomSelect', { dataZoomSelectActive: false });
      slot.zoomArmed = po.zoomable;
      if (po.zoomable && slot.zoom) chart.dispatchAction({ type: 'dataZoom', ...slot.zoom });
      else if (!po.zoomable) slot.zoom = null;
      slot.resetBtn.hidden = !slot.zoom;
      if (slot.band && slot.bandFocus != null) applyBandFocus(slot, chart);
    }
  };

  NS.families = Object.assign(NS.families || {}, { individual });
})();
