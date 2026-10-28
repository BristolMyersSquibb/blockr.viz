// @ts-check
/**
 * Chart block: clicks, the brush and the hover pickers of the individual
 * family (scatter, line, band). Methods of the chart view; adds to the
 * handler table of interact.js.
 *
 *   - A click sends what the model says the mark is (D6, D7): a level as a
 *     categorical filter, or a scatter point as a zero-width range. A line
 *     click waits 275 ms, so a double-click (the zoom reset) cancels it.
 *   - The scatter brush sends the brushed points' extent, their series, or
 *     an explicit drill column's values in their rows (D9; B4). A range
 *     taken in a facet panel carries the panel's key (D10).
 *   - The hover pickers find the nearest line or band level from the
 *     cursor's position on every move, so a dropped mouseout cannot leave a
 *     line highlighted.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});

  // How long a click guards against the brush clear that follows it (a line
  // longer), and how long a line click waits for a second click.
  const GUARD_MS = 150;
  const LINE_GUARD_MS = 450;
  const LINE_HOLD_MS = 275;
  // How far (px) the cursor may be from a line, or a band's centre line, to hover it.
  const HOVER_PX = 14;
  const BAND_HOVER_PX = 60;

  /** Hover: show the nearest band level's ribbon, promote the nearest line.
   * @param {any} view @param {any} slot @param {any} chart @param {any} zr */
  const hoverEvents = (view, slot, chart, zr) => {
    zr.on('mousemove', (/** @type {any} */ e) => {
      if (view.config.type === 'band' && slot.band) {
        const lvl = view._nearestBandLevel(slot, chart, e.offsetX, e.offsetY);
        if (lvl !== slot.bandFocus) {
          slot.bandFocus = lvl;
          view._applyBandFocus(slot, chart);
        }
      }
      if (!slot.focus) return;
      const si = view._nearestLineSeries(slot, chart, e.offsetX, e.offsetY);
      if (si === slot.focusSi) return;
      slot.hover.si = si;
      if (si == null) view._demoteFocus(slot, chart);
      else view._promoteFocus(slot, chart, si);
    });
    zr.on('globalout', () => {
      if (view.config.type === 'band' && slot.band && slot.bandFocus != null) {
        slot.bandFocus = null;
        view._applyBandFocus(slot, chart);
      }
      if (!slot.focus) return;
      slot.hover.si = null;
      view._demoteFocus(slot, chart);
    });
  };

  /** The zoom: a double-click or the reset button clears it; the window is
   * kept for a redraw.
   * @param {any} view @param {any} slot @param {any} chart @param {any} zr */
  const zoomEvents = (view, slot, chart, zr) => {
    // A double-click resets the zoom and cancels the line click it queued.
    zr.on('dblclick', () => {
      if (!NS.isLineType(view.config.type)) return;
      if (view._lineClickTimer) {
        view._lineClickTimer();
        view._lineClickTimer = null;
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
    view.scope.on(slot.resetBtn, 'click', () => {
      if (slot.chart) slot.chart.dispatchAction({ type: 'dataZoom', start: 0, end: 100 });
    });
  };

  /** A click on a mark. @param {any} view @param {any} slot @param {any} chart */
  const clickEvents = (view, slot, chart) => {
    chart.on('click', (/** @type {any} */ params) => {
      if (NS.drillState(view.config) === 'off') return;
      if (params.componentType !== 'series') return;
      const isLine = NS.isLineType(view.config.type);
      // A click in brush mode also fires a brush clear: ignore it for a moment.
      view._suppressBrushClear = true;
      view.scope.timeout(() => { view._suppressBrushClear = false; },
                         isLine ? LINE_GUARD_MS : GUARD_MS);
      if (isLine) {
        // The hovered line, taken now, before the cursor drifts.
        const focusName = slot.focus?.series[slot.focusSi]?.name;
        if (view._lineClickTimer) view._lineClickTimer();
        view._lineClickTimer = view.scope.timeout(() => {
          view._lineClickTimer = null;
          view._individualClick(slot, params, focusName);
        }, LINE_HOLD_MS);
        return;
      }
      view._individualClick(slot, params);
    });
  };

  /** The brush: what it caught, and its clear.
   * @param {any} view @param {any} slot @param {any} chart */
  const brushEvents = (view, slot, chart) => {
    chart.on('brushSelected', (/** @type {any} */ params) => {
      if (NS.drillState(view.config) === 'off') return;
      const batch = params.batch && params.batch[0];
      if (!batch) return;
      const selected = (batch.selected || []).flatMap((/** @type {any} */ s) =>
        (s.dataIndex || []).map((/** @type {number} */ di) =>
          ({ seriesIndex: s.seriesIndex, dataIndex: di })));
      // An empty selection clears only a filter a brush made: ECharts also
      // sends one when the cursor is armed on every draw, which would drop
      // a restored range or a point click (D11).
      if (!selected.length) {
        if (view._filter && view._filter.brush) view._brushCleared();
        return;
      }
      view._brushed(slot, selected);
    });

    chart.on('brush', (/** @type {any} */ params) => {
      if (!params.areas || params.areas.length === 0) view._brushCleared();
    });
  };

  /** @param {any} view @param {any} slot */
  NS.interactHandlers.individual = (view, slot) => {
    const chart = slot.chart;
    const zr = chart.getZr();
    hoverEvents(view, slot, chart, zr);
    zoomEvents(view, slot, chart, zr);
    clickEvents(view, slot, chart);
    brushEvents(view, slot, chart);
  };

  const individual = {
    // A click on a mark: a level latches as a categorical filter (or, with
    // a drill_target, is sent as an event); a point latches a range, also
    // with a drill_target. In a facet panel the range carries the panel's key (D10).
    /** @this {any} @param {any} slot @param {any} params @param {string} [focusName] */
    _individualClick(slot, params, focusName) {
      const m = this._memo.model;
      if (!m || m.family !== 'individual') return;
      const panel = NS.model.panelOf(m, slot.facetVal);
      const r = NS.model.individualClick(m, this.config, panel, params, focusName);
      if (!r) return;
      if (!('range' in r)) { this._select(slot, params, r.filters); return; }
      this._selectRange(r.range, false, r.filters || null);
    },

    // A range filter, latched: a point click or a brush (D11).
    /** @this {any} @param {any} range @param {boolean} brush
     *  @param {Record<string, any[]> | null} filters the facet key */
    _selectRange(range, brush, filters) {
      // A second click on the same point clears it, as on a bar.
      const f = this._filter;
      if (!brush && f && f.type === 'range' && !f.brush &&
          JSON.stringify([f.range, f.filters || null]) === JSON.stringify([range, filters || null])) {
        this._reset();
        return;
      }
      this._filter = { type: 'range', range, filters, brush };
      this._hasBrushFilter = true;
      this._refreshSelection();
      this._sendSelection(NS.keys.rangeSelection(range, filters));
    },

    // The footer, and the marks when something dims or was dimmed.
    /** @this {any} */
    _refreshSelection() {
      if (this._filter || this._slots.some((/** @type {any} */ s) => s && s.dimmed)) {
        this._updateHighlight();
      } else {
        this._updateStatus();
      }
    },

    // What the brush caught (D9): see the header. Points outside the data
    // select nothing and latch nothing (B4).
    /** @this {any} @param {any} slot @param {Array<{ seriesIndex: number, dataIndex: number }>} selected */
    _brushed(slot, selected) {
      const m = this._memo.model;
      if (!m || m.family !== 'individual') return;
      for (const other of this.charts) {
        if (other !== slot.chart) other.dispatchAction({ type: 'brush', areas: [] });
      }
      const panel = NS.model.panelOf(m, slot.facetVal);
      const opt = slot.chart.getOption();
      const caught = NS.model.individualBrush(m, this.config, panel, (opt && opt.series) || [], selected);
      const r = NS.model.brushSelection(m, this.config, panel, caught);
      if (!r) return;
      if (!('range' in r)) {
        this._filter = { type: 'categorical', filters: r.filters, brush: true };
        this._hasBrushFilter = true;
        this._refreshSelection();
        this._sendFilter(r.filters);
        return;
      }
      this._selectRange(r.range, true, r.filters || null);
    },

    // The brush was cleared: so is the filter, unless a click just made it
    // or drill is off, where the brush sent nothing (B4).
    /** @this {any} */
    _brushCleared() {
      if (this._suppressBrushClear) return;
      if (NS.drillState(this.config) === 'off') return;
      this._filter = null;
      this._hasBrushFilter = false;
      this._refreshSelection();
      this._sendSelection(null);
    },

    // The slot takes the panel's hover state and pickers; the hover does not
    // survive a redraw.
    /** @this {any} @param {any} slot @param {any} po */
    _individualPrepare(slot, po) {
      slot.hover = po.hover;
      slot.hover.si = null;
      slot.focusSi = null;
      slot.focus = po.focus;
      slot.band = po.band;
      if (po.focus) po.focus.veil = undefined;
    },

    // Does _individualAfter dispatch an action on this slot?
    /** @this {any} @param {any} slot @param {any} po */
    _individualActions(slot, po) {
      return !!(po.brushable || slot.brushable || po.zoomable || slot.zoomArmed);
    },

    // The brush cursor on a brushable scatter, the zoom drag on a line, each
    // released when the panel stops being one; the zoom window back; a
    // hovered band level's ribbon.
    /** @this {any} @param {any} slot @param {any} po */
    _individualAfter(slot, po) {
      const chart = slot.chart;
      /** @param {string} key @param {Record<string, any>} opt */
      const cursor = (key, opt) => chart.dispatchAction({ type: 'takeGlobalCursor', key, ...opt });
      if (po.brushable) {
        cursor('brush', { brushOption: { brushType: 'rect', brushMode: 'single' } });
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
      if (slot.band && slot.bandFocus != null) this._applyBandFocus(slot, chart);
    },

    /** The band level whose centre line is nearest the cursor vertically (at the cursor's x),
     * or null outside the plot or far from every line.
     * @this {any} @param {any} slot @param {any} chart @param {number} px @param {number} py
     * @returns {string | null} */
    _nearestBandLevel(slot, chart, px, py) {
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
    },

    /** Show the focused level's ribbon, empty the others. @this {any} @param {any} slot @param {any} chart */
    _applyBandFocus(slot, chart) {
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
    },

    /** The line nearest a cursor pixel (found in data space, then checked on screen), or null
     * when none is within HOVER_PX.
     * @this {any} @param {any} slot @param {any} chart @param {number} px @param {number} py
     * @returns {number | null} */
    _nearestLineSeries(slot, chart, px, py) {
      const f = slot.focus;
      if (!f) return null;
      let cd;
      // The data axes by index: the veil's axes share the grid.
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
        if (!s || s.id === '__focus__' || s.type !== 'line' ||
            !Array.isArray(s.data) || !s.data.length) continue;
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
    },

    /** Fill the overlay with the hovered line, a little wider, and the veil fading the rest.
     * @this {any} @param {any} slot @param {any} chart @param {number} si */
    _promoteFocus(slot, chart, si) {
      const f = slot.focus;
      if (!f) return;
      const s = f.series[si];
      if (!s || s.id === '__focus__' || s.type !== 'line' ||
          !Array.isArray(s.data) || !s.data.length) return;
      slot.focusSi = si;
      const clr = (s.itemStyle && s.itemStyle.color) || null;
      // Read now, so a light/dark switch after the draw is followed.
      const bg = this._plotSurface(slot);
      f.veil = 'rgba(' + bg.join(',') + ',0.6)';
      chart.setOption({
        series: [{
          // One constant datum: renderItem draws the veil from the grid.
          id: '__scrim__', data: [[0.5, 0.5]]
        }, {
          id: '__focus__', data: s.data, step: f.stepMode || undefined,
          smooth: f.smoothOn, smoothMonotone: f.smoothOn ? 'x' : undefined,
          showSymbol: false, lineStyle: { color: clr, width: f.focusW, opacity: 1 },
          areaStyle: null
        }, {
          id: '__focus_dots__', data: []
        }]
      });
    },

    /** RGB of the surface a slot is painted on: the theme's background, else the first
     * opaque ancestor's, else white. @this {any} @param {any} slot @returns {number[]} */
    _plotSurface(slot) {
      // A fully transparent colour does not count as a surface.
      /** @param {string} s */
      const rgbOf = (s) => (/^rgba\(.*,\s*0(\.0*)?\)$/.test(s) ? null : NS.rgbOf(s));
      if (this.theme && slot.chart) {
        const opt = slot.chart.getOption();
        const got = rgbOf(String(opt && opt.backgroundColor || ''));
        if (got) return got;
      }
      for (let el = slot.chartDiv; el && el.nodeType === 1; el = el.parentElement) {
        const got = rgbOf(getComputedStyle(el).backgroundColor);
        if (got) return got;
      }
      return [255, 255, 255];
    },

    /** Empty the overlay. @this {any} @param {any} slot @param {any} chart */
    _demoteFocus(slot, chart) {
      if (!slot.focus || slot.focusSi == null) return;
      slot.focusSi = null;
      chart.setOption({
        series: [{ id: '__scrim__', data: [] },
                 { id: '__focus__', data: [] }, { id: '__focus_dots__', data: [] }]
      });
    }
  };

  Object.assign(NS.interact, individual);
})();
