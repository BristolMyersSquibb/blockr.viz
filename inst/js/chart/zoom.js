// @ts-check
/**
 * Chart block: drag to zoom, on every chart with a continuous axis.
 *
 *   - A drag on the plot draws a box; on release both axes zoom to it. A
 *     flat drag (under 10 px tall) zooms x only and refits y; a narrow one
 *     zooms y only. On the aggregated types only the value axis zooms, so
 *     the box spans the categories.
 *   - On a scatter the drag is the brush, a selection or a drill. A
 *     Select / Zoom switch over the plot gives the drag to the zoom instead;
 *     Select is the default.
 *   - A double-click or the Reset zoom link goes back to the full view: the
 *     axis limits when set (zoom-option.js), else the data.
 *   - Keep as limits writes the zoomed window into `x_limits` and
 *     `y_limits`; they then show in the subtitle (limits-phrase.js), where
 *     they are edited or cleared.
 * The window lives on the slot and is drawn again after a redraw; it is not
 * part of the block state.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const ZO = NS.zoomOption;

  /** A drag shorter than this on an axis leaves that axis alone (px). */
  const FLAT_PX = 10;
  /** How long a click after a drag is ignored (ms). */
  const AFTER_DRAG_MS = 300;
  const KEYS = /** @type {('xAxis' | 'yAxis')[]} */ (['xAxis', 'yAxis']);

  /** The plot area in the slot's pixels.
   * @param {any} chart @returns {{ x: number, y: number, width: number, height: number } | null} */
  const gridRect = (chart) => {
    const g = chart.getModel().getComponent('grid', 0);
    const cs = g && g.coordinateSystem;
    return cs && cs.getRect ? cs.getRect() : null;
  };

  /** @param {number} v @param {number} lo @param {number} hi */
  const clamp = (v, lo, hi) => Math.min(hi, Math.max(lo, v));

  /** Whether the drag on this slot zooms (false: a scatter in Select mode).
   * @param {ChartView} view @param {ChartSlot} slot */
  const armed = (view, slot) => !!slot.zoomRoles && (!slot.brushable || view.zoomOn === true);

  /**
   * What a drag from `a` to `b` zooms: per axis, the pixel span or null.
   * @param {ChartSlot} slot @param {any} g @param {{ x: number, y: number }} a @param {{ x: number, y: number }} b
   */
  const spans = (slot, g, a, b) => {
    const r = /** @type {any} */ (slot.zoomRoles);
    const x0 = clamp(Math.min(a.x, b.x), g.x, g.x + g.width), x1 = clamp(Math.max(a.x, b.x), g.x, g.x + g.width);
    const y0 = clamp(Math.min(a.y, b.y), g.y, g.y + g.height), y1 = clamp(Math.max(a.y, b.y), g.y, g.y + g.height);
    const wide = x1 - x0 >= FLAT_PX, tall = y1 - y0 >= FLAT_PX;
    return { xAxis: r.xAxis && wide ? [x0, x1] : null, yAxis: r.yAxis && tall ? [y0, y1] : null };
  };

  /** The box drawn while dragging, or null where ECharts draws nothing.
   * @param {any} zr */
  const makeBox = (zr) => {
    const G = root.echarts && root.echarts.graphic;
    if (!G || !G.Rect || !zr.add) return null;
    const box = new G.Rect({
      shape: { x: 0, y: 0, width: 0, height: 0 }, silent: true, z: 1e4,
      style: { fill: 'rgba(0, 114, 178, 0.1)', stroke: 'rgba(0, 114, 178, 0.5)', lineWidth: 1 }
    });
    zr.add(box);
    return box;
  };

  /** Send the slot's window to ECharts, or the full view when it has none.
   * @param {ChartSlot} slot */
  const apply = (slot) => {
    const chart = slot.chart;
    if (!chart || !slot.zoomRoles) return;
    const z = slot.zoom || {};
    // x filters what it leaves out so y refits, unless y has its own window:
    // the filter would move it.
    if (chart.__vzFilterX && slot.zoomRoles.xAxis) {
      chart.setOption({ dataZoom: [{ id: ZO.ZOOM_ID.xAxis, filterMode: z.yAxis ? 'none' : 'filter' }] });
    }
    // A zoomed value axis labels its ticks only, not the window's raw ends.
    /** @type {Record<string, any>} */
    const labels = {};
    for (const k of KEYS) {
      const comp = slot.zoomRoles[k] ? chart.getModel().getComponent(k, 0) : null;
      if (!comp || comp.get('type') !== 'value') continue;
      const off = z[k] ? false : null;
      labels[k] = [{ axisLabel: { showMinLabel: off, showMaxLabel: off } }];
    }
    if (Object.keys(labels).length) chart.setOption(labels);
    const batch = KEYS.filter((k) => slot.zoomRoles && slot.zoomRoles[k]).map((k) => (z[k]
      ? { dataZoomId: ZO.ZOOM_ID[k], startValue: z[k][0], endValue: z[k][1] }
      : { dataZoomId: ZO.ZOOM_ID[k], start: 0, end: 100 }));
    if (batch.length) chart.dispatchAction({ type: 'dataZoom', batch });
  };

  /** @param {ChartView} view @param {ChartSlot} slot */
  const showLinks = (view, slot) => {
    slot.resetBtn.hidden = !slot.zoom;
    if (slot.keepBtn) slot.keepBtn.hidden = !slot.zoom || !view.gear;
  };

  /** @param {ChartView} view @param {ChartSlot} slot */
  const reset = (view, slot) => {
    slot.zoom = null;
    apply(slot);
    showLinks(view, slot);
  };

  const DAY_MS = 864e5;
  /** A window widened to tidy ends, so the axis does not label a raw value:
   * whole days on a time axis, else a hundredth of the span's order.
   * @param {number[]} v @param {string} type */
  const tidy = (v, type) => {
    const span = v[1] - v[0];
    if (!(span > 0)) return v;
    const step = type === 'time' ? (span > 2 * DAY_MS ? DAY_MS : 0)
      : Math.pow(10, Math.floor(Math.log10(span)) - 1);
    if (!step) return v;
    const r = (/** @type {number} */ x) => Number(x.toPrecision(12));
    return [r(Math.floor(v[0] / step) * step), r(Math.ceil(v[1] / step) * step)];
  };

  /** Zoom to the pixel spans: each to data values, within the current view.
   * @param {ChartView} view @param {ChartSlot} slot @param {ReturnType<typeof spans>} s */
  const zoomTo = (view, slot, s) => {
    const chart = slot.chart;
    /** @type {Record<string, number[] | null>} */
    const next = { ...(slot.zoom || {}) };
    for (const k of KEYS) {
      const px = s[k];
      if (!px) continue;
      const finder = k === 'xAxis' ? { xAxisIndex: 0 } : { yAxisIndex: 0 };
      const v = px.map((p) => Number(chart.convertFromPixel(finder, p)));
      if (!v.every(Number.isFinite)) continue;
      const comp = chart.getModel().getComponent(k, 0);
      next[k] = tidy([Math.min(v[0], v[1]), Math.max(v[0], v[1])],
                     String(comp ? comp.get('type') : 'value'));
    }
    // A flat drag on a chart that zooms both ways: y refits to what x shows.
    if (s.xAxis && !s.yAxis && slot.zoomRoles && slot.zoomRoles.yAxis) next.yAxis = null;
    slot.zoom = next.xAxis || next.yAxis ? next : null;
    apply(slot);
    showLinks(view, slot);
  };

  /** A value as the limits field takes it: a date on a time axis, else four
   * significant digits.
   * @param {number} v @param {string} type */
  const limitValue = (v, type) => (type === 'time'
    ? new Date(v).toISOString().slice(0, 10) : Number(v.toPrecision(4)));

  /** The zoomed window into `x_limits` / `y_limits`; the zoom is then spent.
   * @param {ChartView} view @param {ChartSlot} slot */
  const keep = (view, slot) => {
    const chart = slot.chart;
    if (!chart || !slot.zoom || !slot.zoomRoles || !view.gear) return;
    /** @type {Record<string, unknown>} */
    const changes = {};
    for (const k of KEYS) {
      const role = slot.zoomRoles[k];
      if (!role || !slot.zoom[k]) continue;
      const comp = chart.getModel().getComponent(k, 0);
      const type = String(comp ? comp.get('type') : 'value');
      const ext = comp ? comp.axis.scale.getExtent() : slot.zoom[k];
      changes[role + '_limits'] = [limitValue(ext[0], type), limitValue(ext[1], type)];
    }
    slot.zoom = null;
    showLinks(view, slot);
    view.gear.set(changes);
  };

  /** The Select / Zoom switch over a brushable scatter: one per view, on
   * its first panel.
   * @param {ChartView} view @param {ChartSlot} slot @param {number} i */
  const modeSwitch = (view, slot, i) => {
    const show = i === 0 && slot.brushable;
    if (!slot.modeEl) {
      if (!show) return;
      const el = slot.modeEl = document.createElement('span');
      el.className = 'vz-chart-zoom-mode';
      el.setAttribute('role', 'group');
      el.setAttribute('aria-label', 'Drag selects or zooms');
      for (const [on, label] of /** @type {[boolean, string][]} */ ([[false, 'Select'], [true, 'Zoom']])) {
        const b = document.createElement('button');
        b.type = 'button';
        b.textContent = label;
        b.dataset.zoom = String(on);
        /** @type {VizScope} */ (slot.scope).on(b, 'click', () => setMode(view, on));
        el.appendChild(b);
      }
      slot.zoomBar.prepend(el);
    }
    slot.modeEl.hidden = !show;
    for (const b of Array.from(slot.modeEl.children)) {
      b.classList.toggle('is-on', /** @type {HTMLElement} */ (b).dataset.zoom === String(view.zoomOn === true));
    }
  };

  /** Give the scatter drag to the zoom (true) or the brush.
   * @param {ChartView} view @param {boolean} on */
  const setMode = (view, on) => {
    view.zoomOn = on;
    view._slots.forEach((slot, i) => {
      if (!slot || !slot.chart) return;
      if (slot.brushable) {
        slot.chart.dispatchAction({ type: 'takeGlobalCursor', key: 'brush',
                                    brushOption: { brushType: on ? false : 'rect', brushMode: 'single' } });
      }
      modeSwitch(view, slot, i);
    });
  };

  /** Whether a click on the slot is the end of a drag.
   * @param {ChartSlot} slot */
  const justDragged = (slot) => !!slot.zoomDragAt && Date.now() - slot.zoomDragAt < AFTER_DRAG_MS;

  /** The drag, the double-click and the links, once per ECharts instance.
   * @param {ChartView} view @param {ChartSlot} slot */
  const attach = (view, slot) => {
    const chart = slot.chart;
    const zr = chart.getZr();
    /** @type {{ a: { x: number, y: number }, g: any } | null} */
    let drag = null;
    /** @type {any} */
    let box = null;
    const end = () => {
      drag = null;
      if (box) { zr.remove(box); box = null; }
    };
    zr.on('mousedown', (/** @type {any} */ e) => {
      if (!armed(view, slot) || (e.event && e.event.button)) return;
      const g = gridRect(chart);
      if (!g || e.offsetX < g.x || e.offsetX > g.x + g.width ||
          e.offsetY < g.y || e.offsetY > g.y + g.height) return;
      drag = { a: { x: e.offsetX, y: e.offsetY }, g };
    });
    zr.on('mousemove', (/** @type {any} */ e) => {
      if (!drag) return;
      const s = spans(slot, drag.g, drag.a, { x: e.offsetX, y: e.offsetY });
      if (!s.xAxis && !s.yAxis) return;
      if (!box) box = makeBox(zr);
      if (!box) return;
      const g = drag.g;
      const [x0, x1] = s.xAxis || [g.x, g.x + g.width];
      const [y0, y1] = s.yAxis || [g.y, g.y + g.height];
      box.setShape({ x: x0, y: y0, width: x1 - x0, height: y1 - y0 });
    });
    zr.on('mouseup', (/** @type {any} */ e) => {
      if (!drag) return;
      const s = spans(slot, drag.g, drag.a, { x: e.offsetX, y: e.offsetY });
      end();
      if (!s.xAxis && !s.yAxis) return;
      slot.zoomDragAt = Date.now();
      zoomTo(view, slot, s);
    });
    zr.on('globalout', end);
    // A double-click resets the zoom and cancels the line click it queued.
    zr.on('dblclick', () => {
      if (slot.lineClick) {
        slot.lineClick();
        slot.lineClick = null;
      }
      if (slot.zoom) reset(view, slot);
    });
    // The slot's scope goes with this chart; the next chart registers again.
    const scope = /** @type {VizScope} */ (slot.scope);
    scope.on(slot.resetBtn, 'click', () => reset(view, slot));
    if (slot.keepBtn) scope.on(slot.keepBtn, 'click', () => keep(view, slot));
  };

  /**
   * After a draw: the slot takes the option's axis roles, and its window
   * comes back where the axes still zoom.
   * @param {ChartView} view @param {ChartSlot} slot @param {number} i
   * @param {ReturnType<typeof ZO.roles>} roles
   */
  const after = (view, slot, i, roles) => {
    const same = !!roles && !!slot.zoomRoles &&
      KEYS.every((k) => !!(roles && roles[k]) === !!(slot.zoomRoles && slot.zoomRoles[k]));
    slot.zoomRoles = roles;
    if (!same) slot.zoom = null;
    if (slot.zoom) apply(slot);
    showLinks(view, slot);
    modeSwitch(view, slot, i);
  };

  NS.zoom = { attach, after, reset, keep, setMode, justDragged, spans, limitValue, tidy };
})();
