// @ts-check
/**
 * Chart block: drag to zoom, on every chart with a continuous axis.
 *
 *   - A drag on the plot draws a box; on release both axes zoom to it. A
 *     flat drag (under 10 px tall) zooms x only and refits y; a narrow one
 *     zooms y only. On the aggregated types only the value axis zooms, so
 *     the box spans the categories.
 *   - On a scatter the drag is the brush, a selection or a drill. A
 *     Select / Zoom switch in the header row gives the drag to the zoom
 *     instead; Select is the default.
 *   - A double-click or the Reset zoom link goes back to the full view: the
 *     axis limits when set (zoom-option.js), else the data.
 *   - The zoom is a view: it is not saved and sets no limits. Limits are
 *     the `x_limits` and `y_limits` settings (zoom-option.js).
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
    // ECharts' window does not map onto a log axis: a log axis zooms by its
    // min and max, back to its limits (or auto) on a reset.
    const logEnds = chart.__vzLogEnds || {};
    for (const k of KEYS) {
      if (!logEnds[k] || !slot.zoomRoles[k]) continue;
      const w = z[k] || logEnds[k];
      labels[k] = [{ min: w[0] ?? null, max: w[1] ?? null }];
    }
    if (Object.keys(labels).length) chart.setOption(labels);
    const batch = KEYS.filter((k) => slot.zoomRoles && slot.zoomRoles[k] && !logEnds[k]).map((k) => (z[k]
      ? { dataZoomId: ZO.ZOOM_ID[k], startValue: z[k][0], endValue: z[k][1] }
      : { dataZoomId: ZO.ZOOM_ID[k], start: 0, end: 100 }));
    if (batch.length) chart.dispatchAction({ type: 'dataZoom', batch });
    NS.ticks.zoomed(slot, slot.zoom);
  };

  /** @param {ChartSlot} slot */
  const showLinks = (slot) => { slot.resetBtn.hidden = !slot.zoom; };

  /** @param {ChartView} view @param {ChartSlot} slot */
  const reset = (view, slot) => {
    slot.zoom = null;
    apply(slot);
    showLinks(slot);
  };

  const DAY_MS = 864e5;
  /** A window widened to tidy ends, so the axis does not label a raw value:
   * whole days on a time axis, else a hundredth of the span's order.
   * @param {number[]} v @param {string} type */
  const tidy = (v, type) => {
    const span = v[1] - v[0];
    if (!(span > 0)) return v;
    if (type === 'log') return v.map((x) => Number(x.toPrecision(2)));
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
    showLinks(slot);
  };

  const ICONS = {
    select: '<svg width="14" height="14" viewBox="0 0 16 16" aria-hidden="true"><rect x="2.5" y="2.5" ' +
      'width="11" height="11" rx="1" fill="none" stroke="currentColor" stroke-dasharray="2 1.6"/></svg>',
    zoom: '<svg width="14" height="14" viewBox="0 0 16 16" aria-hidden="true" fill="none" ' +
      'stroke="currentColor" stroke-width="1.4" stroke-linecap="round"><circle cx="6.8" cy="6.8" r="4.3"/>' +
      '<path d="M10 10l3.8 3.8M4.8 6.8h4M6.8 4.8v4"/></svg>'
  };

  /** The Select / Zoom switch in the header row, while the view draws a
   * brushable scatter: blockr.ui's segmented control at row size, icons
   * with their names as tooltips.
   * @param {ChartView} view */
  const modeSwitch = (view) => {
    const show = view._slots.some((s) => s && s.brushable);
    let el = view._modeEl;
    if (!el) {
      if (!show) return;
      el = view._modeEl = document.createElement('span');
      el.className = 'blockr-segmented blockr-segmented--xs vz-chart-zoom-mode';
      el.setAttribute('role', 'group');
      el.setAttribute('aria-label', 'Drag selects or zooms');
      for (const [on, name] of /** @type {[boolean, 'select' | 'zoom'][]} */ ([[false, 'select'], [true, 'zoom']])) {
        const b = document.createElement('button');
        b.type = 'button';
        b.className = 'blockr-segmented__seg';
        b.innerHTML = ICONS[name];
        b.title = name === 'zoom' ? 'Drag to zoom' : 'Drag to select';
        b.setAttribute('aria-label', b.title);
        b.dataset.zoom = String(on);
        view.scope.on(b, 'click', () => setMode(view, on));
        el.appendChild(b);
      }
      view.frame.tools.prepend(el);
    }
    el.hidden = !show;
    for (const b of Array.from(el.children)) {
      const on = /** @type {HTMLElement} */ (b).dataset.zoom === String(view.zoomOn === true);
      b.classList.toggle('is-selected', on);
      b.setAttribute('aria-pressed', String(on));
    }
  };

  /** Give the scatter drag to the zoom (true) or the brush.
   * @param {ChartView} view @param {boolean} on */
  const setMode = (view, on) => {
    view.zoomOn = on;
    for (const slot of view._slots) {
      if (slot && slot.chart && slot.brushable) {
        slot.chart.dispatchAction({ type: 'takeGlobalCursor', key: 'brush',
                                    brushOption: { brushType: on ? false : 'rect', brushMode: 'single' } });
      }
    }
    modeSwitch(view);
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
    showLinks(slot);
    if (i === 0) modeSwitch(view);
  };

  NS.zoom = { attach, after, reset, setMode, justDragged, spans, tidy };
})();
