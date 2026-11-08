// @ts-check
/**
 * Chart block: `x_ticks` and `y_ticks` on the axes. The values are read in
 * core/ticks.js; here they reach ECharts as `customValues` on the axis
 * labels and ticks, which the split lines follow.
 *
 *   - decorateAxis() runs with the limits (zoom-option.js): a step rounds an
 *     auto end of a value axis out to its multiples, and a day or week step
 *     starts a date axis at its first date, so the ticks start there.
 *   - after() runs once the chart is drawn and the panels share their axes:
 *     it reads each axis's extent and sets the ticks over it. A step needs
 *     the extent, which ECharts knows only after it has laid out the data.
 *   - zoomed() runs with the zoom: ECharts draws the custom ticks inside the
 *     window; with fewer than two of them there, the axis draws its own until
 *     the reset.
 * An axis whose setting cannot apply (a step on a log axis, too many ticks)
 * draws its own ticks, the ones its option brought. A date axis keeps its level formatter, which labels
 * a custom tick by its date (a year, a month, a day); ECharts 6 throws on a
 * function formatter there, as custom ticks carry no level.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const T = V.ticks;

  const KEYS = /** @type {('xAxis' | 'yAxis')[]} */ (['xAxis', 'yAxis']);

  /** The ticks setting the type takes for a role, or null.
   * @param {Record<string, any>} cfg @param {'x' | 'y' | null} role */
  const settingOf = (cfg, role) => {
    if (!role) return null;
    const key = role + '_ticks';
    const spec = /** @type {any} */ (V.spec && V.spec.chart)?.[key];
    const a = spec && spec.applies;
    if (a && a.only && !a.only.includes(cfg.type)) return null;
    if (a && a.except && a.except.includes(cfg.type)) return null;
    return T.parse(cfg[key]) ? cfg[key] : null;
  };

  /**
   * An axis with its ticks setting before the draw: a step on a value axis
   * rounds each auto end out to its multiples (an axis that starts at zero,
   * a bar's, keeps zero in); a day or week step starts a date axis at its
   * first date. Ends set by the limits stay.
   * @param {any} ax @param {Record<string, any>} cfg @param {'x' | 'y' | null} role
   * @param {string} type @param {[number | null, number | null]} limits
   */
  const decorateAxis = (ax, cfg, role, type, limits) => {
    const setting = settingOf(cfg, role);
    const p = setting ? T.parse(setting) : null;
    if (!p || p.form !== 'every') return ax;
    if (type === 'time' && (!p.unit || p.unit === 'day' || p.unit === 'week')) {
      if (limits[0] == null && ax.min == null) ax.min = (/** @type {any} */ e) => e.min;
      return ax;
    }
    if (type !== 'value') return ax;
    const zero = ax.scale !== true;
    /** @param {0 | 1} i @param {number} v */
    const out = (i, v) => {
      const r = T.roundOut(setting, v, v);
      return r ? r[i] : v;
    };
    if (limits[0] == null) {
      ax.min = typeof ax.min === 'number' ? out(0, ax.min)
        : (/** @type {any} */ e) => out(0, zero ? Math.min(0, e.min) : e.min);
    }
    if (limits[1] == null) {
      ax.max = typeof ax.max === 'number' ? out(1, ax.max)
        : (/** @type {any} */ e) => out(1, zero ? Math.max(0, e.max) : e.max);
    }
    return ax;
  };

  /** An axis's extent as drawn: the min and max a shared scale set (those
   * wait for the next frame), else the scale's.
   * @param {any} comp @returns {[number, number] | null} */
  const extentOf = (comp) => {
    const ext = comp.axis && comp.axis.scale ? comp.axis.scale.getExtent() : null;
    const lo = comp.get('min'), hi = comp.get('max');
    const a = typeof lo === 'number' ? lo : (ext ? ext[0] : NaN);
    const b = typeof hi === 'number' ? hi : (ext ? ext[1] : NaN);
    return Number.isFinite(a) && Number.isFinite(b) && b > a ? [a, b] : null;
  };

  /** The axis option that draws `vals` as its ticks, or its own when null.
   * @param {number[] | null} vals */
  const axisPatch = (vals) => ({ axisLabel: { customValues: vals }, axisTick: { customValues: vals } });

  /**
   * After a draw: each axis with a ticks setting takes its tick values over
   * the extent it was drawn at. The values stay on the slot for the zoom.
   * @param {ChartView} view @param {ChartSlot} slot
   */
  const after = (view, slot) => {
    const chart = slot.chart;
    const roles = chart && chart.__vzRoles;
    /** @type {Record<string, { vals: number[], own: number[] | null } | null>} */
    const kept = {};
    /** @type {Record<string, any>} */
    const patch = {};
    for (const key of KEYS) {
      const setting = roles ? settingOf(view.settings, roles[key]) : null;
      if (!setting) continue;
      const comp = chart.getModel().getComponent(key, 0);
      if (!comp) continue;
      const ext = extentOf(comp);
      const vals = ext ? T.values(setting, ext[0], ext[1], String(comp.get('type'))) : null;
      // The ticks the option brought (a log axis's 1-2-5), for when these cannot apply.
      const tick = comp.get('axisTick');
      const own = tick && Array.isArray(tick.customValues) ? tick.customValues : null;
      kept[key] = vals ? { vals, own } : null;
      if (vals) patch[key] = [axisPatch(vals)];
    }
    slot.ticks = Object.keys(kept).length ? kept : null;
    if (Object.keys(patch).length) chart.setOption(patch, { lazyUpdate: true });
  };

  /**
   * With the zoom window: the ticks inside it, or the axis's own while fewer
   * than two of them are.
   * @param {ChartSlot} slot @param {Record<string, number[] | null> | null} zoom
   */
  const zoomed = (slot, zoom) => {
    if (!slot.ticks || !slot.chart) return;
    /** @type {Record<string, any>} */
    const patch = {};
    for (const key of KEYS) {
      const t = slot.ticks[key];
      if (!t) continue;
      const w = zoom && zoom[key];
      const inView = w ? t.vals.filter((v) => v >= w[0] && v <= w[1]) : t.vals;
      patch[key] = [axisPatch(inView.length >= 2 ? t.vals : t.own)];
    }
    if (Object.keys(patch).length) slot.chart.setOption(patch);
  };

  /** The tick values an axis draws now, for a form picked in the gear or the
   * sentence to start from; [] when unknown.
   * @param {ChartView} view @param {'x' | 'y'} role @returns {{ vals: number[], type: string }} */
  const drawn = (view, role) => {
    const slot = view._slots.find((s) => s && s.chart);
    const roles = slot && slot.chart.__vzRoles;
    const key = roles ? KEYS.find((k) => roles[k] === role) : null;
    const comp = slot && key ? slot.chart.getModel().getComponent(key, 0) : null;
    if (!comp || !comp.axis) return { vals: [], type: 'value' };
    const type = String(comp.get('type'));
    /** @type {number[]} */
    let vals = [];
    try {
      vals = comp.axis.getTicksCoords().map((/** @type {any} */ t) => Number(t.tickValue))
        .filter(Number.isFinite);
    } catch (e) { vals = []; }
    return { vals, type };
  };

  /** The axis a ticks setting is on ('time', 'log' or 'value') and the ticks
   * it draws now, for the gear (VizGearHost.ticks). A log scale set in the
   * gear counts before the chart redraws with it; before the first draw the
   * x column says whether the axis holds dates.
   * @param {ChartView} view @param {string} key @param {Record<string, any>} v
   * @returns {VizTicksAxis} */
  const axisFor = (view, key, v) => {
    const role = key === 'x_ticks' ? 'x' : 'y';
    const d = drawn(view, role);
    if (!d.vals.length && role === 'x' && v.x) {
      d.type = NS.axes.axisTypeFor(view.columns, view.data, v.x, { temporal: true });
    }
    const tk = role + '_transform';
    const spec = /** @type {any} */ (V.spec.chart)[tk];
    const only = spec && spec.applies && spec.applies.only;
    const log = v[tk] === 'log10' && (!only || only.includes(v.type));
    return { type: log ? 'log' : (d.type === 'log' ? 'value' : d.type), drawn: d.vals };
  };

  NS.ticks = { settingOf, decorateAxis, after, zoomed, drawn, axisFor };
})();
