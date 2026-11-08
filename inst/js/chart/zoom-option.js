// @ts-check
/**
 * Chart block: axis limits and the zoom window in the option. Every family's
 * option passes through decorate() before it is drawn:
 *
 *   - `x_ticks` and `y_ticks` round an auto end to their step (ticks.js).
 *   - `x_limits` and `y_limits` become the axis min and max. `y_limits` is
 *     the value axis: y on the individual types, whichever axis is not a
 *     category on the aggregated ones. Marks outside are clipped.
 *   - Every continuous axis that can zoom gets a dataZoom component with no
 *     interaction of its own; zoom.js moves it from the drag. On the
 *     aggregated types only the value axis zooms. The window is relative to
 *     the axis extent, so a reset returns to the limits.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** The dataZoom component of each axis. */
  const ZOOM_ID = { xAxis: 'vz-zoom-x', yAxis: 'vz-zoom-y' };

  /** @param {any} a */
  const first = (a) => (Array.isArray(a) ? a[0] : a);

  /** An axis type as ECharts reads it: x is a category unless it says not.
   * @param {any} ax @param {'xAxis' | 'yAxis'} key */
  const axisType = (ax, key) => (ax && ax.type) || (key === 'xAxis' ? 'category' : 'value');

  /**
   * Which limits each axis takes ('x', 'y' or null), or null for a chart
   * without axes.
   * @param {any} option @param {Record<string, any>} cfg
   * @returns {{ xAxis: 'x' | 'y' | null, yAxis: 'x' | 'y' | null } | null}
   */
  const roles = (option, cfg) => {
    const x = first(option && option.xAxis), y = first(option && option.yAxis);
    if (!x || !y) return null;
    const cx = axisType(x, 'xAxis') !== 'category', cy = axisType(y, 'yAxis') !== 'category';
    if (NS.familyOf(cfg.type) === 'aggregated') {
      if (cy) return { xAxis: null, yAxis: 'y' };
      return cx ? { xAxis: 'y', yAxis: null } : null;
    }
    return { xAxis: cx ? 'x' : null, yAxis: cy ? 'y' : null };
  };

  /** The limits the type takes for a role, or null.
   * @param {Record<string, any>} cfg @param {'x' | 'y'} role */
  const limitsOf = (cfg, role) => {
    const key = role + '_limits';
    const spec = V.spec && V.spec.chart && V.spec.chart[key];
    const a = spec && spec.applies;
    if (a && a.only && !a.only.includes(cfg.type)) return null;
    if (a && a.except && a.except.includes(cfg.type)) return null;
    const v = cfg[key];
    return Array.isArray(v) && v.length === 2 ? v : null;
  };

  /** Whether a role has a limit set, either end.
   * @param {Record<string, any>} cfg @param {'x' | 'y' | null} role */
  const hasLimits = (cfg, role) => {
    const lim = role ? limitsOf(cfg, role) : null;
    return !!lim && lim.some((v) => v != null && v !== '');
  };

  /** A limit as the axis reads it: a date as UTC milliseconds on a time
   * axis; nothing at or below zero on a log axis.
   * @param {unknown} v @param {string} type @returns {number | null} */
  const axisValue = (v, type) => {
    if (v == null || v === '') return null;
    let n = typeof v === 'number' ? v : Number(v);
    if (!Number.isFinite(n) && typeof v === 'string' && type === 'time') n = Date.parse(v);
    if (!Number.isFinite(n)) return null;
    if (type === 'log' && n <= 0) return null;
    return n;
  };

  /** The from and to of a role's limits on an axis, each null for auto.
   * @param {Record<string, any>} cfg @param {'x' | 'y' | null} role @param {string} type
   * @returns {[number | null, number | null]} */
  const ends = (cfg, role, type) => {
    const lim = role ? limitsOf(cfg, role) : null;
    return lim ? [axisValue(lim[0], type), axisValue(lim[1], type)] : [null, null];
  };

  /**
   * The option with the limits on its axes and a zoom component per
   * zooming axis. The option passed in is not changed (the memo keeps it).
   * @param {any} option @param {Record<string, any>} cfg
   * @returns {{ option: any, roles: ReturnType<typeof roles>, filterX: boolean,
   *            logEnds: Record<string, (number | null)[]> }}
   */
  const decorate = (option, cfg) => {
    const r = roles(option, cfg);
    /** The limit ends of each log axis: zoom.js zooms those through min and max.
     * @type {Record<string, (number | null)[]>} */
    const logEnds = {};
    if (!r) return { option, roles: null, filterX: false, logEnds };
    const out = { ...option };
    /** @type {any[]} */
    const zoom = [];
    let filterX = false;
    for (const key of /** @type {('xAxis' | 'yAxis')[]} */ (['xAxis', 'yAxis'])) {
      const role = r[key];
      if (!role) continue;
      const many = Array.isArray(option[key]);
      const axes = many ? option[key].slice() : [option[key]];
      const ax = { ...axes[0] };
      const [lo, hi] = ends(cfg, role, axisType(ax, key));
      if (axisType(ax, key) === 'log') logEnds[key] = [lo, hi];
      if (lo != null) ax.min = lo;
      if (hi != null) ax.max = hi;
      NS.ticks.decorateAxis(ax, cfg, role, axisType(ax, key), [lo, hi]);
      axes[0] = ax;
      out[key] = many ? axes : ax;
      // x filters what it leaves out, so a flat drag refits y; y only moves.
      const filter = key === 'xAxis' && NS.familyOf(cfg.type) === 'individual' &&
        cfg.type !== 'band';
      if (filter) filterX = true;
      zoom.push({
        type: 'inside', id: ZOOM_ID[key], [key === 'xAxis' ? 'xAxisIndex' : 'yAxisIndex']: 0,
        filterMode: filter ? 'filter' : 'none',
        zoomOnMouseWheel: false, moveOnMouseMove: false, moveOnMouseWheel: false,
        preventDefaultMouseMove: false
      });
    }
    if (zoom.length) {
      out.dataZoom = zoom;
      // A custom series (a box, an interval) does not clip unless told.
      if (Array.isArray(option.series)) {
        out.series = option.series.map((/** @type {any} */ s) =>
          (s && s.type === 'custom' ? { ...s, clip: true } : s));
      }
    }
    return { option: out, roles: r, filterX, logEnds };
  };

  NS.zoomOption = { decorate, roles, ends, axisValue, hasLimits, ZOOM_ID };
})();
