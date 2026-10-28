// @ts-check
/**
 * Chart block: dates on a time axis. A date in ms, the period a column is
 * sampled at, an x value or span as a tooltip prints it. Starts NS.axes.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** A date in ms: a number as is, an ISO date as UTC midnight, an R date-time string ("2020-03-01
   * 12:30:00") as that wall-clock time in UTC. @param {any} v @returns {number} */
  const timeValue = (v) => {
    if (v == null || v === '') return NaN;
    if (typeof v === 'number') return v;
    const s = String(v);
    if (/^\d{4}-\d{2}-\d{2}$/.test(s)) return Date.parse(s + 'T00:00:00Z');
    const t = s.replace(' ', 'T');
    return Date.parse(/[zZ]|[+-]\d{2}:?\d{2}$/.test(t) ? t : t + 'Z');
  };

  /** The period a time column is sampled at, from the median gap between its distinct values:
   * 'year', 'quarter', 'month', 'day' or 'time'. @param {any[]} rows @param {string} col
   * @returns {string} */
  const timePeriod = (rows, col) => {
    const seen = new Set();
    for (const r of rows || []) {
      const t = timeValue(r[col]);
      if (Number.isFinite(t)) seen.add(t);
      if (seen.size > 2000) break;
    }
    const ts = [...seen].sort((a, b) => a - b);
    if (ts.length < 2) return 'day';
    const gaps = [];
    for (let i = 1; i < ts.length; i++) gaps.push(ts[i] - ts[i - 1]);
    gaps.sort((a, b) => a - b);
    const days = gaps[gaps.length >> 1] / 86400000;
    if (days >= 360) return 'year';
    if (days >= 85) return 'quarter';
    if (days >= 27) return 'month';
    return days >= 0.99 ? 'day' : 'time';
  };

  const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];

  /**
   * An x value for a tooltip: a category, a date named by its period, or a
   * trimmed number.
   * @param {any} v @param {string} xAxisType @param {any[] | null} xCats
   * @param {string | null} [period]
   */
  const fmtXVal = (v, xAxisType, xCats, period) => {
    if (v == null || v === '') return '';
    if (xAxisType === 'category') {
      return (xCats && xCats[v] != null) ? String(xCats[v]) : String(v);
    }
    if (xAxisType === 'time') {
      const d = new Date(timeValue(v));
      if (isNaN(d.getTime())) return String(v);
      const y = d.getUTCFullYear(), mo = d.getUTCMonth();
      if (period === 'year') return String(y);
      if (period === 'quarter') return 'Q' + (Math.floor(mo / 3) + 1) + ' ' + y;
      if (period === 'month') return MONTHS[mo] + ' ' + y;
      if (period === 'time') return d.toISOString().slice(0, 16).replace('T', ' ');
      return d.toISOString().slice(0, 10);
    }
    return V.util.num(Number(v));
  };

  /** The span between two x values: days on a time axis, units on a value axis, nothing on a
   * category axis. @param {any} a @param {any} b @param {string} xAxisType */
  const durationStr = (a, b, xAxisType) => {
    if (xAxisType === 'category') return '';
    const s = Number(a), e = Number(b);
    if (isNaN(s) || isNaN(e)) return '';
    if (xAxisType === 'time') {
      const days = Math.round((e - s) / 86400000);
      return days + (Math.abs(days) === 1 ? ' day' : ' days');
    }
    return String(Math.round((e - s) * 100) / 100);
  };

  NS.axes = Object.assign(NS.axes || {}, { timeValue, timePeriod, fmtXVal, durationStr });
})();
