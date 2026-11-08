// @ts-check
/**
 * Axis ticks (`x_ticks`, `y_ticks`), read the way R checks them
 * (check_ticks() in R/spec-check.R): one value is a step, a number or a
 * count and a date unit ("7 days"); two or more are the ticks themselves.
 * The gear (gear/ticks.js), the sentence (gear/engine.js) and the chart
 * (chart/ticks.js) all read and write the value through here.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  const UNITS = ['day', 'week', 'month', 'quarter', 'year'];
  /** Months per unit; days and weeks step by a fixed length instead. */
  const MONTHS = /** @type {Record<string, number>} */ ({ month: 1, quarter: 3, year: 12 });
  const DAY_MS = 864e5;
  /** More ticks than this on one axis and the axis draws its own. */
  const MAX_TICKS = 60;
  const LOG_NOTE = 'A log axis takes a list, such as 1, 10, 100.';
  const LIST_NOTE = 'A list needs two values or more.';

  /** @param {number} x */
  const tidy = (x) => Number(x.toPrecision(12));

  /**
   * The value as parts: a step (`n`, and a date `unit` or null), a list, or
   * null for auto.
   * @param {unknown} v
   * @returns {{ form: 'every', n: number, unit: string | null } | { form: 'at', list: (number | string)[] } | null}
   */
  const parse = (v) => {
    if (v == null || v === '') return null;
    if (Array.isArray(v)) {
      const list = v.filter((x) => x != null && x !== '');
      return list.length ? { form: 'at', list: /** @type {(number | string)[]} */ (list) } : null;
    }
    if (typeof v === 'number') return v > 0 ? { form: 'every', n: v, unit: null } : null;
    const m = /^(\d*\.?\d+)?\s*(day|week|month|quarter|year)s?$/i.exec(String(v).trim());
    if (m) return { form: 'every', n: m[1] ? Number(m[1]) : 1, unit: m[2].toLowerCase() };
    const n = Number(v);
    return Number.isFinite(n) && n > 0 ? { form: 'every', n, unit: null } : null;
  };

  /** A step as the value R keeps: a number, or "<n> <unit>s".
   * @param {number} n @param {string | null} unit */
  const step = (n, unit) => (unit ? n + ' ' + unit + (n === 1 ? '' : 's') : n);

  /** A unit as a word after its count: "day", "days". @param {string} unit @param {number} n */
  const unitWord = (unit, n) => unit + (n === 1 ? '' : 's');

  /** The value as the sentence prints it (slot_ticks_text() in R).
   * @param {unknown} v */
  const text = (v) => {
    const p = parse(v);
    if (!p) return 'auto';
    if (p.form === 'at') return 'at ' + p.list.join(', ');
    if (!p.unit) return 'every ' + p.n;
    return 'every ' + (p.n === 1 ? p.unit : p.n + ' ' + unitWord(p.unit, p.n));
  };

  /** A list as typed, "0, 25, 50": numbers where they read as numbers.
   * @param {string} t @returns {(number | string)[]} */
  const parseList = (t) => t.split(/[,;]/).map((x) => x.trim()).filter(Boolean)
    .map((x) => x.replace(/^−/, '-'))
    .map((x) => (Number.isFinite(Number(x)) ? Number(x) : x));

  /** Text typed for the setting back to a value: "auto", "25", "every 7
   * days", "month", "at 0, 25, 50" or "0, 25, 50".
   * @param {string} t @returns {unknown} */
  const fromText = (t) => {
    const s = String(t ?? '').trim().replace(/^every\s+/i, '');
    if (!s || /^auto$/i.test(s)) return null;
    if (/^at\s+/i.test(s) || /[,;]/.test(s)) {
      const list = parseList(s.replace(/^at\s+/i, ''));
      return list.length > 1 ? list : (list.length ? list[0] : null);
    }
    const p = parse(s);
    return p && p.form === 'every' ? step(p.n, p.unit) : null;
  };

  /** Why a value cannot go on its axis, or null: a step on a log axis, a list
   * of one.
   * @param {unknown} v @param {boolean} log @returns {string | null} */
  const note = (v, log) => {
    const p = parse(v);
    if (!p) return null;
    if (p.form === 'every' && log) return LOG_NOTE;
    if (p.form === 'at' && p.list.length < 2) return LIST_NOTE;
    return null;
  };

  /** A tick value as the axis reads it: a date as UTC milliseconds.
   * @param {number | string} v @param {string} type @returns {number | null} */
  const axisValue = (v, type) => {
    let n = typeof v === 'number' ? v : Number(v);
    if (!Number.isFinite(n) && type === 'time') n = Date.parse(String(v));
    if (!Number.isFinite(n)) return null;
    if (type === 'log' && n <= 0) return null;
    return n;
  };

  /** The first day of the month at or after `ms`, in UTC. @param {number} ms */
  const monthStart = (ms) => {
    const d = new Date(ms);
    const at = Date.UTC(d.getUTCFullYear(), d.getUTCMonth(), 1);
    return at < ms ? Date.UTC(d.getUTCFullYear(), d.getUTCMonth() + 1, 1) : at;
  };

  /**
   * The tick values over [lo, hi] on an axis of `type` ('value', 'time' or
   * 'log'), or null when the axis should draw its own: unset, a step on a
   * log axis, fewer than two ticks in view, or more than MAX_TICKS.
   * Numbers step from the multiples of the step; days and weeks from `lo`;
   * months, quarters and years from the first of a month.
   * @param {unknown} v @param {number} lo @param {number} hi @param {string} type
   * @returns {number[] | null}
   */
  const values = (v, lo, hi, type) => {
    const p = parse(v);
    if (!p || !(hi > lo)) return null;
    /** @type {number[]} */
    let out = [];
    if (p.form === 'at') {
      out = /** @type {number[]} */ (p.list.map((x) => axisValue(x, type))
        .filter((x) => x != null && x >= lo && x <= hi));
    } else if (type === 'log') {
      return null;
    } else if (type === 'time' && p.unit && MONTHS[p.unit]) {
      const by = p.n * MONTHS[p.unit];
      const d0 = new Date(monthStart(lo));
      // Quarters and years start in January, April, July and October.
      const shift = p.unit === 'month' ? 0 : (12 - (d0.getUTCMonth() % MONTHS[p.unit])) % MONTHS[p.unit];
      for (let k = shift; out.length <= MAX_TICKS; k += by) {
        const t = Date.UTC(d0.getUTCFullYear(), d0.getUTCMonth() + k, 1);
        if (t > hi) break;
        out.push(t);
      }
    } else if (type === 'time') {
      const by = p.n * (p.unit === 'week' ? 7 : 1) * DAY_MS;
      for (let t = lo; t <= hi && out.length <= MAX_TICKS; t += by) out.push(t);
    } else {
      const s = p.n;
      if ((hi - lo) / s > MAX_TICKS) return null;
      for (let k = Math.ceil(tidy(lo / s)); tidy(k * s) <= hi; k++) out.push(tidy(k * s));
    }
    return out.length >= 2 && out.length <= MAX_TICKS ? out : null;
  };

  /** A step's tidy ends around [lo, hi] on a value axis, the auto ends of an
   * axis with a step; null for a list, a date axis or no step.
   * @param {unknown} v @param {number} lo @param {number} hi @returns {[number, number] | null} */
  const roundOut = (v, lo, hi) => {
    const p = parse(v);
    if (!p || p.form !== 'every' || p.unit || !Number.isFinite(lo) || !Number.isFinite(hi)) return null;
    const s = p.n;
    return [tidy(Math.floor(tidy(lo / s)) * s), tidy(Math.ceil(tidy(hi / s)) * s)];
  };

  /** The value a form starts with when picked in the gear or the sentence,
   * from the ticks drawn now: their step, or their list.
   * @param {'every' | 'at'} form @param {number[]} drawn @param {string} type */
  const start = (form, drawn, type) => {
    const ok = (drawn || []).filter(Number.isFinite);
    if (form === 'at') {
      const list = ok.length >= 2 ? ok : [0, 1];
      return type === 'time' ? list.map((t) => new Date(t).toISOString().slice(0, 10)) : list;
    }
    const d = ok.length >= 2 ? tidy(ok[1] - ok[0]) : 0;
    if (type === 'time') {
      const days = Math.max(1, Math.round(d / DAY_MS));
      if (days >= 360) return step(Math.max(1, Math.round(days / 365)), 'year');
      if (days >= 28) return step(Math.max(1, Math.round(days / 30)), 'month');
      return days % 7 === 0 ? step(days / 7, 'week') : step(days, 'day');
    }
    return d > 0 ? d : 1;
  };

  V.ticks = { UNITS, MAX_TICKS, LOG_NOTE, LIST_NOTE, parse, step, unitWord, text, fromText,
              parseList, note, values, roundOut, start, axisValue };
})();
