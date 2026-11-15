// @ts-check
/**
 * The ticks of a numeric swimlane axis, spaced by the column's drawn width.
 * R sends the axis with ticks for no particular width and, for a numeric
 * swimlane, its domain and unit (R/table-groups-cols.R table_groups_axis());
 * once the table is laid out, fit() replaces the ticks with the finest step
 * of the 1 / 2 / 2.5 / 5 ladder that keeps labels 60px apart, the first
 * after the unit ("Day 0"). A study-day axis (`x_days`) labels its ticks
 * "D1", "D20", with day 1 always a tick and no day 0 (dayTicks()). A date
 * axis keeps R's ticks.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const esc = V.util.esc;

  /** The steps, times a power of ten. */
  const STEPS = [1, 2, 2.5, 5];

  /**
   * The ticks across `d0` to `d1` drawn `w` px wide, labels `gap` px apart.
   * @param {number} d0 @param {number} d1 @param {number} w @param {number} [gap]
   * @returns {number[]}
   */
  function ticks(d0, d1, w, gap = 60) {
    const range = d1 - d0;
    if (!(range > 0) || !(w > 0)) return [];
    const min = range * gap / w;
    let mag = Math.pow(10, Math.floor(Math.log10(min)));
    let step = 0;
    while (!step) {
      for (const s of STEPS) if (!step && s * mag >= min) step = s * mag;
      mag *= 10;
    }
    /** @type {number[]} */
    const out = [];
    for (let x = Math.ceil(d0 / step - 1e-9) * step; x <= d1 + 1e-9; x += step) {
      out.push(Math.round(x * 1e6) / 1e6 || 0);
    }
    return out;
  }

  /**
   * Study-day ticks across `d0` to `d1` drawn `w` px wide: whole-day steps of
   * the 1 / 2 / 5 ladder, day 1 always among them and a round tick within
   * three fifths of a step of it dropped, as the patient profile's axis
   * (R's cells_axis_day_ticks()).
   * @param {number} d0 @param {number} d1 @param {number} w @param {number} [gap]
   * @returns {number[]}
   */
  function dayTicks(d0, d1, w, gap = 60) {
    const range = d1 - d0;
    if (!(range > 0) || !(w > 0)) return [];
    const min = Math.max(1, range * gap / w);
    let mag = Math.pow(10, Math.floor(Math.log10(min)));
    let step = 0;
    while (!step) {
      for (const s of [1, 2, 5]) if (!step && s * mag >= min) step = s * mag;
      mag *= 10;
    }
    /** @type {number[]} */
    let out = [];
    for (let x = Math.ceil(d0 / step - 1e-9) * step; x <= d1 + 1e-9; x += step) out.push(Math.round(x));
    if (d0 <= 1 && d1 >= 1) {
      out = out.filter((x) => Math.abs(x - 1) >= 0.6 * step).concat(1).sort((a, b) => a - b);
    }
    return out;
  }

  /** A study day's label: x = 1 is "D1", x = 0 the day before, "D-1".
   * @param {number} x */
  const dayLabel = (x) => 'D' + String(x >= 1 ? x : x - 1);

  /**
   * Fit every numeric swimlane axis in `host` to its width; an axis is
   * redrawn only when its width changed.
   * @param {ParentNode} host
   */
  function fit(host) {
    const axes = /** @type {HTMLElement[]} */ (Array.from(host.querySelectorAll('.vz-table-groups-axis-in[data-d1]')));
    // Every width read before any write, so the table is laid out once.
    const ws = axes.map((el) => el.clientWidth);
    axes.forEach((el, k) => {
      const w = ws[k];
      if (!w || el.getAttribute('data-w') === String(w)) return;
      const d0 = Number(el.getAttribute('data-d0'));
      const d1 = Number(el.getAttribute('data-d1'));
      const unit = el.getAttribute('data-unit');
      const days = el.getAttribute('data-days') === '1';
      const at = days ? dayTicks(d0, d1, w) : ticks(d0, d1, w);
      if (at.length < 2) return;
      el.setAttribute('data-w', String(w));
      el.innerHTML = at.map((x, j) => {
        const p = Math.round((x - d0) / (d1 - d0) * 1e4) / 100;
        const text = days ? dayLabel(x) : (j === 0 && unit ? unit + ' ' : '') + String(x);
        return '<span' + (p <= 8 ? ' class="is-first"' : p >= 92 ? ' class="is-last"' : '') +
          ' style="left:' + p + '%">' + esc(text) + '</span>';
      }).join('');
    });
  }

  V.table.axis = { ticks, dayTicks, dayLabel, fit };
})();
