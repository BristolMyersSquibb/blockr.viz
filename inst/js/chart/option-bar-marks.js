// @ts-check
/**
 * Chart block: what a bar carries besides its bar. The sizes every bar
 * shares, the rounded value end, value labels at a bar's end, text on the
 * marks (`label`), the level names on a stack (`legend = "marks"`), and the
 * names of the highlighted bars in semibold. Pure: text is measured with
 * the function the caller passes.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});
  const option = /** @type {any} */ (NS.option = NS.option || {});
  const parts = /** @type {any} */ (option.aggParts = option.aggParts || {});

  // Waterfall sign colours (new_waterfall_block's defaults): semantic, so
  // they override the palette.
  const WATERFALL_COLORS = { increase: '#009E73', decrease: '#dc2626', total: '#bbbbbb' };

  // The corner radius of a bar's value end (--blockr-mark-radius). The end
  // at zero is the axis and stays square; a floating step rounds both ends.
  const MARK_RADIUS = 2;
  /** @param {boolean} vertical @param {boolean} [floating] @returns {number[]} */
  const barRadius = (vertical, floating) => {
    const r = MARK_RADIUS;
    if (floating) return [r, r, r, r];
    return vertical ? [r, r, 0, 0] : [0, r, r, 0];
  };

  const BAR_MAX = 48;
  const ROW_BAND = 28;     // horizontal row height, one bar per row
  const GROUP_BAR = 14;    // horizontal grouped: px per series bar
  const GROUP_PAD = 12;    // horizontal grouped: gap between groups
  // The category inset a vertical plot loses to the grid's margins.
  const X_INSET = 65;

  /**
   * Value labels on a bar (`value_labels`), at the bar's own end. A stack
   * is labelled once with its total, a grouped bar per bar, a percent bar
   * per segment inside it where the share fits. Colliding labels hide. The
   * value axis gets 12% headroom on the sides the bars reach. Returns the
   * px a grid needs on its right for the widest label. Mutates `series`
   * and `valAxis`.
   * @param {any[]} series @param {any} valAxis
   * @param {{ax: any, vertical: boolean, stacked: boolean, percent: boolean,
   *          fraction: boolean, plotPx: number, bandPx: number,
   *          measure: (s: string) => number, face: string,
   *          cfg: Record<string, any>}} o
   * @returns {number}
   */
  const barValueLabels = (series, valAxis, o) => {
    const textW = o.measure;
    /** @param {any} d */
    const valOf = (d) => (d != null && typeof d === 'object') ? d.value : d;
    const vf = NS.valueFormat(o.cfg);
    // `|| 0` folds a negative zero, which would print as "-0".
    const fmt = o.fraction
      ? (/** @type {number} */ v) => (Math.round(v * 1000) / 10 || 0) + '%'
      : (vf || ((/** @type {number} */ v) => String(NS.ddNum(v || 0))));
    const style = { color: o.ax.labelColor, fontSize: o.ax.fontSize, fontFamily: o.face };

    if (o.percent) {
      const barW = Math.min(48, 0.6 * o.bandPx);
      /** @param {number} v @param {string} t */
      const fits = (v, t) => !o.plotPx || (o.vertical
        ? v * o.plotPx >= o.ax.fontSize + 4 && (!o.bandPx || barW >= textW(t) + 4)
        : v * o.plotPx >= textW(t) + 6);
      for (const s of series) {
        s.label = { show: true, position: 'inside', ...style, color: '#fff',
          formatter: (/** @type {any} */ p) => {
            const v = Number(p.value);
            if (!Number.isFinite(v)) return '';
            const t = Math.round(v * 100) + '%';
            return fits(v, t) ? t : '';
          } };
        s.labelLayout = { hideOverlap: true };
      }
      return 0;
    }

    /** @type {number[]} */
    const ends = [];
    for (const s of series) {
      s.label = { show: !o.stacked, position: 'outside', distance: 4, ...style,
        formatter: (/** @type {any} */ p) => p.value == null ? '' : fmt(Number(p.value)) };
      s.labelLayout = { hideOverlap: true };
      if (!o.stacked) {
        for (const d of s.data) {
          const v = valOf(d);
          if (v != null) ends.push(Number(v));
        }
      }
    }
    if (o.stacked) {
      const n = series.length ? series[0].data.length : 0;
      for (let gi = 0; gi < n; gi++) {
        let tot = 0, any = false;
        for (const s of series) {
          const v = valOf(s.data[gi]);
          if (v != null) { tot += Number(v); any = true; }
        }
        if (!any) continue;
        ends.push(tot);
        // The label rides the outermost segment of the total's sign.
        for (let si = series.length - 1; si >= 0; si--) {
          const d = series[si].data[gi];
          const v = valOf(d);
          if (v == null || (tot < 0 ? v > 0 : v < 0)) continue;
          series[si].data[gi] = {
            ...((typeof d === 'object') ? d : { value: d }),
            label: { show: true, formatter: fmt(tot) }
          };
          break;
        }
      }
    }

    const hasPos = ends.some((v) => v > 0);
    const hasNeg = ends.some((v) => v < 0);
    valAxis.boundaryGap = [hasNeg ? '12%' : 0, hasPos ? '12%' : 0];
    let w = 0;
    for (const v of ends) {
      if (o.vertical || v > 0) w = Math.max(w, textW(fmt(v)));
    }
    // A vertical bar's label is centred on it and may overhang by half.
    if (o.vertical) return Math.max(0, Math.ceil(w / 2 - 10));
    if (!hasPos) return 0;
    // 12% headroom is about 10.7% of the plot past the longest bar; a fraction
    // axis is pinned at 100% and has none to lend.
    const room = valAxis.max === 1 ? 0 : 0.107 * o.plotPx;
    return Math.max(0, Math.ceil(w + 8 - room));
  };

  /**
   * `label` on a bar (model labelOf). A plain or grouped bar writes the text
   * at its end, before its value when values show too. A stack or a 100% bar
   * writes it inside each segment it fits in, from a label-only series, so
   * a stack's totals keep their place; with percent values on, the segment's
   * share follows the text. Mutates `series` and `valAxis`; returns the px a
   * grid needs on its right.
   * @param {any[]} series the bar series, one per colour level (or one)
   * @param {any} valAxis
   * @param {{ textAt: (si: number, gi: number) => string | null, groups: string[],
   *           nBars: number, vertical: boolean, stack: boolean, percent: boolean,
   *           valueLabels: boolean, valuePx: number, bandPx: number, ax: any,
   *           face: string, measure: (s: string) => number }} o
   * @returns {number}
   */
  const barMarkLabels = (series, valAxis, o) => {
    /** @param {any} d */
    const valOf = (d) => (d != null && typeof d === 'object') ? d.value : d;
    const bars = series.slice(0, o.nBars);
    const style = { color: o.ax.labelColor, fontSize: o.ax.fontSize, fontFamily: o.face };

    if (o.stack) {
      // The value axis's reach, as ECharts scales it from the tallest stack.
      let top = 0;
      for (let gi = 0; gi < o.groups.length; gi++) {
        let sum = 0;
        for (const s of bars) { const v = Number(valOf(s.data[gi])); if (Number.isFinite(v) && v > 0) sum += v; }
        top = Math.max(top, sum);
      }
      const perPx = top > 0 && o.valuePx > 0 ? top / o.valuePx : 0;
      const barW = Math.min(BAR_MAX, 0.6 * (o.bandPx || BAR_MAX / 0.6));
      /** @type {any[]} */
      const data = [];
      for (let gi = 0; gi < o.groups.length; gi++) {
        let pos = 0, neg = 0;
        bars.forEach((s, si) => {
          const d = s.data[gi];
          const v = Number(valOf(d));
          if (valOf(d) == null || !Number.isFinite(v) || v === 0) return;
          const mid = v >= 0 ? pos + v / 2 : neg + v / 2;
          if (v >= 0) pos += v; else neg += v;
          const t = o.textAt(si, gi);
          if (t == null) return;
          const shown = (o.percent && o.valueLabels) ? t + ' · ' + Math.round(v * 100) + '%' : t;
          const w = o.measure(shown);
          const len = perPx ? Math.abs(v) / perPx : Infinity;
          const fits = o.vertical ? len >= o.ax.fontSize + 4 && barW >= w + 4 : len >= w + 6;
          if (!fits) return;
          // The share now rides with the text.
          if (o.percent && o.valueLabels) {
            s.data[gi] = { ...((typeof d === 'object') ? d : { value: d }), label: { show: false } };
          }
          data.push(o.vertical ? [o.groups[gi], mid, shown] : [mid, o.groups[gi], shown]);
        });
      }
      if (data.length) {
        series.push({
          id: '__label__', type: 'scatter', data, silent: true, symbolSize: 0, z: 11,
          legendHoverLink: false, animation: false, tooltip: { show: false },
          emphasis: { disabled: true },
          label: { show: true, position: 'inside', ...style, color: '#fff',
                   formatter: (/** @type {any} */ p) => p.value[2] },
          labelLayout: { hideOverlap: true }
        });
      }
      return 0;
    }

    /** @type {Array<{ v: number, text: string }>} */
    const ends = [];
    bars.forEach((s, si) => {
      const texts = o.groups.map((_, gi) => o.textAt(si, gi));
      if (!texts.some((t) => t != null)) return;
      const vf = (o.valueLabels && s.label && typeof s.label.formatter === 'function')
        ? s.label.formatter : null;
      /** @param {any} p */
      const formatter = (p) => {
        const t = texts[p.dataIndex];
        const v = vf ? vf(p) : '';
        return t != null && v ? t + ' · ' + v : (t != null ? t : v);
      };
      s.label = { position: 'outside', distance: 4, ...style, ...(s.label || {}), show: true, formatter };
      s.labelLayout = { hideOverlap: true };
      s.data.forEach((/** @type {any} */ d, /** @type {number} */ gi) => {
        const v = valOf(d);
        if (v != null) ends.push({ v: Number(v), text: formatter({ value: v, dataIndex: gi }) });
      });
    });
    if (!ends.length) return 0;
    const hasPos = ends.some((e) => e.v > 0);
    const hasNeg = ends.some((e) => e.v < 0);
    valAxis.boundaryGap = [hasNeg ? '12%' : 0, hasPos ? '12%' : 0];
    let w = 0;
    for (const e of ends) if (o.vertical || e.v > 0) w = Math.max(w, o.measure(e.text));
    if (o.vertical) return Math.max(0, Math.ceil(w / 2 - 10));
    if (!hasPos) return 0;
    const room = valAxis.max === 1 ? 0 : 0.107 * o.valuePx;
    return Math.max(0, Math.ceil(w + 8 - room));
  };

  /**
   * `legend: "marks"` on a stacked or 100% bar: each series' name at the middle
   * of its segment, beside the last bar (vertical) or above the first bar
   * (horizontal). One label-only series per segment, so a bar's own labels
   * (values, totals) keep their place. Names of thin segments are spread a
   * line apart, in segment order. Appends to `series`; returns the px a vertical
   * grid needs on its right for the longest name.
   * @param {any[]} series the bar series, one per level
   * @param {{ vertical: boolean, groups: string[], bandPx: number, barPx: number,
   *           valuePx: number, measure: (s: string) => number, ink: any }} o
   *   `valuePx`: the value axis's length in px, to keep names a line apart
   * @returns {number}
   */
  const stackNames = (series, o) => {
    /** @param {any} d */
    const valOf = (d) => (d != null && typeof d === 'object') ? d.value : d;
    const n = o.groups.length;
    // The bar to name: the last (or first) one with a segment.
    /** @param {number} gi */
    const filled = (gi) => series.some((s) => { const v = valOf(s.data[gi]); return v != null && Number(v) !== 0; });
    let gi = o.vertical ? n - 1 : 0;
    while (gi >= 0 && gi < n && !filled(gi)) gi += o.vertical ? -1 : 1;
    if (gi < 0 || gi >= n) return 0;
    // The value axis's reach, as ECharts scales it from the tallest stack.
    let top = 0;
    for (let g = 0; g < n; g++) {
      let s = 0;
      for (const sr of series) { const v = Number(valOf(sr.data[g])); if (Number.isFinite(v) && v > 0) s += v; }
      top = Math.max(top, s);
    }
    const perPx = top > 0 && o.valuePx > 0 ? top / o.valuePx : 0;
    /** @type {Array<{ si: number, name: string, at: number, w: number }>} */
    const items = [];
    let pos = 0, neg = 0;
    series.forEach((s, si) => {
      const v = Number(valOf(s.data[gi]));
      if (valOf(s.data[gi]) == null || !Number.isFinite(v) || v === 0) return;
      let mid;
      if (v >= 0) { mid = pos + v / 2; pos += v; } else { mid = neg + v / 2; neg += v; }
      const name = String(s.name);
      // A missing level has no name to write.
      if (!name) return;
      items.push({ si, name, at: mid, w: o.measure(name) });
    });
    // Thin segments put their names closer than a line: spread them, top
    // down (or left to right lying down), each at least a line (a name's
    // width) from the next, and the whole run lifted (or moved left) where it
    // would leave the axis. The order stays the order of the segments.
    if (perPx) {
      const fs = (o.ink && o.ink.fontSize) || 12;
      if (o.vertical) {
        items.sort((x, y) => y.at - x.at);
        const gap = (fs + 3) * perPx;
        for (let k = 1; k < items.length; k++) items[k].at = Math.min(items[k].at, items[k - 1].at - gap);
        const low = items.length ? items[items.length - 1].at : 0;
        if (low < gap / 2) for (const it of items) it.at += gap / 2 - low;
      } else {
        items.sort((x, y) => x.at - y.at);
        for (let k = 1; k < items.length; k++) {
          const gap = ((items[k - 1].w + items[k].w) / 2 + 8) * perPx;
          items[k].at = Math.max(items[k].at, items[k - 1].at + gap);
        }
        const high = items.length ? items[items.length - 1].at + items[items.length - 1].w / 2 * perPx : 0;
        if (high > top) for (const it of items) it.at -= high - top;
      }
    }
    let widest = 0;
    // Back in series order: one label series per level, as the bars.
    items.sort((x, y) => x.si - y.si);
    for (const it of items) {
      widest = Math.max(widest, it.w);
      series.push(o.vertical
        ? NS.nameMark('__legend_' + it.si, it.name, [o.groups[gi], it.at], 'right', o.ink,
                      { distance: o.barPx / 2 + 6 })
        : NS.nameMark('__legend_' + it.si, it.name, [it.at, o.groups[gi]], 'top', o.ink,
                      { distance: o.barPx / 2 + 3 }));
    }
    if (!o.vertical) return 0;
    // The label starts beside the bar and may reach past the plot.
    return Math.max(0, Math.ceil(widest + 6 + o.barPx / 2 + 4 - o.bandPx / 2));
  };

  /**
   * The denominator of "% of panel", named by its columns' labels.
   * @param {Record<string, any>} cfg @param {(col: any) => string} title
   */
  const pctName = (cfg, title) => {
    /** @type {Record<string, any>} */
    const roleCol = { facet: cfg.facet, group: cfg.group, color: cfg.color };
    const roles = Array.isArray(cfg.pct_of) ? cfg.pct_of : (cfg.pct_of ? [cfg.pct_of] : ['facet']);
    const cols = roles.map((/** @type {string} */ r) => roleCol[r]).filter(Boolean);
    return cols.length ? '% of ' + cols.map(title).join(' x ') : '% of total';
  };

  /**
   * An axis label that sets the names in `lit` in semibold text ink, line by
   * line (a vertical axis wraps long names). A name holding rich-text syntax
   * stays as it is.
   * @param {any} axisLabel @param {Set<string>} lit @param {any} ink
   */
  const boldNames = (axisLabel, lit, ink) => {
    const base = axisLabel || {};
    const fmt = base.formatter;
    return {
      ...base,
      formatter: (/** @type {any} */ v, /** @type {number} */ i) => {
        const t = String(typeof fmt === 'function' ? fmt(v, i) : v);
        if (!lit.has(String(v)) || /[{}|]/.test(t)) return t;
        return t.split('\n').map((l) => '{hl|' + l + '}').join('\n');
      },
      rich: { ...(base.rich || {}),
              hl: { fontWeight: 600, color: ink.text, fontSize: base.fontSize || ink.fontSize } }
    };
  };

  Object.assign(parts, {
    barRadius, barValueLabels, barMarkLabels, stackNames, pctName, boldNames, WATERFALL_COLORS,
    BAR: { BAR_MAX, ROW_BAND, GROUP_BAR, GROUP_PAD, X_INSET }
  });
})();
