// @ts-check
/**
 * Chart block: what a bar carries besides its bar. The shared sizes, the
 * rounded value end, value labels, text on the marks (`label`), the level
 * names on a stack (`legend = "marks"`) and highlighted names in semibold.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});
  const option = /** @type {any} */ (NS.option = NS.option || {});
  const parts = /** @type {any} */ (option.aggParts = option.aggParts || {});

  // Waterfall sign colours: semantic, so they override the palette.
  const WATERFALL_COLORS = { increase: '#009E73', decrease: '#dc2626', total: '#bbbbbb' };

  // Corner radius of the value end (--blockr-mark-radius). The end at zero
  // is the axis and stays square; a floating step rounds both ends.
  const MARK_RADIUS = 2;
  /** @param {boolean} vertical @param {boolean} [floating] @returns {number[]} */
  const barRadius = (vertical, floating) => {
    const r = MARK_RADIUS;
    if (floating) return [r, r, r, r];
    return vertical ? [r, r, 0, 0] : [0, r, r, 0];
  };

  /** @param {any} d */
  const valOf = (d) => (d != null && typeof d === 'object') ? d.value : d;

  /** A series datum as an object. @param {any} d */
  const asDatum = (d) => (d !== null && typeof d === 'object' && !Array.isArray(d)) ? d : { value: d };

  /** A datum with `style` merged into its itemStyle. @param {any} d @param {Record<string, any>} style */
  const withItemStyle = (d, style) => {
    const o = asDatum(d);
    return { ...o, itemStyle: { ...o.itemStyle, ...style } };
  };

  /** Text style of a label on the marks. @param {any} ink */
  const labelStyle = (ink) => ({ color: ink.muted, fontSize: ink.fontSize, fontFamily: ink.face });

  /**
   * The tallest positive stack, which ECharts scales the value axis from,
   * and the value one px of the axis spans (0 without a length).
   * @param {any[]} series @param {number} n groups @param {number} valuePx
   */
  const stackScale = (series, n, valuePx) => {
    let top = 0;
    for (let g = 0; g < n; g++) {
      let sum = 0;
      for (const s of series) {
        const v = Number(valOf(s.data[g]));
        if (Number.isFinite(v) && v > 0) sum += v;
      }
      top = Math.max(top, sum);
    }
    return { top, perPx: top > 0 && valuePx > 0 ? top / valuePx : 0 };
  };

  /**
   * A group's drawn segments, bottom up: the series index, the datum, its
   * value and the middle of its stretch on the axis.
   * @param {any[]} series @param {number} gi
   */
  const segmentsAt = (series, gi) => {
    let pos = 0, neg = 0;
    /** @type {Array<{ si: number, d: any, v: number, at: number }>} */
    const out = [];
    series.forEach((s, si) => {
      const d = s.data[gi];
      const v = Number(valOf(d));
      if (valOf(d) == null || !Number.isFinite(v) || v === 0) return;
      const at = (v >= 0 ? pos : neg) + v / 2;
      if (v >= 0) pos += v; else neg += v;
      out.push({ si, d, v, at });
    });
    return out;
  };

  /**
   * Headroom for labels past the bars: sets the axis boundary gap and returns
   * the px a grid needs on its right for the widest end label.
   * @param {Array<{ v: number, text: string }>} ends @param {any} valAxis
   * @param {boolean} vertical @param {number} plotPx @param {(s: string) => number} textW
   */
  const endRoom = (ends, valAxis, vertical, plotPx, textW) => {
    const hasPos = ends.some((e) => e.v > 0);
    const hasNeg = ends.some((e) => e.v < 0);
    valAxis.boundaryGap = [hasNeg ? '12%' : 0, hasPos ? '12%' : 0];
    let w = 0;
    for (const e of ends) if (vertical || e.v > 0) w = Math.max(w, textW(e.text));
    // A vertical bar's label is centred on it and may overhang by half.
    if (vertical) return Math.max(0, Math.ceil(w / 2 - 10));
    if (!hasPos) return 0;
    // 12% headroom is about 10.7% of the plot past the longest bar; a fraction
    // axis is pinned at 100% and has none to lend.
    const room = valAxis.max === 1 ? 0 : 0.107 * plotPx;
    return Math.max(0, Math.ceil(w + 8 - room));
  };

  /**
   * The formatter of a label at a bar's end; `fraction`: values are shares.
   * @param {Record<string, any>} cfg @param {boolean} fraction
   * @returns {(v: number) => string}
   */
  const endFormat = (cfg, fraction) => {
    // `|| 0` folds a negative zero, which would print as "-0".
    if (fraction) return (v) => (Math.round(v * 1000) / 10 || 0) + '%';
    return NS.valueFormat(cfg) || ((v) => String(NS.ddNum(v || 0)));
  };

  const BAR_MAX = 48;
  const ROW_BAND = 28;     // horizontal row height, one bar per row
  const GROUP_BAR = 14;    // horizontal grouped: px per series bar
  const GROUP_PAD = 12;    // horizontal grouped: gap between groups
  // The category inset a vertical plot loses to the grid's margins.
  const X_INSET = 65;

  /**
   * `value_labels` at a bar's end: a stack once with its total, a grouped
   * bar per bar, a percent bar per segment where the share fits. Returns the
   * px a grid needs on its right. Mutates `series` and `valAxis`.
   * @param {any[]} series @param {any} valAxis
   * @param {{ink: any, vertical: boolean, stacked: boolean, percent: boolean,
   *          fraction: boolean, plotPx: number, bandPx: number,
   *          measure: (s: string) => number, cfg: Record<string, any>}} o
   * @returns {number}
   */
  const barValueLabels = (series, valAxis, o) => {
    const textW = o.measure;
    const fmt = endFormat(o.cfg, o.fraction);
    const style = labelStyle(o.ink);

    if (o.percent) {
      const barW = Math.min(48, 0.6 * o.bandPx);
      /** @param {number} v @param {string} t */
      const fits = (v, t) => !o.plotPx || (o.vertical
        ? v * o.plotPx >= o.ink.fontSize + 4 && (!o.bandPx || barW >= textW(t) + 4)
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

    /** @type {Array<{ v: number, text: string }>} */
    const ends = [];
    /** @param {number} v */
    const addEnd = (v) => ends.push({ v, text: fmt(v) });
    for (const s of series) {
      s.label = { show: !o.stacked, position: 'outside', distance: 4, ...style,
        formatter: (/** @type {any} */ p) => p.value == null ? '' : fmt(Number(p.value)) };
      s.labelLayout = { hideOverlap: true };
      if (!o.stacked) {
        for (const d of s.data) {
          const v = valOf(d);
          if (v != null) addEnd(Number(v));
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
        addEnd(tot);
        // The label rides the outermost segment of the total's sign.
        for (let si = series.length - 1; si >= 0; si--) {
          const v = valOf(series[si].data[gi]);
          if (v == null || (tot < 0 ? v > 0 : v < 0)) continue;
          series[si].data[gi] = { ...asDatum(series[si].data[gi]),
                                  label: { show: true, formatter: fmt(tot) } };
          break;
        }
      }
    }

    return endRoom(ends, valAxis, o.vertical, o.plotPx, textW);
  };

  /**
   * `label` on a bar (model labelOf). A plain or grouped bar writes the text
   * at its end, before its value. A stack or 100% bar writes it inside each
   * segment it fits in, from a label-only series. Mutates `series` and
   * `valAxis`; returns the px a grid needs on its right.
   * @param {any[]} series the bar series, one per colour level (or one)
   * @param {any} valAxis
   * @param {{ textAt: (si: number, gi: number) => string | null, groups: string[],
   *           nBars: number, vertical: boolean, stack: boolean, percent: boolean,
   *           valueLabels: boolean, valuePx: number, bandPx: number, ink: any,
   *           measure: (s: string) => number }} o
   * @returns {number}
   */
  const barMarkLabels = (series, valAxis, o) => {
    const bars = series.slice(0, o.nBars);
    const style = labelStyle(o.ink);

    if (o.stack) {
      const { perPx } = stackScale(bars, o.groups.length, o.valuePx);
      const barW = Math.min(BAR_MAX, 0.6 * (o.bandPx || BAR_MAX / 0.6));
      /** @type {any[]} */
      const data = [];
      for (let gi = 0; gi < o.groups.length; gi++) {
        for (const { si, d, v, at } of segmentsAt(bars, gi)) {
          const t = o.textAt(si, gi);
          if (t == null) continue;
          const shown = (o.percent && o.valueLabels) ? t + ' · ' + Math.round(v * 100) + '%' : t;
          const w = o.measure(shown);
          const len = perPx ? Math.abs(v) / perPx : Infinity;
          const fits = o.vertical ? len >= o.ink.fontSize + 4 && barW >= w + 4 : len >= w + 6;
          if (!fits) continue;
          if (o.percent && o.valueLabels) bars[si].data[gi] = { ...asDatum(d), label: { show: false } };
          data.push(o.vertical ? [o.groups[gi], at, shown] : [at, o.groups[gi], shown]);
        }
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
    return endRoom(ends, valAxis, o.vertical, o.valuePx, o.measure);
  };

  /**
   * `legend: "marks"` on a stacked or 100% bar: each series' name at the
   * middle of its segment, beside the last bar (vertical) or above the first
   * (horizontal), as label-only series. Names of thin segments are spread a
   * line apart. Appends to `series`; returns the px a vertical grid needs on
   * its right for the longest name.
   * @param {any[]} series the bar series, one per level
   * @param {{ vertical: boolean, groups: string[], bandPx: number, barPx: number,
   *           valuePx: number, measure: (s: string) => number, ink: any }} o
   *   `valuePx`: the value axis's length in px, to keep names a line apart
   * @returns {number}
   */
  const stackNames = (series, o) => {
    const n = o.groups.length;
    // The bar to name: the last (or first) one with a segment.
    let gi = o.vertical ? n - 1 : 0;
    while (gi >= 0 && gi < n && !segmentsAt(series, gi).length) gi += o.vertical ? -1 : 1;
    if (gi < 0 || gi >= n) return 0;
    const { top, perPx } = stackScale(series, n, o.valuePx);
    // A missing level has no name to write.
    const items = segmentsAt(series, gi)
      .map(({ si, at }) => ({ si, at, name: String(series[si].name) }))
      .filter((it) => it.name)
      .map((it) => ({ ...it, w: o.measure(it.name) }));
    // Thin segments put names closer than a line: spread them, each at least
    // a line (a name's width) from the next, and keep the run on the axis.
    if (perPx) {
      const fs = o.ink.fontSize || 12;
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
   * The x labels of a vertical category axis fitted to the plot, with the
   * `xFit` inputs a resize re-fits them from.
   * @param {any[]} labels @param {number} plotW @param {any} look
   * @param {any} display maps a category to its label (or null) @param {any} [decorate]
   */
  const fitXLabels = (labels, plotW, look, display, decorate) => {
    const xlab = NS.axes.xAxisLabels(labels, (plotW || 0) - X_INSET, false, look.measure, look.ink);
    return { xlab, xFit: { labels, inset: X_INSET, decimate: false, gutter: xlab.bottom,
                           key: xlab.key, formatter: display, decorate } };
  };

  /**
   * An axis label that sets the names in `lit` in semibold text ink, line by
   * line. A name holding rich-text syntax stays as it is.
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
    barRadius, barValueLabels, barMarkLabels, stackNames, boldNames, endFormat, WATERFALL_COLORS,
    asDatum, withItemStyle, labelStyle, fitXLabels,
    BAR: { BAR_MAX, ROW_BAND, GROUP_BAR, GROUP_PAD, X_INSET }
  });
})();
