// @ts-check
/**
 * Chart block: the bar and waterfall options.
 *
 * One panel of the aggregated model in, its ECharts option out, plus what
 * the view needs beside it: the panel height, the x-label fit inputs a
 * resize redoes, and the footer note when a horizontal axis thins its
 * labels. Pure: text is measured with the function the caller passes.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});
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

  /**
   * A plain, stacked, grouped or percent bar.
   * @param {any} m @param {any} panel @param {any} look @param {number} plotW
   */
  const bar = (m, panel, look, plotW) => {
    const { cfg, ink, measure } = look;
    const P = parts.common(look);
    const { ax, esc, title } = P;
    const groups = panel.groups;
    const colors = m.colors;
    const palette = NS.paletteOf(cfg);
    const barMode = cfg.bar_mode || 'stacked';
    const isGrouped = barMode === 'grouped';
    const isPercent = barMode === 'percent';
    const vertical = cfg.orientation === 'vertical';
    const valueLabelsOn = cfg.value_labels === 'on' || cfg.value_labels === true;

    // Horizontal: the row band is fixed, so bars are sized off it; past the
    // panel cap a grouped fan squeezes into its band. Vertical: bands come
    // from the width, capped so a few categories do not draw slabs.
    const hGrouped = isGrouped && colors.length > 0;
    const rowBand = hGrouped ? colors.length * GROUP_BAR + GROUP_PAD : ROW_BAND;
    const rowsH = groups.length * rowBand;
    const hCapped = !vertical && rowsH > NS.PANEL_H_CAP;
    const labelNote = hCapped
      ? NS.axes.thinnedLabelNote(NS.PANEL_H_CAP, groups.length, title(cfg.group)) : null;
    const barLayout = isGrouped
      ? (vertical
          ? { barGap: 0, barCategoryGap: '30%', barMaxWidth: BAR_MAX }
          : (hCapped ? { barGap: 0, barCategoryGap: '20%' } : { barGap: 0, barWidth: GROUP_BAR }))
      : { barWidth: '60%', barMaxWidth: BAR_MAX };

    // `highlight`: a bar without a highlighted row is grey; the colour
    // levels that have one take the palette between them.
    const hlLit = m.hlLit;
    const muted = NS.mutedMark(ink);
    const levelColor = m.hlColors
      ? NS.litLevelColors(colors, m.hlColors, m.colorScale, palette)
      : NS.levelColors(colors, m.colorScale, palette);
    /** @param {any} v @param {string} g @param {string | null} lv */
    const greyed = (v, g, lv) => (v == null || !hlLit || hlLit(panel.facet, g, lv)) ? v
      : (typeof v === 'object' ? { ...v, itemStyle: { ...v.itemStyle, color: muted } }
        : { value: v, itemStyle: { color: muted } });

    /** @type {any[]} */
    const series = [];
    if (colors.length === 0) {
      // A null aggregate stays null: a gap, not a zero bar.
      series.push({
        type: 'bar',
        data: groups.map((/** @type {string} */ g) => {
          const d = NS.model.cellOf(m, panel, g);
          return greyed(d ? d.value : null, g, null);
        }),
        itemStyle: { color: palette[0], borderRadius: barRadius(vertical) },
        barWidth: '60%', barMaxWidth: BAR_MAX, emphasis: { focus: 'self' }
      });
    } else {
      /** @type {Record<string, number>} */
      const groupTotals = {};
      if (isPercent) {
        for (const g of groups) {
          groupTotals[g] = NS.model.cellsOf(m, panel, g)
            .filter((/** @type {any} */ a) => a.value != null)
            .reduce((/** @type {number} */ s, /** @type {any} */ a) => s + a.value, 0);
        }
      }
      for (let ci = 0; ci < colors.length; ci++) {
        const color = colors[ci];
        series.push({
          type: 'bar', name: color,
          // null for a (group, colour) cell without rows, so a stack skips it.
          // A percent datum keeps its raw value for the tooltip.
          data: groups.map((/** @type {string} */ g) => {
            const d = NS.model.cellOf(m, panel, g, color);
            const raw = d ? d.value : null;
            if (raw == null) return null;
            if (isPercent) {
              const tot = groupTotals[g];
              return greyed(tot ? { value: raw / tot, raw } : null, g, color);
            }
            return greyed(raw, g, color);
          }),
          ...(isGrouped ? {} : { stack: 'stack' }),
          itemStyle: { color: levelColor[ci] || muted },
          ...barLayout,
          emphasis: { focus: 'self' }
        });
      }
      // The value end rounds: every grouped bar, and in a stack the
      // outermost segment of each group, which varies with missing cells.
      if (isGrouped) {
        for (const s of series) s.itemStyle.borderRadius = barRadius(vertical);
      } else {
        for (let gi = 0; gi < groups.length; gi++) {
          for (let si = series.length - 1; si >= 0; si--) {
            const d = series[si].data[gi];
            if (d == null) continue;
            series[si].data[gi] = (typeof d === 'object')
              ? { ...d, itemStyle: { ...d.itemStyle, borderRadius: barRadius(vertical) } }
              : { value: d, itemStyle: { borderRadius: barRadius(vertical) } };
            break;
          }
        }
      }
    }

    // "(n)" on the category axis comes from a formatter, so the axis data
    // stays the bare group the clicks and tooltips key off.
    const axisCounts = panel.axisCounts;
    const catLabels = axisCounts
      ? groups.map((/** @type {string} */ g) => NS.withCount(g, axisCounts)) : groups;
    const catFmt = axisCounts ? (/** @type {any} */ v) => NS.withCount(v, axisCounts) : null;
    const gut = NS.axes.yGutter(catLabels, measure);
    const xlab = vertical
      ? NS.axes.xAxisLabels(catLabels, (plotW || 0) - X_INSET, false, measure, ink) : null;
    const catAxis = vertical
      ? { type: 'category', data: groups,
          axisLabel: NS.axes.axisLabelWithDisplay(xlab && xlab.axisLabel, catFmt),
          axisLine: { lineStyle: { color: ink.strong } }, axisTick: { show: false } }
      : { type: 'category', data: groups, inverse: true,
          axisLabel: { color: ax.labelColor, fontSize: ax.fontSize, align: 'left',
                       margin: gut.margin, width: gut.width, overflow: 'truncate',
                       ellipsis: '…', ...(catFmt ? { formatter: catFmt } : {}) },
          axisLine: { show: false }, axisTick: { show: false } };
    // A highlighted bar's name is semibold, as a name written on a mark. A
    // resize that turns the x labels again keeps it (`decorate`).
    /** @type {((axisLabel: any) => any) | null} */
    let decorate = null;
    if (hlLit) {
      const lit = new Set(groups.filter((/** @type {string} */ g) => (colors.length
        ? colors.some((/** @type {string} */ c) => hlLit(panel.facet, g, c))
        : hlLit(panel.facet, g, null))));
      decorate = (axisLabel) => boldNames(axisLabel, lit, ink);
      catAxis.axisLabel = decorate(catAxis.axisLabel);
    }
    // A share needs a colour split (one series is 100% of itself);
    // pct_distinct is a share of the panel and needs none.
    const showPercent = isPercent && colors.length > 0;
    const pctFunc = cfg.func === 'pct_distinct';
    const asFraction = showPercent || pctFunc;
    // `format` / `unit` (format.js): the ticks in the format, the unit once
    // in the axis title. A share keeps its own percent.
    const vf = asFraction ? null : NS.valueFormat(cfg);
    const tickFmt = asFraction ? null : NS.valueFormat(cfg, { unit: false });
    const valAxis = {
      type: 'value',
      name: showPercent ? '% of group total'
        : (pctFunc ? pctName(cfg, title) : NS.axisName(P.valueTitle, cfg.value, cfg)),
      nameLocation: 'middle',
      nameGap: vertical ? 45 : 30,
      nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize },
      axisLabel: {
        color: ax.labelColor, fontSize: ax.fontSize,
        ...(asFraction ? { formatter: (/** @type {number} */ v) => Math.round(v * 100) + '%' } : {}),
        ...(tickFmt ? { formatter: tickFmt } : {})
      },
      ...(asFraction ? { max: 1 } : {}),
      axisLine: { lineStyle: { color: ink.strong } },
      splitLine: { lineStyle: { color: ax.splitLineColor, type: 'dashed' } }
    };
    // A histogram's bins touch: the gap is the only thing that tells one
    // bin from the next.
    if (m.histogram) {
      for (const s of series) {
        if (s.type !== 'bar') continue;
        delete s.barWidth;
        delete s.barMaxWidth;
        s.barCategoryGap = '3%';
        s.barGap = '0%';
      }
    }
    const labelRight = valueLabelsOn
      ? barValueLabels(series, valAxis, {
          ax, vertical, stacked: !isGrouped && !isPercent && colors.length > 0,
          percent: showPercent, fraction: pctFunc,
          plotPx: vertical ? 350 - 30 - 66 : (plotW ? plotW - gut.gridLeft - 5 : 0),
          bandPx: vertical ? (plotW ? (plotW - X_INSET) / Math.max(1, groups.length) : 0)
            : ROW_BAND,
          measure, face: ink.face, cfg
        })
      : 0;
    const markRight = m.markLabels
      ? barMarkLabels(series, valAxis, {
          textAt: (si, gi) => NS.model.labelOf(m, panel, groups[gi], colors.length ? colors[si] : null),
          groups, nBars: Math.max(1, colors.length), vertical,
          stack: !isGrouped && colors.length > 0, percent: showPercent, valueLabels: valueLabelsOn,
          valuePx: vertical ? 350 - 30 - 66 : (plotW ? plotW - gut.gridLeft - 5 : 0),
          bandPx: vertical ? (plotW ? (plotW - X_INSET) / Math.max(1, groups.length) : 0)
            : (hCapped ? NS.PANEL_H_CAP / Math.max(1, groups.length) : ROW_BAND),
          ax, face: ink.face, measure
        })
      : 0;
    NS.axes.addValueLines(series, valAxis, !vertical, asFraction, cfg.value_lines, ink,
      cfg.value_line_names);
    // A histogram's bins are ranges of a number, not categories to name.
    if (!m.histogram) {
      NS.axes.addCategoryMarks(series, vertical, groups, cfg.x_lines, cfg.x_line_names,
        cfg.x_ranges, ink);
    }
    // `legend: "marks"`: the level names on the stack's segments. After the
    // series the tooltip and the highlight read (the first `colors.length`).
    let namesRight = 0;
    const marks = NS.legendPlan(m, cfg).marks;
    if (marks === 'stack' || marks === 'stack-h') {
      const bandPx = vertical
        ? (plotW ? (plotW - X_INSET) / Math.max(1, groups.length) : 0)
        : (hCapped ? NS.PANEL_H_CAP / Math.max(1, groups.length) : ROW_BAND);
      const barPx = vertical ? (bandPx ? Math.min(BAR_MAX, 0.6 * bandPx) : BAR_MAX / 2)
        : Math.min(BAR_MAX, 0.6 * bandPx);
      const valuePx = vertical ? 350 - 30 - 66 : (plotW ? plotW - gut.gridLeft - 5 : 0);
      namesRight = stackNames(series, { vertical, groups, bandPx, barPx, valuePx, measure, ink });
    }

    const fmtRaw = vf || ((/** @type {number} */ n) =>
      Number.isInteger(n) ? n : Math.round(n * 100) / 100);
    // A split bar's card lists every level of the hovered category, the
    // segment under the pointer marked. The item trigger is what says which
    // segment that is; the axis trigger only knows the category.
    /** @type {Record<string, any>} */
    const tooltip = colors.length
      ? { trigger: 'item', confine: true }
      : { trigger: 'axis', axisPointer: { type: 'shadow' }, confine: true };
    /** @returns {{ ps: any[], hit: string | null }} */
    const levelsAt = (/** @type {any} */ arg) => {
      const p = Array.isArray(arg) ? arg[0] : arg;
      if (!p) return { ps: [], hit: null };
      if (!colors.length) return { ps: [].concat(arg), hit: null };
      const ps = series.slice(0, colors.length).map((sr) => {
        const d = sr.data[p.dataIndex];
        const obj = d != null && typeof d === 'object';
        return { name: p.name, seriesName: sr.name, value: obj ? d.value : d,
                 data: obj ? d : null, color: sr.itemStyle.color };
      });
      return { ps, hit: p.seriesName };
    };
    // A split bar's rows name the colour column with the level.
    const colorName = colors.length ? esc(title(cfg.color) || cfg.color) + ' ' : '';
    const ttExtraRows = (/** @type {any} */ head) => {
      const extra = panel.ttRep ? panel.ttRep[head] : null;
      if (!extra) return [];
      /** @type {string[]} */
      const out = [];
      m.ttFields.forEach((/** @type {string} */ c, /** @type {number} */ i) => {
        const v = extra[i];
        if (v != null && v !== '') out.push(NS.tipRow(esc(title(c) || c), esc(v)));
      });
      return out;
    };
    if (showPercent) {
      tooltip.formatter = (/** @type {any} */ arg) => {
        const { ps, hit } = levelsAt(arg);
        if (!ps.length) return '';
        const head = ps[0].axisValueLabel || ps[0].name || '';
        const rows = ps.filter((p) => p && p.value != null).map((p) => {
          const pct = Math.round((Number(p.value) || 0) * 100);
          const raw = p.data && p.data.raw != null ? fmtRaw(p.data.raw) : null;
          return NS.tipRow(colorName + esc(p.seriesName),
            pct + '%' + (raw != null ? ' (' + raw + ')' : ''), p.color,
            hit == null ? null : p.seriesName === hit);
        });
        return NS.tipHead(esc(head)) + rows.concat(ttExtraRows(head)).join('');
      };
    } else {
      // The value named by its aggregation, with n where it says something.
      /** @type {Record<string, number>} */
      const nOf = {};
      for (const a of panel.cells) nOf[a.group + '|||' + a.color] = a.n;
      const aggLabel = P.aggLabel;
      const showN = cfg.func !== 'count' && cfg.func !== 'identity';
      // A count or a sum needs no note naming the aggregation.
      const additive = cfg.func === 'count' || cfg.func === 'sum';
      tooltip.formatter = (/** @type {any} */ arg) => {
        const { ps, hit } = levelsAt(arg);
        if (!ps.length) return '';
        const head = ps[0].axisValueLabel || ps[0].name || '';
        const rows = ps.filter((p) => p && p.value != null).map((p) => {
          const nm = colors.length ? p.seriesName : aggLabel;
          const n = nOf[head + '|||' + (colors.length ? p.seriesName : '__all__')];
          const shown = pctFunc
            ? Math.round(Number(p.value) * 1000) / 10 + '%'
            : fmtRaw(Number(p.value));
          return NS.tipRow(colors.length ? colorName + esc(nm) : esc(nm),
            shown + (pctFunc && n != null ? ' (' + n + ')' : '') +
            (!pctFunc && showN && n != null ? ' (n=' + n + ')' : ''),
            colors.length ? p.color : null,
            hit == null ? null : p.seriesName === hit);
        });
        // The segments add up to the bar only for a count or a sum.
        const vals = ps.filter((p) => p && p.value != null);
        const foot = colors.length && vals.length > 1 && additive && !pctFunc
          ? NS.TIP_SEP + NS.tipRow(esc(aggLabel),
            fmtRaw(vals.reduce((a, p) => a + Number(p.value || 0), 0)))
          : '';
        const note = colors.length && !(additive && !pctFunc) ? NS.tipNote(esc(aggLabel)) : '';
        return NS.tipHead(esc(head)) + note + rows.join('') + foot +
          ttExtraRows(head).join('');
      };
    }
    // The HTML band shows the chips; a hidden legend keeps the selection
    // model the chips toggle.
    const legendOn = colors.length > 0;
    const bottomBase = vertical ? 40 + 26 + (xlab ? xlab.bottom : 0) : 20 + 26;
    return {
      xFit: xlab ? { labels: catLabels, inset: X_INSET, decimate: false, gutter: xlab.bottom,
                     key: xlab.key, formatter: catFmt, decorate } : undefined,
      // Horizontal: one fixed band per row. Vertical: the default canvas plus
      // the turned-label gutter, so labels lengthen the canvas.
      panelH: vertical ? 350 + (xlab ? xlab.bottom : 0)
        : 30 + Math.min(NS.PANEL_H_CAP, rowsH) + bottomBase,
      labelNote,
      option: {
        ...(look.theme ? {} : { backgroundColor: 'transparent' }),
        textStyle: { fontFamily: ink.face },
        tooltip,
        legend: legendOn ? { show: false, data: colors } : undefined,
        grid: vertical
          ? { left: 55, right: 10 + Math.max(labelRight, markRight, namesRight), top: 30,
              bottom: bottomBase }
          : { left: gut.gridLeft, right: 5 + Math.max(labelRight, markRight), top: 30,
              bottom: bottomBase },
        xAxis: vertical ? catAxis : valAxis,
        yAxis: vertical ? valAxis : catAxis,
        series
      }
    };
  };

  /**
   * A waterfall: a bar whose steps float from the running total of the
   * steps before them. A transparent base series carries the offset, a
   * visible delta series the step, coloured by sign; a "total" step
   * (waterfall_totals) restates the running total from zero. Vertical only.
   * @param {any} m @param {any} panel @param {any} look @param {number} plotW
   */
  const waterfall = (m, panel, look, plotW) => {
    const { cfg, ink, measure } = look;
    const P = parts.common(look);
    const { ax, esc } = P;
    const groups = panel.groups;
    // One value per step, summed across a colour split; all-null is null.
    /** @param {string} g */
    const valOf = (g) => {
      const vals = NS.model.cellsOf(m, panel, g)
        .filter((/** @type {any} */ a) => a.value != null).map((/** @type {any} */ a) => a.value);
      return vals.length ? vals.reduce((/** @type {number} */ s, /** @type {number} */ v) => s + v, 0)
        : null;
    };
    const totals = new Set((cfg.waterfall_totals || []).map(String));
    /** @type {any[]} */
    const base = [];
    /** @type {any[]} */
    const delta = [];
    let cum = 0;
    const labelsOn = cfg.value_labels === 'on' || cfg.value_labels === true;
    const wvf = cfg.func === 'pct_distinct' ? null : NS.valueFormat(cfg);
    const fmt = cfg.func === 'pct_distinct'
      ? (/** @type {number} */ v) => (Math.round(v * 1000) / 10 || 0) + '%'
      : (wvf || ((/** @type {number} */ v) => String(NS.ddNum(v || 0))));
    /** @param {number} v @param {boolean} up */
    const lab = (v, up) => labelsOn
      ? { label: { show: true, position: up ? 'top' : 'bottom', formatter: fmt(v) } } : {};
    let lo = 0, hi = 0;
    for (const g of groups) {
      const v = valOf(g);
      const isTotal = totals.has(g);
      if (v == null && !isTotal) {
        // No delta: the bridge holds its total and the step is a gap.
        base.push(0);
        delta.push(null);
        continue;
      }
      if (isTotal) {
        base.push(0);
        delta.push({ value: cum, itemStyle: { color: WATERFALL_COLORS.total,
          borderRadius: barRadius(true) }, ...lab(cum, cum >= 0) });
      } else if (/** @type {number} */ (v) >= 0) {
        base.push(cum);
        delta.push({ value: v, itemStyle: { color: WATERFALL_COLORS.increase,
          borderRadius: barRadius(true, cum !== 0) }, ...lab(/** @type {number} */ (v), true) });
        cum += /** @type {number} */ (v);
      } else {
        const d = /** @type {number} */ (v);
        base.push(cum + d);
        delta.push({ value: -d, itemStyle: { color: WATERFALL_COLORS.decrease,
          borderRadius: barRadius(true, (cum + d) !== 0) }, ...lab(d, false) });
        cum += d;
      }
      lo = Math.min(lo, cum);
      hi = Math.max(hi, cum);
    }

    const axisCounts = panel.axisCounts;
    const countFmt = axisCounts ? (/** @type {any} */ v) => NS.withCount(v, axisCounts) : null;
    const stepLabels = axisCounts
      ? groups.map((/** @type {string} */ g) => NS.withCount(g, axisCounts)) : groups;
    const xlab = NS.axes.xAxisLabels(stepLabels, (plotW || 0) - X_INSET, false, measure, ink);
    const catAxis = {
      type: 'category', data: groups,
      axisLabel: NS.axes.axisLabelWithDisplay(xlab.axisLabel, countFmt),
      axisLine: { lineStyle: { color: ink.strong } },
      axisTick: { show: false }, splitLine: { show: false }
    };
    /** @type {any} */
    const valAxis = {
      type: 'value', name: NS.axisName(P.valueTitle, cfg.value, cfg), nameLocation: 'middle',
      nameGap: 45,
      nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize },
      axisLabel: { color: ax.labelColor, fontSize: ax.fontSize,
                   ...(wvf ? { formatter: NS.valueFormat(cfg, { unit: false }) } : {}) },
      axisLine: { lineStyle: { color: ink.strong } },
      splitLine: { lineStyle: { color: ax.splitLineColor, type: 'dashed' } }
    };
    /** @type {any[]} */
    const wfSeries = [
      { name: 'base', type: 'bar', stack: 'waterfall', barWidth: '60%',
        itemStyle: { color: 'transparent', borderColor: 'transparent' },
        emphasis: { disabled: true }, silent: true, tooltip: { show: false }, data: base },
      // The corners ride on each datum: a total sits on the axis, a step
      // floats.
      { name: 'delta', type: 'bar', stack: 'waterfall', barWidth: '60%',
        emphasis: { focus: 'self' }, data: delta,
        ...(labelsOn
          ? { label: { show: false, distance: 4, color: ax.labelColor, fontSize: ax.fontSize,
                       fontFamily: ink.face },
              labelLayout: { hideOverlap: true } }
          : {}) }
    ];
    let labelRight = 0;
    if (labelsOn) {
      valAxis.boundaryGap = [lo < 0 ? '12%' : 0, hi > 0 ? '12%' : 0];
      let w = 0;
      for (const d of delta) if (d && d.label) w = Math.max(w, measure(d.label.formatter));
      labelRight = Math.max(0, Math.ceil(w / 2 - 10));
    }
    NS.axes.addValueLines(wfSeries, valAxis, false, false, cfg.value_lines, ink,
      cfg.value_line_names);
    return {
      panelH: 350 + xlab.bottom,
      xFit: { labels: stepLabels, inset: X_INSET, decimate: false, gutter: xlab.bottom,
              key: xlab.key, formatter: countFmt },
      labelNote: null,
      option: {
        ...(look.theme ? {} : { backgroundColor: 'transparent' }),
        textStyle: { fontFamily: ink.face },
        tooltip: {
          trigger: 'axis', axisPointer: { type: 'shadow' }, confine: true,
          // Only the visible delta; the base is how the bar floats.
          formatter: (/** @type {any} */ params) => {
            const p = (params || []).find((/** @type {any} */ x) => x.seriesName === 'delta');
            if (!p) return '';
            return NS.tipHead(esc(p.name)) + NS.tipRow(esc(P.aggLabel),
              p.value == null ? '–' : (wvf ? wvf(p.value) : NS.ddNum(p.value)));
          }
        },
        legend: { show: false },
        grid: { left: 55, right: 10 + labelRight, top: 30, bottom: 40 + 26 + xlab.bottom },
        xAxis: catAxis,
        yAxis: valAxis,
        series: wfSeries
      }
    };
  };

  Object.assign(parts, { bar, waterfall, barValueLabels, barRadius, WATERFALL_COLORS });
})();
