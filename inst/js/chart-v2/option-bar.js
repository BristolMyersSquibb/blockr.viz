// @ts-check
/**
 * Chart block v2: the bar and waterfall options.
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
   *          measure: (s: string) => number, face: string}} o
   * @returns {number}
   */
  const barValueLabels = (series, valAxis, o) => {
    const textW = o.measure;
    // `|| 0` folds a negative zero, which would print as "-0".
    const fmt = o.fraction
      ? (/** @type {number} */ v) => (Math.round(v * 1000) / 10 || 0) + '%'
      : (/** @type {number} */ v) => String(NS.ddNum(v || 0));
    /** @param {any} d */
    const valOf = (d) => (d != null && typeof d === 'object') ? d.value : d;
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
   * The denominator of "% of panel", named by its columns.
   * @param {Record<string, any>} cfg
   */
  const pctName = (cfg) => {
    /** @type {Record<string, any>} */
    const roleCol = { facet: cfg.facet, group: cfg.group, color: cfg.color };
    const roles = Array.isArray(cfg.pct_of) ? cfg.pct_of : (cfg.pct_of ? [cfg.pct_of] : ['facet']);
    const cols = roles.map((/** @type {string} */ r) => roleCol[r]).filter(Boolean);
    return cols.length ? '% of ' + cols.join(' x ') : '% of total';
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
      ? NS.axes.thinnedLabelNote(NS.PANEL_H_CAP, groups.length, cfg.group) : null;
    const barLayout = isGrouped
      ? (vertical
          ? { barGap: 0, barCategoryGap: '30%', barMaxWidth: BAR_MAX }
          : (hCapped ? { barGap: 0, barCategoryGap: '20%' } : { barGap: 0, barWidth: GROUP_BAR }))
      : { barWidth: '60%', barMaxWidth: BAR_MAX };

    /** @type {any[]} */
    const series = [];
    if (colors.length === 0) {
      // A null aggregate stays null: a gap, not a zero bar.
      series.push({
        type: 'bar',
        data: groups.map((/** @type {string} */ g) => {
          const d = NS.model.cellOf(m, panel, g);
          return d ? d.value : null;
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
      const colorScale = m.colorScale;
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
              return tot ? { value: raw / tot, raw } : null;
            }
            return raw;
          }),
          ...(isGrouped ? {} : { stack: 'stack' }),
          itemStyle: {
            color: (colorScale && colorScale.color && colorScale.color[color]) ||
              NS.paletteAt(palette, ci)
          },
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
    // A share needs a colour split (one series is 100% of itself);
    // pct_distinct is a share of the panel and needs none.
    const showPercent = isPercent && colors.length > 0;
    const pctFunc = cfg.func === 'pct_distinct';
    const asFraction = showPercent || pctFunc;
    const valAxis = {
      type: 'value',
      name: showPercent ? '% of group total' : (pctFunc ? pctName(cfg) : P.valueTitle),
      nameLocation: 'middle',
      nameGap: vertical ? 45 : 30,
      nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize },
      axisLabel: {
        color: ax.labelColor, fontSize: ax.fontSize,
        ...(asFraction ? { formatter: (/** @type {number} */ v) => Math.round(v * 100) + '%' } : {})
      },
      ...(asFraction ? { max: 1 } : {}),
      axisLine: { lineStyle: { color: ink.strong } },
      splitLine: { lineStyle: { color: ax.splitLineColor, type: 'dashed' } }
    };
    const labelRight = valueLabelsOn
      ? barValueLabels(series, valAxis, {
          ax, vertical, stacked: !isGrouped && !isPercent && colors.length > 0,
          percent: showPercent, fraction: pctFunc,
          plotPx: vertical ? 350 - 30 - 66 : (plotW ? plotW - gut.gridLeft - 5 : 0),
          bandPx: vertical ? (plotW ? (plotW - X_INSET) / Math.max(1, groups.length) : 0)
            : ROW_BAND,
          measure, face: ink.face
        })
      : 0;
    NS.axes.addValueLines(series, valAxis, !vertical, asFraction, cfg.value_lines, ink);

    const fmtRaw = (/** @type {number} */ n) =>
      Number.isInteger(n) ? n : Math.round(n * 100) / 100;
    /** @type {Record<string, any>} */
    const tooltip = { trigger: 'axis', axisPointer: { type: 'shadow' }, confine: true };
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
      tooltip.formatter = (/** @type {any[]} */ ps) => {
        if (!ps || !ps.length) return '';
        const head = ps[0].axisValueLabel || ps[0].name || '';
        const rows = ps.filter((p) => p && p.value != null).map((p) => {
          const pct = Math.round((Number(p.value) || 0) * 100);
          const raw = p.data && p.data.raw != null ? fmtRaw(p.data.raw) : null;
          return NS.tipRow(colorName + esc(p.seriesName),
            pct + '%' + (raw != null ? ' (' + raw + ')' : ''), p.color);
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
      // A split bar's rows add up to its total only for a count or a sum.
      const additive = cfg.func === 'count' || cfg.func === 'sum';
      tooltip.formatter = (/** @type {any[]} */ ps) => {
        if (!ps || !ps.length) return '';
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
            colors.length ? p.color : null);
        });
        const vals = ps.filter((p) => p && p.value != null);
        let foot = '';
        if (colors.length && vals.length > 1 && additive && !pctFunc) {
          foot = NS.TIP_SEP + NS.tipRow(esc(aggLabel),
            fmtRaw(vals.reduce((a, p) => a + Number(p.value || 0), 0)));
        }
        const note = colors.length && !(additive && !pctFunc) ? NS.tipNote(esc(aggLabel)) : '';
        return NS.tipHead(esc(head)) + note + rows.join('') + foot + ttExtraRows(head).join('');
      };
    }
    // The HTML band shows the chips; a hidden legend keeps the selection
    // model the chips toggle.
    const legendOn = colors.length > 0;
    const bottomBase = vertical ? 40 + 26 + (xlab ? xlab.bottom : 0) : 20 + 26;
    return {
      xFit: xlab ? { labels: catLabels, inset: X_INSET, decimate: false, gutter: xlab.bottom,
                     key: xlab.key, formatter: catFmt } : undefined,
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
          ? { left: 55, right: 10 + labelRight, top: 30, bottom: bottomBase }
          : { left: gut.gridLeft, right: 5 + labelRight, top: 30, bottom: bottomBase },
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
    const fmt = cfg.func === 'pct_distinct'
      ? (/** @type {number} */ v) => (Math.round(v * 1000) / 10 || 0) + '%'
      : (/** @type {number} */ v) => String(NS.ddNum(v || 0));
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
      type: 'value', name: P.valueTitle, nameLocation: 'middle', nameGap: 45,
      nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize },
      axisLabel: { color: ax.labelColor, fontSize: ax.fontSize },
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
    NS.axes.addValueLines(wfSeries, valAxis, false, false, cfg.value_lines, ink);
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
              p.value == null ? '–' : NS.ddNum(p.value));
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
