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
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});
  const option = /** @type {any} */ (NS.option = NS.option || {});
  const parts = /** @type {any} */ (option.aggParts = option.aggParts || {});
  const { barRadius, barValueLabels, barMarkLabels, stackNames, pctName, boldNames,
          WATERFALL_COLORS } = parts;
  const { BAR_MAX, ROW_BAND, GROUP_BAR, GROUP_PAD, X_INSET } = parts.BAR;


  /**
   * What every part of a bar's option reads: the panel's groups and colour
   * levels, the layout and the colours.
   * @param {any} m @param {any} panel @param {any} look @param {number} plotW
   */
  const barContext = (m, panel, look, plotW) => {
    const { cfg, ink } = look;
    const groups = panel.groups;
    const colors = m.colors;
    const palette = NS.paletteOf(cfg);
    const barMode = cfg.bar_mode || 'stacked';
    const isGrouped = barMode === 'grouped';
    const isPercent = barMode === 'percent';
    const vertical = cfg.orientation === 'vertical';
    // Horizontal: the row band is fixed, so bars are sized off it; past the
    // panel cap a grouped fan squeezes into its band. Vertical: bands come
    // from the width, capped so a few categories do not draw slabs.
    const rowBand = isGrouped && colors.length > 0 ? colors.length * GROUP_BAR + GROUP_PAD : ROW_BAND;
    const rowsH = groups.length * rowBand;
    const hCapped = !vertical && rowsH > NS.PANEL_H_CAP;
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
    // A share needs a colour split (one series is 100% of itself);
    // pct_distinct is a share of the panel and needs none.
    const showPercent = isPercent && colors.length > 0;
    const pctFunc = cfg.func === 'pct_distinct';
    const bandPx = vertical ? (plotW ? (plotW - X_INSET) / Math.max(1, groups.length) : 0)
      : (hCapped ? NS.PANEL_H_CAP / Math.max(1, groups.length) : ROW_BAND);
    return { m, panel, look, cfg, ink, plotW, groups, colors, palette, isGrouped, isPercent,
             vertical, rowsH, hCapped, hlLit, muted, levelColor, greyed, showPercent, pctFunc,
             asFraction: showPercent || pctFunc, bandPx,
             valueLabelsOn: cfg.value_labels === 'on' || cfg.value_labels === true };
  };

  /** The bar series: one, or one per colour level. @param {any} k */
  const barSeries = (k) => {
    const { m, panel, groups, colors, vertical, isGrouped, isPercent, greyed } = k;
    if (colors.length === 0) {
      // A null aggregate stays null: a gap, not a zero bar.
      return [{
        type: 'bar',
        data: groups.map((/** @type {string} */ g) => {
          const d = NS.model.cellOf(m, panel, g);
          return greyed(d ? d.value : null, g, null);
        }),
        itemStyle: { color: k.palette[0], borderRadius: barRadius(vertical) },
        barWidth: '60%', barMaxWidth: BAR_MAX, emphasis: { focus: 'self' }
      }];
    }
    const barLayout = isGrouped
      ? (vertical
          ? { barGap: 0, barCategoryGap: '30%', barMaxWidth: BAR_MAX }
          : (k.hCapped ? { barGap: 0, barCategoryGap: '20%' } : { barGap: 0, barWidth: GROUP_BAR }))
      : { barWidth: '60%', barMaxWidth: BAR_MAX };
    /** @type {Record<string, number>} */
    const groupTotals = {};
    if (isPercent) {
      for (const g of groups) {
        groupTotals[g] = NS.model.cellsOf(m, panel, g)
          .filter((/** @type {any} */ a) => a.value != null)
          .reduce((/** @type {number} */ s, /** @type {any} */ a) => s + a.value, 0);
      }
    }
    const series = colors.map((/** @type {string} */ color, /** @type {number} */ ci) => ({
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
      itemStyle: { color: k.levelColor[ci] || k.muted },
      ...barLayout,
      emphasis: { focus: 'self' }
    }));
    roundValueEnds(series, groups.length, vertical, isGrouped);
    return series;
  };

  /**
   * The value end rounds: every grouped bar, and in a stack the outermost
   * segment of each group, which varies with missing cells.
   * @param {any[]} series @param {number} nGroups @param {boolean} vertical @param {boolean} grouped
   */
  const roundValueEnds = (series, nGroups, vertical, grouped) => {
    if (grouped) {
      for (const s of series) s.itemStyle.borderRadius = barRadius(vertical);
      return;
    }
    for (let gi = 0; gi < nGroups; gi++) {
      for (let si = series.length - 1; si >= 0; si--) {
        const d = series[si].data[gi];
        if (d == null) continue;
        series[si].data[gi] = (typeof d === 'object')
          ? { ...d, itemStyle: { ...d.itemStyle, borderRadius: barRadius(vertical) } }
          : { value: d, itemStyle: { borderRadius: barRadius(vertical) } };
        break;
      }
    }
  };

  /**
   * The category axis. "(n)" comes from a formatter, so the axis data stays
   * the bare group the clicks and tooltips key off. A highlighted bar's name
   * is semibold; a resize that turns the x labels again keeps it
   * (`decorate`).
   * @param {any} k @param {any} P
   */
  const barCategoryAxis = (k, P) => {
    const { panel, groups, colors, vertical, hlLit, ink } = k;
    const measure = k.look.measure;
    const axisCounts = panel.axisCounts;
    const catLabels = axisCounts
      ? groups.map((/** @type {string} */ g) => NS.withCount(g, axisCounts)) : groups;
    const catFmt = axisCounts ? (/** @type {any} */ v) => NS.withCount(v, axisCounts) : null;
    const gut = NS.axes.yGutter(catLabels, measure);
    const xlab = vertical
      ? NS.axes.xAxisLabels(catLabels, (k.plotW || 0) - X_INSET, false, measure, ink) : null;
    /** @type {any} */
    const axis = vertical
      ? { type: 'category', data: groups,
          axisLabel: NS.axes.axisLabelWithDisplay(xlab && xlab.axisLabel, catFmt),
          axisLine: { lineStyle: { color: ink.strong } }, axisTick: { show: false } }
      : { type: 'category', data: groups, inverse: true,
          axisLabel: { color: P.ax.labelColor, fontSize: P.ax.fontSize, align: 'left',
                       margin: gut.margin, width: gut.width, overflow: 'truncate',
                       ellipsis: '…', ...(catFmt ? { formatter: catFmt } : {}) },
          axisLine: { show: false }, axisTick: { show: false } };
    /** @type {((axisLabel: any) => any) | null} */
    let decorate = null;
    if (hlLit) {
      const lit = new Set(groups.filter((/** @type {string} */ g) => (colors.length
        ? colors.some((/** @type {string} */ c) => hlLit(panel.facet, g, c))
        : hlLit(panel.facet, g, null))));
      decorate = (axisLabel) => boldNames(axisLabel, lit, ink);
      axis.axisLabel = decorate(axis.axisLabel);
    }
    return { axis, catLabels, catFmt, gut, xlab, decorate };
  };

  /**
   * The value axis: ticks in `format`, the unit once in the title; a share
   * keeps its own percent.
   * @param {any} k @param {any} P
   */
  const barValueAxis = (k, P) => {
    const { cfg, asFraction, showPercent, pctFunc, vertical, ink } = k;
    const tickFmt = asFraction ? null : NS.valueFormat(cfg, { unit: false });
    return {
      type: 'value',
      name: showPercent ? '% of group total'
        : (pctFunc ? pctName(cfg, P.title) : NS.axisName(P.valueTitle, cfg.value, cfg)),
      nameLocation: 'middle',
      nameGap: vertical ? 45 : 30,
      nameTextStyle: { color: P.ax.labelColor, fontSize: P.ax.fontSize },
      axisLabel: {
        color: P.ax.labelColor, fontSize: P.ax.fontSize,
        ...(asFraction ? { formatter: (/** @type {number} */ v) => Math.round(v * 100) + '%' } : {}),
        ...(tickFmt ? { formatter: tickFmt } : {})
      },
      ...(asFraction ? { max: 1 } : {}),
      axisLine: { lineStyle: { color: ink.strong } },
      splitLine: { lineStyle: { color: P.ax.splitLineColor, type: 'dashed' } }
    };
  };

  /**
   * What a bar draws beside its bars: value labels, text on the marks, the
   * reference lines and ranges, and the level names on a stack. Returns the
   * px each needs on the grid's right.
   * @param {any} k @param {any[]} series @param {any} valAxis @param {any} cat
   */
  const barSides = (k, series, valAxis, cat) => {
    const { m, panel, cfg, ink, groups, colors, vertical, isGrouped, isPercent } = k;
    const { measure } = k.look;
    const ax = parts.common(k.look).ax;
    const valuePx = vertical ? 350 - 30 - 66 : (k.plotW ? k.plotW - cat.gut.gridLeft - 5 : 0);
    const labelRight = k.valueLabelsOn
      ? barValueLabels(series, valAxis, {
          ax, vertical, stacked: !isGrouped && !isPercent && colors.length > 0,
          percent: k.showPercent, fraction: k.pctFunc, plotPx: valuePx,
          bandPx: vertical ? k.bandPx : ROW_BAND, measure, face: ink.face, cfg
        })
      : 0;
    const markRight = m.markLabels
      ? barMarkLabels(series, valAxis, {
          textAt: (/** @type {number} */ si, /** @type {number} */ gi) =>
            NS.model.labelOf(m, panel, groups[gi], colors.length ? colors[si] : null),
          groups, nBars: Math.max(1, colors.length), vertical,
          stack: !isGrouped && colors.length > 0, percent: k.showPercent,
          valueLabels: k.valueLabelsOn, valuePx, bandPx: k.bandPx, ax, face: ink.face, measure
        })
      : 0;
    NS.axes.addValueLines(series, valAxis, !vertical, k.asFraction, cfg.value_lines, ink,
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
      const bandPx = k.bandPx;
      const barPx = vertical ? (bandPx ? Math.min(BAR_MAX, 0.6 * bandPx) : BAR_MAX / 2)
        : Math.min(BAR_MAX, 0.6 * bandPx);
      namesRight = stackNames(series, { vertical, groups, bandPx, barPx, valuePx, measure, ink });
    }
    return { labelRight, markRight, namesRight };
  };

  /**
   * The tooltip. A split bar's card lists every level of the hovered
   * category, the segment under the pointer marked: the item trigger says
   * which segment that is, the axis trigger only knows the category.
   * @param {any} k @param {any} P @param {any[]} series
   */
  const barTooltip = (k, P, series) => {
    const { m, panel, cfg, colors } = k;
    const { esc, title } = P;
    const vf = k.asFraction ? null : NS.valueFormat(cfg);
    const fmtRaw = vf || ((/** @type {number} */ n) =>
      Number.isInteger(n) ? n : Math.round(n * 100) / 100);
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
    const extraRows = (/** @type {any} */ head) => {
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
    const t = { levelsAt, colorName, extraRows, fmtRaw };
    tooltip.formatter = k.showPercent ? percentTip(k, P, t) : valueTip(k, P, t);
    return tooltip;
  };

  /** A 100% bar's card: each level's share, its value after it.
   * @param {any} k @param {any} P @param {any} t */
  const percentTip = (k, P, t) => (/** @type {any} */ arg) => {
    const { ps, hit } = t.levelsAt(arg);
    if (!ps.length) return '';
    const head = ps[0].axisValueLabel || ps[0].name || '';
    const rows = ps.filter((/** @type {any} */ p) => p && p.value != null).map((/** @type {any} */ p) => {
      const pct = Math.round((Number(p.value) || 0) * 100);
      const raw = p.data && p.data.raw != null ? t.fmtRaw(p.data.raw) : null;
      return NS.tipRow(t.colorName + P.esc(p.seriesName),
        pct + '%' + (raw != null ? ' (' + raw + ')' : ''), p.color,
        hit == null ? null : p.seriesName === hit);
    });
    return NS.tipHead(P.esc(head)) + rows.concat(t.extraRows(head)).join('');
  };

  /** A bar's card: the value named by its aggregation, with n where it says
   * something; the segments' total only where they add up (a count, a sum).
   * @param {any} k @param {any} P @param {any} t */
  const valueTip = (k, P, t) => {
    const { panel, cfg, colors, pctFunc } = k;
    const esc = P.esc;
    /** @type {Record<string, number>} */
    const nOf = {};
    for (const a of panel.cells) nOf[a.group + '|||' + a.color] = a.n;
    const aggLabel = P.aggLabel;
    const showN = cfg.func !== 'count' && cfg.func !== 'identity';
    const additive = cfg.func === 'count' || cfg.func === 'sum';
    return (/** @type {any} */ arg) => {
      const { ps, hit } = t.levelsAt(arg);
      if (!ps.length) return '';
      const head = ps[0].axisValueLabel || ps[0].name || '';
      const vals = ps.filter((/** @type {any} */ p) => p && p.value != null);
      const rows = vals.map((/** @type {any} */ p) => {
        const nm = colors.length ? p.seriesName : aggLabel;
        const n = nOf[head + '|||' + (colors.length ? p.seriesName : '__all__')];
        const shown = pctFunc ? Math.round(Number(p.value) * 1000) / 10 + '%' : t.fmtRaw(Number(p.value));
        return NS.tipRow(colors.length ? t.colorName + esc(nm) : esc(nm),
          shown + (pctFunc && n != null ? ' (' + n + ')' : '') +
          (!pctFunc && showN && n != null ? ' (n=' + n + ')' : ''),
          colors.length ? p.color : null, hit == null ? null : p.seriesName === hit);
      });
      const foot = colors.length && vals.length > 1 && additive && !pctFunc
        ? NS.TIP_SEP + NS.tipRow(esc(aggLabel),
          t.fmtRaw(vals.reduce((/** @type {number} */ a, /** @type {any} */ p) => a + Number(p.value || 0), 0)))
        : '';
      const note = colors.length && !(additive && !pctFunc) ? NS.tipNote(esc(aggLabel)) : '';
      return NS.tipHead(esc(head)) + note + rows.join('') + foot + t.extraRows(head).join('');
    };
  };

  /**
   * A plain, stacked, grouped or percent bar.
   * @param {any} m @param {any} panel @param {any} look @param {number} plotW
   */
  const bar = (m, panel, look, plotW) => {
    const k = barContext(m, panel, look, plotW);
    const P = parts.common(look);
    const { ink, groups, colors, vertical } = k;
    const series = barSeries(k);
    const cat = barCategoryAxis(k, P);
    const valAxis = barValueAxis(k, P);
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
    const side = barSides(k, series, valAxis, cat);
    const tooltip = barTooltip(k, P, series);
    const { xlab, gut } = cat;
    const bottomBase = vertical ? 40 + 26 + (xlab ? xlab.bottom : 0) : 20 + 26;
    return {
      xFit: xlab ? { labels: cat.catLabels, inset: X_INSET, decimate: false, gutter: xlab.bottom,
                     key: xlab.key, formatter: cat.catFmt, decorate: cat.decorate } : undefined,
      // Horizontal: one fixed band per row. Vertical: the default canvas plus
      // the turned-label gutter, so labels lengthen the canvas.
      panelH: vertical ? 350 + (xlab ? xlab.bottom : 0)
        : 30 + Math.min(NS.PANEL_H_CAP, k.rowsH) + bottomBase,
      labelNote: k.hCapped
        ? NS.axes.thinnedLabelNote(NS.PANEL_H_CAP, groups.length, P.title(k.cfg.group)) : null,
      option: {
        ...(look.theme ? {} : { backgroundColor: 'transparent' }),
        textStyle: { fontFamily: ink.face },
        tooltip,
        // The HTML band shows the chips; a hidden legend keeps the
        // selection model the chips toggle.
        legend: colors.length > 0 ? { show: false, data: colors } : undefined,
        grid: vertical
          ? { left: 55, right: 10 + Math.max(side.labelRight, side.markRight, side.namesRight),
              top: 30, bottom: bottomBase }
          : { left: gut.gridLeft, right: 5 + Math.max(side.labelRight, side.markRight), top: 30,
              bottom: bottomBase },
        xAxis: vertical ? cat.axis : valAxis,
        yAxis: vertical ? valAxis : cat.axis,
        series
      }
    };
  };

  /**
   * A waterfall's two stacked series: the transparent offset each step
   * floats on, and the step itself, coloured by sign; a total step restates
   * the running total from zero. One value per step, summed across a colour
   * split; a step without a value is a gap. `lo` and `hi` bound the bridge.
   * @param {any} m @param {any} panel @param {Record<string, any>} cfg
   * @param {boolean} labelsOn @param {((v: number) => string) | null} wvf
   */
  const waterfallSteps = (m, panel, cfg, labelsOn, wvf) => {
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
    const fmt = cfg.func === 'pct_distinct'
      ? (/** @type {number} */ v) => (Math.round(v * 1000) / 10 || 0) + '%'
      : (wvf || ((/** @type {number} */ v) => String(NS.ddNum(v || 0))));
    /** @param {number} v @param {boolean} up */
    const lab = (v, up) => labelsOn
      ? { label: { show: true, position: up ? 'top' : 'bottom', formatter: fmt(v) } } : {};
    let lo = 0, hi = 0;
    for (const g of panel.groups) {
      const v = valOf(g);
      const isTotal = totals.has(g);
      if (v == null && !isTotal) {
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
    return { base, delta, lo, hi };
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
    const labelsOn = cfg.value_labels === 'on' || cfg.value_labels === true;
    const wvf = cfg.func === 'pct_distinct' ? null : NS.valueFormat(cfg);
    const { base, delta, lo, hi } = waterfallSteps(m, panel, cfg, labelsOn, wvf);

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

  Object.assign(parts, { bar, waterfall });
})();
