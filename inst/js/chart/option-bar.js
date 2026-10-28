// @ts-check
/**
 * Chart block: the bar and waterfall options. One panel of the aggregated
 * model in; its ECharts option, panel height, x-label fit inputs and
 * thinned-label note out.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});
  const option = /** @type {any} */ (NS.option = NS.option || {});
  const parts = /** @type {any} */ (option.aggParts = option.aggParts || {});
  const { barRadius, barValueLabels, barMarkLabels, stackNames, boldNames, endFormat,
          WATERFALL_COLORS, withItemStyle, fitXLabels } = parts;
  const { BAR_MAX, ROW_BAND, GROUP_BAR, GROUP_PAD, X_INSET } = parts.BAR;

  /** What every part of a bar's option reads: groups, colour levels, layout and colours.
   * @param {any} m @param {any} panel @param {any} look @param {number} plotW */
  const barContext = (m, panel, look, plotW) => {
    const { cfg, ink } = look;
    const groups = panel.groups;
    const colors = m.colors;
    const palette = NS.paletteOf(cfg);
    const isGrouped = cfg.position === 'dodge';
    const isPercent = cfg.position === 'fill';
    const vertical = cfg.orientation === 'vertical';
    // Horizontal: a fixed row band; past the panel cap a grouped fan
    // squeezes into its band. Vertical: bands come from the width.
    const rowBand = isGrouped && colors.length > 0 ? colors.length * GROUP_BAR + GROUP_PAD : ROW_BAND;
    const rowsH = groups.length * rowBand;
    const hCapped = !vertical && rowsH > NS.PANEL_H_CAP;
    // `highlight`: a bar without a highlighted row is grey.
    const hlLit = m.hlLit;
    const muted = NS.mutedMark(ink);
    const levelColor = m.hlColors
      ? NS.litLevelColors(colors, m.hlColors, m.colorScale, palette)
      : NS.levelColors(colors, m.colorScale, palette);
    /** @param {any} v @param {string} g @param {string | null} lv */
    const greyed = (v, g, lv) => (v == null || !hlLit || hlLit(panel.facet, g, lv)) ? v
      : withItemStyle(v, { color: muted });
    // A share needs a colour split; pct_distinct is a share of the panel.
    const showPercent = isPercent && colors.length > 0;
    const pctFunc = cfg.func === 'pct_distinct';
    const bandPx = vertical ? (plotW ? (plotW - X_INSET) / Math.max(1, groups.length) : 0)
      : (hCapped ? NS.PANEL_H_CAP / Math.max(1, groups.length) : ROW_BAND);
    return { m, panel, look, cfg, ink, plotW, groups, colors, palette, isGrouped, isPercent,
             vertical, rowsH, hCapped, hlLit, muted, levelColor, greyed, showPercent, pctFunc,
             asFraction: showPercent || pctFunc, bandPx };
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
      // null for a cell without rows, so a stack skips it; a percent datum
      // keeps its raw value for the tooltip.
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
    // Round the value end: every grouped bar, and in a stack the outermost
    // segment of each group.
    if (isGrouped) {
      for (const s of series) s.itemStyle.borderRadius = barRadius(vertical);
      return series;
    }
    for (let gi = 0; gi < groups.length; gi++) {
      const top = series.findLastIndex((/** @type {any} */ s) => s.data[gi] != null);
      if (top >= 0) {
        series[top].data[gi] = withItemStyle(series[top].data[gi],
                                             { borderRadius: barRadius(vertical) });
      }
    }
    return series;
  };

  /** The groups with their "(n)" count, and the formatter that adds it.
   * @param {any} panel */
  const countLabels = (panel) => {
    const counts = panel.axisCounts;
    return {
      catLabels: counts ? panel.groups.map((/** @type {string} */ g) => NS.withCount(g, counts))
        : panel.groups,
      catFmt: counts ? (/** @type {any} */ v) => NS.withCount(v, counts) : null
    };
  };

  /** The category axis. "(n)" comes from a formatter, so the axis data stays the bare group clicks
   * and tooltips key off. A highlighted bar's name is semibold; `decorate` reapplies that after a
   * resize. @param {any} k */
  const barCategoryAxis = (k) => {
    const { panel, groups, colors, vertical, hlLit, ink, look } = k;
    const { catLabels, catFmt } = countLabels(panel);
    const gut = NS.axes.yGutter(catLabels, look.measure);
    /** @type {((axisLabel: any) => any) | null} */
    let decorate = null;
    if (hlLit) {
      const lit = new Set(groups.filter((/** @type {string} */ g) => (colors.length
        ? colors.some((/** @type {string} */ c) => hlLit(panel.facet, g, c))
        : hlLit(panel.facet, g, null))));
      decorate = (axisLabel) => boldNames(axisLabel, lit, ink);
    }
    const fit = vertical ? fitXLabels(catLabels, k.plotW, look, catFmt, decorate) : null;
    const xlab = fit && fit.xlab;
    const axis = NS.axes.categoryAxis(groups, ink, { xlab, display: catFmt, gut });
    if (decorate) axis.axisLabel = decorate(axis.axisLabel);
    return { axis, catLabels, gut, xlab, xFit: fit && fit.xFit };
  };

  /** The value axis: ticks in `format`, the unit once in the title. @param {any} k @param {any} P
   */
  const barValueAxis = (k, P) => {
    const { cfg, asFraction, showPercent, pctFunc, vertical, ink } = k;
    // "% of panel" names the column its denominator is taken over.
    const ofCol = { facet: cfg.facet, category: cfg.category, color: cfg.color }[cfg.percent_of];
    const name = showPercent ? '% of group total'
      : (pctFunc ? '% of ' + (ofCol ? P.title(ofCol) : 'total')
        : NS.axisName(P.valueTitle, cfg.measure, cfg));
    const format = asFraction ? (/** @type {number} */ v) => Math.round(v * 100) + '%'
      : NS.valueFormat(cfg, { unit: false });
    return {
      ...NS.axes.valueAxis(ink, 'value', name, vertical ? 45 : 30, { format, grid: 'dashed' }),
      ...(asFraction ? { max: 1 } : {})
    };
  };

  /** Value labels, mark text, reference lines and ranges and stack level names. Returns the px each
   * needs on the grid's right. @param {any} k @param {any[]} series @param {any} valAxis
   * @param {any} cat */
  const barSides = (k, series, valAxis, cat) => {
    const { m, panel, cfg, ink, groups, colors, vertical, isGrouped, isPercent } = k;
    const { measure } = k.look;
    const valueLabels = cfg.value_labels === true;
    const valuePx = vertical ? 350 - 30 - 66 : (k.plotW ? k.plotW - cat.gut.gridLeft - 5 : 0);
    const labelRight = valueLabels
      ? barValueLabels(series, valAxis, {
          ink, vertical, stacked: !isGrouped && !isPercent && colors.length > 0,
          percent: k.showPercent, fraction: k.pctFunc, plotPx: valuePx,
          bandPx: vertical ? k.bandPx : ROW_BAND, measure, cfg
        })
      : 0;
    const markRight = m.markLabels
      ? barMarkLabels(series, valAxis, {
          textAt: (/** @type {number} */ si, /** @type {number} */ gi) =>
            NS.model.labelOf(m, panel, groups[gi], colors.length ? colors[si] : null),
          groups, nBars: Math.max(1, colors.length), vertical,
          stack: !isGrouped && colors.length > 0, percent: k.showPercent,
          valueLabels, valuePx, bandPx: k.bandPx, ink, measure
        })
      : 0;
    NS.axes.addValueLines(series, valAxis, !vertical, k.asFraction, cfg.y_lines, ink);
    // A histogram's bins are ranges, not categories to name.
    if (!m.histogram) {
      NS.axes.addCategoryMarks(series, vertical, groups, cfg.x_lines, cfg.x_ranges, ink);
    }
    // `legend: "marks"`: after the series the tooltip and highlight read.
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

  /** The tooltip. A split bar's card lists every level of the hovered category with the hovered
   * segment marked (the item trigger knows which). A 100% bar shows each level's share, its value
   * after it; any other bar the value named by its aggregation, with n, and the total only where
   * segments add up (a count, a sum). @param {any} k @param {any} P @param {any[]} series */
  const barTooltip = (k, P, series) => {
    const { m, panel, cfg, colors, showPercent, pctFunc } = k;
    const { title, aggLabel } = P;
    const esc = NS.esc;
    const split = colors.length > 0;
    const vf = k.asFraction ? null : NS.valueFormat(cfg);
    const fmtRaw = vf || ((/** @type {number} */ n) =>
      Number.isInteger(n) ? n : Math.round(n * 100) / 100);
    /** @type {Record<string, number>} */
    const nOf = {};
    for (const a of panel.cells) nOf[a.group + '|||' + a.color] = a.n;
    const showN = cfg.func !== 'count' && cfg.func !== 'identity';
    const additive = cfg.func === 'count' || cfg.func === 'sum';
    const colorName = split ? esc(title(cfg.color) || cfg.color) + ' ' : '';
    /** The rows of the hovered category: one per level, else the hovered bar. */
    const levelsAt = (/** @type {any} */ arg) => {
      const p = Array.isArray(arg) ? arg[0] : arg;
      if (!p) return { ps: [], hit: null };
      if (!split) return { ps: [].concat(arg), hit: null };
      const ps = series.slice(0, colors.length).map((sr) => {
        const d = sr.data[p.dataIndex];
        const obj = d != null && typeof d === 'object';
        return { name: p.name, seriesName: sr.name, value: obj ? d.value : d,
                 data: obj ? d : null, color: sr.itemStyle.color };
      });
      return { ps, hit: p.seriesName };
    };
    const extraRows = (/** @type {any} */ head) => {
      const extra = panel.ttRep ? panel.ttRep[head] : null;
      if (!extra) return '';
      return m.ttFields.map((/** @type {string} */ c, /** @type {number} */ i) =>
        (extra[i] != null && extra[i] !== '') ? NS.tipRow(esc(title(c) || c), esc(extra[i])) : ''
      ).join('');
    };
    /** @param {any} p @param {string} head */
    const valueText = (p, head) => {
      if (showPercent) {
        const raw = p.data && p.data.raw != null ? ' (' + fmtRaw(p.data.raw) + ')' : '';
        return Math.round((Number(p.value) || 0) * 100) + '%' + raw;
      }
      const n = nOf[head + '|||' + (split ? p.seriesName : '__all__')];
      if (pctFunc) return Math.round(Number(p.value) * 1000) / 10 + '%' + (n != null ? ' (' + n + ')' : '');
      return fmtRaw(Number(p.value)) + (showN && n != null ? ' (n=' + n + ')' : '');
    };
    const formatter = (/** @type {any} */ arg) => {
      const { ps, hit } = levelsAt(arg);
      if (!ps.length) return '';
      const head = ps[0].axisValueLabel || ps[0].name || '';
      const vals = ps.filter((/** @type {any} */ p) => p && p.value != null);
      const rows = vals.map((/** @type {any} */ p) => NS.tipRow(
        split ? colorName + esc(p.seriesName) : esc(aggLabel), valueText(p, head),
        split ? p.color : null, hit == null ? null : p.seriesName === hit)).join('');
      const total = split && !showPercent && additive && vals.length > 1
        ? NS.TIP_SEP + NS.tipRow(esc(aggLabel), fmtRaw(vals.reduce(
          (/** @type {number} */ a, /** @type {any} */ p) => a + Number(p.value || 0), 0)))
        : '';
      const note = split && !showPercent && !additive ? NS.tipNote(esc(aggLabel)) : '';
      return NS.tipHead(esc(head)) + note + rows + total + extraRows(head);
    };
    return split ? { trigger: 'item', confine: true, formatter }
      : { trigger: 'axis', axisPointer: { type: 'shadow' }, confine: true, formatter };
  };

  /** A plain, stacked, grouped or percent bar. @param {any} m @param {any} panel @param {any} look
   * @param {number} plotW */
  const bar = (m, panel, look, plotW) => {
    const k = barContext(m, panel, look, plotW);
    const P = parts.common(look);
    const { groups, colors, vertical } = k;
    const series = barSeries(k);
    const cat = barCategoryAxis(k);
    const valAxis = barValueAxis(k, P);
    // A histogram's bins touch.
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
    const { xlab, gut } = cat;
    const bottomBase = vertical ? 40 + 26 + (xlab ? xlab.bottom : 0) : 20 + 26;
    return {
      xFit: cat.xFit || undefined,
      // Horizontal: a fixed band per row. Vertical: canvas plus label gutter.
      panelH: vertical ? 350 + (xlab ? xlab.bottom : 0)
        : 30 + Math.min(NS.PANEL_H_CAP, k.rowsH) + bottomBase,
      labelNote: k.hCapped
        ? NS.axes.thinnedLabelNote(NS.PANEL_H_CAP, groups.length, P.title(k.cfg.category)) : null,
      option: {
        ...P.base,
        tooltip: barTooltip(k, P, series),
        // A hidden legend keeps the selection model the HTML chips toggle.
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
   * A waterfall's offset and step data, coloured by sign; a total step
   * restates the running total from zero. A step is summed across a colour
   * split; one without a value is a gap. `lo` and `hi` bound the bridge.
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
    const totals = new Set(NS.labelled(cfg.waterfall_totals).map((x) => String(x.value)));
    /** @type {any[]} */
    const base = [];
    /** @type {any[]} */
    const delta = [];
    let cum = 0;
    const fmt = endFormat(cfg, cfg.func === 'pct_distinct');
    /** @param {number} v @param {boolean} up */
    const lab = (v, up) => labelsOn
      ? { label: { show: true, position: up ? 'top' : 'bottom', formatter: fmt(v) } } : {};
    /** @param {number} offset @param {number} value @param {string} color
     * @param {boolean} floating @param {number} shown @param {boolean} up */
    const push = (offset, value, color, floating, shown, up) => {
      base.push(offset);
      delta.push({ value, itemStyle: { color, borderRadius: barRadius(true, floating) },
                   ...lab(shown, up) });
    };
    let lo = 0, hi = 0;
    for (const g of panel.groups) {
      const v = valOf(g);
      if (totals.has(g)) {
        push(0, cum, WATERFALL_COLORS.total, false, cum, cum >= 0);
      } else if (v == null) {
        base.push(0);
        delta.push(null);
        continue;
      } else if (v >= 0) {
        push(cum, v, WATERFALL_COLORS.increase, cum !== 0, v, true);
        cum += v;
      } else {
        push(cum + v, -v, WATERFALL_COLORS.decrease, cum + v !== 0, v, false);
        cum += v;
      }
      lo = Math.min(lo, cum);
      hi = Math.max(hi, cum);
    }
    return { base, delta, lo, hi };
  };

  /** A waterfall: a transparent base series carries each step's offset, a visible delta series the
   * step. Vertical only. @param {any} m @param {any} panel @param {any} look @param {number} plotW
   */
  const waterfall = (m, panel, look, plotW) => {
    const { cfg, ink, measure } = look;
    const P = parts.common(look);
    const labelsOn = cfg.value_labels === true;
    const wvf = cfg.func === 'pct_distinct' ? null : NS.valueFormat(cfg);
    const { base, delta, lo, hi } = waterfallSteps(m, panel, cfg, labelsOn, wvf);

    const { catLabels, catFmt } = countLabels(panel);
    const { xlab, xFit } = fitXLabels(catLabels, plotW, look, catFmt);
    const catAxis = { ...NS.axes.categoryAxis(panel.groups, ink, { xlab, display: catFmt }),
                      splitLine: { show: false } };
    const valAxis = NS.axes.valueAxis(ink, 'value', NS.axisName(P.valueTitle, cfg.measure, cfg), 45, {
      format: wvf && NS.valueFormat(cfg, { unit: false }), grid: 'dashed' });
    const label = { show: false, distance: 4, color: ink.muted, fontSize: ink.fontSize,
                    fontFamily: ink.face };
    /** @type {any[]} */
    const wfSeries = [
      { name: 'base', type: 'bar', stack: 'waterfall', barWidth: '60%',
        itemStyle: { color: 'transparent', borderColor: 'transparent' },
        emphasis: { disabled: true }, silent: true, tooltip: { show: false }, data: base },
      { name: 'delta', type: 'bar', stack: 'waterfall', barWidth: '60%',
        emphasis: { focus: 'self' }, data: delta,
        ...(labelsOn ? { label, labelLayout: { hideOverlap: true } } : {}) }
    ];
    let labelRight = 0;
    if (labelsOn) {
      valAxis.boundaryGap = [lo < 0 ? '12%' : 0, hi > 0 ? '12%' : 0];
      let w = 0;
      for (const d of delta) if (d && d.label) w = Math.max(w, measure(d.label.formatter));
      labelRight = Math.max(0, Math.ceil(w / 2 - 10));
    }
    NS.axes.addValueLines(wfSeries, valAxis, false, false, cfg.y_lines, ink);
    return {
      panelH: 350 + xlab.bottom,
      xFit,
      labelNote: null,
      option: {
        ...P.base,
        tooltip: {
          trigger: 'axis', axisPointer: { type: 'shadow' }, confine: true,
          // Only the visible delta; the base is how the bar floats.
          formatter: (/** @type {any} */ params) => {
            const p = (params || []).find((/** @type {any} */ x) => x.seriesName === 'delta');
            if (!p) return '';
            return NS.tipHead(NS.esc(p.name)) + NS.tipRow(NS.esc(P.aggLabel),
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
