// @ts-check
/**
 * Chart block v2: the boxplot and point range options.
 *
 * The model has already summarised every category slot (one per group, or
 * per group and colour level); this lays the slots on a category axis and
 * draws a box, or a centre dot on an interval, per slot. A colour split
 * gives each level its own series, so the legend and the colours come for
 * free; a split boxplot also gives each level its own (hidden) category
 * axis over the same slots, because ECharts dodges boxplot series that
 * share one axis.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});
  const option = /** @type {any} */ (NS.option = NS.option || {});
  const parts = /** @type {any} */ (option.aggParts = option.aggParts || {});

  const X_INSET = 65;

  /**
   * The interval of a point range as a custom series: a line from lo to hi
   * with caps. Both ends map to the value axis (encode), so the axis takes
   * in the whole interval. A datum's own itemStyle opacity dims it.
   * @param {any} name @param {any[]} pts [slot, lo, hi] each @param {string} clr
   * @param {boolean} horizontal
   */
  const whiskerSeries = (name, pts, clr, horizontal) => ({
    type: 'custom',
    name,
    legendHoverLink: false,
    silent: true,
    z: 1,
    data: pts,
    encode: horizontal ? { y: 0, x: [1, 2] } : { x: 0, y: [1, 2] },
    renderItem: (/** @type {any} */ params, /** @type {any} */ api) => {
      const base = api.value(0), lo = api.value(1), hi = api.value(2);
      const pLo = horizontal ? api.coord([lo, base]) : api.coord([base, lo]);
      const pHi = horizontal ? api.coord([hi, base]) : api.coord([base, hi]);
      const w = 4;
      const st = api.style ? api.style() : null;
      const opacity = st && st.opacity != null ? st.opacity : 1;
      const style = { stroke: clr, lineWidth: 1, opacity };
      // Caps run across the whisker.
      const cap = (/** @type {number[]} */ p) => horizontal
        ? { x1: p[0], y1: p[1] - w, x2: p[0], y2: p[1] + w }
        : { x1: p[0] - w, y1: p[1], x2: p[0] + w, y2: p[1] };
      return {
        type: 'group',
        children: [
          { type: 'line', shape: { x1: pLo[0], y1: pLo[1], x2: pHi[0], y2: pHi[1] }, style },
          { type: 'line', shape: cap(pLo), style },
          { type: 'line', shape: cap(pHi), style }
        ]
      };
    }
  });

  /**
   * @param {any} m @param {any} panel @param {any} look @param {number} plotW
   */
  const distribution = (m, panel, look, plotW) => {
    const { cfg, ink, measure } = look;
    if (m.noValue) return null;
    const P = parts.common(look);
    const { ax, esc } = P;
    const SEP = NS.model.BOX_CAT_SEP;
    const isBox = cfg.chart_type === 'boxplot';
    const vertical = cfg.orientation !== 'horizontal';
    const bodyMeta = NS.roles.statMeta(cfg.summary || (isBox ? 'median_q1_q3' : 'mean_se'));
    const whiskMeta = NS.roles.statMeta(cfg.whiskers || 'tukey');
    const palette = NS.paletteOf(cfg);
    const levels = m.levels;
    const split = m.split;
    const scale = m.colorScale;
    const hexFor = (/** @type {string} */ lv, /** @type {number} */ i) =>
      (scale && scale.color && scale.color[lv]) || NS.paletteAt(palette, i);
    const slots = panel.slots;
    const cats = slots.map((/** @type {any} */ s) => s.cat);
    const axisOn = NS.countOn(cfg, 'axis');
    const seriesLevels = split ? levels : ['__all__'];

    /** @type {any[]} */
    let series;
    if (isBox) {
      series = seriesLevels.map((/** @type {string} */ lv, /** @type {number} */ li) => {
        const hex = split ? hexFor(lv, li) : palette[0];
        // ECharts reads datum.value unconditionally, so an empty slot is a
        // NaN five-tuple: nothing draws and the slots stay aligned.
        const EMPTY_BOX = () => ({ value: [NaN, NaN, NaN, NaN, NaN], n: 0 });
        const data = slots.map((/** @type {any} */ s) => (s.level === lv && s.body)
          ? { value: [s.whisker.lo, s.body.lo, s.body.center, s.body.hi, s.whisker.hi], n: s.n }
          : EMPTY_BOX());
        return {
          type: 'boxplot', name: split ? lv : undefined, data,
          ...(split ? (vertical ? { xAxisIndex: li } : { yAxisIndex: li }) : {}),
          itemStyle: { color: hex + '22', borderColor: hex }
        };
      });
    } else {
      // Per level: a silent interval under a clickable centre dot, and a
      // line through the centres when connect_centers is on. All three share
      // the level's name, so one legend chip hides them together.
      const connect = cfg.connect_centers === 'on' || cfg.connect_centers === true;
      series = [];
      seriesLevels.forEach((/** @type {string} */ lv, /** @type {number} */ li) => {
        const hex = split ? hexFor(lv, li) : palette[0];
        /** @type {any[]} */
        const centers = [];
        /** @type {any[]} */
        const whisks = [];
        slots.forEach((/** @type {any} */ s, /** @type {number} */ ci) => {
          if (s.level !== lv || !s.body) return;
          const b = s.body;
          // The slot rides as the datum name, as a box's does.
          centers.push({ value: vertical ? [ci, b.center] : [b.center, ci], name: cats[ci],
                         center: b.center, n: s.n, lo: b.lo, hi: b.hi });
          whisks.push([ci, b.lo, b.hi]);
        });
        if (connect && centers.length > 1) {
          series.push({
            type: 'line', name: split ? lv : undefined, legendHoverLink: false, silent: true,
            z: 2, data: centers.map((c) => c.value), showSymbol: false,
            lineStyle: { color: hex, width: 1.4 }, itemStyle: { color: hex },
            emphasis: { disabled: true }, tooltip: { show: false }
          });
        }
        if (whisks.length) series.push(whiskerSeries(split ? lv : undefined, whisks, hex, !vertical));
        series.push({
          type: 'scatter', name: split ? lv : undefined, data: centers, z: 4, symbolSize: 7,
          itemStyle: { color: hex, cursor: 'pointer' }, emphasis: { disabled: true }
        });
      });
    }

    // Outliers sit on their box's centre line, silent so the box keeps the
    // click and the tooltip, one series per level like the boxes.
    const pointSeries = !(isBox && cfg.box_points === 'outliers') ? []
      : seriesLevels.map((/** @type {string} */ lv, /** @type {number} */ li) => {
        const hex = split ? hexFor(lv, li) : palette[0];
        /** @type {[number, number][]} */
        const pts = [];
        slots.forEach((/** @type {any} */ s, /** @type {number} */ ci) => {
          if (s.level !== lv) return;
          for (const v of s.outliers) pts.push(vertical ? [ci, v] : [v, ci]);
        });
        return {
          type: 'scatter', name: split ? lv : undefined, data: pts, silent: true,
          symbolSize: 5, z: 3, large: true, largeThreshold: 800,
          itemStyle: { color: hex, opacity: 0.85, borderColor: hex, borderWidth: 1 }
        };
      }).filter((/** @type {any} */ s) => s.data.length > 0);

    // What a slot shows: its group with the box's own "(n)", or the group.
    /** @type {Map<string, string> | null} */
    const catLabelMap = axisOn ? new Map() : null;
    if (catLabelMap) {
      for (const s of slots) catLabelMap.set(s.cat, s.group + ' (' + s.count + ')');
    }
    const catLabels = cats.map((/** @type {string} */ c) => catLabelMap
      ? /** @type {string} */ (catLabelMap.get(c)) : (split ? c.split(SEP)[0] : c));
    const gut = NS.axes.yGutter(catLabels, measure);
    const xlab = vertical
      ? NS.axes.xAxisLabels(catLabels, (plotW || 0) - X_INSET, false, measure, ink) : null;
    const catFmt = catLabelMap
      ? (/** @type {string} */ v) => catLabelMap.get(v) || String(v).split(SEP)[0]
      : (split ? (/** @type {string} */ v) => String(v).split(SEP)[0] : null);
    // The tooltip names the statistics it shows: they are configurable.
    const ttHead = (/** @type {any} */ p) => split ? String(p.name).replace(SEP, ' · ') : p.name;
    const boxTooltipFmt = (/** @type {any} */ p) => {
      const d = p.data;
      // n = 0 is an empty slot.
      if (!d || !Array.isArray(d.value) || !d.n) return '';
      // ECharts hands a box back as [index, wLo, bLo, center, bHi, wHi].
      const five = d.value.slice(-5);
      const h = ttHead(p);
      return (h ? NS.tipHead(esc(h), split ? p.color : null) : '') +
        NS.tipRow('n', d.n) +
        NS.tipRow(esc(bodyMeta.center), NS.ddNum(five[2])) +
        NS.tipRow('Box (' + esc(bodyMeta.range) + ')', NS.ddNum(five[1]) + ', ' + NS.ddNum(five[3])) +
        NS.tipRow('Whiskers (' + esc(whiskMeta.range) + ')',
          NS.ddNum(five[0]) + ' – ' + NS.ddNum(five[4]));
    };
    const rangeTooltipFmt = (/** @type {any} */ p) => {
      const d = p.data;
      if (!d || !d.n) return '';
      const h = ttHead(p);
      return (h ? NS.tipHead(esc(h), split ? p.color : null) : '') +
        NS.tipRow('n', d.n) +
        NS.tipRow(esc(bodyMeta.center), NS.ddNum(d.center)) +
        NS.tipRow(esc(bodyMeta.range), NS.ddNum(d.lo) + ' – ' + NS.ddNum(d.hi));
    };
    const bottomBase = 46 + (vertical && xlab ? xlab.bottom : 0);
    // A distribution reads by position and spread, so the value axis fits
    // the data rather than holding zero.
    const valAxis = {
      type: 'value', scale: true, name: P.title(cfg.value), nameLocation: 'middle',
      nameGap: vertical ? 45 : 30, nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize },
      axisLabel: { color: ax.labelColor, fontSize: ax.fontSize },
      axisLine: { lineStyle: { color: ink.strong } },
      ...(vertical ? { splitLine: { lineStyle: { color: ax.splitLineColor, type: 'dashed' } } } : {})
    };
    const catAxis = vertical
      ? { type: 'category', data: cats,
          axisLabel: NS.axes.axisLabelWithDisplay(xlab && xlab.axisLabel, catFmt),
          axisLine: { lineStyle: { color: ink.strong } }, axisTick: { show: false } }
      : { type: 'category', data: cats, inverse: true,
          axisLabel: { color: ax.labelColor, fontSize: ax.fontSize, align: 'left',
                       margin: gut.margin, width: gut.width, overflow: 'truncate', ellipsis: '…',
                       ...(catFmt ? { formatter: catFmt } : {}) },
          axisLine: { show: false } };
    const catAxes = (isBox && split)
      ? seriesLevels.map((/** @type {any} */ _, /** @type {number} */ i) =>
          (i === 0 ? catAxis : { ...catAxis, show: false }))
      : catAxis;
    NS.axes.addValueLines(series, valAxis, !vertical, false, cfg.value_lines, ink);
    return {
      xFit: (vertical && xlab) ? { labels: catLabels, inset: X_INSET, decimate: false,
                                   gutter: xlab.bottom, key: xlab.key, formatter: catFmt }
        : undefined,
      // Horizontal: one 28 px row per drawn slot, capped.
      panelH: vertical ? 350 + (xlab ? xlab.bottom : 0)
        : 30 + Math.min(NS.PANEL_H_CAP, cats.length * 28) + bottomBase,
      labelNote: (!vertical && cats.length * 28 > NS.PANEL_H_CAP)
        ? NS.axes.thinnedLabelNote(NS.PANEL_H_CAP, cats.length, cfg.group) : null,
      option: {
        ...(look.theme ? {} : { backgroundColor: 'transparent' }),
        textStyle: { fontFamily: ink.face },
        tooltip: { trigger: 'item', confine: true, formatter: isBox ? boxTooltipFmt : rangeTooltipFmt },
        legend: split ? { show: false, data: levels } : undefined,
        grid: vertical
          ? { left: 55, right: 10, top: 30, bottom: bottomBase }
          : { left: gut.gridLeft, right: 5, top: 30, bottom: bottomBase },
        xAxis: vertical ? catAxes : valAxis,
        yAxis: vertical ? valAxis : catAxes,
        series: [...series, ...pointSeries]
      }
    };
  };

  Object.assign(parts, { distribution, whiskerSeries });
})();
