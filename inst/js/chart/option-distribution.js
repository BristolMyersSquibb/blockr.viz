// @ts-check
/**
 * Chart block: the boxplot and point range options. The model has already
 * summarised each slot (a group, or group and colour level); this draws a box
 * or a centre dot on an interval per slot. A colour split gives each level
 * its own series, and a split boxplot its own hidden category axis on which
 * it draws only its own slots.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const option = /** @type {any} */ (NS.option = NS.option || {});

  /** The interval of a point range or an error bar as a custom series: a 1.5px line from lo to
   * hi without caps, as the box's whiskers. `encode` maps both ends to the value axis. @param {any} name @param {any[]} pts [slot, lo, hi] each
   * @param {string} clr @param {boolean} horizontal */
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
      const st = api.style ? api.style() : null;
      const opacity = st && st.opacity != null ? st.opacity : 1;
      return {
        type: 'line',
        shape: { x1: pLo[0], y1: pLo[1], x2: pHi[0], y2: pHi[1] },
        style: { stroke: clr, lineWidth: 1.5, opacity }
      };
    }
  });

  /** What every part of a distribution's option reads.
   * @param {any} m @param {any} panel @param {any} look @param {number} plotW
   */
  const distributionContext = (m, panel, look, plotW) => {
    const { cfg } = look;
    const P = option.aggregatedCommon(look);
    const isBox = cfg.type === 'boxplot';
    const palette = NS.paletteOf(look.derived);
    const split = m.split;
    const scale = m.colorScale;
    const hexFor = (/** @type {string} */ lv, /** @type {number} */ i) =>
      (scale && scale.color && scale.color[lv]) || NS.paletteAt(palette, i);
    const slots = panel.slots;
    /** One entry per series level: its name (split only) and colour. */
    const levelInfo = (split ? m.levels : ['__all__']).map(
      (/** @type {string} */ lv, /** @type {number} */ li) =>
        ({ lv, li, name: split ? lv : undefined, hex: split ? hexFor(lv, li) : palette[0] }));
    return {
      m, look, cfg, ink: look.ink, plotW, P, isBox, split, hexFor, slots, levelInfo,
      SEP: NS.model.BOX_CAT_SEP,
      vertical: cfg.orientation !== 'horizontal',
      bodyMeta: NS.agg.statMeta(cfg.summary || (isBox ? 'median_q1_q3' : 'mean_se')),
      whiskMeta: NS.agg.statMeta(cfg.whiskers || 'tukey'),
      levels: m.levels,
      cats: slots.map((/** @type {any} */ s) => s.cat),
      axisOn: NS.countOn(cfg, 'axis')
    };
  };

  /** The slots of a level that drew a body, with their category position. @param {any} k
   * @param {string} lv @returns {Array<{ s: any, ci: number }>} */
  const drawnSlots = (k, lv) => k.slots
    .map((/** @type {any} */ s, /** @type {number} */ ci) => ({ s, ci }))
    .filter((/** @type {any} */ e) => e.s.level === lv && e.s.body);

  // One datum per category, so the datum index is the category index. An empty slot is NaNs.
  const emptyBox = (/** @type {number} */ ci) => ({ value: [ci, NaN, NaN, NaN, NaN, NaN], n: 0 });

  /** The box of one slot: a solid body in the level's colour, the median cut out of it in the
   * surface colour, whiskers without caps. ECharts' own boxplot draws body outline, whiskers and
   * median in one colour, so a solid body would hide the median; this draws the parts apart.
   * The datum is [category, whisker lo, body lo, centre, body hi, whisker hi]. @param {string} clr
   * @param {string} cut @param {boolean} horizontal */
  const boxRender = (clr, cut, horizontal) => (/** @type {any} */ params, /** @type {any} */ api) => {
    const ci = api.value(0);
    const v = [1, 2, 3, 4, 5].map((i) => api.value(i));
    if (v.some((x) => !Number.isFinite(x))) return null;
    const at = (/** @type {number} */ x) => horizontal ? api.coord([x, ci]) : api.coord([ci, x]);
    const band = horizontal ? api.size([0, 1])[1] : api.size([1, 0])[0];
    // The bounds of ECharts' boxWidth default, [7, 50].
    const half = Math.max(7, Math.min(50, band * 0.5)) / 2;
    const st = api.style ? api.style() : null;
    const opacity = st && st.opacity != null ? st.opacity : 1;
    const [wLo, bLo, mid, bHi, wHi] = v.map(at);
    const a = horizontal ? 0 : 1; // the value axis coordinate
    const line = (/** @type {number[]} */ p, /** @type {number[]} */ q, /** @type {string} */ stroke,
                  /** @type {number} */ lineWidth) =>
      ({ type: 'line', shape: { x1: p[0], y1: p[1], x2: q[0], y2: q[1] }, style: { stroke, lineWidth, opacity } });
    const across = (/** @type {number[]} */ p) => horizontal
      ? [[p[0], p[1] - half], [p[0], p[1] + half]] : [[p[0] - half, p[1]], [p[0] + half, p[1]]];
    const lo = Math.min(bLo[a], bHi[a]), len = Math.abs(bHi[a] - bLo[a]);
    const body = horizontal
      ? { x: lo, y: mid[1] - half, width: len, height: 2 * half }
      : { x: mid[0] - half, y: lo, width: 2 * half, height: len };
    const [m0, m1] = across(mid);
    return {
      type: 'group',
      children: [
        line(wLo, bLo, clr, 1.5),
        line(bHi, wHi, clr, 1.5),
        { type: 'rect', shape: { ...body, r: 1.5 }, style: { fill: clr, opacity } },
        line(m0, m1, cut, 1.5)
      ]
    };
  };

  /** One box series per level, a custom series (see boxRender). @param {any} k */
  const distBoxSeries = (k) => k.levelInfo.map((/** @type {any} */ { lv, li, name, hex }) => ({
    type: 'custom', name, id: '__box_' + li,
    data: k.slots.map((/** @type {any} */ s, /** @type {number} */ ci) => (s.level === lv && s.body)
      ? { value: [ci, s.whisker.lo, s.body.lo, s.body.center, s.body.hi, s.whisker.hi], n: s.n,
          name: s.cat }
      : emptyBox(ci)),
    encode: k.vertical ? { x: 0, y: [1, 2, 3, 4, 5] } : { y: 0, x: [1, 2, 3, 4, 5] },
    renderItem: boxRender(hex, k.ink.surface, !k.vertical),
    ...(k.split ? (k.vertical ? { xAxisIndex: li } : { yAxisIndex: li }) : {}),
    itemStyle: { color: hex }
  }));

  /** Per level: a silent interval under a clickable centre dot, and a line through the centres with
   * connect_centers, all under the level's name. @param {any} k */
  const distRangeSeries = (k) => {
    const { cfg, cats, vertical } = k;
    /** @type {any[]} */
    const series = [];
    for (const { lv, name, hex } of k.levelInfo) {
      const drawn = drawnSlots(k, lv);
      const centers = drawn.map(({ s: { body: b, n }, ci }) => ({
        value: vertical ? [ci, b.center] : [b.center, ci], name: cats[ci],
        center: b.center, n, lo: b.lo, hi: b.hi }));
      if (cfg.connect_centers === true && centers.length > 1) {
        series.push({
          type: 'line', name, legendHoverLink: false, silent: true,
          z: 2, data: centers.map((c) => c.value), showSymbol: false,
          lineStyle: { color: hex, width: 1.4 }, itemStyle: { color: hex },
          emphasis: { disabled: true }, tooltip: { show: false }
        });
      }
      if (drawn.length) {
        series.push(whiskerSeries(name, drawn.map(({ s, ci }) => [ci, s.body.lo, s.body.hi]),
                                  hex, !vertical));
      }
      series.push({
        // The dot is ringed in the surface colour, as the box's median is cut out of it, and
        // opaque (ECharts draws a scatter at 0.8) so it is the box's full colour.
        type: 'scatter', name, data: centers, z: 4, symbolSize: 9,
        itemStyle: { color: hex, opacity: 1, borderColor: k.ink.surface, borderWidth: 1.5,
                     cursor: 'pointer' },
        emphasis: { disabled: true }
      });
    }
    return series;
  };

  /** `legend: "marks"` on a point range: each level's name at the centre of its centres.
   * @param {any} k */
  const distCentroidMarks = (k) => {
    const { ink, vertical } = k;
    /** @type {any[]} */
    const marks = [];
    for (const { lv, li } of k.levelInfo) {
      const at = drawnSlots(k, lv).map(({ s, ci }) =>
        vertical ? [ci, s.body.center] : [s.body.center, ci]);
      const c = NS.cloudCentre(at);
      if (!c) continue;
      const mark = NS.nameMark('__legend_' + li, lv, c, vertical ? 'top' : 'right', ink,
                               { distance: 8 });
      mark.labelLayout = { moveOverlap: 'shiftY' };
      marks.push(mark);
    }
    return marks;
  };

  /** Outliers on their box's centre line, silent so the box keeps the click. @param {any} k */
  const distOutlierSeries = (k) => {
    const { cfg, isBox, slots, vertical } = k;
    if (!(isBox && cfg.box_points === 'outliers')) return [];
    return k.levelInfo.map((/** @type {any} */ { lv, name, hex }) => {
      /** @type {[number, number][]} */
      const pts = [];
      slots.forEach((/** @type {any} */ s, /** @type {number} */ ci) => {
        if (s.level !== lv) return;
        for (const v of s.outliers) pts.push(vertical ? [ci, v] : [v, ci]);
      });
      return {
        type: 'scatter', name, data: pts, silent: true,
        symbolSize: 5, z: 3, large: true, largeThreshold: 800,
        itemStyle: { color: hex, opacity: 0.85, borderColor: hex, borderWidth: 1 }
      };
    }).filter((/** @type {any} */ s) => s.data.length > 0);
  };

  /** The slots' category labels, gutter, x-label fit and formatter.
   * @param {any} k
   */
  const distCategories = (k) => {
    const { look, plotW, SEP, split, slots, cats, axisOn, vertical } = k;
    /** @type {Map<string, string> | null} */
    const catLabelMap = axisOn ? new Map() : null;
    if (catLabelMap) {
      for (const s of slots) catLabelMap.set(s.cat, s.group + ' (' + s.count + ')');
    }
    const short = (/** @type {string} */ v) => String(v).split(SEP)[0];
    const catLabels = cats.map((/** @type {string} */ c) => catLabelMap
      ? /** @type {string} */ (catLabelMap.get(c)) : (split ? short(c) : c));
    const catFmt = catLabelMap
      ? (/** @type {string} */ v) => catLabelMap.get(v) || short(v)
      : (split ? short : null);
    const fit = vertical ? option.barMarkParts.fitXLabels(catLabels, plotW, look, catFmt) : null;
    return { catLabels, gut: NS.axes.yGutter(catLabels, look.measure), catFmt,
             xlab: fit && fit.xlab, xFit: fit ? fit.xFit : undefined };
  };

  /** The tooltip formatter; the statistics it names are configurable.
   * @param {any} k
   */
  const distTooltip = (k) => {
    const { cfg, P, SEP, isBox, split, hexFor, slots, levels, bodyMeta, whiskMeta } = k;
    const esc = V.util.esc;
    const ttHead = (/** @type {any} */ p) => split ? String(p.name).replace(SEP, ' · ') : p.name;
    // A split lists every level of the group, the hovered one marked.
    const colorName = split ? esc(P.title(cfg.color) || cfg.color) + ' ' : '';
    const span = (/** @type {any} */ lo, /** @type {any} */ hi) =>
      V.util.num(lo) + ' – ' + V.util.num(hi);
    const splitTooltipFmt = (/** @type {any} */ p) => {
      const at = slots.find((/** @type {any} */ s) => s.cat === p.name);
      if (!at || !at.body) return '';
      const rows = slots
        .filter((/** @type {any} */ s) => s.group === at.group && s.body)
        .map((/** @type {any} */ s) => V.tooltip.row(colorName + esc(s.level),
          V.util.num(s.body.center) + ' (' + span(s.body.lo, s.body.hi) + ')',
          hexFor(s.level, levels.indexOf(s.level)), s === at));
      const foot = colorName + esc(at.level) + ' · n ' + at.n +
        (isBox && at.whisker ? ' · Whiskers (' + esc(whiskMeta.range) + ') ' +
          span(at.whisker.lo, at.whisker.hi) : '');
      return V.tooltip.head(esc(at.group)) +
        V.tooltip.note(esc(bodyMeta.center) + ' (' + esc(bodyMeta.range) + ')') +
        rows.join('') + V.tooltip.SEP + V.tooltip.note(foot);
    };
    const pointTooltipFmt = (/** @type {any} */ p) => {
      const d = p.data;
      // n = 0 is an empty slot.
      if (!d || !d.n || (isBox && !Array.isArray(d.value))) return '';
      const h = ttHead(p);
      const lead = (h ? V.tooltip.head(esc(h), split ? p.color : null) : '') + V.tooltip.row('n', d.n);
      if (!isBox) {
        return lead + V.tooltip.row(esc(bodyMeta.center), V.util.num(d.center)) +
          V.tooltip.row(esc(bodyMeta.range), span(d.lo, d.hi));
      }
      // A box datum is [index, wLo, bLo, center, bHi, wHi].
      const five = d.value.slice(-5);
      return lead + V.tooltip.row(esc(bodyMeta.center), V.util.num(five[2])) +
        V.tooltip.row('Box (' + esc(bodyMeta.range) + ')', V.util.num(five[1]) + ', ' + V.util.num(five[3])) +
        V.tooltip.row('Whiskers (' + esc(whiskMeta.range) + ')', span(five[0], five[4]));
    };
    return split ? splitTooltipFmt : pointTooltipFmt;
  };

  /** The value axis and the category axis (one per level on a split boxplot). @param {any} k
   * @param {any} cat the result of `distCategories` */
  const distAxes = (k, cat) => {
    const { cfg, ink, P, isBox, split, cats, vertical, levelInfo } = k;
    // The value axis fits the data rather than holding zero.
    const valAxis = {
      ...NS.axes.valueAxis(ink, 'value', NS.unitTitle(P.title(cfg.measure), cfg),
                           vertical ? 45 : 30, { format: NS.valueFormat(cfg, { unit: false }),
                                                 grid: vertical ? 'dashed' : undefined }),
      scale: true
    };
    const catAxis = NS.axes.categoryAxis(cats, ink,
      { xlab: cat.xlab, display: cat.catFmt, gut: cat.gut, ticks: true });
    const catAxes = (isBox && split)
      ? levelInfo.map((/** @type {any} */ _, /** @type {number} */ i) =>
          (i === 0 ? catAxis : { ...catAxis, show: false }))
      : catAxis;
    return { valAxis, catAxes };
  };

  /**
   * @param {any} m @param {any} panel @param {any} look @param {number} plotW
   */
  const distribution = (m, panel, look, plotW) => {
    if (m.noValue) return null;
    const k = distributionContext(m, panel, look, plotW);
    const { cfg, ink, P, isBox, split, levels, cats, vertical } = k;
    const series = isBox ? distBoxSeries(k) : distRangeSeries(k);
    if (!isBox && split && NS.legendPlan(m, cfg).marks === 'centroid') {
      series.push(...distCentroidMarks(k));
    }
    const pointSeries = distOutlierSeries(k);
    const cat = distCategories(k);
    const { xlab, gut } = cat;
    const bottomBase = 46 + (xlab ? xlab.bottom : 0);
    const { valAxis, catAxes } = distAxes(k, cat);
    NS.axes.addValueLines(series, valAxis, !vertical, false, cfg.value_lines, ink);
    return {
      xFit: cat.xFit,
      // Horizontal: one 28 px row per drawn slot, capped.
      panelH: vertical ? 350 + (xlab ? xlab.bottom : 0)
        : 30 + Math.min(NS.PANEL_H_CAP, cats.length * 28) + bottomBase,
      labelNote: (!vertical && cats.length * 28 > NS.PANEL_H_CAP)
        ? NS.axes.thinnedLabelNote(NS.PANEL_H_CAP, cats.length, P.title(cfg.category)) : null,
      option: {
        ...P.base,
        tooltip: { trigger: 'item', confine: true, formatter: distTooltip(k) },
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

  option.distributionParts = { distribution, whiskerSeries };
})();
