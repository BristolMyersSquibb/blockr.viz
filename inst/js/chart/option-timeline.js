// @ts-check
/**
 * Chart block: the timeline (gantt) option, one ECharts option and canvas
 * height per panel. Nothing here touches the DOM; text is measured with the
 * function the caller passes.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});
  const option = /** @type {any} */ (NS.option = NS.option || {});

  // Config fields the option reads beside the model's.
  const OPTION_FIELDS = ['label', 'drill', 'derived.palette', 'derived.scales'];

  // Lane height and the room around the plot, in CSS px.
  const LANE_PX = 28;
  const PANEL_EXTRA_PX = 80;
  const PANEL_MIN_PX = 200;
  // The opacity of a bar a filter does not light.
  const DIM = 0.15;

  /**
   * @typedef {{ cfg: Record<string, any>, columns: VizColumn[],
   *             ink: typeof NS.INK_DEFAULT, theme: any,
   *             measure: (s: string) => number }} Look
   */

  /**
   * Which marks each series of a panel draws, as mark positions in series
   * order: one series per colour level (marks of no level are not drawn),
   * else one series of every mark. The option and the patch share it.
   * @param {any} m @param {any} panel @returns {number[][]}
   */
  const seriesMarks = (m, panel) => {
    if (!m.color) return [panel.marks.map((/** @type {any} */ _, /** @type {number} */ i) => i)];
    /** @type {Map<string, number[]>} */
    const buckets = new Map(m.color.levels.map((/** @type {any} */ lvl) =>
      /** @type {[string, number[]]} */ ([lvl, []])));
    panel.marks.forEach((/** @type {any[]} */ t, /** @type {number} */ i) => {
      const b = buckets.get(String(t[4] ?? ''));
      if (b) b.push(i);
    });
    return m.color.levels.map((/** @type {any} */ lvl) => buckets.get(lvl) || []);
  };

  /** What every part of the timeline option reads. @param {any} m @param {Look} look */
  const timelineContext = (m, look) => {
    const { cfg, columns, ink, theme } = look;
    const title = (/** @type {any} */ col) => NS.axes.axisTitle(columns, col);
    const { type: xAxisType, cats: xCats, col: x } = m.x;
    const palette = NS.paletteOf(cfg);
    const colorLevels = m.color ? m.color.levels : [];
    const showLegend = colorLevels.length > 0;
    const colors = showLegend ? NS.levelColors(colorLevels, m.color.scale, palette) : null;
    return { m, look, cfg, ink, theme, title, xAxisType, xCats, x, y: m.y.col, palette,
             colorLevels, showLegend, colors, onBarLabel: !!cfg.label };
  };

  /** A bar from start to end on its lane, at least 4px wide, clipped to the plot,
   * with the label column's value on it when it has room. @param {any} k */
  const timelineRenderItem = (k) => {
    const { ink, onBarLabel } = k;
    return (/** @type {any} */ params, /** @type {any} */ api) => {
      const start = api.coord([api.value(0), api.value(2)]);
      const end = api.coord([api.value(1), api.value(2)]);
      const h = api.size([0, 1])[1] * 0.6;
      const barW = Math.max(end[0] - start[0], 4);
      const rect = echarts.graphic.clipRectByRect(
        { x: start[0], y: start[1] - h / 2, width: barW, height: h },
        { x: params.coordSys.x, y: params.coordSys.y,
          width: params.coordSys.width, height: params.coordSys.height }
      );
      if (!rect) return;
      const barLabel = onBarLabel ? String(api.value(5) ?? '') : '';
      // The datum's opacity rides in the style: a filter dims the bars it does not light.
      const st = api.style();
      /** @type {any[]} */
      const children = [{ type: 'rect', shape: Object.assign({}, rect, { r: 3 }), style: st }];
      if (barW > 50 && barLabel) {
        children.push({
          type: 'text',
          style: {
            text: barLabel, x: rect.x + 6, y: rect.y + rect.height / 2, fill: '#fff',
            fontSize: ink.fontSize, fontWeight: 500, fontFamily: ink.face,
            textVerticalAlign: 'middle', truncate: { outerWidth: barW - 12 },
            opacity: st && st.opacity != null ? st.opacity : 1
          }
        });
      }
      return { type: 'group', children };
    };
  };

  /** The hovered bar: label, else series, else lane as headline, then the lane,
   * the interval and its length, the colour and the tooltip fields. @param {any} k */
  const timelineTooltip = (k) => {
    const { m, title, xAxisType, xCats, x, y } = k;
    return (/** @type {any} */ p) => {
      const v = p.value;
      const term = String(v[3] ?? '');
      const colorVal = String(v[4] ?? '');
      const labelVal = m.label != null ? String(v[5] ?? '') : '';
      const detail = String(v[6] ?? '');
      const headline = labelVal || detail || term;
      /** @type {Array<[string, any]>} */
      const pairs = [];
      if (term && term !== headline) pairs.push([title(y) || 'Y', term]);
      if (detail && detail !== headline) pairs.push([title(m.series) || 'Series', detail]);
      const fromTxt = NS.axes.fmtXVal(v[0], xAxisType, xCats);
      if (v[1] != null && v[1] !== v[0]) {
        const toTxt = NS.axes.fmtXVal(v[1], xAxisType, xCats);
        const dur = NS.axes.durationStr(v[0], v[1], xAxisType);
        pairs.push([title(x) || 'From', fromTxt]);
        pairs.push([(m.xend ? title(m.xend) : '') || 'To', toTxt + (dur ? ' (' + dur + ')' : '')]);
      } else {
        pairs.push([title(x) || 'Date', fromTxt]);
      }
      if (colorVal) pairs.push([title(m.color ? m.color.col : null) || 'Color', colorVal]);
      const extra = Array.isArray(v[8]) ? v[8] : [];
      m.ttFields.forEach((/** @type {string} */ c, /** @type {number} */ i) => {
        pairs.push([title(c) || c, extra[i]]);
      });
      return NS.rowTooltip(headline, pairs);
    };
  };

  /** One panel: its bar series, lanes axis and canvas height.
   * @param {any} k @param {any} panel @param {any} renderItem @param {any} formatter
   * @param {any} xAxis */
  const timelinePanel = (k, panel, renderItem, formatter, xAxis) => {
    const { m, look, cfg, ink, theme, palette, colorLevels, showLegend, colors } = k;
    const layout = seriesMarks(m, panel);
    const data = (/** @type {number[]} */ idx) =>
      idx.map((i) => ({ value: panel.marks[i] }));
    const series = showLegend
      ? colorLevels.map((/** @type {any} */ lvl, /** @type {number} */ si) => ({
          type: 'custom', name: lvl, data: data(layout[si]),
          encode: { x: [0, 1], y: 2 }, renderItem
        }))
      : [{ type: 'custom', data: data(layout[0]), encode: { x: [0, 1], y: 2 }, renderItem }];

    const counts = panel.laneCounts;
    const gut = NS.axes.yGutter(counts
      ? panel.lanes.map((/** @type {string} */ t) => NS.withCount(t, counts)) : panel.lanes,
      look.measure);
    const display = counts ? (/** @type {any} */ v) => NS.withCount(v, counts) : null;
    const opt = {
      ...(theme ? {} : { backgroundColor: 'transparent' }),
      color: (m.color && m.color.scale && m.color.scale.color && colors) ? colors : palette,
      textStyle: { fontFamily: ink.face },
      tooltip: { trigger: 'item', confine: true, formatter },
      legend: showLegend ? { show: false, data: colorLevels } : { show: false },
      grid: { left: gut.gridLeft, right: 10, top: 20, bottom: 48 },
      xAxis,
      yAxis: { ...NS.axes.categoryAxis(panel.lanes, ink, { gut, display }),
               splitLine: { show: false } },
      series
    };
    const height = Math.max(PANEL_MIN_PX,
      Math.min(NS.PANEL_H_CAP, panel.lanes.length * LANE_PX) + PANEL_EXTRA_PX) + 'px';
    return { option: NS.cardTooltip(NS.applyDrillEmphasis(opt, cfg), ink), height };
  };

  /**
   * @param {any} m the timeline model
   * @param {Look} look
   * @returns {{ panels: Array<{ option: any, height: string }> }}
   */
  const timeline = (m, look) => {
    const k = timelineContext(m, look);
    const renderItem = timelineRenderItem(k);
    const formatter = timelineTooltip(k);
    // The time axis, shared by every panel.
    const xAxis = {
      ...NS.axes.valueAxis(k.ink, k.xAxisType, k.title(k.x), 28, { grid: 'dashed' }),
      scale: true,
      ...(k.xCats ? { data: k.xCats } : {})
    };
    const panels = m.panels.map((/** @type {any} */ panel) =>
      timelinePanel(k, panel, renderItem, formatter, xAxis));
    return { panels };
  };
  timeline.FIELDS = OPTION_FIELDS;

  /**
   * The legend chips: the colour levels in the option's colours.
   * @param {any} m @param {Record<string, any>} cfg
   * @returns {{ col: string, items: Array<{ name: string, color: string }> } | null}
   */
  const timelineLegend = (m, cfg) => {
    if (!m.color) return null;
    const colors = NS.levelColors(m.color.levels, m.color.scale, NS.paletteOf(cfg));
    return { col: m.color.col,
             items: m.color.levels.map((/** @type {any} */ lvl, /** @type {number} */ i) =>
               ({ name: String(lvl), color: colors[i] })) };
  };

  /**
   * The highlight patch for one panel: every bar's opacity from whether its
   * mark (lane and facet) is lit. Without a filter, and nothing dimmed from
   * before, the patch is empty.
   * @param {any} m @param {any} panel @param {Record<string, any>} cfg
   * @param {((keys: Record<string, any>, rows: () => any[]) => boolean) | null} lit
   * @param {boolean} wasDimmed
   * @returns {{ series: any[], dimmed: boolean }}
   */
  const timelinePatch = (m, panel, cfg, lit, wasDimmed) => {
    const layout = seriesMarks(m, panel);
    if (!lit && !wasDimmed) return { series: layout.map(() => ({})), dimmed: false };
    const series = layout.map((idx) => ({
      data: idx.map((i) => {
        const t = panel.marks[i];
        const on = !lit || lit(NS.model.timelineKeys(cfg, panel.facet, t), () => [panel.rows[i]]);
        return { value: t, itemStyle: { opacity: on ? 1 : DIM } };
      })
    }));
    return { series, dimmed: !!lit };
  };

  Object.assign(option, { timeline, timelineLegend, timelinePatch, timelineSeriesMarks: seriesMarks });
})();
