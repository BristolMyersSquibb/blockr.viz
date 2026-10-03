// @ts-check
/**
 * Chart block: the aggregated option.
 *
 * The aggregated model and how it should look in, one ECharts option per
 * panel out, built by the chart type's builder (option-bar.js,
 * option-radial.js, option-distribution.js). Beside it: the legend band's
 * chips, and the highlight patch, which dims the marks a filter does not
 * light (D5).
 *
 * Every panel result carries its canvas height, the inputs a resize needs
 * to re-fit turned x labels (`xFit`), and a footer note when a horizontal
 * axis had to thin its labels.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});
  const option = /** @type {any} */ (NS.option = NS.option || {});
  const parts = /** @type {any} */ (option.aggParts = option.aggParts || {});

  // Config fields the option reads beside the model's.
  const OPTION_FIELDS = ['chart_type', 'bar_mode', 'orientation', 'value_labels', 'value_lines',
    'pct_of', 'waterfall_totals', 'connect_centers', 'drill', 'palette', 'scales', 'value',
    'func', 'count_on', 'summary', 'whiskers', 'box_points', 'group', 'color', 'facet'];

  // The canvas of a builder that sizes nothing itself.
  const DEFAULT_PANEL_H = 350;

  /**
   * What every aggregated builder shares: the axis ink, the value's title,
   * the aggregation's name, and the tooltip's label/value rows.
   * @param {{ cfg: Record<string, any>, columns: VizColumn[], ink: any }} look
   */
  parts.common = (look) => {
    const { cfg, columns, ink } = look;
    const title = (/** @type {any} */ col) => NS.axes.axisTitle(columns, col);
    const fn = cfg.func || 'count';
    const col = title(cfg.value);
    const aggLabel = fn === 'count' ? 'Count'
      // "None (as is)": the bar is the value, named by its column alone.
      : fn === 'identity' ? (col || 'Value')
      : fn === 'count_distinct' ? 'Distinct ' + col
      : ((NS.roles.DAgg.AGG_WORDS || {})[fn] || fn) + ' of ' + col;
    return {
      ax: { labelColor: ink.muted, fontSize: ink.fontSize, splitLineColor: ink.border },
      esc: NS.esc,
      title,
      valueTitle: cfg.value === '.count' ? 'Count' : col,
      aggLabel,
      // The aggregation and its value, a share, and n where n is not
      // already the value.
      aggPairs: (/** @type {number} */ value, /** @type {number} */ n,
                 /** @type {number} */ percent) => {
        /** @type {Array<[string, any]>} */
        const pairs = [[aggLabel, NS.ddNum(value)]];
        if (percent != null) pairs.push(['Share', Math.round(percent) + '%']);
        if (cfg.func !== 'count' && cfg.func !== 'identity' && n != null) pairs.push(['n', n]);
        return pairs;
      }
    };
  };

  /** The builder for a chart type. @param {any} m */
  const builderOf = (m) => {
    const ct = m.type;
    if (ct === 'pie') return parts.pie;
    if (NS.DISTRIBUTION_TYPES.includes(ct)) return parts.distribution;
    if (ct === 'treemap') return parts.treemap;
    if (ct === 'radar') return parts.radar;
    return m.cumulative ? parts.waterfall : parts.bar;
  };

  /**
   * @param {any} m the aggregated model
   * @param {{ cfg: Record<string, any>, columns: VizColumn[], ink: any, theme: any,
   *           measure: (s: string) => number, widths?: number[] }} look
   * @returns {{ panels: Array<{ option: any, height: string, panelH: number, xFit: any }>,
   *             labelNote: string | null }}
   */
  const aggregated = (m, look) => {
    const build = builderOf(m);
    /** @type {string | null} */
    let labelNote = null;
    const panels = m.panels.map((/** @type {any} */ panel, /** @type {number} */ i) => {
      const r = build(m, panel, look, (look.widths || [])[i] || 0);
      if (!r) return { option: null, height: DEFAULT_PANEL_H + 'px', panelH: DEFAULT_PANEL_H, xFit: null };
      if (r.labelNote) labelNote = r.labelNote;
      const panelH = r.panelH != null ? r.panelH : DEFAULT_PANEL_H;
      return {
        option: NS.cardTooltip(NS.applyDrillEmphasis(r.option, look.cfg), look.ink),
        // A one-row panel is 104 px and that is right; 64 only catches a
        // degenerate option.
        height: (r.panelH != null ? Math.max(64, Math.round(r.panelH)) : DEFAULT_PANEL_H) + 'px',
        panelH,
        xFit: r.xFit || null
      };
    });
    return { panels, labelNote };
  };
  aggregated.FIELDS = OPTION_FIELDS;

  /**
   * The legend band's chips: a distribution's split levels, else the
   * colour levels of a bar or radar. Pie, treemap and waterfall have none.
   * @param {any} m @param {Record<string, any>} cfg
   * @returns {{ col: string, items: Array<{ name: string, color: string }> } | null}
   */
  const aggregatedLegend = (m, cfg) => {
    const ct = m.type;
    const palette = NS.paletteOf(cfg);
    let levels = [];
    if (m.distribution) levels = m.levels;
    else if (ct !== 'pie' && ct !== 'treemap' && !m.cumulative) levels = m.colors;
    if (!levels.length) return null;
    const colors = NS.levelColors(levels, m.colorScale, palette);
    return { col: cfg.color,
             items: levels.map((/** @type {string} */ name, /** @type {number} */ i) =>
               ({ name, color: colors[i] })) };
  };

  // -- Highlight (D5) ----------------------------------------------------------

  // Series patched only while a filter dims something (or to undo it), so
  // a chart without one gets an empty patch.
  const QUIET_TYPES = ['line', 'scatter', 'custom'];

  /**
   * The highlight patch for one panel: every datum's opacity from whether
   * its mark is lit. `lit(keys)` says whether a mark is lit; null when no
   * filter is active, and every mark is. Read off the option the panel holds now, so what each datum
   * carries (a stack's corner, a waterfall's colour, a percent's raw value,
   * a label) rides along.
   * @param {any} m @param {any} panel @param {Record<string, any>} cfg
   * @param {any} current chart.getOption()
   * @param {((keys: Record<string, any>) => boolean) | null} lit
   * @param {boolean} wasDimmed the panel's quiet series carry dimming from before
   * @returns {{ series: any[], dimmed: boolean }}
   */
  const aggregatedPatch = (m, panel, cfg, current, lit, wasDimmed) => {
    const SEP = NS.model.BOX_CAT_SEP;
    const fv = panel.facet;
    // getOption() hands axes back as arrays; the first is the category axis.
    const first = (/** @type {any} */ a) => (Array.isArray(a) ? a[0] : a);
    const cats = (first(current.xAxis) && first(current.xAxis).data) ||
      (first(current.yAxis) && first(current.yAxis).data) || [];
    /** @param {string | null} g @param {string | null} lv */
    const on = (g, lv) => !lit || lit(NS.model.aggregatedKeys(cfg, fv, g, lv));
    /** @param {any} cat */
    const slotMark = (cat) => {
      const s = String(cat ?? '');
      if (!m.split) return [s, null];
      const [g, lv] = s.split(SEP);
      return [g, lv == null ? null : lv];
    };
    const barSplit = m.type === 'bar' && !m.cumulative && m.colors.length > 0;
    const anyFilter = !!lit;

    const series = (current.series || []).map((/** @type {any} */ s) => {
      if (s.type === 'pie' || s.type === 'treemap') {
        const dim = s.type === 'pie' ? 0.2 : 0.3;
        return { data: (s.data || []).map((/** @type {any} */ d) => d == null ? d
          : ({ ...d, itemStyle: { ...d.itemStyle, opacity: on(d.name, null) ? 1 : dim } })) };
      }
      if (s.type === 'radar') {
        return { data: (s.data || []).map((/** @type {any} */ d) => {
          if (d == null) return d;
          const lv = cfg.color ? d.name : null;
          const lt = on(null, lv);
          return { ...d,
                   lineStyle: { ...d.lineStyle, opacity: lt ? 1 : 0.15 },
                   itemStyle: { ...d.itemStyle, opacity: lt ? 1 : 0.15 },
                   areaStyle: { ...d.areaStyle, opacity: lt ? 0.15 : 0.04 } };
        }) };
      }
      if (s.type === 'boxplot') {
        if (cats.length === 0) return {};
        return { data: (s.data || []).map((/** @type {any} */ v, /** @type {number} */ i) => {
          if (!v || typeof v !== 'object') return v;
          const [g, lv] = slotMark(cats[i]);
          return { ...v, itemStyle: { ...v.itemStyle, opacity: on(g, lv) ? 1 : 0.15 } };
        }) };
      }
      if (QUIET_TYPES.includes(s.type)) {
        // A point range's dots and intervals, a box's outliers. The
        // connecting line and series outside this family stay as they are.
        if (!m.distribution || s.type === 'line' || (!anyFilter && !wasDimmed)) return {};
        return { data: (s.data || []).map((/** @type {any} */ d) => {
          if (d == null) return d;
          const isObj = typeof d === 'object' && !Array.isArray(d);
          const val = isObj ? d.value : d;
          // Which slot the datum sits in: its name, else its slot index.
          let cat = isObj && d.name != null ? d.name : null;
          if (cat == null && Array.isArray(val)) {
            const idx = s.type === 'custom' ? val[0]
              : (cfg.orientation !== 'horizontal' ? val[0] : val[1]);
            cat = cats[idx];
          }
          const [g, lv] = slotMark(cat);
          const op = on(g, lv) ? 1 : 0.15;
          return isObj ? { ...d, itemStyle: { ...d.itemStyle, opacity: op } }
            : { value: d, itemStyle: { opacity: op } };
        }) };
      }
      if (cats.length === 0) return {};
      const lv = barSplit ? s.name : null;
      return { data: (s.data || []).map((/** @type {any} */ v, /** @type {number} */ i) => {
        const isObj = (v && typeof v === 'object' && !Array.isArray(v));
        return {
          value: isObj ? v.value : v,
          ...(isObj && v.raw != null ? { raw: v.raw } : {}),
          ...(isObj && v.label ? { label: v.label } : {}),
          itemStyle: { ...(isObj ? v.itemStyle : undefined),
                       opacity: on(String(cats[i] ?? ''), lv) ? 1 : 0.15 }
        };
      }) };
    });
    return { series, dimmed: anyFilter };
  };

  Object.assign(option, { aggregated, aggregatedLegend, aggregatedPatch });
})();
