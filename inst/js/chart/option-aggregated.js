// @ts-check
/**
 * Chart block: the aggregated option. One ECharts option per panel, built by
 * the chart type's builder, with its canvas height, the `xFit` inputs a
 * resize re-fits and a thinned-label note. Also the legend chips and the
 * highlight patch, which dims the marks a filter does not light.
 *
 * Each builder file exports under its own name: option.barParts (bar,
 * waterfall), option.radialParts (pie, treemap), option.distributionParts
 * (box plot, point range), with option.barMarkParts beside them. Here the
 * part they all share, option.aggregatedCommon(), and builderOf(), which
 * names the builder of each type.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const option = /** @type {any} */ (NS.option = NS.option || {});
  const { withItemStyle } = option.barMarkParts;

  // The canvas of a builder that sizes nothing itself.
  const DEFAULT_PANEL_H = 350;

  /** What every builder shares: the option's base, the value's title, the aggregation's name and
   * the tooltip's rows.
   * @param {ChartLook} look
   */
  option.aggregatedCommon = (look) => {
    const { cfg, columns, ink } = look;
    const title = (/** @type {any} */ col) => NS.axes.axisTitle(columns, col);
    const fn = cfg.func || 'count';
    const col = title(cfg.measure);
    const aggLabel = fn === 'count' ? 'Count'
      : fn === 'identity' ? (col || 'Value')
      : fn === 'count_distinct' ? 'Distinct ' + col
      : (NS.agg.AGG_WORDS[fn] || fn) + ' of ' + col;
    return {
      base: { ...(look.theme ? {} : { backgroundColor: 'transparent' }),
              textStyle: { fontFamily: ink.face } },
      title,
      valueTitle: cfg.measure === '.count' ? 'Count' : col,
      aggLabel,
      // The aggregation and its value, a share, and n where n is not the value.
      aggPairs: (/** @type {number} */ value, /** @type {number} */ n,
                 /** @type {number} */ percent) => {
        /** @type {Array<[string, any]>} */
        const pairs = [[aggLabel, V.util.num(value)]];
        if (percent != null) pairs.push(['Share', Math.round(percent) + '%']);
        if (cfg.func !== 'count' && cfg.func !== 'identity' && n != null) pairs.push(['n', n]);
        return pairs;
      }
    };
  };

  /** The builder for a chart type. @param {any} m */
  const builderOf = (m) => {
    const ct = m.type;
    if (ct === 'pie') return option.radialParts.pie;
    if (NS.DISTRIBUTION_TYPES.includes(ct)) return option.distributionParts.distribution;
    if (ct === 'treemap') return option.radialParts.treemap;
    return m.cumulative ? option.barParts.waterfall : option.barParts.bar;
  };

  /**
   * @param {any} m the aggregated model
   * @param {ChartLook} look
   * @returns {{ panels: Array<{ option: any, height: string, panelH: number, xFit: any }>,
   *             labelNote: string | null }}
   */
  const aggregated = (m, look) => {
    const build = builderOf(m);
    /** @type {string | null} */
    let labelNote = null;
    const panels = m.panels.map((/** @type {any} */ panel, /** @type {number} */ i) => {
      const r = build(m, panel, look, (look.widths || [])[i] || 0);
      if (!r) {
        return { option: null, height: DEFAULT_PANEL_H + 'px', panelH: DEFAULT_PANEL_H, xFit: null };
      }
      if (r.labelNote) labelNote = r.labelNote;
      const panelH = r.panelH != null ? r.panelH : DEFAULT_PANEL_H;
      return {
        option: NS.cardTooltip(NS.quietHover(r.option, look.cfg), look.ink),
        // 64 only catches a degenerate option; a one-row panel is 104.
        height: Math.max(64, Math.round(panelH)) + 'px',
        panelH,
        xFit: r.xFit || null
      };
    });
    return { panels, labelNote };
  };

  /**
   * The legend chips: a distribution's split levels, else a bar's colour
   * levels. Pie, treemap and waterfall have none.
   * @param {any} m @param {ChartSettings} cfg @param {ChartDerived} derived
   * @returns {{ col: string, items: Array<{ name: string, color: string }> } | null}
   */
  const aggregatedLegend = (m, cfg, derived) => {
    const ct = m.type;
    const levels = m.distribution ? m.levels
      : (ct !== 'pie' && ct !== 'treemap' && !m.cumulative ? m.colors : []);
    if (!levels.length) return null;
    const palette = NS.paletteOf(derived);
    // A highlight lists only the levels it lights.
    const colors = (m.hlColors && !m.distribution)
      ? NS.litLevelColors(levels, m.hlColors, m.colorScale, palette)
      : NS.levelColors(levels, m.colorScale, palette);
    const items = levels.map((/** @type {string} */ name, /** @type {number} */ i) =>
      ({ name, color: colors[i] })).filter((/** @type {any} */ it) => it.color);
    // Levels come from the colour column, so it is set.
    return items.length ? { col: /** @type {string} */ (cfg.color), items } : null;
  };

  // Series patched only while a filter dims something (or to undo it).
  const QUIET_TYPES = ['line', 'scatter', 'custom'];

  /**
   * The highlight patch for one panel: every datum outlined or faded by
   * whether its mark is lit (`lit` null: no filter; NS.selStyle()). Built off
   * the current option so what each datum carries (corner, colour, raw
   * value, label) rides along.
   * @param {any} m @param {any} panel @param {Record<string, any>} cfg
   * @param {any} current chart.getOption()
   * @param {((keys: Record<string, any>) => boolean) | null} lit
   * @param {boolean} wasDimmed the panel's quiet series carry dimming from before
   * @param {Ink} [ink]
   * @returns {{ series: any[], dimmed: boolean }}
   */
  const aggregatedPatch = (m, panel, cfg, current, lit, wasDimmed, ink = NS.INK_DEFAULT) => {
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
    /** @param {boolean} lit1 @param {any} s */
    const look = (lit1, s) => NS.selStyle(lit1, anyFilter, ink, s.itemStyle);

    const series = (current.series || []).map((/** @type {any} */ s) => {
      // A name written on the marks is no mark of the data.
      if (typeof s.id === 'string' &&
          (s.id.indexOf('__legend_') === 0 || s.id === '__label__')) return {};
      // The waterfall's invisible base: an outline would show it.
      if (s.silent && s.type === 'bar') return {};
      if (s.type === 'pie' || s.type === 'treemap') {
        return { data: (s.data || []).map((/** @type {any} */ d) => d == null ? d
          : withItemStyle(d, look(on(d.name, null), s))) };
      }
      // A box series is custom, one datum per category (option-distribution.js).
      if (typeof s.id === 'string' && s.id.indexOf('__box_') === 0) {
        if (cats.length === 0) return {};
        return { data: (s.data || []).map((/** @type {any} */ v, /** @type {number} */ i) => {
          if (!v || typeof v !== 'object') return v;
          const [g, lv] = slotMark(cats[i]);
          return withItemStyle(v, look(on(g, lv), s));
        }) };
      }
      if (QUIET_TYPES.includes(s.type)) {
        // A point range's dots and intervals, a box's outliers.
        if (!m.distribution || s.type === 'line' || (!anyFilter && !wasDimmed)) return {};
        return { data: (s.data || []).map((/** @type {any} */ d) => {
          if (d == null) return d;
          const isObj = typeof d === 'object' && !Array.isArray(d);
          const val = isObj ? d.value : d;
          let cat = isObj && d.name != null ? d.name : null;
          if (cat == null && Array.isArray(val)) {
            const idx = s.type === 'custom' ? val[0]
              : (cfg.orientation !== 'horizontal' ? val[0] : val[1]);
            cat = cats[idx];
          }
          const [g, lv] = slotMark(cat);
          return withItemStyle(d, look(on(g, lv), s));
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
                       ...look(on(String(cats[i] ?? ''), lv), s) }
        };
      }) };
    });
    return { series, dimmed: anyFilter };
  };

  Object.assign(option, { aggregated, aggregatedLegend, aggregatedPatch });
})();
