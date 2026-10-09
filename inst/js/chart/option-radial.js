// @ts-check
/**
 * Chart block: the pie, treemap and radar options.
 *
 * None of them has a category axis: a pie slice and a treemap tile label
 * themselves, and a radar puts the groups on its spokes and draws one
 * shape per colour level. Each draws on the default 350 px canvas.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});
  const option = /** @type {any} */ (NS.option = NS.option || {});
  const parts = /** @type {any} */ (option.aggParts = option.aggParts || {});

  /**
   * A slice or tile per group: the group's total over its cells and the
   * rows behind it, in the board scale's colour or the palette's. Groups
   * with nothing to show drop out.
   * @param {any} m @param {any} panel @param {Record<string, any>} cfg
   */
  const groupTotals = (m, panel, cfg) => {
    const gScale = NS.scaleFor(cfg, cfg.group);
    const palette = NS.paletteOf(cfg);
    return panel.groups.map((/** @type {string} */ g, /** @type {number} */ i) => {
      const cells = NS.model.cellsOf(m, panel, g);
      const total = cells.reduce((/** @type {number} */ s, /** @type {any} */ a) => s + (a.value ?? 0), 0);
      const n = cells.reduce((/** @type {number} */ s, /** @type {any} */ a) => s + (a.n || 0), 0);
      return { name: g, value: total, n,
               itemStyle: { color: (gScale && gScale.color && gScale.color[g]) ||
                 NS.paletteAt(palette, i) } };
    }).filter((/** @type {any} */ d) => d.value > 0);
  };

  /** @param {any} m @param {any} panel @param {any} look */
  const pie = (m, panel, look) => {
    const { cfg, ink } = look;
    const P = parts.common(look);
    return {
      option: {
        ...(look.theme ? {} : { backgroundColor: 'transparent' }),
        textStyle: { fontFamily: ink.face },
        tooltip: { trigger: 'item', confine: true,
                   formatter: (/** @type {any} */ p) => NS.rowTooltip(p.name,
                     P.aggPairs(p.value, p.data && p.data.n, p.percent), p.color) },
        series: [{ type: 'pie', radius: ['30%', '70%'], data: groupTotals(m, panel, cfg),
                   label: { show: true, fontSize: ink.fontSize, formatter: '{b}' },
                   emphasis: { itemStyle: { shadowBlur: 10, shadowColor: 'rgba(0,0,0,0.2)' } } }]
      }
    };
  };

  /** @param {any} m @param {any} panel @param {any} look */
  const treemap = (m, panel, look) => {
    const { cfg, ink } = look;
    const P = parts.common(look);
    return {
      option: {
        ...(look.theme ? {} : { backgroundColor: 'transparent' }),
        textStyle: { fontFamily: ink.face },
        tooltip: { trigger: 'item', confine: true,
                   formatter: (/** @type {any} */ p) => NS.rowTooltip(p.name,
                     P.aggPairs(p.value, p.data && p.data.n), p.color) },
        series: [{
          type: 'treemap', data: groupTotals(m, panel, cfg), left: 10, right: 10, top: 2,
          bottom: 2, roam: false, nodeClick: false, breadcrumb: { show: false },
          label: { show: true, fontSize: ink.fontSize,
                   formatter: (/** @type {any} */ p) => p.name + '\n' + NS.ddNum(Number(p.value)) },
          itemStyle: { borderColor: ink.surface, borderWidth: 2, gapWidth: 2 },
          emphasis: { itemStyle: { shadowBlur: 10, shadowColor: 'rgba(0,0,0,0.15)' } }
        }]
      }
    };
  };

  /**
   * A radar: the groups are the spokes, each colour level one shape, each
   * vertex the (group, colour) cell's value. The spokes share one max (the
   * grid's under fixed panel scales), so shapes compare.
   * @param {any} m @param {any} panel @param {any} look
   */
  const radar = (m, panel, look) => {
    const { cfg, ink } = look;
    const P = parts.common(look);
    const groups = panel.groups;
    const colors = m.colors;
    const palette = NS.paletteOf(cfg);
    const colorScale = m.colorScale;
    const maxVal = m.sharedMax ||
      (Math.max(NS.maxOf(panel.cells.map((/** @type {any} */ a) => a.value ?? 0)), 0) || 1);
    const indicator = groups.map((/** @type {string} */ g) => ({ name: g, max: maxVal }));
    // A missing cell is a true zero for the counting aggregations; for the
    // others there is no value, and null leaves a gap.
    const gapVal = ['count', 'count_distinct', 'sum'].includes(cfg.func) ? 0 : null;
    /** @param {string} g @param {string | null} c */
    const cellVal = (g, c) => {
      const d = NS.model.cellOf(m, panel, g, c);
      return d ? d.value : gapVal;
    };
    /** @param {any} name @param {any[]} vals @param {string} col */
    const mkShape = (name, vals, col) => ({
      name, value: vals,
      itemStyle: { color: col },
      lineStyle: { color: col, width: 2 },
      areaStyle: { color: col, opacity: 0.15 }
    });
    const data = colors.length === 0
      ? [mkShape(P.valueTitle, groups.map((/** @type {string} */ g) => cellVal(g, null)), palette[0])]
      : colors.map((/** @type {string} */ c, /** @type {number} */ ci) => mkShape(
          c, groups.map((/** @type {string} */ g) => cellVal(g, c)),
          (colorScale && colorScale.color && colorScale.color[c]) || NS.paletteAt(palette, ci)));
    const legendOn = colors.length > 0;
    return {
      option: {
        ...(look.theme ? {} : { backgroundColor: 'transparent' }),
        textStyle: { fontFamily: ink.face },
        tooltip: {
          trigger: 'item', confine: true,
          formatter: (/** @type {any} */ p) =>
            NS.tipHead((legendOn ? P.esc(P.title(cfg.color) || cfg.color) + ' ' : '') +
              P.esc(p.name), legendOn ? p.color : null) +
            NS.tipNote(P.esc(P.aggLabel)) +
            groups.map((/** @type {string} */ g, /** @type {number} */ i) => {
              const v = p.value ? p.value[i] : null;
              return NS.tipRow(P.esc(g), v == null ? '–' : NS.ddNum(v));
            }).join('')
        },
        legend: legendOn ? { show: false, data: colors } : { show: false },
        radar: {
          indicator,
          radius: '62%',
          center: ['50%', '50%'],
          axisName: { color: ink.muted, fontSize: ink.fontSize, overflow: 'truncate', width: 90 },
          axisLine: { lineStyle: { color: ink.strong } },
          splitLine: { lineStyle: { color: ink.border } },
          splitArea: { show: false }
        },
        series: [{ type: 'radar', data, symbol: 'circle', symbolSize: 4,
                   emphasis: { focus: 'self' } }]
      }
    };
  };

  Object.assign(parts, { pie, treemap, radar });
})();
