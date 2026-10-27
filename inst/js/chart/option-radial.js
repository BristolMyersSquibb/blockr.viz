// @ts-check
/**
 * Chart block: the pie and treemap options.
 *
 * Neither has a category axis: a pie slice and a treemap tile label
 * themselves. Each draws on the default 350 px canvas.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});
  const option = /** @type {any} */ (NS.option = NS.option || {});
  const parts = /** @type {any} */ (option.aggParts = option.aggParts || {});

  /**
   * A slice or tile per group: the group's total over its cells and the
   * rows behind it, in the board scale's colour or the palette's, and its
   * `label` text where there is one. Groups with nothing to show drop out.
   * @param {any} m @param {any} panel @param {Record<string, any>} cfg
   */
  const groupTotals = (m, panel, cfg) => {
    const gScale = NS.scaleFor(cfg, cfg.category);
    const palette = NS.paletteOf(cfg);
    return panel.groups.map((/** @type {string} */ g, /** @type {number} */ i) => {
      const cells = NS.model.cellsOf(m, panel, g);
      const total = cells.reduce((/** @type {number} */ s, /** @type {any} */ a) => s + (a.value ?? 0), 0);
      const n = cells.reduce((/** @type {number} */ s, /** @type {any} */ a) => s + (a.n || 0), 0);
      const text = NS.model.labelOf(m, panel, g);
      return { name: g, value: total, n, ...(text != null ? { text } : {}),
               itemStyle: { color: (gScale && gScale.color && gScale.color[g]) ||
                 NS.paletteAt(palette, i) } };
    }).filter((/** @type {any} */ d) => d.value > 0);
  };

  /** @param {any} m @param {any} panel @param {any} look */
  const pie = (m, panel, look) => {
    const { cfg, ink } = look;
    const P = parts.common(look);
    const data = groupTotals(m, panel, cfg);
    return {
      option: {
        ...(look.theme ? {} : { backgroundColor: 'transparent' }),
        textStyle: { fontFamily: ink.face },
        tooltip: { trigger: 'item', confine: true,
                   formatter: (/** @type {any} */ p) => NS.rowTooltip(p.name,
                     P.aggPairs(p.value, p.data && p.data.n, p.percent), p.color) },
        series: [{ type: 'pie', radius: ['30%', '70%'], data,
                   // The slice's `label` text, else its name.
                   label: { show: true, fontSize: ink.fontSize,
                            formatter: data.some((/** @type {any} */ d) => d.text != null)
                              ? (/** @type {any} */ p) =>
                                  (p.data && p.data.text != null ? p.data.text : p.name)
                              : '{b}' },
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
                   formatter: (/** @type {any} */ p) =>
                     (p.data && p.data.text != null ? p.data.text : p.name) + '\n' +
                     NS.ddNum(Number(p.value)) },
          itemStyle: { borderColor: ink.surface, borderWidth: 2, gapWidth: 2 },
          emphasis: { itemStyle: { shadowBlur: 10, shadowColor: 'rgba(0,0,0,0.15)' } }
        }]
      }
    };
  };

  Object.assign(parts, { pie, treemap });
})();
