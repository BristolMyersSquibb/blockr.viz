// @ts-check
/**
 * Chart block: the pie and treemap options. Neither has a category axis;
 * each draws on the default 350 px canvas.
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
   * A slice or tile per group: its total, the rows behind it, its colour and
   * `label` text. Groups with nothing to show drop out.
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

  /** The text a mark shows: its `label` column's value, else its name. @param {any} p */
  const markText = (p) => (p.data && p.data.text != null ? p.data.text : p.name);

  /** The builder of a pie (`pie` true) or a treemap. @param {boolean} pie */
  const builder = (pie) => (/** @type {any} */ m, /** @type {any} */ panel,
    /** @type {any} */ look) => {
    const { cfg, ink } = look;
    const P = parts.common(look);
    const data = groupTotals(m, panel, cfg);
    const label = { show: true, fontSize: ink.fontSize };
    const shadow = (/** @type {number} */ a) =>
      ({ itemStyle: { shadowBlur: 10, shadowColor: 'rgba(0,0,0,' + a + ')' } });
    return {
      option: {
        ...P.base,
        tooltip: { trigger: 'item', confine: true,
                   formatter: (/** @type {any} */ p) => NS.rowTooltip(p.name,
                     P.aggPairs(p.value, p.data && p.data.n, pie ? p.percent : undefined),
                     p.color) },
        series: [pie
          ? { type: 'pie', radius: ['30%', '70%'], data,
              label: { ...label, formatter: data.some((/** @type {any} */ d) => d.text != null)
                ? markText : '{b}' },
              emphasis: shadow(0.2) }
          : { type: 'treemap', data, left: 10, right: 10, top: 2, bottom: 2, roam: false,
              nodeClick: false, breadcrumb: { show: false },
              label: { ...label,
                       formatter: (/** @type {any} */ p) =>
                         markText(p) + '\n' + NS.ddNum(Number(p.value)) },
              itemStyle: { borderColor: ink.surface, borderWidth: 2, gapWidth: 2 },
              emphasis: shadow(0.15) }]
      }
    };
  };

  Object.assign(parts, { pie: builder(true), treemap: builder(false) });
})();
