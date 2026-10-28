// @ts-check
/**
 * Chart block: what a click may do (`drill`, `drill_target`) and what a
 * `highlight` lights. The selection itself is selection.js.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** The tri-state drill: 'off', 'auto' or 'column'. @param {Record<string, any>} cfg */
  const drillState = (cfg) => (!cfg.drill ? 'off' : cfg.drill === 'auto' ? 'auto' : 'column');

  /** The column a click filters on (the family's natural key for auto), or null for a
   * geometric selection. @param {Record<string, any>} cfg @returns {string | null} */
  const drillColumn = (cfg) => {
    const d = cfg.drill;
    if (!d) return null;
    if (d !== 'auto') return d;
    const fam = NS.familyOf(cfg.type);
    if (fam === 'aggregated') return cfg.category || null;
    if (fam === 'timeline') return cfg.y || null;
    return cfg.series || cfg.color || null;
  };

  /** A drill_target makes a click an event sent to another block. @param {Record<string, any>} cfg */
  const transientDrill = (cfg) => !!(cfg.drill_target && String(cfg.drill_target).trim());

  /** Hover shows the tooltip and nothing else: no emphasis, no blur of the
   * other marks, on every chart. The hand cursor shows where a click
   * selects; with drill off the chart is a display. Mutates the option.
   * @param {any} option @param {Record<string, any>} cfg */
  const quietHover = (option, cfg) => {
    if (!option || !Array.isArray(option.series)) return option;
    const cursor = drillState(cfg) === 'off' ? 'default' : 'pointer';
    for (const s of option.series) {
      if (!s) continue;
      // ECharts still scales a line's symbols on an axis hover unless `scale`
      // is off. A custom series' shapes say so themselves (emphasisDisabled).
      s.emphasis = { disabled: true, scale: false };
      s.cursor = cursor;
      if (s.itemStyle) s.itemStyle.cursor = cursor;
      if (s.lineStyle) s.lineStyle.cursor = cursor;
    }
    return option;
  };

  // A selection outlines the marks it takes and fades the others to this
  // share of their opacity; hover draws nothing (quietHover()).
  const SEL_DIM = 0.25;
  const SEL_OUTLINE = 1.5;

  /** A mark's style under the selection. `filtered`: something is selected;
   * `on`: this mark is. Without a selection the mark has its series' own
   * border (`base`, the series' itemStyle).
   * @param {boolean} on @param {boolean} filtered @param {Ink} ink @param {any} [base] */
  const selStyle = (on, filtered, ink, base) => (filtered && on
    ? { opacity: 1, borderColor: ink.text, borderWidth: SEL_OUTLINE }
    : { opacity: filtered ? SEL_DIM : 1,
        borderColor: (base && base.borderColor) || 'transparent',
        borderWidth: (base && base.borderWidth) || 0 });

  // `highlight` names a column and some of its values. A mark is lit when its
  // rows hold one of them (chart-keys.js markLit); the others are drawn in
  // the muted grey, behind. The types that draw it:
  const HIGHLIGHT_TYPES = ['bar', 'scatter', 'line', 'area'];

  /** The highlight as a filter, {col: [values]}, or null.
   * @param {Record<string, any>} cfg @returns {Record<string, string[]> | null} */
  const highlightOf = (cfg) => {
    const h = cfg.highlight;
    const col = h && typeof h === 'object' ? Object.keys(h)[0] : null;
    if (!col || !HIGHLIGHT_TYPES.includes(cfg.type)) return null;
    const vals = NS.labelled(h[col]).map((/** @type {{ value: any }} */ x) => x.value).filter(NS.hasVal).map(String);
    return vals.length ? { [col]: vals } : null;
  };

  Object.assign(NS, { drillState, drillColumn, transientDrill, quietHover, highlightOf,
                      selStyle, SEL_DIM });
})();
