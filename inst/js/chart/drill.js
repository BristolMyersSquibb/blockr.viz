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

  /** With drill off the chart is a display: no hover emphasis, no hand cursor. Mutates
   * the option. @param {any} option @param {Record<string, any>} cfg */
  const applyDrillEmphasis = (option, cfg) => {
    if (drillState(cfg) === 'off' && option && Array.isArray(option.series)) {
      for (const s of option.series) {
        if (!s) continue;
        // A line's emphasis without focus or scaling draws its dot at the
        // crosshair; it is no drill affordance, so it stays.
        const crosshairDot = s.type === 'line' && s.emphasis &&
          s.emphasis.focus === 'none' && s.emphasis.scale === false;
        if (!crosshairDot) s.emphasis = { disabled: true };
        s.cursor = 'default';
        if (s.itemStyle) s.itemStyle.cursor = 'default';
        if (s.lineStyle) s.lineStyle.cursor = 'default';
      }
    }
    return option;
  };

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

  Object.assign(NS, { drillState, drillColumn, transientDrill, applyDrillEmphasis, highlightOf });
})();
