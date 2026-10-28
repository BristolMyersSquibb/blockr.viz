// @ts-check
/**
 * Chart block: the data tooltip on the ECharts card. The rows are core's
 * (core/tooltip.js); here the card every tooltip sits on, and a headline
 * over label/value pairs.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** A headline over label/value rows; empty values are dropped.
   * @param {any} headline @param {Array<[string, any]>} pairs @param {any} [color] */
  const rowTooltip = (headline, pairs, color) =>
    V.tooltip.head(V.util.esc(headline), color) + pairs
      .filter((p) => p[1] != null && p[1] !== '')
      .map((p) => V.tooltip.row(V.util.esc(p[0]), V.util.esc(p[1])))
      .join('');

  /** The raised card every tooltip sits on; mutates the option. @param {any} option @param {Ink} ink */
  const cardTooltip = (option, ink) => {
    const tts = Array.isArray(option.tooltip) ? option.tooltip
      : option.tooltip ? [option.tooltip] : [];
    for (const t of tts) {
      Object.assign(t, {
        backgroundColor: ink.raised, borderColor: ink.border, borderWidth: 1,
        padding: [6, 10],
        textStyle: { color: ink.text, fontSize: 12, fontFamily: ink.face },
        extraCssText: 'border-radius:8px;box-shadow:' + ink.shadow + ';'
      });
    }
    return option;
  };

  Object.assign(NS, { rowTooltip, cardTooltip });
})();
