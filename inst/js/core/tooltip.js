// @ts-check
/**
 * The data tooltip both blocks draw: a headline and label/value rows,
 * styled by tooltip.css. The pieces take HTML the caller has escaped
 * (V.util.esc); a colour is escaped here.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const esc = V.util.esc;

  /** A colour swatch, or nothing without a colour. @param {unknown} c */
  const swatch = (c) => (typeof c === 'string' && c
    ? '<span class="vz-tt-sw" style="background:' + esc(c) + '"></span>' : '');

  /** @param {string} html @param {unknown} [color] */
  const head = (html, color) =>
    '<div class="vz-tt-head">' + swatch(color) + '<span>' + html + '</span></div>';

  /**
   * A label/value row. `hit` marks a row of a card listing every level of a
   * colour split: true for the level under the pointer, false for the others.
   * @param {string} label @param {unknown} value @param {unknown} [color]
   * @param {boolean | null} [hit]
   */
  const row = (label, value, color, hit) =>
    '<div class="vz-tt-row' + (hit === true ? ' is-hit' : hit === false ? ' is-dim' : '') +
    '">' + swatch(color) + '<span class="vz-tt-label">' +
    label + '</span><span class="vz-tt-value">' + value + '</span></div>';

  /** A muted line of its own, for what the rows below it are. @param {string} html */
  const note = (html) => '<div class="vz-tt-row vz-tt-label">' + html + '</div>';

  V.tooltip = { swatch, head, row, note, SEP: '<div class="vz-tt-sep"></div>' };
})();
