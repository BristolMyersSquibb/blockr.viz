// @ts-check
/**
 * Helpers every block view shares: one HTML escaper and one way to send an
 * input to R. Timers and listeners are not here; they belong to a Scope
 * (scope.js), so they go away with their block.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /** @type {Record<string, string>} */
  const ENTITIES = { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' };

  /**
   * Text made safe for HTML content and for a quoted attribute value.
   * @param {unknown} x
   * @returns {string}
   */
  const esc = (x) => String(x == null ? '' : x).replace(/[&<>"']/g, (c) => ENTITIES[c]);

  /** The input handler type R registered for each input (R/kit-input.R). */
  const TYPES = { select: 'blockr.viz.selection', capture: 'blockr.viz.capture' };

  /**
   * Send one of a view's inputs to R: `<id>_set`, `<id>_select` or
   * `<id>_capture`, typed so R's input handler checks it on arrival, or
   * `<id>_need` (the data revision the view holds no rows for). Priority
   * "event", so the same value sent twice arrives twice.
   * @param {string} id The view's element id.
   * @param {'set' | 'select' | 'capture' | 'need'} kind
   * @param {unknown} value
   * @param {string} [block] For `set`: the block whose spec reads it.
   * @returns {boolean} Whether Shiny took it.
   */
  const send = (id, kind, value, block) => {
    const shiny = root.Shiny;
    if (!id || !shiny || typeof shiny.setInputValue !== 'function') return false;
    const type = kind === 'set' ? 'blockr.viz.' + block
      : kind === 'need' ? null : TYPES[kind];
    const name = id + '_' + kind + (type ? ':' + type : '');
    shiny.setInputValue(name, value, { priority: 'event' });
    return true;
  };

  V.util = { esc, send };
})();
