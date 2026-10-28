// @ts-check
/**
 * Helpers every block view shares: one HTML escaper, one way to send an
 * input to R (and a selection, with its nonce), what a column
 * description (VizColumn) says about its values, and a number for reading. Timers and listeners are
 * not here; they belong to a Scope (scope.js), so they go away with their
 * block.
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
  const TYPES = { select: 'blockr.viz.selection', capture: 'blockr.viz.capture',
                  data: 'blockr.viz.data_view' };

  /**
   * Send one of a view's inputs to R: `<id>_set`, `<id>_select`,
   * `<id>_capture` or `<id>_data` (core/data-view.js), typed so R's input
   * handler checks it on arrival, or
   * `<id>_need` (the data revision the view holds no rows for). Priority
   * "event", so the same value sent twice arrives twice.
   * @param {string} id The view's element id.
   * @param {'set' | 'select' | 'capture' | 'data' | 'need'} kind
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

  // One counter for the page: every selection a view sends carries a new
  // number, so R tells a second click on the same mark (or a second clear)
  // from a board re-evaluation that hands back the value it already had.
  let nonce = 0;

  /**
   * Send a view's selection to R as `<id>_select`: `{selection, nonce}`,
   * `selection` null for a clear.
   * @param {string} id The view's element id.
   * @param {Record<string, unknown> | null} selection
   * @returns {boolean} Whether Shiny took it.
   */
  const select = (id, selection) => send(id, 'select', { selection, nonce: ++nonce });

  /**
   * A column that holds numbers: a number or a duration. A date or
   * date-time is not; isTemporal() names those.
   * @param {VizColumn | null | undefined} c
   */
  const isNumeric = (c) => !!c && (c.class === 'number' || c.class === 'duration');

  /** @param {VizColumn | null | undefined} c */
  const isTemporal = (c) => !!c && (c.class === 'date' || c.class === 'datetime');

  /**
   * A number for reading: up to two decimals, and a value that would round
   * to 0 keeps two significant digits (0.0034). Display only; the data
   * keeps full precision. Anything else comes back as it is.
   * @param {unknown} v
   * @returns {unknown}
   */
  const num = (v) => {
    if (typeof v !== 'number' || !isFinite(v)) return v;
    if (Number.isInteger(v)) return v.toLocaleString();
    if (Math.abs(v) < 0.005) return v.toLocaleString(undefined, { maximumSignificantDigits: 2 });
    return v.toLocaleString(undefined, { maximumFractionDigits: 2 });
  };

  /**
   * A raw observation for reading: num() of it at three significant digits.
   * @param {unknown} v
   * @returns {unknown}
   */
  const num3 = (v) => (typeof v === 'number' && isFinite(v) ? num(Number(v.toPrecision(3))) : v);

  V.util = { esc, send, select, isNumeric, isTemporal, num, num3 };
})();
