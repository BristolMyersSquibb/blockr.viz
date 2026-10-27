// @ts-check
/**
 * The Shiny output binding every block view shares. R sends a view's payload
 * as the value of an output (R/kit-output.R); Shiny keeps the last value per
 * output id and hands it to the binding again whenever an element under that
 * id binds, so a re-mounted panel draws without asking R.
 *
 * What the binding adds to Shiny's:
 * - one view per element, built on the first value, with its own Scope;
 * - a value already drawn (Shiny replays it on every re-bind) is skipped;
 * - rows are kept per id, because R leaves them out of a payload whose data
 *   revision the client holds; a payload without rows the client does not
 *   hold asks R for them once (`<id>_need`);
 * - an error without a message (a silent req()) keeps the picture;
 * - when Shiny unbinds an element that has left the page, its view is
 *   disposed, and its rows are dropped after a grace period unless a new
 *   element under the same id mounts first.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /** @type {Map<string, { rev: number, data: unknown }>} */
  const rows = new Map();
  /** @type {Map<string, VizMount>} */
  const live = new Map();
  /** @type {Map<string, () => void>} */
  const dropping = new Map();
  /** Owns the timers that drop rows; it lives as long as the page. */
  const pageScope = new V.Scope();

  /**
   * @param {HTMLElement} el
   * @param {VizViewClass} View
   * @param {number} grace
   * @returns {VizMount}
   */
  function mount(el, View, grace) {
    const old = live.get(el.id);
    if (old) old.scope.dispose();
    const cancelDrop = dropping.get(el.id);
    if (cancelDrop) {
      cancelDrop();
      dropping.delete(el.id);
    }
    const scope = new V.Scope();
    /** @type {VizMount} */
    const m = { el, scope, view: scope.own(new View(el, scope)), last: null,
                needRev: null };
    live.set(el.id, m);
    // A re-bind of the same element unbinds it first; only an element that
    // has left the page loses its view.
    const $el = root.jQuery(el);
    const onUnbound = () => {
      scope.timeout(() => {
        if (!el.isConnected) unmount(m, grace);
      }, 0);
    };
    $el.on('shiny:unbound', onUnbound);
    scope.own({ dispose: () => $el.off('shiny:unbound', onUnbound) });
    /** @type {any} */ (el)._viz = m;
    return m;
  }

  /**
   * @param {VizMount} m
   * @param {number} grace
   */
  function unmount(m, grace) {
    m.scope.dispose();
    /** @type {any} */ (m.el)._viz = null;
    const id = m.el.id;
    if (live.get(id) !== m) return;
    live.delete(id);
    dropping.set(id, pageScope.timeout(() => {
      dropping.delete(id);
      if (!live.has(id)) rows.delete(id);
    }, grace));
  }

  /**
   * @param {HTMLElement} el
   * @param {VizPayload | null} payload
   * @param {VizViewClass} View
   * @param {number} grace
   */
  function render(el, payload, View, grace) {
    const m = /** @type {any} */ (el)._viz || mount(el, View, grace);
    if (payload == null || m.last === payload) return;
    m.last = payload;
    let p = payload;
    if (p.data != null) {
      rows.set(el.id, { rev: p.data_rev, data: p.data });
    } else {
      const held = rows.get(el.id);
      if (!held || held.rev !== p.data_rev) {
        if (m.needRev !== p.data_rev) {
          m.needRev = p.data_rev;
          V.util.send(el.id, 'need', p.data_rev);
        }
        return;
      }
      p = Object.assign({}, p, { data: held.data });
    }
    m.needRev = null;
    m.view.render(p);
  }

  /**
   * Register the output binding for one kind of view.
   * @param {string} name The binding's name ("blockr.viz.chart").
   * @param {string} view The elements' `data-viz-view` ("chart").
   * @param {VizViewClass} View Builds the view: `new View(el, scope)`.
   * @param {{ grace?: number }} [opts] `grace`: how long, in ms, the rows of
   *   a removed element are kept for a new element under the same id.
   * @returns {boolean} Whether Shiny took it.
   */
  function binding(name, view, View, opts = {}) {
    const shiny = root.Shiny;
    if (!shiny || !shiny.OutputBinding || !root.jQuery) return false;
    const grace = opts.grace == null ? 5000 : opts.grace;
    const b = new shiny.OutputBinding();
    Object.assign(b, {
      /** @param {unknown} scope */
      find: (scope) => root.jQuery(scope)
        .find('.blockr-viz-output[data-viz-view="' + view + '"]'),
      /** @param {HTMLElement} el @param {VizPayload | null} payload */
      renderValue: (el, payload) => render(el, payload, View, grace),
      /** @param {HTMLElement} el @param {{ message?: string }} err */
      renderError: (el, err) => {
        const m = /** @type {any} */ (el)._viz;
        if (err && err.message && m && typeof m.view.error === 'function') {
          m.view.error(err.message);
        }
      },
      clearError: () => {}
    });
    shiny.outputBindings.register(b, name);
    return true;
  }

  V.binding = binding;
})();
