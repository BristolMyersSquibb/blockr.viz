// @ts-check
/**
 * The Shiny output binding every block view shares. A Shiny output decides
 * when R builds a view's payload; the payload itself arrives as a
 * `blockr-viz:payload` message, sent the moment it is built, and the
 * output's value, which Shiny sends once the whole flush is done, is a token
 * that says so (R/kit-output.R). A session without messages (a test) sends
 * the payload as the output's value, and the binding draws that as well.
 *
 * What the binding adds to Shiny's:
 * - the last payload posted for an id is kept: an element that binds after
 *   the message came, or again after a move, draws from it without asking R;
 * - one view per element, built on the first value, with its own Scope;
 * - a value already drawn (Shiny replays it on every re-bind) is skipped;
 * - the view is told whether the rows are new to it (`rowsChanged`), so it
 *   parses and indexes them only then;
 * - rows are kept per id, because R leaves them out of a payload whose data
 *   revision the client holds; a payload without rows the client does not
 *   hold asks R for them once (`<id>_need`). The column description is kept
 *   with them: such a payload carries `columns` only when they changed;
 * - an error without a message (a silent req()) keeps the picture;
 * - a light/dark switch (data-bs-theme anywhere on the page) renders the
 *   last value again, so a view reads the new colours;
 * - with a `busy` label, the view is built as soon as Shiny finds the
 *   element, and the busy cue counts the wait for R's first value from
 *   when the element shows;
 * - when Shiny unbinds an element that has left the page, its view is
 *   disposed, and its rows are dropped after a grace period unless a new
 *   element under the same id mounts first.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /** @type {Map<string, { rev: number, data: unknown, columns: any }>} */
  const rows = new Map();
  /** The last payload R posted for each id. @type {Map<string, VizPayload>} */
  const posted = new Map();
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
                needRev: null, drawn: null };
    scope.mutate(document.documentElement, () => { if (m.drawn) m.view.render(m.drawn, false); },
                 { attributes: true, attributeFilter: ['data-bs-theme'], subtree: true });
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
    // A payload posted before this element bound, or to the one before it.
    const waiting = posted.get(el.id);
    if (waiting) draw(m, waiting);
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
      if (!live.has(id)) {
        rows.delete(id);
        posted.delete(id);
      }
    }, grace));
  }

  /**
   * @param {HTMLElement} el
   * @param {VizPayload | null} payload
   * @param {VizViewClass} View
   * @param {number} grace
   */
  function render(el, payload, View, grace) {
    const m = /** @type {VizMount} */ (/** @type {any} */ (el)._viz || mount(el, View, grace));
    // The output's token: the payload came, or comes, as a message.
    if (payload && /** @type {any} */ (payload).posted != null) {
      const p = posted.get(el.id);
      if (p) {
        draw(m, p);
      } else if (m.needRev !== payload.data_rev) {
        // A token without its message: the page was not there to take it.
        m.needRev = payload.data_rev;
        V.util.send(el.id, 'need', payload.data_rev);
      }
      return;
    }
    draw(m, payload);
  }

  /**
   * R's `blockr-viz:payload`: keep it, and draw it if its element has a view.
   * @param {{ id?: string, payload?: VizPayload | null }} msg
   */
  function post(msg) {
    if (!msg || !msg.id || !msg.payload) return;
    posted.set(msg.id, msg.payload);
    const m = live.get(msg.id);
    if (m) draw(m, msg.payload);
  }

  /**
   * @param {VizMount} m
   * @param {VizPayload | null} payload
   */
  function draw(m, payload) {
    const el = m.el;
    if (payload == null || m.last === payload) return;
    m.last = payload;
    let p = payload;
    if (p.data != null) {
      rows.set(el.id, { rev: p.data_rev, data: p.data, columns: p.columns });
    } else {
      const held = rows.get(el.id);
      if (!held || held.rev !== p.data_rev) {
        if (m.needRev !== p.data_rev) {
          m.needRev = p.data_rev;
          V.util.send(el.id, 'need', p.data_rev);
        }
        return;
      }
      // `columns` absent: the ones held. Present, null included: the new ones.
      if (p.columns === undefined) {
        p = Object.assign({}, p, { data: held.data, columns: held.columns });
      } else {
        held.columns = p.columns;
        p = Object.assign({}, p, { data: held.data });
      }
    }
    m.needRev = null;
    // The view parses its rows only when they are new to it.
    const rowsChanged = !m.drawn || m.drawn.data_rev !== p.data_rev;
    m.drawn = p;
    m.view.render(p, rowsChanged);
  }

  /**
   * A found element without a view waits for R's first value. A board that
   * builds blocks lazily binds the element when its panel shows, and R
   * speaks a second or more later. The view is built now, so its busy line
   * exists, and the busy cue's watch() (core/busy.js) counts the wait.
   * @param {HTMLElement} el @param {VizViewClass} View @param {number} grace
   * @param {string} label
   */
  function early(el, View, grace, label) {
    if (!el.id || /** @type {any} */ (el)._viz) return;
    mount(el, View, grace);
    const busy = root.Blockr && root.Blockr.busy;
    if (busy && typeof busy.watch === 'function') busy.watch(el.id, label);
  }

  /**
   * Register the output binding for one kind of view.
   * @param {string} name The binding's name ("blockr.viz.chart").
   * @param {string} view The elements' `data-viz-view` ("chart").
   * @param {VizViewClass} View Builds the view: `new View(el, scope)`.
   * @param {{ grace?: number, busy?: string }} [opts] `grace`: how long, in
   *   ms, the rows of a removed element are kept for a new element under the
   *   same id. `busy`: the busy cue's word for the first wait ("drawing").
   * @returns {boolean} Whether Shiny took it.
   */
  function binding(name, view, View, opts = {}) {
    const shiny = root.Shiny;
    if (!shiny || !shiny.OutputBinding || !root.jQuery) return false;
    const grace = opts.grace == null ? 5000 : opts.grace;
    const b = new shiny.OutputBinding();
    Object.assign(b, {
      /** @param {unknown} scope */
      find: (scope) => {
        const els = root.jQuery(scope).find('.blockr-viz-output[data-viz-view="' + view + '"]');
        const label = opts.busy;
        if (label) for (const el of Array.from(els)) early(el, View, grace, label);
        return els;
      },
      /** @param {HTMLElement} el @param {VizPayload | null} payload */
      renderValue: (el, payload) => render(el, payload, View, grace),
      /** @param {HTMLElement} el @param {{ message?: string }} err */
      renderError: (el, err) => {
        const m = /** @type {any} */ (el)._viz;
        if (err && err.message && m && typeof m.view.error === 'function') {
          m.view.error(err.message);
        }
      },
      clearError: () => {},
      // Shiny marks a recalculating output with a class on its element, and
      // takes it off when the value arrives. The element holds the whole
      // view, so each change restyles every node under it: a table of
      // 57,000 nodes twice more than it needs. The view has its own busy
      // cue (core/busy.js), and its payload arrives before the output's
      // value, so the class would dim a picture that is already new.
      showProgress: () => {}
    });
    shiny.outputBindings.register(b, name);
    return true;
  }

  binding.post = post;
  V.binding = binding;
  const shiny0 = root.Shiny;
  if (shiny0 && typeof shiny0.addCustomMessageHandler === 'function') {
    shiny0.addCustomMessageHandler('blockr-viz:payload', post);
  }
})();
