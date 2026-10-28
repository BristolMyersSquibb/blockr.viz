// @ts-check
/**
 * The table view without the block's output binding: a Web page download, a
 * knitted document, interactive_table(). R writes an element of class
 * `blockr-viz-static` with `data-viz-view="table"` holding the payload as
 * JSON (`<script type="application/json">`), in the shape the block sends.
 *
 * - `renderStatic(el, payload, opts)` draws `payload` on `el`; again on the
 *   same element, it re-renders through the same view, so search, sort,
 *   folds and scroll stay, and `data: null` keeps the rows it has. `opts.gear === false`: no gear, plain titles.
 *   `opts.still`: every row and no tools, as for a printed page.
 * - Every such element is drawn on DOM ready and when one is added later
 *   (renderUI). No gear unless `data-viz-gear="on"`;
 *   `data-viz-still="off"` keeps the row window and the tools.
 * - R's `blockr-viz-static` message `{id, payload}` re-renders the element
 *   with that id.
 *
 * A click goes out as `<id>_select`, as from the block; without Shiny it
 * goes nowhere. The page's tables live as long as the page, under one Scope.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const T = V.table;
  const SEL = '.blockr-viz-static[data-viz-view="table"]';

  const page = new V.Scope();
  /** @type {WeakMap<Element, { view: InstanceType<VizViewClass>, scope: VizScope, data?: unknown }>} */
  const mounts = new WeakMap();

  /**
   * Draw `payload` on `el`, or redraw it there.
   * @param {HTMLElement} el
   * @param {VizPayload} payload
   * @param {TableViewOpts} [opts]
   */
  function renderStatic(el, payload, opts = {}) {
    let m = mounts.get(el);
    if (!m) {
      const scope = page.own(new V.Scope());
      const View = /** @type {new (el: HTMLElement, scope: VizScope, o: TableViewOpts) => InstanceType<VizViewClass>} */ (
        /** @type {unknown} */ (T.View));
      m = { scope, view: scope.own(new View(el, scope, opts)) };
      mounts.set(el, m);
      scope.own({ dispose: () => mounts.delete(el) });
    }
    // No rows means the rows drawn last, as in the block.
    if (payload.data == null && m.data != null) payload = Object.assign({}, payload, { data: m.data });
    else m.data = payload.data;
    m.view.render(payload);
  }

  /**
   * The options an element asks for.
   * @param {Element} el
   * @returns {TableViewOpts}
   */
  const optsOf = (el) => ({ gear: el.getAttribute('data-viz-gear') === 'on',
                            still: el.getAttribute('data-viz-still') !== 'off' });

  /** @param {Element} el */
  function fromJson(el) {
    const json = el.querySelector(':scope > script[type="application/json"]');
    if (mounts.has(el) || !json) return;
    let p;
    try {
      p = /** @type {VizPayload} */ (JSON.parse(json.textContent || '{}'));
    } catch (e) {
      return;
    }
    renderStatic(/** @type {HTMLElement} */ (el), p, optsOf(el));
  }

  /**
   * Draw every table under `node` not drawn yet.
   * @param {ParentNode} [node]
   */
  function mount(node) {
    const at = node || root.document;
    if (at instanceof root.Element && at.matches(SEL)) fromJson(at);
    for (const el of Array.from(at.querySelectorAll(SEL))) fromJson(/** @type {Element} */ (el));
  }

  function start() {
    mount();
    page.mutate(root.document.body, (/** @type {MutationRecord[]} */ recs) => {
      for (const r of recs) {
        for (const n of Array.from(r.addedNodes)) if (n instanceof root.Element) mount(/** @type {Element} */ (n));
      }
    }, { childList: true, subtree: true });
  }

  if (root.document.readyState === 'loading') page.on(root.document, 'DOMContentLoaded', start);
  else start();

  const shiny = root.Shiny;
  if (shiny && typeof shiny.addCustomMessageHandler === 'function') {
    shiny.addCustomMessageHandler('blockr-viz-static',
      (/** @type {{ id: string, payload: VizPayload }} */ msg) => {
        const el = root.document.getElementById(msg.id);
        if (el) renderStatic(el, msg.payload, optsOf(el));
      });
  }

  T.renderStatic = renderStatic;
  T.mount = mount;
})();
