// @ts-check
/**
 * Tables on a page without Shiny: the Web page download, a knitted
 * document. R writes each table as an element of class `blockr-viz-static`
 * with `data-viz-view="table"`, holding its payload as JSON
 * (`table_static()`, R/table-dep.R). Each is drawn once by the layout its
 * `config.layout` names, still: every row, no gear, nothing sent.
 *
 * The page's tables live as long as the page, under one Scope.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const T = V.table;

  const page = new V.Scope();
  /** @type {WeakSet<Element>} */
  const drawn = new WeakSet();

  /**
   * Draw one table from the JSON it holds.
   * @param {Element} el
   */
  function draw(el) {
    const json = el.querySelector('script[type="application/json"]');
    if (drawn.has(el) || !json) return;
    drawn.add(el);
    const p = /** @type {VizPayload} */ (JSON.parse(json.textContent || '{}'));
    const Layout = T.layouts[String((p.config && p.config.layout) || 'records')];
    if (!Layout) return;
    const scope = page.own(new V.Scope());
    const frame = new T.Frame(el.appendChild(root.document.createElement('div')), scope);
    const view = /** @type {TableLayoutView & { still?: boolean }} */ (scope.own(new Layout(frame, scope)));
    view.still = true;
    view.render(p);
  }

  /**
   * Draw every table under `node` not drawn yet.
   * @param {ParentNode} [node]
   */
  function mount(node) {
    for (const el of Array.from((node || root.document).querySelectorAll(
      '.blockr-viz-static[data-viz-view="table"]'))) {
      draw(el);
    }
  }

  if (root.document.readyState === 'loading') {
    page.on(root.document, 'DOMContentLoaded', () => mount());
  } else {
    mount();
  }

  T.mount = mount;
})();
