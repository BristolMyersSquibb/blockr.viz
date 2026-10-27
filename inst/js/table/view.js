// @ts-check
/**
 * The table block's view, registered with the shared output binding as
 * `blockr.viz.table`. It builds the frame once and hands each payload to the
 * layout named in `config.layout`; a layout switch disposes the old
 * layout's view and everything it set up. Loaded last of the table scripts.
 *
 * The gear comes from the core gear engine (`Blockr.viz.gear`) once it is
 * loaded; until then the view has no gear.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const T = V.table;

  class TableView {
    /**
     * @param {HTMLElement} el
     * @param {VizScope} scope
     */
    constructor(el, scope) {
      this.scope = scope;
      this.frame = new T.Frame(el, scope);
      /** @type {string | null} */
      this.layout = null;
      /** @type {TableLayoutView | null} */
      this.view = null;
      /** @type {VizScope | null} */
      this.viewScope = null;
      const G = /** @type {any} */ (V).gear;
      /** @type {{ update(p: VizPayload): void } | null} */
      this.gear = G && typeof G.mount === 'function'
        ? scope.own(G.mount({ head: this.frame.head, id: el.id, block: 'table',
                              spec: V.spec.table, scope }))
        : null;
    }

    /** @param {VizPayload} p */
    render(p) {
      const layout = String((p.config && p.config.layout) || 'records');
      if (layout !== this.layout) this.switchTo(layout);
      if (this.gear) this.gear.update(p);
      if (this.view) {
        this.view.render(p);
      } else {
        this.frame.empty({ kind: 'error', message: 'The ' + layout + ' layout cannot be drawn here.' });
      }
    }

    /** @param {string} layout */
    switchTo(layout) {
      if (this.viewScope) this.viewScope.dispose();
      this.view = null;
      this.layout = layout;
      this.frame.empty(null);
      const Layout = T.layouts[layout];
      if (!Layout) return;
      this.viewScope = new V.Scope();
      this.view = this.viewScope.own(new Layout(this.frame, this.viewScope));
    }

    /** @param {string} message */
    error(message) {
      this.frame.empty({ kind: 'error', message });
    }

    dispose() {
      if (this.viewScope) this.viewScope.dispose();
      this.viewScope = null;
      this.view = null;
    }
  }

  T.View = TableView;
  V.binding('blockr.viz.table', 'table', TableView);
})();
