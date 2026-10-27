// @ts-check
/**
 * The table block's view, registered with the shared output binding as
 * `blockr.viz.table`. It builds the frame once and hands each payload to the
 * layout named in `config.layout`; a layout switch disposes the old
 * layout's view and everything it set up. Loaded last of the table scripts.
 *
 * The gear (gear.js) and the title slots come from the core gear engine
 * and core/titles.js when they are loaded; without them the view has no
 * gear and plain titles.
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
      this.gear = T.gear.mount(this.frame, scope, () => this.info());
      const S = /** @type {any} */ (V).titles;
      if (this.gear && S && typeof S.slots === 'function') {
        const gear = this.gear;
        this.frame.slots = scope.own(S.slots({ gear: gear.engine, scope, openGear: gear.open }));
      }
    }

    /** @param {VizPayload} p */
    render(p) {
      const layout = String((p.config && p.config.layout) || 'records');
      if (layout !== this.layout) this.switchTo(layout);
      if (this.view) {
        this.view.render(p);
      } else {
        this.frame.empty({ kind: 'error', message: 'The ' + layout + ' layout cannot be drawn here.' });
      }
      // After the layout: the gear's rules read what it now draws.
      if (this.gear) this.gear.update(p);
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

    /** @returns {TableInfo} */
    info() {
      const v = this.view;
      return v && v.info ? v.info() : { structured: false, finished: false, numbers: true };
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
