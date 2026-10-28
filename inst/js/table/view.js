// @ts-check
/**
 * The table block's view, registered with the shared output binding as
 * `blockr.viz.table`. It builds the frame once and hands each payload to the
 * layout named in `config.layout`; a layout switch disposes the old
 * layout's view and everything it set up. Loaded last of the table scripts.
 *
 * The gear (gear.js) is the core gear engine; with it the titles carry
 * core/titles.js's slots. A view built with `gear: false` (a printed page)
 * has neither.
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
     * @param {TableViewOpts} [opts] `gear: false` mounts no gear and no
     *   title slots; `still` draws every row with no tools (a printed page).
     */
    constructor(el, scope, opts = {}) {
      this.scope = scope;
      this.el = el;
      this.still = !!opts.still;
      this.frame = new T.Frame(el, scope);
      /** @type {string | null} */
      this.layout = null;
      /** @type {TableLayoutView | null} */
      this.view = null;
      /** @type {VizScope | null} */
      this.viewScope = null;
      this.gear = opts.gear === false ? null : T.gear.mount(this.frame, scope, () => this.info());
      // The prepare script's controls, under the header.
      this.script = this.gear ? V.scriptBand(this.frame.el, scope, this.gear.engine) : null;
      if (this.script) this.frame.body.before(this.script.el);
    }

    /** @param {VizPayload} p */
    render(p) {
      const layout = String((p.config && p.config.layout) || 'records');
      // A switch takes the old layout's rows away before the new one draws,
      // which would pull a scrolled panel up; it keeps its place.
      const sc = layout !== this.layout ? this.frame.scroller() : null;
      const top = sc ? sc.scrollTop : 0;
      if (layout !== this.layout) this.switchTo(layout);
      if (this.view) {
        this.view.render(p);
        if (sc) {
          sc.scrollTop = top;
          this.scope.frame(() => { sc.scrollTop = top; });
        }
      } else {
        this.frame.empty({ kind: 'error', message: 'The ' + layout + ' layout cannot be drawn here.' });
      }
      // After the layout: the gear's rules read what it now draws.
      if (this.gear) this.gear.update(p);
      if (this.script) this.script.update(/** @type {TableDerived} */ (p.derived || {}).script);
      this.frame.busyDone();
    }

    /** @param {string} layout */
    switchTo(layout) {
      if (this.viewScope) this.viewScope.dispose();
      this.view = null;
      this.layout = layout;
      this.frame.el.setAttribute('data-layout', layout);
      this.frame.empty(null);
      const Layout = T.layouts[layout];
      if (!Layout) return;
      this.viewScope = new V.Scope();
      this.view = this.viewScope.own(new Layout(this.frame, this.viewScope, { still: this.still }));
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
  V.binding('blockr.viz.table', 'table', TableView, { busy: 'drawing' });
})();
