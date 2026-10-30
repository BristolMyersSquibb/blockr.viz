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

  /** How long a layout pick holds the panel when R does not answer it. */
  const HOLD_MS = 20000;

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
      /** A layout pick R has not drawn yet: the panel's scroll at the
       * click, and the layout picked. */
      /** @type {{ sc: Element, top: number, to: string, release: () => void } | null} */
      this.held = null;
      this.gear = opts.gear === false ? null
        : T.gear.mount(this.frame, scope, (layout) => this.info(layout), (to) => this.hold(to));
      // The prepare script's controls, under the header.
      this.script = this.gear ? V.scriptBand(this.frame.el, scope, this.gear.engine) : null;
      if (this.script) this.frame.body.before(this.script.el);
    }

    /** @param {VizPayload} p */
    render(p) {
      const layout = String((p.config && p.config.layout) || 'records');
      // A switch takes the old layout's rows away before the new one draws,
      // which would pull a scrolled panel up; it keeps its place, the place
      // at the click when the gear picked the layout. The place comes from
      // the panel's scroll events: reading it here would restyle every row
      // of the old layout first.
      const switching = layout !== this.layout;
      const at = switching ? this.held || this.frame.scrollPlace() : null;
      if (switching) this.switchTo(layout);
      // The layouts paint the titles, before the gear's update: the words that name the
      // prepare script's values need its controls now.
      const engine = this.gear && this.gear.engine;
      if (engine && engine.useScript) engine.useScript(/** @type {TableDerived} */ (p.derived || {}).script);
      if (this.view) {
        this.view.render(p);
        if (this.held && (switching || layout === this.held.to)) this.unhold();
        if (at) {
          const { sc, top } = at;
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

    /**
     * A layout pick in the gear. The tray shows the new layout's rows at
     * once, and a shorter tray would pull the scrolled panel up before R
     * sends the new layout: the frame keeps its height until it draws.
     * @param {string} to
     */
    hold(to) {
      const h = this.held;
      if (h) {
        h.release();
        h.to = to;
      } else {
        const at = this.frame.scrollPlace();
        if (!at) return;
        if (this.frame.height > 0) this.frame.el.style.minHeight = this.frame.height + 'px';
        this.held = { sc: at.sc, top: at.top, to, release: () => {} };
      }
      // R may refuse the pick; the frame lets go after a while.
      /** @type {NonNullable<TableView['held']>} */ (this.held).release =
        this.scope.timeout(() => this.unhold(), HOLD_MS);
    }

    unhold() {
      if (!this.held) return;
      this.held.release();
      this.held = null;
      this.frame.el.style.minHeight = '';
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

    /** @param {string} [layout] Nothing special until R draws it. @returns {TableInfo} */
    info(layout) {
      const v = layout == null || layout === this.layout ? this.view : null;
      return v && v.info ? v.info() : { structured: false, finished: false, numbers: true };
    }

    /** @param {string} message */
    error(message) {
      this.frame.empty({ kind: 'error', message });
    }

    dispose() {
      this.unhold();
      if (this.viewScope) this.viewScope.dispose();
      this.viewScope = null;
      this.view = null;
    }
  }

  T.View = TableView;
  V.binding('blockr.viz.table', 'table', TableView, { busy: 'drawing' });
})();
