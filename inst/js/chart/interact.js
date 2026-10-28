// @ts-check
/**
 * Chart block: clicks, the selection and what goes to R. A click turns the
 * clicked mark into a filter (the family's model says how), latches it or,
 * with a drill_target, sends it as an event. A latched filter dims the marks
 * it does not light (D5), a range filter the points outside it (D11); the
 * family's option module builds the patch. Methods of the chart view.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});

  // Event handlers per family, attached once per ECharts instance. They read
  // the view's live state, since an instance outlives many draws.
  const handlers = {
    // A click on a bar segment, slice, tile, box or point range sends its
    // mark's keys (D2, D3), or the drill column's values in the rows under it.
    /** @param {any} view @param {any} slot */
    aggregated(view, slot) {
      slot.chart.on('click', (/** @type {any} */ params) => {
        const cfg = view.config;
        if (NS.drillState(cfg) === 'off') return;
        const m = view._memo.model;
        if (!m || m.family !== 'aggregated') return;
        const at = NS.model.aggregatedMarkAt(m, cfg, params);
        if (!at) return;
        const keys = NS.model.aggregatedKeys(cfg, slot.facetVal, at.group, at.level);
        const filters = NS.model.aggregatedClick(cfg, keys,
          () => NS.model.rowsUnder(view.data, view._ix, keys));
        if (!filters) return;
        view._select(slot, params, filters);
      });
    },

    /** @param {any} view @param {any} slot */
    timeline(view, slot) {
      slot.chart.on('click', (/** @type {any} */ params) => {
        if (NS.drillState(view.config) === 'off') return;
        if (params.componentType !== 'series' || !params.value) return;
        const filters = NS.model.timelineClick(view.config, slot.facetVal, params.value);
        if (!filters) return;
        view._select(slot, params, filters);
      });
    }
  };

  /** The same columns with the same values, in any order.
   * @param {Record<string, any[]>} a @param {Record<string, any[]>} b */
  const sameFilters = (a, b) => {
    const norm = (/** @type {Record<string, any[]>} */ x) => JSON.stringify(
      Object.keys(x || {}).sort().map((k) => [k, (x[k] || []).map(String).sort()]));
    return norm(a) === norm(b);
  };

  const interact = {
    /** @this {any} @param {any} slot @param {string} family */
    _attachHandlers(slot, family) {
      const h = /** @type {Record<string, any>} */ (handlers)[family];
      if (h) h(this, slot);
      // A click on empty plot space clears a latched filter, unless a line
      // is under the cursor (a click would pick it).
      slot.chart.getZr().on('click', (/** @type {any} */ e) => {
        if (e && e.target) return;
        if (slot.focus && slot.focusSi != null) return;
        if (!this._filter && !this._hasBrushFilter) return;
        this._reset();
      });
    },

    // A click always sends (D1): latched, the filter becomes the selection;
    // with a drill_target it is an event and nothing latches. A second click
    // on the selected mark clears the selection.
    /** @this {any} @param {any} slot @param {any} params @param {Record<string, any[]>} filters */
    _select(slot, params, filters) {
      if (NS.transientDrill(this.config)) {
        this._flashMark(slot, params);
        this._showReceipt(filters);
        this._sendFilter(filters);
        return;
      }
      const f = this._filter;
      if (f && f.type === 'categorical' && sameFilters(f.filters, filters)) {
        this._reset();
        return;
      }
      this._filter = { type: 'categorical', filters };
      this._updateHighlight();
      this._sendFilter(filters);
    },

    /** @this {any} @param {Record<string, any[]>} filters */
    _sendFilter(filters) {
      this._sendSelection({ type: 'categorical', filters });
    },

    // What R gets for a click, a brush or a clear: `{selection, nonce}`,
    // `selection` null for a clear. The nonce tells R a second click on the
    // same mark from a board re-evaluation.
    /** @this {any} @param {Record<string, any> | null} selection */
    _sendSelection(selection) {
      if (!this.el.id) return;
      V.util.send(this.el.id, 'select', { selection, nonce: ++this._drillSeq });
    },

    /** @this {any} */
    _reset() {
      this._filter = null;
      this._hasBrushFilter = false;
      for (const chart of this.charts) chart.dispatchAction({ type: 'brush', areas: [] });
      this._updateHighlight();
      this._sendSelection(null);
    },

    /** The footer, the legend chips, then the marks. @this {any} @param {boolean} [lazy] */
    _updateHighlight(lazy) {
      this._updateStatus();
      this._markLegendChips();
      this._applyHighlight(lazy);
    },

    // The selection as opacities, one patch per panel and nothing redrawn:
    // a mark is lit when its keys match the filter, or, for a filter on a
    // column it does not carry, when one of its rows does (D5). A range
    // filter dims the points outside it (D11). `lazy`: part of a draw, so
    // ECharts applies the patch with the draw's other follow-ups.
    /** @this {any} @param {boolean} [lazy] */
    _applyHighlight(lazy) {
      const fam = this._family();
      const m = this._memo.model;
      const sel = this._filter;
      const f = sel ? (sel.filters || null) : null;
      const range = sel && sel.type === 'range' ? sel.range : null;
      const lit = f
        ? (/** @type {Record<string, any>} */ keys, /** @type {any} */ rows) => NS.keys.markLit(keys,
            rows || (() => NS.model.rowsUnder(this.data, this._ix, keys)), f)
        : null;
      // A range only exists on the individual family.
      const catLit = range ? null : lit;
      this._slots.forEach((/** @type {any} */ slot, /** @type {number} */ i) => {
        if (!slot || !slot.chart) return;
        const chart = slot.chart;
        const current = chart.getOption();
        if (!current || !current.series) return;
        const panel = m && m.family === fam ? m.panels[i] : null;
        /** @type {{ series: any[], dimmed: boolean }} */ let patch;
        if (panel && fam === 'aggregated') {
          patch = NS.option.aggregatedPatch(m, panel, this.config, current, catLit, !!slot.dimmed);
        } else if (panel && fam === 'timeline') {
          patch = NS.option.timelinePatch(m, panel, this.config, catLit, !!slot.dimmed);
        } else if (panel && fam === 'individual') {
          const o = this._memo.option && this._memo.option.panels[i];
          const drawn = o && o.option && o.option.series && o.option.series.length === current.series.length
            ? o.option.series : current.series;
          patch = NS.option.individualPatch(m, panel, drawn, lit, !!slot.dimmed, f, range);
        } else {
          patch = { series: current.series.map(() => ({})), dimmed: false };
        }
        slot.dimmed = patch.dimmed;
        chart.setOption({ series: patch.series }, lazy ? { lazyUpdate: true } : false);
      });
    },

    // Light the clicked mark briefly: a trace on its box, or a ring at the
    // click where the mark has no box. One signal at a time.
    /** @this {any} @param {any} slot @param {any} params */
    _flashMark(slot, params) {
      const host = slot && slot.chartDiv;
      const ev = params && params.event;
      if (!host || !ev) return;
      this._clearSignal();
      const t = ev.target;
      const boxy = t && t.shape &&
        typeof t.shape.width === 'number' && typeof t.shape.height === 'number';
      if (boxy && t.getBoundingRect) {
        const r = t.getBoundingRect().clone();
        if (t.transform) r.applyTransform(t.transform);
        if (r.width > 0 && r.height > 0) {
          const box = NS.mk('span', 'dd-drill-flash');
          Object.assign(box.style, { left: r.x + 'px', top: r.y + 'px',
                                     width: r.width + 'px', height: r.height + 'px' });
          const rad = t.shape.r;
          if (rad != null) {
            box.style.borderRadius = (Array.isArray(rad) ? rad : [rad])
              .map((/** @type {number} */ v) => v + 'px').join(' ');
          }
          this._mount(host, box, 2000);
          return;
        }
      }
      const x = ev.offsetX, y = ev.offsetY;
      if (typeof x !== 'number' || typeof y !== 'number') return;
      const ring = NS.mk('span', 'dd-drill-pulse');
      Object.assign(ring.style, { left: x + 'px', top: y + 'px' });
      this._mount(host, ring, 1200);
    },

    // A self-removing signal; the timeout covers an off-screen block.
    /** @this {any} @param {HTMLElement} host @param {HTMLElement} el @param {number} ms */
    _mount(host, el, ms) {
      const drop = () => {
        el.remove();
        if (this._signalEl === el) this._signalEl = null;
      };
      this.scope.on(el, 'animationend', drop);
      this._signalEl = el;
      host.appendChild(el);
      this.scope.timeout(drop, ms);
    },

    /** @this {any} */
    _clearSignal() {
      if (this._signalEl) this._signalEl.remove();
      this._signalEl = null;
    }
  };

  NS.interact = interact;
  NS.interactHandlers = handlers;
})();
