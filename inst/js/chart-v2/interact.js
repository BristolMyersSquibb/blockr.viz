// @ts-check
/**
 * Chart block v2: clicks, the selection and what goes to R.
 *
 * A click turns the clicked mark into a filter (the family's model says
 * how, from the mark's keys), latches it or, with a ctrl_target, sends it
 * as an event. The selection is one filter, {type, filters}; the footer and
 * every clear read it. Methods of the chart view, like chrome.js.
 *
 * A latched filter dims the marks it does not light (D5): the family's
 * option module builds the patch from each mark's keys.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});

  // Event handlers per family, attached once per ECharts instance. They read
  // the view's live state, never values captured at draw time, because an
  // instance outlives many draws.
  const handlers = {
    // A click on a bar segment, slice, tile, radar shape, box or point range
    // sends its mark's keys (D2, D3), or the drill column's values in the
    // rows under it.
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

  const interact = {
    /** @this {any} @param {any} slot @param {string} family */
    _attachHandlers(slot, family) {
      const h = /** @type {Record<string, any>} */ (handlers)[family];
      if (h) h(this, slot);
    },

    // A click always sends (D1): latched, the filter becomes the selection;
    // with a ctrl_target it is an event and nothing latches.
    /** @this {any} @param {any} slot @param {any} params @param {Record<string, any[]>} filters */
    _select(slot, params, filters) {
      if (NS.transientDrill(this.config)) {
        this._flashMark(slot, params);
        this._showReceipt(filters);
        this._sendFilter(filters);
        return;
      }
      this._filter = { type: 'categorical', filters };
      this._updateHighlight();
      this._sendFilter(filters);
    },

    /** @this {any} @param {Record<string, any[]>} filters */
    _sendFilter(filters) {
      if (!this.el.id) return;
      // The counter tells R a second click on the same mark from a board
      // re-evaluation, which would otherwise look the same.
      Shiny.setInputValue(this.el.id + '_action',
        NS.keys.filterMessage(filters, ++this._drillSeq), { priority: 'event' });
    },

    // Rows a mark stands for, filtered on the drill column.
    /** @this {any} @param {any[]} rows */
    _emitDrill(rows) {
      const c = NS.drillColumn(this.config);
      if (!c) return;
      const filters = NS.keys.fromRows(c, rows);
      if (filters) this._sendFilter(filters);
    },

    // The type of what a clear clears (B1); with nothing latched, the type
    // v1 sends: categorical on an aggregated chart, else range.
    /** @this {any} */
    _clearType() {
      if (this._filter) return this._filter.type;
      return this._family() === 'aggregated' ? 'categorical' : 'range';
    },

    /** @this {any} @param {'categorical' | 'range' | 'point'} type */
    _sendClearFilter(type) {
      if (!this.el.id) return;
      Shiny.setInputValue(this.el.id + '_action', NS.keys.clearMessage(type),
                          { priority: 'event' });
    },

    // The gear dropped the filter (a mapping or drill change): the footer
    // says so at once (D4), then R hears it.
    /** @this {any} */
    _gearClearFilter() {
      const type = this._clearType();
      this._filter = null;
      this._hasBrushFilter = false;
      this._updateStatus();
      this._sendClearFilter(type);
    },

    /** @this {any} */
    _reset() {
      const type = this._clearType();
      this._filter = null;
      this._hasBrushFilter = false;
      this._clearBrush();
      this._updateHighlight();
      this._sendClearFilter(type);
    },

    /** @this {any} */
    _clearBrush() {
      for (const chart of this.charts) chart.dispatchAction({ type: 'brush', areas: [] });
    },

    /** The footer, then the marks. @this {any} */
    _updateHighlight() {
      this._updateStatus();
      this._applyHighlight();
    },

    // The selection as opacities, one patch per panel and nothing redrawn
    // (D5): a mark is lit when its keys match the filter, or, for a filter
    // on a column it does not carry, when one of its rows does.
    /** @this {any} */
    _applyHighlight() {
      const fam = this._family();
      const m = this._memo.model;
      // A brush shows its own area; only a click or a restored filter dims.
      const f = this._filter && this._filter.type === 'categorical' && !this._filter.brush
        ? this._filter.filters : null;
      const lit = f
        ? (/** @type {Record<string, any>} */ keys, /** @type {any} */ rows) => NS.keys.markLit(keys,
            rows || (() => NS.model.rowsUnder(this.data, this._ix, keys)), f)
        : null;
      this._slots.forEach((/** @type {any} */ slot, /** @type {number} */ i) => {
        if (!slot || !slot.chart) return;
        const chart = slot.chart;
        const current = chart.getOption();
        if (!current || !current.series) return;
        const panel = m && m.family === fam ? m.panels[i] : null;
        /** @type {{ series: any[], dimmed: boolean }} */
        let patch;
        if (panel && fam === 'aggregated') {
          patch = NS.option.aggregatedPatch(m, panel, this.config, current, lit, !!slot.dimmed);
        } else if (panel && fam === 'timeline') {
          patch = NS.option.timelinePatch(m, panel, this.config, lit, !!slot.dimmed);
        } else if (panel && fam === 'individual') {
          // The series the panel was drawn with, unnamed ones unnamed.
          const o = this._memo.option && this._memo.option.panels[i];
          const drawn = o && o.option && o.option.series && o.option.series.length === current.series.length
            ? o.option.series : current.series;
          patch = NS.option.individualPatch(m, panel, drawn, lit, !!slot.dimmed, f);
        } else {
          patch = { series: current.series.map(() => ({})), dimmed: false };
        }
        slot.dimmed = patch.dimmed;
        chart.setOption({ series: patch.series }, false);
      });
    },

    // -- Transient drill signal --------------------------------------------

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
          const box = document.createElement('span');
          box.className = 'dd-drill-flash';
          box.style.left = r.x + 'px';
          box.style.top = r.y + 'px';
          box.style.width = r.width + 'px';
          box.style.height = r.height + 'px';
          // The mark's own corners, so the trace sits on it.
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
      const ring = document.createElement('span');
      ring.className = 'dd-drill-pulse';
      ring.style.left = x + 'px';
      ring.style.top = y + 'px';
      this._mount(host, ring, 1200);
    },

    // A self-removing signal; the timeout covers an off-screen block, where
    // animationend never fires.
    /** @this {any} @param {HTMLElement} host @param {HTMLElement} el @param {number} ms */
    _mount(host, el, ms) {
      const drop = () => {
        if (el.parentNode) el.parentNode.removeChild(el);
        if (this._signalEl === el) this._signalEl = null;
      };
      el.addEventListener('animationend', drop);
      this._signalEl = el;
      host.appendChild(el);
      setTimeout(drop, ms);
    },

    /** @this {any} */
    _clearSignal() {
      const el = this._signalEl;
      if (el && el.parentNode) el.parentNode.removeChild(el);
      this._signalEl = null;
    }
  };

  NS.interact = interact;
  NS.interactHandlers = handlers;
})();
