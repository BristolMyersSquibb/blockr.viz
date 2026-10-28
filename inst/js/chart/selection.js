// @ts-check
/**
 * Chart block: the selection and what goes to R. The view owns one.
 *
 * A click always sends. Latched, the clicked mark's filter becomes the
 * selection and a second click on the same mark clears it; with a
 * drill_target the click is an event for another block, nothing latches,
 * and the footer shows a receipt of it for a moment. A latched selection
 * dims the marks it does not light; the view's highlight() draws that.
 *
 * The selection is kept in the shape R reads (chart-keys.js). A brush on a
 * scatter latches a range or the brushed points' series; ECharts clears a
 * brush by itself on some redraws, so a clear that did not come from the
 * reader is told apart (`suppressBrushClear`, `byBrush`).
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** The same columns with the same values, in any order.
   * @param {Record<string, any[]>} a @param {Record<string, any[]>} b */
  const sameFilters = (a, b) => {
    const norm = (/** @type {Record<string, any[]>} */ x) => JSON.stringify(
      Object.keys(x || {}).sort().map((k) => [k, (x[k] || []).map(String).sort()]));
    return norm(a) === norm(b);
  };

  class Selection {
    /** @param {ChartView} view */
    constructor(view) {
      this.view = view;
      /** What the chart has latched, in R's shape. @type {ChartSelection | null} */
      this.current = null;
      /** Whether a brush made `current`. */
      this.byBrush = false;
      /** Whether a brush rectangle may still be drawn on a panel. */
      this.brushDrawn = false;
      /** A click in brush mode also fires a brush clear: true while it is ignored. */
      this.suppressBrushClear = false;
      /** The saved selection R last sent, as JSON. @type {string | null} */
      this._saved = null;
      /** The clicked mark's light. @type {HTMLElement | null} */
      this._signal = null;
    }

    /**
     * A saved selection R sent back. Only a changed one replaces what the
     * reader sees; a chart with a drill_target restores none.
     * @param {unknown} saved
     */
    restore(saved) {
      const key = JSON.stringify(saved ?? null);
      if (key === this._saved) return;
      this._saved = key;
      this.current = NS.transientDrill(this.view.settings) ? null : NS.keys.restore(saved);
      this.byBrush = false;
    }

    /**
     * A click on marks of a level (a bar, a slice, a line, a legend chip).
     * @param {ChartSlot | null} slot @param {any} params ECharts' click, for the flash
     * @param {Record<string, any[]>} filters
     */
    click(slot, params, filters) {
      const v = this.view;
      if (NS.transientDrill(v.settings)) {
        this.flash(slot, params);
        v.frame.flash('Drilled down to ' + NS.keys.describe(filters, v.columns));
        this.send({ type: 'categorical', filters });
        return;
      }
      const c = this.current;
      if (c && c.type === 'categorical' && sameFilters(c.filters, filters)) {
        this.clear();
        return;
      }
      this.current = { type: 'categorical', filters };
      this.byBrush = false;
      v.highlight();
      this.send(this.current);
    }

    /**
     * A point click or a brush, latched, with a drill_target too. A second
     * click on the same point clears it, as on a bar.
     * @param {ChartSelection} sel @param {boolean} brush
     */
    latch(sel, brush) {
      const c = this.current;
      if (!brush && sel.type === 'range' && c && c.type === 'range' && !this.byBrush &&
          JSON.stringify(c) === JSON.stringify(sel)) {
        this.clear();
        return;
      }
      this.current = sel;
      this.byBrush = brush;
      this.brushDrawn = true;
      this.refresh();
      this.send(sel);
    }

    /** The brush was cleared: so is the selection, unless a click just made
     * it or drill is off, where the brush sent nothing. */
    brushCleared() {
      if (this.suppressBrushClear) return;
      if (NS.drillState(this.view.settings) === 'off') return;
      this.current = null;
      this.byBrush = false;
      this.brushDrawn = false;
      this.refresh();
      this.send(null);
    }

    /** The clear button, a click on empty plot space, a second click. */
    clear() {
      const v = this.view;
      this.current = null;
      this.byBrush = false;
      this.brushDrawn = false;
      for (const chart of v.charts) chart.dispatchAction({ type: 'brush', areas: [] });
      v.highlight();
      this.send(null);
    }

    /** The footer, and the marks when something dims or was dimmed. */
    refresh() {
      const v = this.view;
      if (this.current || v._slots.some((s) => s && s.dimmed)) v.highlight();
      else v.status();
    }

    /** What R gets: `{selection, nonce}`, `selection` null for a clear.
     * @param {ChartSelection | null} sel */
    send(sel) {
      const id = this.view.el.id;
      if (id) V.util.select(id, /** @type {Record<string, unknown> | null} */ (sel));
    }

    /** The footer's words for the selection, in column labels; null for none. */
    words() {
      const c = this.current;
      const v = this.view;
      if (!c) return this.brushDrawn ? 'Brush filter active' : null;
      const m = v._memo.model;
      return 'Filtered: ' + (c.type === 'range'
        ? NS.keys.describeRange(c, v.columns, m && m.x ? m.x.type : undefined)
        : NS.keys.describe(c.filters, v.columns));
    }

    /**
     * Light the clicked mark briefly: a trace on its box, or a ring at the
     * click where the mark has no box. One light at a time.
     * @param {ChartSlot | null} slot @param {any} params
     */
    flash(slot, params) {
      const host = slot && slot.chartDiv;
      const ev = params && params.event;
      if (!host || !ev) return;
      if (this._signal) this._signal.remove();
      this._signal = null;
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
              .map((/** @type {number} */ x) => x + 'px').join(' ');
          }
          this._light(host, box, 2000);
          return;
        }
      }
      const x = ev.offsetX, y = ev.offsetY;
      if (typeof x !== 'number' || typeof y !== 'number') return;
      const ring = NS.mk('span', 'dd-drill-pulse');
      Object.assign(ring.style, { left: x + 'px', top: y + 'px' });
      this._light(host, ring, 1200);
    }

    /** A light that removes itself; the timeout covers an off-screen block.
     * @param {HTMLElement} host @param {HTMLElement} el @param {number} ms */
    _light(host, el, ms) {
      const scope = this.view.scope;
      const drop = () => {
        el.remove();
        if (this._signal === el) this._signal = null;
      };
      scope.on(el, 'animationend', drop);
      this._signal = el;
      host.appendChild(el);
      scope.timeout(drop, ms);
    }
  }

  NS.Selection = Selection;
})();
