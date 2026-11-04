// @ts-check
/**
 * Chart block: the selection and what goes to R. The view owns one.
 *
 * A click always sends. Latched, the clicked mark's filter becomes the
 * selection and a second click on the same mark clears it; with a
 * drill_target a click, a point or a brush is an event for another block
 * (drill()): nothing latches, the mark gets a trace that fades, and the
 * footer shows a receipt of it for a moment. A second click on the mark
 * just sent clears it. A latched selection dims the
 * marks it does not light; the view's highlight() draws that.
 *
 * The selection is kept in the shape R reads (chart-keys.js). A latched brush
 * on a scatter is the range of the brushed points; ECharts clears a brush by
 * itself on some redraws, so a clear that did not come from the reader is
 * told apart (`suppressBrushClear`, `byBrush`).
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  // How long a redraw's brush clear is ignored: ECharts reports it on the
  // draw or the frame after.
  const REDRAW_MS = 150;

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
      /** The last drill sent to a target, for a second click to take back.
       * @type {ChartSelection | null} */
      this.sent = null;
      /** The last peek sent, as JSON (peek()). */
      this._peeked = 'null';
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
        // A second click on the mark just sent takes it back: the data view
        // lets go and the target is cleared.
        const s = this.sent;
        if (s && s.type === 'categorical' && sameFilters(s.filters, filters)) {
          this.sent = null;
          v.frame.flash('Drill cleared');
          this.send(null);
          return;
        }
        this.drill({ type: 'categorical', filters }, () => this.flash(slot, params));
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
     * A drill sent to another block: the mark's light, the footer's receipt,
     * the selection once. Nothing latches.
     * @param {ChartSelection} sel @param {() => void} light
     */
    drill(sel, light) {
      this.sent = sel;
      light();
      this.view.frame.flash('Drilled down to ' + this.describe(sel));
      this.send(sel);
    }

    /**
     * A point click or a brush, latched: a chart without a drill_target. A
     * second click on the same point clears it, as on a bar.
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

    /** A redraw empties the brush before the family draws it again, and
     * ECharts reports that as a clear. For a moment, it is not the reader's:
     * with a drill target it would release the claim the brush sent. */
    redrawing() {
      this.suppressBrushClear = true;
      this.view.scope.timeout(() => { this.suppressBrushClear = false; }, REDRAW_MS);
    }

    /** The brush was cleared: so is the selection, unless a click just made
     * it, drill is off, where the brush only peeked (the peek goes), or the
     * brush was a drill, which the target owns from then on. */
    brushCleared() {
      if (this.suppressBrushClear) return;
      if (NS.drillState(this.view.settings) === 'off') {
        this.peek(null);
        return;
      }
      if (NS.transientDrill(this.view.settings)) return;
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
      this.sent = null;
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

    /**
     * A brush on a chart with drill off: it filters nothing, but the data
     * view shows what it caught. Sent as `<id>_peek`, once per change.
     * @param {ChartSelection | null} sel
     */
    peek(sel) {
      const key = JSON.stringify(sel);
      if (key === this._peeked) return;
      this._peeked = key;
      const id = this.view.el.id;
      if (id) V.util.send(id, 'peek', { selection: sel, nonce: Date.now() });
    }

    /** What R gets: `{selection, nonce}`, `selection` null for a clear.
     * @param {ChartSelection | null} sel */
    send(sel) {
      const id = this.view.el.id;
      if (id) V.util.select(id, /** @type {Record<string, unknown> | null} */ (sel));
    }

    /** The footer's words for the selection, in column labels; null for none:
     * the line shows only while something is selected. */
    words() {
      return this.current ? 'Filtered: ' + this.describe(this.current) : null;
    }

    /** A selection in column labels. @param {ChartSelection} sel */
    describe(sel) {
      const v = this.view;
      const m = v._memo.model;
      return sel.type === 'range'
        ? NS.keys.describeRange(sel, v.columns, m && m.x ? m.x.type : undefined)
        : NS.keys.describe(sel.filters, v.columns);
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
      const t = ev.target;
      const boxy = t && t.shape &&
        typeof t.shape.width === 'number' && typeof t.shape.height === 'number';
      if (boxy && t.getBoundingRect) {
        const r = t.getBoundingRect().clone();
        if (t.transform) r.applyTransform(t.transform);
        if (this.trace(slot, r, t.shape.r)) return;
      }
      const x = ev.offsetX, y = ev.offsetY;
      if (typeof x !== 'number' || typeof y !== 'number') return;
      const ring = NS.mk('span', 'vz-chart-pulse');
      Object.assign(ring.style, { left: x + 'px', top: y + 'px' });
      this._light(host, ring, 1200);
    }

    /**
     * The trace a clicked bar gets, on a box in the slot's pixels: a bar, or
     * the rectangle a drill brush took. False when the box is empty.
     * @param {ChartSlot | null} slot
     * @param {{ x: number, y: number, width: number, height: number }} r
     * @param {number | number[]} [rad] the mark's corner radius
     */
    trace(slot, r, rad) {
      const host = slot && slot.chartDiv;
      if (!host || !(r.width > 0 && r.height > 0)) return false;
      const box = NS.mk('span', 'vz-chart-flash');
      Object.assign(box.style, { left: r.x + 'px', top: r.y + 'px',
                                 width: r.width + 'px', height: r.height + 'px' });
      if (rad != null) {
        box.style.borderRadius = (Array.isArray(rad) ? rad : [rad])
          .map((/** @type {number} */ x) => x + 'px').join(' ');
      }
      this._light(host, box, 2000);
      return true;
    }

    /** A light that removes itself; the timeout covers an off-screen block.
     * @param {HTMLElement} host @param {HTMLElement} el @param {number} ms */
    _light(host, el, ms) {
      const scope = this.view.scope;
      const drop = () => {
        el.remove();
        if (this._signal === el) this._signal = null;
      };
      if (this._signal) this._signal.remove();
      scope.on(el, 'animationend', drop);
      this._signal = el;
      host.appendChild(el);
      scope.timeout(drop, ms);
    }
  }

  NS.Selection = Selection;
})();
