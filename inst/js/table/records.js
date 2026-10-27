// @ts-check
/**
 * The records layout: the input rows as a table (dom.js), or as cards with
 * `look = "cards"` (cards.js), with the drill (drill.js).
 *
 * R sends the table in `payload.data` (a TableModel), so a payload whose
 * data revision is unchanged (a drill, a title edit) reuses the drawn table
 * and keeps its sort, folds and scroll. `config.selection` is R's record of
 * the drill; a click shows at once and R's next payload confirms it.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const T = V.table;

  /**
   * A selection from the config, if it has the shape of one.
   * @param {unknown} x
   * @returns {TableSelection | null}
   */
  function asSelection(x) {
    const s = /** @type {any} */ (x);
    return s && typeof s === 'object' && s.filters && typeof s.filters === 'object' ? s : null;
  }

  /** @param {number} shown @param {number} total */
  function countText(shown, total) {
    if (!total) return '';
    const noun = total === 1 ? ' row' : ' rows';
    return shown === total ? total + noun : shown + ' of ' + total + noun;
  }

  class Records {
    /**
     * @param {TableFrame} frame
     * @param {VizScope} scope
     */
    constructor(frame, scope) {
      this.frame = frame;
      this.scope = scope;
      this.grid = new T.Grid(frame.box, scope);
      this.cardsHost = scope.node(document.createElement('div'));
      this.cardsHost.className = 'blockr-cards-host';
      this.cardsHost.hidden = true;
      frame.box.appendChild(this.cardsHost);
      /** @type {TableModel | null} */
      this.model = null;
      /** @type {VizPayload | null} */
      this.payload = null;
      /** The selection shown: R's, or a click R has not answered. */
      this.sel = /** @type {TableSelection | null} */ (null);
      /** The last claim sent to a drill target. */
      this.sent = /** @type {TableSelection | null} */ (null);
      this.drawnWith = '';
      /** @type {Record<string, string>} */
      this.labels = {};
      this.drill = T.drill.wire(this.grid, frame, scope, {
        model: () => this.model,
        selection: () => this.sel,
        select: (s) => {
          this.sel = s;
          this.repaint();
        },
        sent: (s) => {
          this.sent = s;
          this.footer();
        },
        transient: () => this.transient()
      });
      this.grid.hooks.drawn = () => {
        this.drill.paint();
        this.footer();
      };
      frame.onSearch((q) => {
        if (this.cardsOn()) this.drawCards();
        else this.grid.search(q);
      });
      scope.on(this.cardsHost, 'click', (e) => {
        const t = /** @type {Element | null} */ (e.target);
        const row = t && t.closest ? t.closest('.tc-row[data-i]') : null;
        if (row) this.drill.clickRow(Number(row.getAttribute('data-i')), row);
      });
    }

    /** @returns {TableConfig} */
    config() {
      return /** @type {TableConfig} */ (/** @type {unknown} */ (
        this.payload ? this.payload.config : {}));
    }

    transient() {
      const t = this.config().drill_target;
      return !!(t && String(t).trim());
    }

    cardsOn() {
      return !!this.model && this.config().look === 'cards' && !!this.model.cards;
    }

    /** @param {VizPayload} p */
    render(p) {
      this.payload = p;
      const cfg = this.config();
      const d = /** @type {TableDerived} */ (p.derived || {});
      const m = /** @type {TableModel | null} */ (/** @type {unknown} */ (p.data || null));
      this.labels = Object.fromEntries((p.columns || []).map((c) => [c.name, c.label || c.name]));
      this.sel = asSelection(cfg.selection);
      if (!this.transient()) this.sent = null;
      const f = this.frame;
      f.titles(d.titles);
      const has = !!m && !d.empty;
      f.showTools({ search: has && cfg.search !== false, download: has && cfg.download !== false,
                    downloads: d.downloads });
      if (f.empty(d.empty) || !m) {
        this.model = null;
        this.drawnWith = '';
        this.grid.clear();
        this.cardsHost.hidden = true;
        f.count('');
        f.receipt(null, null);
        return;
      }
      f.structured(!!(m.sections && m.sections.length) || !!(m.rows && m.rows.keys));
      this.draw(m, cfg);
    }

    /**
     * Draw the model afresh when it or how it is drawn changed, else repaint
     * the drill only.
     * @param {TableModel} m
     * @param {TableConfig} cfg
     */
    draw(m, cfg) {
      const how = [cfg.look, cfg.sortable, cfg.collapsible].join();
      if (m === this.model && how === this.drawnWith) {
        this.repaint();
        return;
      }
      this.model = m;
      this.drawnWith = how;
      if (this.cardsOn()) {
        this.grid.clear();
        this.cardsHost.hidden = false;
        this.drawCards();
        return;
      }
      this.cardsHost.hidden = true;
      this.cardsHost.innerHTML = '';
      this.grid.query = this.frame.query();
      this.grid.set(m, { sortable: cfg.sortable !== false, collapsible: cfg.collapsible !== false });
    }

    /** The search applied to the card rows. */
    cardOrder() {
      const m = /** @type {TableModel} */ (this.model);
      const n = Math.min(m.n, m.cards ? m.cards.n : 0);
      const q = this.frame.query();
      const all = Array.from({ length: n }, (_, i) => i);
      return q ? all.filter((i) => T.rowText(m, i).includes(q)) : all;
    }

    drawCards() {
      const m = /** @type {TableModel} */ (this.model);
      const order = this.cardOrder();
      T.cards.render(this.cardsHost, /** @type {TableCards} */ (m.cards), this.scope,
                     { order, rowClass: (i) => this.drill.rowClass(i) });
      this.footer();
    }

    /** Show the selection: lit rows and columns, and the footer. */
    repaint() {
      if (!this.model) return;
      if (this.cardsOn()) this.drawCards();
      else this.grid.draw();
    }

    /** The row count and the receipt line. */
    footer() {
      const m = this.model;
      const f = this.frame;
      if (!m) return;
      const structured = !!(m.sections && m.sections.length) || !!(m.rows && m.rows.indent);
      const c = this.cardsOn() ? { shown: this.cardOrder().length, total: m.n }
        : this.grid.counts();
      f.count(structured ? '' : countText(c.shown, c.total));
      if (this.transient()) {
        f.receipt(this.sent ? 'Drilled down to ' + T.drill.label(this.sent, this.labels) : null,
                  null);
        return;
      }
      const sel = this.sel;
      if (!sel) {
        f.receipt(null, null);
        return;
      }
      const d = /** @type {TableDerived} */ (this.payload ? this.payload.derived : {});
      const fromR = T.drill.same(sel, asSelection(this.config().selection)) && d.receipt;
      f.receipt(fromR || 'Filtered: ' + T.drill.label(sel, this.labels), () => {
        V.util.send(f.id, 'select', { selection: null, nonce: Date.now() });
        this.sel = null;
        this.repaint();
      });
    }

    dispose() {
      this.grid.clear();
    }
  }

  T.layouts.records = Records;
})();
