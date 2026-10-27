// @ts-check
/**
 * Chart block: everything around the canvases.
 *
 * The card: a header with the titles, the download control and the gear
 * (core/gear, in a tray under the header), the prepare script's strip, the
 * panel grid, the legend band, the caption and the status footer, which
 * names the filter a click set and clears it. These are methods of the
 * chart view (view.js copies them onto its prototype); they read the view's
 * state and decide nothing about the picture.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});
  // How long the footer holds a drill receipt, and how long it takes to
  // leave. The receipt's age drives the animation, so a footer rebuilt
  // while the receipt is on screen resumes rather than restarts it.
  const RECEIPT_HOLD_MS = 1400;
  const RECEIPT_FADE_MS = 1100;
  // Backstop for an off-screen block, where animationend never fires.
  const RECEIPT_GRACE_MS = 600;
  const now = () => (typeof performance !== 'undefined' && performance.now)
    ? performance.now() : Date.now();

  /** @param {string} tag @param {string} cls @param {HTMLElement} [parent] */
  const mk = (tag, cls, parent) => {
    const e = document.createElement(tag);
    e.className = cls;
    if (parent) parent.appendChild(e);
    return e;
  };

  const chrome = {
    /** @this {any} */
    _buildDOM() {
      this.el.innerHTML = '';
      this.card = mk('div', 'dd-card', this.el);
      // The header: titles on the left, then the tools and the gear; open,
      // the tray sits under them and pushes the chart down.
      const head = this.gearHeader = mk('div', 'blockr-gear-header vz-head', this.card);
      this.titleWrap = mk('div', 'dd-chart-titles vz-head__titles', head);
      this.titleEl = mk('div', 'dd-chart-title', this.titleWrap);
      this.subtitleEl = mk('div', 'dd-chart-subtitle', this.titleWrap);
      this.toolsEl = mk('div', 'vz-head__tools', head);
      this.downloadEl = mk('div', 'dd-chart-dl', this.toolsEl);
      mk('div', 'blockr-busy-line', this.card).setAttribute('aria-live', 'polite');
      this.bandEl = mk('div', 'dd-mapping-band', this.card);
      this.bandEl.style.display = 'none';
      this.chartGrid = mk('div', 'dd-chart-grid', this.card);
      this.legendEl = mk('div', 'dd-legend-band', this.card);
      this.legendEl.style.display = 'none';
      this.captionEl = mk('div', 'dd-chart-caption', this.card);
      this.statusEl = mk('div', 'dd-status-footer', this.card);
      this.clearBtn = /** @type {HTMLButtonElement} */ (mk('button', 'dd-status-clear'));
      this.clearBtn.type = 'button';
      this.clearBtn.textContent = '×';
      this.clearBtn.setAttribute('aria-label', 'Clear filter');
      this.clearBtn.setAttribute('data-blockr-tooltip', 'Clear filter');
      this.scope.on(this.clearBtn, 'click', () => this._reset());
      this._mountGear(head);
      this._updateStatus();
      this._updateTitles(null);
    },

    // The core gear engine in a tray under the header, and the sentence
    // whose words open their setting. Without the engine there is neither.
    /** @this {any} @param {HTMLElement} head */
    _mountGear(head) {
      const G = V.gear;
      if (!G || typeof G.mount !== 'function') return;
      const t = this._tray = G.tray(head, this.scope, 'Chart settings');
      this.popoverEl = t.band;
      this.gearBtn = t.button;
      this._gear = this.scope.own(G.mount(t.band, {
        block: 'chart', id: this.el.id, scope: this.scope, typeKey: 'type',
        title: 'Chart settings', host: NS.gear.host(this)
      }));
      if (V.titles && typeof V.titles.slots === 'function') {
        this._slots_ = this.scope.own(V.titles.slots({
          gear: this._gear, scope: this.scope, openGear: () => t.set(true)
        }));
      }
    },

    // The title, subtitle and caption R resolved, their words live where the
    // gear has a control for them; hidden while empty.
    /** @this {any} @param {any} titles */
    _updateTitles(titles) {
      const x = titles || {};
      /** @param {any} b */
      const band = (b) => (typeof b === 'string' ? { text: b } : b || null);
      /** @param {any} b */
      const shows = (b) => !!b && (!!b.text || (Array.isArray(b.offers) && b.offers.length > 0));
      const [t, s, c] = [band(x.title), band(x.subtitle), band(x.caption)];
      const paint = (/** @type {HTMLElement} */ e, /** @type {any} */ b) => {
        if (this._slots_) this._slots_.paint(e, b);
        else e.textContent = b && b.text ? b.text : '';
        e.style.display = shows(b) ? '' : 'none';
      };
      paint(this.titleEl, t);
      paint(this.subtitleEl, s);
      paint(this.captionEl, c);
      this.titleWrap.style.display = shows(t) || shows(s) ? '' : 'none';
    },

    // The formats R offers, as core/download.js draws them. Reaching for
    // the control composes the picture and sends it to R, so it is there by
    // the time a format is picked.
    /** @this {any} @param {any} list */
    _updateDownloads(list) {
      const show = this.config.download !== 'off' && Array.isArray(list) && list.length > 0;
      this.downloadEl.hidden = !show;
      if (typeof V.downloads === 'function') {
        V.downloads(this.downloadEl, show ? list : [], this.scope,
                    { onOpen: () => this._downloadImage(true) });
      }
    },

    // -- Legend band ---------------------------------------------------------

    // One row of chips for every panel. A chip filters its level, as a
    // click on a mark of that level would (D8): the other levels dim, a
    // second click sends again (D1), Reset clears. The swatch follows the
    // chart's mark; the band sits below the plot or above it
    // (`plan`, from NS.legendPlan), and is gone when the names are on the marks.
    /** @this {any}
     *  @param {{col: string, items: Array<{name: string, color: string}>} | null} spec
     *  @param {{ position?: string, swatch?: string }} [plan] */
    _updateLegendBand(spec, plan) {
      const el = this.legendEl;
      if (!el) return;
      if (!spec || !spec.items.length) {
        el.style.display = 'none';
        el.innerHTML = '';
        this._legendCol = null;
        return;
      }
      this._legendCol = spec.col;
      this._placeLegendBand(plan && plan.position === 'top' ? 'top' : 'bottom');

      el.innerHTML = '';
      if (this._chipScope) this._chipScope.dispose();
      this._chipScope = this.scope.own(new V.Scope());
      const swatch = (plan && plan.swatch) || 'square';
      const title = this._legendTitleName(spec.items, spec.col);
      if (title) {
        const t = document.createElement('span');
        t.className = 'dd-legend-title';
        t.textContent = title;
        el.appendChild(t);
      }
      for (const it of spec.items) {
        const chip = document.createElement('button');
        chip.type = 'button';
        chip.className = 'dd-legend-chip';
        chip.dataset.level = it.name;
        const sw = document.createElement('span');
        sw.className = 'dd-legend-swatch dd-legend-swatch--' + swatch;
        sw.style.background = it.color;
        const lb = document.createElement('span');
        lb.textContent = it.name;
        chip.appendChild(sw);
        chip.appendChild(lb);
        this._chipScope.on(chip, 'click', () => this._legendChipClick(spec.col, it.name));
        el.appendChild(chip);
      }
      el.style.display = '';
      this._markLegendChips();
    },

    // The band under the grid, or over it, between the title band and the
    // plot.
    /** @this {any} @param {'top' | 'bottom'} position */
    _placeLegendBand(position) {
      const el = this.legendEl;
      if (!el || !this.chartGrid) return;
      el.classList.toggle('dd-legend-band--top', position === 'top');
      const inPlace = position === 'top'
        ? el.nextElementSibling === this.chartGrid
        : el.previousElementSibling === this.chartGrid;
      if (inPlace) return;
      if (position === 'top') this.card.insertBefore(el, this.chartGrid);
      else this.card.insertBefore(el, this.chartGrid.nextSibling);
    },

    // A chip click: its level as a filter, or with an explicit drill column
    // that column's values in the level's rows. Nothing with drill off.
    /** @this {any} @param {string} col @param {string} level */
    _legendChipClick(col, level) {
      const cfg = this.config;
      if (NS.drillState(cfg) === 'off') return;
      const filters = cfg.drill === 'auto'
        ? NS.keys.fromKeys({ [col]: level })
        : NS.keys.fromRows(cfg.drill, NS.model.rowsUnder(this.data, this._ix, { [col]: level }));
      if (filters) this._select(null, null, filters);
    },

    // The chip whose level the latched filter selects is marked.
    /** @this {any} */
    _markLegendChips() {
      const el = this.legendEl;
      if (!el || !this._legendCol) return;
      const f = this._filter && this._filter.filters;
      const vals = f && f[this._legendCol];
      const on = vals ? new Set(vals.map((/** @type {any} */ v) => (NS.keys.isMissing(v) ? '' : String(v))))
        : null;
      for (const chip of el.querySelectorAll('.dd-legend-chip')) {
        const c = /** @type {HTMLElement} */ (chip);
        c.classList.toggle('dd-legend-chip-on', !!on && on.has(c.dataset.level || ''));
      }
    },

    // The colour column's label before the chips: only for a column that has
    // one (a name alone says nothing the levels do not), and not when a
    // level already carries that name.
    /** @this {any} @param {any[]} items @param {string} col */
    _legendTitleName(items, col) {
      const meta = NS.axes.colMeta(this.columns, col);
      const title = meta && meta.label && meta.label !== meta.name ? meta.label : null;
      if (!title || !items || !items.length) return null;
      const taken = items.some((it) =>
        String(it != null && typeof it === 'object' ? it.name : it) === title);
      return taken ? null : title;
    },

    // -- Footer ----------------------------------------------------------------

    // The footer names the last filter sent, in column labels (D4). A live
    // drill receipt wins the line while it lasts.
    /** @this {any} */
    _updateStatus() {
      if (!this.statusEl) return;
      this.statusEl.innerHTML = '';
      this.clearBtn.hidden = true;

      if (this._receipt) {
        const age = now() - (this._receiptAt || 0);
        if (age < RECEIPT_HOLD_MS + RECEIPT_FADE_MS) {
          const rec = document.createElement('span');
          rec.className = 'dd-status-text dd-status-receipt';
          rec.textContent = this._receipt;
          // A negative delay resumes the fade where the receipt is.
          rec.style.animation = `dd-receipt-out ${RECEIPT_FADE_MS}ms linear ` +
            `${RECEIPT_HOLD_MS - age}ms both`;
          this.scope.on(rec, 'animationend', () => this._dropReceipt());
          this.statusEl.appendChild(rec);
          return;
        }
        this._receipt = null;
      }

      const f = this._filter;
      const hasFilter = !!f || this._hasBrushFilter;
      // Nothing set, nothing said: the line is there only while a filter is,
      // with a clear button after it. A cap message is a warning about the
      // picture.
      if (!hasFilter && !this._capMessage) return;

      if (hasFilter) {
        let text = 'Brush filter active';
        if (f && f.type === 'categorical') {
          text = 'Filtered: ' + NS.keys.describe(f.filters, this.columns);
        } else if (f && f.type === 'range') {
          const m = this._memo.model;
          text = 'Filtered: ' + NS.keys.describeRange(f.range, this.columns,
            m && m.x ? m.x.type : undefined, f.filters);
        }
        const span = document.createElement('span');
        span.className = 'dd-status-text' + (this._returning ? ' dd-status-returning' : '');
        span.textContent = text;
        this.statusEl.appendChild(span);
        this.statusEl.appendChild(this.clearBtn);
        this.clearBtn.hidden = false;
      }

      if (this._capMessage) {
        const cap = document.createElement('span');
        cap.className = 'dd-status-cap';
        cap.textContent = this._capMessage;
        this.statusEl.appendChild(cap);
      }
    },

    // The receipt after a transient drill: past tense, about what the reader
    // did, in the same words as the footer (D4).
    /** @this {any} @param {Record<string, any[]>} filters */
    _showReceipt(filters) {
      this._receipt = 'Drilled down to ' + NS.keys.describe(filters, this.columns);
      // The clock starts at the click; a redraw never restarts it.
      this._receiptAt = now();
      this._updateStatus();
      if (this._receiptTimer) this._receiptTimer();
      this._receiptTimer = this.scope.timeout(() => this._dropReceipt(),
        RECEIPT_HOLD_MS + RECEIPT_FADE_MS + RECEIPT_GRACE_MS);
    },

    // The resting line fades back in rather than snapping into the slot.
    /** @this {any} */
    _dropReceipt() {
      if (!this._receipt) return;
      this._receipt = null;
      this._returning = true;
      this._updateStatus();
      this._returning = false;
    }
  };

  /** The bands' plain text, for the picture.
   * @param {any} titles @returns {Record<string, string>} */
  const titleText = (titles) => {
    const x = titles || {};
    /** @type {Record<string, string>} */
    const out = {};
    for (const k of ['title', 'subtitle', 'caption']) {
      const b = x[k];
      out[k] = typeof b === 'string' ? b : (b && b.text) || '';
    }
    return out;
  };

  NS.chrome = chrome;
  NS.titleText = titleText;
})();
