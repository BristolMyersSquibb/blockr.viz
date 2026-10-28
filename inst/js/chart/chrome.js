// @ts-check
/**
 * Chart block: everything around the canvases. The card: a header with the
 * titles, the download control and the gear (a tray under the header), the
 * panel grid, the legend band, the caption and the status footer. Methods of
 * the chart view (view.js mixes them in); they decide nothing about the picture.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});
  // How long the footer holds a drill receipt, and how long it takes to
  // leave. The receipt's age drives the animation, so a rebuilt footer
  // resumes it. The grace is a backstop for an off-screen block, where
  // animationend never fires.
  const RECEIPT_HOLD_MS = 1400;
  const RECEIPT_FADE_MS = 1100;
  const RECEIPT_GRACE_MS = 600;
  const mk = NS.mk;

  const chrome = {
    /** @this {any} */
    _buildDOM() {
      this.el.innerHTML = '';
      this.card = mk('div', 'dd-card', this.el);
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

    // The gear engine in a tray under the header, and the title sentences whose words open it.
    /** @this {any} @param {HTMLElement} head */
    _mountGear(head) {
      const t = this._tray = V.gear.tray(head, this.scope, 'Chart settings');
      this.popoverEl = t.band;
      this.gearBtn = t.button;
      this._gear = this.scope.own(V.gear.mount(t.band, {
        block: 'chart', id: this.el.id, scope: this.scope, typeKey: 'type',
        title: 'Chart settings', host: NS.gear.host(this)
      }));
      this._slots_ = this.scope.own(V.titles.slots({
        gear: this._gear, scope: this.scope, openGear: () => t.set(true)
      }));
    },

    // The title, subtitle and caption R resolved ({text, parts, offers} or
    // null), hidden while empty.
    /** @this {any} @param {any} titles */
    _updateTitles(titles) {
      const x = titles || {};
      /** @param {any} b */
      const shows = (b) => !!b && (!!b.text || (Array.isArray(b.offers) && b.offers.length > 0));
      const [t, s, c] = [x.title, x.subtitle, x.caption];
      const paint = (/** @type {HTMLElement} */ e, /** @type {any} */ b) => {
        this._slots_.paint(e, b);
        e.style.display = shows(b) ? '' : 'none';
      };
      paint(this.titleEl, t);
      paint(this.subtitleEl, s);
      paint(this.captionEl, c);
      this.titleWrap.style.display = shows(t) || shows(s) ? '' : 'none';
    },

    // The formats R offers (core/download.js). Opening the control sends the
    // picture to R; an empty state has none, so Image is left out.
    /** @this {any} @param {any} list */
    _updateDownloads(list) {
      const offer = (Array.isArray(list) ? list : [])
        .filter((/** @type {any} */ f) => !(this._blank && f.ext === 'png'));
      const show = this.config.download !== false && offer.length > 0;
      this.downloadEl.hidden = !show;
      V.downloads(this.downloadEl, show ? offer : [], this.scope,
                  { onOpen: () => this._downloadImage(true) });
    },

    // One row of chips for every panel. A chip filters its level, as a click
    // on a mark of that level would (D8). The swatch follows the chart's
    // mark; the band sits below or above the plot (`plan`, NS.legendPlan).
    /** @this {any}
     *  @param {{col: string, items: Array<{name: string, color: string}>} | null} spec
     *  @param {{ position: 'top' | 'bottom', swatch: string }} plan */
    _updateLegendBand(spec, plan) {
      const el = this.legendEl;
      if (!spec || !spec.items.length) {
        el.style.display = 'none';
        el.innerHTML = '';
        this._legendCol = null;
        return;
      }
      this._legendCol = spec.col;
      const top = plan.position === 'top';
      el.classList.toggle('dd-legend-band--top', top);
      const inPlace = top ? el.nextElementSibling === this.chartGrid
        : el.previousElementSibling === this.chartGrid;
      if (!inPlace) this.card.insertBefore(el, top ? this.chartGrid : this.chartGrid.nextSibling);
      el.innerHTML = '';
      if (this._chipScope) this._chipScope.dispose();
      this._chipScope = this.scope.own(new V.Scope());
      // The colour column's label before the chips: only a label that differs from the name,
      // and not when a level already carries it.
      const meta = NS.axes.colMeta(this.columns, spec.col);
      const title = meta && meta.label && meta.label !== meta.name ? meta.label : null;
      if (title && !spec.items.some((it) => String(it.name) === title)) {
        mk('span', 'dd-legend-title', el).textContent = title;
      }
      for (const it of spec.items) {
        const chip = /** @type {HTMLButtonElement} */ (mk('button', 'dd-legend-chip', el));
        chip.type = 'button';
        chip.dataset.level = it.name;
        const sw = mk('span', 'dd-legend-swatch dd-legend-swatch--' + plan.swatch, chip);
        sw.style.background = it.color;
        mk('span', '', chip).textContent = it.name;
        this._chipScope.on(chip, 'click', () => this._legendChipClick(spec.col, it.name));
      }
      el.style.display = '';
      this._markLegendChips();
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

    /** @this {any} */
    _markLegendChips() {
      const el = this.legendEl;
      if (!this._legendCol) return;
      const f = this._filter && this._filter.filters;
      const vals = f && f[this._legendCol];
      const on = vals ? new Set(vals.map((/** @type {any} */ v) => (NS.keys.isMissing(v) ? '' : String(v))))
        : null;
      for (const chip of el.querySelectorAll('.dd-legend-chip')) {
        const c = /** @type {HTMLElement} */ (chip);
        c.classList.toggle('dd-legend-chip-on', !!on && on.has(c.dataset.level || ''));
      }
    },

    // The footer names the last filter sent, in column labels (D4). A live
    // drill receipt wins the line while it lasts.
    /** @this {any} */
    _updateStatus() {
      const bar = this.statusEl;
      bar.innerHTML = '';
      this.clearBtn.hidden = true;
      if (this._receipt) {
        const age = performance.now() - this._receiptAt;
        if (age < RECEIPT_HOLD_MS + RECEIPT_FADE_MS) {
          const rec = mk('span', 'dd-status-text dd-status-receipt', bar);
          rec.textContent = this._receipt;
          // A negative delay resumes the fade where the receipt is.
          rec.style.animation = `dd-receipt-out ${RECEIPT_FADE_MS}ms linear ` +
            `${RECEIPT_HOLD_MS - age}ms both`;
          this.scope.on(rec, 'animationend', () => this._dropReceipt());
          return;
        }
        this._receipt = null;
      }
      // The line is there only while a filter is; a cap message warns about the picture.
      const f = this._filter;
      if (f || this._hasBrushFilter) {
        const m = this._memo.model;
        const text = !f ? 'Brush filter active' : 'Filtered: ' + (f.type === 'range'
          ? NS.keys.describeRange(f.range, this.columns, m && m.x ? m.x.type : undefined, f.filters)
          : NS.keys.describe(f.filters, this.columns));
        mk('span', 'dd-status-text' + (this._returning ? ' dd-status-returning' : ''), bar)
          .textContent = text;
        bar.appendChild(this.clearBtn);
        this.clearBtn.hidden = false;
      }
      if (this._capMessage) mk('span', 'dd-status-cap', bar).textContent = this._capMessage;
    },

    // The receipt after a transient drill, in the footer's words (D4).
    /** @this {any} @param {Record<string, any[]>} filters */
    _showReceipt(filters) {
      this._receipt = 'Drilled down to ' + NS.keys.describe(filters, this.columns);
      this._receiptAt = performance.now();
      this._updateStatus();
      if (this._receiptTimer) this._receiptTimer();
      this._receiptTimer = this.scope.timeout(() => this._dropReceipt(),
        RECEIPT_HOLD_MS + RECEIPT_FADE_MS + RECEIPT_GRACE_MS);
    },

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
  const titleText = (titles) => Object.fromEntries(['title', 'subtitle', 'caption']
    .map((k) => [k, (titles && titles[k] && titles[k].text) || '']));

  NS.chrome = chrome;
  NS.titleText = titleText;
})();
