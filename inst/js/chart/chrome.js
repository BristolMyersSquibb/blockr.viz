// @ts-check
/**
 * Chart block: everything around the canvases.
 *
 * The card (gear header, settings band, title band, panel grid, legend
 * band, caption, footer), the gear host for drilldown-config.js, the titles
 * and their sentence words, the shared legend band, and the status footer.
 * These are methods of the chart view (view.js copies them onto its
 * prototype); they read the view's state and decide nothing about the
 * picture.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});

  // How long the footer holds a drill receipt, and how long it takes to
  // leave. The receipt's age drives the animation, so a footer rebuilt
  // while the receipt is on screen resumes rather than restarts it.
  const RECEIPT_HOLD_MS = 1400;
  const RECEIPT_FADE_MS = 1100;
  // Backstop for an off-screen block, where animationend never fires.
  const RECEIPT_GRACE_MS = 600;
  const now = () => (typeof performance !== 'undefined' && performance.now)
    ? performance.now() : Date.now();

  const chrome = {
    /** @this {any} */
    _buildDOM() {
      // The mapping band is R's markup, moved into the card below; park it
      // outside before clearing so a rebuild does not destroy it.
      const parked = this.el.querySelector(':scope > .dd-card > .dd-mapping-band');
      if (parked && this.el.parentNode) this.el.parentNode.appendChild(parked);
      this.el.innerHTML = '';

      this.card = document.createElement('div');
      this.card.className = 'dd-card';
      this.el.appendChild(this.card);

      const gearHeader = document.createElement('div');
      gearHeader.className = 'blockr-gear-header';
      this.gearBtn = document.createElement('button');
      this.gearBtn.type = 'button';
      this.gearBtn.className = 'blockr-gear-btn';
      this.gearBtn.innerHTML = (typeof Blockr !== 'undefined' && Blockr.icons)
        ? Blockr.icons.gear : '⚙';
      this.gearBtn.setAttribute('data-blockr-tooltip', 'Settings');
      this.gearBtn.setAttribute('aria-label', 'Chart settings');
      this.gearBtn.setAttribute('aria-haspopup', 'dialog');
      this.gearBtn.setAttribute('aria-expanded', 'false');
      this.gearBtn.addEventListener('click', (/** @type {Event} */ e) => {
        e.stopPropagation();
        this._togglePopover();
      });
      // The download control is R's (download links are server-driven); the
      // card only decides where it sits.
      this._hoistDownload(gearHeader);
      // With capture_export, opening the download menu composes the picture
      // and sends it to R, so it is there by the time a format is picked.
      gearHeader.addEventListener('click', (/** @type {Event} */ e) => {
        const t = /** @type {Element} */ (e.target);
        if (!this.config || !this.config.capture_export || !t.closest) return;
        const host = t.closest('.blockr-action-menu__trigger, .dd-chart-dl .blockr-tool');
        if (host) this._downloadImage(true);
      }, true);
      this.gearHeader = gearHeader;
      gearHeader.appendChild(this.gearBtn);
      this.card.appendChild(gearHeader);

      // The busy cue's line (busy-cue.js writes it).
      const busyLine = document.createElement('div');
      busyLine.className = 'blockr-busy-line';
      busyLine.setAttribute('aria-live', 'polite');
      this.card.appendChild(busyLine);

      // The settings band: in flow, between the header and the chart.
      this.popoverEl = document.createElement('div');
      this.popoverEl.className = 'blockr-settings blockr-settings--beak dd-popover';
      this.card.appendChild(this.popoverEl);

      // The prepare script's control strip, under the header.
      const band = this._bandEl();
      if (band) this.card.appendChild(band);
      if (this._popoverOpen) {
        this.popoverEl.classList.add('blockr-settings--open');
        this.gearBtn.classList.add('blockr-gear-active');
        this.gearBtn.setAttribute('aria-expanded', 'true');
      }

      // Titles are HTML, once above a facet grid, on the header's left.
      this.titleWrap = document.createElement('div');
      this.titleWrap.className = 'dd-chart-titles';
      this.titleEl = document.createElement('div');
      this.titleEl.className = 'dd-chart-title';
      this.subtitleEl = document.createElement('div');
      this.subtitleEl.className = 'dd-chart-subtitle';
      this.titleWrap.appendChild(this.titleEl);
      this.titleWrap.appendChild(this.subtitleEl);
      gearHeader.insertBefore(this.titleWrap, gearHeader.firstChild);

      this.chartGrid = document.createElement('div');
      this.chartGrid.className = 'dd-chart-grid';
      this.card.appendChild(this.chartGrid);

      // One legend for every panel, under the grid; hidden while empty.
      this.legendEl = document.createElement('div');
      this.legendEl.className = 'dd-legend-band';
      this.legendEl.style.display = 'none';
      this.card.appendChild(this.legendEl);

      this.captionEl = document.createElement('div');
      this.captionEl.className = 'dd-chart-caption';
      this.card.appendChild(this.captionEl);

      this.statusEl = document.createElement('div');
      this.statusEl.className = 'dd-status-footer';
      this.card.appendChild(this.statusEl);
      this._updateStatus();
      this._updateTitles();
    },

    // R renders the download host beside the chart element; it moves into
    // the gear header.
    /** @this {any} @param {HTMLElement} gearHeader */
    _hoistDownload(gearHeader) {
      const host = this.el && this.el.parentElement
        ? /** @type {HTMLElement | null} */ (this.el.parentElement.querySelector('.dd-chart-dl-host'))
        : null;
      if (!host) return;
      host.style.display = '';
      host.classList.add('dd-chart-dl');
      gearHeader.appendChild(host);
    },

    // The prepare script's strip: in the card once moved there, else the
    // sibling R rendered.
    /** @this {any} */
    _bandEl() {
      const inCard = this.el.querySelector(':scope > .dd-card > .dd-mapping-band');
      if (inCard) return inCard;
      const host = this.el && this.el.parentNode;
      if (!host || !host.querySelector) return null;
      return host.querySelector(':scope > .dd-mapping-band');
    },

    // -- The gear ----------------------------------------------------------

    /** The host the shared gear engine renders from. @this {any} */
    _makeConfig() {
      const R = NS.roles;
      const DCfg = /** @type {typeof VizDrilldownConfig} */ (
        (typeof Blockr !== 'undefined' && Blockr.DrilldownConfig) || window.DrilldownConfig);
      return new DCfg({
        popoverEl: () => this.popoverEl,
        roles: R.ROLES,
        config: () => this.config,
        columns: () => this.columns,
        context: () => this._family(),
        currentType: () => this.config.chart_type,
        sections: () => this._sectionsSpec(),
        sectionsForFamily: (/** @type {string} */ fam) => R.FAMILY_ROLES[fam],
        secondary: R.DD_SECONDARY,
        typeKey: 'chart_type',
        typeTiles: true,
        typeIcon: (/** @type {string} */ t) => R.TYPE_ICONS[t] || '',
        // Presentation only: waterfall is a bar option, band lists under
        // Aggregated and gantt under Individual by how they treat rows.
        typeGroups: [
          { label: 'Aggregated',
            types: NS.AGGREGATED_TYPES.filter((/** @type {string} */ t) => t !== 'waterfall').concat(['band']) },
          { label: 'Individual',
            types: NS.INDIVIDUAL_TYPES.filter((/** @type {string} */ t) => t !== 'band').concat(NS.TIMELINE_TYPES) }
        ],
        familyFor: NS.familyOf,
        // drill is a capability and value is required to draw: both survive
        // a family switch.
        carryKeep: ['drill', 'value'],
        entryRequired: (/** @type {string} */ role) => role === 'value' && this._family() === 'aggregated',
        drillAutoLabel: () => {
          const fam = this._family();
          if (this.config.chart_type === 'radar') return 'Auto, the clicked shape';
          return fam === 'aggregated' ? 'Auto, the clicked group'
            : fam === 'timeline' ? 'Auto, the clicked lane'
            : this.config.chart_type === 'line' ? 'Auto, the clicked series'
            : 'Auto, the selected point';
        },
        title: 'Chart settings',
        onChange: (/** @type {string} */ key) => {
          // A pick through a sentence word is done: its menu does not stay
          // marked open until R repaints the sentence (B5).
          this._closeSlot();
          if (key === 'func') this._reconcileMetric();
          // A mapping onto a column the rows do not carry yet: R has to send
          // the data again, so hold the last picture until it does.
          if (this._mappingNeedsData()) this._awaitData = true;
          else this._render();
          this._sendConfig();
        },
        onMults: () => this._sendMults(),
        onClearFilter: () => this._gearClearFilter(),
        ensureDefaults: () => NS.model.familyDefaults(this.config, this.columns),
        afterTypeChange: () => this._updateFamilyClass(),
        isOpen: () => this._popoverOpen,
        reopen: () => this._openPopover(),
        bandEl: () => this._bandEl()
      });
    },

    /** @this {any} */
    _renderConfig() { this._cfg.render(); this._cfg.renderBand(); },

    // The family's sections; the count column only once a count surface is
    // picked, and the drill target section on every family.
    /** @this {any} */
    _sectionsSpec() {
      const base = NS.roles.FAMILY_ROLES[this._family()];
      const countOff = !this.config.count_on || this.config.count_on === 'off';
      const spec = (!countOff || !base.presentation) ? base : {
        ...base,
        presentation: base.presentation.filter((/** @type {any} */ e) =>
          (typeof e === 'string' ? e : e.role) !== 'count_col')
      };
      return { ...spec, ctrlSection: true };
    },

    /** @this {any} */
    _reconcileMetric() { NS.roles.DAgg.reconcileValue(this.config, this.columns); },

    /** Does the config name a column the rows do not carry? @this {any} */
    _mappingNeedsData() {
      const rows = this.data;
      if (!rows || !rows.length || !rows[0]) return false;
      const have = rows[0];
      return ['group', 'value', 'x', 'y', 'xend', 'series', 'color', 'facet', 'label']
        .some((k) => {
          const v = this.config[k];
          return typeof v === 'string' && v && v !== '.count' && !(v in have);
        });
    },

    /** @this {any} */
    _updateFamilyClass() {
      const fams = ['dd-family-aggregated', 'dd-family-individual', 'dd-family-timeline'];
      const cls = 'dd-family-' + this._family();
      this.el.classList.remove(...fams);
      this.el.classList.add(cls);
      if (this.popoverEl) {
        this.popoverEl.classList.remove(...fams);
        this.popoverEl.classList.add(cls);
      }
    },

    /** @this {any} */
    _togglePopover() { this._popoverOpen ? this._closePopover() : this._openPopover(); },
    /** @this {any} */
    _openPopover() {
      this.popoverEl.classList.add('blockr-settings--open');
      this._popoverOpen = true;
      this.gearBtn.classList.add('blockr-gear-active');
      this.gearBtn.setAttribute('aria-expanded', 'true');
      // The band is in flow, so the chart has less room now.
      this._resizeCharts();
    },
    /** @this {any} */
    _closePopover() {
      this.popoverEl.classList.remove('blockr-settings--open');
      this._popoverOpen = false;
      this.gearBtn.classList.remove('blockr-gear-active');
      this.gearBtn.setAttribute('aria-expanded', 'false');
      this._resizeCharts();
    },

    // What R gets after a gear edit: every setting, verbatim where null and
    // "" mean different things.
    /** @this {any} */
    _sendConfig() {
      if (!this.el.id) return;
      const c = this.config;
      /** @type {Record<string, any>} */
      const msg = {
        action: 'config',
        group: c.group,
        color: c.color || '',
        facet: c.facet || '',
        value: c.value,
        func: c.func,
        chart_type: c.chart_type,
        x: c.x || '',
        y: c.y || '',
        xend: c.xend || '',
        sort_by: c.sort_by || '',
        sort_dir: c.sort_dir || 'asc',
        orientation: c.orientation || 'horizontal',
        bar_mode: c.bar_mode || 'stacked',
        value_labels: (c.value_labels === 'on' || c.value_labels === true) ? 'on' : 'off',
        na_group: c.na_group || 'level',
        pct_of: c.pct_of || 'facet',
        baseline: c.baseline || 'zero',
        series: c.series || '',
        label: c.label || '',
        drill: c.drill || '',
        smoother: c.smoother || 'none',
        connect: c.connect || 'monotone',
        identity_line: c.identity_line || 'off',
        // "" clears every line, so these are always sent.
        value_lines: c.value_lines == null ? '' : String(c.value_lines),
        x_lines: c.x_lines == null ? '' : String(c.x_lines),
        box_points: c.box_points || 'none',
        // band_series / band_refs are R's output, never sent back.
        band_window: c.band_window || 'adaptive',
        band_size: c.band_size == null ? 45 : c.band_size,
        band_min_n: c.band_min_n == null ? 12 : c.band_min_n,
        band_id: c.band_id || '',
        ref_hi: c.ref_hi || '',
        ref_lo: c.ref_lo || '',
        count_on: c.count_on || 'off',
        count_col: c.count_col || '',
        facet_scales: c.facet_scales || 'fixed',
        facet_cols: c.facet_cols == null ? '' : String(c.facet_cols),
        download: (c.download === false || c.download === 'off') ? 'off' : 'on',
        lo: c.lo || '',
        hi: c.hi || '',
        tt_fields: Array.isArray(c.tt_fields) ? c.tt_fields : [],
        // null is auto, "" is none.
        title: c.title ?? null,
        subtitle: c.subtitle ?? null,
        caption: c.caption ?? null,
        ctrl_target: c.ctrl_target || '',
        ctrl_table: c.ctrl_table || '',
        script: c.script == null ? '' : String(c.script)
      };
      // Script control values; an emptied multi-select ships as "" so R can
      // tell it from no value at all.
      for (const spec of (c.script_inputs || [])) {
        if (!spec || !spec.key || spec.kind === 'error') continue;
        const v = c[spec.key];
        msg[spec.key] = (Array.isArray(v) && !v.length) ? '' : (v == null ? '' : v);
      }
      Shiny.setInputValue(this.el.id + '_action', msg, { priority: 'event' });
    },

    /** @this {any} */
    _sendMults() {
      if (!this.el.id) return;
      Shiny.setInputValue(this.el.id + '_action', {
        action: 'set_mults',
        line_width_mult: this.config.line_width_mult ?? 1.0,
        dot_size_mult: this.config.dot_size_mult ?? 1.0
      }, { priority: 'event' });
    },

    // -- Titles --------------------------------------------------------------

    // The title, subtitle and caption from the text R resolved, with the
    // offers for settings whose clause dropped. Hidden while empty.
    /** @this {any} */
    _updateTitles() {
      if (!this.titleWrap || !this.titleEl || !this.subtitleEl || !this.captionEl) return;
      const cfg = this.config || {};
      const t = cfg.title_resolved || '';
      const s = cfg.subtitle_resolved || '';
      const cap = cfg.caption_resolved || '';
      // A repaint replaces the node an open slot menu is anchored to.
      this._closeSlot();
      const some = (/** @type {any} */ o) => Array.isArray(o) && o.length > 0;
      const tOffered = some(cfg.title_offers);
      const sOffered = some(cfg.subtitle_offers);
      const capOffered = some(cfg.caption_offers);
      this._paintTitle(this.titleEl, t, cfg.title_parts, cfg.title_offers);
      this._paintTitle(this.subtitleEl, s, cfg.subtitle_parts, cfg.subtitle_offers);
      this.titleEl.style.display = (t || tOffered) ? '' : 'none';
      this.subtitleEl.style.display = (s || sOffered) ? '' : 'none';
      this.titleWrap.style.display = (t || s || tOffered || sOffered) ? '' : 'none';
      this._paintTitle(this.captionEl, cap, cfg.caption_parts, cfg.caption_offers);
      this.captionEl.style.display = (cap || capOffered) ? '' : 'none';
    },

    /** @this {any} @param {HTMLElement} el @param {string} text @param {any[]} [parts] @param {any[]} [offers] */
    _paintTitle(el, text, parts, offers) {
      const s = this._sentence();
      s.paint(el, text, parts);
      s.paintOffers(el, offers);
    },

    // The sentence painter, shared with the heatmap (drilldown-config.js).
    /** @this {any} */
    _sentence() {
      if (!this._slotsInst) {
        this._slotsInst = new Blockr.SentenceSlots({
          ddc: () => this._cfg,
          config: () => this.config,
          openGear: () => this._openPopover()
        });
      }
      return this._slotsInst;
    },
    /** @this {any} @param {string} key */
    _slotLabel(key) { return this._sentence().label(key); },
    /** @this {any} @param {string} key @param {HTMLElement} anchor @param {string} [by] */
    _openSlot(key, anchor, by) { this._sentence().open(key, anchor, by); },
    /** @this {any} */
    _closeSlot() { if (this._slotsInst) this._slotsInst.close(); },

    // -- Legend band ---------------------------------------------------------

    // One row of chips for every panel. A chip filters its level, as a
    // click on a mark of that level would (D8): the other levels dim, a
    // second click sends again (D1), Reset clears.
    /** @this {any}
     *  @param {{col: string, items: Array<{name: string, color: string}>} | null} spec */
    _updateLegendBand(spec) {
      const el = this.legendEl;
      if (!el) return;
      if (!spec || !spec.items.length) {
        el.style.display = 'none';
        el.innerHTML = '';
        this._legendCol = null;
        return;
      }
      this._legendCol = spec.col;

      el.innerHTML = '';
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
        sw.className = 'dd-legend-swatch';
        sw.style.background = it.color;
        const lb = document.createElement('span');
        lb.textContent = it.name;
        chip.appendChild(sw);
        chip.appendChild(lb);
        chip.addEventListener('click', () => this._legendChipClick(spec.col, it.name));
        el.appendChild(chip);
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

    // The colour column's title over the chips, unless a level already
    // carries that name.
    /** @this {any} @param {any[]} items @param {string} col */
    _legendTitleName(items, col) {
      const title = NS.axes.axisTitle(this.columns, col);
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

      if (this._receipt) {
        const age = now() - (this._receiptAt || 0);
        if (age < RECEIPT_HOLD_MS + RECEIPT_FADE_MS) {
          const rec = document.createElement('span');
          rec.className = 'dd-status-text dd-status-receipt';
          rec.textContent = this._receipt;
          // A negative delay resumes the fade where the receipt is.
          rec.style.animation = `dd-receipt-out ${RECEIPT_FADE_MS}ms linear ` +
            `${RECEIPT_HOLD_MS - age}ms both`;
          rec.addEventListener('animationend', () => this._dropReceipt());
          this.statusEl.appendChild(rec);
          return;
        }
        this._receipt = null;
      }

      const f = this._filter;
      const hasFilter = !!f || this._hasBrushFilter;
      // Transient drill with nothing latched: no line. A brush still latches,
      // and a cap message is a warning about the picture.
      const resting = !hasFilter && NS.transientDrill(this.config);
      if (resting && !this._capMessage) return;

      let text = 'No filter active';
      if (f && f.type === 'categorical') {
        text = 'Filtered: ' + NS.keys.describe(f.filters, this.columns);
      } else if (f && f.type === 'range') {
        const m = this._memo.model;
        text = 'Filtered: ' + NS.keys.describeRange(f.range, this.columns,
          m && m.x ? m.x.type : undefined, f.filters);
      } else if (this._hasBrushFilter) {
        text = 'Brush filter active';
      }

      if (!resting) {
        const span = document.createElement('span');
        span.className = 'dd-status-text' + (this._returning ? ' dd-status-returning' : '');
        span.textContent = text;
        this.statusEl.appendChild(span);
      }

      if (this._capMessage) {
        const cap = document.createElement('span');
        cap.className = 'dd-status-cap';
        cap.textContent = this._capMessage;
        this.statusEl.appendChild(cap);
      }

      if (hasFilter) {
        const resetBtn = document.createElement('button');
        resetBtn.className = 'dd-status-reset';
        resetBtn.textContent = 'Reset';
        resetBtn.addEventListener('click', () => this._reset());
        this.statusEl.appendChild(resetBtn);
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
      clearTimeout(this._receiptTimer);
      this._receiptTimer = setTimeout(() => this._dropReceipt(),
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

  NS.chrome = chrome;
})();
