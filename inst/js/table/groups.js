// @ts-check
/**
 * The groups layout: one row per level of `by` (an outer level folds its
 * inner ones), one column per summary, each cell a mark (marks.js), with
 * the column's scale printed once under its header. The records view does
 * the rest (search, sort, drill, cards, footer); this adds the marks, the
 * legend, the fold note, and the picture the downloads take.
 *
 * Looks: `compact` is the matrix (square tile cells, titles on their side),
 * set on the table as data-summarize-compact; `cards` draws each row as
 * cards with the column's mark in each.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const T = V.table;
  const esc = V.util.esc;


  /**
   * The legend: one titled group per colour column.
   * @param {HTMLElement} el
   * @param {TableLegend | null | undefined} lg
   */
  function paintLegend(el, lg) {
    const groups = (lg && lg.groups) || [];
    el.hidden = !groups.length;
    el.innerHTML = groups.map((g) => '<span class="vz-table-groups-legend-group">' +
      '<span class="vz-table-groups-legend-title">' + esc(g.title) + '</span>' +
      (g.items || []).map((it) => '<span class="vz-table-groups-legend-item">' + (it.shape != null
        ? '<svg class="vz-table-groups-legend-shape" viewBox="-7 -7 14 14">' + V.table.marks.shape(Number(it.shape)) + '</svg>'
        : '<i style="background:' + esc(it.color) + '"></i>') +
        esc(it.label) + '</span>').join('') + '</span>').join('');
  }

  /**
   * Keeps the header in view while the panel scrolls: moves it down by as
   * much of the table as has scrolled out of view, and stops at the last row.
   * @param {TableFrame} frame
   */
  function followHeader(frame) {
    const box = frame.box;
    const thead = /** @type {HTMLElement | null} */ (box.querySelector('table.blockr-table > thead'));
    const table = thead && thead.parentElement;
    if (!thead || !table) return;
    const sp = frame.scroller();
    const top = sp ? sp.getBoundingClientRect().top : 0;
    // offsetHeight, not a rect: a rect would include the transform set here.
    const max = table.offsetHeight - thead.offsetHeight;
    const y = Math.max(0, Math.min(top - table.getBoundingClientRect().top, max));
    thead.style.transform = y > 0 ? 'translateY(' + y + 'px)' : '';
    box.classList.toggle('scrolled', y > 0);
  }

  class Groups extends /** @type {any} */ (T.Records) {
    /**
     * @param {TableFrame} frame
     * @param {VizScope} scope
     * @param {TableLayoutOpts} [opts]
     */
    constructor(frame, scope, opts = {}) {
      super(frame, scope, opts);
      frame.el.classList.add('vz-table-groups');
      scope.own({ dispose: () => frame.el.classList.remove('vz-table-groups') });
      this.legend = scope.node(document.createElement('div'));
      this.legend.className = 'vz-table-groups-legend';
      frame.box.before(this.legend);
      const grid = /** @type {TableGrid} */ (this.grid);
      const drillClass = /** @type {(i: number) => string} */ (grid.hooks.rowClass);
      grid.hooks.rowClass = (i) => [drillClass(i), this.rowKind(i)].filter(Boolean).join(' ');
      grid.hooks.cell = (c, i) => T.marks.cell(c, i, { labels: this.labelsOn() });
      T.tooltip.wire(frame.box, scope, () => this.model);
      // One capture listener on the document sees every scroller's scroll:
      // the dock moves a card between containers after it is drawn.
      const follow = () => followHeader(frame);
      if (!this.still) {
        scope.on(document, 'scroll', follow, { capture: true, passive: true });
        scope.on(window, 'resize', follow);
      }
      /** What the last picture was of. */
      this.pictured = '';
      /** The picture of `pictured` on its way to R. @type {Promise<unknown> | null} */
      this.sending = null;
    }

    labelsOn() {
      return this.config().value_labels !== false;
    }

    /** @returns {boolean} */
    grouped() {
      const by = this.config().by;
      return Array.isArray(by) && by.length > 0;
    }

    /**
     * A row's place in the nesting: a parent of inner rows, an inner row,
     * the last inner row before the next parent.
     * @param {number} i
     */
    rowKind(i) {
      const m = /** @type {TableModel | null} */ (this.model);
      const ind = m && m.rows && m.rows.indent;
      if (!ind) return 'vz-table-groups-row';
      const next = i + 1 < ind.length ? ind[i + 1] : null;
      if (next != null && next > ind[i]) return 'vz-table-groups-row is-parent';
      if (ind[i] > 0) return 'vz-table-groups-row is-child' + (next == null || next < ind[i] ? ' is-group-end' : '');
      return 'vz-table-groups-row';
    }

    /** @param {VizPayload} p */
    render(p) {
      super.render(p);
      const d = /** @type {TableDerived} */ (p.derived || {});
      paintLegend(this.legend, this.model && !this.cardsOn() ? d.legend : null);
      const grid = /** @type {TableGrid} */ (this.grid);
      const fonts = root.document.fonts && root.document.fonts.ready
        ? root.document.fonts.ready : Promise.resolve();
      fonts.then(() => tipCutTitles(grid.table));
    }

    /**
     * Search and sorting need groups; a picture has no tools.
     * @param {TableConfig} cfg
     * @param {boolean} has
     * @param {TableDerived} d
     */
    tools(cfg, has, d) {
      if (this.still) return { search: false, download: false };
      return { search: has && this.grouped() && cfg.search !== false,
               download: has && cfg.download !== false, downloads: d.downloads,
               onOpen: () => this.sendPicture() };
    }

    /**
     * @param {TableConfig} cfg
     * @param {TableModel} m
     * @returns {TableGridFlags}
     */
    flags(cfg, m) {
      const labels = this.labelsOn();
      const axis = cfg.column_axis !== false;
      /** @type {TableGrid} */ (this.grid).hooks.axis = axis ? (c) => T.marks.axis(c, labels) : undefined;
      /** @type {Record<string, string>} */
      const attrs = { 'data-summarize-values': labels ? 'on' : 'off',
                      'data-summarize-axis': axis ? 'on' : 'off' };
      if (cfg.bar_width && cfg.bar_width !== 'fit') attrs['data-summarize-width'] = cfg.bar_width;
      if (cfg.look === 'compact') attrs['data-summarize-compact'] = '1';
      if (m.rows && m.rows.indent && m.rows.indent.some((x) => x > 0)) attrs['data-summarize-nested'] = '1';
      return { sortable: cfg.sortable !== false && this.grouped(), collapsible: true, all: this.still,
               cls: 'vz-table-groups-table', stubCls: 'vz-table-groups-label-col', attrs };
    }

    /** A card draws its column's mark, without the value it prints itself. */
    cardOpts() {
      const m = /** @type {TableModel} */ (this.model);
      return { glyph: (/** @type {number} */ k, /** @type {number} */ i) =>
        T.marks.glyph(m.cols[k] ? m.cols[k].mark : null, i, false) };
    }

    /**
     * The fold note in place of a row count.
     * @param {TableModel} m
     */
    countLine(m) {
      return m.note || '';
    }

    footer() {
      if (this.still) {
        if (this.model) this.frame.count(this.countLine(this.model));
        return;
      }
      super.footer();
    }

    /** @returns {TableInfo} */
    info() {
      return { structured: false, finished: false, numbers: true };
    }

    /**
     * Send R the picture of the table as drawn, once per payload.
     * @returns {Promise<unknown> | null} The sending, which a click on a
     *   download format waits for; null without a table.
     */
    sendPicture() {
      const p = /** @type {VizPayload | null} */ (this.payload);
      if (!p || !this.model) return null;
      const key = p.state_rev + ':' + p.data_rev;
      if (key === this.pictured) return this.sending;
      this.pictured = key;
      const width = Math.round(this.frame.el.getBoundingClientRect().width) || 900;
      this.sending = picture(Object.assign({}, p, { data: this.model }), width).then(
        (pic) => V.util.send(this.frame.id, 'capture', pic),
        // Without a picture R paints the table itself.
        () => { this.pictured = ''; });
      return this.sending;
    }
  }

  /**
   * A compact matrix's rotated titles stop at their cap (max-height) with an
   * ellipsis; each one cut gets its full text as the tooltip. The text is
   * measured on a canvas, so a table drawn in a hidden panel gets them too
   * (blockr.ui's own cut-off check reads widths, and these overflow upwards).
   * @param {HTMLTableElement | null | undefined} table
   */
  function tipCutTitles(table) {
    if (!table || !table.isConnected || table.getAttribute('data-summarize-compact') !== '1') return;
    const canvas = table.ownerDocument.createElement('canvas');
    const ctx = canvas.getContext ? canvas.getContext('2d') : null;
    if (!ctx) return;
    table.querySelectorAll('thead th.vz-table-col-header .blockr-col-name').forEach((n) => {
      const th = n.closest('th');
      const cs = root.getComputedStyle(n);
      const cap = parseFloat(cs.maxHeight);
      if (!th || !isFinite(cap)) return;
      ctx.font = cs.font || [cs.fontStyle, cs.fontWeight, cs.fontSize, cs.fontFamily].join(' ');
      const text = (n.textContent || '').trim();
      if (ctx.measureText(text).width > cap) th.setAttribute('data-blockr-tooltip', text);
      else th.removeAttribute('data-blockr-tooltip');
    });
  }

  /**
   * Wait for the fonts and two frames, so the table is laid out.
   * @param {VizScope} scope
   * @returns {Promise<void>}
   */
  function settled(scope) {
    const fonts = root.document.fonts && root.document.fonts.ready
      ? root.document.fonts.ready : Promise.resolve();
    return fonts.then(() => new Promise((r) => scope.frame(() => scope.frame(() => r(undefined)))));
  }

  /**
   * The picture's pixel ratio: R's (`derived.capture_ratio`, the board's
   * option), 2 on a payload without one.
   * @param {VizPayload} p
   */
  const pictureRatio = (p) =>
    Number(/** @type {TableDerived} */ (p.derived || {}).capture_ratio) || 2;

  /**
   * A picture of a groups table drawn off screen from `payload`: every row,
   * the titles, legend and caption, no tools. For the Image and PowerPoint
   * downloads, at the width the export asks for.
   * @param {VizPayload} payload Its `data` is the table model.
   * @param {number} width CSS px.
   * @param {number} [ratio] Pixel ratio of the PNG.
   * @returns {Promise<VizPicture>}
   */
  function picture(payload, width, ratio) {
    const snap = root.snapdom;
    if (!snap || typeof snap.toCanvas !== 'function') {
      return Promise.reject(new Error('snapdom is not loaded'));
    }
    return V.capture({ width, height: 20000, draw: (host) => {
      const scope = new V.Scope();
      const el = host.appendChild(root.document.createElement('div'));
      const frame = new T.Frame(el, scope);
      frame.el.classList.add('vz-table-groups-capture');
      const view = new Groups(frame, scope, { still: true });
      view.render(payload);
      return settled(scope)
        .then(() => snap.toCanvas(frame.el, { scale: ratio || pictureRatio(payload), dpr: 1,
                                              embedFonts: true,
                                              backgroundColor: '#ffffff' }))
        .then((/** @type {HTMLCanvasElement} */ canvas) => ({
          png: canvas.toDataURL('image/png'),
          width: Math.ceil(frame.el.offsetWidth), height: Math.ceil(frame.el.offsetHeight)
        }))
        .finally(() => scope.dispose());
    } });
  }

  T.layouts.groups = /** @type {TableLayoutClass} */ (/** @type {unknown} */ (Groups));
  T.picture = picture;
})();
