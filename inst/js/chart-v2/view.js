// @ts-check
/**
 * Chart block v2: the chart view.
 *
 * One view per chart container. It holds what R sent (columns, rows,
 * config) and the reader's state (the selection, legend toggles, the gear),
 * and draws by running the family's model and option and applying the
 * result: one ECharts instance per facet panel in a CSS grid, as v1 does.
 * The view decides nothing about the picture.
 *
 * What reruns (3-design.md): new rows rebuild the index; the model is
 * memoised on the rows and the config fields it reads, the option on the
 * model and the fields it reads; a click patches opacities; a resize only
 * resizes. Every draw still hands the option to ECharts, as v1 does.
 *
 * The chrome, the interactions and the export are mixed in from
 * chrome.js, interact.js and export.js.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});

  // The families v2 draws; the rest show an empty state until built.
  // `widths`: the option reads each panel's width (x labels, value labels).
  // `alwaysHighlight`: every draw ends with a highlight patch, as v1's
  // aggregated draw did; the others patch only while a filter is active.
  /** @typedef {{ model: any, option: any, legend: any, widths: boolean,
   *              alwaysHighlight: boolean }} FamilyImpl */
  /** @type {Record<string, FamilyImpl>} */
  const FAMILIES = {
    aggregated: { model: NS.model.aggregated, option: NS.option.aggregated,
                  legend: NS.option.aggregatedLegend, widths: true, alwaysHighlight: true },
    timeline: { model: NS.model.timeline, option: NS.option.timeline,
                legend: NS.option.timelineLegend, widths: false, alwaysHighlight: false }
  };

  // What a distribution panel draws without a numeric value.
  const NO_VALUE_HTML = '<div class="vd-empty-state"><p class="vd-empty-text">' +
    'Pick a numeric Value to plot its distribution</p></div>';

  /** @param {Record<string, any>} cfg @param {string[]} fields */
  const pick = (cfg, fields) => {
    /** @type {Record<string, any>} */
    const out = {};
    for (const f of fields) out[f] = cfg[f];
    return JSON.stringify(out);
  };

  /** @type {any} */
  let measureCtx = null;

  // The chrome, interaction and export methods, on the prototype the view
  // extends. Typed `any`: they live in other files.
  const Mixins = /** @type {any} */ (function Mixins() {});
  Object.assign(Mixins.prototype, NS.chrome, NS.interact, NS.exporter);

  class ChartView extends Mixins {
    /** @param {HTMLElement & Record<string, any>} el */
    constructor(el) {
      super();
      /** @type {any} */
      this.el = el;
      /** @type {any[]} */
      this.data = [];
      this._ix = new NS.RowIndex();
      // Bumped whenever the rows change; the model memo keys on it.
      this._rowsGen = 0;
      /** @type {VizColumn[]} */
      this.columns = [];
      this._columnsKey = '[]';
      /** @type {Record<string, any>} */
      this.config = {};
      /** @type {Record<string, any>} */
      this.argHelp = {};
      /** @type {any[]} */
      this.charts = [];
      /** @type {any[]} */
      this._slots = [];
      /** @type {string | null} */
      this._renderShape = null;
      /** @type {string | null} */
      this._lastDataStr = null;
      /** @type {any} */
      this._lastDataRev = null;
      // The selection: {type, filters} or null (chart-keys.js).
      /** @type {{ type: 'categorical' | 'range' | 'point', filters: Record<string, any[]> } | null} */
      this._filter = null;
      this._hasBrushFilter = false;
      this._awaitData = false;
      // Click counter riding on every filter message.
      this._drillSeq = 0;
      /** @type {HTMLElement | null} */
      this._signalEl = null;
      /** @type {string | null} */
      this._receipt = null;
      /** @type {any} */
      this._receiptTimer = null;
      this._returning = false;
      this._receiptAt = 0;
      /** @type {Set<string>} */
      this._legendOff = new Set();
      /** @type {string | null} */
      this._legendKey = null;
      /** @type {any} */
      this._slotsInst = null;
      /** @type {string | null} */
      this._capMessage = null;
      /** @type {any} */
      this.theme = null;
      this._ink = NS.INK_DEFAULT;
      /** @type {{ modelKey: string | null, model: any, optionKey: string | null, option: any }} */
      this._memo = { modelKey: null, model: null, optionKey: null, option: null };
      /** @type {boolean | undefined} */
      this._popoverOpen = undefined;
      /** @type {any} */
      this._resizeObserver = null;
      /** @type {any} */
      this._resizeRaf = null;
      // DOM fields, built by _buildDOM().
      /** @type {any} */ this.card = null;
      /** @type {any} */ this.gearBtn = null;
      /** @type {any} */ this.gearHeader = null;
      /** @type {any} */ this.popoverEl = null;
      /** @type {any} */ this.titleWrap = null;
      /** @type {any} */ this.titleEl = null;
      /** @type {any} */ this.subtitleEl = null;
      /** @type {any} */ this.captionEl = null;
      /** @type {any} */ this.chartGrid = null;
      /** @type {any} */ this.legendEl = null;
      /** @type {any} */ this.statusEl = null;
      this._buildDOM();
      /** @type {any} */
      this._cfg = this._makeConfig();
    }

    // The selection as v1 kept it: a value (or values) and a column. Read
    // by code and tests that know one column.
    get _selected() { return NS.keys.selectionView(this._filter && this._filter.filters).selected; }
    get _selectedColumn() { return NS.keys.selectionView(this._filter && this._filter.filters).column; }

    _family() { return NS.familyOf(this.config.chart_type); }

    /** @param {any} theme */
    setTheme(theme) {
      const normalized = (theme && theme !== 'default') ? theme : null;
      if (this.theme === normalized) return;
      this.theme = normalized;
      if (this.data && this.data.length) this._render();
    }

    /**
     * A message from R. The rows are parsed only when they changed: an
     * unchanged data_rev (or the same payload string) keeps the rows held.
     * @param {any} columns @param {any} data @param {any} config
     * @param {any} args @param {any} [dataRev]
     */
    setData(columns, data, config, args, dataRev) {
      this.columns = columns || [];
      this._columnsKey = JSON.stringify(this.columns);
      this.config = config || {};
      if (args) this.argHelp = args;

      const unchanged =
        (dataRev != null && dataRev === this._lastDataRev && Array.isArray(this.data)) ||
        (typeof data === 'string' && data === this._lastDataStr);
      if (!unchanged) {
        this._ix = new NS.RowIndex();
        this._rowsGen++;
        this._lastDataStr = typeof data === 'string' ? data : null;
        this.data = NS.rowsFromPayload(data);
      }
      this._lastDataRev = dataRev != null ? dataRev : null;
      // Whatever the chart waited for, this is the answer.
      this._awaitData = false;

      NS.model.defaults(this.config, this.columns);

      // A saved selection. A transient chart never latches, so it restores
      // none.
      const saved = NS.keys.restored(this.config);
      if (saved && !NS.transientDrill(this.config)) {
        this._filter = { type: 'categorical', filters: saved };
      }

      this._renderConfig();
      this._updateTitles();
      this._render();
    }

    // -- Drawing -------------------------------------------------------------

    _render() {
      this._ink = NS.readInk(this.el);
      const empty = NS.model.gate(this.data, this.columns, this.config);
      if (empty) { this._showEmpty(empty); return; }
      const fam = this._family();
      const impl = FAMILIES[fam];
      if (!impl) {
        this._showEmpty(NS.model.emptyState(
          'The v2 chart engine does not draw ' + this.config.chart_type + ' charts yet.'));
        return;
      }
      this._draw(fam, impl);
      // Once more after layout settles, and again whenever the grid resizes
      // (a dock tab coming to the front).
      setTimeout(() => { this._resizeCharts(); }, 300);
      this._observeResize();
    }

    /** @param {string} fam @param {FamilyImpl} impl */
    _draw(fam, impl) {
      const m = this._model(impl);
      if (m.empty) { this._showEmpty(m.empty); return; }
      this._applyFacetGrid(m.nFacets);
      // Panels without rows draw no slot, so the panel count is the shape.
      this._syncShape(fam, m.panels.length);
      // Every panel is laid out before any width is read, so the first
      // panel fits its labels to its own track, not the whole row (B9).
      m.panels.forEach((/** @type {any} */ panel, /** @type {number} */ i) => {
        const slot = this._ensureSlot(i, panel.label, m.single);
        slot.facetVal = panel.facet;
      });
      const widths = m.panels.map((/** @type {any} */ _, /** @type {number} */ i) =>
        this._slots[i].chartDiv.clientWidth);
      const o = this._option(impl, m, impl.widths ? widths : null);
      m.panels.forEach((/** @type {any} */ panel, /** @type {number} */ i) => {
        const slot = this._slots[i];
        const po = o.panels[i];
        if (!po.option) {
          // Nothing to draw here: the empty state owns the div.
          if (slot.chart) { slot.chart.dispose(); slot.chart = null; }
          this._setSlotHeight(slot, po.height);
          slot.chartDiv.innerHTML = NO_VALUE_HTML;
          return;
        }
        const hChanged = this._setSlotHeight(slot, po.height);
        const existed = !!slot.chart;
        const chart = this._ensureSlotChart(slot, fam);
        // What a resize needs to re-fit turned x labels: a copy, since the
        // re-fit updates it, plus the slot and its height without the gutter.
        chart.__xFit = po.xFit
          ? { ...po.xFit, slot, baseH: po.panelH - po.xFit.gutter } : undefined;
        chart.setOption(po.option, true);
        slot.dimmed = false;
        // A fresh instance measures its height; a kept one needs telling.
        if (existed && hChanged) chart.resize();
      });
      this.charts = this._slots.map((s) => s.chart).filter(Boolean);
      this._harmoniseAxes();
      this._updateLegendBand(impl.legend(m, this.config));
      this._capMessage = o.labelNote || null;
      if (impl.alwaysHighlight) {
        this._updateHighlight();
      } else {
        this._updateStatus();
        if (this._filter) this._applyHighlight();
      }
    }

    /** @param {{ model: any }} impl */
    _model(impl) {
      const key = this._rowsGen + '\u0000' + this._columnsKey + '\u0000' +
        pick(this.config, impl.model.FIELDS);
      if (this._memo.modelKey !== key) {
        this._memo.model = impl.model({ rows: this.data, ix: this._ix,
                                        columns: this.columns, cfg: this.config });
        this._memo.modelKey = key;
        this._memo.optionKey = null;
      }
      return this._memo.model;
    }

    /** @param {{ option: any }} impl @param {any} m @param {number[] | null} widths */
    _option(impl, m, widths) {
      const ink = this._ink;
      const key = this._memo.modelKey + '\u0000' + pick(this.config, impl.option.FIELDS) +
        '\u0000' + JSON.stringify(ink) + '\u0000' + (this.theme || '') +
        '\u0000' + (widths ? widths.join(',') : '');
      if (this._memo.optionKey !== key) {
        this._memo.option = impl.option(m, {
          cfg: this.config, columns: this.columns, ink, theme: this.theme,
          measure: this._measure(ink), widths: widths || []
        });
        this._memo.optionKey = key;
      }
      return this._memo.option;
    }

    // Text width in the axis font, on a canvas shared by every chart. The
    // font is set once: an option build measures synchronously.
    /** @param {typeof NS.INK_DEFAULT} ink @returns {(s: string) => number} */
    _measure(ink) {
      const ctx = measureCtx || (measureCtx = document.createElement('canvas').getContext('2d'));
      ctx.font = `${ink.fontSize}px ${ink.face}`;
      return (s) => ctx.measureText(s).width;
    }

    // One numeric domain across the panels ("fixed"), read back off what
    // each panel drew, so nothing a panel drew falls outside it.
    _harmoniseAxes() {
      const mode = NS.facetScales(this.config);
      if (mode === 'free') return;
      const charts = this._slots.map((s) => s.chart).filter(Boolean);
      if (charts.length < 2) return;
      for (const key of (mode === 'free_y' ? ['xAxis'] : ['xAxis', 'yAxis'])) {
        const extents = charts.map((c) => {
          const comp = c.getModel().getComponent(key, 0);
          if (!comp || comp.get('type') === 'category') return null;
          return comp.axis.scale.getExtent();
        });
        const d = NS.axes.sharedDomain(extents);
        if (!d) continue;
        for (const c of charts) c.setOption({ [key]: [{ min: d[0], max: d[1] }] });
      }
    }

    // -- Panels ----------------------------------------------------------------

    // The single-panel class, and the row width when facet_cols pins it
    // (auto leaves the stylesheet's auto-fit grid alone).
    /** @param {number} nFacets */
    _applyFacetGrid(nFacets) {
      this.chartGrid.classList.toggle('dd-chart-grid-single', nFacets <= 1);
      const n = parseInt(this.config.facet_cols, 10);
      this.chartGrid.style.gridTemplateColumns =
        (n > 0 && nFacets > 1) ? `repeat(${Math.min(n, nFacets)}, minmax(0, 1fr))` : '';
    }

    _teardownCharts() {
      for (const s of this._slots) { if (s && s.chart) s.chart.dispose(); }
      this._slots = [];
      this.charts = [];
      this._renderShape = null;
      this.chartGrid.innerHTML = '';
    }

    /** @param {string} html */
    _showEmpty(html) {
      this._teardownCharts();
      this._updateLegendBand(null);
      this.chartGrid.innerHTML = html;
      // The footer survives the empty state, so a filter that emptied the
      // chart can still be reset.
      this._updateStatus();
    }

    // Instances survive a redraw unless the family, the panel count or the
    // theme changed (an ECharts theme is fixed at init).
    /** @param {string} family @param {number} count */
    _syncShape(family, count) {
      const shape = family + '|' + count + '|' + (this.theme || '');
      if (this._renderShape !== shape) {
        this._teardownCharts();
        this._renderShape = shape;
      }
    }

    /** @param {number} i @param {string | null} facetLabel @param {boolean} singleFacet */
    _ensureSlot(i, facetLabel, singleFacet) {
      let slot = this._slots[i];
      if (!slot) {
        /** @type {HTMLElement} */
        let container;
        /** @type {HTMLElement | null} */
        let labelEl = null;
        if (singleFacet) {
          container = this.chartGrid;
        } else {
          container = document.createElement('div');
          container.className = 'dd-facet';
          labelEl = document.createElement('div');
          labelEl.className = 'dd-facet-label';
          container.appendChild(labelEl);
          this.chartGrid.appendChild(container);
        }
        const chartDiv = document.createElement('div');
        chartDiv.className = 'dd-chart';
        container.appendChild(chartDiv);
        slot = { container, labelEl, chartDiv, chart: null, facetVal: null, dimmed: false,
                 hover: { si: null }, seriesByColorByVal: null,
                 brushable: false, zoomArmed: false, zoom: null,
                 focus: null, focusSi: null, band: null, bandFocus: null };
        this._slots[i] = slot;
      }
      if (slot.labelEl) {
        slot.labelEl.textContent = facetLabel ?? '';
        slot.labelEl.style.display = facetLabel ? '' : 'none';
      }
      return slot;
    }

    /** @param {any} slot @param {string} family */
    _ensureSlotChart(slot, family) {
      if (!slot.chart) {
        slot.chartDiv.innerHTML = '';
        slot.chart = echarts.init(slot.chartDiv, this.theme || undefined);
        this._attachHandlers(slot, family);
      }
      return slot.chart;
    }

    /** @param {any} slot @param {string} h */
    _setSlotHeight(slot, h) {
      if (slot.chartDiv.style.height === h) return false;
      slot.chartDiv.style.height = h;
      return true;
    }

    // -- Size ------------------------------------------------------------------

    // Skipped while the grid is hidden or has no box: a dock keeps hidden
    // panels mounted, and their size changes when they come to the front.
    _resizeCharts() {
      if (!this.chartGrid || this.chartGrid.offsetParent === null) return;
      if (!this.chartGrid.clientWidth || !this.chartGrid.clientHeight) return;
      for (const c of this.charts) {
        c.resize();
        this._refitXLabels(c);
      }
    }

    // A resize keeps the option built at the old width, so turned or
    // wrapped x labels stay as they were. Measure again at the new width
    // and, when the layout changed, patch the labels, the grid's bottom
    // gutter and the canvas height by the gutter's change.
    /** @param {any} chart */
    _refitXLabels(chart) {
      const fit = chart.__xFit;
      if (!fit) return;
      const lab = NS.axes.xAxisLabels(fit.labels, chart.getWidth() - fit.inset, fit.decimate,
                                      this._measure(this._ink), this._ink);
      if (fit.key === lab.key) return;
      fit.key = lab.key;
      const delta = lab.bottom - fit.gutter;
      fit.gutter = lab.bottom;
      const grid = (chart.getOption().grid || [])[0] || {};
      /** @type {any} */
      const xPatch = { axisLabel: NS.axes.axisLabelWithDisplay(lab.axisLabel, fit.formatter) };
      // A titled x axis moves its title below the turned text.
      if (fit.nameGap) xPatch.nameGap = lab.bottom ? lab.bottom + 16 : 28;
      chart.setOption({ xAxis: [xPatch], grid: { bottom: (Number(grid.bottom) || 0) + delta } });
      // The canvas last; the observer comes back here once, and finds the
      // key unchanged.
      if (delta && fit.slot &&
          this._setSlotHeight(fit.slot, Math.round(fit.baseH + lab.bottom) + 'px')) {
        chart.resize();
      }
    }

    // One resize per animation frame, however many ticks a relayout fires.
    _observeResize() {
      if (this._resizeObserver) this._resizeObserver.disconnect();
      this._resizeObserver = new ResizeObserver(() => {
        if (this._resizeRaf) return;
        this._resizeRaf = requestAnimationFrame(() => {
          this._resizeRaf = null;
          this._resizeCharts();
        });
      });
      this._resizeObserver.observe(this.chartGrid);
    }

    resize() { this._resizeCharts(); }

    dispose() {
      if (this._resizeObserver) this._resizeObserver.disconnect();
      if (this._resizeRaf) { cancelAnimationFrame(this._resizeRaf); this._resizeRaf = null; }
      this._teardownCharts();
      // The slot menu is portalled to <body>, outside the card.
      this._closeSlot();
    }
  }

  NS.ChartView = ChartView;
  NS.FAMILIES = FAMILIES;
})();
