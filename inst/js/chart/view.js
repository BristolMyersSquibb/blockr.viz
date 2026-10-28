// @ts-check
/**
 * Chart block: the chart view, one per chart container.
 *
 * It holds what R sent (columns, rows, config) and the reader's state (the
 * selection, the gear), and draws by running the family's model and option:
 * one ECharts instance per facet panel in a CSS grid. The model is memoised
 * on the rows and the config fields it reads, the option on the model and
 * its fields; a click patches opacities; a resize only resizes.
 *
 * Built by the shared output binding (core/binding.js) with a Scope that
 * owns everything set up outside the element. The chrome, interactions and
 * export are mixed in from chrome.js, interact.js and export.js.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});
  const mk = NS.mk;

  // `widths`: the option reads each panel's width. `alwaysHighlight`: every
  // draw ends with a highlight patch; the others patch only while a filter
  // dims. `prepare` / `after`: view methods run on a panel's slot before and
  // after its option is set. `actions`: tells whether `after` dispatches an
  // action.
  /** @typedef {{ model: any, option: any, legend: any, widths: boolean,
   *              alwaysHighlight: boolean, prepare?: string, after?: string,
   *              actions?: string }} FamilyImpl */
  /** @type {Record<string, FamilyImpl>} */
  const FAMILIES = {
    aggregated: { model: NS.model.aggregated, option: NS.option.aggregated,
                  legend: NS.option.aggregatedLegend, widths: true, alwaysHighlight: true },
    timeline: { model: NS.model.timeline, option: NS.option.timeline,
                legend: NS.option.timelineLegend, widths: false, alwaysHighlight: false },
    individual: { model: NS.model.individual, option: NS.option.individual,
                  legend: NS.option.individualLegend, widths: true, alwaysHighlight: false,
                  prepare: '_individualPrepare', after: '_individualAfter',
                  actions: '_individualActions' }
  };

  const NO_VALUE_HTML = '<div class="vd-empty-state"><p class="vd-empty-text">' +
    'Pick a numeric Value to plot its distribution</p></div>';

  /** A memo key of the config fields; `derived.x` is R's derived x.
   * @param {Record<string, any>} cfg @param {string[]} fields */
  const pick = (cfg, fields) => {
    /** @type {Record<string, any>} */ const out = {};
    for (const f of fields) {
      out[f] = f.startsWith('derived.') ? cfg.derived[f.slice(8)] : cfg[f];
    }
    return JSON.stringify(out);
  };

  /** @type {any} */ let measureCtx = null;

  const Mixins = /** @type {any} */ (function Mixins() {});
  Object.assign(Mixins.prototype, NS.chrome, NS.scriptBand, NS.interact, NS.exporter);

  class ChartView extends Mixins {
    /** @param {HTMLElement & Record<string, any>} el @param {VizScope} scope */
    constructor(el, scope) {
      super();
      /** @type {any} */ this.el = el;
      this.scope = scope;
      /** @type {any[]} */ this.data = [];
      /** @type {any} */ this._lastDataRev = null;
      /** @type {any} */ this._lastSelection = null;
      this._ix = new NS.RowIndex();
      // Bumped whenever the rows change; the model memo keys on it.
      this._rowsGen = 0;
      /** @type {VizColumn[]} */ this.columns = [];
      /** @type {Record<string, any>} */ this.config = {};
      /** @type {Record<string, any>} */ this.derived = {};
      /** @type {any[]} */ this.charts = [];
      /** @type {any[]} */ this._slots = [];
      /** @type {string | null} */ this._renderShape = null;
      /** Owns the panels' ECharts instances; a new shape gets a new one.
       * @type {VizScope | null} */
      this._shapeScope = null;
      // The selection (chart-keys.js): {type: 'categorical', filters} or
      // {type: 'range', range, filters (the facet key)}, `brush` when a brush made it.
      /** @type {{ type: 'categorical' | 'range', filters?: Record<string, any[]> | null,
       *           range?: any, brush?: boolean } | null} */
      this._filter = null;
      this.drawn = false;
      this._drillSeq = 0;
      /** @type {string | null} */ this.theme = null;
      this._ink = NS.INK_DEFAULT;
      /** @type {{ modelKey: string | null, model: any, optionKey: string | null, option: any }} */
      this._memo = { modelKey: null, model: null, optionKey: null, option: null };
      /** @type {(() => void) | null} */ this._settle = null;
      this._buildDOM();
      // One resize per animation frame.
      /** @type {(() => void) | null} */ let pending = null;
      scope.resize(this.chartGrid, () => {
        if (pending) return;
        pending = scope.frame(() => {
          pending = null;
          this._resizeCharts();
        });
      });
    }

    _family() { return NS.familyOf(this.config.type); }

    /** The ECharts theme R named; a theme R sent whole is registered once.
     * @param {any} theme */
    _setTheme(theme) {
      let name = typeof theme === 'string' ? theme : (theme && theme.name) || null;
      if (theme && typeof theme === 'object' && theme.theme && name) {
        root.echarts.registerTheme(name, theme.theme);
      }
      if (name === 'default') name = null;
      this.theme = name;
    }

    /**
     * A payload from R (core/binding.js hands it over with its rows). The
     * rows are parsed only when their revision changed.
     * @param {VizPayload} p
     */
    render(p) {
      const d = /** @type {Record<string, any>} */ (p.derived || {});
      this.derived = d;
      this.columns = d.column_meta || [];
      this._columnsKey = JSON.stringify(this.columns);
      if (p.data_rev !== this._lastDataRev || !this.data.length) {
        this._ix = new NS.RowIndex();
        this._rowsGen++;
        this.data = NS.rowsFromPayload(p.data);
        this._lastDataRev = p.data_rev;
      }
      this.config = NS.model.settings(p.config, d, this.columns);
      this._restoreSelection(p.config && p.config.selection);
      this._setTheme(d.theme);
      // The gear first: the title sentences open what it offers.
      if (this._gear) this._gear.update(Object.assign({}, p, { columns: this.columns }));
      this._titleText = NS.titleText(d.titles);
      this._updateTitles(d.titles);
      this._updateScriptBand(d.script);
      this.drawn = false;
      this._render();
      this._updateDownloads(d.downloads);
      this._busyDone();
    }

    _busyDone() {
      const busy = root.Blockr.busy;
      if (!busy) return;
      this.scope.frame(() => this.scope.frame(() => busy.stop(this.el.id)));
    }

    // A saved selection R sent back. Only a changed one replaces what the
    // reader sees; a transient chart restores none.
    /** @param {any} sel */
    _restoreSelection(sel) {
      const key = JSON.stringify(sel ?? null);
      if (key === this._lastSelection) return;
      this._lastSelection = key;
      if (NS.transientDrill(this.config)) { this._filter = null; return; }
      const range = NS.keys.restoredRange(this.config);
      const saved = range ? null : NS.keys.restored(this.config);
      this._filter = range ? { type: 'range', range: range.range, filters: range.filters }
        : (saved ? { type: 'categorical', filters: saved } : null);
    }

    /** An error R sent: said where the chart would be. @param {string} message */
    error(message) {
      this._showEmpty(NS.model.emptyState(NS.esc(message)));
      this._updateDownloads(this.derived.downloads);
      this._busyDone();
    }

    _render() {
      this._ink = NS.readInk(this.el);
      const empty = NS.model.gate(this.data, this.columns, this.config);
      if (empty) { this._showEmpty(empty); return; }
      const fam = this._family();
      this._blank = false;
      this._draw(fam, FAMILIES[fam]);
      // Once more after layout settles.
      if (this._settle) this._settle();
      this._settle = this.scope.timeout(() => { this._resizeCharts(); }, 300);
    }

    /** @param {string} fam @param {FamilyImpl} impl */
    _draw(fam, impl) {
      const m = this._model(impl);
      if (m.empty) { this._showEmpty(m.empty); return; }
      const grid = this.chartGrid;
      grid.classList.toggle('dd-chart-grid-single', m.nFacets <= 1);
      // The row width when facet_cols pins it.
      const cols = parseInt(this.config.facet_cols, 10);
      grid.style.gridTemplateColumns =
        (cols > 0 && m.nFacets > 1) ? `repeat(${Math.min(cols, m.nFacets)}, minmax(0, 1fr))` : '';
      // Instances survive a redraw unless the family, panel count or theme changed (an
      // ECharts theme is fixed at init).
      const shape = fam + '|' + m.panels.length + '|' + (this.theme || '');
      if (this._renderShape !== shape) {
        this._teardownCharts();
        this._renderShape = shape;
      }
      // Every panel is laid out before any width is read, so the first panel
      // fits its labels to its own track (B9).
      m.panels.forEach((/** @type {any} */ panel, /** @type {number} */ i) => {
        this._ensureSlot(i, panel.label, m.single).facetVal = panel.facet;
      });
      const widths = m.panels.map((/** @type {any} */ _, /** @type {number} */ i) =>
        this._slots[i].chartDiv.clientWidth);
      const o = this._option(impl, m, impl.widths ? widths : null);
      m.panels.forEach((/** @type {any} */ panel, /** @type {number} */ i) => {
        const slot = this._slots[i];
        const po = o.panels[i];
        if (!po.option) {
          if (slot.scope) slot.scope.dispose();
          slot.scope = null;
          slot.chart = null;
          this._setSlotHeight(slot, po.height);
          slot.chartDiv.innerHTML = NO_VALUE_HTML;
          return;
        }
        const hChanged = this._setSlotHeight(slot, po.height);
        const existed = !!slot.chart;
        const chart = this._ensureSlotChart(slot, fam);
        // What a resize needs to re-fit turned x labels (copied: the re-fit
        // updates it), plus the slot and its height without the gutter.
        chart.__xFit = po.xFit
          ? { ...po.xFit, slot, baseH: po.panelH - po.xFit.gutter } : undefined;
        if (impl.prepare) this[impl.prepare](slot, po);
        // When an action follows, ECharts runs it with the option in one update.
        const actionFollows = !!impl.actions && this[impl.actions](slot, po);
        chart.setOption(po.option, actionFollows ? { notMerge: true, lazyUpdate: true } : true);
        slot.dimmed = false;
        if (existed && hChanged) chart.resize();
        if (impl.after) this[impl.after](slot, po);
      });
      this.charts = this._slots.map((s) => s.chart).filter(Boolean);
      // Lazy patches: ECharts applies them in one update on the next frame.
      this._harmoniseAxes();
      const plan = NS.legendPlan(m, this.config);
      this._updateLegendBand(plan.band ? impl.legend(m, this.config) : null, plan);
      this._capMessage = o.labelNote || null;
      if (impl.alwaysHighlight) {
        this._updateHighlight(true);
      } else {
        this._updateStatus();
        if (this._filter) this._applyHighlight(true);
      }
      this.drawn = true;
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

    // Text width in a CSS font, on a shared canvas.
    /** @param {string} font @returns {(s: string) => number} */
    _measureFont(font) {
      const ctx = measureCtx || (measureCtx = document.createElement('canvas').getContext('2d'));
      ctx.font = font;
      return (s) => ctx.measureText(s).width;
    }

    /** The axis font. @param {typeof NS.INK_DEFAULT} ink */
    _measure(ink) { return this._measureFont(`${ink.fontSize}px ${ink.face}`); }

    // One numeric domain across the panels ("fixed"), read back off what each drew.
    _harmoniseAxes() {
      const mode = NS.facetScales(this.config);
      if (mode === 'free') return;
      const charts = this.charts;
      if (charts.length < 2) return;
      for (const key of (mode === 'free_y' ? ['xAxis'] : ['xAxis', 'yAxis'])) {
        const extents = charts.map((c) => {
          const comp = c.getModel().getComponent(key, 0);
          if (!comp || comp.get('type') === 'category') return null;
          return comp.axis.scale.getExtent();
        });
        const d = NS.axes.sharedDomain(extents);
        if (!d) continue;
        for (const c of charts) c.setOption({ [key]: [{ min: d[0], max: d[1] }] }, { lazyUpdate: true });
      }
    }

    _teardownCharts() {
      if (this._shapeScope) this._shapeScope.dispose();
      this._shapeScope = null;
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
      this._updateStatus();
      this.drawn = true;
      this._blank = true;
    }

    /** @param {number} i @param {string | null} facetLabel @param {boolean} singleFacet */
    _ensureSlot(i, facetLabel, singleFacet) {
      let slot = this._slots[i];
      if (!slot) {
        const container = singleFacet ? this.chartGrid : mk('div', 'dd-facet', this.chartGrid);
        const labelEl = singleFacet ? null : mk('div', 'dd-facet-label', container);
        const chartDiv = mk('div', 'dd-chart', container);
        const resetBtn = /** @type {HTMLButtonElement} */ (mk('button', 'dd-zoom-reset', container));
        resetBtn.type = 'button';
        resetBtn.textContent = 'Reset zoom';
        resetBtn.hidden = true;
        container.classList.add('dd-zoom-host');
        slot = { container, labelEl, chartDiv, resetBtn, chart: null, facetVal: null, dimmed: false,
                 hover: { si: null }, brushable: false, zoomArmed: false, zoom: null,
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
        const shape = this._shapeScope || (this._shapeScope = this.scope.own(new V.Scope()));
        slot.scope = shape.own(new V.Scope());
        slot.chart = slot.scope.own(echarts.init(slot.chartDiv, this.theme || undefined));
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

    // Skipped while the grid is hidden or has no box (a dock keeps hidden panels mounted).
    _resizeCharts() {
      if (this.chartGrid.offsetParent === null) return;
      if (!this.chartGrid.clientWidth || !this.chartGrid.clientHeight) return;
      for (const c of this.charts) {
        c.resize();
        this._refitXLabels(c);
      }
    }

    // A resize keeps the option built at the old width. Measure the x labels
    // again and, when the layout changed, patch the labels, the grid's bottom
    // gutter and the canvas height.
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
      /** @type {any} */ const label = NS.axes.axisLabelWithDisplay(lab.axisLabel, fit.formatter);
      /** @type {any} */ const xPatch = { axisLabel: fit.decorate ? fit.decorate(label) : label };
      // A titled x axis moves its title below the turned text.
      if (fit.nameGap) xPatch.nameGap = lab.bottom ? lab.bottom + 16 : 28;
      chart.setOption({ xAxis: [xPatch], grid: { bottom: (Number(grid.bottom) || 0) + delta } });
      if (delta && fit.slot &&
          this._setSlotHeight(fit.slot, Math.round(fit.baseH + lab.bottom) + 'px')) {
        chart.resize();
      }
    }

    dispose() { this._teardownCharts(); }
  }

  V.binding('blockr.viz.chart', 'chart', ChartView, { busy: 'drawing' });
  NS.registerCapture(ChartView);
})();
