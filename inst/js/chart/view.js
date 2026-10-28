// @ts-check
/**
 * Chart block: the chart view, one per chart container.
 *
 * It holds what R sent (columns, rows, config) and the reader's state (the
 * selection, the gear), and draws by running the family's model and option
 * (family-*.js): one ECharts instance per facet panel in a CSS grid.
 * The model is memoised on the rows and the config fields it reads, the
 * option on the model and its fields; a click patches opacities; a resize
 * only resizes.
 *
 * Built by the shared output binding (core/binding.js) with a Scope that
 * owns everything set up outside the element. The view owns its parts: the
 * core frame (header, titles, gear, downloads, footer), the script band,
 * the legend band and the selection.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const mk = NS.mk;

  /** @type {VizEmpty} */
  const NO_VALUE = { kind: 'empty', message: 'Pick a numeric Value to plot its distribution' };

  /** A memo key of the config fields; `derived.x` is R's derived x.
   * @param {Record<string, any>} cfg @param {string[]} fields */
  const pick = (cfg, fields) => {
    /** @type {Record<string, any>} */ const out = {};
    for (const f of fields) {
      out[f] = f.startsWith('derived.') ? cfg.derived[f.slice(8)] : cfg[f];
    }
    return JSON.stringify(out);
  };

  /** @type {CanvasRenderingContext2D | null} */ let measureCtx = null;

  class ChartView {
    /** @param {HTMLElement} el @param {VizScope} scope */
    constructor(el, scope) {
      this.el = el;
      this.scope = scope;
      /** @type {any[]} */ this.data = [];
      /** @type {number | null} */ this._lastDataRev = null;
      this._ix = new NS.RowIndex();
      // Bumped whenever the rows change; the model memo keys on it.
      this._rowsGen = 0;
      /** @type {VizColumn[]} */ this.columns = [];
      this._columnsKey = '';
      /** The settings the drawing reads (NS.model.settings()). @type {Record<string, any>} */
      this.config = {};
      /** @type {ChartDerived} */ this.derived = /** @type {ChartDerived} */ ({});
      /** @type {any[]} */ this.charts = [];
      /** @type {ChartSlot[]} */ this._slots = [];
      /** @type {string | null} */ this._renderShape = null;
      /** Owns the panels' ECharts instances; a new shape gets a new one.
       * @type {VizScope | null} */
      this._shapeScope = null;
      this.drawn = false;
      /** An empty state is drawn. */
      this._blank = false;
      /** @type {string | null} */ this.theme = null;
      /** @type {Ink} */ this._ink = NS.INK_DEFAULT;
      /** @type {{ modelKey: string | null, model: any, optionKey: string | null, option: any }} */
      this._memo = { modelKey: null, model: null, optionKey: null, option: null };
      /** @type {(() => void) | null} */ this._settle = null;
      this.selection = new NS.Selection(this);
      this.frame = new V.Frame(el, scope, { cls: 'vz-chart' });
      this.gear = this.frame.mountGear({ block: 'chart', typeKey: 'type',
                                         title: 'Chart settings', host: NS.gear.host(this) });
      const body = this.frame.body;
      this.band = V.scriptBand(body, scope, this.gear);
      this.chartGrid = mk('div', 'dd-chart-grid', body);
      this.legend = new NS.Legend(this, body);
      this.status();
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

    /** The family that draws the current type. @returns {ChartFamily} */
    family() { return NS.families[NS.familyOf(this.config.type)]; }

    /** The ECharts theme R named (drilldown-theme-register.js registered
     * it); "default" is ECharts' own. @param {ChartTheme | null | undefined} theme */
    _setTheme(theme) {
      this.theme = theme && theme !== 'default' ? theme : null;
    }

    /** A payload from R (core/binding.js hands it over with its rows). The rows are parsed only
     * when their revision changed. @param {VizPayload} p */
    render(p) {
      const d = /** @type {ChartDerived} */ (p.derived || {});
      this.derived = d;
      this.columns = p.columns || [];
      this._columnsKey = JSON.stringify(this.columns);
      if (p.data_rev !== this._lastDataRev || !this.data.length) {
        this._ix = new NS.RowIndex();
        this._rowsGen++;
        this.data = NS.rowsFromPayload(p.data);
        this._lastDataRev = p.data_rev;
      }
      this.config = NS.model.settings(p.config, d, this.columns);
      this.selection.restore(p.config && p.config.selection);
      this._setTheme(d.theme);
      // The gear first: the title sentences open what it offers.
      this.gear.update(p);
      this.frame.titles(d.titles);
      this.band.update(d.script);
      this.drawn = false;
      this._render();
      this._downloads();
      this.frame.busyDone();
    }

    /** An error R sent: said where the chart would be. @param {string} message */
    error(message) {
      this._showEmpty({ kind: 'error', message });
      this._downloads();
      this.frame.busyDone();
    }

    // The formats R offers. Opening the control sends the picture to R (a
    // click on a format waits for it); an empty state has none, so Image is
    // left out.
    _downloads() {
      const offer = (this.derived.downloads || []).filter((f) => !(this._blank && f.ext === 'png'));
      this.frame.downloads(this.config.download !== false ? offer : [], () =>
        NS.picture(this).then((/** @type {VizPicture} */ pic) => V.util.send(this.el.id, 'capture', pic)));
    }

    /** The footer: what the selection holds, with its clear button. */
    status() {
      this.frame.status(this.selection.words(), () => this.selection.clear());
    }

    /** The footer, the legend chips, then the marks. @param {boolean} [lazy] */
    highlight(lazy) {
      this.status();
      this.legend.mark(this.selection.current);
      this._applyHighlight(lazy);
    }

    // The selection as opacities, one patch per panel and nothing redrawn:
    // a mark is lit when its keys match the selection's filters, or, for a
    // filter on a column it does not carry, when one of its rows does. A
    // range dims the points outside it. `lazy`: part of a draw, so ECharts
    // applies the patch with the draw's other follow-ups.
    /** @param {boolean} [lazy] */
    _applyHighlight(lazy) {
      const fam = this.family();
      const m = this._memo.model;
      const sel = this.selection.current;
      const filters = sel ? (sel.filters || null) : null;
      const range = sel && sel.type === 'range' ? sel : null;
      /** @type {ChartLit | null} */
      const lit = filters
        ? (keys, rows) => NS.keys.markLit(keys,
            rows || (() => NS.model.rowsUnder(this.data, this._ix, keys)), filters)
        : null;
      this._slots.forEach((slot, i) => {
        if (!slot || !slot.chart) return;
        const current = slot.chart.getOption();
        if (!current || !current.series) return;
        const panel = m && m.family === fam.name ? m.panels[i] : null;
        const drawn = this._memo.option && this._memo.option.panels[i];
        const patch = panel
          ? fam.patch({ m, panel, cfg: this.config, current, drawn: drawn && drawn.option,
                        lit, filters, range, dimmed: !!slot.dimmed })
          : { series: current.series.map(() => ({})), dimmed: false };
        slot.dimmed = patch.dimmed;
        slot.chart.setOption({ series: patch.series }, lazy ? { lazyUpdate: true } : false);
      });
    }

    _render() {
      this._ink = NS.readInk(this.el);
      const empty = NS.model.gate(this.data, this.columns, this.config);
      if (empty) { this._showEmpty(empty); return; }
      this._blank = false;
      this._draw(this.family());
      // Once more after layout settles.
      if (this._settle) this._settle();
      this._settle = this.scope.timeout(() => { this._resizeCharts(); }, 300);
    }

    /** @param {ChartFamily} fam */
    _draw(fam) {
      const m = this._model(fam);
      if (m.empty) { this._showEmpty(m.empty); return; }
      const grid = this.chartGrid;
      grid.classList.toggle('dd-chart-grid-single', m.nFacets <= 1);
      // The row width when facet_cols pins it.
      const cols = parseInt(this.config.facet_cols, 10);
      grid.style.gridTemplateColumns =
        (cols > 0 && m.nFacets > 1) ? `repeat(${Math.min(cols, m.nFacets)}, minmax(0, 1fr))` : '';
      // Instances survive a redraw unless the family, panel count or theme changed (an
      // ECharts theme is fixed at init).
      const shape = fam.name + '|' + m.panels.length + '|' + (this.theme || '');
      if (this._renderShape !== shape) {
        this._teardownCharts();
        this._renderShape = shape;
      }
      // Every panel is laid out before any width is read, so the first panel
      // fits its labels to its own track, not to the whole grid.
      m.panels.forEach((/** @type {any} */ panel, /** @type {number} */ i) => {
        this._ensureSlot(i, panel.label, m.single).facetVal = panel.facet;
      });
      const widths = m.panels.map((/** @type {any} */ _, /** @type {number} */ i) =>
        this._slots[i].chartDiv.clientWidth);
      const o = this._option(fam, m, fam.widths ? widths : null);
      m.panels.forEach((/** @type {any} */ panel, /** @type {number} */ i) => {
        const slot = this._slots[i];
        const po = o.panels[i];
        if (!po.option) {
          if (slot.scope) slot.scope.dispose();
          slot.scope = null;
          slot.chart = null;
          this._setSlotHeight(slot, po.height);
          slot.chartDiv.textContent = '';
          V.Frame.message(slot.chartDiv, NO_VALUE);
          return;
        }
        const hChanged = this._setSlotHeight(slot, po.height);
        const existed = !!slot.chart;
        const chart = this._ensureSlotChart(slot, fam);
        // What a resize needs to re-fit turned x labels (copied: the re-fit
        // updates it), plus the slot and its height without the gutter.
        chart.__xFit = po.xFit
          ? { ...po.xFit, slot, baseH: po.panelH - po.xFit.gutter } : undefined;
        if (fam.prepare) fam.prepare(slot, po);
        // When an action follows, ECharts runs it with the option in one update.
        const actionFollows = !!fam.actions && fam.actions(slot, po);
        chart.setOption(po.option, actionFollows ? { notMerge: true, lazyUpdate: true } : true);
        slot.dimmed = false;
        if (existed && hChanged) chart.resize();
        if (fam.after) fam.after(slot, po);
      });
      this.charts = this._slots.map((s) => s.chart).filter(Boolean);
      // Lazy patches: ECharts applies them in one update on the next frame.
      this._harmoniseAxes();
      const plan = NS.legendPlan(m, this.config);
      this.legend.update(plan.band ? fam.legend(m, this.config) : null, plan);
      this.frame.note(o.labelNote || null);
      if (fam.alwaysHighlight) {
        this.highlight(true);
      } else {
        this.status();
        if (this.selection.current) this._applyHighlight(true);
      }
      this.drawn = true;
    }

    /** @param {ChartFamily} fam */
    _model(fam) {
      const key = this._rowsGen + '\u0000' + this._columnsKey + '\u0000' +
        pick(this.config, fam.model.FIELDS);
      if (this._memo.modelKey !== key) {
        this._memo.model = fam.model({ rows: this.data, ix: this._ix,
                                        columns: this.columns, cfg: this.config });
        this._memo.modelKey = key;
        this._memo.optionKey = null;
      }
      return this._memo.model;
    }

    /** @param {ChartFamily} fam @param {any} m @param {number[] | null} widths */
    _option(fam, m, widths) {
      const ink = this._ink;
      const key = this._memo.modelKey + '\u0000' + pick(this.config, fam.option.FIELDS) +
        '\u0000' + JSON.stringify(ink) + '\u0000' + (this.theme || '') +
        '\u0000' + (widths ? widths.join(',') : '');
      if (this._memo.optionKey !== key) {
        this._memo.option = fam.option(m, {
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
      if (!ctx) return (s) => 7 * s.length;
      ctx.font = font;
      return (s) => ctx.measureText(s).width;
    }

    /** The axis font. @param {Ink} ink */
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

    /** Say why there is no chart where it would be. @param {VizEmpty} e */
    _showEmpty(e) {
      this._teardownCharts();
      this.legend.update(null);
      V.Frame.message(this.chartGrid, e);
      this.frame.note(null);
      this.status();
      this.drawn = true;
      this._blank = true;
    }

    /** @param {number} i @param {string | null} facetLabel @param {boolean} singleFacet
     * @returns {ChartSlot} */
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
        slot = { container, labelEl, chartDiv, resetBtn, chart: null, scope: null, facetVal: null,
                 dimmed: false, hover: { si: null }, brushable: false, zoomArmed: false, zoom: null,
                 focus: null, focusSi: null, band: null, bandFocus: null, lineClick: null };
        this._slots[i] = slot;
      }
      if (slot.labelEl) {
        slot.labelEl.textContent = facetLabel ?? '';
        slot.labelEl.style.display = facetLabel ? '' : 'none';
      }
      return slot;
    }

    /** A new ECharts instance gets the family's handlers, and a click on
     * empty plot space that clears the selection, unless a line is under
     * the cursor (the click picks it).
     * @param {ChartSlot} slot @param {ChartFamily} fam */
    _ensureSlotChart(slot, fam) {
      if (!slot.chart) {
        slot.chartDiv.textContent = '';
        const shape = this._shapeScope || (this._shapeScope = this.scope.own(new V.Scope()));
        const scope = slot.scope = shape.own(new V.Scope());
        slot.chart = scope.own(root.echarts.init(slot.chartDiv, this.theme || undefined));
        fam.attach(this, slot);
        slot.chart.getZr().on('click', (/** @type {any} */ e) => {
          if (e && e.target) return;
          if (slot.focus && slot.focusSi != null) return;
          const sel = this.selection;
          if (sel.current || sel.brushDrawn) sel.clear();
        });
      }
      return slot.chart;
    }

    /** @param {ChartSlot} slot @param {string} h */
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
