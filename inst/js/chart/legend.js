// @ts-check
/**
 * Chart block: the legend. legendPlan() says where the level names go: in
 * the band of chips below or above the plot, or on the marks themselves (at
 * a line's end, beside a stack, at a group's centre). The band is Legend,
 * owned by the view: one row of chips for every panel. A chip selects its
 * level, as a click on a mark of that level would; the swatch follows the
 * chart's mark.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const mk = NS.mk;

  // Past this many levels the names on the marks of a line crowd each other.
  const MARKS_MAX_LEVELS = 8;
  // Past this many levels a legend cannot be read: no band, no names on the
  // marks. The tooltip names the level instead.
  const LEGEND_MAX = 15;

  // `shape`: the markers its levels take, in order, cycling past the last (ECharts built-ins).
  const SHAPE_SYMBOLS = ['circle', 'triangle', 'diamond', 'rect', 'roundRect', 'pin', 'arrow'];
  // Each marker as an SVG path in a 10 x 10 box: the legend's swatch, and the picture's (export.js).
  /** @type {Record<string, string>} */
  const SHAPE_PATHS = {
    circle: 'M5 0.5 A4.5 4.5 0 1 1 5 9.5 A4.5 4.5 0 1 1 5 0.5 Z',
    triangle: 'M5 0.5 L9.5 9.5 L0.5 9.5 Z',
    diamond: 'M5 0 L10 5 L5 10 L0 5 Z',
    rect: 'M1 1 H9 V9 H1 Z',
    roundRect: 'M3.5 1 H6.5 A2.5 2.5 0 0 1 9 3.5 V6.5 A2.5 2.5 0 0 1 6.5 9 H3.5 ' +
      'A2.5 2.5 0 0 1 1 6.5 V3.5 A2.5 2.5 0 0 1 3.5 1 Z',
    pin: 'M5 10 L1.6 5.6 A4 4 0 1 1 8.4 5.6 Z',
    arrow: 'M5 0 L9.5 10 L5 7.5 L0.5 10 Z'
  };
  const SVG_NS = 'http://www.w3.org/2000/svg';

  /** A marker's swatch: a small inline SVG in the chip's ink. @param {string} symbol
   * @param {HTMLElement} parent */
  const shapeSwatch = (symbol, parent) => {
    const svg = document.createElementNS(SVG_NS, 'svg');
    svg.setAttribute('viewBox', '0 0 10 10');
    svg.setAttribute('class', 'vz-chart-legend-shape');
    svg.setAttribute('aria-hidden', 'true');
    const path = document.createElementNS(SVG_NS, 'path');
    path.setAttribute('d', SHAPE_PATHS[symbol] || SHAPE_PATHS.circle);
    svg.appendChild(path);
    parent.appendChild(svg);
    return svg;
  };

  /** Can a legend of n levels be read? @param {number} n */
  const legendFits = (n) => n <= LEGEND_MAX;

  /**
   * How the names go on the marks of this chart, or null where they have no
   * good place (the legend band stays).
   *   end       the end of each line, at its last point
   *   stack     beside the last vertical bar, at each segment's middle
   *   stack-h   above the first horizontal bar, at each segment's centre
   *   centroid  the centre of mass of each colour group's points
   *   slices    a pie or treemap names its own slices
   * @param {any} m the model @param {Record<string, any>} cfg
   * @returns {'end' | 'stack' | 'stack-h' | 'centroid' | 'slices' | null}
   */
  const marksKind = (m, cfg) => {
    if (m.family === 'individual') {
      // Chips that are not the series have no mark to sit on.
      if (!m.legend || m.byColor || !legendFits(m.legend.items.length)) return null;
      if (m.type === 'scatter') {
        return (m.x.type === 'category' || m.y.type === 'category') ? null : 'centroid';
      }
      if (m.type === 'line' || m.type === 'area' || m.type === 'band') {
        // A crowd of lines (one per entity) cannot be named.
        return (m.crowd || !m.levels.length || m.levels.length > MARKS_MAX_LEVELS) ? null : 'end';
      }
      return null;
    }
    if (m.family === 'aggregated') {
      if (m.type === 'pie' || m.type === 'treemap') return 'slices';
      if (m.type === 'bar' && !m.histogram && !m.cumulative && m.colors.length &&
          legendFits(m.colors.length) && cfg.position !== 'dodge') {
        const vertical = cfg.orientation === 'vertical';
        // A level the named bar lacks would have no name: the legend stays.
        return stackCovers(m, vertical) ? (vertical ? 'stack' : 'stack-h') : null;
      }
      if (m.type === 'pointrange' && m.split && legendFits(m.levels.length)) return 'centroid';
    }
    return null;
  };

  /** Does the bar the names go on (the last with a segment, or the first lying down) carry
   * every level, in every panel? @param {any} m the aggregated model @param {boolean} vertical */
  const stackCovers = (m, vertical) => (m.panels || []).every((/** @type {any} */ p) => {
    /** @param {any} g */
    const segs = (g) => (p.cells || []).filter((/** @type {any} */ a) =>
      String(a.group) === String(g) && a.value != null && Number(a.value) !== 0);
    const order = vertical ? [...p.groups].reverse() : p.groups;
    const g = order.find((/** @type {any} */ x) => segs(x).length);
    if (g == null) return true;
    const there = new Set(segs(g).map((/** @type {any} */ a) => String(a.color)));
    return m.colors.filter((/** @type {any} */ c) => c !== '' && c != null)
      .every((/** @type {any} */ c) => there.has(String(c)));
  });

  /** The swatch of a legend chip: a line for lines, a dot for points, else a square.
   * @param {any} type @returns {'line' | 'dot' | 'square'} */
  const swatchOf = (type) => (type === 'line' || type === 'band') ? 'line'
    : (type === 'scatter' || type === 'pointrange') ? 'dot' : 'square';

  /** What the legend does: whether the HTML band shows and where, how the names go on the
   * marks, the swatch. "marks" where the chart has no place for names keeps the band below.
   * @param {any} m the model @param {Record<string, any>} cfg
   * @returns {{ setting: string, marks: ReturnType<typeof marksKind>, band: boolean,
   *             position: 'bottom' | 'top', swatch: 'line' | 'dot' | 'square' }} */
  const legendPlan = (m, cfg) => {
    const setting = cfg.legend;
    const marks = setting === 'marks' ? marksKind(m, cfg) : null;
    return { setting, marks, band: setting !== 'none' && !marks,
             position: setting === 'top' ? 'top' : 'bottom', swatch: swatchOf(cfg.type) };
  };

  /** The mean of a group's finite x and y, or null without a point.
   * @param {any[][]} pts @returns {[number, number] | null} */
  const cloudCentre = (pts) => {
    let sx = 0, sy = 0, n = 0;
    for (const p of pts) {
      const x = Number(p[0]), y = Number(p[1]);
      if (p[0] == null || p[1] == null || !Number.isFinite(x) || !Number.isFinite(y)) continue;
      sx += x; sy += y; n++;
    }
    return n ? [sx / n, sy / n] : null;
  };

  /** A name written at a point and nothing else: a silent scatter series with no symbol and a
   * haloed label. The id keeps the highlight patch off it.
   * @param {string} id @param {string} name @param {number[]} at @param {string} position
   * @param {any} ink @param {Record<string, any>} [extra] */
  const nameMark = (id, name, at, position, ink, extra) => ({
    id, type: 'scatter', data: [at], silent: true, symbolSize: 0, z: 11,
    legendHoverLink: false, animation: false, tooltip: { show: false },
    emphasis: { disabled: true },
    label: {
      show: true, formatter: name, position,
      color: ink.text, fontSize: ink.fontSize, fontWeight: 600,
      textBorderColor: ink.surface, textBorderWidth: 3,
      ...extra
    }
  });

  class Legend {
    /** @param {ChartView} view @param {HTMLElement} parent */
    constructor(view, parent) {
      this.view = view;
      this.el = mk('div', 'vz-chart-legend', parent);
      this.el.hidden = true;
      /** The column the chips are levels of. @type {string | null} */
      this.col = null;
      /** Owns the chips' listeners. @type {VizScope | null} */
      this._scope = null;
    }

    /**
     * Draw the chips, or hide the band with no spec or more chips than can be read. `shapes`, the
     * markers of a `shape` column, follow the colour chips as a group of their own.
     * @param {{ col: string | null, items: Array<{ name: string, color: string }>,
     *           shapes?: { col: string, items: Array<{ name: string, symbol: string }> } | null } | null} spec
     * @param {{ position: 'top' | 'bottom', swatch: string }} [plan]
     */
    update(spec, plan) {
      const el = this.el;
      const colours = !!spec && !!spec.col && spec.items.length > 0 && legendFits(spec.items.length);
      const shapes = spec && spec.shapes && spec.shapes.items.length && legendFits(spec.shapes.items.length)
        ? spec.shapes : null;
      if (!spec || !plan || (!colours && !shapes)) {
        el.hidden = true;
        el.textContent = '';
        this.col = null;
        return;
      }
      this.col = colours ? spec.col : null;
      this._place(plan.position === 'top');
      el.textContent = '';
      if (this._scope) this._scope.dispose();
      const scope = this._scope = this.view.scope.own(new V.Scope());
      if (colours) this._colourChips(/** @type {any} */ (spec), plan, scope);
      if (shapes) this._shapeChips(shapes);
      el.hidden = false;
      this.mark(this.view.selection.current);
    }

    /** The colour chips, after the colour column's title.
     * @param {{ col: string, items: Array<{ name: string, color: string }> }} spec
     * @param {{ swatch: string }} plan @param {VizScope} scope */
    _colourChips(spec, plan, scope) {
      const el = this.el;
      // Which column the chips are levels of, before them: its label, else
      // its name. Not when a level already reads the same.
      const meta = NS.axes.colMeta(this.view.columns, spec.col);
      const title = String((meta && meta.label) || spec.col);
      if (!spec.items.some((it) => String(it.name) === title)) {
        mk('span', 'vz-chart-legend-title', el).textContent = title;
      }
      for (const it of spec.items) {
        const chip = /** @type {HTMLButtonElement} */ (mk('button', 'vz-chart-legend-chip', el));
        chip.type = 'button';
        chip.dataset.level = it.name;
        const sw = mk('span', 'vz-chart-legend-swatch vz-chart-legend-swatch--' + plan.swatch, chip);
        sw.style.background = it.color;
        mk('span', '', chip).textContent = it.name;
        scope.on(chip, 'click', () => this._click(spec.col, it.name));
      }
    }

    /** The shape chips: the column's label, else its name, then a marker and a name per level.
     * They select nothing. @param {{ col: string, items: Array<{ name: string, symbol: string }> }} shapes */
    _shapeChips(shapes) {
      const el = this.el;
      const meta = NS.axes.colMeta(this.view.columns, shapes.col);
      mk('span', 'vz-chart-legend-title vz-chart-legend-title--shape', el).textContent =
        String((meta && meta.label) || shapes.col);
      for (const it of shapes.items) {
        const chip = mk('span', 'vz-chart-legend-chip vz-chart-legend-chip--shape', el);
        chip.dataset.shape = it.symbol;
        shapeSwatch(it.symbol, chip);
        mk('span', '', chip).textContent = it.name;
      }
    }

    /** Above or below the panel grid. @param {boolean} top */
    _place(top) {
      const el = this.el;
      const grid = this.view.chartGrid;
      el.classList.toggle('vz-chart-legend--top', top);
      const inPlace = top ? el.nextElementSibling === grid : el.previousElementSibling === grid;
      if (!inPlace) grid.parentNode?.insertBefore(el, top ? grid : grid.nextSibling);
    }

    // A chip's level as a filter, or with an explicit drill column that
    // column's values in the level's rows. Nothing with drill off.
    /** @param {string} col @param {string} level */
    _click(col, level) {
      const v = this.view;
      const cfg = v.settings;
      if (NS.drillState(cfg) === 'off') return;
      const filters = cfg.drill === 'auto'
        ? NS.keys.fromKeys({ [col]: level })
        : NS.keys.fromRows(cfg.drill, NS.model.rowsUnder(v.data, v._ix, { [col]: level }));
      if (filters) v.selection.click(null, null, filters);
    }

    /** Ring the chips of the selected levels. @param {ChartSelection | null} sel */
    mark(sel) {
      if (!this.col) return;
      const f = sel && sel.filters;
      const vals = f && f[this.col];
      const on = vals ? new Set(vals.map((v) => (NS.keys.isMissing(v) ? '' : String(v)))) : null;
      for (const chip of this.el.querySelectorAll('button.vz-chart-legend-chip')) {
        const c = /** @type {HTMLElement} */ (chip);
        c.classList.toggle('vz-chart-legend-chip--on', !!on && on.has(c.dataset.level || ''));
      }
    }
  }

  Object.assign(NS, { legendPlan, legendFits, cloudCentre, nameMark, Legend, SHAPE_SYMBOLS, SHAPE_PATHS });
})();
