// @ts-check
/**
 * Chart block: the facet panels' layout. A panel grid (facet_row) puts each
 * panel at its row and column with strips on top and on the right; the
 * panels' numeric axes are shared across a wrap or along a grid's rows and
 * columns, and a grid labels only its outer axes.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const mk = NS.mk;
  // The bottom margin of a grid panel whose x labels are hidden.
  const X_HIDDEN_BOTTOM = 10;

  /** In a grid, every row gets its grid key (NS.model.gridKey()). New keys
   * are new rows to the model and the index.
   * @param {ChartView} view @param {boolean} rowsChanged */
  const gridRows = (view, rowsChanged) => {
    const g = view.settings.facet_grid;
    const keep = NS.model.gridKeepsMissing(view.settings);
    const key = g ? g.row + '\u0000' + (g.col || '') + '\u0000' + keep : '';
    if (!rowsChanged && key === view._gridKeyOf) return;
    view._gridKeyOf = key;
    if (!g) return;
    const col = NS.model.GRID_COL;
    for (const r of view.data) r[col] = NS.model.gridKey(r, g, keep);
    if (!rowsChanged) {
      view._ix = new NS.RowIndex();
      view._rowsGen++;
    }
  };

  /** The grid's rows and columns, in level order, and the grid row the
   * panels start on (2 below a strip of column labels), or null without a
   * grid. A combination without rows has no panel and leaves its cell empty.
   * @param {ChartView} view @param {any} m @returns {ChartGridLayout | null} */
  const gridLayout = (view, m) => {
    const g = view.settings.facet_grid;
    if (!g || !m.panels.length) return null;
    const rows = new Set(), cols = new Set();
    for (const p of m.panels) {
      if (p.facet == null || p.facet === '__all__') return null;
      const lv = NS.model.gridLevels(p.facet);
      rows.add(lv.row);
      cols.add(lv.col);
    }
    const order = (/** @type {Set<string>} */ s) => [...s].sort();
    // A strip reads "(missing)" for a missing level and, when show_n covers
    // facets, counts the rows (or ids) its panels draw.
    const drawn = view.data.filter((r) => r[NS.model.GRID_COL] != null);
    /** @param {string | null} col */
    const labeller = (col) => {
      const counts = col && NS.countOn(view.settings, 'facet') ? NS.labelCounts(drawn, view.settings, col) : null;
      return (/** @type {string} */ lv) => {
        const n = counts ? NS.withCount(lv, counts).slice(lv.length) : '';
        return (lv === '' ? '(missing)' : lv) + n;
      };
    };
    const rowLab = labeller(g.row), colLab = labeller(g.col);
    const r = order(rows), c = order(cols);
    return { rows: r, cols: c, top: g.col ? 2 : 1, rowLabels: r.map(rowLab), colLabels: c.map(colLab) };
  };

  /** A panel's cell: its grid row and column, after the strip of column
   * labels when there is one. @param {ChartSlot} slot
   * @param {ChartGridLayout | null} layout @param {any} facet */
  const place = (slot, layout, facet) => {
    const st = slot.container.style;
    if (!layout) {
      st.gridRow = st.gridColumn = '';
      slot.gridRow = slot.gridCol = null;
      return;
    }
    const lv = NS.model.gridLevels(facet);
    slot.gridRow = layout.rows.indexOf(lv.row);
    slot.gridCol = layout.cols.indexOf(lv.col);
    st.gridRow = String(slot.gridRow + layout.top);
    st.gridColumn = String(slot.gridCol + 1);
  };

  /** The grid's strips: column levels on top, row levels on the right, as in
   * ggplot2's facet_grid(). Rebuilt on every draw.
   * @param {ChartView} view @param {ChartGridLayout | null} layout */
  const strips = (view, layout) => {
    for (const el of view._stripEls) el.remove();
    view._stripEls = [];
    if (!layout) return;
    const top = layout.top;
    const add = (/** @type {string} */ cls, /** @type {string} */ text,
                 /** @type {number} */ row, /** @type {number} */ col) => {
      const el = mk('div', 'vz-chart-strip ' + cls, view.chartGrid);
      el.textContent = text;
      el.title = text;
      el.style.gridRow = String(row);
      el.style.gridColumn = String(col);
      view._stripEls.push(el);
    };
    if (top === 2) {
      layout.colLabels.forEach((c, j) => add('vz-chart-strip--col', c, 1, j + 1));
    }
    layout.rowLabels.forEach((r, i) => add('vz-chart-strip--row', r, i + top, layout.cols.length + 1));
  };

  /** A row strip as tall as its row's panels at most, so a long label wraps
   * into a second line instead of making the row taller. Run once the
   * panels have their heights. @param {ChartView} view */
  const fitStrips = (view) => {
    /** @type {Map<number, number>} */ const tall = new Map();
    for (const s of view._slots) {
      if (!s || s.gridRow == null) continue;
      // The chart's own height: the container stretches to the row, which a
      // long strip may have made taller.
      const h = parseFloat(s.chartDiv.style.height) || s.chartDiv.getBoundingClientRect().height;
      tall.set(s.gridRow, Math.max(tall.get(s.gridRow) || 0, h));
    }
    let i = 0;
    for (const el of view._stripEls) {
      if (!el.classList.contains('vz-chart-strip--row')) continue;
      const h = tall.get(i++);
      el.style.maxHeight = h ? Math.floor(h) + 'px' : '';
    }
  };

  /** In a grid, which axis carries the value and how each axis is shared:
   * the value axis within a row ("free_y", "free") or across all panels
   * ("fixed"); the other axis within a column ("free") or across all. The
   * value runs along x on horizontal bars and on a gantt. Null outside a grid.
   * @param {ChartSettings} cfg
   * @returns {{ valueKey: string, otherKey: string, valueBy: 'row' | 'all', otherBy: 'col' | 'all' } | null} */
  const share = (cfg) => {
    if (!cfg.facet_grid) return null;
    const valueOnX = cfg.type === 'gantt' ||
      (NS.familyOf(cfg.type) === 'aggregated' && cfg.orientation === 'horizontal');
    return {
      valueKey: valueOnX ? 'xAxis' : 'yAxis',
      otherKey: valueOnX ? 'yAxis' : 'xAxis',
      valueBy: cfg.facet_scales === 'fixed' ? 'all' : 'row',
      otherBy: cfg.facet_scales === 'free' ? 'col' : 'all'
    };
  };

  // One numeric domain across the panels ("fixed"), read back off what each drew.
  // In a grid see share(). A shared value axis lands on a nice step.
  /** @param {ChartView} view */
  const harmonise = (view) => {
    const mode = NS.facetScales(view.settings);
    const gs = share(view.settings);
    if (mode === 'free' && !gs) return;
    const slots = view._slots.filter((s) => s && s.chart);
    if (slots.length < 2) return;
    /** @param {string} key @param {ChartSlot[]} group */
    const shareAxis = (key, group) => {
      if (group.length < 2) return;
      let type = null;
      const extents = group.map((s) => {
        const comp = s.chart.getModel().getComponent(key, 0);
        if (!comp || comp.get('type') === 'category') return null;
        type = comp.get('type');
        return comp.axis.scale.getExtent();
      });
      const d = NS.axes.sharedDomain(extents);
      if (!d) return;
      /** @type {Record<string, number>} */ let patch = { min: d[0], max: d[1] };
      if (type === 'value') {
        const [min, max, interval] = NS.axes.niceShared(d[0], d[1]);
        patch = { min, max, interval };
      }
      // A set limit wins over the shared domain; the axis then picks its own steps.
      const roles = group[0].chart.__vzRoles;
      const [lo, hi] = NS.zoomOption.ends(view.settings, roles ? roles[key] : null, String(type));
      if (lo != null || hi != null) patch = { min: lo ?? patch.min, max: hi ?? patch.max };
      for (const s of group) s.chart.setOption({ [key]: [patch] }, { lazyUpdate: true });
    };
    if (!gs) {
      shareAxis('xAxis', slots);
      if (mode === 'fixed') shareAxis('yAxis', slots);
      return;
    }
    for (const g of groups(slots, gs.valueBy)) shareAxis(gs.valueKey, g);
    for (const g of groups(slots, gs.otherBy)) shareAxis(gs.otherKey, g);
  };

  /** The slots by grid row, by grid column, or all in one group.
   * @param {ChartSlot[]} slots @param {'row' | 'col' | 'all'} by @returns {ChartSlot[][]} */
  const groups = (slots, by) => {
    if (by === 'all') return [slots];
    /** @type {Map<any, ChartSlot[]>} */ const out = new Map();
    for (const s of slots) {
      const k = by === 'row' ? s.gridRow : s.gridCol;
      if (!out.has(k)) out.set(k, []);
      /** @type {ChartSlot[]} */ (out.get(k)).push(s);
    }
    return [...out.values()];
  };

  /** In a grid, as in ggplot2: y labels and title on the first panel of a
   * row, x labels and title on the lowest panel of a column, when that axis
   * is shared along the row or column. The margins stay, so the panels line up.
   * @param {ChartView} view */
  const outerAxes = (view) => {
    const gs = share(view.settings);
    const slots = view._slots.filter((s) => s && s.chart);
    for (const s of slots) s.outerHidden = null;
    if (!gs) return;
    const yAlongRow = gs.valueKey === 'yAxis' || gs.otherBy === 'all';
    const xAlongCol = gs.otherKey === 'xAxis' || gs.valueBy === 'all';
    /** @type {Map<any, number>} */ const firstCol = new Map();
    /** @type {Map<any, number>} */ const lastRow = new Map();
    for (const s of slots) {
      const c = /** @type {number} */ (s.gridCol), r = /** @type {number} */ (s.gridRow);
      if (!firstCol.has(s.gridRow) || c < /** @type {number} */ (firstCol.get(s.gridRow))) firstCol.set(s.gridRow, c);
      if (!lastRow.has(s.gridCol) || r > /** @type {number} */ (lastRow.get(s.gridCol))) lastRow.set(s.gridCol, r);
    }
    const off = { axisLabel: { show: false }, name: '' };
    for (const s of slots) {
      const hide = {
        x: xAlongCol && s.gridRow !== lastRow.get(s.gridCol),
        y: yAlongRow && s.gridCol !== firstCol.get(s.gridRow)
      };
      /** @type {Record<string, any>} */ const patch = {};
      const has = (/** @type {string} */ key) => !!s.chart.getModel().getComponent(key, 0);
      if (hide.x && has('xAxis')) patch.xAxis = [off];
      if (hide.y && has('yAxis')) patch.yAxis = [off];
      if (!patch.xAxis && !patch.yAxis) continue;
      s.outerHidden = hide;
      // Hidden x labels give their gutter back: the grid's bottom margin and
      // the panel shrink alike, so the plot keeps its height and lines up.
      const px = parseFloat(s.chartDiv.style.height);
      const grid = (s.chart.getOption().grid || [])[0] || {};
      const cut = Number(grid.bottom) - X_HIDDEN_BOTTOM;
      const shrink = patch.xAxis && cut > 0 && Number.isFinite(px);
      if (shrink) patch.grid = { bottom: X_HIDDEN_BOTTOM };
      s.chart.setOption(patch, { lazyUpdate: true });
      if (shrink && view._setSlotHeight(s, Math.round(px - cut) + 'px')) s.chart.resize();
    }
  };

  NS.panels = { gridRows, layout: gridLayout, place, strips, fitStrips, share, harmonise, groups, outerAxes };
})();
