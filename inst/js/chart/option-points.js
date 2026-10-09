// @ts-check
/**
 * Chart block: the scatter and line series of the individual option.
 *
 * Per model series: the points (a scatter, or a line in its connect mode),
 * then R's smoother for that series (scatter only) and its lo/hi error
 * bars. A smoother or an error bar shares its series' name, so one legend
 * toggle takes them together. A line chart also gets the hover overlay
 * (a veil, the hovered line and its dots), which interact-individual.js
 * fills, and an axis tooltip that follows the hovered line.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});
  const option = /** @type {any} */ (NS.option = NS.option || {});
  const parts = /** @type {any} */ (option.indParts = option.indParts || {});

  const BASE_LINE_WIDTH = 1.4;
  const BASE_SCATTER_SIZE = 6;
  const BASE_LINE_MARKER = 4;
  // Rows an axis tooltip lists before "+n more".
  const TT_ROW_CAP = 12;

  /**
   * The scatter or line series of one panel, with their overlays.
   * @param {any} m the individual model @param {any} panel @param {any} look
   */
  const points = (m, panel, look) => {
    const { cfg } = look;
    const isLine = m.isLine;
    const dm = cfg.dot_size_mult ?? 1.0;
    const lm = cfg.line_width_mult ?? 1.0;
    const lineMarkerPx = m.symbol === 'none' ? BASE_LINE_MARKER : BASE_LINE_MARKER * dm;
    // One select drives step and smoothing: monotone (the default),
    // straight segments, or a step at the start, middle or end.
    const connect = cfg.connect || 'monotone';
    const stepMode = connect.indexOf('step-') === 0 ? connect.slice(5) : null;
    const smoothOn = isLine && connect === 'monotone';
    const smoother = cfg.smoother || 'none';
    const smootherSeries = cfg.smoother_series || null;
    const loCol = cfg.lo, hiCol = cfg.hi;
    const encodeX = (/** @type {any} */ v) => m.x.type === 'category' ? String(v ?? '') : Number(v);

    // Native emphasis is off on both: a line's hover highlight is the
    // overlay, and a scatter's would strobe a dense cloud.
    const mkSeries = (/** @type {any} */ name, /** @type {any[]} */ data, /** @type {string} */ clr) => ({
      type: isLine ? 'line' : 'scatter',
      name,
      data,
      step: isLine && stepMode ? stepMode : undefined,
      smooth: smoothOn ? true : undefined,
      smoothMonotone: smoothOn ? 'x' : undefined,
      // Clicks and hover on the line itself, not only on its symbols.
      triggerLineEvent: isLine ? true : undefined,
      symbol: isLine ? m.symbol : 'circle',
      symbolSize: isLine ? lineMarkerPx : BASE_SCATTER_SIZE * dm,
      itemStyle: { color: clr, cursor: 'pointer' },
      lineStyle: isLine ? { width: BASE_LINE_WIDTH * lm, opacity: m.lineOpacity } : undefined,
      emphasis: { disabled: true }
    });

    // R fits the smoother per group, nested by facet level when faceted.
    const smootherLine = (/** @type {any} */ groupName) => {
      if (smoother === 'none' || !smootherSeries) return null;
      const bucket = (m.facet && panel.facet !== '__all__')
        ? smootherSeries[panel.facet] : smootherSeries;
      if (!bucket) return null;
      const s = bucket[groupName != null ? String(groupName) : '__all__'];
      if (!s || !s.x || !s.y) return null;
      /** @type {any[]} */
      const out = [];
      for (let i = 0; i < s.x.length; i++) {
        if (Number.isFinite(s.x[i]) && Number.isFinite(s.y[i])) out.push([s.x[i], s.y[i]]);
      }
      return out.length >= 2 ? out : null;
    };

    /** @type {any[]} */
    const series = [];
    for (const s of panel.series) {
      series.push(mkSeries(s.level, s.pts, s.color));
      if (smoother !== 'none' && !isLine) {
        const ln = smootherLine(s.level);
        if (ln) {
          series.push({
            type: 'line', name: s.level || 'fit', legendHoverLink: false, data: ln,
            silent: true, showSymbol: false,
            lineStyle: { color: s.color, width: 2, type: 'solid', opacity: 0.9 }, z: 2
          });
        }
      }
      // Error bars, on a line and on a scatter (a forest plot is a scatter
      // with lo/hi whiskers).
      if (loCol && hiCol && s.src.length) {
        const errPts = s.src
          .filter((/** @type {any} */ r) => r[m.x.col] != null && r[loCol] != null && r[hiCol] != null)
          .map((/** @type {any} */ r) => [encodeX(r[m.x.col]), Number(r[loCol]), Number(r[hiCol])]);
        if (errPts.length) {
          series.push(option.aggParts.whiskerSeries(s.level || 'errbar', errPts, s.color, false));
        }
      }
    }
    return { series, stepMode, smoothOn, markerPx: 8 * dm, focusW: 2.5 * lm };
  };

  /**
   * The hover overlay of a line chart, mounted on every line chart (empty
   * until a line is hovered) so the series keep one shape: a veil over the
   * plot on its own hidden 0..1 axes, then the hovered line and its dots.
   * `state.veil` is the veil's fill, set when a line is promoted.
   * @param {{ veil?: string }} state
   */
  const lineOverlay = (state) => [{
    id: '__scrim__', type: 'custom', xAxisIndex: 1, yAxisIndex: 1, data: [],
    silent: true, legendHoverLink: false, z: 8, animation: false,
    renderItem: (/** @type {any} */ params) => {
      const cs = params.coordSys;
      return { type: 'rect', shape: { x: cs.x, y: cs.y, width: cs.width, height: cs.height },
               style: { fill: state.veil || 'rgba(255,255,255,0.6)' } };
    }
  }, {
    id: '__focus__', name: '__focus__', type: 'line', data: [], silent: true,
    showSymbol: false, legendHoverLink: false, z: 9, animation: false
  }, {
    id: '__focus_dots__', name: '__focus_dots__', type: 'scatter', data: [], silent: true,
    legendHoverLink: false, z: 10, animation: false
  }];

  /**
   * The line chart's tooltip: per x position, each line's value there,
   * capped. While a line is hovered (`hover.si`): with one line per colour
   * level, every line's row with that one marked (the colour-split card);
   * with one line per series (a subject), that line's row only. Away from
   * every line of a split chart, none.
   * @param {any} m @param {any} look @param {{ si: number | null }} hover
   */
  const lineTooltip = (m, look, hover) => {
    const title = (/** @type {any} */ col) => NS.axes.axisTitle(look.columns, col);
    const ttSuffix = ttSuffixOf(m, look);
    return {
      trigger: 'axis',
      axisPointer: { type: 'line' },
      confine: true,
      formatter: (/** @type {any} */ ps) => {
        if (!Array.isArray(ps)) ps = [ps];
        let rows = ps.filter((/** @type {any} */ p) =>
          p && p.seriesType === 'line' && Array.isArray(p.value) && p.seriesName !== '__focus__');
        if (!rows.length) return '';
        if (hover.si == null && m.seriesCount > 1) return '';
        const byColor = !m.seriesCol && !!m.color && m.seriesCount > 1;
        if (hover.si != null && !byColor) {
          rows = rows.filter((/** @type {any} */ p) => p.seriesIndex === hover.si);
          if (!rows.length) return '';
        }
        // One row per series, the first of replicates at one x.
        const seen = new Set();
        rows = rows.filter((/** @type {any} */ p) => {
          if (seen.has(p.seriesIndex)) return false;
          seen.add(p.seriesIndex);
          return true;
        });
        const head = m.x.cats
          ? (rows[0].axisValueLabel ?? String(rows[0].value[0]))
          : title(m.x.col) + ': ' + NS.ddNum3(rows[0].value[0]);
        // The hovered line first, so the cap never hides it.
        if (byColor) {
          rows.sort((/** @type {any} */ a, /** @type {any} */ b) =>
            (b.seriesIndex === hover.si ? 1 : 0) - (a.seriesIndex === hover.si ? 1 : 0));
        }
        // A colour level is named with its column, as on a split bar.
        const colorName = byColor ? NS.esc(title(m.color) || m.color) + ' ' : '';
        const lines = rows.slice(0, TT_ROW_CAP).map((/** @type {any} */ p) => {
          const nm = m.splitCol ? colorName + NS.esc(p.seriesName) : NS.esc(title(m.y.col));
          return NS.tipRow(nm, NS.ddNum3(p.value[1]) + ttSuffix(p.value),
                           m.splitCol ? p.color : null,
                           byColor ? p.seriesIndex === hover.si : null);
        });
        if (rows.length > TT_ROW_CAP) lines.push(NS.tipNote('+' + (rows.length - TT_ROW_CAP) + ' more'));
        return NS.tipHead(NS.esc(head)) + lines.join('');
      }
    };
  };

  /**
   * The tooltip fields as a "  (col: v, col2: v2)" suffix on a line's row.
   * @param {any} m @param {any} look
   */
  const ttSuffixOf = (m, look) => (/** @type {any} */ val) => {
    if (!m.ttFields.length || !Array.isArray(val)) return '';
    const out = [];
    for (let i = 0; i < m.ttFields.length; i++) {
      const v = val[2 + i];
      if (v != null && v !== '') {
        out.push(NS.esc(NS.axes.axisTitle(look.columns, m.ttFields[i]) || m.ttFields[i]) + ': ' +
                 NS.esc(typeof v === 'number' ? NS.ddNum3(v) : v));
      }
    }
    return out.length ? '  (' + out.join(', ') + ')' : '';
  };

  /**
   * The point tooltip (scatter and band): the series level as headline,
   * else the y title; x and y by their titles, the split column, then the
   * tooltip fields.
   * @param {any} m @param {any} look
   */
  const pointTooltip = (m, look) => {
    const title = (/** @type {any} */ col) => NS.axes.axisTitle(look.columns, col);
    return {
      trigger: 'item', confine: true,
      formatter: (/** @type {any} */ p) => {
        const lvl = (m.splitCol && p.seriesName) ? p.seriesName : null;
        const headline = lvl || title(m.y.col) || 'Point';
        /** @type {Array<[string, any]>} */
        const pairs = [
          [title(m.x.col) || 'X', NS.axes.fmtXVal(p.value[0], m.x.type, null)],
          [title(m.y.col) || 'Y', NS.ddNum3(p.value[1])]
        ];
        if (lvl) pairs.push([title(m.splitCol) || 'Series', lvl]);
        m.ttFields.forEach((/** @type {string} */ c, /** @type {number} */ i) => {
          const v = p.value[2 + i];
          pairs.push([title(c) || c, typeof v === 'number' ? NS.ddNum3(v) : v]);
        });
        return NS.rowTooltip(headline, pairs, lvl ? p.color : null);
      }
    };
  };

  Object.assign(parts, { points, lineOverlay, lineTooltip, pointTooltip,
                         BASE_LINE_WIDTH, BASE_SCATTER_SIZE });
})();
