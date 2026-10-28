// @ts-check
/**
 * Chart block: the scatter and line series of the individual option. Per
 * model series: the points, R's smoother (scatter only) and the lo/hi error
 * bars, which share its name so one legend toggle takes them together. A line
 * chart also gets the hover overlay and an axis tooltip.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const option = /** @type {any} */ (NS.option = NS.option || {});
  const parts = /** @type {any} */ (option.indParts = option.indParts || {});

  // A crowd of lines (one per entity) stays thin so it reads as a texture.
  const BASE_LINE_WIDTH = 2;
  const CROWD_LINE_WIDTH = 1.4;
  const BASE_SCATTER_SIZE = 6;
  const BASE_LINE_MARKER = 9;
  // A line's dots show only while points are at least SPARSE_DOT_PX apart (at
  // most SPARSE_DOT_N points without a width); closer, they hide the line.
  const SPARSE_DOT_PX = 24;
  const SPARSE_DOT_N = 20;
  // Rows an axis tooltip lists before "+n more".
  const TT_ROW_CAP = 12;
  // The dash of each `linetype` level after the first, which is solid.
  const DASHES = [[6, 4], [2, 3], [8, 3, 2, 3], [1, 2]];
  // Each nested interval adds this opacity, so the inner one reads darker.
  const BAND_OPACITY = 0.14;
  // The id of a `linetype` segment: the tooltip folds a line's segments into one row.
  const SEG_ID = /^__seg_(\d+)_/;

  /** The name at a line's end, nudged apart from its neighbours' names.
   * @param {any} ink */
  const endLabelOf = (ink) => ({
    endLabel: {
      show: true, formatter: '{a}', distance: 6,
      color: ink.text, fontSize: ink.fontSize, fontWeight: 600
    },
    labelLayout: { moveOverlap: 'shiftY' }
  });

  /** Room on the right for end labels: the longest name, 6.5 px a character, up to 160.
   * @param {any[]} names */
  const endLabelPad = (names) => {
    const n = Math.max(0, ...names.map((s) => String(s ?? '').length));
    return n ? Math.min(160, n * 6.5 + 12) : 0;
  };

  /** Whether a panel's lines are sparse enough to carry dots.
   * @param {any} panel @param {number} plotW the plot's width in px, 0 if unknown */
  const sparseLines = (panel, plotW) => {
    let n = 0;
    for (const s of panel.series) if (s.pts.length > n) n = s.pts.length;
    if (!n) return false;
    if (plotW > 0) return n < 2 || plotW / (n - 1) >= SPARSE_DOT_PX;
    return n <= SPARSE_DOT_N;
  };

  /** A `labelLayout` that moves a label up or down, a line at a time, until it covers no label
   * placed before it (`moveOverlap` ignores scatter labels). Spacers sharing a `room` place their
   * labels around each other; a label that comes before the last one seen starts a new layout pass.
   * @param {string} position 'right', 'top' or 'inside' @param {number} distance
   * @param {{ placed: any[], lastS: number, lastD: number }} [room] */
  const labelSpacer = (position, distance, room) => {
    const r = room || { placed: [], lastS: -1, lastD: -1 };
    return (/** @type {any} */ p) => {
      const si = p.seriesIndex ?? 0;
      if (si < r.lastS || (si === r.lastS && p.dataIndex <= r.lastD)) r.placed = [];
      r.lastS = si;
      r.lastD = p.dataIndex;
      const placed = r.placed;
      const l = p.labelRect, s = p.rect, h = l.height;
      const y = position === 'top' ? s.y - distance - h : s.y + s.height / 2 - h / 2;
      const hit = (/** @type {number} */ dy) => placed.some((q) =>
        l.x < q.x + q.width && q.x < l.x + l.width && y + dy < q.y + q.height && q.y < y + dy + h);
      let dy = 0;
      for (let k = 1; k <= 12 && hit(dy); k++) dy = (k % 2 ? -1 : 1) * Math.ceil(k / 2) * h;
      placed.push({ x: l.x, y: y + dy, width: l.width, height: h });
      return { dy };
    };
  };

  /** What every part of a panel's points reads. @param {any} m the individual model
   * @param {any} panel @param {any} look @param {number} plotW the plot's width in px */
  const pointsContext = (m, panel, look, plotW) => {
    const { cfg, ink } = look;
    const isLine = m.isLine;
    const dm = cfg.point_size;
    const lm = cfg.line_width;
    const crowd = isLine && m.crowd;
    // An area's fill already shows where the values are; dots would clutter it.
    const dots = isLine && !crowd && cfg.type !== 'area' &&
      m.symbol !== 'none' && sparseLines(panel, plotW);
    const stepMode = cfg.connect.indexOf('step_') === 0 ? cfg.connect.slice(5) : null;
    // On a log axis an interval end at or below zero: the low end is cut at
    // the axis floor, a high end there drops the interval.
    const floorY = look.logFloorY ?? null;
    const floorX = look.logFloorX ?? null;
    /** @param {any} v */
    const loEnd = (v) => (v == null ? null
      : (floorY != null && !(Number(v) > 0) ? floorY : Number(v)));
    /** @param {any} v */
    const hiEnd = (v) => (v == null || (floorY != null && !(Number(v) > 0)) ? null : Number(v));
    const isArea = cfg.type === 'area';
    // `legend: "marks"` names the marks: at each line's end, or at the centre
    // of each cloud of points.
    const plan = NS.legendPlan(m, cfg);
    const muted = NS.mutedMark(ink);

    return {
      cfg, ink, m, panel, isLine, dm, lm, crowd, dots, stepMode, floorX, loEnd, hiEnd, isArea,
      lineMarkerPx: BASE_LINE_MARKER * dm,
      lineW: (crowd ? CROWD_LINE_WIDTH : BASE_LINE_WIDTH) * lm,
      surface: ink.surface,
      smoothOn: isLine && cfg.connect === 'monotone',
      smoother: cfg.smoother,
      smootherSeries: cfg.derived.smoother,
      pairs: parts.log.intervalPairs(cfg),
      // A colour split stacks its areas, unless the layout says side by side.
      stackArea: isArea && m.splitCol && cfg.position !== 'dodge',
      ltCol: isLine && cfg.linetype ? cfg.linetype : null,
      directLabels: isLine && plan.marks === 'end',
      cloudLabels: !isLine && plan.marks === 'centroid',
      encodeX: m.x.encode ||
        ((/** @type {any} */ v) => m.x.type === 'category' ? String(v ?? '') : Number(v)),
      // `highlight`: the muted series in grey; the highlighted lines named at their end.
      clrOf: (/** @type {any} */ s) => (s.muted ? muted : s.color),
      hlNames: isLine && !!m.hl && !!m.splitCol
    };
  };

  /** One scatter or line series. A scatter's native emphasis is off (it would strobe a dense
   * cloud); a line keeps it, without scaling, for the dot at the crosshair; the crowd's hover is
   * the overlay. @param {any} k @param {any} name @param {any[]} data @param {string} clr
   * @param {boolean} [isMuted] */
  const mkSeries = (k, name, data, clr, isMuted = false) => {
    const { isLine, stepMode, smoothOn, crowd, lineMarkerPx, dm, dots, surface, lineW, m, isArea,
            stackArea, directLabels, hlNames, ink } = k;
    return /** @type {any} */ ({
      type: isLine ? 'line' : 'scatter',
      name,
      data,
      step: isLine && stepMode ? stepMode : undefined,
      smooth: smoothOn ? true : undefined,
      smoothMonotone: smoothOn ? 'x' : undefined,
      // Clicks and hover on the line itself, not only on its symbols.
      triggerLineEvent: isLine ? true : undefined,
      symbol: isLine && crowd ? 'none' : 'circle',
      symbolSize: isLine ? lineMarkerPx : BASE_SCATTER_SIZE * dm,
      showSymbol: isLine ? dots : undefined,
      itemStyle: isLine
        ? { color: clr, cursor: 'pointer', borderColor: surface, borderWidth: 1.5 }
        : { color: clr, cursor: 'pointer' },
      lineStyle: isLine ? { width: lineW, opacity: m.lineOpacity, cap: 'round', join: 'round' } : undefined,
      emphasis: (isLine && !crowd)
        ? { scale: false, focus: 'none', lineStyle: { width: lineW } }
        : { disabled: true },
      ...(isArea ? { areaStyle: { opacity: stackArea ? 0.7 : 0.25 },
                     ...(stackArea ? { stack: 'area' } : {}) } : {}),
      // The drill patch tells the muted part of a highlight from the lit part by this.
      ...(isMuted ? { blockrMuted: true } : {}),
      ...((m.hl ? (directLabels || hlNames) && !isMuted : directLabels) ? endLabelOf(ink) : {})
    });
  };

  /** `linetype`: the levels in the order the rows first carry them; the first is solid.
   * @param {any} k @returns {string[]} */
  const lineTypeLevels = ({ ltCol, panel }) => {
    const seen = new Set();
    if (ltCol) for (const s of panel.series) for (const r of s.rows) seen.add(String(r[ltCol] ?? ''));
    return [...seen];
  };

  /** One line split at its linetype levels; a segment takes the level of the point it leads to, so
   * a forecast is dashed from the last observed point on. @param {any} k @param {string[]} ltLevels
   * @param {any} s @param {number} si */
  const lineSegments = (k, ltLevels, s, si) => {
    const { ltCol, clrOf } = k;
    const lv = s.rows.map((/** @type {any} */ r) => String(r[ltCol] ?? ''));
    return ltLevels.map((L, j) => {
      const data = s.pts.map((/** @type {any} */ p, /** @type {number} */ i) =>
        (lv[i] === L || lv[i + 1] === L) ? p : [p[0], null]);
      const sr = mkSeries(k, s.level, data, clrOf(s), !!s.muted);
      sr.id = '__seg_' + si + '_' + j;
      if (j > 0) {
        sr.lineStyle = { ...sr.lineStyle, type: DASHES[(j - 1) % DASHES.length] };
        // One end label per line, on its first segment.
        delete sr.endLabel;
        delete sr.labelLayout;
      }
      return sr;
    }).filter((sr) => sr.data.some((/** @type {any} */ p) => p[1] != null));
  };

  /** `ymin`/`ymax` on a line: a band per pair, the first the outermost; grey
   * unless colour splits the lines. @param {any} k @param {any} s */
  const lineBands = (k, s) => {
    const { m, clrOf, ink, pairs, loEnd, hiEnd } = k;
    const clr = m.splitCol ? clrOf(s) : ink.muted;
    const out = [];
    for (const [loCol, hiCol] of pairs) {
      const b = {
        x: s.pts.map((/** @type {any} */ p) => p[0]),
        lo: s.rows.map((/** @type {any} */ r) => loEnd(r[loCol])),
        hi: s.rows.map((/** @type {any} */ r) => hiEnd(r[hiCol]))
      };
      const rib = parts.ribbon(s.level, b, 'lo', 'hi', clr, BAND_OPACITY);
      if (rib) { rib.z = 1; out.push(rib); }
    }
    return out;
  };

  /** R's smoother of one group, nested by facet level when faceted.
   * @param {any} k @param {any} groupName */
  const smootherLine = (k, groupName) => {
    const { smootherSeries, m, panel } = k;
    if (!smootherSeries) return null;
    const bucket = (m.facet && panel.facet !== '__all__')
      ? smootherSeries[panel.facet] : smootherSeries;
    const s = bucket && bucket[groupName != null ? String(groupName) : '__all__'];
    if (!s || !s.x || !s.y) return null;
    const out = [];
    for (let i = 0; i < s.x.length; i++) {
      if (Number.isFinite(s.x[i]) && Number.isFinite(s.y[i])) out.push([s.x[i], s.y[i]]);
    }
    return out.length >= 2 ? out : null;
  };

  /** Per level: its bands, line or points, fit and error bars. @param {any} k */
  const panelSeries = (k) => {
    const { panel, isLine, pairs, ltCol, clrOf, smoother, m, hiEnd, loEnd, floorX, encodeX } = k;
    const ltLevels = lineTypeLevels(k);
    /** @type {any[]} */ const series = [];
    panel.series.forEach((/** @type {any} */ s, /** @type {number} */ si) => {
      if (isLine && pairs.length && s.rows.length) series.push(...lineBands(k, s));
      if (ltCol && ltLevels.length > 1) {
        series.push(...lineSegments(k, ltLevels, s, si));
      } else {
        series.push(mkSeries(k, s.level, s.pts, clrOf(s), !!s.muted));
      }
      // With a highlight the level's smoother goes with its highlighted points.
      const fitHere = !s.muted || !panel.series.some((/** @type {any} */ o) =>
        !o.muted && o.level === s.level);
      if (smoother !== 'none' && !isLine && fitHere) {
        const ln = smootherLine(k, s.level);
        if (ln) {
          series.push({
            type: 'line', name: s.level || 'fit', legendHoverLink: false, data: ln,
            silent: true, showSymbol: false,
            lineStyle: { color: clrOf(s), width: 2, type: 'solid', opacity: 0.9 }, z: 2
          });
        }
      }
      // A forest plot is a scatter with ymin/ymax whiskers, one per pair.
      if (!isLine && pairs.length && s.src.length) {
        for (const [loCol, hiCol] of pairs) {
          const errPts = s.src
            .filter((/** @type {any} */ r) => r[m.x.col] != null && r[loCol] != null &&
              hiEnd(r[hiCol]) != null && (floorX == null || Number(r[m.x.col]) > 0))
            .map((/** @type {any} */ r) => [encodeX(r[m.x.col]), loEnd(r[loCol]), Number(r[hiCol])]);
          if (errPts.length) {
            series.push(option.aggParts.whiskerSeries(s.level || 'errbar', errPts, clrOf(s), false));
          }
        }
      }
    });
    return series;
  };

  /** `x_ranges`: named stretches of x, shaded behind the marks.
   * @param {any} k @param {any[]} series */
  const addRanges = (k, series) => {
    const { cfg, ink, m, floorX, encodeX } = k;
    const ranges = NS.rangesOf(cfg.x_ranges);
    const host = series.find((sr) => sr.type === 'line' || sr.type === 'scatter');
    if (!ranges.length || !host) return;
    host.markArea = {
      silent: true,
      itemStyle: { color: ink.muted, opacity: 0.08 },
      label: { position: 'insideTop', color: ink.muted, fontSize: ink.fontSize },
      // On a category axis an end names its category, as the x lines do.
      data: ranges.map((/** @type {any} */ r) => (m.x.type === 'category'
        ? [{ name: r.name || '', xAxis: NS.axes.catOf(r.from, m.x.cats || [], true) },
           { xAxis: NS.axes.catOf(r.to, m.x.cats || [], true) }]
        : [{ name: r.name || '', xAxis: floorX != null && !(Number(r.from) > 0) ? floorX : encodeX(r.from) },
           { xAxis: floorX != null && !(Number(r.to) > 0) ? null : encodeX(r.to) }]))
        .filter((/** @type {any} */ d) => d[0].xAxis != null && d[1].xAxis != null)
    };
  };

  /** `label`: the column's value beside each point that has one, as one label-only series per panel
   * (its id keeps the patch and the brush off it). A highlight without `label` names its points by
   * the highlighted value, where that value picks out one or two points of the panel.
   * @param {any} k @param {any[]} series @param {any} room */
  const pointLabels = (k, series, room) => {
    const { isLine, m, cfg, panel, lineMarkerPx, dm, ink } = k;
    const hlCol = !isLine && m.hl && !cfg.label ? Object.keys(m.hl)[0] : null;
    const labelCol = cfg.label || hlCol;
    if (!labelCol) return;
    /** @type {Map<string, number>} */
    const hlCount = new Map();
    if (hlCol) {
      for (const s of panel.series) {
        if (s.muted) continue;
        for (const r of s.rows) {
          const v = String(r[hlCol]);
          hlCount.set(v, (hlCount.get(v) || 0) + 1);
        }
      }
    }
    /** @type {any[]} */ const data = [];
    for (const s of panel.series) {
      if (hlCol && s.muted) continue;
      s.rows.forEach((/** @type {any} */ r, /** @type {number} */ i) => {
        const v = r[labelCol];
        const p = s.pts[i];
        if (!NS.hasVal(v) || v === 'NA' || !p || p[0] == null || p[1] == null) return;
        if (hlCol && (hlCount.get(String(v)) || 0) > 2) return;
        data.push([p[0], p[1], String(v)]);
      });
    }
    if (!data.length) return;
    const position = isLine ? 'top' : 'right';
    const distance = isLine ? lineMarkerPx / 2 + 2 : (BASE_SCATTER_SIZE * dm) / 2 + 3;
    series.push({
      ...NS.nameMark('__label__', /** @type {any} */ ((/** @type {any} */ prm) => prm.value[2]),
                     [], position, ink, {
        distance, color: hlCol ? ink.text : ink.secondary, fontWeight: hlCol ? 600 : undefined
      }),
      data,
      labelLayout: labelSpacer(position, distance, room)
    });
  };

  /** Each colour group's name at the centre of its points, one spacer shared.
   * @param {any} k @param {any[]} series @param {any} room */
  const cloudNames = (k, series, room) => {
    const { cloudLabels, panel, ink } = k;
    if (!cloudLabels) return;
    const spacer = labelSpacer('inside', 0, room);
    panel.series.forEach((/** @type {any} */ s, /** @type {number} */ si) => {
      const c = NS.cloudCentre(s.pts);
      if (!c || s.muted || s.level == null || s.level === '') return;
      const mark = NS.nameMark('__legend_' + si, String(s.level), c, 'inside', ink);
      mark.labelLayout = spacer;
      series.push(mark);
    });
  };

  /** The scatter or line series of one panel, with their overlays. @param {any} m the individual
   * model @param {any} panel @param {any} look @param {number} [plotW] the plot's width in px */
  const points = (m, panel, look, plotW = 0) => {
    const k = pointsContext(m, panel, look, plotW);
    const { directLabels, hlNames, stepMode, smoothOn, lineMarkerPx, lm } = k;
    const series = panelSeries(k);
    addRanges(k, series);
    const room = { placed: [], lastS: -1, lastD: -1 };
    pointLabels(k, series, room);
    cloudNames(k, series, room);
    const labelPad = (directLabels || hlNames)
      ? endLabelPad(panel.series.filter((/** @type {any} */ s) => !(m.hl && s.muted))
        .map((/** @type {any} */ s) => s.level))
      : 0;
    return { series, stepMode, smoothOn, markerPx: lineMarkerPx, focusW: 2.25 * lm, labelPad };
  };

  /** The hover overlay, on every line chart (empty until a line is hovered) so the series keep one
   * shape: a veil on its own hidden 0..1 axes, then the hovered line and its dots. `state.veil` is
   * the fill, set on promotion. @param {{ veil?: string }} state */
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

  /** The tooltip fields of a point's value (they follow x and y) as [title, text] pairs.
   * @param {any} m @param {any} look @param {any} val @returns {Array<[string, any]>} */
  const ttPairs = (m, look, val) => (Array.isArray(val) ? m.ttFields : [])
    .map((/** @type {string} */ c, /** @type {number} */ i) => /** @type {[string, any]} */ ([
      NS.axes.axisTitle(look.columns, c) || c,
      typeof val[2 + i] === 'number' ? V.util.num3(val[2 + i]) : val[2 + i]
    ])).filter((/** @type {[string, any]} */ [, v]) => v != null && v !== '');

  /** The tooltip fields as a "  (col: v, col2: v2)" suffix on a line's row.
   * @param {any} m @param {any} look */
  const ttSuffixOf = (m, look) => (/** @type {any} */ val) => {
    const pairs = ttPairs(m, look, val);
    return pairs.length
      ? '  (' + pairs.map(([t, v]) => V.util.esc(t) + ': ' + V.util.esc(v)).join(', ') + ')' : '';
  };

  /**
   * The line chart's tooltip on a hairline crosshair: each line's value at x,
   * highest first, capped. A crowd shows only the line nearest the cursor
   * (`hover.si`).
   * @param {any} m @param {any} look @param {{ si: number | null }} hover
   */
  const lineTooltip = (m, look, hover) => {
    const title = (/** @type {any} */ col) => NS.axes.axisTitle(look.columns, col);
    const ttSuffix = ttSuffixOf(m, look);
    const vf = NS.valueFormat(look.cfg);
    return {
      trigger: 'axis',
      axisPointer: { type: 'line', label: { show: false },
                     lineStyle: { color: look.ink.muted, width: 1, type: 'solid' } },
      confine: true,
      formatter: (/** @type {any} */ ps) => {
        if (!Array.isArray(ps)) ps = [ps];
        let rows = ps.filter((/** @type {any} */ p) =>
          p && p.seriesType === 'line' && Array.isArray(p.value) && p.seriesName !== '__focus__');
        if (m.crowd) {
          if (hover.si == null) return '';
          rows = rows.filter((/** @type {any} */ p) => p.seriesIndex === hover.si);
        }
        // One row per line, the first of replicates at one x; the segments
        // a `linetype` cuts a line into are one line.
        const seen = new Set();
        rows = rows.filter((/** @type {any} */ p) => {
          if (p.value[1] == null) return false;
          const seg = SEG_ID.exec(p.seriesId || '');
          const key = seg ? 'seg' + seg[1] : 'i' + p.seriesIndex;
          if (seen.has(key)) return false;
          seen.add(key);
          return true;
        });
        if (!rows.length) return '';
        const head = m.x.cats
          ? (rows[0].axisValueLabel ?? String(rows[0].value[0]))
          : (m.x.type === 'time'
            ? NS.axes.fmtXVal(rows[0].value[0], 'time', null, m.x.period)
            : title(m.x.col) + ': ' + V.util.num3(rows[0].value[0]));
        // Highest first, the order the lines stack in at the crosshair.
        rows.sort((/** @type {any} */ a, /** @type {any} */ b) =>
          (Number(b.value[1]) || 0) - (Number(a.value[1]) || 0));
        const lines = rows.slice(0, TT_ROW_CAP).map((/** @type {any} */ p) => {
          const nm = m.splitCol ? V.util.esc(p.seriesName) : V.util.esc(title(m.y.col));
          return V.tooltip.row(nm, (vf ? vf(p.value[1]) : V.util.num3(p.value[1])) + ttSuffix(p.value),
                           m.splitCol ? p.color : null, null);
        });
        if (rows.length > TT_ROW_CAP) lines.push(V.tooltip.note('+' + (rows.length - TT_ROW_CAP) + ' more'));
        return V.tooltip.head(V.util.esc(head)) + lines.join('');
      }
    };
  };

  /** The point tooltip (scatter and band): the series level or the y title as
   * headline, then x, y, the split column and the tooltip fields.
   * @param {any} m @param {any} look */
  const pointTooltip = (m, look) => {
    const title = (/** @type {any} */ col) => NS.axes.axisTitle(look.columns, col);
    const vf = NS.valueFormat(look.cfg);
    return {
      trigger: 'item', confine: true,
      formatter: (/** @type {any} */ p) => {
        const lvl = (m.splitCol && p.seriesName) ? p.seriesName : null;
        const headline = lvl || title(m.y.col) || 'Point';
        /** @type {Array<[string, any]>} */
        const pairs = [
          [title(m.x.col) || 'X', NS.axes.fmtXVal(p.value[0], m.x.type, null, m.x.period)],
          [title(m.y.col) || 'Y', vf ? vf(p.value[1]) : V.util.num3(p.value[1])]
        ];
        if (lvl) pairs.push([title(m.splitCol) || 'Series', lvl]);
        pairs.push(...ttPairs(m, look, p.value));
        return NS.rowTooltip(headline, pairs, lvl ? p.color : null);
      }
    };
  };

  Object.assign(parts, { points, labelSpacer, lineOverlay, lineTooltip, pointTooltip, endLabelOf, endLabelPad });
})();
