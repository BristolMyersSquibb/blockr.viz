// @ts-check
/**
 * Chart block: the band series of the individual option. A band draws R's
 * windowed statistics (derived.band.series: per facet, per level, x with a
 * centre line and inner and outer intervals), not rows. Per level: a ribbon,
 * a halo and the centre line. One level in a panel also gets the outer ribbon
 * and the outliers; with several, the ribbons stay hidden until the hover
 * picker shows one, since overlapping fills invent colours.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});
  const option = /** @type {any} */ (NS.option = NS.option || {});
  const parts = /** @type {any} */ (option.indParts = option.indParts || {});

  const BASE_LINE_WIDTH = 1.4;

  /**
   * A ribbon between two edges: one polygon per run where both exist, so a
   * band cut for thin n breaks. Null when no run has two points.
   * @param {any} name @param {any} b @param {string} loKey @param {string} hiKey
   * @param {string} clr @param {number} op
   */
  const ribbon = (name, b, loKey, hiKey, clr, op) => {
    const lo = b[loKey], hi = b[hiKey];
    if (!Array.isArray(lo) || !Array.isArray(hi)) return null;
    const runs = [];
    /** @type {number[] | null} */
    let cur = null;
    for (let i = 0; i < b.x.length; i++) {
      if (lo[i] != null && hi[i] != null) {
        if (!cur) { cur = []; runs.push(cur); }
        cur.push(i);
      } else { cur = null; }
    }
    const solid = runs.filter((r) => r.length > 1);
    if (!solid.length) return null;
    const item = [b.x[solid[0][0]], hi[solid[0][0]]];
    return {
      type: 'custom', name, legendHoverLink: false, silent: true,
      itemStyle: { color: clr }, __bandItem: item,
      z: 2, data: [item],
      encode: { x: 0, y: 1 },
      renderItem: (/** @type {any} */ params, /** @type {any} */ api) => {
        const kids = [];
        for (const r of solid) {
          const pts = [];
          for (const i of r) pts.push(api.coord([b.x[i], hi[i]]));
          for (let k = r.length - 1; k >= 0; k--) pts.push(api.coord([b.x[r[k]], lo[r[k]]]));
          kids.push({ type: 'polygon', shape: { points: pts }, style: { fill: clr, opacity: op } });
        }
        return { type: 'group', children: kids };
      }
    };
  };

  /** Outlier points: the observations outside the local Tukey fence, as R
   * classified them. @param {any} name @param {any} b @param {string} clr @param {number} dm
   */
  const outlierPoints = (name, b, clr, dm) => {
    if (!Array.isArray(b.out_x) || !b.out_x.length) return null;
    const pts = b.out_x.map((/** @type {any} */ x, /** @type {number} */ i) =>
      (Array.isArray(b.out_id) ? [x, b.out_y[i], b.out_id[i]] : [x, b.out_y[i]]));
    return {
      type: 'scatter', name, data: pts, z: 5, legendHoverLink: false,
      symbolSize: Math.max(4, 6 * dm),
      itemStyle: { color: 'transparent', borderColor: clr, borderWidth: 1.3, cursor: 'pointer' },
      emphasis: { disabled: true }
    };
  };

  /**
   * The centre line of one level, with a halo under it so lines crossing a
   * ribbon read. Returns the line's index in `series`.
   * @param {any[]} series @param {any} b @param {string} clr
   * @param {string | undefined} nm @param {any} ink @param {number} lm
   * @param {boolean} named whether the line carries its name at its end
   */
  const bandCenterLines = (series, b, clr, nm, ink, lm, named) => {
    const cpts = b.x.map((/** @type {any} */ x, /** @type {number} */ i) =>
      [x, b.center[i] == null ? null : b.center[i]]);
    series.push({
      type: 'line', name: nm, data: cpts, symbol: 'none', showSymbol: false,
      z: 6, silent: true, legendHoverLink: false,
      itemStyle: { color: clr },
      lineStyle: { color: ink.surface, width: (BASE_LINE_WIDTH + 2) * lm },
      emphasis: { disabled: true }
    });
    const at = series.length;
    series.push({
      type: 'line', name: nm, data: cpts, symbol: 'none', showSymbol: false,
      z: 7, triggerLineEvent: true,
      itemStyle: { color: clr, cursor: 'pointer' },
      lineStyle: { color: clr, width: (BASE_LINE_WIDTH + 0.8) * lm },
      emphasis: { disabled: true },
      ...(named && nm ? parts.endLabelOf(ink) : {})
    });
    return at;
  };

  /**
   * The band series of one panel.
   * @param {any} m the individual model @param {any} panel @param {any} look
   * @returns {{ series: any[], band: any, markLineIdx: number, many: boolean, labelPad: number }}
   */
  const band = (m, panel, look) => {
    const { cfg, ink } = look;
    const dm = cfg.point_size;
    const lm = cfg.line_width;
    const series = [];
    const panelBand = parts.log.bandBucket(cfg, panel);
    if (!panelBand) return { series, band: null, markLineIdx: -1, many: false, labelPad: 0 };
    // `legend: "marks"`: each centre line carries its name at its end.
    const named = NS.legendPlan(m, cfg).marks === 'end';
    const allLevels = m.levels.length ? m.levels : ['__all__'];
    const levelPos = NS.firstIndex(allLevels);
    const levels = allLevels.filter((/** @type {string} */ cl) =>
      panelBand[cl] || (allLevels.length === 1 && panelBand['__all__']));
    const many = levels.length > 1;
    /** @type {Record<string, number>} */
    const centerIdx = {};
    /** @type {Record<string, {idx: number, item: any}>} */
    const ribbonIdx = {};
    let firstCenter = -1;
    for (const cl of levels) {
      const b = panelBand[cl] || (levels.length === 1 ? panelBand['__all__'] : null);
      if (!b || !Array.isArray(b.x)) continue;
      // The colour of the level's data-wide position, not the panel's first.
      const clr = m.levels.length ? m.levelColors[/** @type {number} */ (levelPos.get(cl))]
        : m.palette[0];
      const nm = cl === '__all__' ? undefined : cl;
      const inner = ribbon(nm, b, 'lo', 'hi', clr, 0.28);
      if (!many) {
        const outer = ribbon(nm, b, 'olo', 'ohi', clr, 0.13);
        if (outer) series.push(outer);
        if (inner) series.push(inner);
        if (cfg.box_points === 'outliers') {
          const op = outlierPoints(nm, b, clr, dm);
          if (op) series.push(op);
        }
      } else if (inner) {
        // The inner interval only, empty until the hover picker shows it.
        ribbonIdx[cl] = { idx: series.length, item: inner.__bandItem };
        inner.data = [];
        series.push(inner);
      }
      const at = bandCenterLines(series, b, clr, nm, ink, lm, named);
      centerIdx[cl] = at;
      if (firstCenter < 0) firstCenter = at;
    }
    return {
      series,
      // What the hover picker patches: ribbon data only.
      band: many ? { centerIdx, ribbonIdx, total: series.length } : null,
      // Helper lines hang off a centre line, which the hover never empties.
      markLineIdx: firstCenter,
      many,
      labelPad: named ? parts.endLabelPad(levels) : 0
    };
  };

  /** The reference limits R reduced from their columns (hi, then lo) that hold a number.
   * @param {any} cfg @returns {any[]} */
  const refLimits = (cfg) => {
    const refs = (cfg.derived.band && cfg.derived.band.refs) || {};
    return [refs.hi, refs.lo].filter((r) => r && Number.isFinite(Number(r.value)));
  };

  /**
   * The value axis of a band panel, fitted to what it draws (a custom series
   * puts one datum into the extent) and opened to a reference limit up to
   * 60 % more span, rounded outward. Null when nothing numeric is drawn.
   * @param {any} cfg @param {any} panel @param {boolean} many
   * @returns {{ lo: number, hi: number } | null}
   */
  const bandExtent = (cfg, panel, many) => {
    const drawn = ['center', 'lo', 'hi'];
    if (!many) {
      drawn.push('olo', 'ohi');
      if (cfg.box_points === 'outliers') drawn.push('out_y');
    }
    const vals = parts.log.bandValues(cfg, panel, drawn)
      .filter((v) => v != null).map(Number).filter(Number.isFinite);
    let lo = NS.minOf(vals), hi = NS.maxOf(vals);
    if (!(Number.isFinite(lo) && Number.isFinite(hi) && hi > lo)) return null;
    const span = hi - lo;
    for (const r of refLimits(cfg)) {
      const v = Number(r.value);
      const nlo = Math.min(lo, v), nhi = Math.max(hi, v);
      if (nhi - nlo <= span * 1.6) { lo = nlo; hi = nhi; }
    }
    const pad = (hi - lo) * 0.06;
    const [aLo, aHi] = parts.log.niceDomain(lo - pad, hi + pad);
    return { lo: aLo, hi: aHi };
  };

  /**
   * The reference limits R reduced from their columns, as markLine data,
   * named with the column's title and, when it varies, its range. A limit
   * outside the frame is pinned to the edge and says which way.
   * @param {any} cfg @param {{ lo: number, hi: number } | null} ext @param {any} ink
   * @param {VizColumn[]} columns
   */
  const bandRefLines = (cfg, ext, ink, columns) => {
    const out = [];
    for (const r of refLimits(cfg)) {
      const v = Number(r.value);
      const base = NS.axes.axisTitle(columns, r.col) + ' ' + NS.ddNum3(v) +
        (Number(r.n_distinct) > 1
          ? ' (' + NS.ddNum3(Number(r.lo)) + '–' + NS.ddNum3(Number(r.hi)) + ')' : '');
      let at = v, off = '';
      if (ext && v > ext.hi) { at = ext.hi; off = ' ↑ off scale'; }
      else if (ext && v < ext.lo) { at = ext.lo; off = ' ↓ off scale'; }
      out.push({
        yAxis: at,
        lineStyle: { color: ink.danger, width: 1 * cfg.line_width, type: off ? 'dotted' : 'dashed',
                     opacity: off ? 0.6 : 0.45 },
        label: { show: true, position: 'insideEndTop', color: ink.muted,
                 fontSize: ink.fontSize, fontWeight: 400, formatter: base + off }
      });
    }
    return out;
  };

  Object.assign(parts, { band, bandExtent, bandRefLines, ribbon });
})();
