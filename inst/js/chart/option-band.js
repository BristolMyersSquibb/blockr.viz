// @ts-check
/**
 * Chart block: the band series of the individual option.
 *
 * A band draws R's windowed statistics (config.band_series: per facet, per
 * level, x with a centre line and inner and outer intervals), not rows, so
 * the browser and R cannot disagree. Per level: a ribbon (two edges as one
 * polygon per run where both exist), a halo under the centre line and the
 * centre line. With one level in a panel it also gets the outer ribbon and
 * the outlier points; with several, the ribbons are hidden until the hover
 * picker shows one, since overlapping fills invent colours.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});
  const option = /** @type {any} */ (NS.option = NS.option || {});
  const parts = /** @type {any} */ (option.indParts = option.indParts || {});

  const BASE_LINE_WIDTH = 1.4;

  /**
   * A ribbon between two edges: one polygon per run where both edges
   * exist, so a band cut for thin n breaks rather than bridging the gap.
   * Null when no run has two points.
   * @param {any} name @param {any} b @param {string} loKey @param {string} hiKey
   * @param {string} clr @param {number} op
   */
  const ribbon = (name, b, loKey, hiKey, clr, op) => {
    const lo = b[loKey], hi = b[hiKey];
    if (!Array.isArray(lo) || !Array.isArray(hi)) return null;
    /** @type {number[][]} */
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
        /** @type {any[]} */
        const kids = [];
        for (const r of solid) {
          /** @type {any[]} */
          const pts = [];
          for (const i of r) pts.push(api.coord([b.x[i], hi[i]]));
          for (let k = r.length - 1; k >= 0; k--) pts.push(api.coord([b.x[r[k]], lo[r[k]]]));
          kids.push({ type: 'polygon', shape: { points: pts }, style: { fill: clr, opacity: op } });
        }
        return { type: 'group', children: kids };
      }
    };
  };

  /**
   * Outlier points: the observations outside the local Tukey fence, as R
   * classified them.
   * @param {any} name @param {any} b @param {string} clr @param {number} dm
   */
  const outlierPoints = (name, b, clr, dm) => {
    if (!Array.isArray(b.out_x) || !b.out_x.length) return null;
    /** @type {any[]} */
    const pts = [];
    for (let i = 0; i < b.out_x.length; i++) {
      const p = [b.out_x[i], b.out_y[i]];
      if (Array.isArray(b.out_id)) p.push(b.out_id[i]);
      pts.push(p);
    }
    return {
      type: 'scatter', name, data: pts, z: 5, legendHoverLink: false,
      symbolSize: Math.max(4, 6 * dm),
      itemStyle: { color: 'transparent', borderColor: clr, borderWidth: 1.3, cursor: 'pointer' },
      emphasis: { disabled: true }
    };
  };

  /**
   * The band series of one panel.
   * @param {any} m the individual model @param {any} panel @param {any} look
   * @returns {{ series: any[], band: any, markLineIdx: number, many: boolean, levelsDrawn: string[] }}
   */
  const band = (m, panel, look) => {
    const { cfg, ink } = look;
    const dm = cfg.dot_size_mult ?? 1.0;
    const lm = cfg.line_width_mult ?? 1.0;
    const bandData = cfg.band_series || null;
    /** @type {any[]} */
    const series = [];
    if (!bandData) return { series, band: null, markLineIdx: -1, many: false, levelsDrawn: [] };
    // A facet panel holds its own cohort, so its own band; an unfaceted
    // chart reads __all__.
    const panelBand = bandData[panel.facet] || bandData['__all__'] || null;
    const allLevels = m.levels.length ? m.levels : ['__all__'];
    const levelPos = NS.firstIndex(allLevels);
    const levels = allLevels.filter((/** @type {string} */ cl) => panelBand && (
      panelBand[cl] || (allLevels.length === 1 && panelBand['__all__'])));
    const many = levels.length > 1;
    /** @type {Record<string, number>} */
    const centerIdx = {};
    /** @type {Record<string, {idx: number, item: any}>} */
    const ribbonIdx = {};
    let firstCenter = -1;
    for (const cl of levels) {
      const b = panelBand
        ? (panelBand[cl] || (levels.length === 1 ? panelBand['__all__'] : null)) : null;
      if (!b || !Array.isArray(b.x)) continue;
      // The palette index of the level in the data-wide order, so a panel
      // holding one level does not take the first colour.
      const clr = m.levels.length ? m.levelColors[/** @type {number} */ (levelPos.get(cl))]
        : m.palette[0];
      const nm = cl === '__all__' ? undefined : cl;
      if (!many) {
        if (Array.isArray(b.olo)) {
          const rv = ribbon(nm, b, 'olo', 'ohi', clr, 0.13);
          if (rv) series.push(rv);
        }
        const ri = ribbon(nm, b, 'lo', 'hi', clr, 0.28);
        if (ri) series.push(ri);
        if (cfg.box_points === 'outliers') {
          const op = outlierPoints(nm, b, clr, dm);
          if (op) series.push(op);
        }
      } else {
        // The inner interval only, empty until the hover picker shows it.
        const ri = ribbon(nm, b, 'lo', 'hi', clr, 0.28);
        if (ri) {
          ribbonIdx[cl] = { idx: series.length, item: ri.__bandItem };
          ri.data = [];
          series.push(ri);
        }
      }
      /** @type {any[]} */
      const cpts = [];
      for (let i = 0; i < b.x.length; i++) cpts.push([b.x[i], b.center[i] == null ? null : b.center[i]]);
      // A halo under every centre line, so lines crossing a ribbon read.
      series.push({
        type: 'line', name: nm, data: cpts, symbol: 'none', showSymbol: false,
        z: 6, silent: true, legendHoverLink: false,
        itemStyle: { color: clr },
        lineStyle: { color: ink.surface, width: (BASE_LINE_WIDTH + 2) * lm },
        emphasis: { disabled: true }
      });
      centerIdx[cl] = series.length;
      if (firstCenter < 0) firstCenter = series.length;
      series.push({
        type: 'line', name: nm, data: cpts, symbol: 'none', showSymbol: false,
        z: 7, triggerLineEvent: true,
        itemStyle: { color: clr, cursor: 'pointer' },
        lineStyle: { color: clr, width: (BASE_LINE_WIDTH + 0.8) * lm },
        emphasis: { disabled: true }
      });
    }
    return {
      series,
      // What the hover picker patches: ribbon data only.
      band: many ? { centerIdx, ribbonIdx, total: series.length } : null,
      // Helper lines hang off a centre line, which the hover never empties.
      markLineIdx: firstCenter,
      many,
      levelsDrawn: levels
    };
  };

  /**
   * The value axis of a band panel, fitted to what it draws (a custom
   * series puts one datum into the extent) and opened to a reference
   * limit up to 60 % more span, rounded outward to a nice step. Null when
   * nothing numeric is drawn.
   * @param {any} cfg @param {any} panel @param {boolean} many
   * @returns {{ lo: number, hi: number } | null}
   */
  const bandExtent = (cfg, panel, many) => {
    const bandData = cfg.band_series;
    const bandRefs = cfg.band_refs || null;
    let lo = Infinity, hi = -Infinity;
    const scan = (/** @type {any} */ v) => {
      if (v == null) return;
      const n = Number(v);
      if (!Number.isFinite(n)) return;
      if (n < lo) lo = n;
      if (n > hi) hi = n;
    };
    const pb = bandData ? (bandData[panel.facet] || bandData['__all__']) : null;
    if (pb) {
      const drawn = ['center', 'lo', 'hi'];
      if (!many) {
        drawn.push('olo', 'ohi');
        if (cfg.box_points === 'outliers') drawn.push('out_y');
      }
      for (const k of Object.keys(pb)) {
        const b2 = pb[k];
        if (!b2) continue;
        for (const key of drawn) if (Array.isArray(b2[key])) for (const v of b2[key]) scan(v);
      }
    }
    if (!(Number.isFinite(lo) && Number.isFinite(hi) && hi > lo)) return null;
    const span = hi - lo;
    if (bandRefs) {
      for (const k of ['hi', 'lo']) {
        const r = bandRefs[k];
        if (!r) continue;
        const v = Number(r.value);
        if (!Number.isFinite(v)) continue;
        const nlo = Math.min(lo, v), nhi = Math.max(hi, v);
        if (nhi - nlo <= span * 1.6) { lo = nlo; hi = nhi; }
      }
    }
    const pad = (hi - lo) * 0.06;
    let aLo = lo - pad, aHi = hi + pad;
    const aSpan = aHi - aLo;
    if (aSpan > 0) {
      const mag = Math.pow(10, Math.floor(Math.log10(aSpan)));
      const rel = aSpan / mag;
      const step = rel >= 5 ? mag : (rel >= 2 ? mag / 2 : mag / 5);
      aLo = Math.floor(aLo / step) * step;
      aHi = Math.ceil(aHi / step) * step;
    }
    return { lo: aLo, hi: aHi };
  };

  /**
   * The reference limits R reduced from their columns, as markLine data:
   * named with the column and, when it varies, the range it came from. A
   * limit outside the frame is pinned to the edge and says which way.
   * @param {any} cfg @param {{ lo: number, hi: number } | null} ext @param {any} ink
   */
  const bandRefLines = (cfg, ext, ink) => {
    const bandRefs = cfg.band_refs || null;
    const lm = cfg.line_width_mult ?? 1.0;
    /** @type {any[]} */
    const out = [];
    if (!bandRefs) return out;
    for (const k of ['hi', 'lo']) {
      const r = bandRefs[k];
      if (!r || !Number.isFinite(Number(r.value))) continue;
      const v = Number(r.value);
      const base = r.col + ' ' + NS.ddNum3(v) + (Number(r.n_distinct) > 1
        ? ' (' + NS.ddNum3(Number(r.lo)) + '–' + NS.ddNum3(Number(r.hi)) + ')' : '');
      let at = v, off = '';
      if (ext && v > ext.hi) { at = ext.hi; off = ' ↑ off scale'; }
      else if (ext && v < ext.lo) { at = ext.lo; off = ' ↓ off scale'; }
      out.push({
        yAxis: at,
        lineStyle: { color: ink.danger, width: 1 * lm, type: off ? 'dotted' : 'dashed',
                     opacity: off ? 0.6 : 0.45 },
        label: { show: true, position: 'insideEndTop', color: ink.muted,
                 fontSize: ink.fontSize, fontWeight: 400, formatter: base + off }
      });
    }
    return out;
  };

  Object.assign(parts, { band, bandExtent, bandRefLines });
})();
