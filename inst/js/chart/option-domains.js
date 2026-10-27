// @ts-check
/**
 * Chart block: the value domains of the individual option. A nice domain
 * around a range, the identity line's domain, the positive extent of the
 * values a log axis can show, the log ticks shared across a facet grid, and
 * a band held above a log axis's floor. Pure.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});
  const option = /** @type {any} */ (NS.option = NS.option || {});
  const parts = /** @type {any} */ (option.indParts = option.indParts || {});

  /**
   * A domain rounded outward to a nice step: an explicit min/max turns
   * off ECharts' own rounding.
   * @param {number} lo @param {number} hi
   */
  const niceDomain = (lo, hi) => {
    const span = hi - lo;
    const mag = Math.pow(10, Math.floor(Math.log10(span)));
    const rel = span / mag;
    const step = rel >= 5 ? mag : (rel >= 2 ? mag / 2 : mag / 5);
    return [Math.floor(lo / step) * step, Math.ceil(hi / step) * step];
  };

  /**
   * The identity line (y = x) over one domain shared by both axes, so the
   * diagonal runs corner to corner. Null without two numeric ranges.
   * @param {any[]} series
   */
  const identityDomain = (series) => {
    let xLo = Infinity, xHi = -Infinity, yLo = Infinity, yHi = -Infinity;
    for (const s of series) {
      if (s.type !== 'scatter' || !Array.isArray(s.data)) continue;
      for (const p of s.data) {
        const px = Number(p[0]), py = Number(p[1]);
        if (Number.isFinite(px)) { if (px < xLo) xLo = px; if (px > xHi) xHi = px; }
        if (Number.isFinite(py)) { if (py < yLo) yLo = py; if (py > yHi) yHi = py; }
      }
    }
    const rLo = Math.min(xLo, yLo), rHi = Math.max(xHi, yHi);
    if (!(Number.isFinite(rLo) && Number.isFinite(rHi) && rLo < rHi)) return null;
    return niceDomain(rLo, rHi);
  };

  // -- Log axes ----------------------------------------------------------------

  const pos = (/** @type {any} */ v) => v != null && v !== '' && Number(v) > 0;

  /** The interval column pairs, lo and hi. @param {Record<string, any>} cfg */
  const intervalPairs = (cfg) => {
    const lo = [].concat(cfg.lo || []).filter(Boolean);
    const hi = [].concat(cfg.hi || []).filter(Boolean);
    return lo.map((l, i) => [l, hi[i]]).filter((p) => p[1]);
  };

  /** A panel's band levels, keyed by level. @param {any} cfg @param {any} panel */
  const bandBucket = (cfg, panel) => {
    const bs = cfg.band_series;
    return bs ? (bs[panel.facet] || bs['__all__'] || null) : null;
  };

  /**
   * The positive values one panel draws along an axis: the points, and on y
   * the interval ends, the band and the smoother. [lo, hi] or null.
   * @param {any} m @param {any} panel @param {any} cfg @param {'x' | 'y'} axis
   */
  const positiveExtent = (m, panel, cfg, axis) => {
    let lo = Infinity, hi = -Infinity;
    const scan = (/** @type {any} */ v) => {
      if (!pos(v)) return;
      const n = Number(v);
      if (n < lo) lo = n;
      if (n > hi) hi = n;
    };
    const k = axis === 'x' ? 0 : 1;
    for (const s of panel.series) for (const p of s.pts) scan(p[k]);
    if (axis === 'y') {
      const pairs = intervalPairs(cfg);
      if (pairs.length) {
        for (const s of panel.series) {
          for (const r of s.rows) for (const [a, b] of pairs) { scan(r[a]); scan(r[b]); }
        }
      }
      const pb = m.isBand ? bandBucket(cfg, panel) : null;
      if (pb) {
        for (const b of Object.values(pb)) {
          if (!b) continue;
          for (const key of ['center', 'lo', 'hi', 'olo', 'ohi', 'out_y']) {
            if (Array.isArray(b[key])) for (const v of b[key]) scan(v);
          }
        }
      }
    }
    return Number.isFinite(lo) ? [lo, hi] : null;
  };

  /**
   * Each panel's log domain per axis (null on a linear axis), shared across
   * the panels where the facet scales are, and what the log axes leave out:
   * the guide lines and band outliers at or below zero. The model counted
   * the rows.
   * @param {any} m @param {Record<string, any>} cfg
   */
  const logDomains = (m, cfg) => {
    const mode = NS.facetScales(cfg);
    const n = m.panels.length;
    /** @param {'x' | 'y'} axis @param {boolean} on @param {boolean} shared */
    const per = (axis, on, shared) => {
      if (!on) return new Array(n).fill(null);
      const exts = m.panels.map((/** @type {any} */ p) => positiveExtent(m, p, cfg, axis));
      if (shared) {
        const all = exts.filter(Boolean);
        const dom = all.length
          ? NS.axes.logTicks(Math.min(...all.map((e) => e[0])), Math.max(...all.map((e) => e[1])))
          : null;
        return new Array(n).fill(dom);
      }
      return exts.map((e) => (e ? NS.axes.logTicks(e[0], e[1]) : null));
    };
    let dropped = 0;
    if (m.logY) {
      dropped += [].concat(cfg.value_lines || []).filter((v) => !pos(v)).length;
      if (m.isBand && cfg.box_points === 'outliers') {
        for (const p of m.panels) {
          for (const b of Object.values(bandBucket(cfg, p) || {})) {
            if (b && Array.isArray(b.out_y)) dropped += b.out_y.filter((v) => !pos(v)).length;
          }
        }
      }
    }
    if (m.logX) dropped += [].concat(cfg.x_lines || []).filter((v) => !pos(v)).length;
    return {
      x: per('x', m.logX, mode !== 'free'),
      y: per('y', m.logY, mode === 'fixed'),
      dropped
    };
  };

  /**
   * A band on a log axis: a low edge at or below zero is cut at the floor,
   * a high edge or centre there is a gap, and outliers there are not drawn.
   * @param {any} bs band_series @param {number} floor
   */
  const clampBand = (bs, floor) => {
    if (!bs) return bs;
  /** @type {Record<string, any>} */
    const out = {};
    for (const [f, levels] of Object.entries(bs)) {
      if (!levels || typeof levels !== 'object') { out[f] = levels; continue; }
      /** @type {Record<string, any>} */
      const lv = {};
      for (const [k, b] of Object.entries(levels)) {
        if (!b || typeof b !== 'object') { lv[k] = b; continue; }
        const c = { ...b };
        for (const key of ['lo', 'olo']) {
          if (Array.isArray(b[key])) c[key] = b[key].map((/** @type {any} */ v) => (v == null ? null : (pos(v) ? v : floor)));
        }
        for (const key of ['center', 'hi', 'ohi']) {
          if (Array.isArray(b[key])) c[key] = b[key].map((/** @type {any} */ v) => (pos(v) ? v : null));
        }
        if (Array.isArray(b.out_y)) {
          const keep = b.out_y.map(pos);
          for (const key of ['out_x', 'out_y', 'out_id']) {
            if (Array.isArray(b[key])) c[key] = b[key].filter((/** @type {any} */ _, /** @type {number} */ i) => keep[i]);
          }
        }
        lv[k] = c;
      }
      out[f] = lv;
    }
    return out;
  };

  parts.log = { niceDomain, identityDomain, pos, intervalPairs, bandBucket, positiveExtent,
                logDomains, clampBand };
})();
