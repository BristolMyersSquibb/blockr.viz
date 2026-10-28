// @ts-check
/**
 * Chart block: the value domains of the individual option. A nice domain, the
 * identity line's domain, the positive extent a log axis can show, the log
 * ticks shared across a facet grid, and a band held above a log axis's floor.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const option = /** @type {any} */ (NS.option = NS.option || {});

  /** A domain rounded outward to a nice step (an explicit min/max turns off
   * ECharts' own rounding). @param {number} lo @param {number} hi */
  const niceDomain = (lo, hi) => {
    const span = hi - lo;
    const mag = Math.pow(10, Math.floor(Math.log10(span)));
    const rel = span / mag;
    const step = rel >= 5 ? mag : (rel >= 2 ? mag / 2 : mag / 5);
    return [Math.floor(lo / step) * step, Math.ceil(hi / step) * step];
  };

  /** One domain for both axes, so the identity line runs corner to corner.
   * Null without two numeric ranges. @param {any[]} series */
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

  const pos = (/** @type {any} */ v) => v != null && v !== '' && Number(v) > 0;

  /** The interval column pairs, lo and hi. @param {Record<string, any>} cfg */
  const intervalPairs = (cfg) => {
    const lo = /** @type {any[]} */ ([]).concat(cfg.ymin || []).filter(Boolean);
    const hi = /** @type {any[]} */ ([]).concat(cfg.ymax || []).filter(Boolean);
    return lo.map((l, i) => [l, hi[i]]).filter((p) => p[1]);
  };

  /** A panel's band levels, keyed by level. @param {ChartDerived} derived @param {any} panel */
  const bandBucket = (derived, panel) => {
    const bs = derived.band && derived.band.series;
    return bs ? (bs[panel.facet] || bs['__all__'] || null) : null;
  };

  /** The values of a band panel's series under `keys`, across levels.
   * @param {ChartDerived} derived @param {any} panel @param {string[]} keys @returns {any[]} */
  const bandValues = (derived, panel, keys) => Object.values(bandBucket(derived, panel) || {})
    .flatMap((/** @type {Record<string, any>} */ b) =>
      (b ? keys.flatMap((key) => (Array.isArray(b[key]) ? b[key] : [])) : []));

  /**
   * [lo, hi] of the positive values one panel draws along an axis (on y also
   * the interval ends and the band), or null.
   * @param {any} m @param {any} panel @param {any} cfg @param {ChartDerived} derived
   * @param {'x' | 'y'} axis
   */
  const positiveExtent = (m, panel, cfg, derived, axis) => {
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
      for (const s of panel.series) {
        for (const r of s.rows) for (const [a, b] of pairs) { scan(r[a]); scan(r[b]); }
      }
      if (m.isBand) {
        for (const v of bandValues(derived, panel, ['center', 'lo', 'hi', 'olo', 'ohi', 'out_y'])) scan(v);
      }
    }
    return Number.isFinite(lo) ? [lo, hi] : null;
  };

  /** Each panel's log domain per axis (null on a linear axis), shared across the panels where the
   * facet scales are, and the count of guide lines and band outliers at or below zero a log axis
   * leaves out. @param {any} m @param {ChartSettings} cfg @param {ChartDerived} derived */
  const logDomains = (m, cfg, derived) => {
    const mode = NS.facetScales(cfg);
    const n = m.panels.length;
    /** @param {'x' | 'y'} axis @param {boolean} on @param {boolean} shared */
    const per = (axis, on, shared) => {
      if (!on) return new Array(n).fill(null);
      const exts = m.panels.map((/** @type {any} */ p) => positiveExtent(m, p, cfg, derived, axis));
      if (shared) {
        const all = exts.filter(Boolean);
        const dom = all.length
          ? NS.axes.logTicks(Math.min(...all.map((/** @type {number[]} */ e) => e[0])), Math.max(...all.map((/** @type {number[]} */ e) => e[1])))
          : null;
        return new Array(n).fill(dom);
      }
      return exts.map((/** @type {number[] | null} */ e) => (e ? NS.axes.logTicks(e[0], e[1]) : null));
    };
    let dropped = 0;
    if (m.logY) {
      dropped += NS.labelled(cfg.y_lines).filter((/** @type {{ value: any }} */ x) => !pos(x.value)).length;
      if (m.isBand && cfg.box_points === 'outliers') {
        for (const p of m.panels) dropped += bandValues(derived, p, ['out_y']).filter((v) => !pos(v)).length;
      }
    }
    if (m.logX) dropped += NS.labelled(cfg.x_lines).filter((/** @type {{ value: any }} */ x) => !pos(x.value)).length;
    return {
      x: per('x', m.logX, mode !== 'free'),
      y: per('y', m.logY, mode === 'fixed'),
      dropped
    };
  };

  /** A band on a log axis: a low edge at or below zero is cut at the floor, a high edge or centre
   * there is a gap, outliers there are dropped. @param {any} bs the band's series
   * @param {number} floor */
  const clampBand = (bs, floor) => {
    if (!bs) return bs;
    /** @param {any} b */
    const clampLevel = (b) => {
      const c = { ...b };
      /** @param {string[]} keys @param {(v: any) => any} fn */
      const map = (keys, fn) => {
        for (const key of keys) if (Array.isArray(b[key])) c[key] = b[key].map(fn);
      };
      map(['lo', 'olo'], (v) => (v == null ? null : (pos(v) ? v : floor)));
      map(['center', 'hi', 'ohi'], (v) => (pos(v) ? v : null));
      if (Array.isArray(b.out_y)) {
        const keep = b.out_y.map(pos);
        for (const key of ['out_x', 'out_y', 'out_id']) {
          if (Array.isArray(b[key])) {
            c[key] = b[key].filter((/** @type {any} */ _, /** @type {number} */ i) => keep[i]);
          }
        }
      }
      return c;
    };
    /** @type {Record<string, any>} */
    const out = {};
    for (const [f, levels] of Object.entries(bs)) {
      out[f] = levels && Object.fromEntries(
        Object.entries(levels).map(([k, b]) => [k, b && clampLevel(b)]));
    }
    return out;
  };

  option.domainParts = { niceDomain, identityDomain, pos, intervalPairs, bandBucket, bandValues, logDomains, clampBand };
})();
