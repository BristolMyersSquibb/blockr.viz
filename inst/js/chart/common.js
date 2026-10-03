// @ts-check
/**
 * Chart block: the namespace and what every other file shares.
 *
 * The chart is a set of plain scripts, loaded in the order listed in
 * scripts.txt, that share one namespace: window.Blockr.chart. In node the
 * pure files run in a vm context without a window, so the namespace hangs
 * off whatever global object the script runs in.
 *
 * Comments cite the click rules (D1-D11) and fixes (B1-B11) by their IDs
 * in blockr.design open/chart-block-v2/decisions.md.
 *
 * This file holds no chart rules: chart types and families, the ink the
 * canvas draws with, number and tooltip formatting, the palette.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});

  const AGGREGATED_TYPES = ['bar', 'waterfall', 'pie', 'treemap', 'boxplot', 'pointrange', 'radar'];
  const INDIVIDUAL_TYPES = ['scatter', 'line', 'band'];
  const TIMELINE_TYPES = ['gantt'];
  // Box and point range summarize raw values per group in the browser.
  const DISTRIBUTION_TYPES = ['boxplot', 'pointrange'];

  /** @param {any} type @returns {'aggregated' | 'timeline' | 'individual'} */
  const familyOf = (type) => AGGREGATED_TYPES.includes(type) ? 'aggregated'
    : TIMELINE_TYPES.includes(type) ? 'timeline' : 'individual';

  /**
   * @typedef {{ muted: string, strong: string, border: string, surface: string,
   *             danger: string, raised: string, text: string, shadow: string,
   *             fontSize: number, face: string }} Ink
   */
  // The light tokens, used when the page loads no token sheet. ECharts cannot
  // resolve var(), so the view reads the tokens into an ink object per draw.
  /** @type {Readonly<Ink>} */
  const INK_DEFAULT = Object.freeze({
    muted: '#6b7280',
    strong: '#d1d5db',
    border: '#e5e7eb',
    surface: '#ffffff',
    danger: '#dc2626',
    raised: '#ffffff',
    text: '#111827',
    shadow: '0 4px 12px rgba(0, 0, 0, 0.1)',
    fontSize: 11,
    face: "'Open Sans', system-ui, sans-serif"
  });

  /**
   * The design tokens the canvas draws with, read off `el`.
   * @param {Element} el
   * @returns {Ink}
   */
  const readInk = (el) => {
    const cs = getComputedStyle(el);
    /** @param {string} name @param {string} fb */
    const tok = (name, fb) => cs.getPropertyValue('--blockr-' + name).trim() || fb;
    return {
      muted: tok('color-text-muted', INK_DEFAULT.muted),
      strong: tok('color-border-strong', INK_DEFAULT.strong),
      border: tok('color-border-default', INK_DEFAULT.border),
      surface: tok('color-bg-surface', INK_DEFAULT.surface),
      danger: tok('color-border-danger', INK_DEFAULT.danger),
      raised: tok('color-bg-raised', INK_DEFAULT.raised),
      text: tok('color-text-default', INK_DEFAULT.text),
      shadow: tok('shadow-md', INK_DEFAULT.shadow),
      fontSize: parseFloat(tok('mark-font-size', '11')) || 11,
      face: getComputedStyle(document.body).fontFamily || INK_DEFAULT.face
    };
  };

  // Escape a data value for the tooltip HTML.
  /** @param {any} s */
  const esc = (s) => String(s == null ? '' : s)
    .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');

  // The data tooltip (design system, Charts): a headline and label/value
  // rows, styled by chart.css. Callers pass escaped HTML.
  /** @param {any} c */
  const tipSwatch = (c) => typeof c === 'string' && c
    ? '<span class="dd-tt-sw" style="background:' + c + '"></span>' : '';
  /** @param {string} text @param {any} [color] */
  const tipHead = (text, color) =>
    '<div class="dd-tt-head">' + tipSwatch(color) + '<span>' + text + '</span></div>';
  /** @param {string} label @param {any} value @param {any} [color] */
  const tipRow = (label, value, color) =>
    '<div class="dd-tt-row">' + tipSwatch(color) + '<span class="dd-tt-label">' +
    label + '</span><span class="dd-tt-value">' + value + '</span></div>';

  /** A muted line of its own, for what the rows below it are. @param {string} text */
  const tipNote = (text) => '<div class="dd-tt-row dd-tt-label">' + text + '</div>';
  const TIP_SEP = '<div class="dd-tt-sep"></div>';

  /**
   * A headline over label/value rows; empty values are dropped.
   * @param {any} headline @param {Array<[string, any]>} pairs @param {any} [color]
   */
  const rowTooltip = (headline, pairs, color) =>
    tipHead(esc(headline), color) + pairs
      .filter((p) => p[1] != null && p[1] !== '')
      .map((p) => tipRow(esc(p[0]), esc(p[1])))
      .join('');

  /**
   * The raised card every tooltip sits on. Mutates and returns the option.
   * @param {any} option @param {Ink} ink
   */
  const cardTooltip = (option, ink) => {
    const tts = Array.isArray(option.tooltip) ? option.tooltip
      : option.tooltip ? [option.tooltip] : [];
    for (const t of tts) {
      Object.assign(t, {
        backgroundColor: ink.raised, borderColor: ink.border, borderWidth: 1,
        padding: [6, 10],
        textStyle: { color: ink.text, fontSize: 12, fontFamily: ink.face },
        extraCssText: 'border-radius:8px;box-shadow:' + ink.shadow + ';'
      });
    }
    return option;
  };

  // Display formatting only; data keeps full precision so a click filter
  // round-trips. Six significant digits.
  /** @param {any} v */
  const ddNum = (v) => {
    if (typeof v !== 'number' || !isFinite(v)) return v;
    if (Number.isInteger(v)) return v.toLocaleString();
    return Number(v.toPrecision(6)).toLocaleString(undefined, { maximumFractionDigits: 4 });
  };
  // Three significant digits, for raw observations glanced at on hover.
  /** @param {any} v */
  const ddNum3 = (v) => {
    if (typeof v !== 'number' || !isFinite(v)) return v;
    if (Number.isInteger(v)) return v.toLocaleString();
    return Number(v.toPrecision(3)).toLocaleString(undefined, { maximumFractionDigits: 4 });
  };

  // Okabe-Ito, the fallback when the board theme sends no `palette`. Past
  // its end every level is one grey: recycling would make level 8 read as
  // level 1. Mirrored by dd_level_palette() in R/theme-palette.R.
  const BLOCKR_PALETTE = ['#0072B2', '#D55E00', '#F0E442', '#009E73', '#56B4E9', '#E69F00', '#CC79A7'];
  const PALETTE_OVERFLOW = '#9AA0A6';
  /** @param {string[]} pal @param {number} i */
  const paletteAt = (pal, i) => (i < pal.length ? pal[i] : PALETTE_OVERFLOW);

  /** The series pool: the board theme's categorical role, else the built-in.
   *  @param {Record<string, any>} cfg @returns {string[]} */
  const paletteOf = (cfg) => {
    const p = cfg && cfg.palette;
    return (Array.isArray(p) && p.length) ? p : BLOCKR_PALETTE;
  };

  /** The board scale for `varName`, or null when the scale targets another
   *  column. @param {Record<string, any>} cfg @param {any} varName */
  const scaleFor = (cfg, varName) => {
    const sc = cfg && cfg.scales;
    return (sc && varName && sc.var === varName) ? sc : null;
  };

  /**
   * One colour per level: the board scale's colour, else the palette in
   * level order. The legend band and the option both read this.
   * @param {any[]} levels @param {any} scale @param {string[]} palette
   * @returns {string[]}
   */
  const levelColors = (levels, scale, palette) => levels.map((lvl, i) =>
    (scale && scale.color && scale.color[lvl]) || paletteAt(palette, i));

  /** @param {any} v */
  const hasVal = (v) => v !== null && v !== undefined && v !== '' && v !== '(none)';

  // Largest / smallest without spreading into arguments (a RangeError past
  // about 100k values). NaN as soon as one value is NaN, like Math.max.
  /** @param {ArrayLike<any>} xs */
  const maxOf = (xs) => {
    let m = -Infinity;
    for (let i = 0; i < xs.length; i++) {
      const v = +xs[i];
      if (v !== v) return NaN;
      if (v > m) m = v;
    }
    return m;
  };
  /** @param {ArrayLike<any>} xs */
  const minOf = (xs) => {
    let m = Infinity;
    for (let i = 0; i < xs.length; i++) {
      const v = +xs[i];
      if (v !== v) return NaN;
      if (v < m) m = v;
    }
    return m;
  };

  /** Value -> its first position in `arr`, for a lookup made once per row.
   *  @param {any[]} arr @returns {Map<any, number>} */
  const firstIndex = (arr) => {
    const m = new Map();
    for (let i = 0; i < arr.length; i++) if (!m.has(arr[i])) m.set(arr[i], i);
    return m;
  };

  /** The tri-state drill: 'off', 'auto' or 'column'. @param {Record<string, any>} cfg */
  const drillState = (cfg) => {
    const d = cfg.drill;
    if (!d || d === '') return 'off';
    return d === 'auto' ? 'auto' : 'column';
  };

  /**
   * The column a click filters on, or null for a geometric selection. An
   * override names its column; auto takes the family's natural key.
   * @param {Record<string, any>} cfg @returns {string | null}
   */
  const drillColumn = (cfg) => {
    const d = cfg.drill;
    if (!d || d === '') return null;
    if (d !== 'auto') return d;
    if (cfg.chart_type === 'radar') return cfg.color || null;
    const fam = familyOf(cfg.chart_type);
    if (fam === 'aggregated') return cfg.group || null;
    if (fam === 'timeline') return cfg.y || null;
    return cfg.series || cfg.color || null;
  };

  /** A ctrl_target makes a click an event sent to another block. @param {Record<string, any>} cfg */
  const transientDrill = (cfg) => {
    const t = cfg.ctrl_target;
    return !!(t && String(t).trim());
  };

  /**
   * With drill off the chart is a display: no hover emphasis and no hand
   * cursor. Mutates and returns the option.
   * @param {any} option @param {Record<string, any>} cfg
   */
  const applyDrillEmphasis = (option, cfg) => {
    if (drillState(cfg) === 'off' && option && Array.isArray(option.series)) {
      for (const s of option.series) {
        if (!s) continue;
        s.emphasis = { disabled: true };
        s.cursor = 'default';
        if (s.itemStyle) s.itemStyle.cursor = 'default';
        if (s.lineStyle) s.lineStyle.cursor = 'default';
      }
    }
    return option;
  };

  /**
   * Panel scales per family: free_y only exists where the value axis is y.
   * @param {Record<string, any>} cfg @returns {'fixed' | 'free' | 'free_y'}
   */
  const facetScales = (cfg) => {
    const m = cfg.facet_scales || 'fixed';
    if (m !== 'fixed' && m !== 'free' && m !== 'free_y') return 'fixed';
    if (m === 'free_y' && familyOf(cfg.chart_type) !== 'individual') return 'free';
    return m;
  };

  // Ceiling for a panel's plot height. A canvas taller than the browser's
  // area limit renders blank, so past it the lanes squeeze instead.
  const PANEL_H_CAP = 4000;

  Object.assign(NS, {
    AGGREGATED_TYPES, INDIVIDUAL_TYPES, TIMELINE_TYPES, DISTRIBUTION_TYPES, familyOf,
    INK_DEFAULT, readInk, esc, tipHead, tipRow, tipNote, TIP_SEP, rowTooltip, cardTooltip, ddNum, ddNum3,
    BLOCKR_PALETTE, PALETTE_OVERFLOW, paletteAt, paletteOf, scaleFor, levelColors,
    hasVal, maxOf, minOf, firstIndex, drillState, drillColumn, transientDrill,
    applyDrillEmphasis, facetScales, PANEL_H_CAP
  });
})();
