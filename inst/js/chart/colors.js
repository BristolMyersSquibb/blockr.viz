// @ts-check
/**
 * Chart block: the colours the canvas draws with. ECharts cannot resolve
 * var(), so the view reads the design tokens into an ink object per draw.
 * The series colours come from the board theme's palette and scale, else
 * from the built-in palette.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  // Light tokens for a page without a token sheet. ECharts cannot resolve
  // var(), so the view reads the tokens into an ink object per draw.
  /** @type {Readonly<Ink>} */
  const INK_DEFAULT = Object.freeze({
    muted: '#6b7280',
    secondary: '#374151',
    strong: '#d1d5db',
    border: '#e5e7eb',
    surface: '#ffffff',
    danger: '#dc2626',
    raised: '#ffffff',
    text: '#111827',
    shadow: '0 4px 12px rgba(0, 0, 0, 0.1)',
    fontSize: 12,
    face: "'Open Sans', system-ui, sans-serif"
  });

  /** @type {Record<string, string>} */
  const INK_TOKENS = {
    muted: 'color-text-muted', secondary: 'color-text-secondary', strong: 'color-border-strong',
    border: 'color-border-default', surface: 'color-bg-surface', danger: 'color-border-danger',
    raised: 'color-bg-raised', text: 'color-text-default', shadow: 'shadow-md'
  };

  /** The design tokens the canvas draws with, read off `el`. @param {Element} el @returns {Ink} */
  const readInk = (el) => {
    const cs = getComputedStyle(el);
    /** @param {string} name */
    const tok = (name) => cs.getPropertyValue('--blockr-' + name).trim();
    const ink = /** @type {Record<string, any>} */ ({ ...INK_DEFAULT });
    for (const [key, name] of Object.entries(INK_TOKENS)) ink[key] = tok(name) || ink[key];
    ink.fontSize = parseFloat(tok('mark-font-size')) || 12;
    ink.face = getComputedStyle(document.body).fontFamily || INK_DEFAULT.face;
    return /** @type {Ink} */ (ink);
  };

  // blockr.theme's "Blockr" palette (Okabe-Ito without black and yellow), the
  // fallback when the board theme sends no `palette`. Past its end every level
  // is one grey: recycling would make level 7 read as level 1. Mirrors
  // dd_level_palette() in R/theme-palette.R.
  const BLOCKR_PALETTE = ['#0072B2', '#E69F00', '#009E73', '#CC79A7', '#56B4E9', '#D55E00'];
  const PALETTE_OVERFLOW = '#9AA0A6';
  /** @param {string[]} pal @param {number} i */
  const paletteAt = (pal, i) => (i < pal.length ? pal[i] : PALETTE_OVERFLOW);

  /** The series pool: the board theme's categorical role, else the built-in.
   *  @param {ChartDerived} derived @returns {string[]} */
  const paletteOf = (derived) => {
    const p = derived.palette;
    return (Array.isArray(p) && p.length) ? p : BLOCKR_PALETTE;
  };

  /** The board scale for `varName`, or null when it targets another column.
   *  @param {ChartDerived} derived @param {any} varName */
  const scaleFor = (derived, varName) => {
    const sc = derived.scales;
    return (sc && varName && sc.var === varName) ? sc : null;
  };

  /** One colour per level: the board scale's colour, else the palette in level order.
   * @param {any[]} levels @param {any} scale @param {string[]} palette @returns {string[]} */
  const levelColors = (levels, scale, palette) => levels.map((lvl, i) =>
    (scale && scale.color && scale.color[lvl]) || paletteAt(palette, i));

  /** A CSS colour as [r, g, b], or null. @param {any} c */
  const rgbOf = (c) => {
    const t = String(c || '').trim();
    let m = /^#([0-9a-f]{3})$/i.exec(t);
    if (m) return m[1].split('').map((h) => parseInt(h + h, 16));
    const h6 = /^#([0-9a-f]{6})$/i.exec(t);
    if (h6) return [0, 2, 4].map((i) => parseInt(h6[1].slice(i, i + 2), 16));
    m = /^rgba?\(\s*(\d+)[\s,]+(\d+)[\s,]+(\d+)/i.exec(t);
    return m ? [Number(m[1]), Number(m[2]), Number(m[3])] : null;
  };

  /** A mark that is not highlighted: 40% of the muted text over the surface (about #c4c7cc
   * on white), so it recedes in either scheme. @param {Ink} ink */
  const mutedMark = (ink) => {
    const a = rgbOf(ink.muted), b = rgbOf(ink.surface);
    if (!a || !b) return ink.strong;
    const hex = a.map((v, i) => Math.round(0.4 * v + 0.6 * b[i]).toString(16).padStart(2, '0'));
    return '#' + hex.join('');
  };

  /** One colour per lit level, null for the others. The palette is counted over the lit
   * levels, so a lit level never lands past its end.
   * @param {any[]} levels @param {Set<any>} lit @param {any} scale @param {string[]} palette
   * @returns {Array<string | null>} */
  const litLevelColors = (levels, lit, scale, palette) => {
    const colors = levelColors(levels.filter((l) => lit.has(l)), scale, palette);
    let k = 0;
    return levels.map((l) => (lit.has(l) ? colors[k++] : null));
  };

  Object.assign(NS, { INK_DEFAULT, readInk, BLOCKR_PALETTE, paletteAt, paletteOf, scaleFor, levelColors,
                      rgbOf, mutedMark, litLevelColors });
})();
