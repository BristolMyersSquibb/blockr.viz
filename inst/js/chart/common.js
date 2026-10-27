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
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});

  const AGGREGATED_TYPES = ['bar', 'histogram', 'waterfall', 'pie', 'treemap', 'boxplot', 'pointrange'];
  const INDIVIDUAL_TYPES = ['scatter', 'line', 'area', 'band'];
  // An area is a line with its area filled: everything a line does, it does.
  /** @param {any} t */
  const isLineType = (t) => t === 'line' || t === 'area';
  const TIMELINE_TYPES = ['gantt'];
  // Box and point range summarize raw values per group in the browser.
  const DISTRIBUTION_TYPES = ['boxplot', 'pointrange'];

  /** @param {any} type @returns {'aggregated' | 'timeline' | 'individual'} */
  const familyOf = (type) => AGGREGATED_TYPES.includes(type) ? 'aggregated'
    : TIMELINE_TYPES.includes(type) ? 'timeline' : 'individual';

  /**
   * @typedef {{ muted: string, secondary: string, strong: string, border: string,
   *             surface: string, danger: string, raised: string, text: string, shadow: string,
   *             fontSize: number, face: string }} Ink
   */
  // The light tokens, used when the page loads no token sheet. ECharts cannot
  // resolve var(), so the view reads the tokens into an ink object per draw.
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
      secondary: tok('color-text-secondary', INK_DEFAULT.secondary),
      strong: tok('color-border-strong', INK_DEFAULT.strong),
      border: tok('color-border-default', INK_DEFAULT.border),
      surface: tok('color-bg-surface', INK_DEFAULT.surface),
      danger: tok('color-border-danger', INK_DEFAULT.danger),
      raised: tok('color-bg-raised', INK_DEFAULT.raised),
      text: tok('color-text-default', INK_DEFAULT.text),
      shadow: tok('shadow-md', INK_DEFAULT.shadow),
      fontSize: parseFloat(tok('mark-font-size', '12')) || 12,
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
  /**
   * `hit` marks a row in a card that lists every level of a colour split:
   * true for the level under the pointer, false for the others.
   * @param {string} label @param {any} value @param {any} [color]
   * @param {boolean | null} [hit]
   */
  const tipRow = (label, value, color, hit) =>
    '<div class="dd-tt-row' + (hit === true ? ' is-hit' : hit === false ? ' is-dim' : '') +
    '">' + tipSwatch(color) + '<span class="dd-tt-label">' +
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
  // round-trips. The "number" format, as the table block's
  // (dt_format_num() in R/table-push.R): up to two decimals, trailing zeros
  // dropped, and a value that would round to 0 keeps two significant
  // digits (0.0034).
  /** @param {any} v */
  const ddNum = (v) => {
    if (typeof v !== 'number' || !isFinite(v)) return v;
    if (Number.isInteger(v)) return v.toLocaleString();
    if (Math.abs(v) < 0.005) return v.toLocaleString(undefined, { maximumSignificantDigits: 2 });
    return v.toLocaleString(undefined, { maximumFractionDigits: 2 });
  };
  // Raw observations glanced at on hover: three significant digits at
  // most, then as ddNum().
  /** @param {any} v */
  const ddNum3 = (v) => {
    if (typeof v !== 'number' || !isFinite(v)) return v;
    return ddNum(Number(v.toPrecision(3)));
  };

  // blockr.theme's "Blockr" palette (Okabe-Ito without black and yellow), the
  // fallback when the board theme sends no `palette`. Past its end every level
  // is one grey: recycling would make level 7 read as level 1. Mirrored by
  // dd_level_palette() in R/theme-palette.R.
  const BLOCKR_PALETTE = ['#0072B2', '#E69F00', '#009E73', '#CC79A7', '#56B4E9', '#D55E00'];
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

  // -- Highlight ---------------------------------------------------------------
  // `highlight_by` names a column, `highlight` some of its values. A mark is
  // highlighted when its rows hold one of them (the rule a drill selection
  // lights marks by, chart-keys.js markLit); every other mark is drawn in
  // the muted grey, behind. The types that draw it:
  const HIGHLIGHT_TYPES = ['bar', 'scatter', 'line', 'area'];

  /**
   * The highlight as a filter, {col: [values]}, or null for none.
   * @param {Record<string, any>} cfg @returns {Record<string, string[]> | null}
   */
  const highlightOf = (cfg) => {
    if (!cfg || !HIGHLIGHT_TYPES.includes(cfg.chart_type) || !hasVal(cfg.highlight_by)) return null;
    const v = cfg.highlight;
    const vals = (Array.isArray(v) ? v : [v]).filter(hasVal).map(String);
    return vals.length ? { [String(cfg.highlight_by)]: vals } : null;
  };

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

  /**
   * The colour of a mark that is not highlighted: 40% of the muted text
   * over the surface, so it recedes in either scheme (about #c4c7cc on
   * white).
   * @param {any} ink
   */
  const mutedMark = (ink) => {
    const a = rgbOf(ink && ink.muted), b = rgbOf(ink && ink.surface);
    if (!a || !b) return (ink && ink.strong) || INK_DEFAULT.strong;
    const hex = a.map((v, i) => Math.round(0.4 * v + 0.6 * b[i]).toString(16).padStart(2, '0'));
    return '#' + hex.join('');
  };

  /**
   * One colour per level when only the `lit` levels are highlighted: the
   * board scale's colour, else the palette counted over the lit levels, so
   * a highlighted level never lands past the palette's end. Null for a level
   * that is not lit.
   * @param {any[]} levels @param {Set<any>} lit @param {any} scale @param {string[]} palette
   * @returns {Array<string | null>}
   */
  const litLevelColors = (levels, lit, scale, palette) => {
    let k = 0;
    return levels.map((lvl) => {
      if (!lit.has(lvl)) return null;
      const i = k++;
      return (scale && scale.color && scale.color[lvl]) || paletteAt(palette, i);
    });
  };

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
        // A line's emphasis without focus or scaling is what draws its dot
        // at the crosshair; it is no drill affordance, so it stays.
        const crosshairDot = s.type === 'line' && s.emphasis &&
          s.emphasis.focus === 'none' && s.emphasis.scale === false;
        if (!crosshairDot) s.emphasis = { disabled: true };
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

  /**
   * Which axes a chart draws on a log scale: `y_scale` on a scatter, line or
   * band, `x_scale` on a scatter. The model also needs the axis to be a
   * number (not a date or a category).
   * @param {Record<string, any>} cfg @returns {{ x: boolean, y: boolean }}
   */
  const logAxes = (cfg) => {
    const t = cfg.chart_type;
    return {
      y: cfg.y_scale === 'log' && (t === 'scatter' || t === 'line' || t === 'band'),
      x: cfg.x_scale === 'log' && t === 'scatter'
    };
  };

  // -- Legend ------------------------------------------------------------------

  // Where the colour legend goes: a band below the plot (the default) or above
  // it, the names written on the marks, or nothing.
  const LEGEND_MODES = ['bottom', 'top', 'marks', 'none'];
  // Past this many levels the names on the marks of a line crowd each other.
  const MARKS_MAX_LEVELS = 8;

  /**
   * The `legend` setting. A config from before it, with `direct_labels` on,
   * means the names on the marks.
   * @param {Record<string, any>} cfg
   * @returns {'bottom' | 'top' | 'marks' | 'none'}
   */
  const legendSetting = (cfg) => {
    const v = cfg && cfg.legend;
    if (LEGEND_MODES.includes(v)) return v;
    const dl = cfg && cfg.direct_labels;
    return (dl === 'on' || dl === true) ? 'marks' : 'bottom';
  };

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
      // Chips that are not the series (a series column apart from the colour)
      // have no mark to sit on.
      if (!m.legend || m.byColor) return null;
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
          (cfg.bar_mode || 'stacked') !== 'grouped') {
        const vertical = cfg.orientation === 'vertical';
        // The names sit on one bar (the last, or the first lying down); a
        // level that bar lacks would have no name, so the legend stays.
        return stackCovers(m, vertical) ? (vertical ? 'stack' : 'stack-h') : null;
      }
      if (m.type === 'pointrange' && m.split) return 'centroid';
    }
    return null;
  };

  /**
   * Does the bar the names go on (the last with a segment, or the first lying
   * down) carry every level, in every panel?
   * @param {any} m the aggregated model @param {boolean} vertical
   */
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

  /**
   * The swatch of a legend chip, by the chart's mark: a short line for lines,
   * a dot for points, a square for the rest.
   * @param {any} type @returns {'line' | 'dot' | 'square'}
   */
  const swatchOf = (type) => (type === 'line' || type === 'band') ? 'line'
    : (type === 'scatter' || type === 'pointrange') ? 'dot' : 'square';

  /**
   * What the legend does for this chart: whether the HTML band shows and
   * where, how the names go on the marks (null when they do not), and the
   * swatch. "marks" where the chart has no place for names keeps the band,
   * at the bottom.
   * @param {any} m the model @param {Record<string, any>} cfg
   * @returns {{ setting: string, marks: ReturnType<typeof marksKind>, band: boolean,
   *             position: 'bottom' | 'top', swatch: 'line' | 'dot' | 'square' }}
   */
  const legendPlan = (m, cfg) => {
    const setting = legendSetting(cfg);
    const swatch = swatchOf(cfg.chart_type);
    if (setting === 'none') return { setting, marks: null, band: false, position: 'bottom', swatch };
    if (setting === 'marks') {
      const marks = marksKind(m, cfg);
      return { setting, marks, band: !marks, position: 'bottom', swatch };
    }
    return { setting, marks: null, band: true, position: setting, swatch };
  };

  /**
   * The centre of mass of a group's points: the mean of the finite x and y.
   * Null without a point.
   * @param {any[][]} pts @returns {[number, number] | null}
   */
  const cloudCentre = (pts) => {
    let sx = 0, sy = 0, n = 0;
    for (const p of pts) {
      const x = Number(p[0]), y = Number(p[1]);
      if (p[0] == null || p[1] == null || !Number.isFinite(x) || !Number.isFinite(y)) continue;
      sx += x; sy += y; n++;
    }
    return n ? [sx / n, sy / n] : null;
  };

  /**
   * A name written at a point and nothing else: a silent scatter series with
   * no symbol and a label with a halo in the surface colour, so it reads over
   * the marks beneath. Semibold text ink, as the line end labels. The id
   * keeps the highlight patch off it.
   * @param {string} id @param {string} name @param {number[]} at
   * @param {string} position @param {any} ink @param {Record<string, any>} [extra]
   */
  const nameMark = (id, name, at, position, ink, extra) => ({
    id, type: 'scatter', data: [at], silent: true, symbolSize: 0, z: 11,
    legendHoverLink: false, animation: false, tooltip: { show: false },
    emphasis: { disabled: true },
    label: {
      show: true, formatter: name, position,
      color: (ink && ink.text) || '#111827', fontSize: (ink && ink.fontSize) || 12,
      fontWeight: 600,
      textBorderColor: (ink && ink.surface) || '#ffffff', textBorderWidth: 3,
      ...extra
    }
  });

  // Ceiling for a panel's plot height. A canvas taller than the browser's
  // area limit renders blank, so past it the lanes squeeze instead.
  const PANEL_H_CAP = 4000;

  Object.assign(NS, {
    AGGREGATED_TYPES, INDIVIDUAL_TYPES, TIMELINE_TYPES, DISTRIBUTION_TYPES, familyOf, isLineType,
    INK_DEFAULT, readInk, esc, tipHead, tipRow, tipNote, TIP_SEP, rowTooltip, cardTooltip, ddNum, ddNum3,
    BLOCKR_PALETTE, PALETTE_OVERFLOW, paletteAt, paletteOf, scaleFor, levelColors,
    HIGHLIGHT_TYPES, highlightOf, mutedMark, litLevelColors,
    hasVal, maxOf, minOf, firstIndex, drillState, drillColumn, transientDrill,
    applyDrillEmphasis, facetScales, logAxes, PANEL_H_CAP,
    LEGEND_MODES, MARKS_MAX_LEVELS, legendSetting, marksKind, swatchOf, legendPlan,
    cloudCentre, nameMark
  });
})();
