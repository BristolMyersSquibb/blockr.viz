// @ts-check
/**
 * Chart block: the namespace (window.Blockr.viz.chart) and what every other
 * file shares: chart families, ink, tooltip and number formatting, palette,
 * highlight, drill and legend rules. In node the namespace hangs off the
 * vm context's global. Comments cite click rules (D1-D11) and fixes (B1-B11)
 * from blockr.design open/chart-block-v2/decisions.md.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});

  const AGGREGATED_TYPES = ['bar', 'histogram', 'waterfall', 'pie', 'treemap', 'boxplot', 'pointrange'];
  // Box and point range summarize raw values per group in the browser.
  const DISTRIBUTION_TYPES = ['boxplot', 'pointrange'];
  // An area is a line with its area filled.
  /** @param {any} t */
  const isLineType = (t) => t === 'line' || t === 'area';

  /** @param {any} type @returns {'aggregated' | 'timeline' | 'individual'} */
  const familyOf = (type) => AGGREGATED_TYPES.includes(type) ? 'aggregated'
    : type === 'gantt' ? 'timeline' : 'individual';

  /** @typedef {{ muted: string, secondary: string, strong: string, border: string, surface: string,
   *              danger: string, raised: string, text: string, shadow: string, fontSize: number,
   *              face: string }} Ink */
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

  /** Escape a data value for the tooltip HTML. @param {any} s */
  const esc = (s) => String(s == null ? '' : s)
    .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');

  // The data tooltip: a headline and label/value rows, styled by chart.css.
  // Callers pass escaped HTML.
  /** @param {any} c */
  const tipSwatch = (c) => typeof c === 'string' && c
    ? '<span class="dd-tt-sw" style="background:' + c + '"></span>' : '';
  /** @param {string} text @param {any} [color] */
  const tipHead = (text, color) =>
    '<div class="dd-tt-head">' + tipSwatch(color) + '<span>' + text + '</span></div>';
  /** `hit` marks a row of a card listing every level of a colour split: true for the level
   * under the pointer, false for the others.
   * @param {string} label @param {any} value @param {any} [color] @param {boolean | null} [hit] */
  const tipRow = (label, value, color, hit) =>
    '<div class="dd-tt-row' + (hit === true ? ' is-hit' : hit === false ? ' is-dim' : '') +
    '">' + tipSwatch(color) + '<span class="dd-tt-label">' +
    label + '</span><span class="dd-tt-value">' + value + '</span></div>';

  /** A muted line of its own, for what the rows below it are. @param {string} text */
  const tipNote = (text) => '<div class="dd-tt-row dd-tt-label">' + text + '</div>';
  const TIP_SEP = '<div class="dd-tt-sep"></div>';

  /** A headline over label/value rows; empty values are dropped.
   * @param {any} headline @param {Array<[string, any]>} pairs @param {any} [color] */
  const rowTooltip = (headline, pairs, color) =>
    tipHead(esc(headline), color) + pairs
      .filter((p) => p[1] != null && p[1] !== '')
      .map((p) => tipRow(esc(p[0]), esc(p[1])))
      .join('');

  /** The raised card every tooltip sits on; mutates the option. @param {any} option @param {Ink} ink */
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
  // round-trips. As the table block's "number" format (dt_format_num()): up
  // to two decimals, and a value that would round to 0 keeps two significant
  // digits (0.0034).
  /** @param {any} v */
  const ddNum = (v) => {
    if (typeof v !== 'number' || !isFinite(v)) return v;
    if (Number.isInteger(v)) return v.toLocaleString();
    if (Math.abs(v) < 0.005) return v.toLocaleString(undefined, { maximumSignificantDigits: 2 });
    return v.toLocaleString(undefined, { maximumFractionDigits: 2 });
  };
  /** Raw observations on hover: three significant digits at most. @param {any} v */
  const ddNum3 = (v) => {
    if (typeof v !== 'number' || !isFinite(v)) return v;
    return ddNum(Number(v.toPrecision(3)));
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
   *  @param {Record<string, any>} cfg @returns {string[]} */
  const paletteOf = (cfg) => {
    const p = cfg.derived.palette;
    return (Array.isArray(p) && p.length) ? p : BLOCKR_PALETTE;
  };

  /** The board scale for `varName`, or null when it targets another column.
   *  @param {Record<string, any>} cfg @param {any} varName */
  const scaleFor = (cfg, varName) => {
    const sc = cfg.derived.scales;
    return (sc && varName && sc.var === varName) ? sc : null;
  };

  /** One colour per level: the board scale's colour, else the palette in level order.
   * @param {any[]} levels @param {any} scale @param {string[]} palette @returns {string[]} */
  const levelColors = (levels, scale, palette) => levels.map((lvl, i) =>
    (scale && scale.color && scale.color[lvl]) || paletteAt(palette, i));

  /** @param {any} v */
  const hasVal = (v) => v !== null && v !== undefined && v !== '' && v !== '(none)';

  /** A `numbers` or `values` setting, `[{value, label}]`, or [] when unset.
   * @param {any} v @returns {{ value: any, label: string | null }[]} */
  const labelled = (v) => (Array.isArray(v) ? v : []);

  /** The `x_ranges` setting, `{name: [from, to]}`, as a list of spans.
   * @param {any} r @returns {{ name: string, from: any, to: any }[]} */
  const rangesOf = (r) => (r && typeof r === 'object' ? Object.entries(r) : [])
    .map(([name, span]) => {
      const s = labelled(span);
      return { name, from: s[0] ? s[0].value : null, to: s[1] ? s[1].value : null };
    }).filter((x) => x.from != null && x.to != null);

  // `highlight` names a column and some of its values. A mark is lit when its
  // rows hold one of them (chart-keys.js markLit); the others are drawn in
  // the muted grey, behind. The types that draw it:
  const HIGHLIGHT_TYPES = ['bar', 'scatter', 'line', 'area'];

  /** The highlight as a filter, {col: [values]}, or null.
   * @param {Record<string, any>} cfg @returns {Record<string, string[]> | null} */
  const highlightOf = (cfg) => {
    const h = cfg.highlight;
    const col = h && typeof h === 'object' ? Object.keys(h)[0] : null;
    if (!col || !HIGHLIGHT_TYPES.includes(cfg.type)) return null;
    const vals = labelled(h[col]).map((x) => x.value).filter(hasVal).map(String);
    return vals.length ? { [col]: vals } : null;
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

  // Largest / smallest without spreading into arguments (a RangeError past
  // about 100k values). NaN as soon as one value is NaN, like Math.max.
  /** @param {ArrayLike<any>} xs @param {number} init @param {(v: number, m: number) => boolean} wins */
  const extreme = (xs, init, wins) => {
    let m = init;
    for (let i = 0; i < xs.length; i++) {
      const v = +xs[i];
      if (v !== v) return NaN;
      if (wins(v, m)) m = v;
    }
    return m;
  };
  /** @param {ArrayLike<any>} xs */
  const maxOf = (xs) => extreme(xs, -Infinity, (v, m) => v > m);
  /** @param {ArrayLike<any>} xs */
  const minOf = (xs) => extreme(xs, Infinity, (v, m) => v < m);

  /** Value -> its first position in `arr`, for a lookup made once per row.
   *  @param {any[]} arr @returns {Map<any, number>} */
  const firstIndex = (arr) => {
    const m = new Map();
    for (let i = 0; i < arr.length; i++) if (!m.has(arr[i])) m.set(arr[i], i);
    return m;
  };

  /** The tri-state drill: 'off', 'auto' or 'column'. @param {Record<string, any>} cfg */
  const drillState = (cfg) => (!cfg.drill ? 'off' : cfg.drill === 'auto' ? 'auto' : 'column');

  /** The column a click filters on (the family's natural key for auto), or null for a
   * geometric selection. @param {Record<string, any>} cfg @returns {string | null} */
  const drillColumn = (cfg) => {
    const d = cfg.drill;
    if (!d) return null;
    if (d !== 'auto') return d;
    const fam = familyOf(cfg.type);
    if (fam === 'aggregated') return cfg.category || null;
    if (fam === 'timeline') return cfg.y || null;
    return cfg.series || cfg.color || null;
  };

  /** A drill_target makes a click an event sent to another block. @param {Record<string, any>} cfg */
  const transientDrill = (cfg) => !!(cfg.drill_target && String(cfg.drill_target).trim());

  /** With drill off the chart is a display: no hover emphasis, no hand cursor. Mutates
   * the option. @param {any} option @param {Record<string, any>} cfg */
  const applyDrillEmphasis = (option, cfg) => {
    if (drillState(cfg) === 'off' && option && Array.isArray(option.series)) {
      for (const s of option.series) {
        if (!s) continue;
        // A line's emphasis without focus or scaling draws its dot at the
        // crosshair; it is no drill affordance, so it stays.
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

  /** Panel scales: free_y only exists where the value axis is y.
   * @param {Record<string, any>} cfg @returns {'fixed' | 'free' | 'free_y'} */
  const facetScales = (cfg) =>
    (cfg.facet_scales === 'free_y' && familyOf(cfg.type) !== 'individual' ? 'free' : cfg.facet_scales);

  /** The axes drawn on a log scale: `y_transform` on a scatter, line or band, `x_transform`
   * on a scatter (the model also needs a numeric axis).
   * @param {Record<string, any>} cfg @returns {{ x: boolean, y: boolean }} */
  const logAxes = (cfg) => {
    const t = cfg.type;
    return {
      y: cfg.y_transform === 'log10' && (t === 'scatter' || t === 'line' || t === 'band'),
      x: cfg.x_transform === 'log10' && t === 'scatter'
    };
  };

  // Past this many levels the names on the marks of a line crowd each other.
  const MARKS_MAX_LEVELS = 8;

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
      // Chips that are not the series have no mark to sit on.
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
          cfg.position !== 'dodge') {
        const vertical = cfg.orientation === 'vertical';
        // A level the named bar lacks would have no name: the legend stays.
        return stackCovers(m, vertical) ? (vertical ? 'stack' : 'stack-h') : null;
      }
      if (m.type === 'pointrange' && m.split) return 'centroid';
    }
    return null;
  };

  /** Does the bar the names go on (the last with a segment, or the first lying down) carry
   * every level, in every panel? @param {any} m the aggregated model @param {boolean} vertical */
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

  /** The swatch of a legend chip: a line for lines, a dot for points, else a square.
   * @param {any} type @returns {'line' | 'dot' | 'square'} */
  const swatchOf = (type) => (type === 'line' || type === 'band') ? 'line'
    : (type === 'scatter' || type === 'pointrange') ? 'dot' : 'square';

  /** What the legend does: whether the HTML band shows and where, how the names go on the
   * marks, the swatch. "marks" where the chart has no place for names keeps the band below.
   * @param {any} m the model @param {Record<string, any>} cfg
   * @returns {{ setting: string, marks: ReturnType<typeof marksKind>, band: boolean,
   *             position: 'bottom' | 'top', swatch: 'line' | 'dot' | 'square' }} */
  const legendPlan = (m, cfg) => {
    const setting = cfg.legend;
    const marks = setting === 'marks' ? marksKind(m, cfg) : null;
    return { setting, marks, band: setting !== 'none' && !marks,
             position: setting === 'top' ? 'top' : 'bottom', swatch: swatchOf(cfg.type) };
  };

  /** The mean of a group's finite x and y, or null without a point.
   * @param {any[][]} pts @returns {[number, number] | null} */
  const cloudCentre = (pts) => {
    let sx = 0, sy = 0, n = 0;
    for (const p of pts) {
      const x = Number(p[0]), y = Number(p[1]);
      if (p[0] == null || p[1] == null || !Number.isFinite(x) || !Number.isFinite(y)) continue;
      sx += x; sy += y; n++;
    }
    return n ? [sx / n, sy / n] : null;
  };

  /** A name written at a point and nothing else: a silent scatter series with no symbol and a
   * haloed label. The id keeps the highlight patch off it.
   * @param {string} id @param {string} name @param {number[]} at @param {string} position
   * @param {any} ink @param {Record<string, any>} [extra] */
  const nameMark = (id, name, at, position, ink, extra) => ({
    id, type: 'scatter', data: [at], silent: true, symbolSize: 0, z: 11,
    legendHoverLink: false, animation: false, tooltip: { show: false },
    emphasis: { disabled: true },
    label: {
      show: true, formatter: name, position,
      color: ink.text, fontSize: ink.fontSize, fontWeight: 600,
      textBorderColor: ink.surface, textBorderWidth: 3,
      ...extra
    }
  });

  /** A new element with a class, appended to `parent` when given.
   * @param {string} tag @param {string} cls @param {HTMLElement} [parent] */
  const mk = (tag, cls, parent) => {
    const e = document.createElement(tag);
    e.className = cls;
    if (parent) parent.appendChild(e);
    return e;
  };

  // Ceiling for a panel's plot height: a canvas past the browser's area
  // limit renders blank, so past it the lanes squeeze instead.
  const PANEL_H_CAP = 4000;

  Object.assign(NS, {
    DISTRIBUTION_TYPES, familyOf, isLineType,
    INK_DEFAULT, readInk, esc, tipHead, tipRow, tipNote, TIP_SEP, rowTooltip, cardTooltip, ddNum, ddNum3,
    BLOCKR_PALETTE, paletteAt, paletteOf, scaleFor, levelColors,
    highlightOf, rgbOf, mutedMark, litLevelColors,
    hasVal, labelled, rangesOf, maxOf, minOf, firstIndex, drillState, drillColumn, transientDrill,
    applyDrillEmphasis, facetScales, logAxes, PANEL_H_CAP,
    legendPlan,
    cloudCentre, nameMark, mk
  });
})();
