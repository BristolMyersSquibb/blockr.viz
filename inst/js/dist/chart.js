// blockr.viz bundle b3c39f642417fa189a30762e77676f6e, built by tools/bundle.js. Do not edit.
// sources: drilldown-theme-register.js chart/common.js chart/memo.js chart/chart-types.js chart/colors.js chart/drill.js chart/tooltip.js chart/legend.js chart/format.js chart/agg.js chart/chart-index.js chart/chart-keys.js chart/axes-time.js chart/axes-labels.js chart/model-common.js chart/model-timeline.js chart/model-aggregated.js chart/model-individual.js chart/option-timeline.js chart/option-bar-marks.js chart/option-bar.js chart/option-radial.js chart/option-distribution.js chart/option-aggregated.js chart/option-points.js chart/option-band.js chart/option-domains.js chart/option-individual.js chart/ticks.js chart/zoom-option.js chart/gear.js chart/selection.js chart/family-aggregated.js chart/family-timeline.js chart/family-individual.js chart/zoom.js chart/export.js chart/panels.js chart/view.js
// --- drilldown-theme-register.js
// @ts-check
// Register the "blockr" echarts theme directly in the browser. The drill-down
// block initialises echarts via `echarts.init(el, name)`, so the name must be
// known to `window.echarts` before init runs. Echarts' built-in themes (dark,
// vintage, etc.) are loaded from echarts4r's bundled theme directory via a
// separate htmlDependency; this file only handles the custom "blockr" palette.
(() => {
  if (typeof window === 'undefined' || !window.echarts) return;
  if (typeof window.echarts.registerTheme !== 'function') return;
  window.echarts.registerTheme('blockr', {
    color: ['#0072B2', '#E69F00', '#009E73', '#CC79A7', '#56B4E9', '#D55E00'],
    backgroundColor: '#ffffff',
    textStyle: { color: '#333333', fontFamily: 'Open Sans' }
  });
})();
// --- chart/common.js
// @ts-check
/**
 * Chart block: the namespace (Blockr.viz.chart), which every chart file
 * fills, and the small helpers they share: elements, set values, the
 * labelled lists of the spec's wire form, extremes and first positions.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart = {};

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

  // A fixed plot height, shorter in a panel grid (facet_row), where panels
  // stack in rows. Heights that grow with the data (bar rows, gantt lanes)
  // do not go through here.
  /** @param {any} cfg @param {number} h @returns {number} */
  const panelH = (cfg, h) => (cfg && cfg.facet_grid ? Math.round(h * 0.65) : h);

  Object.assign(NS, { hasVal, labelled, rangesOf, maxOf, minOf, firstIndex, mk, PANEL_H_CAP, panelH });
})();
// --- chart/memo.js
// @ts-check
/**
 * Chart block: a memo keyed on what the computation read. A model or an
 * option builder runs on settings and a `derived` that record every field
 * read (`derived.x` for a field of derived); its result is reused while
 * those fields hold the same values and the other inputs (`extra`) are the
 * same. No list of fields is kept by hand, so a field that is read cannot be
 * missing from the key.
 *
 * The same inputs take the same path through the code and read the same
 * fields, so equal values in the fields read last time mean an equal
 * result. The inputs record reads only while the computation runs; a
 * reused result costs one comparison of those fields.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** A field's value: `derived.x` from derived, else from the settings.
   * @param {Record<string, any>} cfg @param {Record<string, any>} derived @param {string} f */
  const valueAt = (cfg, derived, f) => (f.startsWith('derived.') ? derived[f.slice(8)] : cfg[f]);

  /** @param {Record<string, any>} cfg @param {Record<string, any>} derived
   * @param {string[]} reads @param {string} extra */
  const keyOf = (cfg, derived, reads, extra) =>
    JSON.stringify(reads.map((f) => valueAt(cfg, derived, f))) + '\u0000' + extra;

  /**
   * `obj` with every field read recorded in `reads`, under `prefix`.
   * @template {Record<string, any>} T
   * @param {T} obj @param {Set<string>} reads @param {string} prefix @returns {T}
   */
  const tracking = (obj, reads, prefix) => new Proxy(obj, {
    get(t, k) {
      if (typeof k === 'string') reads.add(prefix + k);
      return t[/** @type {any} */ (k)];
    }
  });

  class ReadMemo {
    constructor() {
      /** The fields the last computation read, sorted. @type {string[] | null} */
      this.reads = null;
      /** @type {string | null} */
      this.key = null;
      /** @type {any} */
      this.value = null;
    }

    /**
     * The result for these settings, `derived` and `extra`: the last one
     * while what it read is unchanged, else `compute` run on recording
     * inputs.
     * @param {ChartSettings} cfg
     * @param {ChartDerived} derived
     * @param {string} extra The other inputs, as one string.
     * @param {(cfg: ChartSettings, derived: ChartDerived) => any} compute
     * @returns {boolean} Whether `compute` ran.
     */
    update(cfg, derived, extra, compute) {
      if (this.reads && this.key === keyOf(cfg, derived, this.reads, extra)) return false;
      /** @type {Set<string>} */
      const reads = new Set();
      this.value = compute(tracking(cfg, reads, ''), tracking(derived, reads, 'derived.'));
      this.reads = [...reads].sort();
      this.key = keyOf(cfg, derived, this.reads, extra);
      return true;
    }
  }

  NS.ReadMemo = ReadMemo;
})();
// --- chart/chart-types.js
// @ts-check
/**
 * Chart block: the chart types and the family that draws each. A family
 * shares a model and an option builder (family-*.js): aggregated types
 * summarise rows into marks, the timeline draws a lane per level, the
 * individual types draw a mark per row.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  const AGGREGATED_TYPES = ['bar', 'histogram', 'waterfall', 'pie', 'treemap', 'boxplot', 'pointrange'];
  // Box and point range summarize raw values per group in the browser.
  const DISTRIBUTION_TYPES = ['boxplot', 'pointrange'];
  // An area is a line with its area filled.
  /** @param {any} t */
  const isLineType = (t) => t === 'line' || t === 'area';

  /** @param {any} type @returns {'aggregated' | 'timeline' | 'individual'} */
  const familyOf = (type) => AGGREGATED_TYPES.includes(type) ? 'aggregated'
    : type === 'gantt' ? 'timeline' : 'individual';

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

  Object.assign(NS, { DISTRIBUTION_TYPES, familyOf, isLineType, facetScales, logAxes });
})();
// --- chart/colors.js
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
// --- chart/drill.js
// @ts-check
/**
 * Chart block: what a click may do (`drill`, `drill_target`) and what a
 * `highlight` lights. The selection itself is selection.js.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** The tri-state drill: 'off', 'auto' or 'column'. @param {Record<string, any>} cfg */
  const drillState = (cfg) => (!cfg.drill ? 'off' : cfg.drill === 'auto' ? 'auto' : 'column');

  /** The column a click filters on (the family's natural key for auto), or null for a
   * geometric selection. @param {Record<string, any>} cfg @returns {string | null} */
  const drillColumn = (cfg) => {
    const d = cfg.drill;
    if (!d) return null;
    if (d !== 'auto') return d;
    const fam = NS.familyOf(cfg.type);
    if (fam === 'aggregated') return cfg.category || null;
    if (fam === 'timeline') return cfg.y || null;
    return cfg.series || cfg.color || null;
  };

  /** A drill_target makes a click an event sent to another block. @param {Record<string, any>} cfg */
  const transientDrill = (cfg) => !!(cfg.drill_target && String(cfg.drill_target).trim());

  /** Hover shows the tooltip and nothing else: no emphasis, no blur of the
   * other marks, on every chart. The hand cursor shows where a click
   * selects; with drill off the chart is a display. Mutates the option.
   * @param {any} option @param {Record<string, any>} cfg */
  const quietHover = (option, cfg) => {
    if (!option || !Array.isArray(option.series)) return option;
    const cursor = drillState(cfg) === 'off' ? 'default' : 'pointer';
    for (const s of option.series) {
      if (!s) continue;
      // ECharts still scales a line's symbols on an axis hover unless `scale`
      // is off. A custom series' shapes say so themselves (emphasisDisabled).
      s.emphasis = { disabled: true, scale: false };
      s.cursor = cursor;
      if (s.itemStyle) s.itemStyle.cursor = cursor;
      if (s.lineStyle) s.lineStyle.cursor = cursor;
    }
    return option;
  };

  // A selection outlines the marks it takes and fades the others to this
  // share of their opacity; hover draws nothing (quietHover()).
  const SEL_DIM = 0.25;
  const SEL_OUTLINE = 1.5;

  /** A mark's style under the selection. `filtered`: something is selected;
   * `on`: this mark is. Without a selection the mark has its series' own
   * border (`base`, the series' itemStyle).
   * @param {boolean} on @param {boolean} filtered @param {Ink} ink @param {any} [base] */
  const selStyle = (on, filtered, ink, base) => (filtered && on
    ? { opacity: 1, borderColor: ink.text, borderWidth: SEL_OUTLINE }
    : { opacity: filtered ? SEL_DIM : 1,
        borderColor: (base && base.borderColor) || 'transparent',
        borderWidth: (base && base.borderWidth) || 0 });

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
    const vals = NS.labelled(h[col]).map((/** @type {{ value: any }} */ x) => x.value).filter(NS.hasVal).map(String);
    return vals.length ? { [col]: vals } : null;
  };

  Object.assign(NS, { drillState, drillColumn, transientDrill, quietHover, highlightOf,
                      selStyle, SEL_DIM });
})();
// --- chart/tooltip.js
// @ts-check
/**
 * Chart block: the data tooltip on the ECharts card. The rows are core's
 * (core/tooltip.js); here the card every tooltip sits on, and a headline
 * over label/value pairs.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** A headline over label/value rows; empty values are dropped.
   * @param {any} headline @param {Array<[string, any]>} pairs @param {any} [color] */
  const rowTooltip = (headline, pairs, color) =>
    V.tooltip.head(V.util.esc(headline), color) + pairs
      .filter((p) => p[1] != null && p[1] !== '')
      .map((p) => V.tooltip.row(V.util.esc(p[0]), V.util.esc(p[1])))
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

  Object.assign(NS, { rowTooltip, cardTooltip });
})();
// --- chart/legend.js
// @ts-check
/**
 * Chart block: the legend. legendPlan() says where the level names go: in
 * the band of chips below or above the plot, or on the marks themselves (at
 * a line's end, beside a stack, at a group's centre). The band is Legend,
 * owned by the view: one row of chips for every panel. A chip selects its
 * level, as a click on a mark of that level would; the swatch follows the
 * chart's mark.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const mk = NS.mk;

  // Past this many levels the names on the marks of a line crowd each other.
  const MARKS_MAX_LEVELS = 8;
  // Past this many levels a legend cannot be read: no band, no names on the
  // marks. The tooltip names the level instead.
  const LEGEND_MAX = 15;

  /** Can a legend of n levels be read? @param {number} n */
  const legendFits = (n) => n <= LEGEND_MAX;

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
      if (!m.legend || m.byColor || !legendFits(m.legend.items.length)) return null;
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
          legendFits(m.colors.length) && cfg.position !== 'dodge') {
        const vertical = cfg.orientation === 'vertical';
        // A level the named bar lacks would have no name: the legend stays.
        return stackCovers(m, vertical) ? (vertical ? 'stack' : 'stack-h') : null;
      }
      if (m.type === 'pointrange' && m.split && legendFits(m.levels.length)) return 'centroid';
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

  class Legend {
    /** @param {ChartView} view @param {HTMLElement} parent */
    constructor(view, parent) {
      this.view = view;
      this.el = mk('div', 'vz-chart-legend', parent);
      this.el.hidden = true;
      /** The column the chips are levels of. @type {string | null} */
      this.col = null;
      /** Owns the chips' listeners. @type {VizScope | null} */
      this._scope = null;
    }

    /**
     * Draw the chips, or hide the band with no spec or more chips than can be read.
     * @param {{ col: string, items: Array<{ name: string, color: string }> } | null} spec
     * @param {{ position: 'top' | 'bottom', swatch: string }} [plan]
     */
    update(spec, plan) {
      const el = this.el;
      if (!spec || !spec.items.length || !legendFits(spec.items.length) || !plan) {
        el.hidden = true;
        el.textContent = '';
        this.col = null;
        return;
      }
      this.col = spec.col;
      this._place(plan.position === 'top');
      el.textContent = '';
      if (this._scope) this._scope.dispose();
      const scope = this._scope = this.view.scope.own(new V.Scope());
      // Which column the chips are levels of, before them: its label, else
      // its name. Not when a level already reads the same.
      const meta = NS.axes.colMeta(this.view.columns, spec.col);
      const title = String((meta && meta.label) || spec.col);
      if (!spec.items.some((it) => String(it.name) === title)) {
        mk('span', 'vz-chart-legend-title', el).textContent = title;
      }
      for (const it of spec.items) {
        const chip = /** @type {HTMLButtonElement} */ (mk('button', 'vz-chart-legend-chip', el));
        chip.type = 'button';
        chip.dataset.level = it.name;
        const sw = mk('span', 'vz-chart-legend-swatch vz-chart-legend-swatch--' + plan.swatch, chip);
        sw.style.background = it.color;
        mk('span', '', chip).textContent = it.name;
        scope.on(chip, 'click', () => this._click(spec.col, it.name));
      }
      el.hidden = false;
      this.mark(this.view.selection.current);
    }

    /** Above or below the panel grid. @param {boolean} top */
    _place(top) {
      const el = this.el;
      const grid = this.view.chartGrid;
      el.classList.toggle('vz-chart-legend--top', top);
      const inPlace = top ? el.nextElementSibling === grid : el.previousElementSibling === grid;
      if (!inPlace) grid.parentNode?.insertBefore(el, top ? grid : grid.nextSibling);
    }

    // A chip's level as a filter, or with an explicit drill column that
    // column's values in the level's rows. Nothing with drill off.
    /** @param {string} col @param {string} level */
    _click(col, level) {
      const v = this.view;
      const cfg = v.settings;
      if (NS.drillState(cfg) === 'off') return;
      const filters = cfg.drill === 'auto'
        ? NS.keys.fromKeys({ [col]: level })
        : NS.keys.fromRows(cfg.drill, NS.model.rowsUnder(v.data, v._ix, { [col]: level }));
      if (filters) v.selection.click(null, null, filters);
    }

    /** Ring the chips of the selected levels. @param {ChartSelection | null} sel */
    mark(sel) {
      if (!this.col) return;
      const f = sel && sel.filters;
      const vals = f && f[this.col];
      const on = vals ? new Set(vals.map((v) => (NS.keys.isMissing(v) ? '' : String(v)))) : null;
      for (const chip of this.el.querySelectorAll('.vz-chart-legend-chip')) {
        const c = /** @type {HTMLElement} */ (chip);
        c.classList.toggle('vz-chart-legend-chip--on', !!on && on.has(c.dataset.level || ''));
      }
    }
  }

  Object.assign(NS, { legendPlan, legendFits, cloudCentre, nameMark, Legend });
})();
// --- chart/format.js
// @ts-check
/**
 * Chart block: the number format of a chart's values (`format`, `unit`), as
 * on the tile block. "number" is V.util.num (core), "compact" 1.2K / 3.4M / 5.6B,
 * "percent" a fraction x100, a whole number 0 to 6 that many decimals.
 * `unit` is free text printed after the number. Format "number" with no unit
 * makes valueFormat() return null and callers keep their own formatting.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** @param {number} v */
  const compact = (v) => {
    const a = Math.abs(v);
    /** @param {number} d @param {string} s */
    const short = (d, s) => String(Math.round((v / d) * 10) / 10) + s;
    if (a >= 1e9) return short(1e9, 'B');
    if (a >= 1e6) return short(1e6, 'M');
    if (a >= 1e3) return short(1e3, 'K');
    return String(V.util.num(v));
  };

  /** @param {number} v @param {number} digits */
  const fixedNum = (v, digits) =>
    (v || 0).toLocaleString(undefined, { minimumFractionDigits: digits, maximumFractionDigits: digits });

  /**
   * The decimals a `format` fixes (2 or "2"), or null.
   * @param {any} format @returns {number | null}
   */
  const fixedDigits = (format) => {
    if (format == null || format === '' || typeof format === 'boolean') return null;
    const d = Number(format);
    return Number.isInteger(d) && d >= 0 && d <= 6 ? d : null;
  };

  /**
   * The formatter for a chart's values, or null to keep the chart's own.
   * `unit: false` leaves the unit off (axis ticks; the title carries it).
   * @param {Record<string, any>} cfg @param {{ unit?: boolean }} [opts]
   * @returns {((v: any) => string) | null}
   */
  const valueFormat = (cfg, opts = {}) => {
    const fixed = fixedDigits(cfg.format);
    const format = fixed != null ? 'number' : (cfg.format || 'number');
    const unit = cfg.unit ? String(cfg.unit) : '';
    const withUnit = opts.unit !== false && unit !== '';
    if (format === 'number' && !withUnit && fixed == null) return null;
    /** @type {(v: number) => string} */
    const base = format === 'percent'
      // `|| 0` folds a negative zero.
      ? (v) => (Math.round(v * 1000) / 10 || 0) + '%'
      : format === 'compact' ? compact
        : fixed != null ? (v) => fixedNum(v, fixed) : (v) => String(V.util.num(v));
    return (v) => {
      const n = Number(v);
      if (v == null || !isFinite(n)) return '';
      return base(n) + (withUnit ? ' ' + unit : '');
    };
  };

  /** An axis title with the unit, "Sum of amount (CHF)". @param {string} title @param {Record<string, any>} cfg */
  const unitTitle = (title, cfg) => {
    const unit = cfg.unit ? String(cfg.unit) : '';
    if (!unit) return title;
    return title ? title + ' (' + unit + ')' : unit;
  };

  /** An axis name. `title` is the column's label (or name, axes.axisTitle), `col` the column.
   * Labelled: label and unit, "Growth (%)". Else the unit alone, else the column name. Empty when
   * the chart's title carries it. @param {string} title @param {any} col
   * @param {ChartSettings} cfg @param {ChartDerived} derived @param {{ unit?: boolean }} [opts] */
  const axisName = (title, col, cfg, derived, opts = {}) => {
    const unit = opts.unit !== false && cfg.unit ? String(cfg.unit) : '';
    const labelled = !!title && title !== col;
    const name = labelled ? (unit ? unitTitle(title, cfg) : title) : (unit || title || '');
    const t = derived.titles && derived.titles.title;
    const head = String((t && t.text) || '').toLowerCase();
    if (name && head.includes(name.toLowerCase())) return '';
    return name;
  };

  NS.valueFormat = valueFormat;
  NS.fixedDigits = fixedDigits;
  NS.unitTitle = unitTitle;
  NS.axisName = axisName;
})();
// --- chart/agg.js
// @ts-check
/**
 * Chart block: the names of the aggregations and statistics. R aggregates
 * (R/chart-cells.R); the browser only words what it draws.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** One word per aggregation, for composed labels ("Mean AGE").
   * @type {Record<string, string>} */
  const AGG_WORDS = {
    count: 'Count', count_distinct: 'Distinct', mean: 'Mean',
    median: 'Median', sum: 'Sum', min: 'Min', max: 'Max'
  };

  // The words of a `summary` / `whiskers` statistic in a tooltip. An unknown
  // statistic reads as Tukey's whiskers.
  /** @type {Array<{value: string, center: string, range: string}>} */
  const STAT_META = [
    { value: 'tukey', center: '', range: '1.5×IQR' },
    { value: 'median_q1_q3', center: 'Median', range: 'Q1–Q3' },
    { value: 'mean_sd', center: 'Mean', range: '±1 SD' },
    { value: 'mean_2sd', center: 'Mean', range: '±2 SD' },
    { value: 'mean_se', center: 'Mean', range: '±1 SE' },
    { value: 'mean_ci95', center: 'Mean', range: '95% CI' },
    { value: 'p5_p95', center: 'Median', range: 'P5–P95' },
    { value: 'p10_p90', center: 'Median', range: 'P10–P90' },
    { value: 'min_max', center: 'Median', range: 'Min–Max' }
  ];
  /** @param {string} stat */
  const statMeta = (stat) => STAT_META.find((s) => s.value === stat) || STAT_META[0];

  NS.agg = { AGG_WORDS, statMeta };
})();
// --- chart/chart-index.js
// @ts-check
/**
 * Chart block: rows and the lookups over them.
 *
 * rowsFromPayload() turns the column object R sends into rows. RowIndex
 * buckets rows by a column once and answers every later lookup from a Map,
 * which is what keeps a chart with a level per patient linear. labelCounts()
 * is the "(n)" next to an axis or facet label.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** @typedef {'nz' | 'raw'} KeyMode */
  // Shared by every miss.
  const NO_ROWS = /** @type {any[]} */ (/** @type {unknown} */ (Object.freeze([])));

  // Lookups over row arrays, memoized on the rows array itself. Callers
  // share the results and must not mutate them. A key is the column value as
  // a string: 'nz' is String(v ?? '') (null joins the '' level), 'raw' is
  // String(v) (null is "null").
  class RowIndex {
    constructor() {
      /** @type {WeakMap<any[], Map<string, any>>} */
      this._memo = new WeakMap();
    }

    /** @param {any[]} rows @param {string} key @param {() => any} build */
    _cached(rows, key, build) {
      let m = this._memo.get(rows);
      if (!m) { m = new Map(); this._memo.set(rows, m); }
      if (!m.has(key)) m.set(key, build());
      return m.get(key);
    }

    /** @param {KeyMode} mode @returns {(v: any) => string} */
    static keyFn(mode) {
      return mode === 'nz' ? (v) => String(v ?? '') : (v) => String(v);
    }

    /** Rows by key of `col`, in row order; Map order is first-seen key order. @param {any[]} rows
     * @param {string} col @param {KeyMode} mode @returns {Map<string, any[]>} */
    buckets(rows, col, mode) {
      return this._cached(rows, 'b\u0000' + mode + '\u0000' + col, () => {
        const key = RowIndex.keyFn(mode);
        /** @type {Map<string, any[]>} */
        const out = new Map();
        for (const r of rows) {
          const k = key(r[col]);
          const b = out.get(k);
          if (b) b.push(r); else out.set(k, [r]);
        }
        return out;
      });
    }

    /** Rows by key of `colA`, then of `colB` (nested, so no two pairs collide). @param {any[]} rows
     * @param {string} colA @param {KeyMode} modeA @param {string} colB @param {KeyMode} modeB
     * @returns {Map<string, Map<string, any[]>>} */
    buckets2(rows, colA, modeA, colB, modeB) {
      return this._cached(rows,
        'b2\u0000' + modeA + '\u0000' + colA + '\u0000' + modeB + '\u0000' + colB, () => {
          const keyA = RowIndex.keyFn(modeA), keyB = RowIndex.keyFn(modeB);
          /** @type {Map<string, Map<string, any[]>>} */
          const out = new Map();
          for (const r of rows) {
            const a = keyA(r[colA]);
            let inner = out.get(a);
            if (!inner) { inner = new Map(); out.set(a, inner); }
            const b = keyB(r[colB]);
            const bucket = inner.get(b);
            if (bucket) bucket.push(r); else inner.set(b, [r]);
          }
          return out;
        });
    }

    /** The rows whose `col` keys to `k`. @param {any[]} rows @param {string} col
     * @param {KeyMode} mode @param {string} k @returns {any[]} */
    get(rows, col, mode, k) {
      return this.buckets(rows, col, mode).get(k) || NO_ROWS;
    }

    /** The rows keyed `a` on `colA` and `b` on `colB`. @param {any[]} rows @param {string} colA
     * @param {KeyMode} modeA @param {string} colB @param {KeyMode} modeB @param {string} a
     * @param {string} b @returns {any[]} */
    get2(rows, colA, modeA, colB, modeB, a, b) {
      const inner = this.buckets2(rows, colA, modeA, colB, modeB).get(a);
      return (inner && inner.get(b)) || NO_ROWS;
    }

    /** Distinct keys of `col`, first-seen order. @param {any[]} rows @param {string} col
     * @param {KeyMode} mode @returns {string[]} */
    levels(rows, col, mode) {
      return this._cached(rows, 'l\u0000' + mode + '\u0000' + col,
        () => Array.from(this.buckets(rows, col, mode).keys()));
    }
  }

  /** Rows from what R sends: a JSON string, a column object (a column may be dictionary-encoded as
   * {__enc__: 'dict', levels, codes}) or rows already. @param {any} data @returns {any[]} */
  const rowsFromPayload = (data) => {
    if (typeof data === 'string') data = JSON.parse(data);
    if (!data) return [];
    if (Array.isArray(data)) return data;
    const keys = Object.keys(data);
    for (const k of keys) {
      const col = data[k];
      if (col && !Array.isArray(col) && col.__enc__ === 'dict') {
        const levels = col.levels || [];
        const codes = col.codes || [];
        const out = new Array(codes.length);
        for (let i = 0; i < codes.length; i++) out[i] = levels[codes[i]];
        data[k] = out;
      }
    }
    const n = keys.length > 0 ? (Array.isArray(data[keys[0]]) ? data[keys[0]].length : 1) : 0;
    const rows = new Array(n);
    for (let i = 0; i < n; i++) {
      /** @type {Record<string, any>} */
      const row = {};
      for (const k of keys) row[k] = Array.isArray(data[k]) ? data[k][i] : data[k];
      rows[i] = row;
    }
    return rows;
  };

  /** The values a row holds in a column: one, or in a cell the distinct
   * values of its rows. @param {any} v @returns {any[]} */
  const valuesOf = (v) => (Array.isArray(v) ? v : [v]);

  /**
   * Count per value of `dimCol`: distinct `id` when set, else rows. With
   * func 'identity' it is the id column's first value per group as it is.
   * A cell counts the rows it stands for and brings its distinct ids.
   * @param {any[]} rows @param {Record<string, any>} cfg
   * @param {string | null | undefined} dimCol
   * @returns {Map<string, any>}
   */
  const labelCounts = (rows, cfg, dimCol) => {
    const idCol = cfg.id;
    const asIs = !!idCol && cfg.func === 'identity';
    /** @type {Map<string, any>} */
    const out = new Map();
    /** @type {Map<string, Set<any>>} */
    const ids = new Map();
    for (const r of rows) {
      const dv = dimCol ? String(r[dimCol] ?? '') : 'Total';
      if (asIs) {
        const id = valuesOf(r[idCol])[0];
        if (!out.has(dv) && id != null) out.set(dv, id);
      } else if (idCol) {
        for (const id of valuesOf(r[idCol])) {
          if (id == null) continue;
          let s = ids.get(dv);
          if (!s) { s = new Set(); ids.set(dv, s); }
          s.add(id);
          out.set(dv, s.size);
        }
      } else {
        out.set(dv, (out.get(dv) || 0) + (r['.n'] ?? 1));
      }
    }
    return out;
  };

  /**
   * " (n)" after a label, from a counts map; a non-integer rounds to 2 places.
   * @param {any} label @param {Map<string, any> | null | undefined} counts
   */
  const withCount = (label, counts) => {
    const s = String(label ?? '');
    if (!counts) return s;
    const v = counts.get(s);
    if (v == null) return s;
    const disp = (typeof v === 'number' && !Number.isInteger(v))
      ? Math.round(v * 100) / 100 : v;
    return s + ' (' + disp + ')';
  };

  /** Does show_n cover `surface` ('axis' | 'facet')?
   *  @param {Record<string, any>} cfg @param {string} surface */
  const countOn = (cfg, surface) => {
    const v = cfg.show_n;
    return v === surface || v === 'both';
  };

  Object.assign(NS, { RowIndex, NO_ROWS, rowsFromPayload, valuesOf, labelCounts, withCount, countOn });
})();
// --- chart/chart-keys.js
// @ts-check
/**
 * Chart block: a mark's keys and the filters made of them.
 *
 * A mark carries its keys, the column values that define it ({AETERM:
 * "Rash"}, plus the facet column inside a facet panel). A click sends them
 * as a filter {col: [values]}, the footer names the filter in column labels,
 * and a restored filter lights the marks that hold filtered rows. A missing
 * value ('' or null) is a key too, sent as null, which R matches with
 * is.na().
 *
 * A selection has the shape R reads (R/kit-input.R), in memory as on the
 * wire: `{type: 'categorical', filters}`, or for a point or a brush on
 * numeric axes `{type: 'range', x, y, x_range, y_range}`, with the panel's
 * key in `filters` when it was taken in a facet panel.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** @typedef {Record<string, any[]>} Filters */

  /** Is a key value missing? The aggregation keys it as ''. @param {any} v */
  const isMissing = (v) => v == null || v === '';

  /** A filter from a mark's keys: one value per column, null when missing.
   * @param {Record<string, any>} keys @returns {Filters} */
  const fromKeys = (keys) => {
    /** @type {Filters} */
    const out = {};
    for (const col of Object.keys(keys)) {
      const v = keys[col];
      out[col] = [isMissing(v) ? null : String(v)];
    }
    return out;
  };

  /**
   * A filter on `col` with the distinct non-missing values of `rows`, or null.
   * A cell holds the distinct values of its rows.
   * @param {string} col @param {any[]} rows @returns {Filters | null}
   */
  const fromRows = (col, rows) => {
    const vals = [...new Set(
      (rows || []).flatMap((r) => (r ? r[col] : null)).filter((v) => v != null)
    )].map(String);
    return vals.length ? { [col]: vals } : null;
  };

  /**
   * A filter as R sends it back: {col: [values]}; a scalar is one value.
   * Null when no column has a value.
   * @param {any} obj @returns {Filters | null}
   */
  const clean = (obj) => {
    if (!obj || typeof obj !== 'object' || Array.isArray(obj)) return null;
    /** @type {Filters} */
    const out = {};
    for (const col of Object.keys(obj)) {
      const v = obj[col];
      const vals = (Array.isArray(v) ? v : [v]).map((x) => (isMissing(x) ? null : String(x)));
      if (vals.length) out[col] = vals;
    }
    return Object.keys(out).length ? out : null;
  };

  /**
   * A range selection on columns `x` and `y`. `y` goes with `y_range`, and
   * the panel's key in `filters` only when there is one.
   * @param {string} x @param {string | null} y @param {any[]} xRange
   * @param {any[] | null} yRange @param {Filters | null} [filters]
   * @returns {ChartRangeSelection}
   */
  const range = (x, y, xRange, yRange, filters) => {
    /** @type {ChartRangeSelection} */
    const sel = { type: 'range', x, y: yRange ? y : null, x_range: xRange, y_range: yRange };
    if (filters && Object.keys(filters).length) sel.filters = filters;
    return sel;
  };

  /** A map's scalar as R sends it: every value of a map comes as an array
   * (wire_map()), `"range"` as `["range"]`. @param {any} v */
  const scalar = (v) => (Array.isArray(v) ? v[0] : v);

  /**
   * The selection a saved one restores, cleaned, or null for none or one
   * the chart cannot show.
   * @param {any} sel @returns {ChartSelection | null}
   */
  const restore = (sel) => {
    if (!sel || typeof sel !== 'object') return null;
    if (scalar(sel.type) === 'range') {
      const x = scalar(sel.x);
      if (!x || !Array.isArray(sel.x_range)) return null;
      const yr = Array.isArray(sel.y_range) && sel.y_range.length ? sel.y_range : null;
      return range(x, scalar(sel.y) || null, sel.x_range, yr, clean(sel.filters));
    }
    const filters = clean(sel.filters);
    return filters ? { type: 'categorical', filters } : null;
  };

  // Above this many values a column is named with its count, not its values.
  const MAX_LISTED = 3;

  /** The words for a filter, in column labels: "Reported Term = Rash", "Severity = (missing)",
   * "Patient, 214 values". @param {Filters} filters @param {VizColumn[]} columns @returns {string}
   */
  const describe = (filters, columns) => Object.keys(filters).map((col) => {
    const label = NS.axes.axisTitle(columns, col);
    const vals = filters[col];
    if (vals.length > MAX_LISTED) return label + ', ' + vals.length + ' values';
    return label + ' = ' + vals.map((v) => (isMissing(v) ? '(missing)' : v)).join(', ');
  }).join(', ');

  /** Decimals that show a number to three significant digits; none for a whole number.
   * @param {number} v */
  const decimalsOf = (v) => {
    if (!Number.isFinite(v) || Number.isInteger(v)) return 0;
    const s = String(Number(v.toPrecision(3)));
    if (/e/i.test(s)) return 0;
    const dot = s.indexOf('.');
    return dot < 0 ? 0 : s.length - dot - 1;
  };

  /** A range in words: "2 to 86", "21.4 to 30.0" (both ends at the same decimals), "= 15" for a
   * single value. Dates on a time axis. @param {any[]} r @param {string} [axisType] */
  const rangeWords = (r, axisType) => {
    const [lo, hi] = r;
    /** @param {any} v @param {number} d */
    const fmt = (v, d) => {
      if (axisType === 'time') return NS.axes.fmtXVal(v, 'time', null);
      if (typeof v !== 'number' || !Number.isFinite(v)) return String(v);
      return v.toLocaleString(undefined, { minimumFractionDigits: d, maximumFractionDigits: d });
    };
    const d = Math.max(decimalsOf(Number(lo)), decimalsOf(Number(hi)));
    if (lo === hi) return '= ' + fmt(lo, decimalsOf(Number(lo)));
    return fmt(lo, d) + ' to ' + fmt(hi, d);
  };

  /**
   * The words for a range filter: "Study Day 2 to 86, Value 21.4 to 30.0";
   * a point reads "Study Day = 15, Value = 29.2". The facet key of a range
   * taken in a panel follows: ", Arm = Placebo".
   * @param {ChartRangeSelection} sel
   * @param {VizColumn[]} columns @param {string} [xAxisType]
   */
  const describeRange = (sel, columns, xAxisType) => {
    const parts = [NS.axes.axisTitle(columns, sel.x) + ' ' + rangeWords(sel.x_range, xAxisType)];
    if (sel.y_range && sel.y) {
      parts.push(NS.axes.axisTitle(columns, sel.y) + ' ' + rangeWords(sel.y_range));
    }
    if (sel.filters && Object.keys(sel.filters).length) parts.push(describe(sel.filters, columns));
    return parts.join(', ');
  };

  /** A value as a filter compares it: missing folds to ''. @param {any} v */
  const keyOf = (v) => (isMissing(v) ? '' : String(v));

  // Each filtered column's values as a set, made once per filter.
  /** @type {WeakMap<Filters, Record<string, Set<string>>>} */
  const valueSets = new WeakMap();
  /** @param {Filters} filters @returns {Record<string, Set<string>>} */
  const setsOf = (filters) => {
    let s = valueSets.get(filters);
    if (!s) {
      s = {};
      for (const col of Object.keys(filters)) s[col] = new Set(filters[col].map(keyOf));
      valueSets.set(filters, s);
    }
    return s;
  };

  /** Does a row satisfy every column of the filter? A null in the filter takes the rows where the
   * column is missing. A cell that holds a column's distinct values satisfies it with any of them.
   * @param {any} row @param {Filters} filters */
  const rowMatches = (row, filters) => {
    const sets = setsOf(filters);
    for (const col of Object.keys(sets)) {
      const v = row[col];
      if (Array.isArray(v) && v.length) {
        if (!v.some((x) => sets[col].has(keyOf(x)))) return false;
      } else if (!sets[col].has(keyOf(Array.isArray(v) ? null : v))) return false;
    }
    return true;
  };

  /**
   * Is a value within [lo, hi]? Numbers compare as numbers with a little
   * slack; anything else as text (ISO dates sort that way).
   * @param {any} v @param {any[] | null} r
   */
  const within = (v, r) => {
    if (!r) return true;
    const [lo, hi] = r;
    if (typeof lo === 'number' && typeof hi === 'number') {
      const n = Number(v);
      const eps = 1e-12 * Math.max(1, Math.abs(lo), Math.abs(hi));
      return n >= lo - eps && n <= hi + eps;
    }
    const s = String(v);
    return s >= String(lo) && s <= String(hi);
  };

  /**
   * Is a drawn point ([x, y, ...]) inside a range selection?
   * @param {any[]} p @param {{ x_range: any[], y_range: any[] | null }} sel
   */
  const pointInRange = (p, sel) =>
    within(p[0], sel.x_range) && within(p[1], sel.y_range);

  /**
   * Is a mark lit under a filter? When every filtered column is one of the
   * mark's keys, the keys decide; otherwise the mark is lit when one of its
   * rows passes the filter. `rows` may be a function, called only if needed.
   * @param {Record<string, any>} keys @param {any[] | (() => any[])} rows
   * @param {Filters} filters
   */
  const markLit = (keys, rows, filters) => {
    const sets = setsOf(filters);
    const cols = Object.keys(sets);
    if (cols.every((c) => c in keys)) {
      return cols.every((c) => sets[c].has(keyOf(keys[c])));
    }
    const rs = typeof rows === 'function' ? rows() : rows;
    for (const r of rs || []) if (rowMatches(r, filters)) return true;
    return false;
  };

  NS.keys = { isMissing, fromKeys, fromRows, range, restore,
              describe, describeRange, rowMatches, pointInRange, markLit,
              MAX_LISTED };
})();
// --- chart/axes-time.js
// @ts-check
/**
 * Chart block: dates on a time axis. A date in ms, the period a column is
 * sampled at, an x value or span as a tooltip prints it. Starts NS.axes.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** A date in ms: a number as is, an ISO date as UTC midnight, an R date-time string ("2020-03-01
   * 12:30:00") as that wall-clock time in UTC. @param {any} v @returns {number} */
  const timeValue = (v) => {
    if (v == null || v === '') return NaN;
    if (typeof v === 'number') return v;
    const s = String(v);
    if (/^\d{4}-\d{2}-\d{2}$/.test(s)) return Date.parse(s + 'T00:00:00Z');
    const t = s.replace(' ', 'T');
    return Date.parse(/[zZ]|[+-]\d{2}:?\d{2}$/.test(t) ? t : t + 'Z');
  };

  /** The period a time column is sampled at, from the median gap between its distinct values:
   * 'year', 'quarter', 'month', 'day' or 'time'. @param {any[]} rows @param {string} col
   * @returns {string} */
  const timePeriod = (rows, col) => {
    const seen = new Set();
    for (const r of rows || []) {
      const t = timeValue(r[col]);
      if (Number.isFinite(t)) seen.add(t);
      if (seen.size > 2000) break;
    }
    const ts = [...seen].sort((a, b) => a - b);
    if (ts.length < 2) return 'day';
    const gaps = [];
    for (let i = 1; i < ts.length; i++) gaps.push(ts[i] - ts[i - 1]);
    gaps.sort((a, b) => a - b);
    const days = gaps[gaps.length >> 1] / 86400000;
    if (days >= 360) return 'year';
    if (days >= 85) return 'quarter';
    if (days >= 27) return 'month';
    return days >= 0.99 ? 'day' : 'time';
  };

  const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];

  /**
   * An x value for a tooltip: a category, a date named by its period, or a
   * trimmed number.
   * @param {any} v @param {string} xAxisType @param {any[] | null} xCats
   * @param {string | null} [period]
   */
  const fmtXVal = (v, xAxisType, xCats, period) => {
    if (v == null || v === '') return '';
    if (xAxisType === 'category') {
      return (xCats && xCats[v] != null) ? String(xCats[v]) : String(v);
    }
    if (xAxisType === 'time') {
      const d = new Date(timeValue(v));
      if (isNaN(d.getTime())) return String(v);
      const y = d.getUTCFullYear(), mo = d.getUTCMonth();
      if (period === 'year') return String(y);
      if (period === 'quarter') return 'Q' + (Math.floor(mo / 3) + 1) + ' ' + y;
      if (period === 'month') return MONTHS[mo] + ' ' + y;
      if (period === 'time') return d.toISOString().slice(0, 16).replace('T', ' ');
      return d.toISOString().slice(0, 10);
    }
    return V.util.num(Number(v));
  };

  /** The span between two x values: days on a time axis, units on a value axis, nothing on a
   * category axis. @param {any} a @param {any} b @param {string} xAxisType */
  const durationStr = (a, b, xAxisType) => {
    if (xAxisType === 'category') return '';
    const s = Number(a), e = Number(b);
    if (isNaN(s) || isNaN(e)) return '';
    if (xAxisType === 'time') {
      const days = Math.round((e - s) / 86400000);
      return days + (Math.abs(days) === 1 ? ' day' : ' days');
    }
    return String(Math.round((e - s) * 100) / 100);
  };

  NS.axes = Object.assign(NS.axes || {}, { timeValue, timePeriod, fmtXVal, durationStr });
})();
// --- chart/axes-labels.js
// @ts-check
/**
 * Chart block: axes and their labels, as plain functions. Axis types and
 * category order, titles, the label gutter and x label ladder (text measured
 * with a caller-passed function), log ticks, helper lines and marks.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  const { timeValue } = NS.axes;

  /** @param {VizColumn[] | null | undefined} columns @param {any} name */
  const colMeta = (columns, name) =>
    /** @type {any} */ ((columns || []).find((c) => c.name === name));

  /** The column's label when it has one, else its name. @param {VizColumn[]} columns
   * @param {any} col @returns {string} */
  const axisTitle = (columns, col) => {
    if (!col) return '';
    const c = colMeta(columns, col);
    if (c && c.label && c.label !== c.name) return c.label;
    return col;
  };

  /**
   * 'category' for a categorical column, 'time' for a *DT column holding
   * epoch ms (or, with `temporal`, a Date or date-time column), else 'value'.
   * @param {VizColumn[]} columns @param {any[]} rows @param {any} colName
   * @param {{ temporal?: boolean }} [opts]
   * @returns {'category' | 'time' | 'value'}
   */
  const axisTypeFor = (columns, rows, colName, opts = {}) => {
    if (!colName) return 'value';
    const meta = colMeta(columns, colName);
    if (opts.temporal && V.util.isTemporal(meta)) return 'time';
    if (meta && !V.util.isNumeric(meta)) return 'category';
    if (/DT$/i.test(colName)) {
      const sample = (rows || []).find((r) => r[colName] != null);
      if (sample && typeof sample[colName] === 'number' && sample[colName] > 1e11) {
        return 'time';
      }
    }
    return 'value';
  };

  /** The categories of a column: factor levels (others after), else by a companion number (AVISIT
   * by AVISITN), else in order of appearance. @param {VizColumn[]} columns @param {any[]} rows
   * @param {string} colName @returns {string[]} */
  const orderedCategories = (columns, rows, colName) => {
    const src = rows || [];
    const meta = colMeta(columns, colName);
    const present = new Set(src.map((r) => String(r[colName] ?? '')));
    if (meta && Array.isArray(meta.levels) && meta.levels.length) {
      const out = meta.levels.map(String).filter((/** @type {string} */ l) => present.has(l));
      const inOut = new Set(out);
      return out.concat([...present].filter((k) => !inOut.has(k)));
    }
    /** @type {Record<string, string>} */
    const companions = { AVISIT: 'AVISITN' };
    const companion = companions[colName];
    if (companion && src.length && src[0][companion] !== undefined) {
      /** @type {Record<string, number>} */
      const mins = {};
      for (const r of src) {
        const k = String(r[colName] ?? '');
        const v = Number(r[companion]);
        if (!isNaN(v) && (mins[k] == null || v < mins[k])) mins[k] = v;
      }
      return Object.keys(mins).sort((a, b) => mins[a] - mins[b]);
    }
    return [...present];
  };

  /** Order levels by the scale's order, else the factor levels, else alphabetically; unknown levels
   * last. @param {any[]} levels @param {any} scale @param {VizColumn[]} columns
   * @param {string} colName @returns {any[]} */
  const orderLevels = (levels, scale, columns, colName) => {
    /** @param {any[]} ref */
    const by = (ref) => {
      /** @type {Map<string, number>} */
      const idx = new Map(ref.map((l, i) => /** @type {[string, number]} */ ([String(l), i])));
      const at = (/** @type {any} */ x) => (idx.has(x) ? /** @type {number} */ (idx.get(x)) : 1e9);
      return levels.slice().sort((a, b) => (at(a) - at(b)) || a.localeCompare(b));
    };
    if (scale && Array.isArray(scale.order) && scale.order.length) return by(scale.order);
    const meta = colMeta(columns, colName);
    if (meta && Array.isArray(meta.levels) && meta.levels.length) return by(meta.levels);
    return levels.slice().sort();
  };

  /** The label gutter of a horizontal category axis, fitted to the widest label up to a cap.
   * @param {any[]} labels @param {(s: string) => number} measure */
  const yGutter = (labels, measure) => {
    const LABEL_CAP = 145;
    // Slack for the fallback font measured before the page font loads.
    const PAD = 10;
    let w = 0;
    for (const l of labels) {
      const tw = measure(String(l ?? ''));
      if (tw > w) w = tw;
      if (w >= LABEL_CAP) break;
    }
    w = Math.min(Math.ceil(w) + PAD, LABEL_CAP);
    return { width: w, margin: w + 5, gridLeft: w + 15 };
  };

  /**
   * One domain over the panels' extents, or null with fewer than two.
   * @param {Array<number[] | null>} extents @returns {number[] | null}
   */
  const sharedDomain = (extents) => {
    let lo = Infinity, hi = -Infinity, n = 0;
    for (const ext of extents) {
      if (!ext || !Number.isFinite(ext[0]) || !Number.isFinite(ext[1])) continue;
      if (ext[0] < lo) lo = ext[0];
      if (ext[1] > hi) hi = ext[1];
      n++;
    }
    return (n >= 2 && lo < hi) ? [lo, hi] : null;
  };

  /**
   * A shared value domain on a nice step, and the step: the finest 1-2-5 step
   * that spans it in at most seven intervals. Each panel's own extent is already
   * nice, but on its own step: [0, 50] by 10 and [0, 51] by 3 give [0, 51],
   * which ECharts labels 0, 10, ..., 50, 51.
   * @param {number} lo @param {number} hi @returns {number[]}
   */
  const niceShared = (lo, hi) => {
    const tidy = (/** @type {number} */ v) => Number(v.toPrecision(12));
    let mag = Math.pow(10, Math.floor(Math.log10((hi - lo) / 10)));
    for (;;) {
      for (const m of [1, 2, 5]) {
        const step = tidy(m * mag);
        const a = Math.floor(tidy(lo / step)), b = Math.ceil(tidy(hi / step));
        if (b - a <= 7) return [tidy(a * step), tidy(b * step), step];
      }
      mag *= 10;
    }
  };

  // ECharts labels a log axis at powers of ten only, which leaves two or
  // three labels. Up to three decades the ticks are 1-2-5 (1-1.5-2-3-5-7
  // within one decade); the axis ends on the ticks either side of the data.

  /**
   * The domain and ticks of a log axis over positive lo..hi, or null.
   * @param {number} lo @param {number} hi
   * @returns {{ min: number, max: number, ticks: number[] } | null}
   */
  const logTicks = (lo, hi) => {
    if (!(lo > 0) || !(hi > 0) || !Number.isFinite(lo) || !Number.isFinite(hi)) return null;
    if (hi < lo) [lo, hi] = [hi, lo];
    const dec = Math.log10(hi / lo);
    const steps = dec > 3 ? [1] : (dec > 1.2 ? [1, 2, 5] : [1, 1.5, 2, 3, 5, 7]);
    /** @type {number[]} */
    const all = [];
    for (let k = Math.floor(Math.log10(lo)) - 1; k <= Math.ceil(Math.log10(hi)) + 1; k++) {
      for (const st of steps) all.push(Number((st * Math.pow(10, k)).toPrecision(6)));
    }
    const below = all.filter((t) => t <= lo);
    const above = all.filter((t) => t >= hi);
    let min = below.length ? below[below.length - 1] : lo;
    let max = above.length ? above[0] : hi;
    // One value: a step either side, so the point is not on the frame.
    if (min === max) {
      const i = all.indexOf(min);
      min = all[Math.max(0, i - 1)];
      max = all[Math.min(all.length - 1, i + 1)];
    }
    return { min, max, ticks: all.filter((t) => t >= min && t <= max) };
  };

  /** Turn a value axis into a log axis over a logTicks() domain.
   * @param {any} axis @param {{ min: number, max: number, ticks: number[] }} dom
   */
  const logAxis = (axis, dom) => {
    axis.type = 'log';
    axis.logBase = 10;
    delete axis.scale;
    axis.min = dom.min;
    axis.max = dom.max;
    axis.axisLabel = { ...(axis.axisLabel || {}), customValues: dom.ticks };
    axis.axisTick = { ...(axis.axisTick || {}), customValues: dom.ticks };
    return axis;
  };

  // A long x label goes down a ladder: one line; wrapped onto up to three flat
  // lines; turned 90 degrees onto two; and, on an ordered axis (`decimate`),
  // thinned. A word too wide for the budget breaks mid-word.
  const LABEL_LINE_H = 15;   // 11px text, 1.35 leading
  const FLAT_MAX_LINES = 3;
  const TURN_MAX_LINES = 2;
  const TURN_CAP = 160;      // px of height a turned label may spend
  // Always reserved: a turned label grows the panel and can bring one up.
  const SCROLLBAR = 16;

  /** Greedy wrap on spaces; `hard` says a word had to be broken mid-word.
   * @param {(s: string) => number} measure @param {any} text @param {number} width
   * @returns {{ lines: string[], hard: boolean }} */
  const wrapLabel = (measure, text, width) => {
    const words = String(text ?? '').split(/\s+/).filter(Boolean);
    const lines = [];
    let hard = false;
    let cur = '';
    const pushWord = (/** @type {string} */ word) => {
      let rest = word;
      while (measure(rest) > width && rest.length > 1) {
        let k = rest.length;
        while (k > 1 && measure(rest.slice(0, k)) > width) k--;
        lines.push(rest.slice(0, k));
        rest = rest.slice(k);
        hard = true;
      }
      cur = rest;
    };
    for (const word of words) {
      const cand = cur ? cur + ' ' + word : word;
      if (measure(cand) <= width) { cur = cand; continue; }
      if (cur) lines.push(cur);
      cur = '';
      pushWord(word);
    }
    if (cur) lines.push(cur);
    return { lines: lines.length ? lines : [''], hard };
  };

  /** The layout's identity: a resize that keeps it needs no new layout.
   * @param {any} axisLabel */
  const xLabelKey = (axisLabel) =>
    `${axisLabel.rotate}|${axisLabel.interval}|${axisLabel.__wrapAt}|${axisLabel.__lines}`;

  /**
   * An axisLabel whose formatter first maps the category to what it shows
   * (a "(n)" count), then wraps it.
   * @param {any} axisLabel @param {((v: any) => any) | null | undefined} display
   */
  const axisLabelWithDisplay = (axisLabel, display) => {
    const base = axisLabel || {};
    const wrap = base.formatter;
    if (!display && !wrap) return base;
    return {
      ...base,
      formatter: (/** @type {any} */ v) => {
        const shown = display ? display(v) : v;
        return wrap ? wrap(shown) : shown;
      }
    };
  };

  /** A titled axis (the title centred on its edge) with ink labels and line. `grid` ('dashed' or
   * 'solid') adds split lines. @param {any} ink @param {string} type @param {string} name
   * @param {number} gap @param {{ format?: any, grid?: string }} [o] */
  const valueAxis = (ink, type, name, gap, o = {}) => ({
    type, name, nameLocation: 'middle', nameGap: gap,
    nameTextStyle: { color: ink.muted, fontSize: ink.fontSize },
    axisLabel: { color: ink.muted, fontSize: ink.fontSize,
                 ...(o.format ? { formatter: o.format } : {}) },
    axisLine: { lineStyle: { color: ink.strong } },
    ...(o.grid ? { splitLine: { lineStyle: { color: ink.border, type: o.grid } } } : {})
  });

  /** A category axis. With `xlab` (the fitted x labels) it runs along x, else down the left:
   * inverse, its labels cut at the gutter `gut`. `display` maps a category to its label; `ticks`
   * keeps the left axis's ticks. @param {any[]} data @param {any} ink
   * @param {{ xlab?: any, display?: any, gut?: any, ticks?: boolean }} o */
  const categoryAxis = (data, ink, o) => o.xlab
    ? { type: 'category', data, axisLabel: axisLabelWithDisplay(o.xlab.axisLabel, o.display),
        axisLine: { lineStyle: { color: ink.strong } }, axisTick: { show: false } }
    : { type: 'category', data, inverse: true,
        axisLabel: { color: ink.muted, fontSize: ink.fontSize, align: 'left',
                     margin: o.gut.margin, width: o.gut.width, overflow: 'truncate',
                     ellipsis: '…', ...(o.display ? { formatter: o.display } : {}) },
        axisLine: { show: false }, ...(o.ticks ? {} : { axisTick: { show: false } }) };

  /** Category labels for an x axis: flat when they fit their slot, else wrapped flat, else turned.
   * `bottom` is the extra grid gutter they cost. @param {any[]} labels @param {number} availW plot
   * width minus the grid's margins @param {boolean} decimate thin turned labels (ordered axes only)
   * @param {(s: string) => number} measure @param {{ muted: string, fontSize: number }} ink
   * @returns {{ axisLabel: any, bottom: number, key: string }} */
  const xAxisLabels = (labels, availW, decimate, measure, ink) => {
    const m = (/** @type {any} */ s) => measure(String(s ?? ''));
    let widest = 0;
    for (const l of labels) {
      const tw = m(l);
      if (tw > widest) widest = tw;
    }
    const PAD = 8;
    const n = Math.max(1, labels.length);
    // A hidden panel measures 0: assume a typical width.
    const slot = ((availW > 0 ? availW : 600) - SCROLLBAR) / n;
    const base = { color: ink.muted, fontSize: ink.fontSize, interval: 0, lineHeight: LABEL_LINE_H };
    const fitted = (/** @type {any} */ axisLabel, /** @type {number} */ bottom) =>
      ({ axisLabel, bottom, key: xLabelKey(axisLabel) });

    if (widest + PAD <= slot) return fitted({ ...base, rotate: 0, __wrapAt: 0, __lines: 1 }, 0);

    // Flat, unless a word had to break or it takes more than three lines.
    const flatW = Math.max(10, slot - PAD);
    const flat = labels.map((l) => wrapLabel(m, l, flatW));
    const flatLines = NS.maxOf(flat.map((f) => f.lines.length));
    if (flatLines <= FLAT_MAX_LINES && !flat.some((f) => f.hard)) {
      const axisLabel = {
        ...base, rotate: 0, __wrapAt: Math.round(flatW), __lines: flatLines,
        formatter: (/** @type {string} */ v) => wrapLabel(m, v, flatW).lines.join('\n')
      };
      return fitted(axisLabel, (flatLines - 1) * LABEL_LINE_H);
    }

    // Turned: past two lines the rest joins the last one.
    const turnedLines = (/** @type {string} */ v) => {
      const w = wrapLabel(m, v, TURN_CAP).lines;
      if (w.length <= TURN_MAX_LINES) return w;
      return w.slice(0, TURN_MAX_LINES - 1).concat(w.slice(TURN_MAX_LINES - 1).join(' '));
    };
    const turned = labels.map(turnedLines);
    const turnLines = NS.maxOf(turned.map((t) => t.length));
    const len = Math.ceil(NS.maxOf(turned.map((t) => NS.maxOf(t.map(m))))) + PAD;
    // Turned text needs a line height per line; `interval: k` skips k labels.
    const room = Math.max(13, turnLines * LABEL_LINE_H);
    const interval = decimate ? Math.max(0, Math.ceil(room / slot) - 1) : 0;
    const axisLabel = {
      ...base, rotate: 90, interval, __wrapAt: TURN_CAP, __lines: turnLines,
      formatter: (/** @type {string} */ v) => turnedLines(v).join('\n')
    };
    return fitted(axisLabel, len);
  };

  // Past this height per label ECharts drops labels silently.
  const CAT_LABEL_H = 14;

  /**
   * The footer note for a category axis squeezed past one label per row, or
   * null. `groupCol` is the column's title.
   * @param {number} plotH @param {number} nCats @param {any} groupCol
   * @returns {string | null}
   */
  const thinnedLabelNote = (plotH, nCats, groupCol) => {
    if (!nCats || plotH / nCats >= CAT_LABEL_H) return null;
    const shown = Math.max(1, Math.floor(plotH / CAT_LABEL_H));
    const what = groupCol || 'categories';
    return `Axis labels thinned: ${shown} of ${nCats} ${what} labelled` +
      ' — filter to fewer to see them all.';
  };

  /**
   * One helper line as a markLine datum: a quiet dash behind the data, its
   * value at the end. `text` replaces the value (null: no label); `flipped`:
   * the y axis runs top-down, so a vertical line is labelled at its start.
   * @param {number | string} v The value, or a category on a category axis.
   * @param {boolean} vertical @param {number} width
   * @param {string | null | undefined} text @param {boolean} flipped
   * @param {{ text: string, muted: string, fontSize?: number }} ink
   */
  const guideLine = (v, vertical, width, text, flipped, ink) => ({
    [vertical ? 'xAxis' : 'yAxis']: v,
    lineStyle: { color: ink.text, opacity: 0.55, type: [8, 4], width },
    label: {
      show: text !== null,
      position: vertical ? (flipped ? 'start' : 'end') : 'insideEndTop',
      formatter: text != null ? text : String(V.util.num(v)),
      color: ink.muted, fontSize: ink.fontSize || 12, opacity: 1
    }
  });

  /** `value_lines` on the value axis: a markLine on the first series, and axis bounds that take in the
   * lines and the data. Mutates `series[0]` and `valAxis`. @param {any[]} series
   * @param {any} valAxis @param {boolean} onX @param {boolean} fraction the axis runs 0..1 while
   * the reader types percent @param {any} lines `[{value, label}]`
   * @param {{ text: string, muted: string }} ink */
  const addValueLines = (series, valAxis, onX, fraction, lines, ink) => {
    const keep = /** @type {{ value: any, label: string | null }[]} */ (NS.labelled(lines)).filter((x) => Number.isFinite(Number(x.value)));
    const nm = keep.map((x) => x.label);
    const typed = keep.map((x) => Number(x.value));
    const vals = typed.map((/** @type {number} */ v) => (fraction ? v / 100 : v));
    if (!vals.length || !series.length) return;
    series[0].markLine = {
      silent: true, symbol: 'none',
      // onX: a horizontal chart, whose category axis is inverse.
      data: vals.map((/** @type {number} */ v, /** @type {number} */ i) => guideLine(v, onX, 1,
        nm[i] ? String(nm[i])
          : typed[i] === 0 ? null
            : (fraction ? V.util.num(typed[i]) + '%' : undefined), onX, ink))
    };
    // A bar axis keeps 0 in range; a `scale` axis fits the data. A line on
    // the rounded edge would read as the frame, so the edge moves a step out.
    const zero = !valAxis.scale;
    const lo = NS.minOf(vals), hi = NS.maxOf(vals);
    /** @param {{min: number, max: number}} e */
    const ext = (e) => ({
      lo: Math.min(lo, e.min, zero ? 0 : Infinity),
      hi: Math.max(hi, e.max, zero ? 0 : -Infinity)
    });
    /** @param {{min: number, max: number}} e */
    const step = (e) => {
      const d = ext(e), span = d.hi - d.lo;
      if (!(span > 0)) return 0;
      const mag = Math.pow(10, Math.floor(Math.log10(span)));
      const rel = span / mag;
      return rel >= 5 ? mag : (rel >= 2 ? mag / 2 : mag / 5);
    };
    /** @param {number} at @param {number} st */
    const onLine = (/** @type {number} */ at, /** @type {number} */ st) => vals.some((v) => Math.abs(v - at) < st * 1e-6);
    /** The axis edge on `dir` (1 top, -1 bottom): a rounded step past the lines and data. */
    const edge = (/** @type {number} */ dir) => (/** @type {any} */ e) => {
      const st = step(e);
      if (!st) return undefined;
      const at = dir > 0 ? Math.ceil(ext(e).hi / st) * st : Math.floor(ext(e).lo / st) * st;
      // A line at 0 on a bar axis is the baseline itself.
      if (dir < 0 && zero && at === 0) return undefined;
      return onLine(at, st) ? at + dir * st : at;
    };
    if (valAxis.max == null) valAxis.max = edge(1);
    if (valAxis.min == null) valAxis.min = edge(-1);
  };

  const ISO_DAY = /^\d{4}-\d{2}-\d{2}/;

  /**
   * The category a typed x position marks: the one it names, or, when the
   * categories are ordered dates, the one whose period holds the date.
   * `edge`: a date past either end takes that end. null when it marks none.
   * @param {any} v @param {any[]} cats @param {boolean} [edge]
   * @returns {string | null}
   */
  const catOf = (v, cats, edge = false) => {
    if (v == null || v === '' || !Array.isArray(cats) || !cats.length) return null;
    const s = String(v);
    for (const c of cats) if (String(c) === s) return String(c);
    if (!ISO_DAY.test(s) || cats.length < 2) return null;
    let ts = cats.map((c) => (ISO_DAY.test(String(c)) ? timeValue(c) : NaN));
    if (ts[0] > ts[ts.length - 1]) {
      cats = cats.slice().reverse();
      ts = ts.slice().reverse();
    }
    for (let i = 0; i < ts.length; i++) {
      if (!Number.isFinite(ts[i]) || (i > 0 && !(ts[i] > ts[i - 1]))) return null;
    }
    const t = timeValue(s);
    const last = ts.length - 1;
    // The last category's period is as long as the one before it.
    const end = ts[last] + (ts[last] - ts[last - 1]);
    if (!Number.isFinite(t)) return null;
    if (t < ts[0]) return edge ? String(cats[0]) : null;
    if (t >= end) return edge ? String(cats[last]) : null;
    let i = last;
    while (ts[i] > t) i--;
    return String(cats[i]);
  };

  /** `x_lines` and `x_ranges` on a bar's category axis: a line through each marked bar, a shade
   * over the bars a range spans. Mutates `series[0]`, adding to the markLine the value lines put
   * there. @param {any[]} series @param {boolean} onX the categories run along x
   * @param {any[]} cats @param {any} lines `x_lines` @param {any} ranges `x_ranges`
   * @param {{ text: string, muted: string, fontSize?: number }} ink */
  const addCategoryMarks = (series, onX, cats, lines, ranges, ink) => {
    if (!series.length) return;
    const key = onX ? 'xAxis' : 'yAxis';
    /** @type {any[]} */
    const data = [];
    /** @type {{ value: any, label: string | null }[]} */ (NS.labelled(lines)).forEach(({ value: v, label }) => {
      const c = catOf(v, cats);
      if (c == null) return;
      // Unnamed, a line on its own category needs no label.
      const text = label ? String(label) : (c === String(v) ? null : String(v));
      data.push(guideLine(c, onX, 1, text, false, ink));
    });
    if (data.length) {
      const ml = series[0].markLine;
      if (ml && Array.isArray(ml.data)) ml.data = ml.data.concat(data);
      else series[0].markLine = { silent: true, symbol: 'none', data };
    }
    /** @type {any[]} */
    const areas = [];
    for (const r of NS.rangesOf(ranges)) {
      const a = catOf(r.from, cats, true), b = catOf(r.to, cats, true);
      if (a == null || b == null) continue;
      // Both ends past the same edge: the range misses the axis.
      const ia = cats.map(String).indexOf(a), ib = cats.map(String).indexOf(b);
      if (ia === ib && catOf(r.from, cats) == null && catOf(r.to, cats) == null) continue;
      areas.push([{ name: r.name || '', [key]: ia <= ib ? a : b }, { [key]: ia <= ib ? b : a }]);
    }
    if (areas.length) {
      series[0].markArea = {
        silent: true,
        itemStyle: { color: ink.muted || '#6b7280', opacity: 0.08 },
        label: { position: onX ? 'insideTop' : 'insideRight', color: ink.muted || '#6b7280',
                 fontSize: ink.fontSize || 12 },
        data: areas
      };
    }
  };

  Object.assign(NS.axes, {
    colMeta, axisTitle, axisTypeFor, orderedCategories, orderLevels, yGutter,
    sharedDomain, niceShared, logTicks, logAxis, wrapLabel, axisLabelWithDisplay, valueAxis, categoryAxis,
    xAxisLabels, thinnedLabelNote, guideLine, addValueLines, catOf, addCategoryMarks,
    LABEL_LINE_H
  });
})();
// --- chart/model-common.js
// @ts-check
/**
 * Chart block: the model rules every family shares.
 *
 * The settings the builders read, from the payload's config, and the gate
 * in front of every draw: the empty states for no rows, an unset required
 * role, too many colour levels and a mapped column the rows do not carry.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const model = /** @type {any} */ (NS.model = {});

  const FIXED = ['waterfall', 'pie', 'treemap'];

  // The grid key column and the separator between its row and column level,
  // as R joins them (chart_grid_data() in R/chart-derived.R).
  const GRID_COL = '.facet_grid';
  const GRID_SEP = '\u001f';

  /** A row's grid key: its row level, then its column level. A missing level
   * is '' when `keepMissing`, as a bar keeps a missing facet as its own panel;
   * else the row has no key and no panel, as a line drops it.
   * @param {Record<string, any>} row @param {{ row: string, col: string | null }} g
   * @param {boolean} [keepMissing] @returns {string | null} */
  const gridKey = (row, g, keepMissing = true) => {
    const miss = (/** @type {string | null} */ c) => c != null && (row[c] == null || row[c] === '');
    if (!keepMissing && (miss(g.row) || miss(g.col))) return null;
    const lv = (/** @type {string | null} */ c) => (c == null || row[c] == null ? '' : String(row[c]));
    return lv(g.row) + GRID_SEP + lv(g.col);
  };

  /** Does the grid keep a row with a missing level? A bar, unless `missing`
   * drops it; a line, scatter or timeline never draws such a row in a wrap
   * either. @param {Record<string, any>} cfg */
  const gridKeepsMissing = (cfg) => NS.familyOf(cfg.type) === 'aggregated' && cfg.missing !== 'drop';

  /** The facet levels of an individual or timeline chart, sorted. In a grid a
   * row without a key ('') has no panel.
   * @param {any[]} rows @param {any} ix @param {Record<string, any>} cfg @returns {string[]} */
  const facetLevels = (rows, ix, cfg) => {
    if (!cfg.facet) return ['__all__'];
    const lv = ix.levels(rows, cfg.facet, 'nz').slice().sort();
    return cfg.facet_grid ? lv.filter((/** @type {string} */ f) => f !== '') : lv;
  };

  /** A grid key split into its row and column level. @param {string} key */
  const gridLevels = (key) => {
    const i = String(key).indexOf(GRID_SEP);
    return i < 0 ? { row: String(key), col: '' } : { row: key.slice(0, i), col: key.slice(i + 1) };
  };

  /**
   * `keys` with the facet panel's key: the facet column and its level, or in
   * a grid the row and column columns and their levels. Mutates `keys`.
   * @param {Record<string, any>} keys @param {string | null} facetCol
   * @param {{ row: string, col: string | null } | null | undefined} grid @param {any} facetVal
   */
  const facetKeys = (keys, facetCol, grid, facetVal) => {
    if (!facetCol || facetVal == null || facetVal === '__all__') return keys;
    if (!grid || facetCol !== GRID_COL) {
      keys[facetCol] = facetVal;
      return keys;
    }
    const lv = gridLevels(facetVal);
    keys[grid.row] = lv.row;
    if (grid.col) keys[grid.col] = lv.col;
    return keys;
  };
  /** @param {unknown} v */
  const set = (v) => v != null && v !== '' && !(Array.isArray(v) && !v.length);

  /** The way a bar (horizontal), boxplot or point range (vertical) lies when `orientation` is
   * not given. @param {string} type @returns {'horizontal' | 'vertical'} */
  const defaultLie = (type) => (type === 'bar' ? 'horizontal' : 'vertical');

  /** An aggregated chart's axes: the category column, the column it aggregates (`measure`, ".count"
   * for a row count), the statistic and the way it lies. `orientation` maps nothing: the category
   * is `category` either way. A histogram's category is the bin column R adds beside x.
   * @param {Record<string, any>} c */
  const axes = (c) => {
    if (c.type === 'histogram') {
      return { category: set(c.x) ? c.x + '_bin' : null, measure: '.count', func: 'count',
               orientation: 'vertical' };
    }
    const fixed = FIXED.includes(c.type);
    const func = NS.DISTRIBUTION_TYPES.includes(c.type) ? 'mean' : (c.func || 'count');
    return { category: set(c.category) ? c.category : null,
             measure: func === 'count' ? '.count' : (set(c.value) ? c.value : ''), func,
             orientation: fixed ? null : (c.orientation || defaultLie(c.type)) };
  };

  /** The defaults that depend on the chart: the statistics of a box and a point range, and the
   * sort. A date, number or factor category keeps its own order, any other sorts by value; an unset
   * direction is descending by value and ascending otherwise. A mapping is never picked here: an
   * unset one asks for a pick (gate()). Mutates `cfg`. @param {Record<string, any>} cfg
   * @param {VizColumn[]} columns */
  const defaults = (cfg, columns) => {
    const fam = NS.familyOf(cfg.type);
    if (NS.DISTRIBUTION_TYPES.includes(cfg.type)) {
      const box = cfg.type === 'boxplot';
      if (!cfg.summary) cfg.summary = box ? 'median_q1_q3' : 'mean_se';
      if (box && !cfg.whiskers) cfg.whiskers = 'tukey';
      if (!set(cfg.sort_by)) cfg.sort_by = 'data';
    } else if (fam === 'timeline' && !set(cfg.sort_by)) {
      cfg.sort_by = 'onset';
    }
    if (fam === 'individual') return;
    if (!set(cfg.sort_by)) {
      const meta = columns.find((c) => c.name === cfg.category);
      const ordered = !!meta && (V.util.isNumeric(meta) || V.util.isTemporal(meta) ||
        (Array.isArray(meta.levels) && meta.levels.length > 0));
      cfg.sort_by = ordered ? 'data' : 'value';
    }
    if (!set(cfg.sort_dir)) cfg.sort_dir = cfg.sort_by === 'value' ? 'desc' : 'asc';
  };

  /** The settings the model and option builders read: each spec argument, its default where the
   * payload's config leaves it out, and an aggregated chart's axes (axes()). R's `derived` goes
   * beside them.
   * @param {Record<string, any>} config @param {VizColumn[]} [columns] @returns {ChartSettings} */
  const settings = (config, columns = []) => {
    const spec = /** @type {VizSpec} */ (V.spec.chart || {});
    /** @type {Record<string, any>} */
    const cfg = {};
    for (const k of Object.keys(spec)) cfg[k] = spec[k].default ?? null;
    for (const [k, v] of Object.entries(config || {})) if (v != null) cfg[k] = v;
    // A colour, facet or series column the data does not hold (a board adds
    // its Subgroup column only once a subgroup is picked) is unset, as in 0.2:
    // every row would land in one empty facet or level. So is one that is NA
    // in every row (`all_na`), which lets upstream switch the split off by
    // filling the column with NA.
    for (const k of ['color', 'facet', 'facet_row', 'series']) {
      if (!cfg[k] || !columns.length) continue;
      const col = columns.find((c) => c.name === cfg[k]);
      if (!col || col.all_na) cfg[k] = null;
    }
    // A grid: the families facet by one column holding both levels (gridKey()),
    // which the view adds to the rows. `facet_grid` keeps the two columns.
    cfg.facet_grid = cfg.facet_row ? { row: cfg.facet_row, col: cfg.facet || null } : null;
    if (cfg.facet_grid) cfg.facet = GRID_COL;
    if (NS.familyOf(cfg.type) === 'aggregated') Object.assign(cfg, axes(cfg));
    defaults(cfg, columns);
    return /** @type {ChartSettings} */ (cfg);
  };

  /** The gear's label of a spec argument. @param {string} key */
  const specLabel = (key) => {
    const arg = /** @type {VizSpec} */ (V.spec.chart || {})[key];
    return arg && arg.gear ? arg.gear.label : key;
  };

  /** Why there is no chart, in plain text. @param {string} message @returns {VizEmpty} */
  const emptyState = (message) => ({ kind: 'empty', message });

  /**
   * The empty state a config draws instead of a chart, or null when it can
   * draw.
   * @param {any[]} rows @param {VizColumn[]} columns @param {ChartSettings} cfg
   * @param {ChartDerived} derived @returns {VizEmpty | null}
   */
  const gate = (rows, columns, cfg, derived) => {
    const fam = NS.familyOf(cfg.type);
    // The settings a family cannot draw without, as [setting, spec key].
    /** @type {[string, string][]} */
    const required = cfg.type === 'histogram' ? [['x', 'x']]
      : fam === 'aggregated' ? [['category', 'category']]
      : [['x', 'x'], ['y', 'y']];
    const unset = required.filter(([k]) => !NS.hasVal(cfg[k]));
    if (unset.length) {
      return emptyState('Pick ' + unset.map(([, key]) => specLabel(key)).join(' and ') +
                        ' to plot.');
    }

    // A histogram's category is the bin column R adds beside x.
    const req = cfg.type === 'histogram' ? [[specLabel('x'), cfg.x]]
      : fam === 'aggregated'
        ? [[specLabel('category'), cfg.category],
           [specLabel('value'), cfg.measure !== '.count' ? cfg.measure : null]]
        : [['X', cfg.x], ['Y', cfg.y]];
    const avail = columns.map((c) => c.name);
    const missing = req
      .filter(([, v]) => v && !avail.includes(v))
      .map(([lbl, v]) => `${lbl} = "${v}"`);
    if (missing.length) {
      const availTxt = avail.length
        ? ' Columns available here: ' + avail.slice(0, 30).join(', ') +
          (avail.length > 30 ? ', …' : '') + '.'
        : '';
      return emptyState('Mapped column not in data: ' + missing.join(', ') + '.' + availTxt +
        ' A rename, flatten or pivot upstream may have changed the column ' +
        'name — re-pick it in the gear.');
    }

    if (rows.length === 0) return emptyState('No data to chart');

    // A band whose series R could not compute says why.
    const band = derived.band;
    if (cfg.type === 'band' && band && !band.series && band.note) {
      return emptyState(String(band.note));
    }

    return null;
  };

  /**
   * A panel's strip label: the facet value, with "(n)" when show_n covers
   * facets; null for a single panel.
   * @param {any[]} rows @param {Record<string, any>} cfg @param {number} nFacets
   * @returns {(facet: string) => string | null}
   */
  const facetLabels = (rows, cfg, nFacets) => {
    const counts = NS.countOn(cfg, 'facet') ? NS.labelCounts(rows, cfg, cfg.facet) : null;
    return (fv) => (nFacets === 1 || fv === '__all__' ? null
      : (counts ? NS.withCount(fv, counts) : fv));
  };

  /** The tooltip columns of a config, less `drop` (columns the tooltip shows anyway).
   * @param {Record<string, any>} cfg @param {string[]} [drop] @returns {string[]} */
  const tooltipFields = (cfg, drop = []) => [].concat(cfg.tooltip || []).map(String)
    .filter((c) => c && c !== 'null' && c !== '(none)' && !drop.includes(c));

  Object.assign(model, { settings, defaultLie, FIXED, defaults, gate, emptyState, facetLabels,
                         tooltipFields, GRID_COL, gridKey, gridKeepsMissing, gridLevels, facetKeys,
                         facetLevels });
})();
// --- chart/model-timeline.js
// @ts-check
/**
 * Chart block: the timeline (gantt) model.
 *
 * Rows and config in, what to draw out: the panels (one per facet level
 * holding rows), their lanes in sort order, the lane and facet counts, the
 * colour levels, and one mark per row with an x. A mark is a tuple, since a
 * swim-lane chart has a mark per event; ECharts hands it back as
 * `params.value` on a click:
 *
 *   [start, end, lane, term, colour, label, series, drill, tooltip, open]
 *
 * start and end are x coordinates (a category index on a category x, ms on
 * a time x; end is start for an event without one), lane is the lane's position in the
 * panel, term the lane value as a string, colour/label/series/drill the
 * columns' values or '', tooltip the tooltip fields' values or 0. With an
 * `open` column, open is 1 where it is TRUE (an ongoing event) and 0 elsewhere;
 * an open event without an end runs to the last x or end in the data.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const model = /** @type {any} */ (NS.model);

  /**
   * @typedef {{ facet: string, label: string | null, lanes: string[],
   *             laneCounts: Map<string, any> | null, marks: any[][],
   *             rows: any[] }} TimelinePanel
   * @typedef {{ family: 'timeline', empty: VizEmpty | null,
   *             x: { col: string, type: string, cats: string[] | null,
   *                  period: string | null },
   *             y: { col: string }, xend: string | null, series: string | null,
   *             label: string | null, color: { col: string, levels: any[], scale: any } | null,
   *             facet: string | null, nFacets: number, single: boolean,
   *             ttFields: string[], panels: TimelinePanel[] }} TimelineModel
   */

  /** The x axis: its type (a Date or date-time is a time axis), its categories, and the period
   * a time axis is sampled at. @param {VizColumn[]} columns @param {any[]} rows @param {string} x */
  const xAxisOf = (columns, rows, x) => {
    const type = NS.axes.axisTypeFor(columns, rows, x, { temporal: true });
    return { col: x, type,
             cats: type === 'category' ? NS.axes.orderedCategories(columns, rows, x) : null,
             period: type === 'time' ? NS.axes.timePeriod(rows, x) : null };
  };

  /** An x value as a coordinate: a category's first position, a date in ms (it arrives as an ISO
   * string), or the number. @param {string} type @param {string[] | null} cats
   * @returns {(v: any) => number} */
  const xCoordOf = (type, cats) => {
    const xPos = cats ? NS.firstIndex(cats) : null;
    if (xPos) return (v) => xPos.get(String(v ?? '')) ?? 0;
    return type === 'time' ? NS.axes.timeValue : Number;
  };

  /** @param {{ rows: any[], ix: any, columns: VizColumn[], cfg: ChartSettings, derived: ChartDerived }} input
   * @returns {TimelineModel} */
  const timeline = ({ rows, ix, columns, cfg, derived }) => {
    const { xend, color, facet, sort_by, series, label } = cfg;
    const open = cfg.open || null;
    // gate() let only a chart with x and y through.
    const x = /** @type {string} */ (cfg.x), y = /** @type {string} */ (cfg.y);
    const ttFields = model.tooltipFields(cfg);
    // The drill column's value rides on every mark.
    const drill = NS.drillColumn(cfg);
    const sortDir = cfg.sort_dir === 'desc' ? -1 : 1;
    /** @type {TimelineModel} */
    const out = {
      family: 'timeline', empty: null,
      x: { col: x, type: 'value', cats: null, period: null }, y: { col: y },
      xend: xend || null, series: series || null, label: label ?? null,
      color: null, facet: facet || null, nFacets: 1, single: true, ttFields, panels: []
    };
    if (!x || !y) {
      out.empty = NS.model.emptyState('Select X (start) and Y (term) columns');
      return out;
    }

    out.x = xAxisOf(columns, rows, x);
    const { type: xAxisType, cats: xCats } = out.x;

    const facets = model.facetLevels(rows, ix, cfg);
    out.nFacets = facets.length;
    out.single = facets.length === 1;

    const colorScale = NS.scaleFor(derived, color);
    if (color) {
      const levels = NS.axes.orderLevels(ix.levels(rows, color, 'nz'), colorScale, columns, color);
      if (levels.length) out.color = { col: color, levels, scale: colorScale };
    }

    const xCoord = xCoordOf(xAxisType, xCats);
    /** @param {any} v */
    const isOpen = (v) => v === true || v === 'TRUE' || v === 'true';
    /** @param {any} r @returns {number} */
    const endOf = (r) => (xend && r[xend] != null ? xCoord(r[xend]) : NaN);
    // Where an open event without an end stops: the last x or end in the data.
    let lastX = -Infinity;
    if (open) {
      for (const r of rows) {
        for (const v of [r[x] != null ? xCoord(r[x]) : NaN, endOf(r)]) {
          if (!Number.isNaN(v) && v > lastX) lastX = v;
        }
      }
    }

    // Lanes in sort order: by name for 'alpha', else by the smallest value
    // per lane of x ('onset') or of a column; a lane without one is left out.
    /** @param {any[]} rs @returns {string[]} */
    const sortTerms = (rs) => {
      const sb = sort_by || 'onset';
      if (sb === 'alpha') {
        return [...new Set(rs.map((r) => String(r[y] ?? '')))]
          .sort((a, b) => a.localeCompare(b) * sortDir);
      }
      const sortCol = (sb === 'onset') ? x : sb;
      // Another column sorts on its number, a date or date-time in ms.
      const sv = sortCol === x ? xCoord
        : (V.util.isTemporal(NS.axes.colMeta(columns, sortCol)) ? NS.axes.timeValue : Number);
      // A plain object on purpose: its key order (integer-like keys first)
      // is the tie order.
      /** @type {Record<string, number>} */
      const mins = {};
      for (const r of rs) {
        const k = String(r[y] ?? '');
        const v = sv(r[sortCol]);
        if (!isNaN(v) && (mins[k] == null || v < mins[k])) mins[k] = v;
      }
      return Object.keys(mins).sort((a, b) => (mins[a] - mins[b]) * sortDir);
    };

    const facetLabel = model.facetLabels(rows, cfg, out.nFacets);
    // Fixed scales give every panel the same lanes; free scales keep each its own.
    const sharedTerms = NS.facetScales(cfg) !== 'free' ? sortTerms(rows) : null;
    const countAxis = NS.countOn(cfg, 'axis');

    for (const fv of facets) {
      const prow = fv === '__all__' ? rows : ix.get(rows, facet, 'raw', fv);
      if (!prow.length) continue;
      const terms = sharedTerms || sortTerms(prow);
      /** @type {Map<string, number>} */
      const laneOf = new Map();
      terms.forEach((t, i) => laneOf.set(t, i));

      // One mark per row with an x and a known lane; `markRows` is the row
      // behind each mark, for a filter on a column the mark does not carry.
      const marks = [];
      const markRows = [];
      for (const r of prow) {
        if (r[x] == null) continue;
        const term = String(r[y] ?? '');
        const lane = laneOf.get(term);
        if (lane === undefined) continue;
        const s = xCoord(r[x]);
        // A date that does not parse has no place on a time axis.
        if (xAxisType === 'time' && Number.isNaN(s)) continue;
        const hasEnd = xend && r[xend] != null &&
          !Number.isNaN(xAxisType === 'time' ? xCoord(r[xend]) : Number(r[xend]));
        const op = open && isOpen(r[open]) ? 1 : 0;
        const e = hasEnd ? xCoord(r[xend]) : (op ? Math.max(lastX, s) : s);
        marks.push([s, e, lane, term, (color != null ? r[color] ?? '' : ''),
                    (label != null ? (r[label] ?? '') : ''),
                    (series != null ? r[series] ?? '' : ''),
                    (drill != null ? (r[drill] ?? '') : ''),
                    ttFields.length ? ttFields.map((/** @type {string} */ c) => r[c] ?? '') : 0,
                    ...(open ? [op] : [])]);
        markRows.push(r);
      }
      out.panels.push({
        facet: fv,
        label: facetLabel(fv),
        // Events per lane, or distinct ids; a lane has no "as is" value.
        lanes: terms, laneCounts: countAxis ? NS.labelCounts(prow, { id: cfg.id }, y) : null,
        marks, rows: markRows
      });
    }
    return out;
  };

  /** The keys of a timeline mark: its lane, and the facet in a facet panel. The colour is no key.
   * @param {Record<string, any>} cfg @param {any} facetVal @param {any[]} mark
   * @returns {Record<string, any>} */
  const timelineKeys = (cfg, facetVal, mark) => {
    /** @type {Record<string, any>} */
    const keys = { [cfg.y]: mark[3] };
    return model.facetKeys(keys, cfg.facet, cfg.facet_grid, facetVal);
  };

  /**
   * The filter a click on a timeline mark sends, or null. Auto: the mark's
   * keys. A drill column: the event's own value of it, none when it has none.
   * @param {Record<string, any>} cfg @param {any} facetVal @param {any[]} mark
   * @returns {Record<string, any[]> | null}
   */
  const timelineClick = (cfg, facetVal, mark) => {
    const drill = NS.drillColumn(cfg);
    if (!drill) return null;
    if (cfg.drill === 'auto') return NS.keys.fromKeys(timelineKeys(cfg, facetVal, mark));
    const dv = mark[7];
    if (dv == null || dv === '') return null;
    return { [drill]: [String(dv)] };
  };

  Object.assign(model, { timeline, timelineKeys, timelineClick });
})();
// --- chart/model-aggregated.js
// @ts-check
/**
 * Chart block: the aggregated model (bar, waterfall, pie, treemap, and the
 * distribution marks boxplot and point range).
 *
 * Rows and config in, what to draw out: the cells, the panels (one per facet
 * level), each panel's groups in sort order, the colour levels, the "(n)"
 * counts, and for the distribution marks the category slots with their
 * summaries.
 *
 * The rows are R's cells, one per mark (R/chart-cells.R): the keys under
 * their column names, the rows each stands for (`.n`), its aggregate
 * (`.value`) and what else its mark draws. Nothing is aggregated here.
 *
 * A mark is addressed by (group, colour level) inside a panel; its keys are
 * the group, the colour where it splits the mark (bar segments, boxes, point
 * ranges), and the facet in a facet panel.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const model = /** @type {any} */ (NS.model = NS.model || {});

  // A colour-split distribution slot is `group + SEP + level`: ECharts
  // dedupes a category axis by value, so the bare group would collapse the
  // boxes of one group into one slot.
  const BOX_CAT_SEP = '\u0000';

  // Whether each panel picks its own categories. In a grid only "free" does:
  // "free_y" frees the value axis per row and keeps the categories shared.
  /** @param {any} cfg @returns {boolean} */
  const freeCategories = (cfg) => (cfg.facet_grid ? cfg.facet_scales === 'free'
    : NS.facetScales(cfg) === 'free');

  /** In a grid, each column's categories over the panels of that column.
   * @param {any[]} agg @param {any[]} rows @param {VizColumn[]} columns
   * @param {any} cfg @param {any} derived @returns {Map<string, string[]>} */
  const columnGroups = (agg, rows, columns, cfg, derived) => {
    const colOf = (/** @type {any} */ key) => (key == null ? null : model.gridLevels(key).col);
    /** @type {Map<string, string[]>} */ const out = new Map();
    for (const c of new Set(agg.map((/** @type {any} */ a) => colOf(a.facet)))) {
      const cells = agg.filter((/** @type {any} */ a) => colOf(a.facet) === c);
      const crows = rows.filter((r) => colOf(r[model.GRID_COL]) === c);
      out.set(/** @type {string} */ (c), orderGroups(cells, crows, columns, cfg, derived));
    }
    return out;
  };

  /**
   * R's cells as the aggregation engine gives them: {facet, group, color,
   * value, n}. A cell R marks `.drop` (a missing key under missing = "drop")
   * is not drawn.
   * @param {any[]} rows @param {Record<string, any>} cfg
   */
  const cellsFromRows = (rows, cfg) => {
    const { category, color, facet, func } = cfg;
    const out = [];
    for (const r of rows) {
      if (r['.drop']) continue;
      out.push({
        facet: facet ? String(r[facet] ?? '') : '__all__',
        group: category ? String(r[category] ?? '') : 'Total',
        color: color ? String(r[color] ?? '') : '__all__',
        value: func === 'count' ? r['.n'] : (r['.value'] ?? null),
        n: r['.n']
      });
    }
    return out;
  };

  /** The order of a panel's groups on the category axis. A bridge always keeps the data order (a
   * value sort scrambles the running total). "data" is factor levels, else time or numeric order
   * for a date or number column, else first appearance in the rows. "alpha" is factor levels, else
   * by name. "value" is the total across colour cells. Any other value names a column whose
   * smallest value per group orders it. @param {any[]} cells the panel's aggregate cells
   * @param {any[]} rows @param {VizColumn[]} columns @param {Record<string, any>} cfg
   * @param {ChartDerived} derived @returns {string[]} */
  const orderGroups = (cells, rows, columns, cfg, derived) => {
    const groups = [...new Set(cells.map((a) => a.group))];
    const sortBy = cfg.sort_by || 'alpha';
    const sortDir = cfg.sort_dir === 'desc' ? -1 : 1;
    const groupCol = cfg.category;
    const meta = NS.axes.colMeta(columns, groupCol);
    const levels = meta && Array.isArray(meta.levels) && meta.levels.length
      ? meta.levels.map(String) : null;
    // Every bin of a histogram, the empty ones too: a missing bin would
    // close the gap in the distribution it stands for. R's edges list them.
    if (cfg.type === 'histogram') {
      // R's edges, a data frame sent as columns.
      const edges = derived.hist && derived.hist.label;
      const bins = Array.isArray(edges) && edges.length ? edges.map(String) : levels;
      if (bins) return bins;
    }
    const levelAt = new Map((levels || []).map((/** @type {string} */ l, /** @type {number} */ i) => [l, i]));

    const dataOrder = () => {
      const present = new Set(groups);
      if (levels) {
        return levels.filter((/** @type {string} */ l) => present.has(l)).concat(groups.filter((g) => !levelAt.has(g)));
      }
      // A date or a number orders by itself, a missing value last. Dates
      // travel as ISO strings, which sort as text.
      if (V.util.isTemporal(meta) || V.util.isNumeric(meta)) {
        const num = !V.util.isTemporal(meta);
        const key = (/** @type {string} */ g) => (num ? Number(g) : g);
        const missing = (/** @type {string} */ g) => g === '' || (num && isNaN(Number(g)));
        const known = groups.filter((g) => !missing(g));
        known.sort((a, b) => (key(a) < key(b) ? -1 : key(a) > key(b) ? 1 : 0));
        return known.concat(groups.filter(missing));
      }
      const seen = new Set(rows.map((r) => (groupCol ? String(r[groupCol] ?? '') : 'Total')));
      return [...seen].filter((g) => present.has(g)).concat(groups.filter((g) => !seen.has(g)));
    };

    if (cfg.type === 'waterfall') return dataOrder();
    if (sortBy === 'data') {
      const ord = dataOrder();
      return sortDir < 0 ? ord.reverse() : ord;
    }
    if (sortBy === 'alpha') {
      const at = (/** @type {string} */ g) => levelAt.get(g) ?? 1e9;
      return groups.sort((a, b) => ((at(a) - at(b)) || a.localeCompare(b)) * sortDir);
    }
    if (sortBy === 'value') {
      /** @type {Record<string, number>} */
      const totals = {};
      for (const a of cells) totals[a.group] = (totals[a.group] || 0) + (a.value ?? 0);
      return groups.sort((a, b) => ((totals[a] || 0) - (totals[b] || 0)) * sortDir);
    }
    if (!groupCol) return groups.sort((a, b) => a.localeCompare(b) * sortDir);
    /** @type {Record<string, number>} */
    const mins = {};
    // A cell brings the column's smallest value in its rows (`.sort`).
    for (const r of rows) {
      const g = String(r[groupCol] ?? '');
      const v = Number(r['.sort']);
      if (!isNaN(v) && (mins[g] == null || v < mins[g])) mins[g] = v;
    }
    return groups.sort((a, b) => {
      const av = mins[a], bv = mins[b];
      if (av == null && bv == null) return a.localeCompare(b) * sortDir;
      if (av == null) return 1;
      if (bv == null) return -1;
      return (av - bv) * sortDir;
    });
  };

  /** The first row's tooltip-field values per group, per facet ('__all__' for all rows).
   * @param {any[]} rows @param {Record<string, any>} cfg @param {string[]} ttFields
   * @returns {Map<string, Record<string, any[]>>} */
  const ttRepValues = (rows, cfg, ttFields) => {
    const groupCol = cfg.category, facetCol = cfg.facet;
    /** @type {Map<string, Record<string, any[]>>} */
    const byFacet = new Map([['__all__', {}]]);
    // A cell brings the fields of its first row, and the cells come in the
    // order of their first rows.
    for (const r of rows) {
      const g = groupCol ? String(r[groupCol] ?? '') : 'Total';
      const recs = [byFacet.get('__all__')];
      if (facetCol) {
        const fv = String(r[facetCol] ?? '');
        if (!byFacet.has(fv)) byFacet.set(fv, {});
        recs.push(byFacet.get(fv));
      }
      for (const rec of recs) if (rec && !(g in rec)) rec[g] = ttFields.map((c) => r['.tt.' + c] ?? '');
    }
    return byFacet;
  };

  /**
   * `label` on a bar, slice or tile: per facet, group and colour level, the
   * one value the cell's rows carry; null when they disagree. Pie and
   * treemap draw one mark per group, so `byColor` false keys every row
   * '__all__'.
   * @param {any[]} rows @param {Record<string, any>} cfg @param {boolean} byColor
   * @returns {Map<string, string | null>} keyed facet|||group|||color
   */
  const markLabels = (rows, cfg, byColor) => {
    const groupCol = cfg.category, facetCol = cfg.facet;
    const colorCol = byColor ? cfg.color : null;
    /** @type {Map<string, string | null>} */
    const out = new Map();
    // A cell brings the distinct values of its rows, two at most.
    for (const r of rows) {
      const key = (facetCol ? String(r[facetCol] ?? '') : '__all__') + '|||' +
        (groupCol ? String(r[groupCol] ?? '') : 'Total') + '|||' +
        (colorCol ? String(r[colorCol] ?? '') : '__all__');
      for (const v of NS.valuesOf(r['.label'])) {
        if (v == null || v === '' || v === 'NA') continue;
        const t = String(v);
        const cur = out.get(key);
        if (cur === undefined) out.set(key, t);
        else if (cur !== null && cur !== t) out.set(key, null);
      }
    }
    return out;
  };

  /**
   * The category slots of a distribution panel and what each one draws: one
   * per group, or per (group, level) when colour splits the marks. An empty
   * (group, level) slot is dropped unless a fixed facet grid needs every
   * slot in every panel. One panel is no grid: a facet column the data does
   * not hold (a board's "Subgroup" before a subgroup is picked) puts every
   * row in one panel, and that panel keeps no empty slot.
   * @param {{ rows: any[], ix: any, cfg: Record<string, any>, groups: string[],
   *           levels: string[], facet: string, panels: number }} p
   */
  const distributionSlots = ({ rows, ix, cfg, groups, levels, facet, panels }) => {
    const groupBy = cfg.category, colorCol = cfg.color, facetCol = cfg.facet;
    const isBox = cfg.type === 'boxplot';
    // Keyed 'nz', so the panel of a missing facet value holds its rows.
    const facetRows = (facetCol && facet !== '__all__')
      ? ix.get(rows, facetCol, 'nz', String(facet)) : rows;
    const split = levels.length > 0;
    const keepEmpty = !!facetCol && panels > 1 && !freeCategories(cfg);
    const outliersOn = isBox && cfg.box_points === 'outliers';
    const idCol = cfg.id;

    const slots = [];
    for (const g of groups) {
      for (const lv of (split ? levels : ['__all__'])) {
        const c = (lv === '__all__'
          ? ix.get(facetRows, groupBy, 'raw', g)
          : ix.get2(facetRows, groupBy, 'raw', colorCol, 'nz', g, lv))[0];
        const n = (c && c['.vn']) || 0;
        if (split && !keepEmpty && n === 0) continue;
        const body = n ? { center: c['.center'], lo: c['.lo'], hi: c['.hi'] } : null;
        slots.push({
          cat: split ? g + BOX_CAT_SEP + lv : g, group: g, level: lv,
          n, count: idCol ? ((c && c['.count']) || 0) : n, body,
          whisker: isBox && body ? { lo: c['.wlo'], hi: c['.whi'] } : null,
          outliers: outliersOn && body ? NS.valuesOf(c['.out'] ?? []) : []
        });
      }
    }
    return slots;
  };

  /**
   * @typedef {{ facet: string, label: string | null, groups: string[], cells: any[],
   *             axisCounts: Map<string, any> | null, ttRep: Record<string, any[]> | null,
   *             slots: any[] | null }} AggregatedPanel
   * @typedef {{ family: 'aggregated', empty: VizEmpty | null, type: string,
   *             cumulative: boolean, colors: string[], colorScale: any,
   *             levels: string[], split: boolean, distribution: boolean,
   *             noValue: boolean, ttFields: string[],
   *             facet: string | null, nFacets: number, single: boolean,
   *             ix: any, panels: AggregatedPanel[], histogram: boolean,
   *             markLabels: Map<string, string | null> | null,
   *             hl: Record<string, string[]> | null,
   *             hlLit: ((facet: string, group: string, level: string | null) => boolean) | null,
   *             hlColors: Set<string> | null }} AggregatedModel
   */

  /** @param {{ rows: any[], ix: any, columns: VizColumn[], cfg: ChartSettings, derived: ChartDerived }} input
   * @returns {AggregatedModel} */
  const aggregated = ({ rows, ix, columns, cfg, derived }) => {
    // A histogram is a bar over the bins R adds to the rows.
    const ct = cfg.type === 'histogram' ? 'bar' : cfg.type;
    const { category, color, facet, measure } = cfg;
    const distribution = NS.DISTRIBUTION_TYPES.includes(ct);
    /** @type {AggregatedModel} */
    const out = {
      family: 'aggregated', empty: null, type: ct, cumulative: cfg.type === 'waterfall',
      colors: [], colorScale: NS.scaleFor(derived, color), levels: [], split: false,
      distribution, noValue: distribution && (!measure || measure === '.count'),
      ttFields: [], facet: facet || null, nFacets: 1, single: true,
      ix, panels: [], histogram: cfg.type === 'histogram', markLabels: null,
      hl: null, hlLit: null, hlColors: null
    };
    const agg = cellsFromRows(rows, cfg);
    if (agg.length === 0) {
      out.empty = model.emptyState('No data to chart');
      return out;
    }

    const facets = [...new Set(agg.map((/** @type {any} */ a) => a.facet))].sort();
    out.nFacets = facets.length;
    out.single = facets.length === 1;
    out.colors = NS.axes.orderLevels(
      [...new Set(agg.map((/** @type {any} */ a) => a.color))].filter((c) => c !== '__all__'),
      out.colorScale, columns, color);
    if (distribution && color) {
      out.levels = NS.axes.orderLevels(
        ix.levels(rows, color, 'nz').filter((/** @type {string} */ l) => l !== ''),
        out.colorScale, columns, color);
    }
    out.split = out.levels.length > 0;

    // Fixed scales give every panel the same categories; pie and treemap
    // label their own slices.
    const sharedGroups = (!freeCategories(cfg) && ct !== 'pie' && ct !== 'treemap')
      ? orderGroups(agg, rows, columns, cfg, derived) : null;
    // A free grid shares the categories down a column, as the panels of a
    // column share the category axis.
    const colGroups = (!sharedGroups && cfg.facet_grid && ct !== 'pie' && ct !== 'treemap')
      ? columnGroups(agg, rows, columns, cfg, derived) : null;

    const isBar = (ct === 'bar' || ct === 'waterfall');
    out.ttFields = (isBar && !out.cumulative) ? model.tooltipFields(cfg) : [];
    // On-mark text: a bar (not a waterfall step or histogram bin), slice or tile.
    if (cfg.label && ((ct === 'bar' && !out.cumulative && !out.histogram) ||
                      ct === 'pie' || ct === 'treemap')) {
      out.markLabels = markLabels(rows, cfg, ct === 'bar');
    }
    const ttRep = out.ttFields.length ? ttRepValues(rows, cfg, out.ttFields) : null;
    const countAxis = isBar && NS.countOn(cfg, 'axis');
    const facetLabel = model.facetLabels(rows, cfg, out.nFacets);

    for (const fv of facets) {
      const cells = ix.get(agg, 'facet', 'raw', fv);
      const groups = sharedGroups || (colGroups && colGroups.get(model.gridLevels(fv).col)) ||
        orderGroups(cells, rows, columns, cfg, derived);
      const restrict = !!facet && fv !== '__all__';
      let axisCounts = null;
      if (countAxis) {
        axisCounts = NS.labelCounts(restrict ? ix.get(rows, facet, 'nz', fv) : rows, cfg, category);
      }
      out.panels.push({
        facet: fv,
        label: facetLabel(fv),
        groups, cells, axisCounts,
        ttRep: ttRep ? (ttRep.get(restrict ? String(fv) : '__all__') || {}) : null,
        slots: (distribution && !out.noValue)
          ? distributionSlots({ rows, ix, cfg, groups, levels: out.levels, facet: fv,
                                panels: facets.length }) : null
      });
    }
    highlightMarks(out, rows, ix, cfg);
    return out;
  };

  /** The highlight: whether a bar (or a segment of one) holds a highlighted row, and with a colour
   * split the colour levels that do, which alone get palette colours. Bars only.
   * @param {AggregatedModel} out @param {any[]} rows @param {any} ix
   * @param {Record<string, any>} cfg */
  const highlightMarks = (out, rows, ix, cfg) => {
    const hl = NS.highlightOf(cfg);
    if (!hl || out.cumulative || out.histogram || out.type !== 'bar') return;
    /** @type {Map<string, boolean>} */
    const memo = new Map();
    /** @param {string} fv @param {string} g @param {string | null} lv @returns {boolean} */
    const hlLit = (fv, g, lv) => {
      const k = fv + '\u0000' + g + '\u0000' + (lv == null ? '\u0001' : lv);
      const seen = memo.get(k);
      if (seen !== undefined) return seen;
      const keys = aggregatedKeys(cfg, fv, g, lv);
      // A highlight on a column the cells do not hold: R marked the cells
      // with a matching row (`.hl`).
      const lit = !Object.keys(hl).every((c) => c in keys)
        ? rowsUnder(rows, ix, keys).some((r) => r['.hl'] === true)
        : !!NS.keys.markLit(keys, () => rowsUnder(rows, ix, keys), hl);
      memo.set(k, lit);
      return lit;
    };
    out.hl = hl;
    out.hlLit = hlLit;
    if (out.colors.length) {
      /** @type {Set<string>} */
      const lit = new Set();
      for (const p of out.panels) {
        for (const a of p.cells) {
          if (a.value != null && hlLit(p.facet, a.group, a.color)) lit.add(a.color);
        }
      }
      out.hlColors = lit;
    }
  };

  /**
   * The aggregate cell of (group, colour) in a panel: the first, or
   * undefined. Without a colour, the group's first cell.
   * @param {AggregatedModel} m @param {AggregatedPanel} panel @param {string} g @param {string | null} [c]
   */
  const cellOf = (m, panel, g, c) => (c == null
    ? m.ix.get(panel.cells, 'group', 'raw', g)[0]
    : m.ix.get2(panel.cells, 'group', 'raw', 'color', 'raw', g, c)[0]);

  /** The cells of one group in a panel. @param {AggregatedModel} m
   *  @param {AggregatedPanel} panel @param {string} g @returns {any[]} */
  const cellsOf = (m, panel, g) => m.ix.get(panel.cells, 'group', 'raw', g);

  /** The `label` text of a bar, slice or tile, or null.
   *  @param {AggregatedModel} m @param {AggregatedPanel} panel @param {string} g
   *  @param {string | null} [c] @returns {string | null} */
  const labelOf = (m, panel, g, c) => (m.markLabels
    ? (m.markLabels.get(panel.facet + '|||' + g + '|||' + (c == null ? '__all__' : c)) ?? null)
    : null);

  /**
   * The keys of an aggregated mark: the group, the colour where colour
   * splits the mark, and the facet in a facet panel. A missing value is ''.
   * @param {Record<string, any>} cfg @param {any} facetVal
   * @param {string | null} group @param {string | null} level
   * @returns {Record<string, string>}
   */
  const aggregatedKeys = (cfg, facetVal, group, level) => {
    /** @type {Record<string, string>} */
    const keys = {};
    if (group != null) keys[cfg.category] = group;
    if (cfg.color && level != null) keys[cfg.color] = level;
    model.facetKeys(keys, cfg.facet, cfg.facet_grid, facetVal);
    return keys;
  };

  /**
   * The (group, level) a click landed on, or null when it has no mark. Bars
   * read the colour off the series, a split distribution off its slot.
   * @param {AggregatedModel} m @param {Record<string, any>} cfg @param {any} params
   * @returns {{ group: string | null, level: string | null } | null}
   */
  const aggregatedMarkAt = (m, cfg, params) => {
    if (!params || (params.componentType && params.componentType !== 'series')) return null;
    const g = params.name || (params.value && params.value[0]);
    if (!g) return null;
    if (m.distribution && m.split && typeof g === 'string') {
      const [grp, lv] = g.split(BOX_CAT_SEP);
      return { group: grp, level: lv == null ? null : lv };
    }
    const level = (m.type === 'bar' && !m.cumulative && m.colors.includes(params.seriesName))
      ? String(params.seriesName) : null;
    return { group: String(g), level };
  };

  /** The rows under a mark: the rows whose columns hold the mark's keys. @param {any[]} rows
   * @param {any} ix @param {Record<string, any>} keys @returns {any[]} */
  const rowsUnder = (rows, ix, keys) => {
    let cur = rows;
    for (const col of Object.keys(keys)) {
      cur = ix.get(cur, col, 'nz', String(keys[col]));
      if (!cur.length) break;
    }
    return cur;
  };

  /**
   * The filter a click on a mark sends, or null. Auto: the mark's keys. A
   * drill column: its values in the rows under the mark.
   * @param {Record<string, any>} cfg @param {Record<string, string>} keys
   * @param {() => any[]} rows
   * @returns {Record<string, any[]> | null}
   */
  const aggregatedClick = (cfg, keys, rows) => {
    const drill = NS.drillColumn(cfg);
    if (!drill) return null;
    if (cfg.drill !== 'auto') return NS.keys.fromRows(drill, rows());
    return NS.keys.fromKeys(keys);
  };

  Object.assign(model, { aggregated, cellOf, cellsOf, labelOf, aggregatedKeys,
                         aggregatedMarkAt, aggregatedClick, rowsUnder, BOX_CAT_SEP });
})();
// --- chart/model-individual.js
// @ts-check
/**
 * Chart block: the individual model (scatter, line, band).
 *
 * Rows and config in, what to draw out: the axes, the series levels (the
 * series column, else the colour column) with their colours, the legend,
 * and per facet panel its rows and, for scatter and line, one series per
 * level with the points it draws. A band draws R's windowed statistics
 * (derived.band.series), so its panels carry rows only for clicks and
 * highlights.
 *
 * A point is a tuple [x, y, ...tooltip values]; a series' `rows` line up
 * with its `pts`, so a datum index is a row.
 *
 * A mark's keys: a line or band level is keyed by its level and the facet; a
 * scatter point by its series (when set) and the facet, else by its x and y.
 * Colour never widens a point.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const model = /** @type {any} */ (NS.model = NS.model || {});

  // Line charts: past TRAJ_FULL_MAX series the lines thin and lose their
  // markers, past TRAJ_REDUCED_MAX they thin further, past the cap the rest
  // are not drawn (the footer says so).
  const TRAJ_FULL_MAX = 50;
  const TRAJ_REDUCED_MAX = 500;
  const TRAJ_HARD_CAP = 1000;


  /**
   * @typedef {{ level: string | undefined, color: string, rows: any[], pts: any[][],
   *             src: any[], muted?: boolean }} IndSeries
   * @typedef {{ facet: string, label: string | null, rows: any[],
   *             series: IndSeries[] }} IndividualPanel
   * @typedef {{ family: 'individual', empty: VizEmpty | null, type: string,
   *             isLine: boolean, isBand: boolean,
   *             x: { col: string, type: string, cats: string[] | null,
   *                  order: Map<string, number> | null, encode?: (v: any) => any,
   *                  period?: string | null },
   *             y: { col: string, type: string, cats: string[] | null },
   *             seriesCol: string | null, color: string | null, splitCol: string | null,
   *             facet: string | null, grid: { row: string, col: string | null } | null,
   *             levels: string[], levelColors: string[],
   *             palette: string[], colorScale: any, seriesCount: number,
   *             lineOpacity: number, symbol: string, capMessage: string | null,
   *             crowd: boolean, logX: boolean, logY: boolean, logDropped: number,
   *             ttFields: string[], legend: any, byColor: boolean,
   *             nFacets: number, single: boolean, ix: any,
   *             panels: IndividualPanel[], hl: Record<string, string[]> | null }} IndividualModel
   */

  /** The x and y axes (type, categories, how a value is placed) and whether each is logarithmic,
   * set on the model. @param {any[]} rows @param {VizColumn[]} columns @param {any} cfg
   * @param {IndividualModel} out */
  const setAxes = (rows, columns, cfg, out) => {
    const { x, y } = cfg;
    // A Date x is a time axis; a Date y stays a category.
    const xType = NS.axes.axisTypeFor(columns, rows, x, { temporal: true });
    const yType = NS.axes.axisTypeFor(columns, rows, y);
    const xCats = xType === 'category' ? NS.axes.orderedCategories(columns, rows, x) : null;
    const yCats = yType === 'category' ? NS.axes.orderedCategories(columns, rows, y) : null;
    // A line on a categorical x is drawn in axis order.
    const xOrder = xCats
      ? new Map(xCats.map((/** @type {any} */ c, /** @type {number} */ i) => [String(c), i]))
      : null;
    // A date arrives as an ISO string; a time axis places it in ms.
    const encodeX = (/** @type {any} */ v) => xType === 'category' ? String(v ?? '')
      : (xType === 'time' ? NS.axes.timeValue(v) : Number(v));
    out.x = { col: x, type: xType, cats: xCats, order: xOrder, encode: encodeX,
              period: xType === 'time' ? NS.axes.timePeriod(rows, x) : null };
    out.y = { col: y, type: yType, cats: yCats };
    // A row at or below zero on a log axis is not drawn; the footer counts it.
    const logs = NS.logAxes(cfg);
    out.logX = logs.x && xType === 'value';
    out.logY = logs.y && yType === 'value';
  };

  /** The facet panels: per panel its rows and, for scatter and line, one series per level.
   * @param {any[]} rows @param {any} ix @param {any} cfg @param {IndividualModel} out
   * @param {string[]} facets */
  const buildPanels = (rows, ix, cfg, out, facets) => {
    const { isLine, isBand, splitCol, facet, levels } = out;
    const { col: x, type: xType, order: xOrder } = out.x;
    const encodeX = /** @type {(v: any) => any} */ (out.x.encode);
    const y = out.y.col, yType = out.y.type;
    const encodeY = (/** @type {any} */ v) => yType === 'category' ? String(v ?? '') : Number(v);
    /** @param {any} r */
    const packPt = (r) => [encodeX(r[x]), encodeY(r[y]), ...out.ttFields.map((c) => r[c] ?? '')];
    const drawn = (/** @type {any} */ r) => {
      if (r[x] == null || r[y] == null) return false;
      if ((out.logX && !(Number(r[x]) > 0)) || (out.logY && !(Number(r[y]) > 0))) {
        out.logDropped++;
        return false;
      }
      return true;
    };
    /** @param {string | undefined} level @param {any[]} rs @param {string} clr @returns {IndSeries} */
    const mkSeries = (level, rs, clr) => {
      let pts = rs.map(packPt);
      let srows = rs;
      if (isLine) {
        const order = lineOrder(pts, xType, xOrder);
        if (order) {
          pts = order.map((i) => pts[i]);
          srows = order.map((i) => rs[i]);
        }
      }
      // `src` keeps the rows in data order, for the error bars.
      return { level, color: clr, rows: srows, pts, src: rs };
    };
    const facetLabel = model.facetLabels(rows, cfg, out.nFacets);
    for (const fv of facets) {
      // Keyed 'raw': the panel of a missing facet value holds none.
      const prow = fv === '__all__' ? rows : ix.get(rows, /** @type {string} */ (facet), 'raw', fv);
      /** @type {IndSeries[]} */
      const series = [];
      if (!isBand) {
        if (!levels.length) {
          series.push(mkSeries(undefined, prow.filter(drawn), out.palette[0]));
        } else {
          // Keyed 'nz' like the levels: a null split value is the '' series.
          levels.forEach((lv, ci) => series.push(
            mkSeries(lv, ix.get(prow, splitCol, 'nz', lv).filter(drawn), out.levelColors[ci])));
        }
      }
      out.panels.push({
        facet: fv,
        label: facetLabel(fv),
        rows: prow, series
      });
    }
  };

  /** @param {{ rows: any[], ix: any, columns: VizColumn[], cfg: ChartSettings, derived: ChartDerived }} input
   * @returns {IndividualModel} */
  const individual = ({ rows, ix, columns, cfg, derived }) => {
    const ct = cfg.type;
    // gate() let only a chart with x and y through.
    const x = /** @type {string} */ (cfg.x), y = /** @type {string} */ (cfg.y);
    const color = cfg.color || null;
    const facet = cfg.facet || null;
    const seriesCol = cfg.series || null;
    // The series splits; without one, a series per colour level.
    const splitCol = seriesCol || color;
    const isLine = NS.isLineType(ct);
    const isBand = ct === 'band';
    /** @type {IndividualModel} */
    const out = {
      family: 'individual', empty: null, type: ct,
      isLine, isBand,
      x: { col: x, type: 'value', cats: null, order: null },
      y: { col: y, type: 'value', cats: null },
      seriesCol, color, splitCol, facet, grid: cfg.facet_grid || null, levels: [], levelColors: [],
      palette: NS.paletteOf(derived), colorScale: NS.scaleFor(derived, color),
      seriesCount: 1, lineOpacity: 1, symbol: 'circle', capMessage: null, crowd: false,
      logX: false, logY: false, logDropped: 0,
      ttFields: [], legend: null, byColor: false,
      nFacets: 1, single: true, ix, panels: [], hl: null
    };
    if (!x || !y) {
      out.empty = model.emptyState('Select X and Y columns');
      return out;
    }

    // Tooltip fields, less x (the header), y (the value) and the split column.
    out.ttFields = model.tooltipFields(cfg, [x, y, splitCol].filter(Boolean).map(String));

    const facets = model.facetLevels(rows, ix, cfg);
    out.nFacets = facets.length;
    out.single = facets.length === 1;

    /** @type {string[]} */
    let levels = splitCol
      ? NS.axes.orderLevels(ix.levels(rows, splitCol, 'nz'), NS.scaleFor(derived, splitCol),
                            columns, splitCol)
      : [];
    if (isLine && levels.length > TRAJ_HARD_CAP) {
      const what = splitCol ? NS.axes.axisTitle(columns, splitCol) : 'series';
      out.capMessage = `Showing first ${TRAJ_HARD_CAP} of ${levels.length} series` +
        ' (' + what + ') — filter upstream to narrow.';
      levels = levels.slice(0, TRAJ_HARD_CAP);
    }
    out.levels = levels;
    out.seriesCount = levels.length || 1;
    if (isLine) {
      if (out.seriesCount > TRAJ_REDUCED_MAX) { out.lineOpacity = 0.15; out.symbol = 'none'; }
      else if (out.seriesCount > TRAJ_FULL_MAX) { out.lineOpacity = 0.35; out.symbol = 'none'; }
    }
    // One line per entity: hover picks the nearest line.
    out.crowd = isLine && !!seriesCol && out.seriesCount > 1;
    const colours = seriesColours(rows, ix, out);
    out.levelColors = levels.map((lv, i) => colours.of(lv, i));
    legendOf(rows, ix, out, colours.legendColor, columns);
    setAxes(rows, columns, cfg, out);
    buildPanels(rows, ix, cfg, out, facets);
    highlightSeries(out, rows, ix, cfg, columns);
    return out;
  };

  /** The highlight on a scatter or line. A line is highlighted when one of its rows holds a
   * highlighted value, a point by its own row. What is not highlighted is `muted` (drawn grey by
   * the option) and comes first, so the highlighted marks draw over it: a scatter series splits
   * into its muted and its highlighted points. With a colour, the palette is counted over the
   * highlighted colour values only, and the legend lists only those. @param {IndividualModel} m
   * @param {any[]} rows @param {any} ix @param {Record<string, any>} cfg @param {any} columns */
  const highlightSeries = (m, rows, ix, cfg, columns) => {
    const hl = NS.highlightOf(cfg);
    if (!hl || m.isBand) return;
    m.hl = hl;
    const { color } = m;
    /** @param {any} r */
    const rowLit = (r) => NS.keys.rowMatches(r, hl);
    /** @param {IndSeries} s @param {any} facet */
    const lineLit = (s, facet) => NS.keys.markLit(individualKeys(m, facet, s.level), s.rows, hl);

    /** @type {Map<string, string>} */
    const colourOf = new Map();
    if (color) {
      // The colour values the highlighted marks carry.
      /** @type {Set<string>} */
      const lit = new Set();
      for (const p of m.panels) {
        for (const s of p.series) {
          const lr = m.isLine ? (lineLit(s, p.facet) ? s.rows : []) : s.rows.filter(rowLit);
          for (const r of lr) lit.add(String(r[color] ?? ''));
        }
      }
      const order = m.splitCol === color ? m.levels : colourOrder(rows, ix, m, columns);
      const cols = NS.litLevelColors(order, lit, m.colorScale, m.palette);
      /** @type {{ name: string, color: string }[]} */
      const items = [];
      order.forEach((/** @type {string} */ lv, /** @type {number} */ i) => {
        const c = cols[i];
        if (c) { colourOf.set(lv, c); items.push({ name: lv, color: c }); }
      });
      m.legend = items.length ? { col: color, items } : null;
    }
    /** @param {IndSeries} s @param {any[]} rs */
    const colourFor = (s, rs) => {
      if (!color) return s.color;
      const cv = rs.length ? String(rs[0][color] ?? '') : String(s.level ?? '');
      return colourOf.get(cv) || s.color;
    };

    for (const p of m.panels) {
      /** @type {IndSeries[]} */
      const back = [];
      /** @type {IndSeries[]} */
      const front = [];
      for (const s of p.series) {
        if (m.isLine) {
          if (lineLit(s, p.facet)) front.push({ ...s, color: colourFor(s, s.rows) });
          else back.push({ ...s, muted: true });
          continue;
        }
        const on = s.rows.map(rowLit);
        /** @param {boolean} want @returns {IndSeries} */
        const part = (want) => {
          const idx = on.map((v, i) => (v === want ? i : -1)).filter((i) => i >= 0);
          const rs = idx.map((i) => s.rows[i]);
          return { level: s.level, color: s.color, rows: rs,
                   pts: idx.map((i) => s.pts[i]), src: rs, muted: !want };
        };
        const mu = part(false), li = part(true);
        if (mu.rows.length) back.push(mu);
        if (li.rows.length) front.push({ ...li, color: colourFor(s, li.rows) });
      }
      p.series = back.concat(front);
    }
  };

  /**
   * The order a line's points are drawn in: by axis position on a
   * categorical x, by value on a numeric one; null on another category
   * axis (data order). Stable, so equal x keep their row order.
   * @param {any[][]} pts @param {string} xType @param {Map<string, number> | null} xOrder
   * @returns {number[] | null}
   */
  const lineOrder = (pts, xType, xOrder) => {
    const idx = pts.map((_, i) => i);
    if (xOrder) {
      const pos = pts.map((p) => xOrder.get(String(p[0])) ?? 0);
      return idx.sort((a, b) => pos[a] - pos[b]);
    }
    if (xType !== 'category') return idx.sort((a, b) => pts[a][0] - pts[b][0]);
    return null;
  };

  /** The colour of each series level, and of each legend chip. With a series column apart from the
   * colour column, a series takes the colour of its first row's colour value: the board scale's,
   * else the next palette colour in the order the values come up (cycling, so a level keeps its
   * colour under a filter). Otherwise a level takes the scale's colour or the palette's in level
   * order. @param {any[]} rows @param {any} ix @param {IndividualModel} m */
  const seriesColours = (rows, ix, m) => {
    const { palette, colorScale, color, seriesCol } = m;
    /** @param {string} v */
    const fromScale = (v) => (colorScale && colorScale.color && colorScale.color[v] != null
      ? colorScale.color[v] : null);
    /** @type {Map<string, string>} colour value -> its cycled colour */
    const lookup = new Map();
    /** @param {string} level @param {number} index */
    const of = (level, index) => {
      if (!color) return palette[0];
      if (seriesCol && seriesCol !== color) {
        const rep = ix.get(rows, seriesCol, 'nz', level)[0];
        const cv = rep ? String(rep[color] ?? '') : '';
        const scaled = fromScale(cv);
        if (scaled != null) return scaled;
        if (!lookup.has(cv)) lookup.set(cv, palette[lookup.size % palette.length]);
        return /** @type {string} */ (lookup.get(cv));
      }
      return fromScale(level) ?? palette[index % palette.length];
    };
    /** @param {string} level @param {number} index */
    const legendColor = (level, index) =>
      fromScale(level) ?? (lookup.get(level) || palette[index % palette.length]);
    return { of, legendColor };
  };

  /** The colour column's values in legend order, the order every chart type lists them in: the
   * board scale's, else factor levels, else alphabetical.
   * @param {any[]} rows @param {any} ix @param {IndividualModel} m @param {any} columns
   * @returns {string[]} */
  const colourOrder = (rows, ix, m, columns) =>
    NS.axes.orderLevels(ix.levels(rows, /** @type {string} */ (m.color), 'nz'), m.colorScale,
                        columns, m.color);

  /** The legend band's chips (the band hides them past NS.legendFits). With a series column apart
   * from the colour, the chips are the colour's values in legend order (colourOrder); the option
   * adds an empty series per chip for the legend to bind to, and past NS.legendFits the tooltip
   * names the colour value. Fills m.legend and m.byColor.
   * @param {any[]} rows @param {any} ix @param {IndividualModel} m
   * @param {(level: string, index: number) => string} legendColor @param {any} columns */
  const legendOf = (rows, ix, m, legendColor, columns) => {
    const { color, seriesCol } = m;
    if (!color) return;
    const colorLevels = ix.levels(rows, color, 'nz');
    if (!colorLevels.length) return;
    m.byColor = !!seriesCol && seriesCol !== color;
    if (m.byColor && !NS.legendFits(colorLevels.length) && !m.ttFields.includes(color)) {
      m.ttFields.push(color);
    }
    const items = m.byColor
      ? colourOrder(rows, ix, m, columns).map((/** @type {string} */ lvl, /** @type {number} */ i) =>
        ({ name: lvl, color: legendColor(lvl, i) }))
      : m.levels.map((lv, i) => ({ name: lv, color: m.levelColors[i] }));
    m.legend = { col: color, items };
  };

  /** The keys of a line, a band level or a scatter series: the split column's level and the facet
   * in a facet panel. @param {IndividualModel} m @param {any} facetVal @param {any} level
   * @returns {Record<string, any>} */
  const individualKeys = (m, facetVal, level) => {
    /** @type {Record<string, any>} */
    const keys = {};
    if (m.splitCol && level != null) keys[m.splitCol] = String(level);
    return model.facetKeys(keys, m.facet, m.grid, facetVal);
  };

  /** The panel a facet value names. @param {IndividualModel} m @param {any} facetVal */
  const panelOf = (m, facetVal) =>
    m.panels.find((p) => p.facet === facetVal) || null;

  /** The rows under a clicked point: the panel's rows at its x and y, within its series when the
   * chart is split. @param {IndividualModel} m @param {IndividualPanel} panel
   * @param {any} seriesName @param {any[]} v the point's value */
  const pointRows = (m, panel, seriesName, v) => {
    const pool = (m.splitCol && seriesName != null)
      ? m.ix.get(panel.rows, m.splitCol, 'nz', String(seriesName)) : panel.rows;
    const xs = String(v[0]), ys = String(v[1]);
    const enc = m.x.encode || ((/** @type {any} */ x) => x);
    return pool.filter((/** @type {any} */ r) =>
      String(enc(r[m.x.col])) === xs && String(r[m.y.col]) === ys);
  };

  /**
   * What a click sends, or null.
   *
   *   - A band: its level; an uncoloured band is not clickable.
   *   - A split line: its level, the hovered one (`focusName`) before the
   *     one ECharts' hit test names.
   *   - A scatter point: its series when one is set, else the point itself,
   *     a zero-width range at its x and y. An unsplit line's point too.
   *
   * Auto sends the mark's keys (the facet with them). A drill column sends
   * its values in the rows under the mark, nothing when there are none; for
   * a point, only to a drill target: the chart's own filter is the point, not
   * every row that shares its drill value.
   * @param {IndividualModel} m @param {Record<string, any>} cfg
   * @param {IndividualPanel | null} panel @param {any} params
   * @param {string} [focusName]
   * @returns {ChartSelection | null}
   */
  const individualClick = (m, cfg, panel, params, focusName) => {
    if (!panel || NS.drillState(cfg) === 'off') return null;
    const explicit = cfg.drill !== 'auto';
    /** @param {Record<string, any[]> | null} f @returns {ChartSelection | null} */
    const wrap = (f) => (f ? { type: 'categorical', filters: f } : null);

    /** @type {string | null} */
    let level = null;
    if (m.isBand) {
      if (!m.splitCol || params.seriesName == null) return null;
      level = String(params.seriesName);
    } else if (m.isLine && m.splitCol) {
      const name = focusName !== undefined ? focusName : params.seriesName;
      if (name != null) level = String(name);
    }
    if (level != null) {
      return wrap(explicit
        ? NS.keys.fromRows(cfg.drill, m.ix.get(panel.rows, m.splitCol, 'nz', level))
        : NS.keys.fromKeys(individualKeys(m, panel.facet, level)));
    }
    const v = params.value;
    if (!Array.isArray(v) || v.length < 2) return null;
    if (explicit && NS.transientDrill(cfg)) {
      return wrap(NS.keys.fromRows(cfg.drill, pointRows(m, panel, params.seriesName, v)));
    }
    if (m.seriesCol && !m.isLine && params.seriesName != null) {
      return wrap(NS.keys.fromKeys(individualKeys(m, panel.facet, params.seriesName)));
    }
    return inPanel(m, panel, m.x.col, m.y.col, [v[0], v[0]], [v[1], v[1]]);
  };

  /**
   * A range taken in a panel; in a facet panel it carries the panel's key
   * as `filters`.
   * @param {IndividualModel} m @param {IndividualPanel | null} panel
   * @param {string} x @param {string | null} y @param {any[]} xRange
   * @param {any[] | null} yRange @returns {ChartRangeSelection}
   */
  const inPanel = (m, panel, x, y, xRange, yRange) => {
    const keys = panel ? individualKeys(m, panel.facet, null) : {};
    return NS.keys.range(x, y, xRange, yRange,
                         Object.keys(keys).length ? NS.keys.fromKeys(keys) : null);
  };

  /**
   * What a brush caught: the x and y of every brushed point (a dataIndex is
   * its own series' index; one outside the series is skipped); with a drill
   * column, the panel's rows at those points, each (x, y) taken once.
   * @param {IndividualModel} m @param {Record<string, any>} cfg
   * @param {IndividualPanel | null} panel @param {any[]} allSeries chart.getOption().series
   * @param {Array<{ seriesIndex: number, dataIndex: number }>} selected
   * @returns {{ xVals: any[], yVals: any[], rows: any[], drill: string | null }}
   */
  const individualBrush = (m, cfg, panel, allSeries, selected) => {
    // Colour never widens a brush: only a drill column reads the rows.
    const drill = cfg.drill && cfg.drill !== 'auto' ? String(cfg.drill) : null;
    const encX = m.x.encode || ((/** @type {any} */ x) => x);
    /** @param {any} x @param {any} y */
    const ptKey = (x, y) => String(x) + '|||' + String(y);
    // The panel's rows by (x, y), for a drill column.
    /** @type {Map<string, any[]>} */
    const rowIndex = new Map();
    if (drill && panel) {
      for (const r of panel.rows) {
        const k = ptKey(encX(r[cfg.x]), r[cfg.y]);
        const b = rowIndex.get(k);
        if (b) b.push(r); else rowIndex.set(k, [r]);
      }
    }
    /** @type {any[]} */
    const xVals = [], yVals = [];
    /** @type {Set<string>} */
    const taken = new Set();
    for (const sel of selected) {
      const drawn = allSeries[sel.seriesIndex];
      // A legend chip or a label series is no point of the data.
      if (drawn && typeof drawn.id === 'string' && /^__(legend|label)_/.test(drawn.id)) continue;
      const sData = drawn && drawn.data;
      if (!sData || sel.dataIndex < 0 || sel.dataIndex >= sData.length) continue;
      const pt = sData[sel.dataIndex];
      if (!pt) continue;
      const vx = Array.isArray(pt) ? pt[0] : pt.value && pt.value[0];
      const vy = Array.isArray(pt) ? pt[1] : pt.value && pt.value[1];
      if (vx != null) xVals.push(vx);
      if (vy != null) yVals.push(vy);
      taken.add(ptKey(vx, vy));
    }
    // Each (x, y) once.
    /** @type {any[]} */
    const rows = [];
    for (const k of taken) for (const r of rowIndex.get(k) || []) rows.push(r);
    return { xVals, yVals, rows, drill };
  };

  /**
   * What a brush sends, from what it caught, or null: to a drill target, a
   * drill column's values in the rows at the points; else the points' x
   * (and, off a line, y) extent, which holds only the points inside the
   * rectangle. In a facet panel the range carries the panel's key.
   * @param {IndividualModel} m @param {Record<string, any>} cfg
   * @param {IndividualPanel | null} panel
   * @param {{ xVals: any[], yVals: any[], rows: any[], drill: string | null }} b
   * @returns {ChartSelection | null}
   */
  const brushSelection = (m, cfg, panel, b) => {
    if (b.drill && NS.transientDrill(cfg)) {
      const f = NS.keys.fromRows(b.drill, b.rows);
      if (f) return { type: 'categorical', filters: f };
    }
    if (!b.xVals.length) return null;
    return inPanel(m, panel, cfg.x, cfg.y, [NS.minOf(b.xVals), NS.maxOf(b.xVals)],
                   (!m.isLine && b.yVals.length) ? [NS.minOf(b.yVals), NS.maxOf(b.yVals)] : null);
  };

  /**
   * y of a line at `x`, interpolated between its points, or null outside
   * its range. On a category x the points are compared by axis position
   * (`xOrder`).
   * @param {any[]} data @param {number} x @param {Map<string, number> | null} xOrder
   * @returns {number | null}
   */
  const interpYAtX = (data, x, xOrder) => {
    const n = data.length;
    if (!n) return null;
    const xat = xOrder
      ? (/** @type {number} */ i) => {
          const v = xOrder.get(String(data[i][0]));
          return v == null ? NaN : v;
        }
      : (/** @type {number} */ i) => data[i][0];
    if (x < xat(0) || x > xat(n - 1)) return null;
    for (let k = 1; k < n; k++) {
      const x0 = xat(k - 1), x1 = xat(k);
      if (x >= x0 && x <= x1) {
        const y0 = data[k - 1][1], y1 = data[k][1];
        return x1 === x0 ? y0 : y0 + (y1 - y0) * (x - x0) / (x1 - x0);
      }
    }
    return null;
  };

  Object.assign(model, { individual, individualKeys, individualClick, individualBrush,
                         brushSelection, panelOf, interpYAtX });
})();
// --- chart/option-timeline.js
// @ts-check
/**
 * Chart block: the timeline (gantt) option, one ECharts option and canvas
 * height per panel. Nothing here touches the DOM; text is measured with the
 * function the caller passes.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const option = /** @type {any} */ (NS.option = NS.option || {});

  // Lane height and the room around the plot, in CSS px.
  const LANE_PX = 28;
  const PANEL_EXTRA_PX = 80;
  const PANEL_MIN_PX = 200;
  // The band above the plot that holds the time axis, its labels and its title.
  const GRID_TOP_PX = 48;

  /** Which marks each series of a panel draws, as mark positions in series order: one series per
   * colour level (marks of no level are not drawn), else one series of every mark. The option and
   * the patch share it. @param {any} m @param {any} panel @returns {number[][]} */
  const seriesMarks = (m, panel) => {
    if (!m.color) return [panel.marks.map((/** @type {any} */ _, /** @type {number} */ i) => i)];
    /** @type {Map<string, number[]>} */
    const buckets = new Map(m.color.levels.map((/** @type {any} */ lvl) =>
      /** @type {[string, number[]]} */ ([lvl, []])));
    panel.marks.forEach((/** @type {any[]} */ t, /** @type {number} */ i) => {
      const b = buckets.get(String(t[4] ?? ''));
      if (b) b.push(i);
    });
    return m.color.levels.map((/** @type {any} */ lvl) => buckets.get(lvl) || []);
  };

  /** What every part of the timeline option reads. @param {any} m @param {ChartLook} look */
  const timelineContext = (m, look) => {
    const { cfg, columns, ink, theme } = look;
    const title = (/** @type {any} */ col) => NS.axes.axisTitle(columns, col);
    const { type: xAxisType, cats: xCats, col: x } = m.x;
    const palette = NS.paletteOf(look.derived);
    const colorLevels = m.color ? m.color.levels : [];
    const showLegend = colorLevels.length > 0;
    const colors = showLegend ? NS.levelColors(colorLevels, m.color.scale, palette) : null;
    return { m, look, cfg, ink, theme, title, xAxisType, xCats, x, y: m.y.col, palette,
             colorLevels, showLegend, colors, onBarLabel: !!cfg.label };
  };

  /** A bar from start to end on its lane, at least 4px wide, clipped to the plot,
   * with the label column's value on it when it has room. An open (ongoing) bar
   * gets a stroked caret 2px past its end, in the bar's colour. @param {any} k */
  const timelineRenderItem = (k) => {
    const { ink, onBarLabel } = k;
    return (/** @type {any} */ params, /** @type {any} */ api) => {
      const start = api.coord([api.value(0), api.value(2)]);
      const end = api.coord([api.value(1), api.value(2)]);
      const h = api.size([0, 1])[1] * 0.6;
      const barW = Math.max(end[0] - start[0], 4);
      const rect = root.echarts.graphic.clipRectByRect(
        { x: start[0], y: start[1] - h / 2, width: barW, height: h },
        { x: params.coordSys.x, y: params.coordSys.y,
          width: params.coordSys.width, height: params.coordSys.height }
      );
      if (!rect) return;
      const barLabel = onBarLabel ? String(api.value(5) ?? '') : '';
      // The datum's opacity rides in the style: a filter dims the bars it does not light.
      const st = api.style();
      /** @type {any[]} */
      const children = [{ type: 'rect', shape: Object.assign({}, rect, { r: 3 }), style: st }];
      if (api.value(9)) {
        const cy = rect.y + rect.height / 2;
        const a = Math.max(rect.height * 0.42, 3);
        const ax = rect.x + rect.width + 2;
        children.push({
          type: 'polyline',
          shape: { points: [[ax, cy - a], [ax + a, cy], [ax, cy + a]] },
          style: { fill: 'none', stroke: st && st.fill, lineWidth: 1.6, lineCap: 'round',
                   lineJoin: 'round', opacity: st && st.opacity != null ? st.opacity : 1 }
        });
      }
      if (barW > 50 && barLabel) {
        children.push({
          type: 'text',
          style: {
            text: barLabel, x: rect.x + 6, y: rect.y + rect.height / 2, fill: '#fff',
            fontSize: ink.fontSize, fontWeight: 500, fontFamily: ink.face,
            textVerticalAlign: 'middle', truncate: { outerWidth: barW - 12 },
            opacity: st && st.opacity != null ? st.opacity : 1
          }
        });
      }
      return { type: 'group', children };
    };
  };

  /** The hovered bar: label, else series, else lane as headline, then the lane,
   * the interval and its length, the colour and the tooltip fields. @param {any} k */
  const timelineTooltip = (k) => {
    const { m, title, xAxisType, xCats, x, y } = k;
    // An interval starts and ends on a day: a date-time prints its hour, a
    // date its day, whatever the gap between starts.
    const period = m.x.period === 'time' ? 'time' : null;
    return (/** @type {any} */ p) => {
      const v = p.value;
      const term = String(v[3] ?? '');
      const colorVal = String(v[4] ?? '');
      const labelVal = m.label != null ? String(v[5] ?? '') : '';
      const detail = String(v[6] ?? '');
      const headline = labelVal || detail || term;
      /** @type {Array<[string, any]>} */
      const pairs = [];
      if (term && term !== headline) pairs.push([title(y) || 'Y', term]);
      if (detail && detail !== headline) pairs.push([title(m.series) || 'Series', detail]);
      const fromTxt = NS.axes.fmtXVal(v[0], xAxisType, xCats, period);
      if (v[9]) {
        pairs.push([title(x) || 'From', fromTxt]);
        pairs.push([(m.xend ? title(m.xend) : '') || 'To', 'ongoing']);
      } else if (v[1] != null && v[1] !== v[0]) {
        const toTxt = NS.axes.fmtXVal(v[1], xAxisType, xCats, period);
        const dur = NS.axes.durationStr(v[0], v[1], xAxisType);
        pairs.push([title(x) || 'From', fromTxt]);
        pairs.push([(m.xend ? title(m.xend) : '') || 'To', toTxt + (dur ? ' (' + dur + ')' : '')]);
      } else {
        pairs.push([title(x) || 'Date', fromTxt]);
      }
      if (colorVal) pairs.push([title(m.color ? m.color.col : null) || 'Color', colorVal]);
      const extra = Array.isArray(v[8]) ? v[8] : [];
      m.ttFields.forEach((/** @type {string} */ c, /** @type {number} */ i) => {
        pairs.push([title(c) || c, extra[i]]);
      });
      return NS.rowTooltip(headline, pairs);
    };
  };

  /** One panel: its bar series, lanes axis and canvas height.
   * @param {any} k @param {any} panel @param {any} renderItem @param {any} formatter
   * @param {any} xAxis */
  const timelinePanel = (k, panel, renderItem, formatter, xAxis) => {
    const { m, look, cfg, ink, theme, palette, colorLevels, showLegend, colors } = k;
    const layout = seriesMarks(m, panel);
    const data = (/** @type {number[]} */ idx) =>
      idx.map((i) => ({ value: panel.marks[i] }));
    const series = showLegend
      ? colorLevels.map((/** @type {any} */ lvl, /** @type {number} */ si) => ({
          type: 'custom', name: lvl, data: data(layout[si]),
          encode: { x: [0, 1], y: 2 }, renderItem
        }))
      : [{ type: 'custom', data: data(layout[0]), encode: { x: [0, 1], y: 2 }, renderItem }];

    const counts = panel.laneCounts;
    const gut = NS.axes.yGutter(counts
      ? panel.lanes.map((/** @type {string} */ t) => NS.withCount(t, counts)) : panel.lanes,
      look.measure);
    const display = counts ? (/** @type {any} */ v) => NS.withCount(v, counts) : null;
    const opt = {
      ...(theme ? {} : { backgroundColor: 'transparent' }),
      color: (m.color && m.color.scale && m.color.scale.color && colors) ? colors : palette,
      textStyle: { fontFamily: ink.face },
      tooltip: { trigger: 'item', confine: true, formatter },
      legend: showLegend ? { show: false, data: colorLevels } : { show: false },
      // The time axis is on top, like a table header: a tall chart shows it before any scrolling.
      grid: { left: gut.gridLeft, right: 10, top: GRID_TOP_PX, bottom: 20 },
      xAxis,
      yAxis: { ...NS.axes.categoryAxis(panel.lanes, ink, { gut, display }),
               splitLine: { show: false } },
      series
    };
    const height = Math.max(PANEL_MIN_PX,
      Math.min(NS.PANEL_H_CAP, panel.lanes.length * LANE_PX) + PANEL_EXTRA_PX) + 'px';
    return { option: NS.cardTooltip(NS.quietHover(opt, cfg), ink), height, axisBand: GRID_TOP_PX };
  };

  /** @param {any} m the timeline model @param {ChartLook} look
   * @returns {{ panels: Array<{ option: any, height: string, axisBand: number }> }} */
  const timeline = (m, look) => {
    const k = timelineContext(m, look);
    const renderItem = timelineRenderItem(k);
    const formatter = timelineTooltip(k);
    // The time axis, shared by every panel.
    const xAxis = {
      ...NS.axes.valueAxis(k.ink, k.xAxisType, k.title(k.x), 28, { grid: 'dashed' }),
      position: 'top',
      scale: true,
      ...(k.xCats ? { data: k.xCats } : {})
    };
    const panels = m.panels.map((/** @type {any} */ panel) =>
      timelinePanel(k, panel, renderItem, formatter, xAxis));
    return { panels };
  };

  /**
   * The legend chips: the colour levels in the option's colours.
   * @param {any} m @param {ChartSettings} cfg @param {ChartDerived} derived
   * @returns {{ col: string, items: Array<{ name: string, color: string }> } | null}
   */
  const timelineLegend = (m, cfg, derived) => {
    if (!m.color) return null;
    const colors = NS.levelColors(m.color.levels, m.color.scale, NS.paletteOf(derived));
    return { col: m.color.col,
             items: m.color.levels.map((/** @type {any} */ lvl, /** @type {number} */ i) =>
               ({ name: String(lvl), color: colors[i] })) };
  };

  /**
   * The highlight patch for one panel: every bar outlined or faded by whether its
   * mark (lane and facet) is lit. Without a filter, and nothing dimmed from
   * before, the patch is empty.
   * @param {any} m @param {any} panel @param {Record<string, any>} cfg
   * @param {((keys: Record<string, any>, rows: () => any[]) => boolean) | null} lit
   * @param {boolean} wasDimmed @param {Ink} [ink]
   * @returns {{ series: any[], dimmed: boolean }}
   */
  const timelinePatch = (m, panel, cfg, lit, wasDimmed, ink = NS.INK_DEFAULT) => {
    const layout = seriesMarks(m, panel);
    if (!lit && !wasDimmed) return { series: layout.map(() => ({})), dimmed: false };
    const series = layout.map((idx) => ({
      data: idx.map((i) => {
        const t = panel.marks[i];
        const on = !lit || lit(NS.model.timelineKeys(cfg, panel.facet, t), () => [panel.rows[i]]);
        return { value: t, itemStyle: NS.selStyle(on, !!lit, ink) };
      })
    }));
    return { series, dimmed: !!lit };
  };

  Object.assign(option, { timeline, timelineLegend, timelinePatch, timelineSeriesMarks: seriesMarks });
})();
// --- chart/option-bar-marks.js
// @ts-check
/**
 * Chart block: what a bar carries besides its bar. The shared sizes, the
 * rounded value end, value labels, text on the marks (`label`), the level
 * names on a stack (`legend = "marks"`) and highlighted names in semibold.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const option = /** @type {any} */ (NS.option = NS.option || {});

  // Waterfall sign colours: semantic, so they override the palette.
  const WATERFALL_COLORS = { increase: '#009E73', decrease: '#dc2626', total: '#bbbbbb' };

  // Corner radius of the value end (--blockr-mark-radius). The end at zero
  // is the axis and stays square; a floating step rounds both ends.
  const MARK_RADIUS = 2;
  /** @param {boolean} vertical @param {boolean} [floating] @returns {number[]} */
  const barRadius = (vertical, floating) => {
    const r = MARK_RADIUS;
    if (floating) return [r, r, r, r];
    return vertical ? [r, r, 0, 0] : [0, r, r, 0];
  };

  /** @param {any} d */
  const valOf = (d) => (d != null && typeof d === 'object') ? d.value : d;

  /** A series datum as an object. @param {any} d */
  const asDatum = (d) => (d !== null && typeof d === 'object' && !Array.isArray(d)) ? d : { value: d };

  /** A datum with `style` merged into its itemStyle. @param {any} d @param {Record<string, any>} style */
  const withItemStyle = (d, style) => {
    const o = asDatum(d);
    return { ...o, itemStyle: { ...o.itemStyle, ...style } };
  };

  /** Text style of a label on the marks. @param {any} ink */
  const labelStyle = (ink) => ({ color: ink.muted, fontSize: ink.fontSize, fontFamily: ink.face });

  /** The tallest positive stack, which ECharts scales the value axis from, and the value one px of
   * the axis spans (0 without a length). @param {any[]} series @param {number} n groups
   * @param {number} valuePx */
  const stackScale = (series, n, valuePx) => {
    let top = 0;
    for (let g = 0; g < n; g++) {
      let sum = 0;
      for (const s of series) {
        const v = Number(valOf(s.data[g]));
        if (Number.isFinite(v) && v > 0) sum += v;
      }
      top = Math.max(top, sum);
    }
    return { top, perPx: top > 0 && valuePx > 0 ? top / valuePx : 0 };
  };

  /** A group's drawn segments, bottom up: the series index, the datum, its value and the middle of
   * its stretch on the axis. @param {any[]} series @param {number} gi */
  const segmentsAt = (series, gi) => {
    let pos = 0, neg = 0;
    /** @type {Array<{ si: number, d: any, v: number, at: number }>} */
    const out = [];
    series.forEach((s, si) => {
      const d = s.data[gi];
      const v = Number(valOf(d));
      if (valOf(d) == null || !Number.isFinite(v) || v === 0) return;
      const at = (v >= 0 ? pos : neg) + v / 2;
      if (v >= 0) pos += v; else neg += v;
      out.push({ si, d, v, at });
    });
    return out;
  };

  /** Headroom for labels past the bars: sets the axis boundary gap and returns the px a grid needs
   * on its right for the widest end label. @param {Array<{ v: number, text: string }>} ends
   * @param {any} valAxis @param {boolean} vertical @param {number} plotPx
   * @param {(s: string) => number} textW */
  const endRoom = (ends, valAxis, vertical, plotPx, textW) => {
    const hasPos = ends.some((e) => e.v > 0);
    const hasNeg = ends.some((e) => e.v < 0);
    valAxis.boundaryGap = [hasNeg ? '12%' : 0, hasPos ? '12%' : 0];
    let w = 0;
    for (const e of ends) if (vertical || e.v > 0) w = Math.max(w, textW(e.text));
    // A vertical bar's label is centred on it and may overhang by half.
    if (vertical) return Math.max(0, Math.ceil(w / 2 - 10));
    if (!hasPos) return 0;
    // 12% headroom is about 10.7% of the plot past the longest bar; a fraction
    // axis is pinned at 100% and has none to lend.
    const room = valAxis.max === 1 ? 0 : 0.107 * plotPx;
    return Math.max(0, Math.ceil(w + 8 - room));
  };

  /** The formatter of a label at a bar's end; `fraction`: values are shares.
   * @param {Record<string, any>} cfg @param {boolean} fraction @returns {(v: number) => string} */
  const endFormat = (cfg, fraction) => {
    // `|| 0` folds a negative zero, which would print as "-0".
    if (fraction) return (v) => (Math.round(v * 1000) / 10 || 0) + '%';
    return NS.valueFormat(cfg) || ((v) => String(V.util.num(v || 0)));
  };

  const BAR_MAX = 48;
  const ROW_BAND = 28;     // horizontal row height, one bar per row
  const GROUP_BAR = 14;    // horizontal grouped: px per series bar
  const GROUP_PAD = 12;    // horizontal grouped: gap between groups
  // The category inset a vertical plot loses to the grid's margins.
  const X_INSET = 65;

  /**
   * `value_labels` at a bar's end: a stack once with its total, a grouped
   * bar per bar, a percent bar per segment where the share fits. Returns the
   * px a grid needs on its right. Mutates `series` and `valAxis`.
   * @param {any[]} series @param {any} valAxis
   * @param {{ink: any, vertical: boolean, stacked: boolean, percent: boolean,
   *          fraction: boolean, plotPx: number, bandPx: number,
   *          measure: (s: string) => number, cfg: Record<string, any>}} o
   * @returns {number}
   */
  const barValueLabels = (series, valAxis, o) => {
    const textW = o.measure;
    const fmt = endFormat(o.cfg, o.fraction);
    const style = labelStyle(o.ink);

    if (o.percent) {
      const barW = Math.min(48, 0.6 * o.bandPx);
      /** @param {number} v @param {string} t */
      const fits = (v, t) => !o.plotPx || (o.vertical
        ? v * o.plotPx >= o.ink.fontSize + 4 && (!o.bandPx || barW >= textW(t) + 4)
        : v * o.plotPx >= textW(t) + 6);
      for (const s of series) {
        s.label = { show: true, position: 'inside', ...style, color: '#fff',
          formatter: (/** @type {any} */ p) => {
            const v = Number(p.value);
            if (!Number.isFinite(v)) return '';
            const t = Math.round(v * 100) + '%';
            return fits(v, t) ? t : '';
          } };
        s.labelLayout = { hideOverlap: true };
      }
      return 0;
    }

    /** @type {Array<{ v: number, text: string }>} */
    const ends = [];
    /** @param {number} v */
    const addEnd = (v) => ends.push({ v, text: fmt(v) });
    for (const s of series) {
      s.label = { show: !o.stacked, position: 'outside', distance: 4, ...style,
        formatter: (/** @type {any} */ p) => p.value == null ? '' : fmt(Number(p.value)) };
      s.labelLayout = { hideOverlap: true };
      if (!o.stacked) {
        for (const d of s.data) {
          const v = valOf(d);
          if (v != null) addEnd(Number(v));
        }
      }
    }
    if (o.stacked) {
      const n = series.length ? series[0].data.length : 0;
      for (let gi = 0; gi < n; gi++) {
        let tot = 0, any = false;
        for (const s of series) {
          const v = valOf(s.data[gi]);
          if (v != null) { tot += Number(v); any = true; }
        }
        if (!any) continue;
        addEnd(tot);
        // The label rides the outermost segment of the total's sign.
        for (let si = series.length - 1; si >= 0; si--) {
          const v = valOf(series[si].data[gi]);
          if (v == null || (tot < 0 ? v > 0 : v < 0)) continue;
          series[si].data[gi] = { ...asDatum(series[si].data[gi]),
                                  label: { show: true, formatter: fmt(tot) } };
          break;
        }
      }
    }

    return endRoom(ends, valAxis, o.vertical, o.plotPx, textW);
  };

  /**
   * `label` on a bar (model labelOf). A plain or grouped bar writes the text
   * at its end, before its value. A stack or 100% bar writes it inside each
   * segment it fits in, from a label-only series. Mutates `series` and
   * `valAxis`; returns the px a grid needs on its right.
   * @param {any[]} series the bar series, one per colour level (or one)
   * @param {any} valAxis
   * @param {{ textAt: (si: number, gi: number) => string | null, groups: string[],
   *           nBars: number, vertical: boolean, stack: boolean, percent: boolean,
   *           valueLabels: boolean, valuePx: number, bandPx: number, ink: any,
   *           measure: (s: string) => number }} o
   * @returns {number}
   */
  const barMarkLabels = (series, valAxis, o) => {
    const bars = series.slice(0, o.nBars);
    const style = labelStyle(o.ink);

    if (o.stack) {
      const { perPx } = stackScale(bars, o.groups.length, o.valuePx);
      const barW = Math.min(BAR_MAX, 0.6 * (o.bandPx || BAR_MAX / 0.6));
      /** @type {any[]} */
      const data = [];
      for (let gi = 0; gi < o.groups.length; gi++) {
        for (const { si, d, v, at } of segmentsAt(bars, gi)) {
          const t = o.textAt(si, gi);
          if (t == null) continue;
          const shown = (o.percent && o.valueLabels) ? t + ' · ' + Math.round(v * 100) + '%' : t;
          const w = o.measure(shown);
          const len = perPx ? Math.abs(v) / perPx : Infinity;
          const fits = o.vertical ? len >= o.ink.fontSize + 4 && barW >= w + 4 : len >= w + 6;
          if (!fits) continue;
          if (o.percent && o.valueLabels) bars[si].data[gi] = { ...asDatum(d), label: { show: false } };
          data.push(o.vertical ? [o.groups[gi], at, shown] : [at, o.groups[gi], shown]);
        }
      }
      if (data.length) {
        series.push({
          id: '__label__', type: 'scatter', data, silent: true, symbolSize: 0, z: 11,
          legendHoverLink: false, animation: false, tooltip: { show: false },
          emphasis: { disabled: true },
          label: { show: true, position: 'inside', ...style, color: '#fff',
                   formatter: (/** @type {any} */ p) => p.value[2] },
          labelLayout: { hideOverlap: true }
        });
      }
      return 0;
    }

    /** @type {Array<{ v: number, text: string }>} */
    const ends = [];
    bars.forEach((s, si) => {
      const texts = o.groups.map((_, gi) => o.textAt(si, gi));
      if (!texts.some((t) => t != null)) return;
      const vf = (o.valueLabels && s.label && typeof s.label.formatter === 'function')
        ? s.label.formatter : null;
      /** @param {any} p */
      const formatter = (p) => {
        const t = texts[p.dataIndex];
        const v = vf ? vf(p) : '';
        return t != null && v ? t + ' · ' + v : (t != null ? t : v);
      };
      s.label = { position: 'outside', distance: 4, ...style, ...(s.label || {}), show: true, formatter };
      s.labelLayout = { hideOverlap: true };
      s.data.forEach((/** @type {any} */ d, /** @type {number} */ gi) => {
        const v = valOf(d);
        if (v != null) ends.push({ v: Number(v), text: formatter({ value: v, dataIndex: gi }) });
      });
    });
    if (!ends.length) return 0;
    return endRoom(ends, valAxis, o.vertical, o.valuePx, o.measure);
  };

  /**
   * `legend: "marks"` on a stacked or 100% bar: each series' name at the
   * middle of its segment, beside the last bar (vertical) or above the first
   * (horizontal), as label-only series. Names of thin segments are spread a
   * line apart. Appends to `series`; returns the px a vertical grid needs on
   * its right for the longest name.
   * @param {any[]} series the bar series, one per level
   * @param {{ vertical: boolean, groups: string[], bandPx: number, barPx: number,
   *           valuePx: number, measure: (s: string) => number, ink: any }} o
   *   `valuePx`: the value axis's length in px, to keep names a line apart
   * @returns {number}
   */
  const stackNames = (series, o) => {
    const n = o.groups.length;
    // The bar to name: the last (or first) one with a segment.
    let gi = o.vertical ? n - 1 : 0;
    while (gi >= 0 && gi < n && !segmentsAt(series, gi).length) gi += o.vertical ? -1 : 1;
    if (gi < 0 || gi >= n) return 0;
    const { top, perPx } = stackScale(series, n, o.valuePx);
    // A missing level has no name to write.
    const items = segmentsAt(series, gi)
      .map(({ si, at }) => ({ si, at, name: String(series[si].name) }))
      .filter((it) => it.name)
      .map((it) => ({ ...it, w: o.measure(it.name) }));
    // Thin segments put names closer than a line: spread them, each at least
    // a line (a name's width) from the next, and keep the run on the axis.
    if (perPx) {
      const fs = o.ink.fontSize || 12;
      if (o.vertical) {
        items.sort((x, y) => y.at - x.at);
        const gap = (fs + 3) * perPx;
        for (let k = 1; k < items.length; k++) items[k].at = Math.min(items[k].at, items[k - 1].at - gap);
        const low = items.length ? items[items.length - 1].at : 0;
        if (low < gap / 2) for (const it of items) it.at += gap / 2 - low;
      } else {
        items.sort((x, y) => x.at - y.at);
        for (let k = 1; k < items.length; k++) {
          const gap = ((items[k - 1].w + items[k].w) / 2 + 8) * perPx;
          items[k].at = Math.max(items[k].at, items[k - 1].at + gap);
        }
        const high = items.length ? items[items.length - 1].at + items[items.length - 1].w / 2 * perPx : 0;
        if (high > top) for (const it of items) it.at -= high - top;
      }
    }
    let widest = 0;
    items.sort((x, y) => x.si - y.si);
    for (const it of items) {
      widest = Math.max(widest, it.w);
      series.push(o.vertical
        ? NS.nameMark('__legend_' + it.si, it.name, [o.groups[gi], it.at], 'right', o.ink,
                      { distance: o.barPx / 2 + 6 })
        : NS.nameMark('__legend_' + it.si, it.name, [it.at, o.groups[gi]], 'top', o.ink,
                      { distance: o.barPx / 2 + 3 }));
    }
    if (!o.vertical) return 0;
    // The label starts beside the bar and may reach past the plot.
    return Math.max(0, Math.ceil(widest + 6 + o.barPx / 2 + 4 - o.bandPx / 2));
  };

  /** The x labels of a vertical category axis fitted to the plot, with the `xFit` inputs a resize
   * re-fits them from. @param {any[]} labels @param {number} plotW @param {any} look
   * @param {any} display maps a category to its label (or null) @param {any} [decorate] */
  const fitXLabels = (labels, plotW, look, display, decorate) => {
    const xlab = NS.axes.xAxisLabels(labels, (plotW || 0) - X_INSET, false, look.measure, look.ink);
    return { xlab, xFit: { labels, inset: X_INSET, decimate: false, gutter: xlab.bottom,
                           key: xlab.key, formatter: display, decorate } };
  };

  /** An axis label that sets the names in `lit` in semibold text ink, line by line. A name holding
   * rich-text syntax stays as it is. @param {any} axisLabel @param {Set<string>} lit
   * @param {any} ink */
  const boldNames = (axisLabel, lit, ink) => {
    const base = axisLabel || {};
    const fmt = base.formatter;
    return {
      ...base,
      formatter: (/** @type {any} */ v, /** @type {number} */ i) => {
        const t = String(typeof fmt === 'function' ? fmt(v, i) : v);
        if (!lit.has(String(v)) || /[{}|]/.test(t)) return t;
        return t.split('\n').map((l) => '{hl|' + l + '}').join('\n');
      },
      rich: { ...(base.rich || {}),
              hl: { fontWeight: 600, color: ink.text, fontSize: base.fontSize || ink.fontSize } }
    };
  };

  option.barMarkParts = {
    barRadius, barValueLabels, barMarkLabels, stackNames, boldNames, endFormat, WATERFALL_COLORS,
    asDatum, withItemStyle, labelStyle, fitXLabels,
    BAR: { BAR_MAX, ROW_BAND, GROUP_BAR, GROUP_PAD, X_INSET }
  };
})();
// --- chart/option-bar.js
// @ts-check
/**
 * Chart block: the bar and waterfall options. One panel of the aggregated
 * model in; its ECharts option, panel height, x-label fit inputs and
 * thinned-label note out.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const option = /** @type {any} */ (NS.option = NS.option || {});
  const { barRadius, barValueLabels, barMarkLabels, stackNames, boldNames, endFormat,
          WATERFALL_COLORS, withItemStyle, fitXLabels } = option.barMarkParts;
  const { BAR_MAX, ROW_BAND, GROUP_BAR, GROUP_PAD, X_INSET } = option.barMarkParts.BAR;

  /** What every part of a bar's option reads: groups, colour levels, layout and colours.
   * @param {any} m @param {any} panel @param {any} look @param {number} plotW */
  const barContext = (m, panel, look, plotW) => {
    const { cfg, ink } = look;
    const groups = panel.groups;
    const colors = m.colors;
    const palette = NS.paletteOf(look.derived);
    const isGrouped = cfg.position === 'dodge';
    const isPercent = cfg.position === 'fill';
    const vertical = cfg.orientation === 'vertical';
    // Horizontal: a fixed row band; past the panel cap a grouped fan
    // squeezes into its band. Vertical: bands come from the width.
    const rowBand = isGrouped && colors.length > 0 ? colors.length * GROUP_BAR + GROUP_PAD : ROW_BAND;
    const rowsH = groups.length * rowBand;
    const hCapped = !vertical && rowsH > NS.PANEL_H_CAP;
    // `highlight`: a bar without a highlighted row is grey.
    const hlLit = m.hlLit;
    const muted = NS.mutedMark(ink);
    const levelColor = m.hlColors
      ? NS.litLevelColors(colors, m.hlColors, m.colorScale, palette)
      : NS.levelColors(colors, m.colorScale, palette);
    /** @param {any} v @param {string} g @param {string | null} lv */
    const greyed = (v, g, lv) => (v == null || !hlLit || hlLit(panel.facet, g, lv)) ? v
      : withItemStyle(v, { color: muted });
    // A share needs a colour split; pct_distinct is a share of the panel.
    const showPercent = isPercent && colors.length > 0;
    const pctFunc = cfg.func === 'pct_distinct';
    const bandPx = vertical ? (plotW ? (plotW - X_INSET) / Math.max(1, groups.length) : 0)
      : (hCapped ? NS.PANEL_H_CAP / Math.max(1, groups.length) : ROW_BAND);
    return { m, panel, look, cfg, ink, plotW, groups, colors, palette, isGrouped, isPercent,
             vertical, rowsH, hCapped, hlLit, muted, levelColor, greyed, showPercent, pctFunc,
             asFraction: showPercent || pctFunc, bandPx };
  };

  /** The bar series: one, or one per colour level. @param {any} k */
  const barSeries = (k) => {
    const { m, panel, groups, colors, vertical, isGrouped, isPercent, greyed } = k;
    if (colors.length === 0) {
      // A null aggregate stays null: a gap, not a zero bar.
      return [{
        type: 'bar',
        data: groups.map((/** @type {string} */ g) => {
          const d = NS.model.cellOf(m, panel, g);
          return greyed(d ? d.value : null, g, null);
        }),
        itemStyle: { color: k.palette[0], borderRadius: barRadius(vertical) },
        barWidth: '60%', barMaxWidth: BAR_MAX, emphasis: { focus: 'self' }
      }];
    }
    const barLayout = isGrouped
      ? (vertical
          ? { barGap: 0, barCategoryGap: '30%', barMaxWidth: BAR_MAX }
          : (k.hCapped ? { barGap: 0, barCategoryGap: '20%' } : { barGap: 0, barWidth: GROUP_BAR }))
      : { barWidth: '60%', barMaxWidth: BAR_MAX };
    /** @type {Record<string, number>} */
    const groupTotals = {};
    if (isPercent) {
      for (const g of groups) {
        groupTotals[g] = NS.model.cellsOf(m, panel, g)
          .filter((/** @type {any} */ a) => a.value != null)
          .reduce((/** @type {number} */ s, /** @type {any} */ a) => s + a.value, 0);
      }
    }
    const series = colors.map((/** @type {string} */ color, /** @type {number} */ ci) => ({
      type: 'bar', name: color,
      // null for a cell without rows, so a stack skips it; a percent datum
      // keeps its raw value for the tooltip.
      data: groups.map((/** @type {string} */ g) => {
        const d = NS.model.cellOf(m, panel, g, color);
        const raw = d ? d.value : null;
        if (raw == null) return null;
        if (isPercent) {
          const tot = groupTotals[g];
          return greyed(tot ? { value: raw / tot, raw } : null, g, color);
        }
        return greyed(raw, g, color);
      }),
      ...(isGrouped ? {} : { stack: 'stack' }),
      itemStyle: { color: k.levelColor[ci] || k.muted },
      ...barLayout,
      emphasis: { focus: 'self' }
    }));
    // Round the value end: every grouped bar, and in a stack the outermost
    // segment of each group.
    if (isGrouped) {
      for (const s of series) s.itemStyle.borderRadius = barRadius(vertical);
      return series;
    }
    for (let gi = 0; gi < groups.length; gi++) {
      const top = series.findLastIndex((/** @type {any} */ s) => s.data[gi] != null);
      if (top >= 0) {
        series[top].data[gi] = withItemStyle(series[top].data[gi],
                                             { borderRadius: barRadius(vertical) });
      }
    }
    return series;
  };

  /** The groups with their "(n)" count, and the formatter that adds it.
   * @param {any} panel */
  const countLabels = (panel) => {
    const counts = panel.axisCounts;
    return {
      catLabels: counts ? panel.groups.map((/** @type {string} */ g) => NS.withCount(g, counts))
        : panel.groups,
      catFmt: counts ? (/** @type {any} */ v) => NS.withCount(v, counts) : null
    };
  };

  /** The category axis. "(n)" comes from a formatter, so the axis data stays the bare group clicks
   * and tooltips key off. A highlighted bar's name is semibold; `decorate` reapplies that after a
   * resize. @param {any} k */
  const barCategoryAxis = (k) => {
    const { panel, groups, colors, vertical, hlLit, ink, look } = k;
    const { catLabels, catFmt } = countLabels(panel);
    const gut = NS.axes.yGutter(catLabels, look.measure);
    /** @type {((axisLabel: any) => any) | null} */
    let decorate = null;
    if (hlLit) {
      const lit = new Set(groups.filter((/** @type {string} */ g) => (colors.length
        ? colors.some((/** @type {string} */ c) => hlLit(panel.facet, g, c))
        : hlLit(panel.facet, g, null))));
      decorate = (axisLabel) => boldNames(axisLabel, lit, ink);
    }
    const fit = vertical ? fitXLabels(catLabels, k.plotW, look, catFmt, decorate) : null;
    const xlab = fit && fit.xlab;
    const axis = NS.axes.categoryAxis(groups, ink, { xlab, display: catFmt, gut });
    if (decorate) axis.axisLabel = decorate(axis.axisLabel);
    return { axis, catLabels, gut, xlab, xFit: fit && fit.xFit };
  };

  /** The value axis: ticks in `format`, the unit once in the title. @param {any} k @param {any} P
   */
  const barValueAxis = (k, P) => {
    const { cfg, asFraction, showPercent, pctFunc, vertical, ink } = k;
    // "% of panel" names the column its denominator is taken over.
    const ofCol = /** @type {Record<string, any>} */ (
      { facet: cfg.facet, category: cfg.category, color: cfg.color })[cfg.percent_of];
    const name = showPercent ? '% of group total'
      : (pctFunc ? '% of ' + (ofCol ? P.title(ofCol) : 'total')
        : NS.axisName(P.valueTitle, cfg.measure, cfg, k.look.derived));
    const format = asFraction ? (/** @type {number} */ v) => Math.round(v * 100) + '%'
      : NS.valueFormat(cfg, { unit: false });
    return {
      ...NS.axes.valueAxis(ink, 'value', name, vertical ? 45 : 30, { format, grid: 'dashed' }),
      ...(asFraction ? { max: 1 } : {})
    };
  };

  /** Value labels, mark text, reference lines and ranges and stack level names. Returns the px each
   * needs on the grid's right. @param {any} k @param {any[]} series @param {any} valAxis
   * @param {any} cat */
  const barSides = (k, series, valAxis, cat) => {
    const { m, panel, cfg, ink, groups, colors, vertical, isGrouped, isPercent } = k;
    const { measure } = k.look;
    const valueLabels = cfg.value_labels === true;
    const valuePx = vertical ? 350 - 30 - 66 : (k.plotW ? k.plotW - cat.gut.gridLeft - 5 : 0);
    const labelRight = valueLabels
      ? barValueLabels(series, valAxis, {
          ink, vertical, stacked: !isGrouped && !isPercent && colors.length > 0,
          percent: k.showPercent, fraction: k.pctFunc, plotPx: valuePx,
          bandPx: vertical ? k.bandPx : ROW_BAND, measure, cfg
        })
      : 0;
    const markRight = m.markLabels
      ? barMarkLabels(series, valAxis, {
          textAt: (/** @type {number} */ si, /** @type {number} */ gi) =>
            NS.model.labelOf(m, panel, groups[gi], colors.length ? colors[si] : null),
          groups, nBars: Math.max(1, colors.length), vertical,
          stack: !isGrouped && colors.length > 0, percent: k.showPercent,
          valueLabels, valuePx, bandPx: k.bandPx, ink, measure
        })
      : 0;
    NS.axes.addValueLines(series, valAxis, !vertical, k.asFraction, cfg.value_lines, ink);
    // A histogram's bins are ranges, not categories to name.
    if (!m.histogram) {
      NS.axes.addCategoryMarks(series, vertical, groups, cfg.x_lines, cfg.x_ranges, ink);
    }
    // `legend: "marks"`: after the series the tooltip and highlight read.
    let namesRight = 0;
    const marks = NS.legendPlan(m, cfg).marks;
    if (marks === 'stack' || marks === 'stack-h') {
      const bandPx = k.bandPx;
      const barPx = vertical ? (bandPx ? Math.min(BAR_MAX, 0.6 * bandPx) : BAR_MAX / 2)
        : Math.min(BAR_MAX, 0.6 * bandPx);
      namesRight = stackNames(series, { vertical, groups, bandPx, barPx, valuePx, measure, ink });
    }
    return { labelRight, markRight, namesRight };
  };

  /** The tooltip. A split bar's card lists every level of the hovered category with the hovered
   * segment marked (the item trigger knows which). A 100% bar shows each level's share, its value
   * after it; any other bar the value named by its aggregation, with n, and the total only where
   * segments add up (a count, a sum). @param {any} k @param {any} P @param {any[]} series */
  const barTooltip = (k, P, series) => {
    const { m, panel, cfg, colors, showPercent, pctFunc } = k;
    const { title, aggLabel } = P;
    const esc = V.util.esc;
    const split = colors.length > 0;
    const vf = k.asFraction ? null : NS.valueFormat(cfg);
    const fmtRaw = vf || ((/** @type {number} */ n) =>
      Number.isInteger(n) ? n : Math.round(n * 100) / 100);
    /** @type {Record<string, number>} */
    const nOf = {};
    for (const a of panel.cells) nOf[a.group + '|||' + a.color] = a.n;
    const showN = cfg.func !== 'count' && cfg.func !== 'identity';
    const additive = cfg.func === 'count' || cfg.func === 'sum';
    const colorName = split ? esc(title(cfg.color) || cfg.color) + ' ' : '';
    /** The rows of the hovered category: one per level, else the hovered bar. */
    const levelsAt = (/** @type {any} */ arg) => {
      const p = Array.isArray(arg) ? arg[0] : arg;
      if (!p) return { ps: [], hit: null };
      if (!split) return { ps: [].concat(arg), hit: null };
      const ps = series.slice(0, colors.length).map((sr) => {
        const d = sr.data[p.dataIndex];
        const obj = d != null && typeof d === 'object';
        return { name: p.name, seriesName: sr.name, value: obj ? d.value : d,
                 data: obj ? d : null, color: sr.itemStyle.color };
      });
      return { ps, hit: p.seriesName };
    };
    const extraRows = (/** @type {any} */ head) => {
      const extra = panel.ttRep ? panel.ttRep[head] : null;
      if (!extra) return '';
      return m.ttFields.map((/** @type {string} */ c, /** @type {number} */ i) =>
        (extra[i] != null && extra[i] !== '') ? V.tooltip.row(esc(title(c) || c), esc(extra[i])) : ''
      ).join('');
    };
    /** @param {any} p @param {string} head */
    const valueText = (p, head) => {
      if (showPercent) {
        const raw = p.data && p.data.raw != null ? ' (' + fmtRaw(p.data.raw) + ')' : '';
        return Math.round((Number(p.value) || 0) * 100) + '%' + raw;
      }
      const n = nOf[head + '|||' + (split ? p.seriesName : '__all__')];
      if (pctFunc) return Math.round(Number(p.value) * 1000) / 10 + '%' + (n != null ? ' (' + n + ')' : '');
      return fmtRaw(Number(p.value)) + (showN && n != null ? ' (n=' + n + ')' : '');
    };
    const formatter = (/** @type {any} */ arg) => {
      const { ps, hit } = levelsAt(arg);
      if (!ps.length) return '';
      const head = /** @type {any} */ (ps[0]).axisValueLabel || ps[0].name || '';
      const vals = ps.filter((/** @type {any} */ p) => p && p.value != null);
      const rows = vals.map((/** @type {any} */ p) => V.tooltip.row(
        split ? colorName + esc(p.seriesName) : esc(aggLabel), valueText(p, head),
        split ? p.color : null, hit == null ? null : p.seriesName === hit)).join('');
      const total = split && !showPercent && additive && vals.length > 1
        ? V.tooltip.SEP + V.tooltip.row(esc(aggLabel), fmtRaw(vals.reduce(
          (/** @type {number} */ a, /** @type {any} */ p) => a + Number(p.value || 0), 0)))
        : '';
      const note = split && !showPercent && !additive ? V.tooltip.note(esc(aggLabel)) : '';
      return V.tooltip.head(esc(head)) + note + rows + total + extraRows(head);
    };
    // An unsplit bar answers anywhere over its category; the pointer stays
    // invisible, as hover marks nothing.
    return split ? { trigger: 'item', confine: true, formatter }
      : { trigger: 'axis', axisPointer: { type: 'none' }, confine: true, formatter };
  };

  /** A plain, stacked, grouped or percent bar. @param {any} m @param {any} panel @param {any} look
   * @param {number} plotW */
  const bar = (m, panel, look, plotW) => {
    const k = barContext(m, panel, look, plotW);
    const P = option.aggregatedCommon(look);
    const { groups, colors, vertical } = k;
    const series = barSeries(k);
    const cat = barCategoryAxis(k);
    const valAxis = barValueAxis(k, P);
    // A histogram's bins touch.
    if (m.histogram) {
      for (const s of series) {
        if (s.type !== 'bar') continue;
        delete s.barWidth;
        delete s.barMaxWidth;
        s.barCategoryGap = '3%';
        s.barGap = '0%';
      }
    }
    const side = barSides(k, series, valAxis, cat);
    const { xlab, gut } = cat;
    const bottomBase = vertical ? 40 + 26 + (xlab ? xlab.bottom : 0) : 20 + 26;
    return {
      xFit: cat.xFit || undefined,
      // Horizontal: a fixed band per row. Vertical: canvas plus label gutter.
      panelH: vertical ? NS.panelH(k.cfg, 350) + (xlab ? xlab.bottom : 0)
        : 30 + Math.min(NS.PANEL_H_CAP, k.rowsH) + bottomBase,
      labelNote: k.hCapped
        ? NS.axes.thinnedLabelNote(NS.PANEL_H_CAP, groups.length, P.title(k.cfg.category)) : null,
      option: {
        ...P.base,
        tooltip: barTooltip(k, P, series),
        // A hidden legend keeps the selection model the HTML chips toggle.
        legend: colors.length > 0 ? { show: false, data: colors } : undefined,
        grid: vertical
          ? { left: 55, right: 10 + Math.max(side.labelRight, side.markRight, side.namesRight),
              top: 30, bottom: bottomBase }
          : { left: gut.gridLeft, right: 5 + Math.max(side.labelRight, side.markRight), top: 30,
              bottom: bottomBase },
        xAxis: vertical ? cat.axis : valAxis,
        yAxis: vertical ? valAxis : cat.axis,
        series
      }
    };
  };

  /**
   * A waterfall's offset and step data, coloured by sign; a total step
   * restates the running total from zero. A step is summed across a colour
   * split; one without a value is a gap. `lo` and `hi` bound the bridge.
   * @param {any} m @param {any} panel @param {Record<string, any>} cfg
   * @param {boolean} labelsOn @param {((v: number) => string) | null} wvf
   */
  const waterfallSteps = (m, panel, cfg, labelsOn, wvf) => {
    /** @param {string} g */
    const valOf = (g) => {
      const vals = NS.model.cellsOf(m, panel, g)
        .filter((/** @type {any} */ a) => a.value != null).map((/** @type {any} */ a) => a.value);
      return vals.length ? vals.reduce((/** @type {number} */ s, /** @type {number} */ v) => s + v, 0)
        : null;
    };
    const totals = new Set(NS.labelled(cfg.waterfall_totals).map((/** @type {{ value: any }} */ x) => String(x.value)));
    /** @type {any[]} */
    const base = [];
    /** @type {any[]} */
    const delta = [];
    let cum = 0;
    const fmt = endFormat(cfg, cfg.func === 'pct_distinct');
    /** @param {number} v @param {boolean} up */
    const lab = (v, up) => labelsOn
      ? { label: { show: true, position: up ? 'top' : 'bottom', formatter: fmt(v) } } : {};
    /** @param {number} offset @param {number} value @param {string} color
     * @param {boolean} floating @param {number} shown @param {boolean} up */
    const push = (offset, value, color, floating, shown, up) => {
      base.push(offset);
      delta.push({ value, itemStyle: { color, borderRadius: barRadius(true, floating) },
                   ...lab(shown, up) });
    };
    let lo = 0, hi = 0;
    for (const g of panel.groups) {
      const v = valOf(g);
      if (totals.has(g)) {
        push(0, cum, WATERFALL_COLORS.total, false, cum, cum >= 0);
      } else if (v == null) {
        base.push(0);
        delta.push(null);
        continue;
      } else if (v >= 0) {
        push(cum, v, WATERFALL_COLORS.increase, cum !== 0, v, true);
        cum += v;
      } else {
        push(cum + v, -v, WATERFALL_COLORS.decrease, cum + v !== 0, v, false);
        cum += v;
      }
      lo = Math.min(lo, cum);
      hi = Math.max(hi, cum);
    }
    return { base, delta, lo, hi };
  };

  /** A waterfall: a transparent base series carries each step's offset, a visible delta series the
   * step. Vertical only. @param {any} m @param {any} panel @param {any} look @param {number} plotW
   */
  const waterfall = (m, panel, look, plotW) => {
    const { cfg, ink, measure } = look;
    const P = option.aggregatedCommon(look);
    const labelsOn = cfg.value_labels === true;
    const wvf = cfg.func === 'pct_distinct' ? null : NS.valueFormat(cfg);
    const { base, delta, lo, hi } = waterfallSteps(m, panel, cfg, labelsOn, wvf);

    const { catLabels, catFmt } = countLabels(panel);
    const { xlab, xFit } = fitXLabels(catLabels, plotW, look, catFmt);
    const catAxis = { ...NS.axes.categoryAxis(panel.groups, ink, { xlab, display: catFmt }),
                      splitLine: { show: false } };
    const valAxis = NS.axes.valueAxis(ink, 'value', NS.axisName(P.valueTitle, cfg.measure, cfg, look.derived), 45, {
      format: wvf && NS.valueFormat(cfg, { unit: false }), grid: 'dashed' });
    const label = { show: false, distance: 4, color: ink.muted, fontSize: ink.fontSize,
                    fontFamily: ink.face };
    /** @type {any[]} */
    const wfSeries = [
      { name: 'base', type: 'bar', stack: 'waterfall', barWidth: '60%',
        itemStyle: { color: 'transparent', borderColor: 'transparent' },
        emphasis: { disabled: true }, silent: true, tooltip: { show: false }, data: base },
      { name: 'delta', type: 'bar', stack: 'waterfall', barWidth: '60%',
        emphasis: { focus: 'self' }, data: delta,
        ...(labelsOn ? { label, labelLayout: { hideOverlap: true } } : {}) }
    ];
    let labelRight = 0;
    if (labelsOn) {
      valAxis.boundaryGap = [lo < 0 ? '12%' : 0, hi > 0 ? '12%' : 0];
      let w = 0;
      for (const d of delta) if (d && d.label) w = Math.max(w, measure(d.label.formatter));
      labelRight = Math.max(0, Math.ceil(w / 2 - 10));
    }
    NS.axes.addValueLines(wfSeries, valAxis, false, false, cfg.value_lines, ink);
    return {
      panelH: NS.panelH(cfg, 350) + xlab.bottom,
      xFit,
      labelNote: null,
      option: {
        ...P.base,
        tooltip: {
          trigger: 'axis', axisPointer: { type: 'none' }, confine: true,
          // Only the visible delta; the base is how the bar floats.
          formatter: (/** @type {any} */ params) => {
            const p = (params || []).find((/** @type {any} */ x) => x.seriesName === 'delta');
            if (!p) return '';
            return V.tooltip.head(V.util.esc(p.name)) + V.tooltip.row(V.util.esc(P.aggLabel),
              p.value == null ? '–' : (wvf ? wvf(p.value) : V.util.num(p.value)));
          }
        },
        legend: { show: false },
        grid: { left: 55, right: 10 + labelRight, top: 30, bottom: 40 + 26 + xlab.bottom },
        xAxis: catAxis,
        yAxis: valAxis,
        series: wfSeries
      }
    };
  };

  option.barParts = { bar, waterfall };
})();
// --- chart/option-radial.js
// @ts-check
/**
 * Chart block: the pie and treemap options. Neither has a category axis;
 * each draws on the default 350 px canvas.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const option = /** @type {any} */ (NS.option = NS.option || {});

  /** A slice or tile per group: its total, the rows behind it, its colour and `label` text. Groups
   * with nothing to show drop out. @param {any} m @param {any} panel
   * @param {ChartSettings} cfg @param {ChartDerived} derived */
  const groupTotals = (m, panel, cfg, derived) => {
    const gScale = NS.scaleFor(derived, cfg.category);
    const palette = NS.paletteOf(derived);
    return panel.groups.map((/** @type {string} */ g, /** @type {number} */ i) => {
      const cells = NS.model.cellsOf(m, panel, g);
      const total = cells.reduce((/** @type {number} */ s, /** @type {any} */ a) => s + (a.value ?? 0), 0);
      const n = cells.reduce((/** @type {number} */ s, /** @type {any} */ a) => s + (a.n || 0), 0);
      const text = NS.model.labelOf(m, panel, g);
      return { name: g, value: total, n, ...(text != null ? { text } : {}),
               itemStyle: { color: (gScale && gScale.color && gScale.color[g]) ||
                 NS.paletteAt(palette, i) } };
    }).filter((/** @type {any} */ d) => d.value > 0);
  };

  /** The text a mark shows: its `label` column's value, else its name. @param {any} p */
  const markText = (p) => (p.data && p.data.text != null ? p.data.text : p.name);

  /** The builder of a pie (`pie` true) or a treemap. @param {boolean} pie */
  const builder = (pie) => (/** @type {any} */ m, /** @type {any} */ panel,
    /** @type {any} */ look) => {
    const { cfg, ink } = look;
    const P = option.aggregatedCommon(look);
    const data = groupTotals(m, panel, cfg, look.derived);
    const label = { show: true, fontSize: ink.fontSize };
    const shadow = (/** @type {number} */ a) =>
      ({ itemStyle: { shadowBlur: 10, shadowColor: 'rgba(0,0,0,' + a + ')' } });
    return {
      option: {
        ...P.base,
        tooltip: { trigger: 'item', confine: true,
                   formatter: (/** @type {any} */ p) => NS.rowTooltip(p.name,
                     P.aggPairs(p.value, p.data && p.data.n, pie ? p.percent : undefined),
                     p.color) },
        series: [pie
          ? { type: 'pie', radius: ['30%', '70%'], data,
              label: { ...label, formatter: data.some((/** @type {any} */ d) => d.text != null)
                ? markText : '{b}' },
              emphasis: shadow(0.2) }
          : { type: 'treemap', data, left: 10, right: 10, top: 2, bottom: 2, roam: false,
              nodeClick: false, breadcrumb: { show: false },
              label: { ...label,
                       formatter: (/** @type {any} */ p) =>
                         markText(p) + '\n' + V.util.num(Number(p.value)) },
              itemStyle: { borderColor: ink.surface, borderWidth: 2, gapWidth: 2 },
              emphasis: shadow(0.15) }]
      }
    };
  };

  option.radialParts = { pie: builder(true), treemap: builder(false) };
})();
// --- chart/option-distribution.js
// @ts-check
/**
 * Chart block: the boxplot and point range options. The model has already
 * summarised each slot (a group, or group and colour level); this draws a box
 * or a centre dot on an interval per slot. A colour split gives each level
 * its own series, and a split boxplot its own hidden category axis on which
 * it draws only its own slots.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const option = /** @type {any} */ (NS.option = NS.option || {});

  /** The interval of a point range or an error bar as a custom series: a 1.5px line from lo to
   * hi without caps, as the box's whiskers. `encode` maps both ends to the value axis. @param {any} name @param {any[]} pts [slot, lo, hi] each
   * @param {string} clr @param {boolean} horizontal */
  const whiskerSeries = (name, pts, clr, horizontal) => ({
    type: 'custom',
    name,
    legendHoverLink: false,
    silent: true,
    z: 1,
    data: pts,
    encode: horizontal ? { y: 0, x: [1, 2] } : { x: 0, y: [1, 2] },
    renderItem: (/** @type {any} */ params, /** @type {any} */ api) => {
      const base = api.value(0), lo = api.value(1), hi = api.value(2);
      const pLo = horizontal ? api.coord([lo, base]) : api.coord([base, lo]);
      const pHi = horizontal ? api.coord([hi, base]) : api.coord([base, hi]);
      const st = api.style ? api.style() : null;
      const opacity = st && st.opacity != null ? st.opacity : 1;
      return {
        type: 'line',
        shape: { x1: pLo[0], y1: pLo[1], x2: pHi[0], y2: pHi[1] },
        style: { stroke: clr, lineWidth: 1.5, opacity }
      };
    }
  });

  /** What every part of a distribution's option reads.
   * @param {any} m @param {any} panel @param {any} look @param {number} plotW
   */
  const distributionContext = (m, panel, look, plotW) => {
    const { cfg } = look;
    const P = option.aggregatedCommon(look);
    const isBox = cfg.type === 'boxplot';
    const palette = NS.paletteOf(look.derived);
    const split = m.split;
    const scale = m.colorScale;
    const hexFor = (/** @type {string} */ lv, /** @type {number} */ i) =>
      (scale && scale.color && scale.color[lv]) || NS.paletteAt(palette, i);
    const slots = panel.slots;
    /** One entry per series level: its name (split only) and colour. */
    const levelInfo = (split ? m.levels : ['__all__']).map(
      (/** @type {string} */ lv, /** @type {number} */ li) =>
        ({ lv, li, name: split ? lv : undefined, hex: split ? hexFor(lv, li) : palette[0] }));
    return {
      m, look, cfg, ink: look.ink, plotW, P, isBox, split, hexFor, slots, levelInfo,
      SEP: NS.model.BOX_CAT_SEP,
      vertical: cfg.orientation !== 'horizontal',
      bodyMeta: NS.agg.statMeta(cfg.summary || (isBox ? 'median_q1_q3' : 'mean_se')),
      whiskMeta: NS.agg.statMeta(cfg.whiskers || 'tukey'),
      levels: m.levels,
      cats: slots.map((/** @type {any} */ s) => s.cat),
      axisOn: NS.countOn(cfg, 'axis')
    };
  };

  /** The slots of a level that drew a body, with their category position. @param {any} k
   * @param {string} lv @returns {Array<{ s: any, ci: number }>} */
  const drawnSlots = (k, lv) => k.slots
    .map((/** @type {any} */ s, /** @type {number} */ ci) => ({ s, ci }))
    .filter((/** @type {any} */ e) => e.s.level === lv && e.s.body);

  // One datum per category, so the datum index is the category index. An empty slot is NaNs.
  const emptyBox = (/** @type {number} */ ci) => ({ value: [ci, NaN, NaN, NaN, NaN, NaN], n: 0 });

  /** The box of one slot: a solid body in the level's colour, the median cut out of it in the
   * surface colour, whiskers without caps. ECharts' own boxplot draws body outline, whiskers and
   * median in one colour, so a solid body would hide the median; this draws the parts apart.
   * The datum is [category, whisker lo, body lo, centre, body hi, whisker hi]. @param {string} clr
   * @param {string} cut @param {boolean} horizontal */
  const boxRender = (clr, cut, horizontal) => (/** @type {any} */ params, /** @type {any} */ api) => {
    const ci = api.value(0);
    const v = [1, 2, 3, 4, 5].map((i) => api.value(i));
    if (v.some((x) => !Number.isFinite(x))) return null;
    const at = (/** @type {number} */ x) => horizontal ? api.coord([x, ci]) : api.coord([ci, x]);
    const band = horizontal ? api.size([0, 1])[1] : api.size([1, 0])[0];
    // The bounds of ECharts' boxWidth default, [7, 50].
    const half = Math.max(7, Math.min(50, band * 0.5)) / 2;
    const st = api.style ? api.style() : null;
    const opacity = st && st.opacity != null ? st.opacity : 1;
    const [wLo, bLo, mid, bHi, wHi] = v.map(at);
    const a = horizontal ? 0 : 1; // the value axis coordinate
    const line = (/** @type {number[]} */ p, /** @type {number[]} */ q, /** @type {string} */ stroke,
                  /** @type {number} */ lineWidth) =>
      ({ type: 'line', shape: { x1: p[0], y1: p[1], x2: q[0], y2: q[1] }, style: { stroke, lineWidth, opacity },
         emphasisDisabled: true });
    const across = (/** @type {number[]} */ p) => horizontal
      ? [[p[0], p[1] - half], [p[0], p[1] + half]] : [[p[0] - half, p[1]], [p[0] + half, p[1]]];
    const lo = Math.min(bLo[a], bHi[a]), len = Math.abs(bHi[a] - bLo[a]);
    const body = horizontal
      ? { x: lo, y: mid[1] - half, width: len, height: 2 * half }
      : { x: mid[0] - half, y: lo, width: 2 * half, height: len };
    const [m0, m1] = across(mid);
    // emphasisDisabled: ECharts lightens a hovered custom shape otherwise.
    return {
      type: 'group',
      emphasisDisabled: true,
      children: [
        line(wLo, bLo, clr, 1.5),
        line(bHi, wHi, clr, 1.5),
        // A selected box is outlined (NS.selStyle(): its border, as stroke).
        { type: 'rect', shape: { ...body, r: 1.5 }, emphasisDisabled: true,
          style: { fill: clr, opacity,
                   ...(st && st.lineWidth > 0 ? { stroke: st.stroke, lineWidth: st.lineWidth } : {}) } },
        line(m0, m1, cut, 1.5)
      ]
    };
  };

  /** One box series per level, a custom series (see boxRender). @param {any} k */
  const distBoxSeries = (k) => k.levelInfo.map((/** @type {any} */ { lv, li, name, hex }) => ({
    type: 'custom', name, id: '__box_' + li,
    data: k.slots.map((/** @type {any} */ s, /** @type {number} */ ci) => (s.level === lv && s.body)
      ? { value: [ci, s.whisker.lo, s.body.lo, s.body.center, s.body.hi, s.whisker.hi], n: s.n,
          name: s.cat }
      : emptyBox(ci)),
    encode: k.vertical ? { x: 0, y: [1, 2, 3, 4, 5] } : { y: 0, x: [1, 2, 3, 4, 5] },
    renderItem: boxRender(hex, k.ink.surface, !k.vertical),
    ...(k.split ? (k.vertical ? { xAxisIndex: li } : { yAxisIndex: li }) : {}),
    itemStyle: { color: hex }
  }));

  /** Per level: a silent interval under a clickable centre dot, and a line through the centres with
   * connect_centers, all under the level's name. @param {any} k */
  const distRangeSeries = (k) => {
    const { cfg, cats, vertical } = k;
    /** @type {any[]} */
    const series = [];
    for (const { lv, name, hex } of k.levelInfo) {
      const drawn = drawnSlots(k, lv);
      const centers = drawn.map(({ s: { body: b, n }, ci }) => ({
        value: vertical ? [ci, b.center] : [b.center, ci], name: cats[ci],
        center: b.center, n, lo: b.lo, hi: b.hi }));
      if (cfg.connect_centers === true && centers.length > 1) {
        series.push({
          type: 'line', name, legendHoverLink: false, silent: true,
          z: 2, data: centers.map((c) => c.value), showSymbol: false,
          lineStyle: { color: hex, width: 1.4 }, itemStyle: { color: hex },
          emphasis: { disabled: true }, tooltip: { show: false }
        });
      }
      if (drawn.length) {
        series.push(whiskerSeries(name, drawn.map(({ s, ci }) => [ci, s.body.lo, s.body.hi]),
                                  hex, !vertical));
      }
      series.push({
        // The dot is ringed in the surface colour, as the box's median is cut out of it, and
        // opaque (ECharts draws a scatter at 0.8) so it is the box's full colour.
        type: 'scatter', name, data: centers, z: 4, symbolSize: 9,
        itemStyle: { color: hex, opacity: 1, borderColor: k.ink.surface, borderWidth: 1.5,
                     cursor: 'pointer' },
        emphasis: { disabled: true }
      });
    }
    return series;
  };

  /** `legend: "marks"` on a point range: each level's name at the centre of its centres.
   * @param {any} k */
  const distCentroidMarks = (k) => {
    const { ink, vertical } = k;
    /** @type {any[]} */
    const marks = [];
    for (const { lv, li } of k.levelInfo) {
      const at = drawnSlots(k, lv).map(({ s, ci }) =>
        vertical ? [ci, s.body.center] : [s.body.center, ci]);
      const c = NS.cloudCentre(at);
      if (!c) continue;
      const mark = NS.nameMark('__legend_' + li, lv, c, vertical ? 'top' : 'right', ink,
                               { distance: 8 });
      mark.labelLayout = { moveOverlap: 'shiftY' };
      marks.push(mark);
    }
    return marks;
  };

  /** Outliers on their box's centre line, silent so the box keeps the click. @param {any} k */
  const distOutlierSeries = (k) => {
    const { cfg, isBox, slots, vertical } = k;
    if (!(isBox && cfg.box_points === 'outliers')) return [];
    return k.levelInfo.map((/** @type {any} */ { lv, name, hex }) => {
      /** @type {[number, number][]} */
      const pts = [];
      slots.forEach((/** @type {any} */ s, /** @type {number} */ ci) => {
        if (s.level !== lv) return;
        for (const v of s.outliers) pts.push(vertical ? [ci, v] : [v, ci]);
      });
      return {
        type: 'scatter', name, data: pts, silent: true,
        symbolSize: 5, z: 3, large: true, largeThreshold: 800,
        itemStyle: { color: hex, opacity: 0.85, borderColor: hex, borderWidth: 1 }
      };
    }).filter((/** @type {any} */ s) => s.data.length > 0);
  };

  /** The slots' category labels, gutter, x-label fit and formatter.
   * @param {any} k
   */
  const distCategories = (k) => {
    const { look, plotW, SEP, split, slots, cats, axisOn, vertical } = k;
    /** @type {Map<string, string> | null} */
    const catLabelMap = axisOn ? new Map() : null;
    if (catLabelMap) {
      for (const s of slots) catLabelMap.set(s.cat, s.group + ' (' + s.count + ')');
    }
    const short = (/** @type {string} */ v) => String(v).split(SEP)[0];
    const catLabels = cats.map((/** @type {string} */ c) => catLabelMap
      ? /** @type {string} */ (catLabelMap.get(c)) : (split ? short(c) : c));
    const catFmt = catLabelMap
      ? (/** @type {string} */ v) => catLabelMap.get(v) || short(v)
      : (split ? short : null);
    const fit = vertical ? option.barMarkParts.fitXLabels(catLabels, plotW, look, catFmt) : null;
    return { catLabels, gut: NS.axes.yGutter(catLabels, look.measure), catFmt,
             xlab: fit && fit.xlab, xFit: fit ? fit.xFit : undefined };
  };

  /** The tooltip formatter; the statistics it names are configurable.
   * @param {any} k
   */
  const distTooltip = (k) => {
    const { cfg, P, SEP, isBox, split, hexFor, slots, levels, bodyMeta, whiskMeta } = k;
    const esc = V.util.esc;
    const ttHead = (/** @type {any} */ p) => split ? String(p.name).replace(SEP, ' · ') : p.name;
    // A split lists every level of the group, the hovered one marked.
    const colorName = split ? esc(P.title(cfg.color) || cfg.color) + ' ' : '';
    const span = (/** @type {any} */ lo, /** @type {any} */ hi) =>
      V.util.num(lo) + ' – ' + V.util.num(hi);
    const splitTooltipFmt = (/** @type {any} */ p) => {
      const at = slots.find((/** @type {any} */ s) => s.cat === p.name);
      if (!at || !at.body) return '';
      const rows = slots
        .filter((/** @type {any} */ s) => s.group === at.group && s.body)
        .map((/** @type {any} */ s) => V.tooltip.row(colorName + esc(s.level),
          V.util.num(s.body.center) + ' (' + span(s.body.lo, s.body.hi) + ')',
          hexFor(s.level, levels.indexOf(s.level)), s === at));
      const foot = colorName + esc(at.level) + ' · n ' + at.n +
        (isBox && at.whisker ? ' · Whiskers (' + esc(whiskMeta.range) + ') ' +
          span(at.whisker.lo, at.whisker.hi) : '');
      return V.tooltip.head(esc(at.group)) +
        V.tooltip.note(esc(bodyMeta.center) + ' (' + esc(bodyMeta.range) + ')') +
        rows.join('') + V.tooltip.SEP + V.tooltip.note(foot);
    };
    const pointTooltipFmt = (/** @type {any} */ p) => {
      const d = p.data;
      // n = 0 is an empty slot.
      if (!d || !d.n || (isBox && !Array.isArray(d.value))) return '';
      const h = ttHead(p);
      const lead = (h ? V.tooltip.head(esc(h), split ? p.color : null) : '') + V.tooltip.row('n', d.n);
      if (!isBox) {
        return lead + V.tooltip.row(esc(bodyMeta.center), V.util.num(d.center)) +
          V.tooltip.row(esc(bodyMeta.range), span(d.lo, d.hi));
      }
      // A box datum is [index, wLo, bLo, center, bHi, wHi].
      const five = d.value.slice(-5);
      return lead + V.tooltip.row(esc(bodyMeta.center), V.util.num(five[2])) +
        V.tooltip.row('Box (' + esc(bodyMeta.range) + ')', V.util.num(five[1]) + ', ' + V.util.num(five[3])) +
        V.tooltip.row('Whiskers (' + esc(whiskMeta.range) + ')', span(five[0], five[4]));
    };
    return split ? splitTooltipFmt : pointTooltipFmt;
  };

  /** The value axis and the category axis (one per level on a split boxplot). @param {any} k
   * @param {any} cat the result of `distCategories` */
  const distAxes = (k, cat) => {
    const { cfg, ink, P, isBox, split, cats, vertical, levelInfo } = k;
    // The value axis fits the data rather than holding zero.
    const valAxis = {
      ...NS.axes.valueAxis(ink, 'value', NS.unitTitle(P.title(cfg.measure), cfg),
                           vertical ? 45 : 30, { format: NS.valueFormat(cfg, { unit: false }),
                                                 grid: vertical ? 'dashed' : undefined }),
      scale: true
    };
    const catAxis = NS.axes.categoryAxis(cats, ink,
      { xlab: cat.xlab, display: cat.catFmt, gut: cat.gut, ticks: true });
    const catAxes = (isBox && split)
      ? levelInfo.map((/** @type {any} */ _, /** @type {number} */ i) =>
          (i === 0 ? catAxis : { ...catAxis, show: false }))
      : catAxis;
    return { valAxis, catAxes };
  };

  /**
   * @param {any} m @param {any} panel @param {any} look @param {number} plotW
   */
  const distribution = (m, panel, look, plotW) => {
    if (m.noValue) return null;
    const k = distributionContext(m, panel, look, plotW);
    const { cfg, ink, P, isBox, split, levels, cats, vertical } = k;
    const series = isBox ? distBoxSeries(k) : distRangeSeries(k);
    if (!isBox && split && NS.legendPlan(m, cfg).marks === 'centroid') {
      series.push(...distCentroidMarks(k));
    }
    const pointSeries = distOutlierSeries(k);
    const cat = distCategories(k);
    const { xlab, gut } = cat;
    const bottomBase = 46 + (xlab ? xlab.bottom : 0);
    const { valAxis, catAxes } = distAxes(k, cat);
    NS.axes.addValueLines(series, valAxis, !vertical, false, cfg.value_lines, ink);
    return {
      xFit: cat.xFit,
      // Horizontal: one 28 px row per drawn slot, capped.
      panelH: vertical ? NS.panelH(cfg, 350) + (xlab ? xlab.bottom : 0)
        : 30 + Math.min(NS.PANEL_H_CAP, cats.length * 28) + bottomBase,
      labelNote: (!vertical && cats.length * 28 > NS.PANEL_H_CAP)
        ? NS.axes.thinnedLabelNote(NS.PANEL_H_CAP, cats.length, P.title(cfg.category)) : null,
      option: {
        ...P.base,
        tooltip: { trigger: 'item', confine: true, formatter: distTooltip(k) },
        legend: split ? { show: false, data: levels } : undefined,
        grid: vertical
          ? { left: 55, right: 10, top: 30, bottom: bottomBase }
          : { left: gut.gridLeft, right: 5, top: 30, bottom: bottomBase },
        xAxis: vertical ? catAxes : valAxis,
        yAxis: vertical ? valAxis : catAxes,
        series: [...series, ...pointSeries]
      }
    };
  };

  option.distributionParts = { distribution, whiskerSeries };
})();
// --- chart/option-aggregated.js
// @ts-check
/**
 * Chart block: the aggregated option. One ECharts option per panel, built by
 * the chart type's builder, with its canvas height, the `xFit` inputs a
 * resize re-fits and a thinned-label note. Also the legend chips and the
 * highlight patch, which dims the marks a filter does not light.
 *
 * Each builder file exports under its own name: option.barParts (bar,
 * waterfall), option.radialParts (pie, treemap), option.distributionParts
 * (box plot, point range), with option.barMarkParts beside them. Here the
 * part they all share, option.aggregatedCommon(), and builderOf(), which
 * names the builder of each type.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const option = /** @type {any} */ (NS.option = NS.option || {});
  const { withItemStyle } = option.barMarkParts;

  // The canvas of a builder that sizes nothing itself.
  const DEFAULT_PANEL_H = 350;

  /** What every builder shares: the option's base, the value's title, the aggregation's name and
   * the tooltip's rows.
   * @param {ChartLook} look
   */
  option.aggregatedCommon = (look) => {
    const { cfg, columns, ink } = look;
    const title = (/** @type {any} */ col) => NS.axes.axisTitle(columns, col);
    const fn = cfg.func || 'count';
    const col = title(cfg.measure);
    const aggLabel = fn === 'count' ? 'Count'
      : fn === 'identity' ? (col || 'Value')
      : fn === 'count_distinct' ? 'Distinct ' + col
      : (NS.agg.AGG_WORDS[fn] || fn) + ' of ' + col;
    return {
      base: { ...(look.theme ? {} : { backgroundColor: 'transparent' }),
              textStyle: { fontFamily: ink.face } },
      title,
      valueTitle: cfg.measure === '.count' ? 'Count' : col,
      aggLabel,
      // The aggregation and its value, a share, and n where n is not the value.
      aggPairs: (/** @type {number} */ value, /** @type {number} */ n,
                 /** @type {number} */ percent) => {
        /** @type {Array<[string, any]>} */
        const pairs = [[aggLabel, V.util.num(value)]];
        if (percent != null) pairs.push(['Share', Math.round(percent) + '%']);
        if (cfg.func !== 'count' && cfg.func !== 'identity' && n != null) pairs.push(['n', n]);
        return pairs;
      }
    };
  };

  /** The builder for a chart type. @param {any} m */
  const builderOf = (m) => {
    const ct = m.type;
    if (ct === 'pie') return option.radialParts.pie;
    if (NS.DISTRIBUTION_TYPES.includes(ct)) return option.distributionParts.distribution;
    if (ct === 'treemap') return option.radialParts.treemap;
    return m.cumulative ? option.barParts.waterfall : option.barParts.bar;
  };

  /**
   * @param {any} m the aggregated model
   * @param {ChartLook} look
   * @returns {{ panels: Array<{ option: any, height: string, panelH: number, xFit: any }>,
   *             labelNote: string | null }}
   */
  const aggregated = (m, look) => {
    const build = builderOf(m);
    /** @type {string | null} */
    let labelNote = null;
    const panels = m.panels.map((/** @type {any} */ panel, /** @type {number} */ i) => {
      const r = build(m, panel, look, (look.widths || [])[i] || 0);
      if (!r) {
        const h0 = NS.panelH(look.cfg, DEFAULT_PANEL_H);
        return { option: null, height: h0 + 'px', panelH: h0, xFit: null };
      }
      if (r.labelNote) labelNote = r.labelNote;
      const panelH = r.panelH != null ? r.panelH : NS.panelH(look.cfg, DEFAULT_PANEL_H);
      return {
        option: NS.cardTooltip(NS.quietHover(r.option, look.cfg), look.ink),
        // 64 only catches a degenerate option; a one-row panel is 104.
        height: Math.max(64, Math.round(panelH)) + 'px',
        panelH,
        xFit: r.xFit || null
      };
    });
    return { panels, labelNote };
  };

  /**
   * The legend chips: a distribution's split levels, else a bar's colour
   * levels. Pie, treemap and waterfall have none.
   * @param {any} m @param {ChartSettings} cfg @param {ChartDerived} derived
   * @returns {{ col: string, items: Array<{ name: string, color: string }> } | null}
   */
  const aggregatedLegend = (m, cfg, derived) => {
    const ct = m.type;
    const levels = m.distribution ? m.levels
      : (ct !== 'pie' && ct !== 'treemap' && !m.cumulative ? m.colors : []);
    if (!levels.length) return null;
    const palette = NS.paletteOf(derived);
    // A highlight lists only the levels it lights.
    const colors = (m.hlColors && !m.distribution)
      ? NS.litLevelColors(levels, m.hlColors, m.colorScale, palette)
      : NS.levelColors(levels, m.colorScale, palette);
    const items = levels.map((/** @type {string} */ name, /** @type {number} */ i) =>
      ({ name, color: colors[i] })).filter((/** @type {any} */ it) => it.color);
    // Levels come from the colour column, so it is set.
    return items.length ? { col: /** @type {string} */ (cfg.color), items } : null;
  };

  // Series patched only while a filter dims something (or to undo it).
  const QUIET_TYPES = ['line', 'scatter', 'custom'];

  /**
   * The highlight patch for one panel: every datum outlined or faded by
   * whether its mark is lit (`lit` null: no filter; NS.selStyle()). Built off
   * the current option so what each datum carries (corner, colour, raw
   * value, label) rides along.
   * @param {any} m @param {any} panel @param {Record<string, any>} cfg
   * @param {any} current chart.getOption()
   * @param {((keys: Record<string, any>) => boolean) | null} lit
   * @param {boolean} wasDimmed the panel's quiet series carry dimming from before
   * @param {Ink} [ink]
   * @returns {{ series: any[], dimmed: boolean }}
   */
  const aggregatedPatch = (m, panel, cfg, current, lit, wasDimmed, ink = NS.INK_DEFAULT) => {
    const SEP = NS.model.BOX_CAT_SEP;
    const fv = panel.facet;
    // getOption() hands axes back as arrays; the first is the category axis.
    const first = (/** @type {any} */ a) => (Array.isArray(a) ? a[0] : a);
    const cats = (first(current.xAxis) && first(current.xAxis).data) ||
      (first(current.yAxis) && first(current.yAxis).data) || [];
    /** @param {string | null} g @param {string | null} lv */
    const on = (g, lv) => !lit || lit(NS.model.aggregatedKeys(cfg, fv, g, lv));
    /** @param {any} cat */
    const slotMark = (cat) => {
      const s = String(cat ?? '');
      if (!m.split) return [s, null];
      const [g, lv] = s.split(SEP);
      return [g, lv == null ? null : lv];
    };
    const barSplit = m.type === 'bar' && !m.cumulative && m.colors.length > 0;
    const anyFilter = !!lit;
    /** @param {boolean} lit1 @param {any} s */
    const look = (lit1, s) => NS.selStyle(lit1, anyFilter, ink, s.itemStyle);

    const series = (current.series || []).map((/** @type {any} */ s) => {
      // A name written on the marks is no mark of the data.
      if (typeof s.id === 'string' &&
          (s.id.indexOf('__legend_') === 0 || s.id === '__label__')) return {};
      // The waterfall's invisible base: an outline would show it.
      if (s.silent && s.type === 'bar') return {};
      if (s.type === 'pie' || s.type === 'treemap') {
        return { data: (s.data || []).map((/** @type {any} */ d) => d == null ? d
          : withItemStyle(d, look(on(d.name, null), s))) };
      }
      // A box series is custom, one datum per category (option-distribution.js).
      if (typeof s.id === 'string' && s.id.indexOf('__box_') === 0) {
        if (cats.length === 0) return {};
        return { data: (s.data || []).map((/** @type {any} */ v, /** @type {number} */ i) => {
          if (!v || typeof v !== 'object') return v;
          const [g, lv] = slotMark(cats[i]);
          return withItemStyle(v, look(on(g, lv), s));
        }) };
      }
      if (QUIET_TYPES.includes(s.type)) {
        // A point range's dots and intervals, a box's outliers.
        if (!m.distribution || s.type === 'line' || (!anyFilter && !wasDimmed)) return {};
        return { data: (s.data || []).map((/** @type {any} */ d) => {
          if (d == null) return d;
          const isObj = typeof d === 'object' && !Array.isArray(d);
          const val = isObj ? d.value : d;
          let cat = isObj && d.name != null ? d.name : null;
          if (cat == null && Array.isArray(val)) {
            const idx = s.type === 'custom' ? val[0]
              : (cfg.orientation !== 'horizontal' ? val[0] : val[1]);
            cat = cats[idx];
          }
          const [g, lv] = slotMark(cat);
          return withItemStyle(d, look(on(g, lv), s));
        }) };
      }
      if (cats.length === 0) return {};
      const lv = barSplit ? s.name : null;
      return { data: (s.data || []).map((/** @type {any} */ v, /** @type {number} */ i) => {
        const isObj = (v && typeof v === 'object' && !Array.isArray(v));
        return {
          value: isObj ? v.value : v,
          ...(isObj && v.raw != null ? { raw: v.raw } : {}),
          ...(isObj && v.label ? { label: v.label } : {}),
          itemStyle: { ...(isObj ? v.itemStyle : undefined),
                       ...look(on(String(cats[i] ?? ''), lv), s) }
        };
      }) };
    });
    return { series, dimmed: anyFilter };
  };

  Object.assign(option, { aggregated, aggregatedLegend, aggregatedPatch });
})();
// --- chart/option-points.js
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
      smootherSeries: look.derived.smoother,
      pairs: option.domainParts.intervalPairs(cfg),
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
      // A line without dots has no symbol at all: ECharts would show one at
      // the hovered x.
      symbol: isLine && (crowd || !dots) ? 'none' : 'circle',
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
      const rib = option.bandParts.ribbon(s.level, b, 'lo', 'hi', clr, BAND_OPACITY);
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
            series.push(option.distributionParts.whiskerSeries(s.level || 'errbar', errPts, clrOf(s), false));
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
    return { series, stepMode, smoothOn, markerPx: lineMarkerPx, focusW: 2.5 * lm, labelPad };
  };

  /** The hover overlay of a crowd of lines (empty until a line is hovered): a veil on its own
   * hidden 0..1 axes, then the hovered line and its dots. `state.veil` is the fill, set on
   * promotion. @param {{ veil?: string }} state */
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
   * The line chart's tooltip at the cursor's x, on an invisible pointer:
   * each line's value at x, highest first, capped. A crowd shows only the line nearest the cursor
   * (`hover.si`).
   * @param {any} m @param {any} look @param {{ si: number | null }} hover
   */
  const lineTooltip = (m, look, hover) => {
    const title = (/** @type {any} */ col) => NS.axes.axisTitle(look.columns, col);
    const ttSuffix = ttSuffixOf(m, look);
    const vf = NS.valueFormat(look.cfg);
    return {
      trigger: 'axis',
      axisPointer: { type: 'none' },
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

  option.pointParts = { points, labelSpacer, lineOverlay, lineTooltip, pointTooltip, endLabelOf, endLabelPad };
})();
// --- chart/option-band.js
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
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const option = /** @type {any} */ (NS.option = NS.option || {});

  const BASE_LINE_WIDTH = 1.4;

  /** A ribbon between two edges: one polygon per run where both exist, so a band cut for thin n
   * breaks. Null when no run has two points. @param {any} name @param {any} b @param {string} loKey
   * @param {string} hiKey @param {string} clr @param {number} op */
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
      ...(named && nm ? option.pointParts.endLabelOf(ink) : {})
    });
    return at;
  };

  /** The band series of one panel. @param {any} m the individual model @param {any} panel
   * @param {any} look
   * @returns {{ series: any[], band: any, markLineIdx: number, many: boolean, labelPad: number }}
   */
  const band = (m, panel, look) => {
    const { cfg, ink } = look;
    const dm = cfg.point_size;
    const lm = cfg.line_width;
    /** @type {any[]} */ const series = [];
    const panelBand = option.domainParts.bandBucket(look.derived, panel);
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
      labelPad: named ? option.pointParts.endLabelPad(levels) : 0
    };
  };

  /** The reference limits R reduced from their columns (hi, then lo) that hold a number.
   * @param {ChartDerived} derived @returns {any[]} */
  const refLimits = (derived) => {
    const refs = (derived.band && derived.band.refs) || {};
    return [refs.hi, refs.lo].filter((r) => r && Number.isFinite(Number(r.value)));
  };

  /**
   * The value axis of a band panel, fitted to what it draws (a custom series
   * puts one datum into the extent) and opened to a reference limit up to
   * 60 % more span, rounded outward. Null when nothing numeric is drawn.
   * @param {ChartSettings} cfg @param {ChartDerived} derived @param {any} panel @param {boolean} many
   * @returns {{ lo: number, hi: number } | null}
   */
  const bandExtent = (cfg, derived, panel, many) => {
    const drawn = ['center', 'lo', 'hi'];
    if (!many) {
      drawn.push('olo', 'ohi');
      if (cfg.box_points === 'outliers') drawn.push('out_y');
    }
    const vals = option.domainParts.bandValues(derived, panel, drawn)
      .filter((/** @type {any} */ v) => v != null).map(Number).filter(Number.isFinite);
    let lo = NS.minOf(vals), hi = NS.maxOf(vals);
    if (!(Number.isFinite(lo) && Number.isFinite(hi) && hi > lo)) return null;
    const span = hi - lo;
    for (const r of refLimits(derived)) {
      const v = Number(r.value);
      const nlo = Math.min(lo, v), nhi = Math.max(hi, v);
      if (nhi - nlo <= span * 1.6) { lo = nlo; hi = nhi; }
    }
    const pad = (hi - lo) * 0.06;
    const [aLo, aHi] = option.domainParts.niceDomain(lo - pad, hi + pad);
    return { lo: aLo, hi: aHi };
  };

  /**
   * The reference limits R reduced from their columns, as markLine data,
   * named with the column's title and, when it varies, its range. A limit
   * outside the frame is pinned to the edge and says which way.
   * @param {ChartSettings} cfg @param {ChartDerived} derived
   * @param {{ lo: number, hi: number } | null} ext @param {any} ink @param {VizColumn[]} columns
   */
  const bandRefLines = (cfg, derived, ext, ink, columns) => {
    const out = [];
    for (const r of refLimits(derived)) {
      const v = Number(r.value);
      const base = NS.axes.axisTitle(columns, r.col) + ' ' + V.util.num3(v) +
        (Number(r.n_distinct) > 1
          ? ' (' + V.util.num3(Number(r.lo)) + '–' + V.util.num3(Number(r.hi)) + ')' : '');
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

  option.bandParts = { band, bandExtent, bandRefLines, ribbon };
})();
// --- chart/option-domains.js
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
      dropped += NS.labelled(cfg.value_lines).filter((/** @type {{ value: any }} */ x) => !pos(x.value)).length;
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
// --- chart/option-individual.js
// @ts-check
/**
 * Chart block: the individual option (scatter, line, band), one ECharts
 * option per panel. The series come from option-points.js or option-band.js;
 * this file adds the helper lines, the identity line and the band's limits
 * on one markLine, the legend's empty series, the axes, the tooltip and the
 * brush toolbox. A panel result also carries the state its slot gets: the
 * hover tracker, the line picker's registry (`focus`), the band picker's
 * ribbons (`band`) and whether it is brushed. Beside it:
 * the legend chips and the highlight patch.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const option = /** @type {any} */ (NS.option = NS.option || {});

  // The canvas height, before turned x labels add their gutter.
  const PANEL_H = 400;
  // The grid's left and right margins, which the x labels do not get.
  const X_INSET = 71;

  // ECharts builds the brush drag controller only for a rendered
  // toolbox, so one is rendered with no size, outside the canvas.
  /** @param {any} feature */
  const hiddenToolbox = (feature) =>
    ({ show: true, itemSize: 0, itemGap: 0, right: -40, top: -40, feature });
  const BRUSH_TOOLS = ['rect', 'lineX', 'clear'];

  const { identityDomain, pos, logDomains, clampBand } = option.domainParts;

  /** What every part of a panel's option reads: config, colours, the panel's
   * log domains (null on a linear axis) and the measured x labels.
   * @param {any} m @param {any} look @param {number} width */
  const panelContext = (m, look, width) => {
    const { cfg, ink, theme } = look;
    const lx = (look.logDom && look.logDom.x) || null;
    const ly = (look.logDom && look.logDom.y) || null;
    const title = (/** @type {any} */ col) => NS.axes.axisTitle(look.columns, col);
    const xCats = m.x.cats;
    // Measured before the height: turned labels grow the canvas.
    const xlab = xCats ? NS.axes.xAxisLabels(xCats, width - X_INSET, true, look.measure, ink) : null;
    return { m, look, width, cfg, ink, theme, lx, ly, title, lm: cfg.line_width, xCats, xlab,
             isLine: m.isLine, isBand: m.isBand, isTime: m.x.type === 'time' };
  };

  /** The panel's series from the band or the points part, with what the option
   * needs beside them. @param {any} k @param {any} panel */
  const panelSeries = (k, panel) => {
    const { m, look, lx, ly, width } = k;
    if (k.isBand) {
      // On a log axis the band's series are clamped above the axis's floor.
      const band = look.derived.band || {};
      const b = option.bandParts.band(m, panel, ly ? { ...look, derived: { ...look.derived,
        band: { ...band, series: clampBand(band.series, ly.min) } } } : look);
      return { series: b.series, mlTarget: Math.max(b.markLineIdx, 0), bandMany: b.many,
               band: b.band, pts: null, labelPad: b.labelPad };
    }
    const pts = option.pointParts.points(m, panel, (lx || ly)
      ? { ...look, logFloorX: lx ? lx.min : null, logFloorY: ly ? ly.min : null } : look,
      width > X_INSET ? width - X_INSET : 0);
    return { series: pts.series, mlTarget: 0, bandMany: false, band: null, pts,
             labelPad: pts.labelPad };
  };

  /** The helper lines, the band's limits and the identity line for the shared
   * markLine, with the domain both axes then share (`idDom`, null without
   * the identity line). @param {any} k @param {any} ext @param {any[]} series */
  const panelRefData = (k, ext, series) => {
    const { m, look, cfg, ink, lx, ly, lm, isBand, isLine } = k;
    const refData = [];
    // An unnamed line at 0 needs no label: the axis prints its value.
    /** @param {any} v @param {any} nm */
    const refText = (v, nm) => nm || (Number(v) === 0 ? null : undefined);
    /** @type {{ value: any, label: string | null }[]} */ (NS.labelled(cfg.x_lines)).forEach(({ value: v, label: nm }) => {
      if (lx && !pos(v)) return;
      if (m.x.type === 'category') {
        const c = NS.axes.catOf(v, m.x.cats || []);
        if (c == null) return;
        const text = nm || (c === String(v) ? null : String(v));
        refData.push(NS.axes.guideLine(c, true, lm, text, false, ink));
        return;
      }
      const at = m.x.encode ? m.x.encode(v) : Number(v);
      if (!Number.isFinite(at)) return;
      const text = m.x.type === 'time'
        ? (nm || NS.axes.fmtXVal(at, 'time', null, m.x.period === 'time' ? 'time' : null))
        : refText(v, nm);
      refData.push(NS.axes.guideLine(at, true, lm, text, false, ink));
    });
    /** @type {{ value: any, label: string | null }[]} */ (NS.labelled(cfg.value_lines)).forEach(({ value: v, label: nm }) => {
      if (ly && !pos(v)) return;
      refData.push(NS.axes.guideLine(Number(v), false, lm, refText(v, nm), false, ink));
    });
    if (isBand) for (const r of option.bandParts.bandRefLines(cfg, look.derived, ext, ink, look.columns)) refData.push(r);
    // y = x is a diagonal only when both axes use the same scale.
    const identityOn = cfg.identity_line === true && !isLine &&
      m.x.type !== 'category' && m.y.type !== 'category' && !lx === !ly;
    /** @type {any} */
    let idDom = null;
    /** @type {any} */
    let idLog = null;
    if (identityOn && lx && ly) {
      idLog = NS.axes.logTicks(Math.min(lx.min, ly.min), Math.max(lx.max, ly.max));
      if (idLog) idDom = [idLog.min, idLog.max];
    } else if (identityOn) {
      idDom = identityDomain(series);
    }
    if (idDom) {
      refData.push([
        { coord: [idDom[0], idDom[0]],
          lineStyle: { color: ink.muted, type: 'dashed', width: 1.5 * lm } },
        { coord: [idDom[1], idDom[1]] }
      ]);
    }
    return { refData, idDom, idLog };
  };

  /** @param {any} k @param {any} ext @param {any} idDom @param {any} idLog */
  const panelAxes = (k, ext, idDom, idLog) => {
    const { m, cfg, ink, lx, ly, xCats, xlab, isLine, isTime } = k;
    // A line chart reads values off the y grid and dates off the crosshair,
    // so it has no x grid and no y axis line.
    const grid = { lineStyle: { color: ink.border, type: 'solid', width: 1 } };
    const xTitle = NS.axisName(k.title(m.x.col), m.x.col, cfg, k.look.derived, { unit: false });
    /** @type {any} */
    const xAxis = {
      ...NS.axes.valueAxis(ink, m.x.type, xTitle, xlab && xlab.bottom ? xlab.bottom + 16 : 28),
      axisLabel: xlab ? xlab.axisLabel : {
        color: ink.muted, fontSize: ink.fontSize, hideOverlap: true,
        // A date axis labels years, then months, then days as it zooms in.
        ...(isTime ? { formatter: { year: '{yyyy}', month: '{MMM}', day: '{d} {MMM}',
                                    hour: '{HH}:{mm}', minute: '{HH}:{mm}' } } : {})
      },
      axisTick: { show: false },
      splitLine: isLine ? { show: false } : grid,
      scale: true
    };
    if (xCats) xAxis.data = xCats;
    if (idDom && m.x.type === 'value') { xAxis.min = idDom[0]; xAxis.max = idDom[1]; }
    if (lx) NS.axes.logAxis(xAxis, idLog || lx);
    /** @type {any} */
    const yAxis = {
      ...NS.axes.valueAxis(ink, m.y.type, NS.axisName(k.title(m.y.col), m.y.col, cfg, k.look.derived), 46,
                           { format: m.y.type === 'value' && NS.valueFormat(cfg, { unit: false }) }),
      nameRotate: 90,
      axisTick: { show: false },
      ...(isLine ? { axisLine: { show: false } } : {}),
      splitLine: grid,
      scale: true
    };
    if (m.y.cats) yAxis.data = m.y.cats;
    if (idDom && m.y.type === 'value') { yAxis.min = idDom[0]; yAxis.max = idDom[1]; }
    if (ext) { yAxis.min = ext.lo; yAxis.max = ext.hi; }
    if (cfg.type === 'area' && m.y.type === 'value' && yAxis.min == null) {
      yAxis.scale = false;
      yAxis.min = (/** @type {any} */ v) => Math.min(0, v.min);
    }
    if (ly) NS.axes.logAxis(yAxis, idLog || ly);
    const scrimAxis = { type: 'value', min: 0, max: 1, show: false, axisPointer: { show: false } };
    return { xAxis, yAxis, scrimAxis };
  };

  /** One panel's option and the state its slot gets. @param {any} m @param {any} panel
   * @param {any} look @param {number} width */
  const panelOption = (m, panel, look, width) => {
    const k = panelContext(m, look, width);
    const { cfg, ink, theme, ly, isLine, isBand, isTime, xlab, xCats } = k;
    const { series, mlTarget, bandMany, band, pts, labelPad } = panelSeries(k, panel);

    // On a log axis the band's frame is the log domain.
    const ext = (isBand && m.y.type === 'value' && series.length)
      ? (ly ? { lo: ly.min, hi: ly.max } : option.bandParts.bandExtent(cfg, look.derived, panel, bandMany)) : null;

    // One markLine per series is all ECharts allows, so they share one.
    const { refData, idDom, idLog } = panelRefData(k, ext, series);
    if (series.length > 0 && refData.length) {
      series[series[mlTarget] ? mlTarget : 0].markLine = {
        silent: true, symbol: 'none',
        lineStyle: { color: ink.danger, type: 'dashed', width: 1.5 * k.lm },
        label: { show: false },
        data: refData
      };
    }
    // With a series column apart from the colour, the chips are colour values
    // no series is named after: an empty series per chip for the legend to bind.
    if (m.byColor && m.legend) {
      for (const it of m.legend.items) {
        series.push({
          type: isLine ? 'line' : 'scatter', name: it.name, data: [],
          itemStyle: { color: it.color }, lineStyle: { color: it.color },
          showSymbol: false, silent: true
        });
      }
    }

    const { xAxis, yAxis, scrimAxis } = panelAxes(k, ext, idDom, idLog);

    // A line, a band and a scatter with series are clicked, not brushed (a
    // brush would race the click on a point).
    const brushable = m.x.type !== 'category' && !isLine && !isBand && !m.seriesCol;
    const hover = { si: null };
    // A crowd of lines (one per series): the line nearest the cursor is what
    // the tooltip names and a click takes, and hover promotes it over a veil
    // (family-individual.js). A few lines are read off the tooltip at the
    // cursor's x, and hover draws nothing.
    const crowd = isLine && m.crowd && pts;
    /** @type {{ veil?: string }} */
    const scrimState = {};
    if (crowd) for (const s of option.pointParts.lineOverlay(scrimState)) series.push(s);
    const focus = crowd ? Object.assign(scrimState, {
      series, stepMode: pts.stepMode || null, smoothOn: pts.smoothOn,
      xAxisType: m.x.type, xOrder: m.x.order, markerPx: pts.markerPx, focusW: pts.focusW,
      yLog: !!ly
    }) : null;

    const opt = {
      ...(theme ? {} : { backgroundColor: 'transparent' }),
      // Dates are placed and labelled in UTC: R writes them without a zone.
      ...(isTime ? { useUTC: true } : {}),
      textStyle: { fontFamily: ink.face },
      // Only the first draw animates: a merge-patch morphs lines while the
      // dots jump to the new layout.
      animationDurationUpdate: 0,
      tooltip: isLine ? option.pointParts.lineTooltip(m, look, hover) : option.pointParts.pointTooltip(m, look),
      // Hidden: its selection model is what the chips toggle.
      legend: { show: false },
      grid: { left: 66, right: 5 + labelPad, top: 30,
              bottom: 52 + (xlab ? xlab.bottom : 0) },
      xAxis: crowd ? [xAxis, scrimAxis] : xAxis,
      yAxis: crowd ? [yAxis, scrimAxis] : yAxis,
      // The drag zoom is zoom.js's, on every continuous axis (zoom-option.js).
      toolbox: brushable ? hiddenToolbox({ brush: { type: BRUSH_TOOLS } }) : undefined,
      // With a drill target, ECharts dims the points outside the rectangle
      // while the reader drags, as a latched selection would, and reports
      // every move: the drill is sent once, at the end of the drag.
      brush: brushable ? {
        toolbox: BRUSH_TOOLS, xAxisIndex: 0, yAxisIndex: 0,
        brushStyle: { color: 'rgba(0, 114, 178, 0.1)', borderColor: 'rgba(0, 114, 178, 0.5)',
                      borderWidth: 1 },
        ...(NS.transientDrill(cfg)
          ? { outOfBrush: { opacity: NS.SEL_DIM }, throttleDelay: 0 }
          : { throttleDelay: 300 })
      } : undefined,
      series
    };
    const panelH = NS.panelH(cfg, PANEL_H) + (xlab ? xlab.bottom : 0);
    return {
      option: NS.cardTooltip(NS.quietHover(opt, cfg), ink),
      height: panelH + 'px',
      panelH,
      xFit: xlab ? { labels: xCats, inset: X_INSET, decimate: true, nameGap: true,
                     gutter: xlab.bottom, key: xlab.key } : null,
      hover, focus, band, brushable
    };
  };

  /**
   * @param {any} m the individual model
   * @param {ChartLook} look
   */
  const individual = (m, look) => {
    const logs = (m.logX || m.logY) ? logDomains(m, look.cfg, look.derived) : null;
    const dropped = (m.logDropped || 0) + (logs ? logs.dropped : 0);
    const logNote = dropped
      ? `Log scale: ${dropped} value${dropped === 1 ? '' : 's'} at or below zero not shown.` : null;
    return {
      panels: m.panels.map((/** @type {any} */ panel, /** @type {number} */ i) =>
        panelOption(m, panel, logs ? { ...look, logDom: { x: logs.x[i], y: logs.y[i] } } : look,
                    (look.widths || [])[i] || 0)),
      labelNote: [m.capMessage, logNote].filter(Boolean).join(' ') || null
    };
  };

  /** The legend band's chips. @param {any} m */
  const individualLegend = (m) => (m.legend && m.legend.items.length ? m.legend : null);

  /** Is a series one of the panel's marks, not an overlay? @param {any} s */
  const isMark = (s) => s && s.id == null && !s.silent &&
    (s.type === 'scatter' || s.type === 'line');

  /**
   * The highlight patch for one panel, from the series the panel was drawn
   * with: a line, a band level or a scatter series is lit when its keys match
   * the filter; on a filter of a column a scatter series does not carry, each
   * point by its own row. A range filter lights the scatter points inside it,
   * and the lines and band levels with a point inside it; its facet key, in
   * `filters`, lights the panel. Overlays follow their series. A lit point
   * is outlined and a lit line drawn a pixel wider; the rest fade
   * (NS.selStyle()). Without a filter, and nothing dimmed from before, the
   * patch is empty.
   * @param {any} m @param {any} panel @param {any[]} drawn the panel option's series
   * @param {((keys: Record<string, any>, rows: any) => boolean) | null} lit
   * @param {boolean} wasDimmed
   * @param {Record<string, any[]> | null} filters
   * @param {{ x_range: any[], y_range: any[] | null } | null} [range]
   * @param {Ink} [ink]
   * @returns {{ series: any[], dimmed: boolean }}
   */
  const individualPatch = (m, panel, drawn, lit, wasDimmed, filters, range, ink = NS.INK_DEFAULT) => {
    if (!lit && !range && !wasDimmed) return { series: drawn.map(() => ({})), dimmed: false };
    const inRange = range ? (/** @type {any[]} */ p) => NS.keys.pointInRange(p, range) : null;
    const NONE = '\u0000';
    const keyOf = (/** @type {any} */ name) => (name == null ? NONE : String(name));
    // A highlight splits a scatter level into muted and highlighted parts
    // (model-individual.js): `byPart` finds the part a drawn series is,
    // `byLevel` the whole level.
    const MUTED = '\u0002';
    /** @type {Map<string, any>} */
    const byPart = new Map();
    /** @type {Map<string, { rows: any[], pts: any[] }>} */
    const byLevel = new Map();
    for (const s of panel.series) {
      byPart.set(keyOf(s.level) + (s.muted ? MUTED : ''), s);
      const k = keyOf(s.level);
      const all = byLevel.get(k) || { rows: [], pts: [] };
      byLevel.set(k, { rows: all.rows.concat(s.rows), pts: all.pts.concat(s.pts) });
    }
    const keyCols = new Set(m.splitCol ? [m.splitCol] : []);
    if (m.facet) keyCols.add(m.facet);
    const byRow = !!lit && !!filters && Object.keys(filters).some((c) => !keyCols.has(c));
    const perPoint = (byRow || !!inRange) && !m.isLine && !m.isBand;
    /** @type {Map<string, boolean>} */
    const levelLit = new Map();
    /** @param {any} name */
    const onLevel = (name) => {
      if (!lit && !inRange) return true;
      const k = keyOf(name);
      if (!levelLit.has(k)) {
        const ms = byLevel.get(k);
        const keys = NS.model.individualKeys(m, panel.facet, name);
        const rows = () => (ms ? ms.rows
          : (m.splitCol && name != null ? m.ix.get(panel.rows, m.splitCol, 'nz', String(name))
            : panel.rows));
        let on = lit ? lit(keys, rows) : true;
        // A line or a band level is lit when one of its points is in range.
        if (on && inRange && ms) on = ms.pts.some(inRange);
        levelLit.set(k, on);
      }
      return /** @type {boolean} */ (levelLit.get(k));
    };
    const filtered = !!lit || !!inRange;
    const DIM = NS.SEL_DIM;
    const series = drawn.map((/** @type {any} */ s) => {
      if (!s || s.id != null) return {};
      /** @param {boolean} on */
      const look = (on) => NS.selStyle(on, filtered, ink, s.itemStyle);
      if (s.silent && s.type !== 'custom' && Array.isArray(s.data) && !s.data.length) return {};
      const ms = byPart.get(keyOf(s.name) + (s.blockrMuted ? MUTED : ''));
      if (isMark(s) && s.type === 'scatter' && ms) {
        if (perPoint) {
          let any = false;
          const keyOn = byRow || !lit || lit(NS.model.individualKeys(m, panel.facet, s.name), () => ms.rows);
          const data = ms.pts.map((/** @type {any} */ p, /** @type {number} */ j) => {
            const on = keyOn && (!byRow || NS.keys.rowMatches(ms.rows[j], filters)) &&
              (!inRange || inRange(p));
            if (on) any = true;
            return { value: p, itemStyle: look(on) };
          });
          // Both parts of a highlighted level answer for it.
          levelLit.set(keyOf(s.name), any || levelLit.get(keyOf(s.name)) === true);
          return { data, itemStyle: { opacity: 1 } };
        }
        return { data: ms.pts, itemStyle: look(onLevel(s.name)) };
      }
      const on = onLevel(s.name);
      if (s.type === 'line') {
        const mark = isMark(s);
        const base = m.isBand ? 1 : (mark ? m.lineOpacity : 0.9);
        const w = s.lineStyle && s.lineStyle.width;
        return { lineStyle: { opacity: on ? base : base * DIM,
                              ...(mark && w ? { width: filtered && on ? w + 1 : w } : {}) },
                 ...(mark ? { itemStyle: { opacity: on ? 1 : DIM } } : {}) };
      }
      return { itemStyle: look(on) };
    });
    return { series, dimmed: filtered };
  };

  Object.assign(option, { individual, individualLegend, individualPatch });
})();
// --- chart/ticks.js
// @ts-check
/**
 * Chart block: `x_ticks` and `y_ticks` on the axes. The values are read in
 * core/ticks.js; here they reach ECharts as `customValues` on the axis
 * labels and ticks, which the split lines follow.
 *
 *   - decorateAxis() runs with the limits (zoom-option.js): a step rounds an
 *     auto end of a value axis out to its multiples, and a day or week step
 *     starts a date axis at its first date, so the ticks start there.
 *   - after() runs once the chart is drawn and the panels share their axes:
 *     it reads each axis's extent and sets the ticks over it. A step needs
 *     the extent, which ECharts knows only after it has laid out the data.
 *   - zoomed() runs with the zoom: ECharts draws the custom ticks inside the
 *     window; with fewer than two of them there, the axis draws its own until
 *     the reset.
 * An axis whose setting cannot apply (a step on a log axis, too many ticks)
 * draws its own ticks, the ones its option brought. A date axis keeps its level formatter, which labels
 * a custom tick by its date (a year, a month, a day); ECharts 6 throws on a
 * function formatter there, as custom ticks carry no level.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const T = V.ticks;

  const KEYS = /** @type {('xAxis' | 'yAxis')[]} */ (['xAxis', 'yAxis']);

  /** The ticks setting the type takes for a role, or null.
   * @param {Record<string, any>} cfg @param {'x' | 'y' | null} role */
  const settingOf = (cfg, role) => {
    if (!role) return null;
    const key = role + '_ticks';
    const spec = /** @type {any} */ (V.spec && V.spec.chart)?.[key];
    const a = spec && spec.applies;
    if (a && a.only && !a.only.includes(cfg.type)) return null;
    if (a && a.except && a.except.includes(cfg.type)) return null;
    return T.parse(cfg[key]) ? cfg[key] : null;
  };

  /**
   * An axis with its ticks setting before the draw: a step on a value axis
   * rounds each auto end out to its multiples (an axis that starts at zero,
   * a bar's, keeps zero in); a day or week step starts a date axis at its
   * first date. Ends set by the limits stay.
   * @param {any} ax @param {Record<string, any>} cfg @param {'x' | 'y' | null} role
   * @param {string} type @param {[number | null, number | null]} limits
   */
  const decorateAxis = (ax, cfg, role, type, limits) => {
    const setting = settingOf(cfg, role);
    const p = setting ? T.parse(setting) : null;
    if (!p || p.form !== 'every') return ax;
    if (type === 'time' && (!p.unit || p.unit === 'day' || p.unit === 'week')) {
      if (limits[0] == null && ax.min == null) ax.min = (/** @type {any} */ e) => e.min;
      return ax;
    }
    if (type !== 'value') return ax;
    const zero = ax.scale !== true;
    /** @param {0 | 1} i @param {number} v */
    const out = (i, v) => {
      const r = T.roundOut(setting, v, v);
      return r ? r[i] : v;
    };
    if (limits[0] == null) {
      ax.min = typeof ax.min === 'number' ? out(0, ax.min)
        : (/** @type {any} */ e) => out(0, zero ? Math.min(0, e.min) : e.min);
    }
    if (limits[1] == null) {
      ax.max = typeof ax.max === 'number' ? out(1, ax.max)
        : (/** @type {any} */ e) => out(1, zero ? Math.max(0, e.max) : e.max);
    }
    return ax;
  };

  /** An axis's extent as drawn: the min and max a shared scale set (those
   * wait for the next frame), else the scale's.
   * @param {any} comp @returns {[number, number] | null} */
  const extentOf = (comp) => {
    const ext = comp.axis && comp.axis.scale ? comp.axis.scale.getExtent() : null;
    const lo = comp.get('min'), hi = comp.get('max');
    const a = typeof lo === 'number' ? lo : (ext ? ext[0] : NaN);
    const b = typeof hi === 'number' ? hi : (ext ? ext[1] : NaN);
    return Number.isFinite(a) && Number.isFinite(b) && b > a ? [a, b] : null;
  };

  /** The axis option that draws `vals` as its ticks, or its own when null.
   * @param {number[] | null} vals */
  const axisPatch = (vals) => ({ axisLabel: { customValues: vals }, axisTick: { customValues: vals } });

  /**
   * After a draw: each axis with a ticks setting takes its tick values over
   * the extent it was drawn at. The values stay on the slot for the zoom.
   * @param {ChartView} view @param {ChartSlot} slot
   */
  const after = (view, slot) => {
    const chart = slot.chart;
    const roles = chart && chart.__vzRoles;
    /** @type {Record<string, { vals: number[], own: number[] | null } | null>} */
    const kept = {};
    /** @type {Record<string, any>} */
    const patch = {};
    for (const key of KEYS) {
      const setting = roles ? settingOf(view.settings, roles[key]) : null;
      if (!setting) continue;
      const comp = chart.getModel().getComponent(key, 0);
      if (!comp) continue;
      const ext = extentOf(comp);
      const vals = ext ? T.values(setting, ext[0], ext[1], String(comp.get('type'))) : null;
      // The ticks the option brought (a log axis's 1-2-5), for when these cannot apply.
      const tick = comp.get('axisTick');
      const own = tick && Array.isArray(tick.customValues) ? tick.customValues : null;
      kept[key] = vals ? { vals, own } : null;
      if (vals) patch[key] = [axisPatch(vals)];
    }
    slot.ticks = Object.keys(kept).length ? kept : null;
    if (Object.keys(patch).length) chart.setOption(patch, { lazyUpdate: true });
  };

  /**
   * With the zoom window: the ticks inside it, or the axis's own while fewer
   * than two of them are.
   * @param {ChartSlot} slot @param {Record<string, number[] | null> | null} zoom
   */
  const zoomed = (slot, zoom) => {
    if (!slot.ticks || !slot.chart) return;
    /** @type {Record<string, any>} */
    const patch = {};
    for (const key of KEYS) {
      const t = slot.ticks[key];
      if (!t) continue;
      const w = zoom && zoom[key];
      const inView = w ? t.vals.filter((v) => v >= w[0] && v <= w[1]) : t.vals;
      patch[key] = [axisPatch(inView.length >= 2 ? t.vals : t.own)];
    }
    if (Object.keys(patch).length) slot.chart.setOption(patch);
  };

  /** The tick values an axis draws now, for a form picked in the gear or the
   * sentence to start from; [] when unknown.
   * @param {ChartView} view @param {'x' | 'y'} role @returns {{ vals: number[], type: string }} */
  const drawn = (view, role) => {
    const slot = view._slots.find((s) => s && s.chart);
    const roles = slot && slot.chart.__vzRoles;
    const key = roles ? KEYS.find((k) => roles[k] === role) : null;
    const comp = slot && key ? slot.chart.getModel().getComponent(key, 0) : null;
    if (!comp || !comp.axis) return { vals: [], type: 'value' };
    const type = String(comp.get('type'));
    /** @type {number[]} */
    let vals = [];
    try {
      vals = comp.axis.getTicksCoords().map((/** @type {any} */ t) => Number(t.tickValue))
        .filter(Number.isFinite);
    } catch (e) { vals = []; }
    return { vals, type };
  };

  /** The axis a ticks setting is on ('time', 'log' or 'value') and the ticks
   * it draws now, for the gear (VizGearHost.ticks). A log scale set in the
   * gear counts before the chart redraws with it; before the first draw the
   * x column says whether the axis holds dates.
   * @param {ChartView} view @param {string} key @param {Record<string, any>} v
   * @returns {VizTicksAxis} */
  const axisFor = (view, key, v) => {
    const role = key === 'x_ticks' ? 'x' : 'y';
    const d = drawn(view, role);
    if (!d.vals.length && role === 'x' && v.x) {
      d.type = NS.axes.axisTypeFor(view.columns, view.data, v.x, { temporal: true });
    }
    const tk = role + '_transform';
    const spec = /** @type {any} */ (V.spec.chart)[tk];
    const only = spec && spec.applies && spec.applies.only;
    const log = v[tk] === 'log10' && (!only || only.includes(v.type));
    return { type: log ? 'log' : (d.type === 'log' ? 'value' : d.type), drawn: d.vals };
  };

  NS.ticks = { settingOf, decorateAxis, after, zoomed, drawn, axisFor };
})();
// --- chart/zoom-option.js
// @ts-check
/**
 * Chart block: axis limits and the zoom window in the option. Every family's
 * option passes through decorate() before it is drawn:
 *
 *   - `x_ticks` and `y_ticks` round an auto end to their step (ticks.js).
 *   - `x_limits` and `y_limits` become the axis min and max. `y_limits` is
 *     the value axis: y on the individual types, whichever axis is not a
 *     category on the aggregated ones. Marks outside are clipped.
 *   - Every continuous axis that can zoom gets a dataZoom component with no
 *     interaction of its own; zoom.js moves it from the drag. On the
 *     aggregated types only the value axis zooms. The window is relative to
 *     the axis extent, so a reset returns to the limits.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** The dataZoom component of each axis. */
  const ZOOM_ID = { xAxis: 'vz-zoom-x', yAxis: 'vz-zoom-y' };

  /** @param {any} a */
  const first = (a) => (Array.isArray(a) ? a[0] : a);

  /** An axis type as ECharts reads it: x is a category unless it says not.
   * @param {any} ax @param {'xAxis' | 'yAxis'} key */
  const axisType = (ax, key) => (ax && ax.type) || (key === 'xAxis' ? 'category' : 'value');

  /**
   * Which limits each axis takes ('x', 'y' or null), or null for a chart
   * without axes.
   * @param {any} option @param {Record<string, any>} cfg
   * @returns {{ xAxis: 'x' | 'y' | null, yAxis: 'x' | 'y' | null } | null}
   */
  const roles = (option, cfg) => {
    const x = first(option && option.xAxis), y = first(option && option.yAxis);
    if (!x || !y) return null;
    const cx = axisType(x, 'xAxis') !== 'category', cy = axisType(y, 'yAxis') !== 'category';
    if (NS.familyOf(cfg.type) === 'aggregated') {
      if (cy) return { xAxis: null, yAxis: 'y' };
      return cx ? { xAxis: 'y', yAxis: null } : null;
    }
    return { xAxis: cx ? 'x' : null, yAxis: cy ? 'y' : null };
  };

  /** The limits the type takes for a role, or null.
   * @param {Record<string, any>} cfg @param {'x' | 'y'} role */
  const limitsOf = (cfg, role) => {
    const key = role + '_limits';
    const spec = /** @type {any} */ (V.spec && V.spec.chart)?.[key];
    const a = spec && spec.applies;
    if (a && a.only && !a.only.includes(cfg.type)) return null;
    if (a && a.except && a.except.includes(cfg.type)) return null;
    const v = cfg[key];
    return Array.isArray(v) && v.length === 2 ? v : null;
  };

  /** Whether a role has a limit set, either end.
   * @param {Record<string, any>} cfg @param {'x' | 'y' | null} role */
  const hasLimits = (cfg, role) => {
    const lim = role ? limitsOf(cfg, role) : null;
    return !!lim && lim.some((v) => v != null && v !== '');
  };

  /** A limit as the axis reads it: a date as UTC milliseconds on a time
   * axis; nothing at or below zero on a log axis.
   * @param {unknown} v @param {string} type @returns {number | null} */
  const axisValue = (v, type) => {
    if (v == null || v === '') return null;
    let n = typeof v === 'number' ? v : Number(v);
    if (!Number.isFinite(n) && typeof v === 'string' && type === 'time') n = Date.parse(v);
    if (!Number.isFinite(n)) return null;
    if (type === 'log' && n <= 0) return null;
    return n;
  };

  /** The from and to of a role's limits on an axis, each null for auto.
   * @param {Record<string, any>} cfg @param {'x' | 'y' | null} role @param {string} type
   * @returns {[number | null, number | null]} */
  const ends = (cfg, role, type) => {
    const lim = role ? limitsOf(cfg, role) : null;
    return lim ? [axisValue(lim[0], type), axisValue(lim[1], type)] : [null, null];
  };

  /**
   * The option with the limits on its axes and a zoom component per
   * zooming axis. The option passed in is not changed (the memo keeps it).
   * @param {any} option @param {Record<string, any>} cfg
   * @returns {{ option: any, roles: ReturnType<typeof roles>, filterX: boolean,
   *            logEnds: Record<string, (number | null)[]> }}
   */
  const decorate = (option, cfg) => {
    const r = roles(option, cfg);
    /** The limit ends of each log axis: zoom.js zooms those through min and max.
     * @type {Record<string, (number | null)[]>} */
    const logEnds = {};
    if (!r) return { option, roles: null, filterX: false, logEnds };
    const out = { ...option };
    /** @type {any[]} */
    const zoom = [];
    let filterX = false;
    for (const key of /** @type {('xAxis' | 'yAxis')[]} */ (['xAxis', 'yAxis'])) {
      const role = r[key];
      if (!role) continue;
      const many = Array.isArray(option[key]);
      const axes = many ? option[key].slice() : [option[key]];
      const ax = { ...axes[0] };
      const [lo, hi] = ends(cfg, role, axisType(ax, key));
      if (axisType(ax, key) === 'log') logEnds[key] = [lo, hi];
      if (lo != null) ax.min = lo;
      if (hi != null) ax.max = hi;
      NS.ticks.decorateAxis(ax, cfg, role, axisType(ax, key), [lo, hi]);
      axes[0] = ax;
      out[key] = many ? axes : ax;
      // x filters what it leaves out, so a flat drag refits y; y only moves.
      const filter = key === 'xAxis' && NS.familyOf(cfg.type) === 'individual' &&
        cfg.type !== 'band';
      if (filter) filterX = true;
      zoom.push({
        type: 'inside', id: ZOOM_ID[key], [key === 'xAxis' ? 'xAxisIndex' : 'yAxisIndex']: 0,
        filterMode: filter ? 'filter' : 'none',
        zoomOnMouseWheel: false, moveOnMouseMove: false, moveOnMouseWheel: false,
        preventDefaultMouseMove: false
      });
    }
    if (zoom.length) {
      out.dataZoom = zoom;
      // A custom series (a box, an interval) does not clip unless told.
      if (Array.isArray(option.series)) {
        out.series = option.series.map((/** @type {any} */ s) =>
          (s && s.type === 'custom' ? { ...s, clip: true } : s));
      }
    }
    return { option: out, roles: r, filterX, logEnds };
  };

  NS.zoomOption = { decorate, roles, ends, axisValue, hasLimits, ZOOM_ID };
})();
// --- chart/gear.js
// @ts-check
/**
 * Chart block: what the chart tells the core gear engine (VizGearHost)
 * beyond the spec: the type tiles, which axis is required, choices drawn
 * from the data or the board (sort orders, column values, drill targets),
 * per-type labels and rows that wait for another setting.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  const TILES = [
    { label: 'Aggregated', values: ['bar', 'histogram', 'waterfall', 'pie', 'treemap',
                                    'boxplot', 'pointrange', 'band'] },
    { label: 'Individual', values: ['scatter', 'line', 'area', 'gantt'] }
  ];
  const NUMERIC_Y = ['scatter', 'line', 'area', 'band'];
  /** Distinct values offered in a picker of a column's values. */
  const MAX_LEVELS = 500;
  const FACET_COLS = [1, 2, 3, 4, 6];
  /** Distinct values a column may have to be offered as a facet, or, when it
   * holds numbers, as a category. */
  const MAX_FACET_LEVELS = 10;
  const MAX_FACET_ROW_LEVELS = 20;
  const MAX_CATEGORY_LEVELS = 50;
  /** The panel scale choices in a grid (facet_row). */
  const GRID_SCALES = [{ value: 'fixed', label: 'Shared across panels' },
                       { value: 'free', label: 'Free per row and column' },
                       { value: 'free_y', label: 'Free per row' }];

  /** @param {string} type */
  const autoLabel = (type) => {
    const fam = NS.familyOf(type);
    if (type === 'histogram') return 'Auto, the clicked bin';
    if (fam === 'aggregated') return 'Auto, the clicked group';
    if (fam === 'timeline') return 'Auto, the clicked lane';
    return NS.isLineType(type) ? 'Auto, the clicked series' : 'Auto, the selected point';
  };

  // Aggregations that read any column (they count distinct values); the others need numbers.
  const ANY_COLUMN = ['count_distinct', 'pct_distinct'];

  /** Whether an aggregated chart only counts rows, so `value` is unused.
   * @param {string} type @param {Record<string, any>} v */
  const counts = (type, v) => !NS.DISTRIBUTION_TYPES.includes(type) && (v.func || 'count') === 'count';

  /** The values of a column a picker offers. A column the chart's cells do not hold comes listed
   * by R (derived.levels). @param {any} view @param {string} col */
  const levelsOf = (view, col) => {
    if (!col) return [];
    const sent = view.derived.levels && view.derived.levels[col];
    const all = sent ? sent.map(String)
      : view._ix.levels(view.data, col, 'nz').filter((/** @type {string} */ x) => x !== '');
    return all.slice(0, MAX_LEVELS).map((/** @type {string} */ x) => ({ value: x, label: x }));
  };

  /** @param {any} view The chart view: its rows, column meta and derived. @returns {VizGearHost} */
  function host(view) {
    const meta = () => view.columns;
    /** @param {string} col */
    /** @param {string} col */
    const levels = (col) => levelsOf(view, col);
    /** @param {string} name */
    const numeric = (name) => {
      return V.util.isNumeric(meta().find((/** @type {VizColumn} */ c) => c.name === name));
    };
    return {
      required(key, v) {
        const type = String(v.type || 'bar');
        if (type === 'histogram') return key === 'x';
        if (NS.familyOf(type) !== 'aggregated') return key === 'x' || key === 'y';
        return key === 'category' || (key === 'value' && !counts(type, v));
      },
      label(key, v) {
        const type = String(v.type || 'bar');
        if (type === 'band' && key === 'x') return 'Timeline';
        if (type === 'band' && key === 'y') return 'Value';
        // As 0.2: an aggregated chart or a histogram has "Reference lines". A bar
        // also takes them on its category axis, so its two rows name their axis.
        // Scatter, line and area keep the spec's Horizontal and Vertical.
        if ((key === 'value_lines' || key === 'x_lines') && NS.familyOf(type) === 'aggregated') {
          if (type !== 'bar') return 'Reference lines';
          return key === 'x_lines' ? 'Reference lines (category axis)' : 'Reference lines (value axis)';
        }
        if (key === 'band_size' && v.band_window === 'fixed') return 'Half-width (x units)';
        // The aggregated types take y_limits on their value axis, whichever way it lies.
        if (key === 'y_limits' && NS.familyOf(type) === 'aggregated') return 'Value limits';
        if (key === 'y_ticks' && NS.familyOf(type) === 'aggregated') return 'Value ticks';
        return null;
      },
      visible(key, v) {
        if (key === 'percent_of') return v.func === 'pct_distinct';
        if (key === 'value') return !counts(String(v.type || 'bar'), v);
        if (key === 'drill_target' || key === 'drill_table') return !!v.drill;
        // Panel scales apply to a wrap and a grid; a grid has its own columns.
        if (key === 'facet_scales') return !!(v.facet || v.facet_row);
        if (key === 'facet_cols') return !v.facet_row;
        return true;
      },
      tiles: (key) => (key === 'type' ? TILES : null),
      columns(key, v, cols) {
        const type = String(v.type || 'bar');
        const num = ['ref_min', 'ref_max', 'ymin', 'ymax'].includes(key) ||
          (key === 'y' && NUMERIC_Y.includes(type)) || (key === 'x' && type === 'histogram') ||
          (key === 'value' && !ANY_COLUMN.includes(String(v.func || 'count')));
        if (num) return cols.filter((c) => numeric(c.name));
        // As 0.2: a facet offers columns with at most 10 distinct values, a
        // category columns that are not numbers or have at most 50. That keeps
        // a prepare script's helper columns and the measures out of them.
        if (key === 'facet') return cols.filter((c) => c.n_unique <= MAX_FACET_LEVELS);
        // Grid rows stack down the card, so they take more levels.
        if (key === 'facet_row') return cols.filter((c) => c.n_unique <= MAX_FACET_ROW_LEVELS);
        if (key === 'category') {
          return cols.filter((c) => c.class !== 'number' || c.n_unique <= MAX_CATEGORY_LEVELS);
        }
        return cols;
      },
      choices(key, v) {
        const type = String(v.type || 'bar');
        if (key === 'drill') return [{ value: 'auto', label: autoLabel(type) }];
        if (key === 'drill_target') {
          const list = view.derived.ctrl_choices || [];
          return [{ value: '', label: 'None' }].concat(list);
        }
        if (key === 'facet_cols') return FACET_COLS.map((n) => ({ value: n, label: n + ' per row' }));
        // A grid frees its scales by row and by column, not by panel.
        if (key === 'facet_scales' && v.facet_row) return GRID_SCALES;
        if (key === 'sort_by') return sortChoices(type, meta());
        if (key === 'waterfall_totals') return levels(String(v.category ?? ''));
        if (key === 'highlight') {
          const h = v.highlight ? Object.keys(/** @type {object} */ (v.highlight))[0] : null;
          return h ? levels(h) : [];
        }
        return null;
      },
      // The axis a ticks setting is on and the ticks it draws now (ticks.js).
      ticks: (key, v) => NS.ticks.axisFor(view, key, v),
      placeholder(key) {
        if (key === 'unit') return 'e.g. CHF';
        if (key === 'title') return 'e.g. AEs by {ARM}';
        if (key === 'subtitle') return 'e.g. {@func} of {label(@value)}[, by {label(@color)}]';
        if (key === 'caption') return 'e.g. {filters} or N = {n} records';
        return null;
      }
    };
  }

  /** @param {string} type @param {VizColumn[]} meta */
  function sortChoices(type, meta) {
    const fixed = type === 'gantt'
      ? [{ value: 'onset', label: 'Onset' }, { value: 'alpha', label: 'A–Z' }]
      : [{ value: 'value', label: 'By value' }, { value: 'data', label: 'Data order' },
         { value: 'alpha', label: 'A–Z' }];
    const cols = meta.filter(V.util.isNumeric)
      .map((c) => ({ value: c.name, label: c.name, note: c.label && c.label !== c.name ? c.label : '' }));
    return fixed.concat(cols);
  }

  NS.gear = { host };
})();
// --- chart/selection.js
// @ts-check
/**
 * Chart block: the selection and what goes to R. The view owns one.
 *
 * A click always sends. Latched, the clicked mark's filter becomes the
 * selection and a second click on the same mark clears it; with a
 * drill_target a click, a point or a brush is an event for another block
 * (drill()): nothing latches, the mark gets a trace that fades, and the
 * footer shows a receipt of it for a moment. A second click on the mark
 * just sent clears it. A latched selection dims the
 * marks it does not light; the view's highlight() draws that.
 *
 * The selection is kept in the shape R reads (chart-keys.js). A latched brush
 * on a scatter is the range of the brushed points; ECharts clears a brush by
 * itself on some redraws, so a clear that did not come from the reader is
 * told apart (`suppressBrushClear`, `byBrush`).
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  // How long a redraw's brush clear is ignored: ECharts reports it on the
  // draw or the frame after.
  const REDRAW_MS = 150;

  /** The same columns with the same values, in any order.
   * @param {Record<string, any[]>} a @param {Record<string, any[]>} b */
  const sameFilters = (a, b) => {
    const norm = (/** @type {Record<string, any[]>} */ x) => JSON.stringify(
      Object.keys(x || {}).sort().map((k) => [k, (x[k] || []).map(String).sort()]));
    return norm(a) === norm(b);
  };

  class Selection {
    /** @param {ChartView} view */
    constructor(view) {
      this.view = view;
      /** What the chart has latched, in R's shape. @type {ChartSelection | null} */
      this.current = null;
      /** Whether a brush made `current`. */
      this.byBrush = false;
      /** Whether a brush rectangle may still be drawn on a panel. */
      this.brushDrawn = false;
      /** A click in brush mode also fires a brush clear: true while it is ignored. */
      this.suppressBrushClear = false;
      /** The saved selection R last sent, as JSON. @type {string | null} */
      this._saved = null;
      /** The clicked mark's light. @type {HTMLElement | null} */
      this._signal = null;
      /** The last drill sent to a target, for a second click to take back.
       * @type {ChartSelection | null} */
      this.sent = null;
      /** The last peek sent, as JSON (peek()). */
      this._peeked = 'null';
    }

    /**
     * A saved selection R sent back. Only a changed one replaces what the
     * reader sees; a chart with a drill_target restores none.
     * @param {unknown} saved
     */
    restore(saved) {
      const key = JSON.stringify(saved ?? null);
      if (key === this._saved) return;
      this._saved = key;
      this.current = NS.transientDrill(this.view.settings) ? null : NS.keys.restore(saved);
      this.byBrush = false;
    }

    /**
     * A click on marks of a level (a bar, a slice, a line, a legend chip).
     * @param {ChartSlot | null} slot @param {any} params ECharts' click, for the flash
     * @param {Record<string, any[]>} filters
     */
    click(slot, params, filters) {
      const v = this.view;
      if (NS.transientDrill(v.settings)) {
        // A second click on the mark just sent takes it back: the data view
        // lets go and the target is cleared.
        const s = this.sent;
        if (s && s.type === 'categorical' && sameFilters(s.filters, filters)) {
          this.sent = null;
          v.frame.flash('Drill cleared');
          this.send(null);
          return;
        }
        this.drill({ type: 'categorical', filters }, () => this.flash(slot, params));
        return;
      }
      const c = this.current;
      if (c && c.type === 'categorical' && sameFilters(c.filters, filters)) {
        this.clear();
        return;
      }
      this.current = { type: 'categorical', filters };
      this.byBrush = false;
      v.highlight();
      this.send(this.current);
    }

    /**
     * A drill sent to another block: the mark's light, the footer's receipt,
     * the selection once. Nothing latches.
     * @param {ChartSelection} sel @param {() => void} light
     */
    drill(sel, light) {
      this.sent = sel;
      light();
      this.view.frame.flash('Drilled down to ' + this.describe(sel));
      this.send(sel);
    }

    /**
     * A point click or a brush, latched: a chart without a drill_target. A
     * second click on the same point clears it, as on a bar.
     * @param {ChartSelection} sel @param {boolean} brush
     */
    latch(sel, brush) {
      const c = this.current;
      if (!brush && sel.type === 'range' && c && c.type === 'range' && !this.byBrush &&
          JSON.stringify(c) === JSON.stringify(sel)) {
        this.clear();
        return;
      }
      this.current = sel;
      this.byBrush = brush;
      this.brushDrawn = true;
      this.refresh();
      this.send(sel);
    }

    /** A redraw empties the brush before the family draws it again, and
     * ECharts reports that as a clear. For a moment, it is not the reader's:
     * with a drill target it would release the claim the brush sent. */
    redrawing() {
      this.suppressBrushClear = true;
      this.view.scope.timeout(() => { this.suppressBrushClear = false; }, REDRAW_MS);
    }

    /** The brush was cleared: so is the selection, unless a click just made
     * it, drill is off, where the brush only peeked (the peek goes), or the
     * brush was a drill, which the target owns from then on. */
    brushCleared() {
      if (this.suppressBrushClear) return;
      if (NS.drillState(this.view.settings) === 'off') {
        this.peek(null);
        return;
      }
      if (NS.transientDrill(this.view.settings)) return;
      this.current = null;
      this.byBrush = false;
      this.brushDrawn = false;
      this.refresh();
      this.send(null);
    }

    /** The clear button, a click on empty plot space, a second click. */
    clear() {
      const v = this.view;
      this.current = null;
      this.sent = null;
      this.byBrush = false;
      this.brushDrawn = false;
      for (const chart of v.charts) chart.dispatchAction({ type: 'brush', areas: [] });
      v.highlight();
      this.send(null);
    }

    /** The footer, and the marks when something dims or was dimmed. */
    refresh() {
      const v = this.view;
      if (this.current || v._slots.some((s) => s && s.dimmed)) v.highlight();
      else v.status();
    }

    /**
     * A brush on a chart with drill off: it filters nothing, but the data
     * view shows what it caught. Sent as `<id>_peek`, once per change.
     * @param {ChartSelection | null} sel
     */
    peek(sel) {
      const key = JSON.stringify(sel);
      if (key === this._peeked) return;
      this._peeked = key;
      const id = this.view.el.id;
      if (id) V.util.send(id, 'peek', { selection: sel, nonce: Date.now() });
    }

    /** What R gets: `{selection, nonce}`, `selection` null for a clear.
     * @param {ChartSelection | null} sel */
    send(sel) {
      const id = this.view.el.id;
      if (id) V.util.select(id, /** @type {Record<string, unknown> | null} */ (sel));
    }

    /** The footer's words for the selection, in column labels; null for none:
     * the line shows only while something is selected. */
    words() {
      return this.current ? 'Filtered: ' + this.describe(this.current) : null;
    }

    /** A selection in column labels. @param {ChartSelection} sel */
    describe(sel) {
      const v = this.view;
      const m = v._memo.model;
      return sel.type === 'range'
        ? NS.keys.describeRange(sel, v.columns, m && m.x ? m.x.type : undefined)
        : NS.keys.describe(sel.filters, v.columns);
    }

    /**
     * Light the clicked mark briefly: a trace on its box, or a ring at the
     * click where the mark has no box. One light at a time.
     * @param {ChartSlot | null} slot @param {any} params
     */
    flash(slot, params) {
      const host = slot && slot.chartDiv;
      const ev = params && params.event;
      if (!host || !ev) return;
      const t = ev.target;
      const boxy = t && t.shape &&
        typeof t.shape.width === 'number' && typeof t.shape.height === 'number';
      if (boxy && t.getBoundingRect) {
        const r = t.getBoundingRect().clone();
        if (t.transform) r.applyTransform(t.transform);
        if (this.trace(slot, r, t.shape.r)) return;
      }
      const x = ev.offsetX, y = ev.offsetY;
      if (typeof x !== 'number' || typeof y !== 'number') return;
      const ring = NS.mk('span', 'vz-chart-pulse');
      Object.assign(ring.style, { left: x + 'px', top: y + 'px' });
      this._light(host, ring, 1200);
    }

    /**
     * The trace a clicked bar gets, on a box in the slot's pixels: a bar, or
     * the rectangle a drill brush took. False when the box is empty.
     * @param {ChartSlot | null} slot
     * @param {{ x: number, y: number, width: number, height: number }} r
     * @param {number | number[]} [rad] the mark's corner radius
     */
    trace(slot, r, rad) {
      const host = slot && slot.chartDiv;
      if (!host || !(r.width > 0 && r.height > 0)) return false;
      const box = NS.mk('span', 'vz-chart-flash');
      Object.assign(box.style, { left: r.x + 'px', top: r.y + 'px',
                                 width: r.width + 'px', height: r.height + 'px' });
      if (rad != null) {
        box.style.borderRadius = (Array.isArray(rad) ? rad : [rad])
          .map((/** @type {number} */ x) => x + 'px').join(' ');
      }
      this._light(host, box, 2000);
      return true;
    }

    /** A light that removes itself; the timeout covers an off-screen block.
     * @param {HTMLElement} host @param {HTMLElement} el @param {number} ms */
    _light(host, el, ms) {
      const scope = this.view.scope;
      const drop = () => {
        el.remove();
        if (this._signal === el) this._signal = null;
      };
      if (this._signal) this._signal.remove();
      scope.on(el, 'animationend', drop);
      this._signal = el;
      host.appendChild(el);
      scope.timeout(drop, ms);
    }
  }

  NS.Selection = Selection;
})();
// --- chart/family-aggregated.js
// @ts-check
/**
 * Chart block: the aggregated family (bar, histogram, waterfall, pie,
 * treemap, box plot, point range) as the view uses it. Its model and option
 * are in model-aggregated.js and option-aggregated.js.
 *
 * A click on a bar segment, slice, tile, box or point range selects the
 * mark's keys, or with a drill column that column's values in the rows
 * under it. Every draw ends with the highlight patch.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** @type {ChartFamily} */
  const aggregated = {
    name: 'aggregated',
    model: NS.model.aggregated,
    option: NS.option.aggregated,
    legend: NS.option.aggregatedLegend,
    widths: true,
    alwaysHighlight: true,

    // The handler reads the view's live state: an instance outlives many draws.
    attach(view, slot) {
      slot.chart.on('click', (/** @type {any} */ params) => {
        if (NS.zoom.justDragged(slot)) return;
        const cfg = view.settings;
        if (NS.drillState(cfg) === 'off') return;
        const m = view._memo.model;
        if (!m || m.family !== 'aggregated') return;
        const at = NS.model.aggregatedMarkAt(m, cfg, params);
        if (!at) return;
        const keys = NS.model.aggregatedKeys(cfg, slot.facetVal, at.group, at.level);
        const filters = NS.model.aggregatedClick(cfg, keys,
          () => NS.model.rowsUnder(view.data, view._ix, keys));
        if (filters) view.selection.click(slot, params, filters);
      });
    },

    // A range selection only exists on the individual family.
    patch: (c) => NS.option.aggregatedPatch(c.m, c.panel, c.cfg, c.current,
                                            c.range ? null : c.lit, c.dimmed, c.ink)
  };

  NS.families = Object.assign(NS.families || {}, { aggregated });
})();
// --- chart/family-timeline.js
// @ts-check
/**
 * Chart block: the timeline family (gantt) as the view uses it. Its model
 * and option are in model-timeline.js and option-timeline.js. A click on a
 * bar selects its lane, or with a drill column that column's values. The
 * time axis stays in view while a tall chart scrolls (stickyAxis()).
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** The first non-transparent background above `el`, else white.
   * @param {Element | null} el */
  const backdrop = (el) => {
    for (; el; el = el.parentElement) {
      const bg = getComputedStyle(el).backgroundColor;
      if (bg && bg !== 'transparent' && bg !== 'rgba(0, 0, 0, 0)') return bg;
    }
    return '#ffffff';
  };

  /** A strip above the panel, sticky at the top of whatever scrolls, holds a copy of the
   * canvas band the time axis is drawn in (`band` px high). It shows once the axis has
   * scrolled out of view and is copied again after every render while it shows. The axis
   * itself stays on the canvas, so the picture and the downloads have it.
   * @param {ChartSlot} slot @param {number} band */
  const stickyAxis = (slot, band) => {
    const scope = slot.scope;
    if (!scope || !band) return;
    let strip = slot.axisStrip;
    if (!strip || !strip.isConnected) {
      strip = slot.axisStrip = /** @type {HTMLCanvasElement} */ (
        scope.node(document.createElement('canvas')));
      strip.className = 'vz-chart-axis-strip';
      strip.setAttribute('aria-hidden', 'true');
      slot.container.insertBefore(strip, slot.chartDiv);
      const s = strip;
      let stuck = false;
      let queued = false;
      const copy = () => {
        const src = slot.chartDiv.querySelector('canvas');
        const w = src ? src.width : 0;
        const css = slot.chartDiv.clientWidth;
        if (!src || !w || !css) return;
        const h = Math.round(Number(s.dataset.band) * w / css);
        if (s.width !== w) s.width = w;
        if (s.height !== h) s.height = h;
        const ctx = s.getContext('2d');
        if (!ctx) return;
        ctx.clearRect(0, 0, w, h);
        ctx.drawImage(src, 0, 0, w, h, 0, 0, w, h);
      };
      // Stuck: the strip sits lower than the panel's top edge, which has scrolled past.
      const update = () => {
        queued = false;
        const now = s.getBoundingClientRect().top - slot.chartDiv.getBoundingClientRect().top > 1;
        if (now === stuck) return;
        stuck = now;
        if (stuck) copy();
        s.style.visibility = stuck ? 'visible' : 'hidden';
      };
      // Any scroll moves the panel: the capture listener sees every scroller's.
      scope.on(document, 'scroll', () => {
        if (!queued) { queued = true; scope.frame(update); }
      }, { capture: true, passive: true });
      slot.chart.on('finished', () => { if (stuck) copy(); });
    }
    strip.dataset.band = String(band);
    strip.style.height = band + 'px';
    strip.style.marginBottom = -band + 'px';
    strip.style.background = backdrop(slot.container);
  };

  /** @type {ChartFamily} */
  const timeline = {
    name: 'timeline',
    model: NS.model.timeline,
    option: NS.option.timeline,
    legend: NS.option.timelineLegend,
    widths: false,
    alwaysHighlight: false,

    attach(view, slot) {
      slot.chart.on('click', (/** @type {any} */ params) => {
        if (NS.zoom.justDragged(slot)) return;
        if (NS.drillState(view.settings) === 'off') return;
        if (params.componentType !== 'series' || !params.value) return;
        const filters = NS.model.timelineClick(view.settings, slot.facetVal, params.value);
        if (filters) view.selection.click(slot, params, filters);
      });
    },

    after(slot, po, view) {
      if (!view.still) stickyAxis(slot, po.axisBand);
    },

    patch: (c) => NS.option.timelinePatch(c.m, c.panel, c.cfg, c.range ? null : c.lit, c.dimmed,
                                          c.ink)
  };

  NS.families = Object.assign(NS.families || {}, { timeline });
})();
// --- chart/family-individual.js
// @ts-check
/**
 * Chart block: the individual family (scatter, line, area, band) as the
 * view uses it: clicks, the brush and the hover pickers. Its model
 * and option are in model-individual.js and option-individual.js.
 *
 *   - A click selects what the model says the mark is: a level as a
 *     categorical selection, or a scatter point as a zero-width range. A
 *     line click waits 275 ms, so a double-click (the zoom reset, zoom.js)
 *     cancels it.
 *   - The scatter brush selects the brushed points' extent, their series, or
 *     an explicit drill column's values in their rows. Points outside the
 *     data select nothing. A range taken in a facet panel carries the panel's
 *     key.
 *   - The hover pickers find the nearest line or band level from the
 *     cursor's position on every move, so a dropped mouseout cannot leave a
 *     line highlighted.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  // How long a click guards against the brush clear that follows it (a line
  // longer), and how long a line click waits for a second click.
  const GUARD_MS = 150;
  const LINE_GUARD_MS = 450;
  const LINE_HOLD_MS = 275;
  // How far (px) the cursor may be from a line, or a band's centre line, to hover it.
  const HOVER_PX = 14;
  const BAND_HOVER_PX = 60;

  /** The band level whose centre line is nearest the cursor vertically (at the cursor's x),
   * or null outside the plot or far from every line.
   * @param {ChartSlot} slot @param {any} chart @param {number} px @param {number} py
   * @returns {string | null} */
  const nearestBandLevel = (slot, chart, px, py) => {
    const b = slot.band;
    if (!b) return null;
    if (!chart.containPixel({ gridIndex: 0 }, [px, py])) return null;
    const opt = chart.getOption();
    const all = (opt && opt.series) || [];
    let best = null, bestD = Infinity;
    for (const lvl of Object.keys(b.centerIdx)) {
      const sv = all[b.centerIdx[lvl]];
      if (!sv || !Array.isArray(sv.data)) continue;
      // The nearest point by x stands in for the line: the grid is dense.
      let py2 = null, dx = Infinity;
      for (const pt of sv.data) {
        if (pt[1] == null) continue;
        const q = chart.convertToPixel({ gridIndex: 0 }, [pt[0], pt[1]]);
        if (!q) continue;
        const d = Math.abs(q[0] - px);
        if (d < dx) { dx = d; py2 = q[1]; }
      }
      if (py2 == null) continue;
      const d = Math.abs(py2 - py);
      if (d < bestD) { bestD = d; best = lvl; }
    }
    return bestD <= BAND_HOVER_PX ? best : null;
  };

  /** Show the focused level's ribbon, empty the others. @param {ChartSlot} slot @param {any} chart */
  const applyBandFocus = (slot, chart) => {
    const b = slot.band;
    if (!b || !b.ribbonIdx) return;
    /** @type {any[]} */ const patch = new Array(b.total);
    for (let i = 0; i < b.total; i++) patch[i] = {};
    for (const lvl of Object.keys(b.ribbonIdx)) {
      const r = b.ribbonIdx[lvl];
      const on = slot.bandFocus != null && String(slot.bandFocus) === String(lvl);
      patch[r.idx] = { data: on ? [r.item] : [] };
    }
    chart.setOption({ series: patch }, { lazyUpdate: true });
  };

  /** The line nearest a cursor pixel (found in data space, then checked on screen), or null
   * when none is within HOVER_PX.
   * @param {ChartSlot} slot @param {any} chart @param {number} px @param {number} py
   * @returns {number | null} */
  const nearestLineSeries = (slot, chart, px, py) => {
    const f = slot.focus;
    if (!f) return null;
    let cd;
    // The data axes by index: the veil's axes share the grid.
    try { cd = chart.convertFromPixel({ xAxisIndex: 0, yAxisIndex: 0 }, [px, py]); } catch (e) { return null; }
    if (!cd || cd[0] == null || cd[1] == null) return null;
    const cx = +cd[0], cy = +cd[1];
    // On a log axis, near is measured in logs, as the screen shows it.
    const yt = f.yLog ? (/** @type {number} */ v) => Math.log10(Math.max(v, 1e-300)) : (/** @type {number} */ v) => v;
    const xOrder = f.xAxisType === 'category' ? f.xOrder : null;
    const series = f.series;
    let bestSi = null, bestY = null, bestX = null, bestDy = Infinity;
    for (let i = 0; i < series.length; i++) {
      const s = series[i];
      if (!s || s.id === '__focus__' || s.type !== 'line' ||
          !Array.isArray(s.data) || !s.data.length) continue;
      const yv = NS.model.interpYAtX(s.data, cx, xOrder);
      if (yv == null) continue;
      const dy = Math.abs(yt(yv) - yt(cy));
      if (dy < bestDy) { bestDy = dy; bestSi = i; bestY = yv; bestX = s.data[0][0]; }
    }
    if (bestSi == null) return null;
    // Near in data can be far on screen; the series' first x converts on a
    // category axis, where cx is a bare index.
    let bp;
    try { bp = chart.convertToPixel({ seriesIndex: bestSi }, [bestX, bestY]); } catch (e) { return bestSi; }
    if (bp && Math.abs(bp[1] - py) > HOVER_PX) return null;
    return bestSi;
  };

  /** RGB of the surface a slot is painted on: the theme's background, else the first
   * opaque ancestor's, else white. @param {ChartView} view @param {ChartSlot} slot
   * @returns {number[]} */
  const plotSurface = (view, slot) => {
    // A fully transparent colour does not count as a surface.
    /** @param {string} s */
    const rgbOf = (s) => (/^rgba\(.*,\s*0(\.0*)?\)$/.test(s) ? null : NS.rgbOf(s));
    if (view.theme && slot.chart) {
      const opt = slot.chart.getOption();
      const got = rgbOf(String(opt && opt.backgroundColor || ''));
      if (got) return got;
    }
    for (let el = /** @type {HTMLElement | null} */ (slot.chartDiv); el && el.nodeType === 1;
         el = el.parentElement) {
      const got = rgbOf(getComputedStyle(el).backgroundColor);
      if (got) return got;
    }
    return [255, 255, 255];
  };

  /** Fill the overlay with the hovered line, a little wider, a dot at each of its points
   * (where the measurements are; as 0.2 drew them), and the veil fading the rest.
   * @param {ChartView} view @param {ChartSlot} slot @param {any} chart @param {number} si */
  const promoteFocus = (view, slot, chart, si) => {
    const f = slot.focus;
    if (!f) return;
    const s = f.series[si];
    if (!s || s.id === '__focus__' || s.type !== 'line' ||
        !Array.isArray(s.data) || !s.data.length) return;
    slot.focusSi = si;
    const clr = (s.itemStyle && s.itemStyle.color) || null;
    // Read now, so a light/dark switch after the draw is followed.
    const bg = plotSurface(view, slot);
    f.veil = 'rgba(' + bg.join(',') + ',0.6)';
    chart.setOption({
      series: [{
        // One constant datum: renderItem draws the veil from the grid.
        id: '__scrim__', data: [[0.5, 0.5]]
      }, {
        id: '__focus__', data: s.data, step: f.stepMode || undefined,
        smooth: f.smoothOn, smoothMonotone: f.smoothOn ? 'x' : undefined,
        showSymbol: false, lineStyle: { color: clr, width: f.focusW, opacity: 1 },
        areaStyle: null
      }, {
        id: '__focus_dots__', data: s.data, symbol: 'circle', symbolSize: 8,
        itemStyle: { color: clr, opacity: 0.8, borderColor: 'rgb(' + bg.join(',') + ')', borderWidth: 2 }
      }]
    });
  };

  /** Empty the overlay. @param {ChartSlot} slot @param {any} chart */
  const demoteFocus = (slot, chart) => {
    if (!slot.focus || slot.focusSi == null) return;
    slot.focusSi = null;
    chart.setOption({
      series: [{ id: '__scrim__', data: [] },
               { id: '__focus__', data: [] }, { id: '__focus_dots__', data: [] }]
    });
  };

  /** Hover: show the nearest band level's ribbon; in a crowd of lines (one per
   * series), promote the nearest line over a veil, so a subject can be
   * followed. Every other chart's hover is the tooltip alone (quietHover()).
   * @param {ChartView} view @param {ChartSlot} slot @param {any} chart @param {any} zr */
  const hoverEvents = (view, slot, chart, zr) => {
    zr.on('mousemove', (/** @type {any} */ e) => {
      if (view.settings.type === 'band' && slot.band) {
        const lvl = nearestBandLevel(slot, chart, e.offsetX, e.offsetY);
        if (lvl !== slot.bandFocus) {
          slot.bandFocus = lvl;
          applyBandFocus(slot, chart);
        }
      }
      if (!slot.focus) return;
      // The nearest line is what the tooltip names and a click takes.
      const si = nearestLineSeries(slot, chart, e.offsetX, e.offsetY);
      if (si === slot.focusSi) return;
      slot.hover.si = si;
      if (si == null) demoteFocus(slot, chart);
      else promoteFocus(view, slot, chart, si);
    });
    zr.on('globalout', () => {
      if (view.settings.type === 'band' && slot.band && slot.bandFocus != null) {
        slot.bandFocus = null;
        applyBandFocus(slot, chart);
      }
      if (!slot.focus) return;
      slot.hover.si = null;
      demoteFocus(slot, chart);
    });
  };

  /** A click on a mark: what the model says it is, latched or sent.
   * @param {ChartView} view @param {ChartSlot} slot @param {any} params @param {string} [focusName] */
  const clickMark = (view, slot, params, focusName) => {
    const m = view._memo.model;
    if (!m || m.family !== 'individual') return;
    const panel = NS.model.panelOf(m, slot.facetVal);
    const sel = NS.model.individualClick(m, view.settings, panel, params, focusName);
    if (!sel) return;
    if (sel.type === 'categorical') view.selection.click(slot, params, sel.filters);
    else if (NS.transientDrill(view.settings)) {
      view.selection.drill(sel, () => view.selection.flash(slot, params));
    } else view.selection.latch(sel, false);
  };

  /** @param {ChartView} view @param {ChartSlot} slot @param {any} chart */
  const clickEvents = (view, slot, chart) => {
    chart.on('click', (/** @type {any} */ params) => {
      if (NS.drillState(view.settings) === 'off' || NS.zoom.justDragged(slot)) return;
      if (params.componentType !== 'series') return;
      const isLine = NS.isLineType(view.settings.type);
      const sel = view.selection;
      // A click in brush mode also fires a brush clear: ignore it for a moment.
      sel.suppressBrushClear = true;
      view.scope.timeout(() => { sel.suppressBrushClear = false; },
                         isLine ? LINE_GUARD_MS : GUARD_MS);
      if (isLine) {
        // The hovered line, taken now, before the cursor drifts.
        const focusName = slot.focus?.series[/** @type {number} */ (slot.focusSi)]?.name;
        if (slot.lineClick) slot.lineClick();
        slot.lineClick = view.scope.timeout(() => {
          slot.lineClick = null;
          clickMark(view, slot, params, focusName);
        }, LINE_HOLD_MS);
        return;
      }
      clickMark(view, slot, params);
    });
  };

  /** What the brush caught, as a selection, or null.
   * @param {ChartView} view @param {ChartSlot} slot
   * @param {Array<{ seriesIndex: number, dataIndex: number }>} selected */
  const caughtSelection = (view, slot, selected) => {
    const m = view._memo.model;
    if (!m || m.family !== 'individual') return null;
    const panel = NS.model.panelOf(m, slot.facetVal);
    const opt = slot.chart.getOption();
    const caught = NS.model.individualBrush(m, view.settings, panel, (opt && opt.series) || [], selected);
    return NS.model.brushSelection(m, view.settings, panel, caught);
  };

  /** The other panels' brushes go; their clears are not the reader's.
   * @param {ChartView} view @param {ChartSlot} slot */
  const clearOthers = (view, slot) => {
    const others = view.charts.filter((c) => c !== slot.chart);
    if (!others.length) return;
    view.selection.suppressBrushClear = true;
    for (const other of others) other.dispatchAction({ type: 'brush', areas: [] });
    view.scope.timeout(() => { view.selection.suppressBrushClear = false; }, GUARD_MS);
  };

  /** What the brush caught, latched; the other panels' brushes cleared.
   * @param {ChartView} view @param {ChartSlot} slot
   * @param {Array<{ seriesIndex: number, dataIndex: number }>} selected */
  const brushed = (view, slot, selected) => {
    const m = view._memo.model;
    if (!m || m.family !== 'individual') return;
    clearOthers(view, slot);
    const sel = caughtSelection(view, slot, selected);
    if (!sel) return;
    // The rectangle drawn again after a redraw catches what it caught.
    const key = JSON.stringify(sel);
    if (key === JSON.stringify(view.selection.current)) return;
    slot.brushSel = key;
    view.selection.latch(sel, true);
  };

  /**
   * The rectangle a brush area covers, in the slot's pixels. A lineX brush
   * spans the grid's height.
   * @param {ChartSlot} slot @param {any} area
   * @returns {{ x: number, y: number, width: number, height: number } | null}
   */
  const areaRect = (slot, area) => {
    const rg = area && area.range;
    if (!Array.isArray(rg) || !rg.length) return null;
    if (Array.isArray(rg[0])) {
      const [[x0, x1], [y0, y1]] = rg;
      return { x: Math.min(x0, x1), y: Math.min(y0, y1),
               width: Math.abs(x1 - x0), height: Math.abs(y1 - y0) };
    }
    /** @type {any} */
    let grid = null;
    try {
      grid = slot.chart.getModel().getComponent('grid').coordinateSystem.getRect();
    } catch (e) { grid = null; }
    const top = grid ? grid.y : 0;
    const height = grid ? grid.height : slot.chartDiv.clientHeight;
    return { x: Math.min(rg[0], rg[1]), y: top, width: Math.abs(rg[1] - rg[0]), height };
  };

  /**
   * A brush on a chart with a drill_target, on release: what it caught goes
   * to the target once, the rectangle goes, and a trace of it fades, as on a
   * clicked bar. ECharts dimmed the other points while the reader dragged.
   * @param {ChartView} view @param {ChartSlot} slot @param {any} area
   */
  const drillBrush = (view, slot, area) => {
    const selected = slot.brushCaught;
    slot.brushCaught = null;
    clearOthers(view, slot);
    const sel = selected && selected.length ? caughtSelection(view, slot, selected) : null;
    const rect = areaRect(slot, area);
    slot.brushAreas = null;
    slot.chart.dispatchAction({ type: 'brush', areas: [] });
    if (!sel) return;
    view.selection.drill(sel, () => { if (rect) view.selection.trace(slot, rect); });
  };

  /** @param {ChartView} view @param {ChartSlot} slot @param {any} chart */
  const brushEvents = (view, slot, chart) => {
    chart.on('brushSelected', (/** @type {any} */ params) => {
      const batch = params.batch && params.batch[0];
      if (!batch) return;
      const selected = (batch.selected || []).flatMap((/** @type {any} */ s) =>
        (s.dataIndex || []).map((/** @type {number} */ di) =>
          ({ seriesIndex: s.seriesIndex, dataIndex: di })));
      // Drill off: the brush filters nothing; the data view shows what it
      // caught. An empty batch is a redraw or a clear: the brush event drops
      // the peek when the rectangle goes.
      if (NS.drillState(view.settings) === 'off') {
        const sel = selected.length ? caughtSelection(view, slot, selected) : null;
        if (sel) {
          slot.brushSel = JSON.stringify(sel);
          view.selection.peek(sel);
        }
        return;
      }
      // With a drill target the brush is sent once, when the drag ends.
      if (NS.transientDrill(view.settings)) {
        slot.brushCaught = selected;
        return;
      }
      // An empty selection clears only a selection a brush made: ECharts
      // also sends one when the cursor is armed on every draw, which would
      // drop a restored range or a point click.
      if (!selected.length) {
        if (view.selection.current && view.selection.byBrush) view.selection.brushCleared();
        return;
      }
      brushed(view, slot, selected);
    });
    chart.on('brush', (/** @type {any} */ params) => {
      const areas = params.areas || [];
      // Kept in data coordinates: a redraw drops the rectangle (after()).
      slot.brushAreas = areas.length ? areas.map((/** @type {any} */ a) => ({
        brushType: a.brushType, coordRange: a.coordRange, xAxisIndex: 0,
        ...(a.brushType === 'rect' ? { yAxisIndex: 0 } : {})
      })) : null;
      if (!areas.length) view.selection.brushCleared();
    });
    // ECharts reports the last selection before the end of the drag; the
    // timeout leaves room for one that comes after it.
    chart.on('brushEnd', (/** @type {any} */ params) => {
      if (NS.drillState(view.settings) === 'off' || !NS.transientDrill(view.settings)) return;
      const areas = (params && params.areas) || [];
      if (!areas.length) return;
      const area = areas[areas.length - 1];
      view.scope.timeout(() => drillBrush(view, slot, area), 0);
    });
  };

  /** @type {ChartFamily} */
  const individual = {
    name: 'individual',
    model: NS.model.individual,
    option: NS.option.individual,
    legend: NS.option.individualLegend,
    widths: true,
    alwaysHighlight: false,

    attach(view, slot) {
      const chart = slot.chart;
      const zr = chart.getZr();
      hoverEvents(view, slot, chart, zr);
      clickEvents(view, slot, chart);
      brushEvents(view, slot, chart);
    },

    // The series as drawn by this option, unless ECharts holds another set.
    patch(c) {
      const drawn = c.drawn && c.drawn.series && c.drawn.series.length === c.current.series.length
        ? c.drawn.series : c.current.series;
      return NS.option.individualPatch(c.m, c.panel, drawn, c.lit, c.dimmed, c.filters, c.range,
                                       c.ink);
    },

    // The slot takes the panel's hover state and pickers; the hover does not
    // survive a redraw.
    prepare(slot, po) {
      slot.hover = po.hover;
      slot.hover.si = null;
      slot.focusSi = null;
      slot.focus = po.focus;
      slot.band = po.band;
      if (po.focus) po.focus.veil = undefined;
    },

    actions: (slot, po) => !!(po.brushable || slot.brushable),

    // The brush cursor on a brushable scatter, unless its drag zooms
    // (zoom.js), released when the panel stops being one; a hovered band
    // level's ribbon.
    after(slot, po, view) {
      const chart = slot.chart;
      /** @param {string} key @param {Record<string, any>} opt */
      const cursor = (key, opt) => chart.dispatchAction({ type: 'takeGlobalCursor', key, ...opt });
      if (po.brushable && !view.zoomOn) {
        cursor('brush', { brushOption: { brushType: 'rect', brushMode: 'single' } });
        // The redraw dropped the rectangle; it stays while its selection or
        // its peek (drill off) does.
        const sel = view.selection;
        if (slot.brushAreas && (slot.brushSel === JSON.stringify(sel.current) ||
                                slot.brushSel === sel._peeked)) {
          chart.dispatchAction({ type: 'brush', areas: slot.brushAreas });
        }
      } else if (slot.brushable) {
        cursor('brush', { brushOption: { brushType: false } });
      }
      slot.brushable = po.brushable;
      if (slot.band && slot.bandFocus != null) applyBandFocus(slot, chart);
    }
  };

  NS.families = Object.assign(NS.families || {}, { individual });
})();
// --- chart/zoom.js
// @ts-check
/**
 * Chart block: drag to zoom, on every chart with a continuous axis.
 *
 *   - A drag on the plot draws a box; on release both axes zoom to it. A
 *     flat drag (under 10 px tall) zooms x only and refits y; a narrow one
 *     zooms y only. On the aggregated types only the value axis zooms, so
 *     the box spans the categories.
 *   - On a scatter the drag is the brush, a selection or a drill. A
 *     Select / Zoom switch in the header row gives the drag to the zoom
 *     instead; Select is the default.
 *   - A double-click or the Reset zoom link goes back to the full view: the
 *     axis limits when set (zoom-option.js), else the data.
 *   - The zoom is a view: it is not saved and sets no limits. Limits are
 *     the `x_limits` and `y_limits` settings (zoom-option.js).
 * The window lives on the slot and is drawn again after a redraw; it is not
 * part of the block state.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const ZO = NS.zoomOption;

  /** A drag shorter than this on an axis leaves that axis alone (px). */
  const FLAT_PX = 10;
  /** How long a click after a drag is ignored (ms). */
  const AFTER_DRAG_MS = 300;
  const KEYS = /** @type {('xAxis' | 'yAxis')[]} */ (['xAxis', 'yAxis']);

  /** The plot area in the slot's pixels.
   * @param {any} chart @returns {{ x: number, y: number, width: number, height: number } | null} */
  const gridRect = (chart) => {
    const g = chart.getModel().getComponent('grid', 0);
    const cs = g && g.coordinateSystem;
    return cs && cs.getRect ? cs.getRect() : null;
  };

  /** @param {number} v @param {number} lo @param {number} hi */
  const clamp = (v, lo, hi) => Math.min(hi, Math.max(lo, v));

  /** Whether the drag on this slot zooms (false: a scatter in Select mode).
   * @param {ChartView} view @param {ChartSlot} slot */
  const armed = (view, slot) => !!slot.zoomRoles && (!slot.brushable || view.zoomOn === true);

  /**
   * What a drag from `a` to `b` zooms: per axis, the pixel span or null.
   * @param {ChartSlot} slot @param {any} g @param {{ x: number, y: number }} a @param {{ x: number, y: number }} b
   */
  const spans = (slot, g, a, b) => {
    const r = /** @type {any} */ (slot.zoomRoles);
    const x0 = clamp(Math.min(a.x, b.x), g.x, g.x + g.width), x1 = clamp(Math.max(a.x, b.x), g.x, g.x + g.width);
    const y0 = clamp(Math.min(a.y, b.y), g.y, g.y + g.height), y1 = clamp(Math.max(a.y, b.y), g.y, g.y + g.height);
    const wide = x1 - x0 >= FLAT_PX, tall = y1 - y0 >= FLAT_PX;
    return { xAxis: r.xAxis && wide ? [x0, x1] : null, yAxis: r.yAxis && tall ? [y0, y1] : null };
  };

  /** The box drawn while dragging, or null where ECharts draws nothing.
   * @param {any} zr */
  const makeBox = (zr) => {
    const G = root.echarts && root.echarts.graphic;
    if (!G || !G.Rect || !zr.add) return null;
    const box = new G.Rect({
      shape: { x: 0, y: 0, width: 0, height: 0 }, silent: true, z: 1e4,
      style: { fill: 'rgba(0, 114, 178, 0.1)', stroke: 'rgba(0, 114, 178, 0.5)', lineWidth: 1 }
    });
    zr.add(box);
    return box;
  };

  /** Send the slot's window to ECharts, or the full view when it has none.
   * @param {ChartSlot} slot */
  const apply = (slot) => {
    const chart = slot.chart;
    if (!chart || !slot.zoomRoles) return;
    const z = slot.zoom || {};
    // x filters what it leaves out so y refits, unless y has its own window:
    // the filter would move it.
    if (chart.__vzFilterX && slot.zoomRoles.xAxis) {
      chart.setOption({ dataZoom: [{ id: ZO.ZOOM_ID.xAxis, filterMode: z.yAxis ? 'none' : 'filter' }] });
    }
    // A zoomed value axis labels its ticks only, not the window's raw ends.
    /** @type {Record<string, any>} */
    const labels = {};
    for (const k of KEYS) {
      const comp = slot.zoomRoles[k] ? chart.getModel().getComponent(k, 0) : null;
      if (!comp || comp.get('type') !== 'value') continue;
      const off = z[k] ? false : null;
      labels[k] = [{ axisLabel: { showMinLabel: off, showMaxLabel: off } }];
    }
    // ECharts' window does not map onto a log axis: a log axis zooms by its
    // min and max, back to its limits (or auto) on a reset.
    const logEnds = chart.__vzLogEnds || {};
    for (const k of KEYS) {
      if (!logEnds[k] || !slot.zoomRoles[k]) continue;
      const w = z[k] || logEnds[k];
      labels[k] = [{ min: w[0] ?? null, max: w[1] ?? null }];
    }
    if (Object.keys(labels).length) chart.setOption(labels);
    const batch = KEYS.filter((k) => slot.zoomRoles && slot.zoomRoles[k] && !logEnds[k]).map((k) => (z[k]
      ? { dataZoomId: ZO.ZOOM_ID[k], startValue: z[k][0], endValue: z[k][1] }
      : { dataZoomId: ZO.ZOOM_ID[k], start: 0, end: 100 }));
    if (batch.length) chart.dispatchAction({ type: 'dataZoom', batch });
    NS.ticks.zoomed(slot, slot.zoom);
  };

  /** @param {ChartSlot} slot */
  const showLinks = (slot) => { slot.resetBtn.hidden = !slot.zoom; };

  /** @param {ChartView} view @param {ChartSlot} slot */
  const reset = (view, slot) => {
    slot.zoom = null;
    apply(slot);
    showLinks(slot);
  };

  const DAY_MS = 864e5;
  /** A window widened to tidy ends, so the axis does not label a raw value:
   * whole days on a time axis, else a hundredth of the span's order.
   * @param {number[]} v @param {string} type */
  const tidy = (v, type) => {
    const span = v[1] - v[0];
    if (!(span > 0)) return v;
    if (type === 'log') return v.map((x) => Number(x.toPrecision(2)));
    const step = type === 'time' ? (span > 2 * DAY_MS ? DAY_MS : 0)
      : Math.pow(10, Math.floor(Math.log10(span)) - 1);
    if (!step) return v;
    const r = (/** @type {number} */ x) => Number(x.toPrecision(12));
    return [r(Math.floor(v[0] / step) * step), r(Math.ceil(v[1] / step) * step)];
  };

  /** Zoom to the pixel spans: each to data values, within the current view.
   * @param {ChartView} view @param {ChartSlot} slot @param {ReturnType<typeof spans>} s */
  const zoomTo = (view, slot, s) => {
    const chart = slot.chart;
    /** @type {Record<string, number[] | null>} */
    const next = { ...(slot.zoom || {}) };
    for (const k of KEYS) {
      const px = s[k];
      if (!px) continue;
      const finder = k === 'xAxis' ? { xAxisIndex: 0 } : { yAxisIndex: 0 };
      const v = px.map((p) => Number(chart.convertFromPixel(finder, p)));
      if (!v.every(Number.isFinite)) continue;
      const comp = chart.getModel().getComponent(k, 0);
      next[k] = tidy([Math.min(v[0], v[1]), Math.max(v[0], v[1])],
                     String(comp ? comp.get('type') : 'value'));
    }
    // A flat drag on a chart that zooms both ways: y refits to what x shows.
    if (s.xAxis && !s.yAxis && slot.zoomRoles && slot.zoomRoles.yAxis) next.yAxis = null;
    slot.zoom = next.xAxis || next.yAxis ? next : null;
    apply(slot);
    showLinks(slot);
  };

  const ICONS = {
    select: '<svg width="14" height="14" viewBox="0 0 16 16" aria-hidden="true"><rect x="2.5" y="2.5" ' +
      'width="11" height="11" rx="1" fill="none" stroke="currentColor" stroke-dasharray="2 1.6"/></svg>',
    zoom: '<svg width="14" height="14" viewBox="0 0 16 16" aria-hidden="true" fill="none" ' +
      'stroke="currentColor" stroke-width="1.4" stroke-linecap="round"><circle cx="6.8" cy="6.8" r="4.3"/>' +
      '<path d="M10 10l3.8 3.8M4.8 6.8h4M6.8 4.8v4"/></svg>'
  };

  /** The Select / Zoom switch in the header row, while the view draws a
   * brushable scatter: blockr.ui's segmented control at row size, icons
   * with their names as tooltips.
   * @param {ChartView} view */
  const modeSwitch = (view) => {
    const show = view._slots.some((s) => s && s.brushable);
    let el = view._modeEl;
    if (!el) {
      if (!show) return;
      el = view._modeEl = document.createElement('span');
      el.className = 'blockr-segmented blockr-segmented--xs vz-chart-zoom-mode';
      el.setAttribute('role', 'group');
      el.setAttribute('aria-label', 'Drag selects or zooms');
      for (const [on, name] of /** @type {[boolean, 'select' | 'zoom'][]} */ ([[false, 'select'], [true, 'zoom']])) {
        const b = document.createElement('button');
        b.type = 'button';
        b.className = 'blockr-segmented__seg';
        b.innerHTML = ICONS[name];
        b.title = name === 'zoom' ? 'Drag to zoom' : 'Drag to select';
        b.setAttribute('aria-label', b.title);
        b.dataset.zoom = String(on);
        view.scope.on(b, 'click', () => setMode(view, on));
        el.appendChild(b);
      }
      view.frame.tools.prepend(el);
    }
    el.hidden = !show;
    for (const b of Array.from(el.children)) {
      const on = /** @type {HTMLElement} */ (b).dataset.zoom === String(view.zoomOn === true);
      b.classList.toggle('is-selected', on);
      b.setAttribute('aria-pressed', String(on));
    }
  };

  /** Give the scatter drag to the zoom (true) or the brush.
   * @param {ChartView} view @param {boolean} on */
  const setMode = (view, on) => {
    view.zoomOn = on;
    for (const slot of view._slots) {
      if (slot && slot.chart && slot.brushable) {
        slot.chart.dispatchAction({ type: 'takeGlobalCursor', key: 'brush',
                                    brushOption: { brushType: on ? false : 'rect', brushMode: 'single' } });
      }
    }
    modeSwitch(view);
  };

  /** Whether a click on the slot is the end of a drag.
   * @param {ChartSlot} slot */
  const justDragged = (slot) => !!slot.zoomDragAt && Date.now() - slot.zoomDragAt < AFTER_DRAG_MS;

  /** The drag, the double-click and the links, once per ECharts instance.
   * @param {ChartView} view @param {ChartSlot} slot */
  const attach = (view, slot) => {
    const chart = slot.chart;
    const zr = chart.getZr();
    /** @type {{ a: { x: number, y: number }, g: any } | null} */
    let drag = null;
    /** @type {any} */
    let box = null;
    const end = () => {
      drag = null;
      if (box) { zr.remove(box); box = null; }
    };
    zr.on('mousedown', (/** @type {any} */ e) => {
      if (!armed(view, slot) || (e.event && e.event.button)) return;
      const g = gridRect(chart);
      if (!g || e.offsetX < g.x || e.offsetX > g.x + g.width ||
          e.offsetY < g.y || e.offsetY > g.y + g.height) return;
      drag = { a: { x: e.offsetX, y: e.offsetY }, g };
    });
    zr.on('mousemove', (/** @type {any} */ e) => {
      if (!drag) return;
      const s = spans(slot, drag.g, drag.a, { x: e.offsetX, y: e.offsetY });
      if (!s.xAxis && !s.yAxis) return;
      if (!box) box = makeBox(zr);
      if (!box) return;
      const g = drag.g;
      const [x0, x1] = s.xAxis || [g.x, g.x + g.width];
      const [y0, y1] = s.yAxis || [g.y, g.y + g.height];
      box.setShape({ x: x0, y: y0, width: x1 - x0, height: y1 - y0 });
    });
    zr.on('mouseup', (/** @type {any} */ e) => {
      if (!drag) return;
      const s = spans(slot, drag.g, drag.a, { x: e.offsetX, y: e.offsetY });
      end();
      if (!s.xAxis && !s.yAxis) return;
      slot.zoomDragAt = Date.now();
      zoomTo(view, slot, s);
    });
    zr.on('globalout', end);
    // A double-click resets the zoom and cancels the line click it queued.
    zr.on('dblclick', () => {
      if (slot.lineClick) {
        slot.lineClick();
        slot.lineClick = null;
      }
      if (slot.zoom) reset(view, slot);
    });
    // The slot's scope goes with this chart; the next chart registers again.
    const scope = /** @type {VizScope} */ (slot.scope);
    scope.on(slot.resetBtn, 'click', () => reset(view, slot));
  };

  /**
   * After a draw: the slot takes the option's axis roles, and its window
   * comes back where the axes still zoom.
   * @param {ChartView} view @param {ChartSlot} slot @param {number} i
   * @param {ReturnType<typeof ZO.roles>} roles
   */
  const after = (view, slot, i, roles) => {
    const same = !!roles && !!slot.zoomRoles &&
      KEYS.every((k) => !!(roles && roles[k]) === !!(slot.zoomRoles && slot.zoomRoles[k]));
    slot.zoomRoles = roles;
    if (!same) slot.zoom = null;
    if (slot.zoom) apply(slot);
    showLinks(slot);
    if (i === 0) modeSwitch(view);
  };

  NS.zoom = { attach, after, reset, setMode, justDragged, spans, tidy };
})();
// --- chart/export.js
// @ts-check
/**
 * Chart block: the chart as one picture. picture() composes the titles,
 * every panel at its place in the grid, the legend band and the caption on
 * one canvas. The view sends it to R when the reader opens the download
 * control; the capture service draws a chart R asks for in a hidden host
 * and answers with it.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** The bands' plain text. @param {VizTitles | null | undefined} titles
   * @returns {Record<string, string>} */
  const titleText = (titles) => Object.fromEntries(['title', 'subtitle', 'caption']
    .map((k) => [k, String((titles && /** @type {any} */ (titles)[k] &&
                            /** @type {any} */ (titles)[k].text) || '')]));

  /** A function that breaks a text block into lines at the picture width:
   * newlines first, then word wrap.
   * @param {number} W @param {number} pad @param {(font: string) => (s: string) => number} measureFont */
  const lineWrapper = (W, pad, measureFont) => {
    /** @param {{ text: string, font: string, color: string }} b */
    return (b) => {
      const width = measureFont(b.font);
      const lines = [];
      for (const src of String(b.text).split(/\r?\n/)) {
        let cur = '';
        for (const w of src.split(/\s+/).filter(Boolean)) {
          const cand = cur ? cur + ' ' + w : w;
          if (cur && width(cand) > W - 2 * pad) {
            lines.push(cur);
            cur = w;
          } else {
            cur = cand;
          }
        }
        lines.push(cur);
      }
      const px = parseFloat((b.font.match(/(\d+(?:\.\d+)?)px/) || ['', '13'])[1]);
      return { lines, lineH: Math.round(px * 1.4) };
    };
  };

  /** Everything the picture is laid out from: the wrapped title and caption
   * blocks, the band heights, the legend's place, the canvas size, the background.
   * @param {ChartView} view @param {DOMRect} gridR @param {number} pr */
  const exportContext = (view, gridR, pr) => {
    const frame = view.frame;
    const text = titleText(view.derived.titles);
    const pad = 12;
    // Text styles come from the live bands, so the picture follows the theme.
    /** @param {Element} el @param {string} fb */
    const fontOf = (el, fb) => {
      const cs = getComputedStyle(el);
      if (!cs.fontSize) return fb + ' ' + view._ink.face;
      return (cs.fontStyle === 'italic' ? 'italic ' : '') + cs.fontWeight + ' ' + cs.fontSize +
        ' ' + cs.fontFamily;
    };
    /** @param {Element} el @param {string} fb */
    const colorOf = (el, fb) => getComputedStyle(el).color || fb;
    const W = Math.round(gridR.width);
    const wrapLines = lineWrapper(W, pad, (font) => view._measureFont(font));
    /** @param {string} key @param {Element} el @param {string} font @param {string} color */
    const block = (key, el, font, color) => {
      if (!text[key]) return null;
      const b = { text: text[key], font: fontOf(el, font), color: colorOf(el, color) };
      return { ...b, ...wrapLines(b) };
    };
    const top = /** @type {any[]} */ ([block('title', frame.titleEl, '600 15px', '#1f2937'),
                                       block('subtitle', frame.subEl, '13px', '#6b7280')])
      .filter(Boolean);
    const cap = block('caption', frame.capEl, 'italic 11px', '#6b7280');
    const topH = top.length
      ? pad + top.reduce((a, b) => a + b.lines.length * b.lineH, 0) + 6 : pad / 2;
    const capH = cap ? 6 + cap.lines.length * cap.lineH + pad : pad;
    const legendEl = view.legend.el;
    const legR = legendEl.offsetParent !== null ? legendEl.getBoundingClientRect() : null;
    const legH = legR ? legR.height : 0;
    const legTop = !!legR && legendEl.classList.contains('vz-chart-legend--top');
    const H = Math.round(topH + gridR.height + legH + capH);

    let bg = getComputedStyle(frame.el).backgroundColor;
    if (!bg || bg === 'transparent' || bg === 'rgba(0, 0, 0, 0)') bg = '#ffffff';
    return { pad, W, H, pr, gridR, fontOf, colorOf, top, cap, topH, legendEl, legR, legH, legTop, bg };
  };

  /** Each panel's bitmap, loaded; a panel whose picture fails is null.
   * @param {any[]} slots @param {number} pr @returns {Promise<any[]>} */
  const panelBitmaps = (slots, pr) => Promise.all(slots.map((/** @type {any} */ s) =>
    new Promise((resolve) => {
      const im = new Image();
      im.onload = () => resolve({ im, slot: s });
      im.onerror = () => resolve(null);
      im.src = s.chart.getDataURL({
        pixelRatio: pr, backgroundColor: 'transparent', excludeComponents: ['toolbox']
      });
    })));

  /** A wrapped text block from `y`; returns the y below it.
   * @param {CanvasRenderingContext2D} ctx @param {any} b @param {number} x @param {number} y */
  const paintBlock = (ctx, b, x, y) => {
    ctx.font = b.font;
    ctx.fillStyle = b.color;
    ctx.textBaseline = 'top';
    for (const ln of b.lines) {
      ctx.fillText(ln, x, y);
      y += b.lineH;
    }
    return y;
  };

  /** Every panel at its place in the grid, with its facet label.
   * @param {CanvasRenderingContext2D} ctx @param {any} k @param {any[]} panels */
  const paintPanels = (ctx, k, panels) => {
    const { gridR, fontOf, colorOf, topH, legH, legTop } = k;
    const y = topH + (legTop ? legH : 0);
    for (const p of panels) {
      if (!p) continue;
      const r = p.slot.chartDiv.getBoundingClientRect();
      const x0 = r.left - gridR.left;
      const y0 = y + (r.top - gridR.top);
      if (p.slot.labelEl) {
        const lr = p.slot.labelEl.getBoundingClientRect();
        ctx.font = fontOf(p.slot.labelEl, '600 11px');
        ctx.fillStyle = colorOf(p.slot.labelEl, '#6b7280');
        ctx.textBaseline = 'top';
        ctx.textAlign = 'center';
        ctx.fillText(p.slot.labelEl.textContent || '',
                     lr.left - gridR.left + lr.width / 2, y + (lr.top - gridR.top) + 2);
        ctx.textAlign = 'left';
      }
      ctx.drawImage(p.im, x0, y0, r.width, r.height);
    }
  };

  /** A panel grid's strips where they are on screen: a column strip on one
   * line, cut to its width; a row strip turned a quarter, its lines wrapped
   * at the strip's height as the page wraps them.
   * @param {CanvasRenderingContext2D} ctx @param {any} k @param {ChartView} view */
  const paintStrips = (ctx, k, view) => {
    const { gridR, fontOf, colorOf, topH, legH, legTop } = k;
    const y = topH + (legTop ? legH : 0);
    for (const el of view._stripEls || []) {
      const r = el.getBoundingClientRect();
      if (!r.width || !r.height) continue;
      const font = fontOf(el, '600 12px');
      const color = colorOf(el, '#6b7280');
      let text = el.textContent || '';
      if (getComputedStyle(el).textTransform === 'uppercase') text = text.toUpperCase();
      const cx = r.left - gridR.left + r.width / 2, cy = y + (r.top - gridR.top) + r.height / 2;
      ctx.font = font;
      ctx.fillStyle = color;
      ctx.textAlign = 'center';
      ctx.textBaseline = 'middle';
      if (el.classList.contains('vz-chart-strip--row')) {
        const wrap = lineWrapper(r.height, 0, (f) => view._measureFont(f))({ text, font, color });
        const n = wrap.lines.length;
        ctx.save();
        ctx.translate(cx, cy);
        ctx.rotate(Math.PI / 2);
        // Turned clockwise, the first line is the rightmost, as vertical-rl.
        wrap.lines.forEach((ln, i) => ctx.fillText(ln, 0, (i - (n - 1) / 2) * wrap.lineH));
        ctx.restore();
      } else {
        const width = view._measureFont(font);
        let t = text;
        while (t.length > 1 && width(t) > r.width) t = t.slice(0, -2) + '\u2026';
        ctx.fillText(t, cx, cy);
      }
    }
    ctx.textAlign = 'left';
    ctx.textBaseline = 'top';
  };

  /** Legend chips where they are on screen; a dimmed chip keeps its opacity.
   * @param {CanvasRenderingContext2D} ctx @param {any} k */
  const paintLegend = (ctx, k) => {
    const { gridR, fontOf, colorOf, topH, legendEl, legR, legTop } = k;
    const bandY = legTop ? topH : topH + gridR.height;
    /** @param {Element} el2 */
    const drawLabel = (el2) => {
      const r2 = el2.getBoundingClientRect();
      ctx.font = fontOf(el2, '11px');
      ctx.fillStyle = colorOf(el2, '#333');
      ctx.textBaseline = 'middle';
      ctx.fillText(el2.textContent || '', r2.left - gridR.left,
                   bandY + (r2.top - legR.top) + r2.height / 2);
    };
    for (const child of Array.from(legendEl.children)) {
      const alpha = parseFloat(getComputedStyle(/** @type {Element} */ (child)).opacity || '1');
      ctx.globalAlpha = Number.isFinite(alpha) ? alpha : 1;
      const sw = /** @type {Element} */ (child).querySelector('.vz-chart-legend-swatch');
      if (sw) {
        const sr = sw.getBoundingClientRect();
        const swCs = getComputedStyle(sw);
        ctx.fillStyle = swCs.backgroundColor || '#999';
        const sx = sr.left - gridR.left, sy = bandY + (sr.top - legR.top);
        // A dot is round, a square and a line have small corners.
        const rad = Math.min(parseFloat(swCs.borderTopLeftRadius) || 0, sr.width / 2, sr.height / 2);
        if (rad > 0 && typeof ctx.roundRect === 'function') {
          ctx.beginPath();
          ctx.roundRect(sx, sy, sr.width, sr.height, rad);
          ctx.fill();
        } else {
          ctx.fillRect(sx, sy, sr.width, sr.height);
        }
        if (sw.nextElementSibling) drawLabel(sw.nextElementSibling);
      } else {
        drawLabel(/** @type {Element} */ (child));
      }
    }
    ctx.globalAlpha = 1;
    ctx.textBaseline = 'top';
  };

  /** The bands and the panels on one canvas, or null without a 2d context.
   * @param {any} k @param {any[]} panels @param {ChartView} view
   * @returns {HTMLCanvasElement | null} */
  const composeCanvas = (k, panels, view) => {
    const { W, H, pr, bg, legR, cap } = k;
    const canvas = document.createElement('canvas');
    canvas.width = W * pr;
    canvas.height = H * pr;
    const ctx = canvas.getContext('2d');
    if (!ctx) return null;
    ctx.scale(pr, pr);
    ctx.fillStyle = bg;
    ctx.fillRect(0, 0, W, H);
    let y = k.pad;
    for (const b of k.top) y = paintBlock(ctx, b, k.pad, y);
    paintPanels(ctx, k, panels);
    paintStrips(ctx, k, view);
    if (legR) paintLegend(ctx, k);
    if (cap) paintBlock(ctx, cap, k.pad, k.topH + k.gridR.height + k.legH + 6);
    return canvas;
  };

  /**
   * The chart as one picture, at `pixelRatio` (else R's capture_ratio).
   * Rejects when nothing is drawn or the grid has no size.
   * @param {ChartView} view @param {{ pixelRatio?: number }} [opts]
   * @returns {Promise<VizPicture>}
   */
  const picture = (view, opts = {}) => {
    const slots = view._slots.filter((s) => s.chart);
    if (!slots.length) return Promise.reject(new Error('the chart has nothing drawn'));
    const gridR = view.chartGrid.getBoundingClientRect();
    if (!gridR.width || !gridR.height) return Promise.reject(new Error('the chart has no size'));
    const pr = opts.pixelRatio || view.derived.capture_ratio || 2;
    const k = exportContext(view, gridR, pr);
    return panelBitmaps(slots, pr).then((panels) => {
      const canvas = composeCanvas(k, panels, view);
      if (!canvas) throw new Error('the browser cannot draw the picture');
      return { png: canvas.toDataURL('image/png'), width: k.W, height: k.H };
    });
  };

  // Capture service: R asks for a picture of a block at a given box
  // (`blockr-viz:capture`: the payload plus `req`, `width`, `height`); a
  // fresh view draws it in a host core/capture.js parks off screen.

  // The capture view draws still, so a panel is complete when ECharts says
  // it finished rendering. A panel that never says so (a host the browser
  // does not lay out) is taken after this long.
  const FINISH_MS = 3000;
  let captureSeq = 0;

  /** Resolves when every panel has finished rendering; at once for an empty
   * state. @param {ChartView} view @param {VizScope} scope @returns {Promise<void>} */
  const whenDrawn = (view, scope) => {
    const charts = view._slots.filter((s) => s.chart).map((s) => s.chart);
    /** @param {any} chart @returns {Promise<void>} */
    const finished = (chart) => new Promise((resolve) => chart.on('finished', () => resolve()));
    const late = new Promise((resolve) => scope.timeout(() => resolve(undefined), FINISH_MS));
    return Promise.race([Promise.all(charts.map(finished)), late]).then(() => {});
  };

  /** @param {ChartViewClass} View @param {ChartCaptureMessage} msg @returns {Promise<VizPicture>} */
  const capturePicture = (View, msg) => {
    const scope = new V.Scope();
    const p = V.capture({
      width: msg.width, height: msg.height,
      draw: (/** @type {HTMLElement} */ host) => new Promise((resolve, reject) => {
        const el = host.appendChild(document.createElement('div'));
        el.id = 'vz-capture-chart-' + (++captureSeq);
        el.style.cssText = 'width:' + msg.width + 'px;height:' + msg.height + 'px;';
        const view = scope.own(new View(el, scope, { still: true }));
        // The board's theme first, so the view draws at once (view.js).
        Promise.resolve(NS.themeReady(msg.derived))
          .then(() => {
            view.render(msg);
            return whenDrawn(view, scope);
          })
          .then(() => picture(view, { pixelRatio: msg.ratio }))
          .then(resolve, reject);
      })
    });
    return p.finally(() => scope.dispose());
  };

  /** Register the capture service's message for the chart's views.
   * @param {ChartViewClass} View */
  const registerCapture = (View) => {
    const shiny = root.Shiny;
    if (!shiny || typeof shiny.addCustomMessageHandler !== 'function') return;
    shiny.addCustomMessageHandler('blockr-viz:capture', (/** @type {ChartCaptureMessage} */ msg) =>
      V.captureReply(msg.req, capturePicture(View, msg)));
  };

  Object.assign(NS, { picture, titleText, registerCapture });
})();
// --- chart/panels.js
// @ts-check
/**
 * Chart block: the facet panels' layout. A panel grid (facet_row) puts each
 * panel at its row and column with strips on top and on the right; the
 * panels' numeric axes are shared across a wrap or along a grid's rows and
 * columns. Every grid panel labels its axes; each axis title shows once
 * per row or column.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const mk = NS.mk;
  // What an x axis title takes below the labels, and the least bottom margin
  // that keeps the labels when the title is dropped.
  const X_NAME_ROOM = 16;
  const X_LABELS_BOTTOM = 30;

  /** In a grid, every row gets its grid key (NS.model.gridKey()). New keys
   * are new rows to the model and the index.
   * @param {ChartView} view @param {boolean} rowsChanged */
  const gridRows = (view, rowsChanged) => {
    const g = view.settings.facet_grid;
    const keep = NS.model.gridKeepsMissing(view.settings);
    const key = g ? g.row + '\u0000' + (g.col || '') + '\u0000' + keep : '';
    if (!rowsChanged && key === view._gridKeyOf) return;
    view._gridKeyOf = key;
    if (!g) return;
    const col = NS.model.GRID_COL;
    for (const r of view.data) r[col] = NS.model.gridKey(r, g, keep);
    if (!rowsChanged) {
      view._ix = new NS.RowIndex();
      view._rowsGen++;
    }
  };

  /** The grid's rows and columns, in level order, and the grid row the
   * panels start on (2 below a strip of column labels), or null without a
   * grid. A combination without rows has no panel and leaves its cell empty.
   * @param {ChartView} view @param {any} m @returns {ChartGridLayout | null} */
  const gridLayout = (view, m) => {
    const g = view.settings.facet_grid;
    if (!g || !m.panels.length) return null;
    const rows = new Set(), cols = new Set();
    for (const p of m.panels) {
      if (p.facet == null || p.facet === '__all__') return null;
      const lv = NS.model.gridLevels(p.facet);
      rows.add(lv.row);
      cols.add(lv.col);
    }
    const order = (/** @type {Set<string>} */ s) => [...s].sort();
    // A strip reads "(missing)" for a missing level and, when show_n covers
    // facets, counts the rows (or ids) its panels draw.
    const drawn = view.data.filter((r) => r[NS.model.GRID_COL] != null);
    /** @param {string | null} col */
    const labeller = (col) => {
      const counts = col && NS.countOn(view.settings, 'facet') ? NS.labelCounts(drawn, view.settings, col) : null;
      return (/** @type {string} */ lv) => {
        const n = counts ? NS.withCount(lv, counts).slice(lv.length) : '';
        return (lv === '' ? '(missing)' : lv) + n;
      };
    };
    const rowLab = labeller(g.row), colLab = labeller(g.col);
    const r = order(rows), c = order(cols);
    return { rows: r, cols: c, top: g.col ? 2 : 1, rowLabels: r.map(rowLab), colLabels: c.map(colLab) };
  };

  /** A panel's cell: its grid row and column, after the strip of column
   * labels when there is one. @param {ChartSlot} slot
   * @param {ChartGridLayout | null} layout @param {any} facet */
  const place = (slot, layout, facet) => {
    const st = slot.container.style;
    if (!layout) {
      st.gridRow = st.gridColumn = '';
      slot.gridRow = slot.gridCol = null;
      return;
    }
    const lv = NS.model.gridLevels(facet);
    slot.gridRow = layout.rows.indexOf(lv.row);
    slot.gridCol = layout.cols.indexOf(lv.col);
    st.gridRow = String(slot.gridRow + layout.top);
    st.gridColumn = String(slot.gridCol + 1);
  };

  /** The grid's strips: column levels on top, row levels on the right, as in
   * ggplot2's facet_grid(). Rebuilt on every draw.
   * @param {ChartView} view @param {ChartGridLayout | null} layout */
  const strips = (view, layout) => {
    for (const el of view._stripEls) el.remove();
    view._stripEls = [];
    if (!layout) return;
    const top = layout.top;
    const add = (/** @type {string} */ cls, /** @type {string} */ text,
                 /** @type {number} */ row, /** @type {number} */ col) => {
      const el = mk('div', 'vz-chart-strip ' + cls, view.chartGrid);
      el.textContent = text;
      el.title = text;
      el.style.gridRow = String(row);
      el.style.gridColumn = String(col);
      view._stripEls.push(el);
    };
    if (top === 2) {
      layout.colLabels.forEach((c, j) => add('vz-chart-strip--col', c, 1, j + 1));
    }
    layout.rowLabels.forEach((r, i) => add('vz-chart-strip--row', r, i + top, layout.cols.length + 1));
  };

  /** A row strip as tall as its row's panels at most, so a long label wraps
   * into a second line instead of making the row taller. Run once the
   * panels have their heights. @param {ChartView} view */
  const fitStrips = (view) => {
    /** @type {Map<number, number>} */ const tall = new Map();
    for (const s of view._slots) {
      if (!s || s.gridRow == null) continue;
      // The chart's own height: the container stretches to the row, which a
      // long strip may have made taller.
      const h = parseFloat(s.chartDiv.style.height) || s.chartDiv.getBoundingClientRect().height;
      tall.set(s.gridRow, Math.max(tall.get(s.gridRow) || 0, h));
    }
    let i = 0;
    for (const el of view._stripEls) {
      if (!el.classList.contains('vz-chart-strip--row')) continue;
      const h = tall.get(i++);
      el.style.maxHeight = h ? Math.floor(h) + 'px' : '';
    }
  };

  /** In a grid, which axis carries the value and how each axis is shared:
   * the value axis within a row ("free_y", "free") or across all panels
   * ("fixed"); the other axis within a column ("free") or across all. The
   * value runs along x on horizontal bars and on a gantt. Null outside a grid.
   * @param {ChartSettings} cfg
   * @returns {{ valueKey: string, otherKey: string, valueBy: 'row' | 'all', otherBy: 'col' | 'all' } | null} */
  const share = (cfg) => {
    if (!cfg.facet_grid) return null;
    const valueOnX = cfg.type === 'gantt' ||
      (NS.familyOf(cfg.type) === 'aggregated' && cfg.orientation === 'horizontal');
    return {
      valueKey: valueOnX ? 'xAxis' : 'yAxis',
      otherKey: valueOnX ? 'yAxis' : 'xAxis',
      valueBy: cfg.facet_scales === 'fixed' ? 'all' : 'row',
      otherBy: cfg.facet_scales === 'free' ? 'col' : 'all'
    };
  };

  // One numeric domain across the panels ("fixed"), read back off what each drew.
  // In a grid see share(). A shared value axis lands on a nice step.
  /** @param {ChartView} view */
  const harmonise = (view) => {
    const mode = NS.facetScales(view.settings);
    const gs = share(view.settings);
    if (mode === 'free' && !gs) return;
    const slots = view._slots.filter((s) => s && s.chart);
    if (slots.length < 2) return;
    /** @param {string} key @param {ChartSlot[]} group */
    const shareAxis = (key, group) => {
      if (group.length < 2) return;
      let type = null;
      const extents = group.map((s) => {
        const comp = s.chart.getModel().getComponent(key, 0);
        if (!comp || comp.get('type') === 'category') return null;
        type = comp.get('type');
        return comp.axis.scale.getExtent();
      });
      const d = NS.axes.sharedDomain(extents);
      if (!d) return;
      /** @type {Record<string, number>} */ let patch = { min: d[0], max: d[1] };
      const roles = group[0].chart.__vzRoles;
      // A tick step sets the shared ends on its multiples (ticks.js).
      const tickStep = type === 'value' && roles
        ? V.ticks.roundOut(NS.ticks.settingOf(view.settings, roles[key]), d[0], d[1]) : null;
      if (tickStep) {
        patch = { min: tickStep[0], max: tickStep[1] };
      } else if (type === 'value') {
        const [min, max, interval] = NS.axes.niceShared(d[0], d[1]);
        patch = { min, max, interval };
      }
      // A set limit wins over the shared domain; the axis then picks its own steps.
      const [lo, hi] = NS.zoomOption.ends(view.settings, roles ? roles[key] : null, String(type));
      if (lo != null || hi != null) patch = { min: lo ?? patch.min, max: hi ?? patch.max };
      for (const s of group) s.chart.setOption({ [key]: [patch] }, { lazyUpdate: true });
    };
    if (!gs) {
      shareAxis('xAxis', slots);
      if (mode === 'fixed') shareAxis('yAxis', slots);
      return;
    }
    for (const g of groups(slots, gs.valueBy)) shareAxis(gs.valueKey, g);
    for (const g of groups(slots, gs.otherBy)) shareAxis(gs.otherKey, g);
  };

  /** The slots by grid row, by grid column, or all in one group.
   * @param {ChartSlot[]} slots @param {'row' | 'col' | 'all'} by @returns {ChartSlot[][]} */
  const groups = (slots, by) => {
    if (by === 'all') return [slots];
    /** @type {Map<any, ChartSlot[]>} */ const out = new Map();
    for (const s of slots) {
      const k = by === 'row' ? s.gridRow : s.gridCol;
      if (!out.has(k)) out.set(k, []);
      /** @type {ChartSlot[]} */ (out.get(k)).push(s);
    }
    return [...out.values()];
  };

  /** In a grid each axis title appears once: y on the first panel of a
   * row, x on the lowest panel of a column, when that axis is shared along
   * the row or column. Every panel keeps its tick labels. A panel without
   * its x title gives that room back: the grid's bottom margin and the panel
   * shrink alike, so the plot keeps its height and the rows line up.
   * @param {ChartView} view */
  const axisTitles = (view) => {
    const gs = share(view.settings);
    const slots = view._slots.filter((s) => s && s.chart);
    for (const s of slots) s.innerTitles = null;
    if (!gs) return;
    const yAlongRow = gs.valueKey === 'yAxis' || gs.otherBy === 'all';
    const xAlongCol = gs.otherKey === 'xAxis' || gs.valueBy === 'all';
    /** @type {Map<any, number>} */ const firstCol = new Map();
    /** @type {Map<any, number>} */ const lastRow = new Map();
    for (const s of slots) {
      const c = /** @type {number} */ (s.gridCol), r = /** @type {number} */ (s.gridRow);
      if (!firstCol.has(s.gridRow) || c < /** @type {number} */ (firstCol.get(s.gridRow))) firstCol.set(s.gridRow, c);
      if (!lastRow.has(s.gridCol) || r > /** @type {number} */ (lastRow.get(s.gridCol))) lastRow.set(s.gridCol, r);
    }
    const off = { name: '' };
    for (const s of slots) {
      const inner = {
        x: xAlongCol && s.gridRow !== lastRow.get(s.gridCol),
        y: yAlongRow && s.gridCol !== firstCol.get(s.gridRow)
      };
      /** @type {Record<string, any>} */ const patch = {};
      const axis = (/** @type {string} */ key) => s.chart.getModel().getComponent(key, 0);
      if (inner.x && axis('xAxis')) patch.xAxis = [off];
      if (inner.y && axis('yAxis')) patch.yAxis = [off];
      if (!patch.xAxis && !patch.yAxis) continue;
      s.innerTitles = inner;
      const px = parseFloat(s.chartDiv.style.height);
      const grid = (s.chart.getOption().grid || [])[0] || {};
      const bottom = Number(grid.bottom);
      const cut = patch.xAxis && axis('xAxis').get('name')
        ? Math.min(X_NAME_ROOM, bottom - X_LABELS_BOTTOM) : 0;
      const shrink = cut > 0 && Number.isFinite(px);
      if (shrink) patch.grid = { bottom: bottom - cut };
      s.chart.setOption(patch, { lazyUpdate: true });
      if (!shrink) continue;
      // A later re-fit of the labels sizes the panel from this base.
      const fit = s.chart.__xFit;
      if (fit && fit.baseH != null) fit.baseH -= cut;
      if (view._setSlotHeight(s, Math.round(px - cut) + 'px')) s.chart.resize();
    }
  };

  NS.panels = { gridRows, layout: gridLayout, place, strips, fitStrips, share, harmonise, groups, axisTitles };
})();
// --- chart/view.js
// @ts-check
/**
 * Chart block: the chart view, one per chart container.
 *
 * It holds what R sent (columns, rows, config) and the reader's state (the
 * selection, the gear), and draws by running the family's model and option
 * (family-*.js): one ECharts instance per facet panel in a CSS grid.
 * The model is memoised on the rows and the config fields it reads, the
 * option on the model and its fields; a click patches opacities; a resize
 * only resizes.
 *
 * Built by the shared output binding (core/binding.js) with a Scope that
 * owns everything set up outside the element. The view owns its parts: the
 * core frame (header, titles, gear, downloads, footer), the script band,
 * the legend band, the data table (core/data-view.js) and the selection.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const mk = NS.mk;

  /** @type {VizEmpty} */
  const NO_VALUE = { kind: 'empty', message: 'Pick a numeric Value to plot its distribution' };

  /** @type {CanvasRenderingContext2D | null} */ let measureCtx = null;

  /** Theme scripts that have run, or failed to: a chart waits once. */
  const themesTried = new Set();

  /**
   * The load of the ECharts theme script R names (`derived.theme_src`,
   * R/chart-dep.R), or null when there is nothing to wait for. A theme that
   * fails to load draws as ECharts' own.
   * @param {ChartDerived | undefined} d
   * @returns {Promise<void> | null}
   */
  const themeReady = (d) => {
    const src = d && d.theme_src;
    if (!src || themesTried.has(src)) return null;
    const done = () => { themesTried.add(src); };
    return V.lazy.script(src).then(done, done);
  };

  // What a payload draws: all of it but the selection, the download menu
  // and the data view, which follow the selection. The rows are keyed by
  // the binding's `rowsChanged`.
  /** @param {VizPayload} p @returns {string} */
  const drawKey = (p) => {
    const config = Object.assign({}, p.config, { selection: null });
    const derived = Object.assign({}, p.derived, { downloads: null, data_view: null });
    return JSON.stringify([p.columns, config, derived]);
  };

  class ChartView {
    /**
     * @param {HTMLElement} el @param {VizScope} scope
     * @param {{ still?: boolean }} [opts] `still` draws without animation
     *   (the capture service's picture).
     */
    constructor(el, scope, opts = {}) {
      this.el = el;
      this.scope = scope;
      this.still = !!opts.still;
      /** @type {any[]} */ this.data = [];
      this._ix = new NS.RowIndex();
      // Bumped whenever the rows change; the model memo keys on it.
      this._rowsGen = 0;
      /** @type {VizColumn[]} */ this.columns = [];
      this._columnsKey = '';
      /** The settings the drawing reads (NS.model.settings()); R's `derived` is
       * beside them. @type {ChartSettings} */
      this.settings = /** @type {ChartSettings} */ ({});
      /** @type {ChartDerived} */ this.derived = /** @type {ChartDerived} */ ({});
      /** @type {any[]} */ this.charts = [];
      /** @type {ChartSlot[]} */ this._slots = [];
      /** @type {string | null} */ this._renderShape = null;
      /** The grid the rows' keys are for (NS.panels.gridRows()). */
      this._gridKeyOf = '';
      /** The grid's strip labels. @type {HTMLElement[]} */
      this._stripEls = [];
      /** Owns the panels' ECharts instances; a new shape gets a new one.
       * @type {VizScope | null} */
      this._shapeScope = null;
      this.drawn = false;
      /** An empty state is drawn. */
      this._blank = false;
      /** A payload held for its theme's script (themeReady()), with whether
       * a held one had new rows. @type {{ p: VizPayload, rows: boolean } | null} */
      this._held = null;
      /** @type {string | null} */ this.theme = null;
      /** @type {Ink} */ this._ink = NS.INK_DEFAULT;
      /** The family's model and option, memoised on the config fields each
       * read (memo.js); `model` and `option` are the current ones.
       * @type {{ models: any, options: any, modelGen: number, model: any, option: any }} */
      this._memo = { models: new NS.ReadMemo(), options: new NS.ReadMemo(), modelGen: 0,
                     model: null, option: null };
      /** @type {(() => void) | null} */ this._settle = null;
      /** The last payload and its drawKey(): a payload that draws the same
       * chart updates in place. @type {VizPayload | null} */
      this._payload = null;
      /** @type {string | null} */ this._drawKey = null;
      /** A scatter's drag zooms instead of brushing (zoom.js). */
      this.zoomOn = false;
      this.selection = new NS.Selection(this);
      this.frame = new V.Frame(el, scope, { cls: 'vz-chart' });
      this.gear = this.frame.mountGear({ block: 'chart', typeKey: 'type',
                                         title: 'Chart settings', host: NS.gear.host(this) });
      const body = this.frame.body;
      this.band = V.scriptBand(body, scope, this.gear);
      this.chartGrid = mk('div', 'vz-chart-grid', body);
      this.legend = new NS.Legend(this, body);
      this.dataView = new V.DataView(this.frame, scope, {
        onToggle: () => scope.frame(() => this._resizeCharts())
      });
      this.status();
      // One resize per animation frame.
      /** @type {(() => void) | null} */ let pending = null;
      scope.resize(this.chartGrid, () => {
        if (pending) return;
        pending = scope.frame(() => {
          pending = null;
          this._resizeCharts();
        });
      });
    }

    /** The family that draws the current type. @returns {ChartFamily} */
    family() { return NS.families[NS.familyOf(this.settings.type)]; }

    /** The ECharts theme R named (drilldown-theme-register.js registers
     * "blockr", a theme's own script the others, themeReady()); "default" is
     * ECharts' own. @param {ChartTheme | null | undefined} theme */
    _setTheme(theme) {
      this.theme = theme && theme !== 'default' ? theme : null;
    }

    /** A payload from R, with its rows (core/binding.js). The rows are parsed
     * and indexed only when the binding says they are new to this view.
     * @param {VizPayload} p @param {boolean} [rowsChanged] */
    render(p, rowsChanged = true) {
      const d = /** @type {ChartDerived} */ (p.derived || {});
      // A theme still on its way holds the drawing; the last payload held
      // draws when it is there, with new rows if any held payload had them.
      const wait = themeReady(d);
      if (wait) {
        const held = this._held;
        this._held = { p, rows: rowsChanged || !!(held && held.rows) };
        if (!held) {
          wait.then(() => {
            const h = this._held;
            this._held = null;
            if (h && !this.scope.disposed) this.render(h.p, h.rows);
          });
        }
        return;
      }
      // A click changes the selection, and with it the download counts and
      // the data view, but nothing the chart draws: those payloads update in
      // place. The same payload again is a theme change, which redraws.
      const key = drawKey(p);
      const same = !rowsChanged && p !== this._payload && key === this._drawKey &&
        this.drawn && !this._blank;
      this._payload = p;
      this._drawKey = key;
      if (same) {
        this._update(p);
        return;
      }
      this.derived = d;
      this.columns = p.columns || [];
      this._columnsKey = JSON.stringify(this.columns);
      if (rowsChanged) {
        this._ix = new NS.RowIndex();
        this._rowsGen++;
        this.data = NS.rowsFromPayload(p.data);
      }
      this.settings = NS.model.settings(p.config, this.columns);
      NS.panels.gridRows(this, rowsChanged);
      this.selection.restore(p.config && p.config.selection);
      this._setTheme(d.theme);
      // The gear first: the title sentences open what it offers.
      this.gear.update(p);
      this.frame.titles(d.titles);
      this.band.update(d.script);
      this.drawn = false;
      this._render();
      this._downloads();
      this.dataView.update(d.data_view);
      this.frame.busyDone();
    }

    /** A payload that draws the same chart: the selection is lit by
     * opacity patches, the gear, menu and data view take theirs.
     * @param {VizPayload} p */
    _update(p) {
      const d = /** @type {ChartDerived} */ (p.derived || {});
      this.derived = d;
      this.settings = NS.model.settings(p.config, this.columns);
      const before = JSON.stringify(this.selection.current ?? null);
      this.selection.restore(p.config && p.config.selection);
      this.gear.update(p);
      if (JSON.stringify(this.selection.current ?? null) !== before) this.highlight();
      this._downloads();
      this.dataView.update(d.data_view);
      this.frame.busyDone();
    }

    /** An error R sent: said where the chart would be. @param {string} message */
    error(message) {
      // An error after a payload held for its theme stands; the held one
      // does not draw over it.
      this._held = null;
      this._drawKey = null;
      this._showEmpty({ kind: 'error', message });
      this._downloads();
      this.frame.busyDone();
    }

    // The formats R offers. Opening the control sends the picture to R (a
    // click on a format waits for it); an empty state has none, so Image is
    // left out.
    _downloads() {
      const offer = (this.derived.downloads || []).filter((f) => !(this._blank && f.ext === 'png'));
      this.frame.downloads(this.settings.download !== false ? offer : [], () =>
        NS.picture(this).then((/** @type {VizPicture} */ pic) => V.util.send(this.el.id, 'capture', pic)));
    }

    /** The footer: what the selection holds, with its clear button. */
    status() {
      this.frame.status(this.selection.words(), () => this.selection.clear());
    }

    /** The footer, the legend chips, then the marks. @param {boolean} [lazy] */
    highlight(lazy) {
      this.status();
      this.legend.mark(this.selection.current);
      this._applyHighlight(lazy);
    }

    // The selection as opacities, one patch per panel and nothing redrawn:
    // a mark is lit when its keys match the selection's filters, or, for a
    // filter on a column it does not carry, when one of its rows does. A
    // range dims the points outside it. `lazy`: part of a draw, so ECharts
    // applies the patch with the draw's other follow-ups.
    /** @param {boolean} [lazy] */
    _applyHighlight(lazy) {
      const fam = this.family();
      const m = this._memo.model;
      const sel = this.selection.current;
      const filters = sel ? (sel.filters || null) : null;
      const range = sel && sel.type === 'range' ? sel : null;
      /** @type {ChartLit | null} */
      const lit = filters
        ? (keys, rows) => NS.keys.markLit(keys,
            rows || (() => NS.model.rowsUnder(this.data, this._ix, keys)), filters)
        : null;
      this._slots.forEach((slot, i) => {
        if (!slot || !slot.chart) return;
        const current = slot.chart.getOption();
        if (!current || !current.series) return;
        const panel = m && m.family === fam.name ? m.panels[i] : null;
        const drawn = this._memo.option && this._memo.option.panels[i];
        const patch = panel
          ? fam.patch({ m, panel, cfg: this.settings, derived: this.derived, current,
                        drawn: drawn && drawn.option, ink: this._ink,
                        lit, filters, range, dimmed: !!slot.dimmed })
          : { series: current.series.map(() => ({})), dimmed: false };
        const wasDimmed = slot.dimmed;
        slot.dimmed = patch.dimmed;
        // An empty patch on a chart that is not dimmed changes nothing.
        if (!wasDimmed && !patch.dimmed && patch.series.every((/** @type {any} */ s) =>
          !s || Object.keys(s).length === 0)) return;
        slot.chart.setOption({ series: patch.series }, lazy ? { lazyUpdate: true } : false);
      });
    }

    _render() {
      this._ink = NS.readInk(this.el);
      const empty = NS.model.gate(this.data, this.columns, this.settings, this.derived);
      if (empty) { this._showEmpty(empty); return; }
      this._blank = false;
      this._draw(this.family());
      // Once more after layout settles.
      if (this._settle) this._settle();
      this._settle = this.scope.timeout(() => { this._settleCharts(); }, 300);
    }

    // Once more after layout settles, for a panel that gets its size late:
    // a chart's first settle, and a chart whose canvas differs from its box.
    _settleCharts() {
      if (this.chartGrid.offsetParent === null) return;
      if (!this.chartGrid.clientWidth || !this.chartGrid.clientHeight) return;
      for (const slot of this._slots) {
        const c = slot && slot.chart;
        if (!c) continue;
        const div = slot.chartDiv;
        if (c.__vzSettled && c.getWidth() === div.clientWidth && c.getHeight() === div.clientHeight) {
          continue;
        }
        c.__vzSettled = true;
        c.resize();
        this._refitXLabels(c);
      }
    }

    /** @param {ChartFamily} fam */
    _draw(fam) {
      const m = this._model(fam);
      if (m.empty) { this._showEmpty(m.empty); return; }
      const grid = this.chartGrid;
      const layout = NS.panels.layout(this, m);
      grid.classList.toggle('vz-chart-grid--single', !layout && m.nFacets <= 1);
      grid.classList.toggle('vz-chart-grid--matrix', !!layout);
      // The row width when facet_cols pins it.
      const cols = Math.trunc(Number(this.settings.facet_cols) || 0);
      grid.style.gridTemplateColumns = layout
        ? `repeat(${layout.cols.length}, minmax(0, 1fr)) auto`
        : (cols > 0 && m.nFacets > 1) ? `repeat(${Math.min(cols, m.nFacets)}, minmax(0, 1fr))` : '';
      // Instances survive a redraw unless the family, panel count, grid or theme changed (an
      // ECharts theme is fixed at init).
      const shape = fam.name + '|' + m.panels.length + '|' + (this.theme || '') + '|' +
        (layout ? layout.rows.length + 'x' + layout.cols.length : '');
      if (this._renderShape !== shape) {
        this._teardownCharts();
        this._renderShape = shape;
      }
      // Every panel is laid out before any width is read, so the first panel
      // fits its labels to its own track, not to the whole grid.
      m.panels.forEach((/** @type {any} */ panel, /** @type {number} */ i) => {
        const slot = this._ensureSlot(i, layout ? null : panel.label, !layout && m.single);
        slot.facetVal = panel.facet;
        NS.panels.place(slot, layout, panel.facet);
      });
      NS.panels.strips(this, layout);
      const widths = m.panels.map((/** @type {any} */ _, /** @type {number} */ i) =>
        this._slots[i].chartDiv.clientWidth);
      const o = this._option(fam, m, fam.widths ? widths : null);
      m.panels.forEach((/** @type {any} */ panel, /** @type {number} */ i) => {
        const slot = this._slots[i];
        const po = o.panels[i];
        if (!po.option) {
          if (slot.scope) slot.scope.dispose();
          slot.scope = null;
          slot.chart = null;
          this._setSlotHeight(slot, po.height);
          slot.chartDiv.textContent = '';
          V.Frame.message(slot.chartDiv, NO_VALUE);
          return;
        }
        const hChanged = this._setSlotHeight(slot, po.height);
        const existed = !!slot.chart;
        const chart = this._ensureSlotChart(slot, fam);
        // What a resize needs to re-fit turned x labels (copied: the re-fit
        // updates it), plus the slot and its height without the gutter.
        chart.__xFit = po.xFit
          ? { ...po.xFit, slot, baseH: po.panelH - po.xFit.gutter } : undefined;
        if (fam.prepare) fam.prepare(slot, po);
        // When an action follows, ECharts runs it with the option in one update.
        const actionFollows = !!fam.actions && fam.actions(slot, po);
        if (slot.brushAreas && (this.selection.byBrush || this.selection._peeked !== 'null')) {
          this.selection.redrawing();
        }
        const dec = NS.zoomOption.decorate(po.option, this.settings);
        chart.__vzRoles = dec.roles;
        chart.__vzFilterX = dec.filterX;
        chart.__vzLogEnds = dec.logEnds;
        // The same option object on the same chart, with the same limits and ticks (which
        // decorate() reads outside the memo), and no brush rectangle to redraw: nothing to set.
        const decKey = JSON.stringify([this.settings.type, this.settings.x_limits,
          this.settings.y_limits, this.settings.x_ticks, this.settings.y_ticks]);
        const unchanged = existed && !hChanged && !actionFollows && !slot.brushAreas &&
          slot.drawnPo === po && slot.drawnDec === decKey && slot.drawnChart === chart;
        if (!unchanged) {
          chart.setOption(this.still ? { ...dec.option, animation: false } : dec.option,
                          actionFollows ? { notMerge: true, lazyUpdate: true } : true);
          slot.dimmed = false;
        }
        slot.drawnPo = po;
        slot.drawnDec = decKey;
        slot.drawnChart = chart;
        if (existed && hChanged) chart.resize();
        if (fam.after) fam.after(slot, po, this);
      });
      this.charts = this._slots.map((s) => s.chart).filter(Boolean);
      // Lazy patches: ECharts applies them in one update on the next frame.
      NS.panels.harmonise(this);
      NS.panels.axisTitles(this);
      NS.panels.fitStrips(this);
      // The ticks over the shared axes, then the zoom windows: the shared
      // axes are the full view they zoom in.
      this._slots.forEach((slot, i) => {
        if (!slot || !slot.chart || i >= m.panels.length) return;
        NS.ticks.after(this, slot);
        NS.zoom.after(this, slot, i, slot.chart.__vzRoles || null);
      });
      const plan = NS.legendPlan(m, this.settings);
      this.legend.update(plan.band ? fam.legend(m, this.settings, this.derived) : null, plan);
      this.frame.note(o.labelNote || null);
      if (fam.alwaysHighlight) {
        this.highlight(true);
      } else {
        this.status();
        if (this.selection.current) this._applyHighlight(true);
      }
      this.drawn = true;
    }

    /** The family's model of the rows, recomputed when the rows, the
     * columns or a config field it read changed. @param {ChartFamily} fam */
    _model(fam) {
      const memo = this._memo;
      const extra = fam.name + '\u0000' + this._rowsGen + '\u0000' + this._columnsKey;
      if (memo.models.update(this.settings, this.derived, extra,
        (/** @type {ChartSettings} */ cfg, /** @type {ChartDerived} */ derived) =>
        fam.model({ rows: this.data, ix: this._ix, columns: this.columns, cfg, derived }))) {
        memo.modelGen++;
      }
      return (memo.model = memo.models.value);
    }

    /** The family's option for the model, recomputed with a new model, ink,
     * theme or panel widths, or when a config field it read changed.
     * @param {ChartFamily} fam @param {any} m @param {number[] | null} widths */
    _option(fam, m, widths) {
      const memo = this._memo;
      const ink = this._ink;
      const extra = fam.name + '\u0000' + memo.modelGen + '\u0000' + JSON.stringify(ink) +
        '\u0000' + (this.theme || '') + '\u0000' + (widths ? widths.join(',') : '');
      memo.options.update(this.settings, this.derived, extra,
        (/** @type {ChartSettings} */ cfg, /** @type {ChartDerived} */ derived) => fam.option(m, {
        cfg, derived, columns: this.columns, ink, theme: this.theme,
        measure: this._measure(ink), widths: widths || []
      }));
      return (memo.option = memo.options.value);
    }

    // Text width in a CSS font, on a shared canvas.
    /** @param {string} font @returns {(s: string) => number} */
    _measureFont(font) {
      const ctx = measureCtx || (measureCtx = document.createElement('canvas').getContext('2d'));
      if (!ctx) return (s) => 7 * s.length;
      ctx.font = font;
      return (s) => ctx.measureText(s).width;
    }

    /** The axis font. @param {Ink} ink */
    _measure(ink) { return this._measureFont(`${ink.fontSize}px ${ink.face}`); }

    _teardownCharts() {
      if (this._shapeScope) this._shapeScope.dispose();
      this._shapeScope = null;
      this._slots = [];
      this.charts = [];
      this._renderShape = null;
      this._stripEls = [];
      this.chartGrid.innerHTML = '';
    }

    /** Say why there is no chart where it would be. @param {VizEmpty} e */
    _showEmpty(e) {
      this._teardownCharts();
      this.legend.update(null);
      V.Frame.message(this.chartGrid, e);
      this.frame.note(null);
      this.status();
      this.drawn = true;
      this._blank = true;
    }

    /** @param {number} i @param {string | null} facetLabel @param {boolean} singleFacet
     * @returns {ChartSlot} */
    _ensureSlot(i, facetLabel, singleFacet) {
      let slot = this._slots[i];
      if (!slot) {
        const container = singleFacet ? this.chartGrid : mk('div', 'vz-chart-facet', this.chartGrid);
        const labelEl = singleFacet ? null : mk('div', 'vz-chart-facet-label', container);
        const chartDiv = mk('div', 'vz-chart-panel', container);
        const zoomBar = mk('div', 'vz-chart-zoom-bar', container);
        // blockr.ui's quiet button, smallest size.
        const resetBtn = /** @type {HTMLButtonElement} */ (mk('button',
          'vz-chart-zoom-reset blockr-btn blockr-btn--quiet blockr-btn--xs', zoomBar));
        resetBtn.type = 'button';
        resetBtn.textContent = 'Reset zoom';
        resetBtn.hidden = true;
        container.classList.add('vz-chart-zoom-host');
        slot = { container, labelEl, chartDiv, resetBtn, zoomBar, chart: null, scope: null, facetVal: null,
                 gridRow: null, gridCol: null, innerTitles: null,
                 dimmed: false, hover: { si: null }, brushable: false, brushAreas: null, brushSel: null, brushCaught: null,
                 zoom: null, zoomRoles: null, zoomDragAt: 0, ticks: null,
                 focus: null, focusSi: null, band: null, bandFocus: null, lineClick: null };
        this._slots[i] = slot;
      }
      if (slot.labelEl) {
        slot.labelEl.textContent = facetLabel ?? '';
        slot.labelEl.style.display = facetLabel ? '' : 'none';
      }
      return slot;
    }

    /** A new ECharts instance gets the family's handlers, and a click on
     * empty plot space that clears the selection, unless a line is under
     * the cursor (the click picks it).
     * @param {ChartSlot} slot @param {ChartFamily} fam */
    _ensureSlotChart(slot, fam) {
      if (!slot.chart) {
        slot.chartDiv.textContent = '';
        const shape = this._shapeScope || (this._shapeScope = this.scope.own(new V.Scope()));
        const scope = slot.scope = shape.own(new V.Scope());
        slot.chart = scope.own(root.echarts.init(slot.chartDiv, this.theme || undefined));
        fam.attach(this, slot);
        NS.zoom.attach(this, slot);
        slot.chart.getZr().on('click', (/** @type {any} */ e) => {
          if ((e && e.target) || NS.zoom.justDragged(slot)) return;
          if (slot.focus && slot.focusSi != null) return;
          const sel = this.selection;
          if (sel.current || sel.brushDrawn) sel.clear();
        });
      }
      return slot.chart;
    }

    /** @param {ChartSlot} slot @param {string} h */
    _setSlotHeight(slot, h) {
      if (slot.chartDiv.style.height === h) return false;
      slot.chartDiv.style.height = h;
      return true;
    }

    // Skipped while the grid is hidden or has no box (a dock keeps hidden panels mounted).
    _resizeCharts() {
      if (this.chartGrid.offsetParent === null) return;
      if (!this.chartGrid.clientWidth || !this.chartGrid.clientHeight) return;
      for (const c of this.charts) {
        c.resize();
        this._refitXLabels(c);
      }
    }

    // A resize keeps the option built at the old width. Measure the x labels
    // again and, when the layout changed, patch the labels, the grid's bottom
    // gutter and the canvas height.
    /** @param {any} chart */
    _refitXLabels(chart) {
      const fit = chart.__xFit;
      if (!fit) return;
      const lab = NS.axes.xAxisLabels(fit.labels, chart.getWidth() - fit.inset, fit.decimate,
                                      this._measure(this._ink), this._ink);
      if (fit.key === lab.key) return;
      fit.key = lab.key;
      const delta = lab.bottom - fit.gutter;
      fit.gutter = lab.bottom;
      const grid = (chart.getOption().grid || [])[0] || {};
      /** @type {any} */ const label = NS.axes.axisLabelWithDisplay(lab.axisLabel, fit.formatter);
      /** @type {any} */ const xPatch = { axisLabel: fit.decorate ? fit.decorate(label) : label };
      // A titled x axis moves its title below the turned text.
      if (fit.nameGap) xPatch.nameGap = lab.bottom ? lab.bottom + 16 : 28;
      chart.setOption({ xAxis: [xPatch], grid: { bottom: (Number(grid.bottom) || 0) + delta } });
      if (delta && fit.slot &&
          this._setSlotHeight(fit.slot, Math.round(fit.baseH + lab.bottom) + 'px')) {
        chart.resize();
      }
    }

    dispose() { this._teardownCharts(); }
  }

  V.binding('blockr.viz.chart', 'chart', ChartView, { busy: 'drawing' });
  NS.themeReady = themeReady;
  NS.registerCapture(ChartView);
})();
//# sourceMappingURL=chart.js.map
