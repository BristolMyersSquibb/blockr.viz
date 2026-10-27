// @ts-check
/**
 * Chart block: axes and their labels, as plain functions.
 *
 * Axis types and category order from the column metadata, axis titles,
 * the category label gutter and the x label ladder (measured with a
 * function the caller passes, so no canvas is needed here), value
 * formatting on an axis, helper lines on a value axis, and the shared
 * domain of a facet grid.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});

  // The time helpers (axes-time.js) load first and start NS.axes.
  const { timeValue } = NS.axes;

  /** @param {VizColumn[] | null | undefined} columns @param {any} name */
  const colMeta = (columns, name) =>
    /** @type {any} */ ((columns || []).find((c) => c.name === name));

  /**
   * An axis title: the column's label when it has one, else its name.
   * @param {VizColumn[]} columns @param {any} col @returns {string}
   */
  const axisTitle = (columns, col) => {
    if (!col) return '';
    const c = colMeta(columns, col);
    if (c && c.label && c.label !== c.name) return c.label;
    return col;
  };

  /**
   * 'category' for a categorical column, 'time' for a *DT column holding
   * epoch milliseconds, else 'value'. With `temporal`, a Date or date-time
   * column (`temporal` in its metadata, R/col-meta.R) is 'time' too; the
   * gantt does not ask for it and keeps such a column a category.
   * @param {VizColumn[]} columns @param {any[]} rows @param {any} colName
   * @param {{ temporal?: boolean }} [opts]
   * @returns {'category' | 'time' | 'value'}
   */
  const axisTypeFor = (columns, rows, colName, opts = {}) => {
    if (!colName) return 'value';
    const meta = colMeta(columns, colName);
    if (opts.temporal && meta && /** @type {any} */ (meta).temporal) return 'time';
    if (meta && meta.type === 'categorical') return 'category';
    if (/DT$/i.test(colName)) {
      const sample = (rows || []).find((r) => r[colName] != null);
      if (sample && typeof sample[colName] === 'number' && sample[colName] > 1e11) {
        return 'time';
      }
    }
    return 'value';
  };

  /**
   * The categories of a column on an axis: factor levels first (values
   * outside them after, in order of appearance), else by a companion number
   * (AVISIT by AVISITN), else in order of appearance.
   * @param {VizColumn[]} columns @param {any[]} rows @param {string} colName
   * @returns {string[]}
   */
  const orderedCategories = (columns, rows, colName) => {
    const src = rows || [];
    const meta = colMeta(columns, colName);
    if (meta && Array.isArray(meta.levels) && meta.levels.length) {
      const present = new Set(src.map((r) => String(r[colName] ?? '')));
      const out = meta.levels.map(String).filter((/** @type {string} */ l) => present.has(l));
      const inOut = new Set(out);
      for (const k of present) if (!inOut.has(k)) { inOut.add(k); out.push(k); }
      return out;
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
    const seen = new Set();
    const out = [];
    for (const r of src) {
      const k = String(r[colName] ?? '');
      if (!seen.has(k)) { seen.add(k); out.push(k); }
    }
    return out;
  };

  /**
   * Order levels: by the board scale's order, else the column's factor
   * levels, else alphabetically. Unknown levels sort last, alphabetically.
   * @param {any[]} levels @param {any} scale @param {VizColumn[]} columns
   * @param {string} colName @returns {any[]}
   */
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

  /**
   * The category label gutter of a horizontal axis, fitted to the widest
   * label up to a cap: the label width, its margin and the grid's left edge.
   * @param {any[]} labels @param {(s: string) => number} measure
   */
  const yGutter = (labels, measure) => {
    const LABEL_CAP = 145;
    // Slack for the fallback font the canvas may measure with before the
    // page font loads; without it short labels truncate.
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
   * One domain over the panels' own (already nice-rounded) extents, or null
   * when fewer than two panels have a numeric one.
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

  // -- Log axes ----------------------------------------------------------------
  //
  // ECharts labels a log axis at powers of ten only (1K, 10K, 100K), which
  // leaves two or three labels on most data. Up to three decades the ticks
  // are 1-2-5 (or 1-1.5-2-3-5-7 within one decade), set as the axis's own
  // values; the gridlines follow them. The axis ends on the ticks either
  // side of the data.

  /**
   * The domain and ticks of a log axis over positive values lo..hi. Null
   * when there is no positive range.
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

  /**
   * Turn a value axis into a log axis over a domain from logTicks().
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

  // -- Category labels on an x axis ------------------------------------------
  //
  // A long label goes down a ladder, cheapest first: one line; wrapped on
  // spaces onto up to three flat lines; turned 90 degrees and wrapped onto
  // two; and, only where the axis is an ordered run (`decimate`), thinned.
  // Nothing is cut: a word too wide for the whole budget breaks mid-word.
  const LABEL_LINE_H = 15;   // 11px text, 1.35 leading
  const FLAT_MAX_LINES = 3;
  const TURN_MAX_LINES = 2;
  const TURN_CAP = 160;      // px of height a turned label may spend
  // Reserved whether or not a scrollbar shows: a turned label grows the
  // panel, which can bring one up and flip a borderline label next time.
  const SCROLLBAR = 16;

  /**
   * Greedy wrap on spaces; `hard` says a word had to be broken mid-word.
   * @param {(s: string) => number} measure @param {any} text @param {number} width
   * @returns {{ lines: string[], hard: boolean }}
   */
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

  /** What a label layout is: turned, thinned, wrap width, line count. A
   *  resize that keeps it needs no new layout. @param {any} axisLabel */
  const xLabelKey = (axisLabel) =>
    `${axisLabel.rotate}|${axisLabel.interval}|${axisLabel.__wrapAt}|${axisLabel.__lines}`;

  /**
   * An axisLabel whose formatter first maps the category to what it shows
   * (a "(n)" count, a level suffix stripped), then wraps it.
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

  /**
   * Category labels for an x axis at a plot width: flat when they fit their
   * slot, else wrapped flat, else turned. `bottom` is the extra grid gutter
   * the labels cost, `key` the layout's identity (xLabelKey).
   * @param {any[]} labels @param {number} availW plot width minus the grid's margins
   * @param {boolean} decimate thin turned labels (ordered axes only)
   * @param {(s: string) => number} measure @param {{ muted: string, fontSize: number }} ink
   * @returns {{ axisLabel: any, bottom: number, key: string }}
   */
  const xAxisLabels = (labels, availW, decimate, measure, ink) => {
    const m = (/** @type {any} */ s) => measure(String(s ?? ''));
    let widest = 0;
    for (const l of labels) {
      const tw = m(l);
      if (tw > widest) widest = tw;
    }
    const PAD = 8;
    const n = Math.max(1, labels.length);
    // A hidden panel measures 0: assume a typical width rather than turn
    // short labels.
    const slot = ((availW > 0 ? availW : 600) - SCROLLBAR) / n;
    const base = { color: ink.muted, fontSize: ink.fontSize, interval: 0, lineHeight: LABEL_LINE_H };

    if (widest + PAD <= slot) {
      const axisLabel = { ...base, rotate: 0, __wrapAt: 0, __lines: 1 };
      return { axisLabel, bottom: 0, key: xLabelKey(axisLabel) };
    }

    // Flat, wrapped to the slot, unless a word had to break or it takes
    // more than three lines.
    const flatW = Math.max(10, slot - PAD);
    const flat = labels.map((l) => wrapLabel(m, l, flatW));
    const flatLines = NS.maxOf(flat.map((f) => f.lines.length));
    if (flatLines <= FLAT_MAX_LINES && !flat.some((f) => f.hard)) {
      const axisLabel = {
        ...base, rotate: 0, __wrapAt: Math.round(flatW), __lines: flatLines,
        formatter: (/** @type {string} */ v) => wrapLabel(m, v, flatW).lines.join('\n')
      };
      return { axisLabel, bottom: (flatLines - 1) * LABEL_LINE_H, key: xLabelKey(axisLabel) };
    }

    // Turned: the wrap budget is the height a label may spend; past two
    // lines the rest joins the last one.
    const turnedLines = (/** @type {string} */ v) => {
      const w = wrapLabel(m, v, TURN_CAP).lines;
      if (w.length <= TURN_MAX_LINES) return w;
      return w.slice(0, TURN_MAX_LINES - 1).concat(w.slice(TURN_MAX_LINES - 1).join(' '));
    };
    const turned = labels.map(turnedLines);
    const turnLines = NS.maxOf(turned.map((t) => t.length));
    const len = Math.ceil(NS.maxOf(turned.map((t) => NS.maxOf(t.map(m))))) + PAD;
    // Turned text needs about a line height of room per line; `interval: k`
    // draws one label and skips k.
    const room = Math.max(13, turnLines * LABEL_LINE_H);
    const interval = decimate ? Math.max(0, Math.ceil(room / slot) - 1) : 0;
    const axisLabel = {
      ...base, rotate: 90, interval, __wrapAt: TURN_CAP, __lines: turnLines,
      formatter: (/** @type {string} */ v) => turnedLines(v).join('\n')
    };
    return { axisLabel, bottom: len, key: xLabelKey(axisLabel) };
  };

  // Height one horizontal category label takes. Past it ECharts drops
  // labels without a word, so the chart says so in the footer.
  const CAT_LABEL_H = 14;

  /**
   * The footer note for a category axis squeezed past one label per row,
   * or null when every label fits. `groupCol` names the categories: the
   * column's title (axisTitle), not its name.
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

  // -- Helper lines on the value axis -----------------------------------------

  /**
   * One helper line as a markLine datum: a quiet long dash behind the data,
   * its value at the end. `text` replaces the printed value (a percent axis
   * runs 0..1); `flipped`: the y axis runs top-down, so a vertical line is
   * labelled at its start.
   * @param {number} v @param {boolean} vertical @param {number} width
   * @param {string | null | undefined} text @param {boolean} flipped
   * @param {{ text: string, muted: string, fontSize?: number }} ink
   * `text` null: no label (an unnamed line at 0, whose value the axis
   * already prints).
   */
  const guideLine = (v, vertical, width, text, flipped, ink) => ({
    [vertical ? 'xAxis' : 'yAxis']: v,
    lineStyle: { color: ink.text, opacity: 0.55, type: [8, 4], width },
    label: {
      show: text !== null,
      position: vertical ? (flipped ? 'start' : 'end') : 'insideEndTop',
      formatter: text != null ? text : String(NS.ddNum(v)),
      color: ink.muted, fontSize: ink.fontSize || 12, opacity: 1
    }
  });

  /**
   * `y_lines` on the value axis (x when the chart lies horizontal): a
   * markLine on the first series, and axis bounds that take in the lines
   * and the data, rounded outward to a nice step. Mutates `series[0]` and
   * `valAxis`.
   * @param {any[]} series @param {any} valAxis @param {boolean} onX
   * @param {boolean} fraction the axis runs 0..1 while the reader types percent
   * @param {any} lines `[{value, label}]`; a label names its line
   *   (`y_lines = c(Target = 2)`), none prints the value
   * @param {{ text: string, muted: string }} ink
   */
  const addValueLines = (series, valAxis, onX, fraction, lines, ink) => {
    const keep = NS.labelled(lines).filter((x) => Number.isFinite(Number(x.value)));
    const nm = keep.map((x) => x.label);
    const typed = keep.map((x) => Number(x.value));
    const vals = typed.map((v) => (fraction ? v / 100 : v));
    if (!vals.length || !series.length) return;
    series[0].markLine = {
      silent: true, symbol: 'none',
      // onX: a horizontal chart, whose category axis is inverse.
      data: vals.map((v, i) => guideLine(v, onX, 1,
        nm[i] ? String(nm[i])
          : typed[i] === 0 ? null
            : (fraction ? NS.ddNum(typed[i]) + '%' : undefined), onX, ink))
    };
    // A bar axis keeps 0 in range; a `scale` axis fits the data. A line on a
    // rounded edge would read as the frame, so the edge moves one step out.
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
    const onLine = (at, st) => vals.some((v) => Math.abs(v - at) < st * 1e-6);
    if (valAxis.max == null) {
      valAxis.max = (/** @type {any} */ e) => {
        const st = step(e);
        if (!st) return undefined;
        const at = Math.ceil(ext(e).hi / st) * st;
        return onLine(at, st) ? at + st : at;
      };
    }
    if (valAxis.min == null) {
      valAxis.min = (/** @type {any} */ e) => {
        const st = step(e);
        if (!st) return undefined;
        const at = Math.floor(ext(e).lo / st) * st;
        // A line at 0 on a bar axis is the baseline itself.
        if (zero && at === 0) return undefined;
        return onLine(at, st) ? at - st : at;
      };
    }
  };

  // -- Lines and ranges on a category axis -------------------------------------

  const ISO_DAY = /^\d{4}-\d{2}-\d{2}/;

  /**
   * The category a typed x position marks: the one it names, or, when the
   * categories are dates in order (rising or falling), the one whose period
   * holds the date (a week's bar holds its Wednesday). `edge`: a date before
   * the first category or after the last takes that end, so a range reaching
   * past the axis shades up to it. null when it marks none.
   * @param {any} v @param {any[]} cats @param {boolean} [edge]
   * @returns {string | null}
   */
  const catOf = (v, cats, edge = false) => {
    if (v == null || v === '' || !Array.isArray(cats) || !cats.length) return null;
    const s = String(v);
    for (const c of cats) if (String(c) === s) return String(c);
    if (!ISO_DAY.test(s) || cats.length < 2) return null;
    let ts = cats.map((c) => (ISO_DAY.test(String(c)) ? timeValue(c) : NaN));
    // Newest first: the same periods, read the other way.
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

  /**
   * `x_lines` and `x_ranges` on a bar's category axis: a line through the
   * bar each marks, a shade over the bars each range spans. ECharts places a
   * category at its band, so a line sits on the bar's centre and a range
   * covers whole bars. Mutates `series[0]`, adding to the markLine the value
   * lines put there.
   * @param {any[]} series @param {boolean} onX the categories run along x
   * @param {any[]} cats @param {any} lines `x_lines` @param {any} ranges `x_ranges`
   * @param {{ text: string, muted: string, fontSize?: number }} ink
   */
  const addCategoryMarks = (series, onX, cats, lines, ranges, ink) => {
    if (!series.length) return;
    const key = onX ? 'xAxis' : 'yAxis';
    /** @type {any[]} */
    const data = [];
    NS.labelled(lines).forEach(({ value: v, label }) => {
      const c = catOf(v, cats);
      if (c == null) return;
      // Unnamed, a line on its own category needs no label (the axis prints
      // it); a date inside a bar's period prints the date.
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
    sharedDomain, logTicks, logAxis, wrapLabel, xLabelKey, axisLabelWithDisplay,
    xAxisLabels, thinnedLabelNote, guideLine, addValueLines, catOf, addCategoryMarks,
    LABEL_LINE_H, CAT_LABEL_H
  });
})();
