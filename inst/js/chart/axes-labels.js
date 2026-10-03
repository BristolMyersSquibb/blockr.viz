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
  const NS = /** @type {any} */ (B.chart = B.chart || {});

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
   * epoch milliseconds, else 'value'.
   * @param {VizColumn[]} columns @param {any[]} rows @param {any} colName
   * @returns {'category' | 'time' | 'value'}
   */
  const axisTypeFor = (columns, rows, colName) => {
    if (!colName) return 'value';
    const meta = colMeta(columns, colName);
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
   * An x value for a tooltip: the category on a category axis, a date on a
   * time axis, else a trimmed number.
   * @param {any} v @param {string} xAxisType @param {any[] | null} xCats
   */
  const fmtXVal = (v, xAxisType, xCats) => {
    if (v == null || v === '') return '';
    if (xAxisType === 'category') {
      return (xCats && xCats[v] != null) ? String(xCats[v]) : String(v);
    }
    if (xAxisType === 'time') {
      const d = new Date(Number(v));
      return isNaN(d.getTime()) ? String(v) : d.toISOString().slice(0, 10);
    }
    return NS.ddNum(Number(v));
  };

  /**
   * The span between two x values: days on a time axis, units on a value
   * axis, nothing on a category axis.
   * @param {any} a @param {any} b @param {string} xAxisType
   */
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
   * or null when every label fits.
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
   * @param {string | undefined} text @param {boolean} flipped
   * @param {{ text: string, muted: string }} ink
   */
  const guideLine = (v, vertical, width, text, flipped, ink) => ({
    [vertical ? 'xAxis' : 'yAxis']: v,
    lineStyle: { color: ink.text, opacity: 0.55, type: [8, 4], width },
    label: {
      show: true,
      position: vertical ? (flipped ? 'start' : 'end') : 'insideEndTop',
      formatter: text != null ? text : String(NS.ddNum(v)),
      color: ink.muted, fontSize: 10, opacity: 1
    }
  });

  /**
   * `value_lines` on the value axis (x when the chart lies horizontal): a
   * markLine on the first series, and axis bounds that take in the lines
   * and the data, rounded outward to a nice step. Mutates `series[0]` and
   * `valAxis`.
   * @param {any[]} series @param {any} valAxis @param {boolean} onX
   * @param {boolean} fraction the axis runs 0..1 while the reader types percent
   * @param {any} valueLines @param {{ text: string, muted: string }} ink
   */
  const addValueLines = (series, valAxis, onX, fraction, valueLines, ink) => {
    const raw = Array.isArray(valueLines) ? valueLines : [];
    const typed = raw.map(Number).filter(Number.isFinite);
    const vals = typed.map((v) => (fraction ? v / 100 : v));
    if (!vals.length || !series.length) return;
    series[0].markLine = {
      silent: true, symbol: 'none',
      // onX: a horizontal chart, whose category axis is inverse.
      data: vals.map((v, i) => guideLine(v, onX, 1,
        fraction ? NS.ddNum(typed[i]) + '%' : undefined, onX, ink))
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

  NS.axes = { colMeta, axisTitle, axisTypeFor, orderedCategories, orderLevels, yGutter,
              fmtXVal, durationStr, sharedDomain, wrapLabel, xLabelKey, axisLabelWithDisplay,
              xAxisLabels, thinnedLabelNote, guideLine, addValueLines, LABEL_LINE_H, CAT_LABEL_H };
})();
