// @ts-check
/**
 * The marks of the groups layout: one cell of a summary column, drawn from
 * the column's `mark` (table.d.ts, TableMark), which R computes
 * (R/table-model-cells*.R). Positions and widths arrive as percentages of
 * the cell, already rounded; the marks place them. Every text goes through
 * the one escaper, and every style value is a number or a colour R chose.
 *
 *   bar        a track and its fill
 *   barsplit   segments of a colour split, stacked or one track per level
 *   bardiv     a bar either side of zero
 *   box        whiskers, caps, box and median
 *   pointrange a range and its centre, with an outer fence
 *   pair       a dumbbell: band, reference, link, from and to
 *   interval   swimlane segments on a time axis
 *   sparkline  a line, its band and reference, the last point
 *   tile       a painted square with the value in it
 *
 * box, pointrange and pair split by colour draw one lane per level.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const esc = V.util.esc;

  /** A number for a style: finite, or 0. */
  const n = (/** @type {unknown} */ x) => {
    const v = Number(x);
    return isFinite(v) ? String(v) : '0';
  };
  /** A colour for a style, escaped for the attribute. */
  const col = (/** @type {unknown} */ x) => esc(x == null ? '' : x);
  /** `left:x%;width:y%` */
  const span = (/** @type {unknown} */ l, /** @type {unknown} */ w) =>
    'left:' + n(l) + '%;width:' + n(w) + '%';
  const tipAttr = (/** @type {unknown} */ t) => t ? ' data-summarize-tip="' + esc(t) + '"' : '';
  /** @param {(string | null)[] | null | undefined} a @param {number} i */
  const at = (a, i) => (a ? a[i] : null);

  /**
   * A track with its fill. An empty or zero width draws an empty track.
   * @param {number | null} w
   * @param {string | null | undefined} fill
   * @param {boolean} sub
   */
  function track(w, fill, sub) {
    return '<div class="blockr-summarize-track' + (sub ? ' is-sub' : '') + '">' +
      (w != null && w > 0 ? '<div class="blockr-summarize-fill" style="width:' + n(w) + '%' +
        (fill ? ';background:' + col(fill) : '') + '"></div>' : '') + '</div>';
  }

  /** @param {string} text @param {number | null | undefined} dw */
  const barval = (text, dw) => '<span class="blockr-summarize-barval" style="width:' + n(dw) +
    'ch">' + text + '</span>';

  /**
   * One segment of a colour split.
   * @param {TableMarkSplit} c
   * @param {number} j Level.
   * @param {number} i Row.
   */
  function segment(c, j, i) {
    const lab = c.mode !== 'grouped' && c.slab ? at(c.slab[j], i) : null;
    return '<div class="blockr-summarize-fill' + (lab ? ' has-lab' : '') + '" data-seg="' + j +
      '" style="width:' + n(c.seg[j][i]) + '%;background:' + col(c.fills[j]) + '"' +
      tipAttr(c.names[j] + ': ' + (c.segv[j][i] == null ? '' : c.segv[j][i])) + '>' +
      (lab ? '<span class="blockr-summarize-seglab lw-' + String(lab).length + '" style="color:' +
        col(c.sink ? c.sink[j] : '') + '">' + esc(lab) + '</span>' : '') + '</div>';
  }

  /**
   * A colour split: stacked in one track, or one track per level.
   * @param {TableMarkSplit} c
   * @param {number} i
   * @param {boolean} labels
   */
  function split(c, i, labels) {
    const grouped = c.mode === 'grouped';
    const ldisp = labels ? c.ldisp : null;
    let out = '';
    c.names.forEach((_, j) => {
      if (!((c.segv[j][i] || 0) > 0)) return;
      const body = segment(c, j, i);
      if (grouped && ldisp) {
        out += '<div class="blockr-summarize-lv blockr-summarize-barwrap">' +
          '<div class="blockr-summarize-track is-lv">' + body + '</div>' +
          barval(esc(at(ldisp[j], i)), c.dw) + '</div>';
      } else {
        out += grouped ? '<div class="blockr-summarize-row3">' + body + '</div>' : body;
      }
    });
    if (grouped && ldisp) return '<div class="blockr-summarize-multi">' + out + '</div>';
    return '<div class="blockr-summarize-track' + (grouped ? ' is-tall' : '') + '">' + out + '</div>';
  }

  /** @param {number | null} w @param {boolean} pos */
  function diverging(w, pos) {
    if (!(w != null && w > 0)) return '<div class="blockr-summarize-dv"></div>';
    return '<div class="blockr-summarize-dv"><div class="blockr-summarize-fill ' +
      (pos ? 'is-pos' : 'is-neg') + '" style="width:' + n(w) + '%"></div></div>';
  }

  /** @param {string} name @param {TableLaneGeom} g @param {number} i @param {boolean} has */
  const lane = (name, g, i, has) => '<div class="blockr-summarize-lane blockr-summarize-' + name +
    (g.bare ? ' is-bare' : '') + '"' + (has ? tipAttr(at(g.tip, i)) : '') + '>';

  /**
   * Box: whiskers, their caps, the box and the median.
   * @param {TableLaneGeom} g
   * @param {number} i
   */
  function box(g, i) {
    const v = (/** @type {keyof TableLaneGeom} */ k) =>
      /** @type {(number | null)[] | undefined} */ (g[k]) ? /** @type {(number | null)[]} */ (g[k])[i] : null;
    if (v('bc') == null) return lane('boxcell', g, i, false) + '</div>';
    let s = lane('boxcell', g, i, true);
    if (v('w1') != null) s += '<i class="lane-wh" style="' + span(v('wl'), v('w1')) + '"></i>';
    if (v('w2') != null) s += '<i class="lane-wh" style="' + span(v('b2'), v('w2')) + '"></i>';
    if (v('w1') != null) s += '<i class="lane-cap" style="left:' + n(v('wl')) + '%"></i>';
    if (v('w2') != null) s += '<i class="lane-cap" style="left:' + n(v('wh')) + '%"></i>';
    if (v('bw') != null) s += '<i class="lane-box" style="' + span(v('bl'), v('bw')) + '"></i>';
    return s + '<i class="lane-med" style="left:' + n(v('bc')) + '%"></i></div>';
  }

  /**
   * Point range: the outer fence, the range, the centre.
   * @param {TableLaneGeom} g
   * @param {number} i
   */
  function pointrange(g, i) {
    const v = (/** @type {keyof TableLaneGeom} */ k) =>
      /** @type {(number | null)[] | undefined} */ (g[k]) ? /** @type {(number | null)[]} */ (g[k])[i] : null;
    if (v('c') == null) return lane('prcell', g, i, false) + '</div>';
    let s = lane('prcell', g, i, true);
    if (v('ow') != null) s += '<i class="lane-fence" style="' + span(v('ol'), v('ow')) + '"></i>';
    if (v('rw') != null) s += '<i class="lane-rng" style="' + span(v('l'), v('rw')) + '"></i>';
    return s + '<i class="lane-ctr" style="left:' + n(v('c')) + '%"></i></div>';
  }

  /**
   * Pair (dumbbell): band, reference line, link, then the two ends.
   * @param {TablePairGeom} g
   * @param {number} i
   * @param {number | null | undefined} rf
   */
  function pair(g, i, rf) {
    const fill = at(g.fill, i);
    let s = '<div class="blockr-summarize-lane blockr-summarize-pacell' +
      (g.dash && g.dash[i] ? ' is-dash' : '') + '"' +
      (fill != null ? ' style="--blockr-summarize-fill:' + col(fill) + '"' : '') +
      tipAttr(at(g.tip, i)) + '>';
    if (g.bw[i] != null) s += '<i class="lane-band" style="' + span(g.bl[i], g.bw[i]) + '"></i>';
    if (rf != null) s += '<i class="lane-ref" style="left:' + n(rf) + '%"></i>';
    if (g.w[i] != null) s += '<i class="lane-link" style="' + span(g.l[i], g.w[i]) + '"></i>';
    if (g.a[i] != null) s += '<i class="lane-from" style="left:' + n(g.a[i]) + '%"></i>';
    if (g.b[i] != null) {
      s += '<i class="lane-to' + (g.open && g.open[i] ? ' is-open' : '') + '" style="left:' +
        n(g.b[i]) + '%"></i>';
    }
    return s + '</div>';
  }

  /**
   * Swimlane segments: [left, width, level (from 1), label?] each.
   * @param {TableMarkInterval} c
   * @param {number} i
   */
  function interval(c, i) {
    let s = '<div class="blockr-summarize-lane blockr-summarize-ivcell" data-d0="' + n(c.d0) +
      '" data-d1="' + n(c.d1) + '"' + (c.dd ? ' data-dd="1"' : '') + '>';
    (c.segs[i] || []).forEach((g, j) => {
      s += '<i class="lane-seg" style="' + span(g[0], g[1]) + ';background:' +
        col(c.fills[Number(g[2]) - 1]) + '"' + (g.length > 3 ? ' data-l="' + esc(g[3]) + '"' : '') +
        ' data-tip="' + esc(at(c.tips[i], j)) + '"></i>';
    });
    return s + '</div>';
  }

  /** SVG points: numbers, commas and spaces only. */
  const points = (/** @type {unknown} */ p) => String(p == null ? '' : p).replace(/[^0-9.,\s-]/g, '');

  /**
   * Sparkline: reference band and line, the value band, the line, the
   * last point.
   * @param {TableMarkSpark} c
   * @param {number} i
   */
  function sparkline(c, i) {
    let s = '<div class="blockr-summarize-lane blockr-summarize-spcell" data-xs="' +
      esc(c.xs[i]) + '" data-ys="' + esc(c.ys[i]) + '">' +
      '<svg viewBox="0 0 100 36" preserveAspectRatio="none">';
    if (c.rby != null) {
      s += '<rect class="lane-refband" x="0" y="' + n(c.rby) + '" width="100" height="' +
        n(c.rbh) + '"></rect>';
    }
    if (c.bd && c.bd[i] != null) s += '<polygon class="lane-band" points="' + points(c.bd[i]) + '"></polygon>';
    if (c.rc != null) {
      s += '<line class="lane-refline" x1="0" y1="' + n(c.rc) + '" x2="100" y2="' + n(c.rc) +
        '" vector-effect="non-scaling-stroke"></line>';
    }
    if (c.pl[i]) {
      s += '<polyline class="lane-ln" points="' + points(c.pl[i]) +
        '" vector-effect="non-scaling-stroke"></polyline>';
    }
    s += '</svg>';
    if (c.dx[i] != null) {
      s += '<i class="lane-dot" style="left:' + n(c.dx[i]) + '%;top:' + n(c.dy[i]) + '%"></i>';
    }
    return s + '</div>';
  }

  /**
   * Whether level `g` of a colour split draws in row `i`.
   * @param {TableMarkLane | TableMarkPair} c
   * @param {any} g
   * @param {number} i
   */
  function drawn(c, g, i) {
    if (c.kind === 'pair') return g.a[i] != null || g.b[i] != null;
    return (c.kind === 'box' ? g.bc[i] : g.c[i]) != null;
  }

  /**
   * A colour split of lanes: one per level that has rows here, each in its
   * colour, with its value when labels are on.
   * @param {TableMarkLane | TableMarkPair} c
   * @param {number} i
   * @param {boolean} labels
   */
  function multi(c, i, labels) {
    let s = '<div class="blockr-summarize-multi">';
    (c.lv || []).forEach((g, j) => {
      if (!drawn(c, g, i)) return;
      const inner = c.kind === 'pair' ? pair(/** @type {TablePairGeom} */ (g), i, c.rf)
        : c.kind === 'box' ? box(g, i) : pointrange(g, i);
      const lab = labels && g.disp && c.dw != null;
      s += '<div class="blockr-summarize-lv' + (lab ? ' blockr-summarize-barwrap' : '') +
        '" style="--blockr-summarize-fill:' + col((c.fills || [])[j]) + '">' + inner +
        (lab ? barval(esc(at(g.disp, i)), c.dw) : '') + '</div>';
    });
    return s + '</div>';
  }

  /**
   * The mark of one cell, without its value: shared by the table's cells
   * and the cards. "" for a column without a mark.
   * @param {TableMark | null | undefined} c
   * @param {number} i
   * @param {boolean} labels
   */
  function glyph(c, i, labels) {
    if (!c) return '';
    switch (c.kind) {
      case 'bar': return track(c.w[i], c.fill, !!(c.sub && c.sub[i]));
      case 'barsplit': return split(c, i, labels);
      case 'bardiv': return diverging(c.w[i], !!c.pos[i]);
      case 'box': return c.multi ? multi(c, i, labels) : box(c, i);
      case 'pointrange': return c.multi ? multi(c, i, labels) : pointrange(c, i);
      case 'pair': return c.multi ? multi(c, i, labels) : pair(/** @type {TablePairGeom} */ (c), i, c.rf);
      case 'interval': return interval(c, i);
      case 'sparkline': return sparkline(c, i);
      default: return '';
    }
  }

  /**
   * A count's text, with its percentage.
   * @param {TableCol} c
   * @param {number} i
   */
  function value(c, i) {
    const p = at(c.pct, i);
    return esc(c.disp[i]) + (p ? ' <span class="blockr-summarize-pct">' + esc(p) + '</span>' : '');
  }

  /**
   * One body cell of a summary column.
   * @param {TableCol} c
   * @param {number} i
   * @param {{ labels: boolean }} o `labels`: the values beside the marks.
   */
  function cell(c, i, o) {
    const m = c.mark;
    if (m && m.kind === 'tile') {
      const bg = at(c.bg, i);
      return '<td class="blockr-summarize-tile-col">' + (bg == null ? ''
        : '<span class="blockr-summarize-tile" style="background:' + col(bg) + ';color:' +
          col(at(c.fg, i)) + '">' + esc(c.disp[i]) + '</span>') + '</td>';
    }
    if (!m) {
      return c.num ? '<td class="blockr-summarize-num dt-col-num">' + value(c, i) + '</td>'
        : '<td class="blockr-summarize-txt' + (c.failed ? ' is-failed' : '') + '">' +
          esc(c.disp[i]) + '</td>';
    }
    if (m.kind === 'interval') {
      return '<td class="blockr-summarize-bar-col' + (m.lg ? ' blockr-summarize-wide' : '') + '">' +
        glyph(m, i, o.labels) + '</td>';
    }
    const g = glyph(m, i, o.labels);
    // A grouped split and a multi lane label each level; a stacked split
    // keeps its one total beside the bar.
    const own = (m.kind === 'barsplit' && m.mode === 'grouped') || (/** @type {any} */ (m).multi);
    const lab = o.labels && !own && 'dw' in m && m.dw != null && c.disp[i] != null;
    return '<td class="blockr-summarize-bar-col' + (m.kind === 'sparkline' ? ' is-spark' : '') + '">' + (lab
      ? '<div class="blockr-summarize-barwrap">' + g + barval(value(c, i), /** @type {any} */ (m).dw) +
        '</div>'
      : g) + '</td>';
  }

  /**
   * The column axis under a leaf header: tick labels at their positions,
   * and the value slot the cells keep beside their marks.
   * @param {TableCol} c
   * @param {boolean} labels
   */
  function axis(c, labels) {
    const a = c.axis;
    if (!a || !a.at || a.at.length < 2) return '';
    const dw = labels && c.mark && 'dw' in c.mark ? /** @type {any} */ (c.mark).dw : null;
    const ticks = a.at.map((p, k) => '<span' + (p <= 8 ? ' class="is-first"' : p >= 92 ? ' class="is-last"' : '') +
      ' style="left:' + n(p) + '%">' + esc(a.labels[k]) + '</span>').join('');
    return '<div class="blockr-summarize-axis' + (dw != null ? ' has-val' : '') + '">' +
      '<span class="blockr-summarize-axis-in">' + ticks + '</span>' +
      (dw != null ? '<span class="blockr-summarize-axis-pad" style="width:' + n(dw) + 'ch"></span>' : '') +
      '</div>';
  }

  V.table.marks = { glyph, cell, axis };
})();
