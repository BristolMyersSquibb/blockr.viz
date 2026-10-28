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
    return '<div class="vz-table-groups-track' + (sub ? ' is-sub' : '') + '">' +
      (w != null && w > 0 ? '<div class="vz-table-groups-fill" style="width:' + n(w) + '%' +
        (fill ? ';background:' + col(fill) : '') + '"></div>' : '') + '</div>';
  }

  /** @param {string} text @param {number | null | undefined} dw */
  const barval = (text, dw) => '<span class="vz-table-groups-barval" style="width:' + n(dw) +
    'ch">' + text + '</span>';

  /**
   * One segment of a colour split.
   * @param {TableMarkSplit} c
   * @param {number} j Level.
   * @param {number} i Row.
   */
  function segment(c, j, i) {
    const lab = c.mode !== 'grouped' && c.segLabels ? at(c.segLabels[j], i) : null;
    return '<div class="vz-table-groups-fill' + (lab ? ' has-lab' : '') + '" data-seg="' + j +
      '" style="width:' + n(c.segWidths[j][i]) + '%;background:' + col(c.fills[j]) + '"' +
      tipAttr(c.names[j] + ': ' + (c.segValues[j][i] == null ? '' : c.segValues[j][i])) + '>' +
      (lab ? '<span class="vz-table-groups-seglab vz-table-lw-' + String(lab).length + '" style="color:' +
        col(c.segInk ? c.segInk[j] : '') + '">' + esc(lab) + '</span>' : '') + '</div>';
  }

  /**
   * A colour split: stacked in one track, or one track per level.
   * @param {TableMarkSplit} c
   * @param {number} i
   * @param {boolean} labels
   */
  function split(c, i, labels) {
    const grouped = c.mode === 'grouped';
    const ldisp = labels ? c.levelValues : null;
    let out = '';
    c.names.forEach((_, j) => {
      if (!((c.segValues[j][i] || 0) > 0)) return;
      const body = segment(c, j, i);
      if (grouped && ldisp) {
        out += '<div class="vz-table-groups-lv vz-table-groups-barwrap">' +
          '<div class="vz-table-groups-track is-lv">' + body + '</div>' +
          barval(esc(at(ldisp[j], i)), c.valueWidth) + '</div>';
      } else {
        out += grouped ? '<div class="vz-table-groups-row3">' + body + '</div>' : body;
      }
    });
    if (grouped && ldisp) return '<div class="vz-table-groups-multi">' + out + '</div>';
    return '<div class="vz-table-groups-track' + (grouped ? ' is-tall' : '') + '">' + out + '</div>';
  }

  /** @param {number | null} w @param {boolean} pos */
  function diverging(w, pos) {
    if (!(w != null && w > 0)) return '<div class="vz-table-groups-dv"></div>';
    return '<div class="vz-table-groups-dv"><div class="vz-table-groups-fill ' +
      (pos ? 'is-pos' : 'is-neg') + '" style="width:' + n(w) + '%"></div></div>';
  }

  /** @param {string} name @param {TableLaneGeom} g @param {number} i @param {boolean} has */
  const lane = (name, g, i, has) => '<div class="vz-table-groups-lane vz-table-groups-' + name +
    (g.bare ? ' is-bare' : '') + '"' + (has ? tipAttr(at(g.tip, i)) : '') + '>';

  /**
   * Box: whiskers, their caps, the box and the median.
   * @param {TableLaneGeom} g
   * @param {number} i
   */
  function box(g, i) {
    const v = (/** @type {keyof TableLaneGeom} */ k) =>
      /** @type {(number | null)[] | undefined} */ (g[k]) ? /** @type {(number | null)[]} */ (g[k])[i] : null;
    if (v('median') == null) return lane('boxcell', g, i, false) + '</div>';
    let s = lane('boxcell', g, i, true);
    if (v('whiskerLowWidth') != null) {
      s += '<i class="vz-table-lane-wh" style="' + span(v('whiskerLow'), v('whiskerLowWidth')) + '"></i>';
    }
    if (v('whiskerHighWidth') != null) {
      s += '<i class="vz-table-lane-wh" style="' + span(v('boxRight'), v('whiskerHighWidth')) + '"></i>';
    }
    if (v('whiskerLowWidth') != null) s += '<i class="vz-table-lane-cap" style="left:' + n(v('whiskerLow')) + '%"></i>';
    if (v('whiskerHighWidth') != null) s += '<i class="vz-table-lane-cap" style="left:' + n(v('whiskerHigh')) + '%"></i>';
    if (v('boxWidth') != null) s += '<i class="vz-table-lane-box" style="' + span(v('boxLeft'), v('boxWidth')) + '"></i>';
    return s + '<i class="vz-table-lane-med" style="left:' + n(v('median')) + '%"></i></div>';
  }

  /**
   * Point range: the outer fence, the range, the centre.
   * @param {TableLaneGeom} g
   * @param {number} i
   */
  function pointrange(g, i) {
    const v = (/** @type {keyof TableLaneGeom} */ k) =>
      /** @type {(number | null)[] | undefined} */ (g[k]) ? /** @type {(number | null)[]} */ (g[k])[i] : null;
    if (v('center') == null) return lane('prcell', g, i, false) + '</div>';
    let s = lane('prcell', g, i, true);
    if (v('fenceWidth') != null) s += '<i class="vz-table-lane-fence" style="' + span(v('fenceLeft'), v('fenceWidth')) + '"></i>';
    if (v('rangeWidth') != null) s += '<i class="vz-table-lane-rng" style="' + span(v('rangeLeft'), v('rangeWidth')) + '"></i>';
    return s + '<i class="vz-table-lane-ctr" style="left:' + n(v('center')) + '%"></i></div>';
  }

  /**
   * Pair (dumbbell): band, reference line, link, then the two ends.
   * @param {TablePairGeom} g
   * @param {number} i
   * @param {number | null | undefined} ref The reference line.
   */
  function pair(g, i, ref) {
    const fill = at(g.fill, i);
    let s = '<div class="vz-table-groups-lane vz-table-groups-pacell' +
      (g.dash && g.dash[i] ? ' is-dash' : '') + '"' +
      (fill != null ? ' style="--vz-table-groups-fill:' + col(fill) + '"' : '') +
      tipAttr(at(g.tip, i)) + '>';
    if (g.bandWidth[i] != null) s += '<i class="vz-table-lane-band" style="' + span(g.bandLeft[i], g.bandWidth[i]) + '"></i>';
    if (ref != null) s += '<i class="vz-table-lane-ref" style="left:' + n(ref) + '%"></i>';
    if (g.linkWidth[i] != null) s += '<i class="vz-table-lane-link" style="' + span(g.linkLeft[i], g.linkWidth[i]) + '"></i>';
    if (g.from[i] != null) s += '<i class="vz-table-lane-from" style="left:' + n(g.from[i]) + '%"></i>';
    if (g.to[i] != null) {
      s += '<i class="vz-table-lane-to' + (g.open && g.open[i] ? ' is-open' : '') + '" style="left:' +
        n(g.to[i]) + '%"></i>';
    }
    return s + '</div>';
  }

  /**
   * Swimlane segments: [left, width, level (from 1), label?] each.
   * @param {TableMarkInterval} c
   * @param {number} i
   */
  function interval(c, i) {
    let s = '<div class="vz-table-groups-lane vz-table-groups-ivcell" data-d0="' + n(c.domainStart) +
      '" data-d1="' + n(c.domainEnd) + '"' + (c.dates ? ' data-dd="1"' : '') + '>';
    (c.segs[i] || []).forEach((g, j) => {
      s += '<i class="vz-table-lane-seg" style="' + span(g[0], g[1]) + ';background:' +
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
    let s = '<div class="vz-table-groups-lane vz-table-groups-spcell" data-xs="' +
      esc(c.xValues[i]) + '" data-ys="' + esc(c.yValues[i]) + '">' +
      '<svg viewBox="0 0 100 36" preserveAspectRatio="none">';
    if (c.refBandY != null) {
      s += '<rect class="vz-table-lane-refband" x="0" y="' + n(c.refBandY) + '" width="100" height="' +
        n(c.refBandHeight) + '"></rect>';
    }
    if (c.band && c.band[i] != null) s += '<polygon class="vz-table-lane-band" points="' + points(c.band[i]) + '"></polygon>';
    if (c.refY != null) {
      s += '<line class="vz-table-lane-refline" x1="0" y1="' + n(c.refY) + '" x2="100" y2="' + n(c.refY) +
        '" vector-effect="non-scaling-stroke"></line>';
    }
    if (c.line[i]) {
      s += '<polyline class="vz-table-lane-ln" points="' + points(c.line[i]) +
        '" vector-effect="non-scaling-stroke"></polyline>';
    }
    s += '</svg>';
    if (c.dotX[i] != null) {
      s += '<i class="vz-table-lane-dot" style="left:' + n(c.dotX[i]) + '%;top:' + n(c.dotY[i]) + '%"></i>';
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
    if (c.kind === 'pair') return g.from[i] != null || g.to[i] != null;
    return (c.kind === 'box' ? g.median[i] : g.center[i]) != null;
  }

  /**
   * A colour split of lanes: one per level that has rows here, each in its
   * colour, with its value when labels are on.
   * @param {TableMarkLane | TableMarkPair} c
   * @param {number} i
   * @param {boolean} labels
   */
  function multi(c, i, labels) {
    let s = '<div class="vz-table-groups-multi">';
    (c.lanes || []).forEach((g, j) => {
      if (!drawn(c, g, i)) return;
      const inner = c.kind === 'pair' ? pair(/** @type {TablePairGeom} */ (g), i, c.reference)
        : c.kind === 'box' ? box(g, i) : pointrange(g, i);
      const lab = labels && g.disp && c.valueWidth != null;
      s += '<div class="vz-table-groups-lv' + (lab ? ' vz-table-groups-barwrap' : '') +
        '" style="--vz-table-groups-fill:' + col((c.fills || [])[j]) + '">' + inner +
        (lab ? barval(esc(at(g.disp, i)), c.valueWidth) : '') + '</div>';
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
      case 'bar': return track(c.width[i], c.fill, !!(c.sub && c.sub[i]));
      case 'barsplit': return split(c, i, labels);
      case 'bardiv': return diverging(c.width[i], !!c.positive[i]);
      case 'box': return c.multi ? multi(c, i, labels) : box(c, i);
      case 'pointrange': return c.multi ? multi(c, i, labels) : pointrange(c, i);
      case 'pair': return c.multi ? multi(c, i, labels) : pair(/** @type {TablePairGeom} */ (c), i, c.reference);
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
    return esc(c.disp[i]) + (p ? ' <span class="vz-table-groups-pct">' + esc(p) + '</span>' : '');
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
      return '<td class="vz-table-groups-tile-col">' + (bg == null ? ''
        : '<span class="vz-table-groups-tile" style="background:' + col(bg) + ';color:' +
          col(at(c.fg, i)) + '">' + esc(c.disp[i]) + '</span>') + '</td>';
    }
    if (!m) {
      return c.num ? '<td class="vz-table-groups-num vz-table-col-num">' + value(c, i) + '</td>'
        : '<td class="vz-table-groups-txt' + (c.failed ? ' is-failed' : '') + '">' +
          esc(c.disp[i]) + '</td>';
    }
    if (m.kind === 'interval') {
      return '<td class="vz-table-groups-bar-col' + (m.large ? ' vz-table-groups-wide' : '') + '">' +
        glyph(m, i, o.labels) + '</td>';
    }
    const g = glyph(m, i, o.labels);
    // A grouped split and a multi lane label each level; a stacked split
    // keeps its one total beside the bar.
    const own = (m.kind === 'barsplit' && m.mode === 'grouped') || (/** @type {any} */ (m).multi);
    const lab = o.labels && !own && 'valueWidth' in m && m.valueWidth != null && c.disp[i] != null;
    return '<td class="vz-table-groups-bar-col' + (m.kind === 'sparkline' ? ' is-spark' : '') + '">' + (lab
      ? '<div class="vz-table-groups-barwrap">' + g + barval(value(c, i), /** @type {any} */ (m).valueWidth) +
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
    const dw = labels && c.mark && 'valueWidth' in c.mark ? /** @type {any} */ (c.mark).valueWidth : null;
    const ticks = a.at.map((p, k) => '<span' + (p <= 8 ? ' class="is-first"' : p >= 92 ? ' class="is-last"' : '') +
      ' style="left:' + n(p) + '%">' + esc(a.labels[k]) + '</span>').join('');
    return '<div class="vz-table-groups-axis' + (dw != null ? ' has-val' : '') + '">' +
      '<span class="vz-table-groups-axis-in">' + ticks + '</span>' +
      (dw != null ? '<span class="vz-table-groups-axis-pad" style="width:' + n(dw) + 'ch"></span>' : '') +
      '</div>';
  }

  V.table.marks = { glyph, cell, axis };
})();
