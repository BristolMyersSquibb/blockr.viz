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
 *   box        whiskers, box and median
 *   pointrange a range and its centre, with an outer fence
 *   pair       a dumbbell: band, reference, link, from and to
 *   interval   swimlane segments on a time axis, with ticks, a line,
 *              point events and an open end, and thin lanes above
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
  /** @param {(string | number | null)[] | null | undefined} a @param {number} i */
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

  /** @param {string} text @param {number | null | undefined} dw @param {boolean} [zero] */
  const barval = (text, dw, zero) => '<span class="vz-table-groups-barval' + (zero ? ' is-zero' : '') +
    '" style="width:' + n(dw) + 'ch">' + text + '</span>';

  /**
   * One segment of a colour split.
   * @param {TableMarkSplit} c
   * @param {number} j Level.
   * @param {number} i Row.
   */
  function segment(c, j, i) {
    const lab = c.mode !== 'grouped' && c.segLabels ? at(c.segLabels[j], i) : null;
    return '<div class="vz-table-groups-fill' + (lab ? ' has-lab' : '') + '" data-seg="' + j +
      '" style="width:' + n(c.segWidths[j][i]) + '%;background:' + col(c.fills[j]) + '">' +
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

  /** @param {string} name @param {TableLaneGeom} g */
  const lane = (name, g) => '<div class="vz-table-groups-lane vz-table-groups-' + name +
    (g.bare ? ' is-bare' : '') + '">';

  /**
   * Box: whiskers, the box and the median cut out of it.
   * @param {TableLaneGeom} g
   * @param {number} i
   */
  function box(g, i) {
    const v = (/** @type {keyof TableLaneGeom} */ k) =>
      /** @type {(number | null)[] | undefined} */ (g[k]) ? /** @type {(number | null)[]} */ (g[k])[i] : null;
    if (v('median') == null) return lane('boxcell', g) + '</div>';
    let s = lane('boxcell', g);
    if (v('whiskerLowWidth') != null) {
      s += '<i class="vz-table-lane-wh" style="' + span(v('whiskerLow'), v('whiskerLowWidth')) + '"></i>';
    }
    if (v('whiskerHighWidth') != null) {
      s += '<i class="vz-table-lane-wh" style="' + span(v('boxRight'), v('whiskerHighWidth')) + '"></i>';
    }
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
    if (v('center') == null) return lane('prcell', g) + '</div>';
    let s = lane('prcell', g);
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
      (fill != null ? ' style="--vz-table-groups-fill:' + col(fill) + '"' : '') + '>';
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
   * One swimlane segment, [left, width, level (from 1), label?, style?], in
   * its style: 1 solid, 2 dashed, 3 hollow (outlined), 4 dot (a ring at its
   * start), 5 step (a down step at its start), 6 outline (filled, with a dark
   * edge).
   * @param {(number | string)[]} g
   * @param {string} fill
   * @param {string | number | null} tip
   * @param {string} [jn] Join classes from joins().
   */
  function seg(g, fill, tip, jn) {
    const lab = g.length > 3 && g[3] !== '' ? ' data-l="' + esc(g[3]) + '"' : '';
    jn = jn || '';
    const t = ' data-tip="' + esc(tip) + '"';
    switch (Number(g[4]) || 1) {
      case 2: return '<i class="vz-table-lane-seg is-dashed' + jn + '" style="' + span(g[0], g[1]) +
        ';--vz-seg:' + col(fill) + '"' + lab + t + '></i>';
      case 3: return '<i class="vz-table-lane-seg is-hollow' + jn + '" style="' + span(g[0], g[1]) +
        ';border-color:' + col(fill) + '"' + lab + t + '></i>';
      case 4: return '<i class="vz-table-lane-adm" style="left:' + n(g[0]) + '%;border-color:' +
        col(fill) + '"' + t + '></i>';
      case 5: return '<i class="vz-table-lane-step" style="left:' + n(g[0]) + '%"' + t + '></i>';
      case 6: return '<i class="vz-table-lane-seg is-outline' + jn + '" style="' + span(g[0], g[1]) + ';background:' +
        col(fill) + '"' + lab + t + '></i>';
      case 7: return '<i class="vz-table-lane-seg is-thinner' + jn + '" style="' + span(g[0], g[1]) + ';background:' +
        col(fill) + '"' + lab + t + '></i>';
      case 8: return '<i class="vz-table-lane-seg is-hairline' + jn + '" style="' + span(g[0], g[1]) + ';background:' +
        col(fill) + '"' + lab + t + '></i>';
      default: return '<i class="vz-table-lane-seg' + jn + '" style="' + span(g[0], g[1]) + ';background:' +
        col(fill) + '"' + lab + t + '></i>';
    }
  }

  /**
   * Where a lane's segments touch: ' is-jl' when another segment ends at this
   * one's start, ' is-jr' when another starts at its end. A run of touching
   * segments then rounds only its free ends, as a bar does. Rings (4) and
   * steps (5) are no segments and neither join nor are joined.
   * @param {(number | string)[][]} segs
   */
  function joins(segs) {
    const solid = (/** @type {(number | string)[]} */ g) => {
      const k = Number(g[4]) || 1;
      return k !== 4 && k !== 5;
    };
    const x0 = segs.map((g) => Number(g[0]));
    const x1 = segs.map((g) => Number(g[0]) + Number(g[1]));
    return segs.map((g, j) => {
      if (!solid(g)) return '';
      let l = false, r = false;
      segs.forEach((h, k) => {
        if (k === j || !solid(h)) return;
        if (Math.abs(x1[k] - x0[j]) < 0.01) l = true;
        if (Math.abs(x0[k] - x1[j]) < 0.01) r = true;
      });
      return (l ? ' is-jl' : '') + (r ? ' is-jr' : '');
    });
  }

  /**
   * Bar labels of one lane, placed by fitLabels() once the lane has a width:
   * on the bar, after it, or hidden. Each knows its bar's ends and where the
   * lane's next bar starts (percent), and whether an arrow follows it.
   * @param {(number | string)[][]} segs
   * @param {(string | null)[] | null | undefined} labels
   * @param {number | null | undefined} open Where the lane's arrow sits.
   */
  function barLabels(segs, labels, open) {
    if (!labels) return '';
    let s = '';
    segs.forEach((g, j) => {
      const t = labels[j];
      if (!t) return;
      const x0 = Number(g[0]), x1 = x0 + Number(g[1]);
      const next = segs.reduce((m, h) => (Number(h[0]) >= x1 - 0.01 && Number(h[0]) < m ? Number(h[0]) : m), 100);
      const arrow = open != null && Math.abs(Number(open) - x1) < 0.5;
      s += '<span class="vz-table-lane-lab" style="left:' + n(x0) + '%" data-x0="' + n(x0) + '" data-x1="' +
        n(x1) + '" data-nx="' + n(next) + '"' + (arrow ? ' data-arrow="1"' : '') + '>' + esc(t) + '</span>';
    });
    return s;
  }

  /**
   * Place the bar labels under `host` that are not placed yet: on the bar when
   * it fits, else after it when the gap to the lane's next bar holds it, else
   * hidden (the tooltip still names the bar).
   * @param {ParentNode} host
   * @param {boolean} [again] Place every label anew (the width changed).
   */
  function fitLabels(host, again) {
    const labs = /** @type {HTMLElement[]} */ (Array.from(
      host.querySelectorAll(again ? '.vz-table-lane-lab' : '.vz-table-lane-lab:not(.is-fit)')));
    if (!labs.length) return;
    // Every read in one pass and every write in another: a write between two
    // reads makes the browser lay the table out again, once per label.
    if (again) {
      labs.forEach((lab) => {
        lab.classList.remove('is-fit', 'is-on', 'is-after');
        lab.hidden = false;
        lab.style.left = n(lab.getAttribute('data-x0')) + '%';
        lab.style.maxWidth = '';
      });
    }
    const room = labs.map((lab) => (lab.parentElement ? lab.parentElement.clientWidth : 0));
    labs.forEach((lab, j) => {
      if (room[j]) lab.classList.add('is-fit');
    });
    const text = labs.map((lab, j) => (room[j] ? lab.offsetWidth : 0));
    labs.forEach((lab, j) => {
      const w = room[j];
      if (!w) return;
      const px = (/** @type {string} */ k) => Number(lab.getAttribute(k)) / 100 * w;
      const tw = text[j];
      if (tw + 6 <= px('data-x1') - px('data-x0')) {
        lab.classList.add('is-on');
        lab.style.maxWidth = n(Math.floor(px('data-x1') - px('data-x0'))) + 'px';
        return;
      }
      const gap = lab.hasAttribute('data-arrow') ? 10 : 3;
      if (px('data-x1') + gap + tw + 3 <= px('data-nx')) {
        lab.classList.add('is-after');
        lab.style.left = 'calc(' + n(lab.getAttribute('data-x1')) + '% + ' + gap + 'px)';
        lab.style.maxWidth = n(Math.floor(px('data-nx') - px('data-x1') - gap)) + 'px';
        return;
      }
      lab.hidden = true;
    });
  }

  /**
   * A lane's opening tag: its domain for the hover's time, and its strip.
   * @param {TableMarkInterval} c
   * @param {string} cls
   * @param {any[] | null | undefined} strip [text, style] boxes.
   * @param {string} [name]
   */
  function laneOpen(c, cls, strip, name) {
    return '<div class="vz-table-groups-lane vz-table-groups-ivcell' + cls + '" data-d0="' +
      n(c.domainStart) + '" data-d1="' + n(c.domainEnd) + '"' + (c.dates ? ' data-dd="1"' : '') +
      (name != null ? ' data-n="' + esc(name) + '"' : '') +
      (strip && strip.length ? ' data-strip="' + esc(JSON.stringify(strip)) + '"' : '') + '>';
  }

  /** A full bar's name at its left, in the cell's gutter; the main one bold.
   * @param {TableMarkInterval} c @param {string | undefined} name @param {boolean} main */
  const laneName = (c, name, main) => (c.mainName == null || name == null ? ''
    : '<span class="vz-table-lane-name' + (main ? ' is-main' : '') + '">' + esc(name) + '</span>');

  /**
   * A thin lane above the main one, or a full bar below it (`f`, a second
   * response assessment): its segments, and its arrow in its own colour when
   * it is still open.
   * @param {TableMarkInterval} c
   * @param {TableSubLane} l
   */
  function thin(c, l) {
    const cls = l.f ? ' is-full' : c.calmLanes ? ' is-thin is-md' : ' is-thin';
    let s = laneOpen(c, cls, l.b, l.n) + (l.f ? laneName(c, l.n, false) : '');
    let last = '';
    const jn = joins(l.s || []);
    (l.s || []).forEach((g, j) => {
      last = c.fills[Number(g[2]) - 1];
      s += seg(g, last, at(l.t, j), jn[j]);
    });
    if (l.o != null) {
      s += '<i class="vz-table-lane-open" style="left:' + n(l.o) + '%;border-left-color:' + col(last) + '"></i>';
    }
    // A thin lane is too low for text; an even lane and a full bar have room.
    if (c.evenLanes || l.f) s += barLabels(l.s || [], l.l, l.o);
    return s + events(l.e, l.et) + '</div>';
  }

  /**
   * Point events [x, shape (, tag)] with their tips. An event with a tag
   * draws its text in a small box just after its position.
   * @param {(number | string)[][] | undefined} ev
   * @param {string[] | undefined} tips
   */
  function events(ev, tips) {
    return (ev || []).map((e, j) => {
      const tip = '" data-tip="' + esc(tips ? tips[j] : '') + '">';
      if (e.length > 2 && e[2] != null && e[2] !== '') {
        return '<span class="vz-table-lane-ev is-tag" style="left:' + n(e[0]) + '%' + tip + esc(e[2]) + '</span>';
      }
      return '<svg class="vz-table-lane-ev" viewBox="-7 -7 14 14" style="left:' + n(e[0]) + '%' + tip +
        shape(Number(e[1])) + '</svg>';
    }).join('');
  }

  /**
   * Swimlane: the main lane's segments and overlays, under the thin lanes
   * when the row has any.
   * @param {TableMarkInterval} c
   * @param {number} i
   */
  function interval(c, i) {
    let s = laneOpen(c, '', c.mainStrip ? c.mainStrip[i] : null) + laneName(c, c.mainName, true);
    const jn = joins(c.segs[i] || []);
    (c.segs[i] || []).forEach((g, j) => {
      s += seg(g, c.fills[Number(g[2]) - 1], at(c.tips[i], j), jn[j]);
    });
    // Overlays, under the pointer only where they carry a tooltip: ticks
    // across the segments, the line through them, the open end, the events.
    ((c.ticks && c.ticks[i]) || []).forEach((x) => {
      s += '<i class="vz-table-lane-tk" style="left:' + n(x) + '%"></i>';
    });
    ((c.band && c.band[i]) || []).forEach((b) => {
      s += '<i class="vz-table-lane-dor" style="' + span(b[0], b[1]) + '"></i>';
    });
    const op = c.openAt ? c.openAt[i] : null;
    if (op != null) s += '<i class="vz-table-lane-open" style="left:' + n(op) + '%"></i>';
    s += barLabels(c.segs[i] || [], c.barLabels ? c.barLabels[i] : null, op);
    s += events(c.events ? c.events[i] : undefined, c.eventTips ? c.eventTips[i] : undefined) + '</div>';
    const sub = c.subLanes ? c.subLanes[i] || [] : [];
    // Named bars keep a gutter on every row, so the bars line up with the axis.
    const named = (/** @type {string} */ h) => (c.mainName == null ? h
      : '<div class="vz-table-groups-ivnamed" style="--vz-lane-lw:' + n(c.laneNameWidth) + 'ch">' +
        h + '</div>');
    if (!sub.length) return named(s);
    // Thin lanes above the main lane, full bars below it. Even lanes: no main
    // lane, every lane full, the overlays across the row.
    return named('<div class="vz-table-groups-ivstack' + (c.evenLanes ? ' is-even' : '') + '">' +
      sub.filter((l) => !l.f).map((l) => thin(c, l)).join('') + s +
      sub.filter((l) => l.f).map((l) => thin(c, l)).join('') + '</div>');
  }

  /**
   * An event shape, centred on 0 in a 14-unit box, about 9px across, over a
   * 1.5px ring in the surface colour: 1 filled triangle, 2 open triangle,
   * 3 cross, 4 square, 5 vertical bar, 6 the empty box a tag sits in (the
   * legend's sign for tagged events). Filled shapes carry a round-joined
   * stroke in their ink, which rounds their corners. The static painter
   * (R/paint-marks.R, paint_shape) draws the same.
   * @param {number} k
   */
  function shape(k) {
    const tri = 'M0,-3.4L3.6,2.8L-3.6,2.8Z';
    const open = 'M0,-3.9L4.1,3.1L-4.1,3.1Z';
    const x = 'M-3.2,-3.2L3.2,3.2M3.2,-3.2L-3.2,3.2';
    const sq = 'M-3.2,-3.2H3.2V3.2H-3.2Z';
    const bar = 'M0,-4.5V4.5';
    const two = (/** @type {string} */ d, /** @type {string} */ cls) =>
      '<path class="vz-ev-ring' + (cls === 'vz-ev-fill' ? '' : ' is-' + cls.slice(6)) + '" d="' + d +
      '"></path><path class="' + cls + '" d="' + d + '"></path>';
    switch (k) {
      case 6: return two('M-5.5,-3.5H5.5V3.5H-5.5Z', 'vz-ev-open');
      case 2: return two(open, 'vz-ev-open');
      case 3: return two(x, 'vz-ev-line');
      case 4: return two(sq, 'vz-ev-fill');
      case 5: return two(bar, 'vz-ev-line');
      default: return two(tri, 'vz-ev-fill');
    }
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
   * @param {boolean} [zero] The column mutes a zero (TableCol.zero).
   */
  function multi(c, i, labels, zero) {
    let s = '<div class="vz-table-groups-multi">';
    (c.lanes || []).forEach((g, j) => {
      if (!drawn(c, g, i)) return;
      const inner = c.kind === 'pair' ? pair(/** @type {TablePairGeom} */ (g), i, c.reference)
        : c.kind === 'box' ? box(g, i) : pointrange(g, i);
      const lab = labels && g.disp && c.valueWidth != null;
      const d = lab ? at(g.disp, i) : null;
      s += '<div class="vz-table-groups-lv' + (lab ? ' vz-table-groups-barwrap' : '') +
        '" style="--vz-table-groups-fill:' + col((c.fills || [])[j]) + '">' + inner +
        (lab ? barval(esc(d), c.valueWidth, !!zero && d != null && Number(d) === 0) : '') + '</div>';
    });
    return s + '</div>';
  }

  /**
   * The mark of one cell, without its value: shared by the table's cells
   * and the cards. "" for a column without a mark.
   * @param {TableMark | null | undefined} c
   * @param {number} i
   * @param {boolean} labels
   * @param {boolean} [zero] The column mutes a zero (TableCol.zero).
   */
  function glyph(c, i, labels, zero) {
    if (!c) return '';
    switch (c.kind) {
      case 'bar': return track(c.width[i], c.fill, !!(c.sub && c.sub[i]));
      case 'barsplit': return split(c, i, labels);
      case 'bardiv': return diverging(c.width[i], !!c.positive[i]);
      case 'box': return c.multi ? multi(c, i, labels, zero) : box(c, i);
      case 'pointrange': return c.multi ? multi(c, i, labels, zero) : pointrange(c, i);
      case 'pair': return c.multi ? multi(c, i, labels, zero) : pair(/** @type {TablePairGeom} */ (c), i, c.reference);
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
      const wide = m.full ? ' vz-table-groups-full' : m.large ? ' vz-table-groups-wide' : '';
      return '<td class="vz-table-groups-bar-col' + wide + '">' +
        glyph(m, i, o.labels) + '</td>';
    }
    const g = glyph(m, i, o.labels, !!c.zero);
    // A grouped split and a multi lane label each level; a stacked split
    // keeps its one total beside the bar.
    const own = (m.kind === 'barsplit' && m.mode === 'grouped') || (/** @type {any} */ (m).multi);
    // Every cell of a labelled column keeps the value slot, so all its tracks
    // end at the same x, whatever the mark. A missing value leaves the slot
    // blank; a zero count prints muted.
    const slot = o.labels && !own && 'valueWidth' in m && m.valueWidth != null;
    const has = c.disp[i] != null;
    return '<td class="vz-table-groups-bar-col' + (m.kind === 'sparkline' ? ' is-spark' : '') + '">' + (slot
      ? '<div class="vz-table-groups-barwrap">' + g + barval(has ? value(c, i) : '',
        /** @type {any} */ (m).valueWidth, has && !!c.zero && at(c.key, i) === 0) + '</div>'
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
    // A swimlane with named bars leaves their gutter blank.
    const lw = /** @type {TableMarkInterval} */ (/** @type {unknown} */ (c.mark || {})).laneNameWidth;
    return '<div class="vz-table-groups-axis' + (dw != null ? ' has-val' : '') + '">' +
      (lw != null ? '<span class="vz-table-groups-axis-lpad" style="--vz-lane-lw:' + n(lw) + 'ch"></span>' : '') +
      '<span class="vz-table-groups-axis-in">' + ticks + '</span>' +
      (dw != null ? '<span class="vz-table-groups-axis-pad" style="width:' + n(dw) + 'ch"></span>' : '') +
      '</div>';
  }

  V.table.marks = { glyph, cell, axis, shape, fitLabels };
})();
