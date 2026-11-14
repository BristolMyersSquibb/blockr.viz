// blockr.viz bundle 2f8afb23786e1f65f36869ba7ab4ad27, built by tools/bundle.js. Do not edit.
// sources: table/html.js table/marks.js table/axis.js table/tooltip.js table/sort.js table/dom.js table/drill.js table/chrome.js table/cards.js table/gear.js table/summaries.js table/records.js table/groups.js table/variables.js table/view.js table/static.js
// --- table/html.js
// @ts-check
/**
 * The markup of a Grid's table (dom.js): header cells, section headers,
 * rows and cells, as HTML text from a TableModel. Every value from the
 * model goes through the one escaper. The classes are those the table CSS
 * styles; data-h, data-s and data-i find the model entry again on a click.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const esc = V.util.esc;

  const CHEV = '<svg class="vz-table-chev" viewBox="0 0 24 24" fill="none" ' +
    'stroke="currentColor" stroke-width="2.4" stroke-linecap="round" ' +
    'stroke-linejoin="round" aria-hidden="true"><path d="M6 9l6 6 6-6"/></svg>';

  /**
   * The text of a header cell: name lines, then the quiet line and the sort
   * arrow in one row under them.
   * @param {TableHeadCell} h
   * @param {boolean} sortable
   */
  function headText(h, sortable) {
    const name = '<span class="blockr-col-name vz-table-arm-name">' +
      String(h.text == null ? '' : h.text).split('\n').map(esc).join('<br>') + '</span>';
    const icon = sortable ? '<span class="blockr-sort-icon"></span>' : '';
    // A spanner aligns by its cell's text-align, which a flex row would undo.
    if (h.col == null && !h.stub) {
      return name + (h.sub ? '<div class="vz-table-th-subrow"><span class="blockr-col-label vz-table-arm-n">' +
        esc(h.sub) + '</span></div>' : '');
    }
    if (h.sub) {
      return name + '<div class="vz-table-th-subrow"><span class="blockr-col-label vz-table-arm-n">' +
        esc(h.sub) + '</span>' + icon + '</div>';
    }
    return '<div class="vz-table-th-namerow">' + name + icon + '</div>';
  }

  /**
   * One header cell; `r` and `c` find it again in `model.head`.
   * @param {TableModel} model
   * @param {number} r
   * @param {number} c
   * @param {{ sortable: boolean, foldAll: boolean, axis?: (c: TableCol) => string }} f
   */
  function th(model, r, c, f) {
    const h = model.head[r][c];
    const col = h.col != null ? model.cols[h.col] : null;
    const sortable = f.sortable && h.col != null && (h.span || 1) === 1;
    const cls = [h.stub ? 'vz-table-stub-header' : 'vz-table-col-header',
      h.stub ? '' : (h.col != null ? 'leaf' : 'group'),
      !h.stub && h.col == null && r > 0 ? 'group-inner' : '',
      col ? (col.num ? 'vz-table-col-num' : 'vz-table-col-txt') : '',
      sortable ? 'blockr-sortable' : '', h.keys ? 'vz-table-keyed' : '',
      h.stub && h.bold ? 'vz-table-stub-header-bold' : ''];
    let inner = headText(h, sortable) + (col && f.axis ? f.axis(col) : '');
    if (h.stub && f.foldAll) {
      cls.push('vz-table-has-foldall');
      inner = '<div class="vz-table-foldall-wrap"><button type="button" class="vz-table-foldall" ' +
        'aria-expanded="true" aria-label="Collapse all groups">' + CHEV +
        (h.text ? '' : '<span class="vz-table-foldall-all">All</span>') + '</button>' +
        (h.text ? '<span class="vz-table-stub-title">' + inner + '</span>' : '') + '</div>';
    }
    return '<th class="' + cls.filter(Boolean).join(' ') + '" data-h="' + r + ',' + c + '"' +
      ((h.span || 1) > 1 ? ' colspan="' + h.span + '"' : '') +
      ((h.rows || 1) > 1 ? ' rowspan="' + h.rows + '"' : '') +
      (h.tip ? ' data-blockr-tooltip="' + esc(h.tip) + '"' : '') +
      (sortable ? ' data-col-index="' + h.col + '" tabindex="0" aria-sort="none"' : '') +
      '>' + inner + '</th>';
  }

  /**
   * One body cell: the text, its fill and ink, or a data bar.
   * @param {TableCol} c
   * @param {number} i
   */
  function td(c, i) {
    const d = c.disp[i];
    const base = 'vz-table-data ' + (c.num ? 'vz-table-num' : 'vz-table-txt');
    /** @type {string[]} */
    const style = [];
    const bg = c.bg ? c.bg[i] : null;
    if (bg) style.push('background:' + bg);
    const fg = c.fg ? c.fg[i] : null;
    if (fg) style.push('color:' + fg);
    const w = c.bar ? c.bar[i] : null;
    if (w != null && c.bar_fill) {
      const pct = Math.round(Math.max(0, Math.min(1, w)) * 10000) / 100;
      style.push('background:linear-gradient(90deg,' + c.bar_fill + ' 0 ' + pct +
                 '%,transparent ' + pct + '%)');
    }
    const st = style.length ? ' style="' + esc(style.join(';')) + '"' : '';
    // A painted empty cell is part of the picture, not missing data.
    if (d == null) {
      return bg ? '<td class="' + base + '"' + st + '></td>'
        : '<td class="' + base + ' vz-table-dash">&mdash;</td>';
    }
    return '<td class="' + base + '"' + st + '>' + esc(d) + '</td>';
  }

  /**
   * A section header row: the chevron, the label and the value.
   * @param {TableModel} m
   * @param {number} s
   * @param {{ shut: boolean, fold: boolean, extra: string }} o
   */
  function section(m, s, o) {
    const sec = /** @type {TableSection[]} */ (m.sections)[s];
    const label = sec.label ? '<span class="vz-table-section-label">' + esc(sec.label + ': ') +
      '</span>' : '';
    const btn = o.fold
      ? '<button class="vz-table-section-btn" type="button" aria-expanded="' + !o.shut + '">' + CHEV
      : '<span class="vz-table-section-btn vz-table-section-btn-static">' +
        '<span class="vz-table-chev" aria-hidden="true"></span>';
    return '<tr class="vz-table-section-header' + (o.shut ? ' collapsed' : '') +
      (sec.keys ? ' vz-table-keyed' : '') + (o.extra ? ' ' + o.extra : '') + '" data-s="' + s +
      '" data-level="' + sec.level + '"><td class="vz-table-section-cell level-' + sec.level +
      ' vz-table-section-nest" colspan="' + (m.cols.length || 1) + '">' + btn + label +
      '<span class="vz-table-section-value">' + esc(sec.value) + '</span>' +
      (o.fold ? '</button>' : '</span>') + '</td></tr>';
  }

  /**
   * How many indent steps row `i`'s label takes: one per level below the
   * shallowest row and per section level past the first. With `flush` (the
   * groups layout, as 0.2 drew it) a row under a folding parent takes one
   * fewer, so it starts on its parent's label edge, right after the
   * parent's chevron; a structured table keeps its child one step in.
   * @param {TableStructure} st @param {number} i @param {boolean} [flush]
   */
  function indentSteps(st, i, flush) {
    const depth = st.indent ? st.indent[i] - st.base : 0;
    const under = flush && st.folds && depth > 0 && !st.toggle[i] ? 1 : 0;
    return depth - under + Math.max(0, st.levels - 1);
  }

  /**
   * The stub cell: indented under its sections and its parent rows, with
   * the fold button of an indented group and the row tint. The indent goes
   * to CSS as a number of steps (`--vz-indent`), which each layout turns
   * into its own padding (indentSteps()).
   * @param {TableModel} m
   * @param {TableStructure} st
   * @param {number} i
   * @param {boolean} shut
   * @param {string} [cls] The layout's own classes for the stub.
   * @param {boolean} [flush] See indentSteps().
   * @param {number} [span] The stub spans this many columns (a heading row).
   */
  function stub(m, st, i, shut, cls, flush, span) {
    const c = m.cols[/** @type {number} */ (m.stub)];
    const steps = indentSteps(st, i, flush);
    /** @type {string[]} */
    const style = [];
    if (steps > 0) style.push('--vz-indent:' + steps);
    const tint = m.rows && m.rows.tint ? m.rows.tint[i] : null;
    if (tint) style.push('box-shadow:inset 3px 0 0 0 ' + tint);
    const d = c.disp[i];
    const btn = st.toggle[i]
      ? '<button class="vz-table-indent-btn" type="button" tabindex="-1" aria-expanded="' +
        !shut + '">' + CHEV + '</button>'
      : '';
    const text = d == null ? '&mdash;' : esc(d);
    // A folded groups heading is followed by the number of rows it hides.
    const nk = span && shut && m.rows && m.rows.count ? m.rows.count[i] : null;
    const count = nk == null ? '' : '<span class="vz-table-heading-count">' + esc(String(nk)) + '</span>';
    // A heading across the row: its text adds nothing to the table's width
    // and ends in an ellipsis only where the table ends.
    return '<td class="vz-table-stub' + (cls ? ' ' + cls : '') + (st.toggle[i] ? ' blockr-has-toggle' : '') +
      (tint ? ' blockr-row-bar' : '') + (span ? ' vz-table-heading-cell' : '') + '"' +
      (span ? ' colspan="' + span + '"' : '') +
      (style.length ? ' style="' + esc(style.join(';')) + '"' : '') + '>' + btn +
      (span ? '<span class="vz-table-heading-text"' + (d == null ? '' : ' title="' + esc(d) + '"') +
        '>' + text + count + '</span>' : text) + '</td>';
  }

  /**
   * How row `i` is drawn as a heading: `span`, one cell across the row (a
   * row that heads others and has no values, marked by R in `rows.span`, or
   * by the layout); "" a plain row.
   * @param {TableModel} m
   * @param {number} i
   * @param {string | undefined} head The layout's choice, if any.
   * @returns {'' | 'span'}
   */
  function headKind(m, i, head) {
    if (m.stub !== 0 || m.cols.length < 2) return '';
    if (head === 'span') return head;
    return m.rows && m.rows.span && m.rows.span[i] ? 'span' : '';
  }

  /**
   * One row. A heading (headKind()) is one cell across the row.
   * @param {TableModel} m
   * @param {TableStructure} st
   * @param {number} i
   * @param {{ last: boolean, shut: boolean, extra: string, stubCls?: string, flush?: boolean,
   *           head?: string, cell?: (c: TableCol, i: number) => string | null }} o
   */
  function row(m, st, i, o) {
    const r = m.rows || {};
    const head = headKind(m, i, o.head);
    const cls = ['vz-table-row'];
    if (r.strong && r.strong[i]) cls.push('vz-table-bold');
    if (r.emph && r.emph[i]) cls.push('vz-table-italic');
    const ind = st.indent ? st.indent[i] : 0;
    // A row over deeper rows is a heading, 13px from the third level on.
    if (head || (st.indent && i + 1 < st.indent.length && st.indent[i + 1] > ind)) {
      cls.push('vz-table-heads');
      if (indentSteps(st, i, o.flush) >= 2) cls.push('vz-table-heads-deep');
    }
    if (o.extra) cls.push(o.extra);
    const open = (/** @type {string[]} */ c) =>
      '<tr class="' + c.join(' ') + '" data-i="' + i + '" data-indent="' + ind + '">';
    /** @type {string[]} */
    const own = [];
    if (st.toggle[i]) own.push('vz-table-indent-toggle');
    if (st.toggle[i] && o.shut) own.push('collapsed');
    const n = m.cols.length;
    if (head === 'span') {
      return open(cls.concat(own, 'vz-table-heading', o.last ? ['vz-table-group-last'] : [])) +
        stub(m, st, i, o.shut, o.stubCls, o.flush, n) + '</tr>';
    }
    cls.push(...own);
    if (o.last) cls.push('vz-table-group-last');
    let out = open(cls);
    m.cols.forEach((c, j) => {
      const mine = j === m.stub || !o.cell ? null : o.cell(c, i);
      out += j === m.stub ? stub(m, st, i, o.shut, o.stubCls, o.flush) : mine != null ? mine : td(c, i);
    });
    return out + '</tr>';
  }

  /**
   * A section title over inner row `i` (`rows.title`): one cell across the
   * row, on the row's label edge, in small bold type.
   * @param {TableModel} m
   * @param {TableStructure} st
   * @param {number} i
   * @param {boolean} [flush] See indentSteps().
   */
  function title(m, st, i, flush) {
    const t = m.rows && m.rows.title ? m.rows.title[i] : null;
    const steps = indentSteps(st, i, flush);
    return '<tr class="vz-table-row-title" aria-hidden="true"><td class="vz-table-stub vz-table-groups-label-col"' +
      ' colspan="' + (m.cols.length || 1) + '"' + (steps > 0 ? ' style="--vz-indent:' + steps + '"' : '') + '>' +
      esc(t == null ? '' : t) + '</td></tr>';
  }

  V.table = V.table || /** @type {VizTableNamespace} */ ({ layouts: {} });
  V.table.html = { th, td, section, row, title, indentSteps, headKind };
})();
// --- table/marks.js
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
   * @param {boolean} even Whether the row's lanes are even.
   */
  function thin(c, l, even) {
    // In an even row a full bar is one more even lane.
    const full = l.f && !even;
    const cls = (full ? ' is-full' : c.calmLanes ? ' is-thin is-md' : ' is-thin') +
      (l.f && c.track ? ' has-track' : '');
    let s = laneOpen(c, cls, l.b, l.n) + (full ? laneName(c, l.n, false) : '');
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
    if (even || l.f) s += barLabels(l.s || [], l.l, l.o);
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
    const sub = c.subLanes ? c.subLanes[i] || [] : [];
    // Even lanes: no main lane, every lane full, the overlays across the row.
    // So it is for the whole column without a main-lane record anywhere, and
    // for a row without one of its own (an opened row holding one lane).
    const even = !!c.evenLanes || (sub.length > 0 && !(c.segs[i] || []).length);
    // An even row has no main bar to name.
    let s = laneOpen(c, c.track && !even ? ' has-track' : '', c.mainStrip ? c.mainStrip[i] : null) +
      (even ? '' : laneName(c, c.mainName, true));
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
    // Named bars keep a gutter on every row, so the bars line up with the axis.
    const named = (/** @type {string} */ h) => (c.mainName == null ? h
      : '<div class="vz-table-groups-ivnamed" style="--vz-lane-lw:' + n(c.laneNameWidth) + 'ch">' +
        h + '</div>');
    if (!sub.length) return named(s);
    // Thin lanes above the main lane, full bars below it.
    return named('<div class="vz-table-groups-ivstack' + (even ? ' is-even' : '') + '">' +
      sub.filter((l) => !l.f).map((l) => thin(c, l, even)).join('') + s +
      sub.filter((l) => l.f).map((l) => thin(c, l, even)).join('') + '</div>');
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
    const fit = a.d1 != null && a.d0 != null
      ? ' data-d0="' + n(a.d0) + '" data-d1="' + n(a.d1) + '"' + (a.unit ? ' data-unit="' + esc(a.unit) + '"' : '')
      : '';
    return '<div class="vz-table-groups-axis' + (dw != null ? ' has-val' : '') + '">' +
      (lw != null ? '<span class="vz-table-groups-axis-lpad" style="--vz-lane-lw:' + n(lw) + 'ch"></span>' : '') +
      '<span class="vz-table-groups-axis-in"' + fit + '>' + ticks + '</span>' +
      (dw != null ? '<span class="vz-table-groups-axis-pad" style="width:' + n(dw) + 'ch"></span>' : '') +
      '</div>';
  }

  V.table.marks = { glyph, cell, axis, shape, fitLabels };
})();
// --- table/axis.js
// @ts-check
/**
 * The ticks of a numeric swimlane axis, spaced by the column's drawn width.
 * R sends the axis with ticks for no particular width and, for a numeric
 * swimlane, its domain and unit (R/table-groups-cols.R table_groups_axis());
 * once the table is laid out, fit() replaces the ticks with the finest step
 * of the 1 / 2 / 2.5 / 5 ladder that keeps labels 60px apart, the first
 * after the unit ("Day 0"). A date axis keeps R's ticks.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const esc = V.util.esc;

  /** The steps, times a power of ten. */
  const STEPS = [1, 2, 2.5, 5];

  /**
   * The ticks across `d0` to `d1` drawn `w` px wide, labels `gap` px apart.
   * @param {number} d0 @param {number} d1 @param {number} w @param {number} [gap]
   * @returns {number[]}
   */
  function ticks(d0, d1, w, gap = 60) {
    const range = d1 - d0;
    if (!(range > 0) || !(w > 0)) return [];
    const min = range * gap / w;
    let mag = Math.pow(10, Math.floor(Math.log10(min)));
    let step = 0;
    while (!step) {
      for (const s of STEPS) if (!step && s * mag >= min) step = s * mag;
      mag *= 10;
    }
    /** @type {number[]} */
    const out = [];
    for (let x = Math.ceil(d0 / step - 1e-9) * step; x <= d1 + 1e-9; x += step) {
      out.push(Math.round(x * 1e6) / 1e6 || 0);
    }
    return out;
  }

  /**
   * Fit every numeric swimlane axis in `host` to its width; an axis is
   * redrawn only when its width changed.
   * @param {ParentNode} host
   */
  function fit(host) {
    const axes = /** @type {HTMLElement[]} */ (Array.from(host.querySelectorAll('.vz-table-groups-axis-in[data-d1]')));
    // Every width read before any write, so the table is laid out once.
    const ws = axes.map((el) => el.clientWidth);
    axes.forEach((el, k) => {
      const w = ws[k];
      if (!w || el.getAttribute('data-w') === String(w)) return;
      const d0 = Number(el.getAttribute('data-d0'));
      const d1 = Number(el.getAttribute('data-d1'));
      const unit = el.getAttribute('data-unit');
      const at = ticks(d0, d1, w);
      if (at.length < 2) return;
      el.setAttribute('data-w', String(w));
      el.innerHTML = at.map((x, j) => {
        const p = Math.round((x - d0) / (d1 - d0) * 1e4) / 100;
        const text = (j === 0 && unit ? unit + ' ' : '') + String(x);
        return '<span' + (p <= 8 ? ' class="is-first"' : p >= 92 ? ' class="is-last"' : '') +
          ' style="left:' + p + '%">' + esc(text) + '</span>';
      }).join('');
    });
  }

  V.table.axis = { ticks, fit };
})();
// --- table/tooltip.js
// @ts-check
/**
 * The hover card of the groups marks, in the chart's card style (the
 * .vz-tt-* rules): the row as the headline, the column's context under it,
 * then one labelled line per number. A bar names its value, a colour split
 * every level with its count and share, a box or a point range its
 * statistics, a pair its two ends. A swimlane segment or a sparkline point
 * says its own text; elsewhere on a swimlane the card reads the time under
 * the pointer, and a labelled segment lights every segment with its label.
 *
 * The listeners sit on the view's box and the card on <body>, both through
 * the view's Scope.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const esc = V.util.esc;

  /** @param {unknown} v */
  function num(v) {
    if (typeof v !== 'number' || !isFinite(v)) return v == null ? '' : String(v);
    if (Number.isInteger(v)) return v.toLocaleString();
    return Number(v.toPrecision(6)).toLocaleString(undefined, { maximumFractionDigits: 4 });
  }

  /** @param {unknown} v @param {unknown} den */
  const share = (v, den) => typeof v === 'number' && typeof den === 'number' && den
    ? ' (' + Math.round(v / den * 100) + '%)' : '';
  const sw = (/** @type {unknown} */ c) => c ? '<span class="vz-tt-sw" style="background:' + esc(c) + '"></span>' : '';
  const head = (/** @type {unknown} */ t, /** @type {unknown} */ c = null) =>
    '<div class="vz-tt-head">' + sw(c) + '<span>' + esc(t) + '</span></div>';
  /** @param {unknown} label @param {unknown} value @param {unknown} [c] @param {boolean | null} [hit] */
  const row = (label, value, c, hit) => '<div class="vz-tt-row' +
    (hit === true ? ' is-hit' : hit === false ? ' is-dim' : '') + '">' + sw(c) +
    '<span class="vz-tt-label">' + esc(label) + '</span><span class="vz-tt-value">' + esc(value) +
    '</span></div>';
  const note = (/** @type {unknown} */ t) => '<div class="vz-tt-row vz-tt-label">' + esc(t) + '</div>';
  const SEP = '<div class="vz-tt-sep"></div>';
  /** @param {unknown} lo @param {unknown} hi */
  const range = (lo, hi) => lo == null || hi == null ? 'undefined (n < 2)' : num(lo) + ' – ' + num(hi);

  /**
   * Which drawn level, of those with a value in this row, the pointer is on.
   * @param {Element} cell
   * @param {Element | null} at
   * @param {string} sel
   * @param {number[]} drawn
   */
  function hitOf(cell, at, sel, drawn) {
    if (!at) return null;
    const k = Array.from(cell.querySelectorAll(sel)).indexOf(at);
    return k >= 0 && k < drawn.length ? drawn[k] : null;
  }

  /**
   * A distribution's lines: n, the centre, the body's range, the whiskers'.
   * @param {string} kind
   * @param {TableLaneGeom} g
   * @param {number} i
   * @param {Record<string, string>} words
   */
  function dist(kind, g, i, words) {
    const r = g.raw || {};
    const at = (/** @type {keyof TableLaneRaw} */ k) => (r[k] ? /** @type {TableNums} */ (r[k])[i] : null);
    let h = g.count && g.count[i] != null ? row('n', num(g.count[i])) : '';
    h += row(words.center || 'Center', num(at('center')));
    if (words.range) h += row(kind === 'box' ? 'Box (' + words.range + ')' : words.range, range(at('low'), at('high')));
    if (words.whisk) {
      h += row(kind === 'box' ? 'Whiskers (' + words.whisk + ')' : words.whisk,
               range(at('whiskerLow'), at('whiskerHigh')));
    }
    return h;
  }

  /**
   * The lines of a colour split: every level with a value, the hit one lit.
   * @param {TableCol} c
   * @param {TableMarkSplit} m
   * @param {number} i
   * @param {Element} cell
   * @param {Element} t
   * @param {Record<string, any>} tt
   */
  function splitLines(c, m, i, cell, t, tt) {
    const fill = t.closest('.vz-table-groups-fill');
    if (!fill) return '';
    const drawn = m.names.map((_, j) => j).filter((j) => (m.segValues[j][i] || 0) > 0);
    const hit = hitOf(cell, fill, '.vz-table-groups-fill', drawn);
    const cv = tt.cvar ? tt.cvar + ' ' : '';
    const lines = drawn.map((j) => row(cv + m.names[j], num(m.segValues[j][i]) + share(m.segValues[j][i], tt.den),
                                       m.fills[j], hit == null ? null : j === hit)).join('');
    const total = m.mode !== 'grouped' && c.disp[i]
      ? SEP + row(tt.meas || 'Value', c.disp[i] + (c.pct && c.pct[i] ? ' ' + c.pct[i] : '') +
                  (tt.den ? ' of ' + num(tt.den) : '')) : '';
    return lines ? lines + total : '';
  }

  /**
   * The lines of lanes split by colour: each level's centre (and range), the
   * hit level's n and whiskers under them.
   * @param {TableMarkLane} m
   * @param {number} i
   * @param {Element} cell
   * @param {Element} mark
   * @param {Record<string, any>} tt
   */
  function multiLines(m, i, cell, mark, tt) {
    const lv = m.lanes || [];
    const drawn = lv.map((_, k) => k).filter((k) => {
      const g = lv[k];
      return (m.kind === 'box' ? (g.median || [])[i] : (g.center || [])[i]) != null;
    });
    const hit = hitOf(cell, mark.closest('.vz-table-groups-lv'), '.vz-table-groups-lv', drawn);
    const w = tt.words || {};
    const at = (/** @type {TableLaneGeom} */ g, /** @type {keyof TableLaneRaw} */ k) =>
      (g.raw && g.raw[k] ? /** @type {TableNums} */ (g.raw[k])[i] : null);
    const cv = tt.cvar ? tt.cvar + ' ' : '';
    let h = drawn.map((k) => row(cv + (m.levels || [])[k], num(at(lv[k], 'center')) +
      (w.range ? ' (' + range(at(lv[k], 'low'), at(lv[k], 'high')) + ')' : ''),
      (m.fills || [])[k], hit == null ? null : k === hit)).join('');
    if (hit != null) {
      const g = lv[hit];
      const foot = [cv + (m.levels || [])[hit]];
      if (g.count && g.count[i] != null) foot.push('n ' + num(g.count[i]));
      if (w.whisk) foot.push((m.kind === 'box' ? 'Whiskers (' + w.whisk + ')' : w.whisk) + ' ' +
                             range(at(g, 'whiskerLow'), at(g, 'whiskerHigh')));
      h += SEP + note(foot.join(' · '));
    }
    return h;
  }

  /**
   * The lines of a pair: its tip's clauses; split by colour, each level's
   * two ends.
   * @param {TableMarkPair} m
   * @param {number} i
   * @param {Element} cell
   * @param {Element} mark
   * @param {Record<string, any>} tt
   */
  function pairLines(m, i, cell, mark, tt) {
    const clauses = (/** @type {unknown} */ tip, /** @type {number} */ skip) =>
      String(tip || '').split(' · ').slice(skip).filter(Boolean).map(note).join('');
    if (!m.multi) return clauses(m.tip ? m.tip[i] : '', 0);
    const lv = m.lanes || [];
    const drawn = lv.map((_, k) => k).filter((k) => lv[k].from[i] != null || lv[k].to[i] != null);
    const hit = hitOf(cell, mark.closest('.vz-table-groups-lv'), '.vz-table-groups-lv', drawn);
    const w = tt.words || {};
    const cv = tt.cvar ? tt.cvar + ' ' : '';
    const lines = drawn.map((k) => row(cv + (m.levels || [])[k], lv[k].fromTo ? lv[k].fromTo[i] : '',
                                       (m.fills || [])[k], hit == null ? null : k === hit)).join('');
    const rest = hit == null ? '' : clauses((lv[hit].tip || [])[i], 2);
    return (w.from && w.to ? note(w.from + ' → ' + w.to) : '') + lines + (rest ? SEP + rest : '');
  }

  /**
   * The card for a mark cell under the pointer, or "".
   * @param {TableModel} model
   * @param {number} k Column.
   * @param {number} i Row.
   * @param {Element} cell
   * @param {Element} t What the pointer is on.
   */
  function card(model, k, i, cell, t) {
    const c = model.cols[k];
    const m = c && c.mark;
    if (!m) return '';
    const tt = /** @type {Record<string, any>} */ (m.hover || {});
    const ctx = [tt.head, tt.sub].filter((x, j, a) => x && a.indexOf(x) === j);
    const label = model.stub != null ? model.cols[model.stub].disp[i] : '';
    const h = head(label) + (ctx.length ? note(ctx.join(' · ')) : '');
    let body = '';
    if (m.kind === 'bar') {
      body = t.closest('.vz-table-groups-fill') && c.disp[i]
        ? row(tt.meas || 'Value', c.disp[i] + (c.pct && c.pct[i] ? ' ' + c.pct[i] : '')) : '';
    } else if (m.kind === 'barsplit') {
      body = splitLines(c, m, i, cell, t, tt);
    } else if (m.kind === 'box' || m.kind === 'pointrange') {
      const mark = t.closest('.vz-table-lane-wh, .vz-table-lane-box, .vz-table-lane-med, .vz-table-lane-fence, .vz-table-lane-rng, .vz-table-lane-ctr');
      body = !mark ? '' : m.multi ? multiLines(m, i, cell, mark, tt)
        : m.raw && m.raw.center && m.raw.center[i] != null ? dist(m.kind, m, i, tt.words || {}) : '';
    } else if (m.kind === 'pair') {
      const mark = t.closest('.vz-table-lane-link, .vz-table-lane-from, .vz-table-lane-to');
      body = mark ? pairLines(m, i, cell, mark, tt) : '';
    }
    return body ? h + body : '';
  }

  /**
   * A swimlane's or sparkline's own text: the segment's, the point's under
   * the pointer, or the time under the pointer.
   * @param {Element} lane
   * @param {Element} t
   * @param {number} x Pointer x within the lane, 0 to 1.
   */
  function laneText(lane, t, x) {
    if (lane.classList.contains('vz-table-groups-ivcell')) {
      const seg = t.closest('.vz-table-lane-seg, .vz-table-lane-ev, .vz-table-lane-adm, .vz-table-lane-step');
      const strip = lane.getAttribute('data-strip');
      if (strip && !(seg && seg.matches('.vz-table-lane-ev'))) {
        return { text: lane.getAttribute('data-n') || '', strip: JSON.parse(strip), color: laneColor(lane) };
      }
      // The open end's arrow belongs to the lane's last segment. Off a
      // segment, the one at the pointer's time, or within a few pixels of it
      // (a thin lane is a few pixels high, a short bar a few pixels wide).
      const open = t.closest('.vz-table-lane-open');
      const w = lane.getBoundingClientRect().width;
      const hit = seg || (open ? lastSeg(lane) : segAt(lane, x, w > 0 ? 800 / w : 0.5));
      if (hit) return { text: hit.getAttribute('data-tip') || '', color: segColor(/** @type {HTMLElement} */ (hit)) };
      const d0 = parseFloat(lane.getAttribute('data-d0') || '');
      const d1 = parseFloat(lane.getAttribute('data-d1') || '');
      if (isNaN(d0) || isNaN(d1)) return null;
      const v = d0 + x * (d1 - d0);
      return { text: '~' + (lane.getAttribute('data-dd') === '1'
        ? new Date(Math.round(v) * 86400000).toISOString().slice(0, 10) : String(Math.round(v * 10) / 10)) };
    }
    const xs = (lane.getAttribute('data-xs') || '').split(',');
    const ys = (lane.getAttribute('data-ys') || '').split(',');
    if (!xs[0]) return null;
    const k = Math.round(x * (xs.length - 1));
    return { text: xs[k] + ' · ' + (ys[k] || '') };
  }

  /** @param {HTMLElement} el A segment, dot or step. */
  const segColor = (el) => el.style.background || el.style.borderColor ||
    el.style.getPropertyValue('--vz-seg') || undefined;

  /** @param {Element} lane The first segment's colour. */
  function laneColor(lane) {
    const el = /** @type {HTMLElement | null} */ (lane.querySelector('.vz-table-lane-seg, .vz-table-lane-adm'));
    return el ? segColor(el) : undefined;
  }

  /**
   * The segment of a lane nearest time `x` (0 to 1) within `tol` percent of
   * the lane, or null.
   * @param {Element} lane
   * @param {number} x
   * @param {number} tol
   */
  function segAt(lane, x, tol) {
    const p = x * 100;
    let best = null;
    let gap = Infinity;
    Array.from(lane.querySelectorAll('.vz-table-lane-seg')).forEach((el) => {
      const st = /** @type {HTMLElement} */ (el).style;
      const l = parseFloat(st.left);
      const r = l + parseFloat(st.width);
      const d = p < l ? l - p : p > r ? p - r : 0;
      if (d <= tol && d < gap) {
        best = el;
        gap = d;
      }
    });
    return best;
  }

  /**
   * The segment of a lane that ends last, or null.
   * @param {Element} lane
   */
  function lastSeg(lane) {
    let best = null;
    let end = -Infinity;
    Array.from(lane.querySelectorAll('.vz-table-lane-seg')).forEach((el) => {
      const st = /** @type {HTMLElement} */ (el).style;
      const r = parseFloat(st.left) + parseFloat(st.width);
      if (r >= end) {
        best = el;
        end = r;
      }
    });
    return best;
  }

  /**
   * A lane's hover strip: one box per record, a hollow one marked late and a
   * dashed one as skipped (marks.js seg()).
   * @param {{ text: string, color?: string, strip: (string | number)[][] }} l
   */
  function stripCard(l) {
    const late = l.strip.filter((b) => Number(b[1]) === 3).length;
    const skip = l.strip.filter((b) => Number(b[1]) === 2).length;
    const sub = [late ? late + ' late' : '', skip ? skip + ' skipped' : ''].filter(Boolean);
    return head(l.text, l.color) + (sub.length ? note(sub.join(' · ')) : '') +
      '<div class="vz-table-strip">' + l.strip.map((b) => {
        const k = Number(b[1]);
        return '<div class="vz-table-strip-box' + (k === 3 ? ' is-late' : k === 2 ? ' is-skip' : '') + '"' +
          (l.color && k !== 2 ? ' style="border-top-color:' + esc(l.color) + '"' : '') + '>' +
          String(b[0]).split('\n').map((line) => esc(line)).join('<br>') + '</div>';
      }).join('') + '</div>';
  }

  /** @param {{ text: string, color?: string, strip?: (string | number)[][] }} l */
  function laneCard(l) {
    if (l.strip) return stripCard(/** @type {any} */ (l));
    const parts = l.text.split(' · ');
    return head(parts[0], l.color) + parts.slice(1).map((p) => {
      const k = p.indexOf(': ');
      return k > 0 ? row(p.slice(0, k), p.slice(k + 2)) : note(p);
    }).join('');
  }

  /**
   * The card for what the pointer is on, or "".
   * @param {() => TableModel | null} model
   * @param {MouseEvent} e
   * @param {(label: string | null) => void} light
   */
  function cardAt(model, e, light) {
    const t = /** @type {Element} */ (e.target);
    const m = model();
    if (!m || !t || !t.closest) return '';
    const lane = t.closest('.vz-table-groups-ivcell, .vz-table-groups-spcell');
    if (lane) {
      const seg = t.closest('.vz-table-lane-seg');
      light(seg ? seg.getAttribute('data-l') : null);
      const r = lane.getBoundingClientRect();
      const l = laneText(lane, t, r.width ? Math.min(Math.max((e.clientX - r.left) / r.width, 0), 1) : 0);
      return l && (l.text || l.strip) ? laneCard(l) : '';
    }
    light(null);
    const cell = t.closest('td.vz-table-groups-bar-col, .vz-table-card-glyph');
    const rowEl = cell && cell.closest('[data-i]');
    if (!cell || !rowEl) return '';
    const k = cell.matches('.vz-table-card-glyph') ? Number(cell.getAttribute('data-k'))
      : /** @type {HTMLTableCellElement} */ (cell).cellIndex;
    return card(m, k, Number(rowEl.getAttribute('data-i')), cell, t);
  }

  /**
   * Show the card under the pointer over `box`.
   * @param {HTMLElement} box
   * @param {VizScope} scope
   * @param {() => TableModel | null} model
   */
  function wire(box, scope, model) {
    const tip = scope.node(root.document.body.appendChild(root.document.createElement('div')));
    tip.className = 'vz-table-lane-tip is-card';
    tip.hidden = true;
    /** @type {string | null} */
    let lit = null;
    const light = (/** @type {string | null} */ label) => {
      if (label === lit) return;
      lit = label;
      box.classList.toggle('seg-hover', !!label);
      box.querySelectorAll('.vz-table-lane-seg').forEach((s) =>
        s.classList.toggle('is-same', !!label && s.getAttribute('data-l') === label));
    };
    const hide = () => {
      tip.hidden = true;
      light(null);
    };
    scope.on(box, 'mousemove', (ev) => {
      const e = /** @type {MouseEvent} */ (ev);
      const html = cardAt(model, e, light);
      if (!html) {
        tip.hidden = true;
        return;
      }
      tip.innerHTML = html;
      tip.hidden = false;
      const w = root.innerWidth || 1000;
      const h = root.innerHeight || 800;
      tip.style.left = Math.min(e.clientX + 12, w - tip.offsetWidth - 8) + 'px';
      const top = e.clientY + 16;
      tip.style.top = (top + tip.offsetHeight > h - 8 ? Math.max(8, e.clientY - tip.offsetHeight - 12) : top) + 'px';
    });
    scope.on(box, 'mouseleave', hide);
    scope.on(box, 'scroll', hide, { passive: true });
  }

  V.table.tooltip = { wire, card };
})();
// --- table/sort.js
// @ts-check
/**
 * Sorting and nesting helpers of the table body (dom.js): sort keys from
 * the display texts, the parents a matching row keeps, and the tree sort
 * of an indented list.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /**
   * The first number in a display text: "1,234.5 kg" is 1234.5.
   * @param {unknown} s
   * @returns {number | null}
   */
  function parseNum(s) {
    if (s == null) return null;
    const m = String(s).match(/-?\d[\d,]*(\.\d+)?/);
    return m ? parseFloat(m[0].replace(/,/g, '')) : null;
  }

  /**
   * Sort keys of one column: numbers where both sides parse, else text.
   * @param {TableCol} col
   * @param {number} n
   */
  function sortKeys(col, n) {
    /** @type {(number | null)[]} */
    const num = new Array(n);
    /** @type {string[]} */
    const str = new Array(n);
    for (let i = 0; i < n; i++) {
      const k = col.key ? col.key[i] : col.disp[i];
      const s = k == null ? '' : String(k).trim();
      str[i] = s;
      num[i] = col.key && typeof k === 'number' ? k : parseNum(s);
    }
    return { num, str };
  }

  /**
   * The matching rows and the rows they sit under: an indented row's
   * parents stay with it, so a match keeps its context.
   * @param {number[]} view Rows in data order.
   * @param {number[]} indent
   */
  function withParents(view, indent) {
    const keep = new Set(view);
    for (const i of view) {
      let cur = indent[i];
      for (let j = i - 1; j >= 0 && cur > 0; j--) {
        if (indent[j] < cur) {
          keep.add(j);
          cur = indent[j];
        }
      }
    }
    return Array.from(keep).sort((a, b) => a - b);
  }

  /**
   * Sort an indented list as a tree: siblings by `cmp`, each row followed
   * by its own children, sorted the same way.
   * @param {number[]} rows Rows in data order.
   * @param {number[]} indent
   * @param {(a: number, b: number) => number} cmp
   * @returns {number[]}
   */
  function treeSort(rows, indent, cmp) {
    if (rows.length < 2) return rows.slice();
    let top = indent[rows[0]];
    for (const i of rows) if (indent[i] < top) top = indent[i];
    /** @type {number[][]} */
    const units = [];
    for (const i of rows) {
      if (indent[i] === top || !units.length) units.push([i]);
      else units[units.length - 1].push(i);
    }
    units.sort((u, v) => cmp(u[0], v[0]));
    return units.flatMap((u) => [u[0]].concat(treeSort(u.slice(1), indent, cmp)));
  }

  Object.assign(V.table, { parseNum, sort: { sortKeys, withParents, treeSort } });
})();
// --- table/dom.js
// @ts-check
/**
 * The table body every layout shares: a Grid draws a TableModel
 * (table.d.ts) into a scroll box, with header sort, search, folding
 * sections and indented groups, and windowed rows above VIRT_MIN items.
 *
 * The body is a list of items, a section header or a row, built from the
 * model: rows that pass the search, sorted within their section, under the
 * headers of the sections they are in, less what a folded section or
 * indented group hides. Only the window of that list around the scroll
 * position is in the DOM. Flat and structured tables take the same path; a
 * flat one has no sections.
 *
 * Clicks that are not the Grid's own (sort, fold) go to `hooks.click`,
 * which the drill sets (drill.js).
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const H = V.table.html;
  const esc = V.util.esc;
  const { sortKeys, withParents, treeSort } = V.table.sort;

  /** One collator for the sort: `localeCompare` makes a new one per call. */
  const collator = new Intl.Collator();
  /** Above this many items only a window of them is in the DOM. */
  const VIRT_MIN = 400;
  /** Items drawn beyond each edge of the visible part. */
  const OVERSCAN = 40;
  /** Row height until a drawn row is measured. */
  const ROW_H = 30;
  /** The class a lit row or section carries (drill.js). */
  const ACTIVE = 'vz-table-row-active';

  /**
   * A row's text as the search reads it: every cell, lower case.
   * @param {TableModel} model
   * @param {number} i
   */
  function rowText(model, i) {
    let t = '';
    for (const c of model.cols) {
      const d = c.disp[i];
      if (d != null) t += d + ' ';
    }
    return t.toLowerCase();
  }

  /**
   * What the item list needs from the model, worked out once per model:
   * each row's block (the rows from one section start to the next), each
   * block's section path, and which rows head an indented group.
   * @param {TableModel} model
   * @param {boolean} collapsible
   * @returns {TableStructure}
   */
  function structure(model, collapsible) {
    const n = model.n;
    const secs = model.sections || [];
    /** @type {number[]} */
    const blk = new Array(n).fill(-1);
    /** @type {Map<number, number[]>} */
    const path = new Map([[-1, []]]);
    const order = secs.map((s, k) => k)
      .sort((a, b) => secs[a].at - secs[b].at || secs[a].level - secs[b].level);
    /** @type {number[]} */
    let cur = [];
    for (const k of order) {
      cur = cur.slice(0, secs[k].level - 1);
      cur[secs[k].level - 1] = k;
      path.set(secs[k].at, cur.filter((x) => x != null));
    }
    let b = -1;
    for (let i = 0; i < n; i++) {
      if (path.has(i) && i >= 0) b = i;
      blk[i] = b;
    }
    const indent = (model.rows && model.rows.indent) || null;
    /** @type {boolean[]} */
    const toggle = new Array(n).fill(false);
    if (indent && collapsible && model.stub != null) {
      for (let i = 0; i + 1 < n; i++) toggle[i] = indent[i + 1] > indent[i];
    }
    let base = 0;
    if (indent && n) {
      base = indent[0];
      for (let i = 1; i < n; i++) if (indent[i] < base) base = indent[i];
    }
    let levels = 0;
    for (const x of secs) if (x.level > levels) levels = x.level;
    return { blk, path, indent, toggle, base, levels, folds: toggle.some(Boolean) };
  }

  /** @param {TableModel | null} m @returns {TableModel} */
  const m0 = (m) => /** @type {TableModel} */ (m);

  class Grid {
    /**
     * @param {HTMLElement} box The scroll box the table goes in.
     * @param {VizScope} scope
     * @param {TableGridHooks} [hooks]
     */
    constructor(box, scope, hooks) {
      this.box = box;
      this.scope = scope;
      /** The Grid's part of the box: the table and its no-match line. */
      this.host = scope.node(document.createElement('div'));
      this.host.className = 'vz-table-slot';
      box.appendChild(this.host);
      /** @type {TableGridHooks} */
      this.hooks = hooks || {};
      /** @type {HTMLTableElement | null} */
      this.table = null;
      /** @type {TableModel | null} */
      this.model = null;
      /** @type {TableGridFlags & { sortable: boolean, collapsible: boolean }} */
      this.flags = { sortable: true, collapsible: true };
      this.query = '';
      /** @type {number | null} */
      this.sortCol = null;
      this.sortDir = 0;
      /** Folded sections, by index, and folded indented groups, by row. */
      this.shut = new Set();
      this.shutRows = new Set();
      /** @type {TableItem[]} */
      this._items = [];
      /** Rows that pass the search. */
      this._shown = 0;
      /** @type {TableStructure | null} */
      this._st = null;
      /** @type {string[] | null} */
      this._text = null;
      /** @type {Map<number, ReturnType<typeof sortKeys>>} */
      this._keys = new Map();
      this.rowH = ROW_H;
      this._measured = false;
      this.win = [0, 0];
      let pending = false;
      scope.on(box, 'click', (e) => this._click(/** @type {MouseEvent} */ (e)));
      scope.on(box, 'keydown', (e) => this._key(/** @type {KeyboardEvent} */ (e)));
      // The box scrolls a table with a height limit, else the panel does: a
      // capture listener on the document sees both.
      scope.on(document, 'scroll', () => {
        if (pending || this._items.length <= VIRT_MIN) return;
        pending = true;
        scope.frame(() => {
          pending = false;
          this._scrolled();
        });
      }, { capture: true, passive: true });
    }

    /**
     * Draw a new model. Sort and folds start over, less the folds over what
     * the selection lights (a saved board opens on its selection); the
     * search stays.
     * @param {TableModel} model
     * @param {TableGridFlags} [flags]
     */
    set(model, flags) {
      this.model = model;
      this.flags = Object.assign({}, flags, { sortable: !flags || flags.sortable !== false,
                                              collapsible: !flags || flags.collapsible !== false });
      this.sortCol = null;
      this.sortDir = 0;
      this._st = structure(model, this.flags.collapsible);
      this._text = null;
      this._keys = new Map();
      this._measured = false;
      this.shut = new Set();
      this.shutRows = new Set();
      if (model.collapsed && this.flags.collapsible) this._shutAll(true);
      if (this.hooks.lit) {
        const lit = this.hooks.lit();
        this.reveal(lit.rows, lit.sections);
      }
      this._build();
      this.draw();
    }

    /** Draw nothing; the box shows something else (cards, a message). */
    clear() {
      this.model = null;
      this.table = null;
      this._items = [];
      this.host.innerHTML = '';
      this.host.hidden = true;
    }

    /** @param {string} q */
    search(q) {
      this.query = String(q || '').trim().toLowerCase();
      this.draw();
    }

    /**
     * @param {number | null} col
     * @param {number} dir 1 ascending, -1 descending, 0 data order.
     */
    sort(col, dir) {
      this.sortCol = dir === 0 ? null : col;
      this.sortDir = col == null ? 0 : dir;
      this._sortIcons();
      this.draw();
    }

    /** @param {number} s @param {boolean} shut */
    fold(s, shut) {
      if (shut) this.shut.add(s);
      else this.shut.delete(s);
      this.draw();
    }

    /** @param {boolean} shut */
    foldAll(shut) {
      this._shutAll(shut);
      this.draw();
    }

    /**
     * Open the sections and parent rows above these rows and section
     * headers, not their own folds; the next draw() shows them.
     * @param {Iterable<number>} rows
     * @param {Iterable<number>} [sections]
     */
    reveal(rows, sections = []) {
      const st = this._st;
      const secs = (this.model && this.model.sections) || [];
      if (!st) return;
      const lit = Array.from(rows);
      for (const i of lit) for (const s of st.path.get(st.blk[i]) || []) this.shut.delete(s);
      const own = new Set(lit);
      if (st.indent) withParents(lit, st.indent).forEach((j) => own.has(j) || this.shutRows.delete(j));
      for (const s of sections) {
        const p = secs[s] ? st.path.get(secs[s].at) || [] : [];
        p.slice(0, Math.max(p.indexOf(s), 0)).forEach((a) => this.shut.delete(a));
      }
    }

    /** Rows that pass the search, of all rows. */
    counts() {
      return { shown: this._shown, total: this.model ? this.model.n : 0 };
    }

    /** Whether the model has anything to fold. */
    folds() {
      const st = this._st;
      return !!this.model && this.flags.collapsible &&
        (!!(this.model.sections && this.model.sections.length) ||
         (!!st && st.toggle.some(Boolean)));
    }

    /** @param {boolean} shut */
    _shutAll(shut) {
      const m = this.model;
      const st = this._st;
      this.shut = new Set(shut && m && m.sections ? m.sections.map((s, k) => k) : []);
      this.shutRows = new Set();
      if (shut && st) st.toggle.forEach((t, i) => { if (t) this.shutRows.add(i); });
    }

    /** The table element and its header; the body comes from draw(). */
    _build() {
      const m = /** @type {TableModel} */ (this.model);
      const axis = this.hooks.axis;
      const f = { sortable: !!this.flags.sortable, foldAll: this.folds(), axis };
      const widths = m.widths || null;
      const cols = widths
        ? '<colgroup>' + widths.map((w) => '<col style="width:' + Number(w) + 'px">').join('') +
          '</colgroup>'
        : '';
      const head = m.head.map((row, r) =>
        '<tr>' + row.map((h, c) => H.th(m, r, c, f)).join('') + '</tr>').join('');
      this.host.hidden = false;
      const fl = this.flags;
      const attrs = Object.entries(fl.attrs || {})
        .map(([k, v]) => ' ' + k.replace(/[^a-z0-9-]/gi, '') + '="' + esc(v) + '"').join('');
      this.host.innerHTML = '<table class="blockr-table' + (widths ? ' vz-table-fixed' : '') +
        (fl.cls ? ' ' + esc(fl.cls) : '') + '"' + attrs + '>' + cols + '<thead>' + head +
        '</thead><tbody></tbody></table>';
      this.table = /** @type {HTMLTableElement} */ (this.host.firstElementChild);
    }

    /** The visible rows, sorted, under their section headers. */
    _list() {
      const m = /** @type {TableModel} */ (this.model);
      const st = /** @type {TableStructure} */ (this._st);
      /** @type {number[]} */
      let view = [];
      if (this.query) {
        if (!this._text) this._text = Array.from({ length: m.n }, (_, i) => rowText(m, i));
        const text = this._text;
        for (let i = 0; i < m.n; i++) if (text[i].includes(this.query)) view.push(i);
        this._shown = view.length;
        if (st.indent) view = withParents(view, st.indent);
      } else {
        view = Array.from({ length: m.n }, (_, i) => i);
      }
      if (!this.query) this._shown = view.length;
      if (this.sortCol != null && this.sortDir !== 0) view = this._sorted(view);
      return this._fold(view, st);
    }

    /** @param {number[]} view */
    _sorted(view) {
      const m = /** @type {TableModel} */ (this.model);
      const st = /** @type {TableStructure} */ (this._st);
      const c = /** @type {number} */ (this.sortCol);
      let k = this._keys.get(c);
      if (!k) {
        k = sortKeys(m.cols[c], m.n);
        this._keys.set(c, k);
      }
      const { num, str } = k;
      const dir = this.sortDir;
      /** @param {number} a @param {number} b */
      const cmp = (a, b) => {
        if (st.blk[a] !== st.blk[b]) return st.blk[a] - st.blk[b];
        const na = num[a];
        const nb = num[b];
        return dir * (na !== null && nb !== null ? na - nb : collator.compare(str[a], str[b]));
      };
      return st.indent ? treeSort(view, st.indent, cmp) : view.slice().sort(cmp);
    }

    /**
     * Section headers in front of each block, less what a fold hides.
     * @param {number[]} view
     * @param {TableStructure} st
     */
    _fold(view, st) {
      /** @type {TableItem[]} */
      const out = [];
      /** @type {number[]} */
      let prev = [];
      let prevBlk = NaN;
      /** @type {number | null} */
      let hideBelow = null;
      // Section titles over inner rows, while the rows keep their order.
      const titles = m0(this.model).rows && m0(this.model).rows.title;
      const sorted = this.sortCol != null && this.sortDir !== 0;
      for (const i of view) {
        const b = st.blk[i];
        const p = /** @type {number[]} */ (st.path.get(b));
        if (b !== prevBlk) {
          let d = 0;
          while (d < p.length && d < prev.length && p[d] === prev[d]) d++;
          for (let L = d; L < p.length; L++) {
            if (p.slice(0, L).some((s) => this.shut.has(s))) break;
            out.push({ s: p[L] });
          }
          prev = p;
          prevBlk = b;
          hideBelow = null;
        }
        if (p.some((s) => this.shut.has(s))) continue;
        const ind = st.indent ? st.indent[i] : 0;
        if (hideBelow != null && ind > hideBelow) continue;
        hideBelow = null;
        if (titles && titles[i] && !sorted) out.push({ t: i });
        out.push({ i });
        if (this.shutRows.has(i)) hideBelow = ind;
      }
      return out;
    }

    /** Rebuild the item list and draw the window. */
    draw() {
      if (!this.model || !this.table) return;
      this._items = this._list();
      this._window();
      this._foldAllButton();
      const empty = !!this.query && this._shown === 0;
      let msg = this.host.querySelector('.vz-table-search-empty');
      if (empty && !msg) {
        msg = document.createElement('div');
        msg.className = 'vz-table-search-empty';
        msg.setAttribute('role', 'status');
        msg.textContent = 'No matching rows';
        this.host.appendChild(msg);
      } else if (!empty && msg) {
        msg.remove();
      }
      if (this.hooks.drawn) this.hooks.drawn();
    }

    /** A new selection: the drawn rows and sections take or drop the lit
     * class, and nothing is drawn again. */
    relight() {
      if (!this.model || !this.table) return;
      const h = this.hooks;
      /** @param {Element} tr @param {string} cls */
      const lit = (tr, cls) => tr.classList.toggle(ACTIVE, cls.split(' ').includes(ACTIVE));
      const tbody = this.table.tBodies[0];
      tbody.querySelectorAll('tr[data-i]').forEach((tr) =>
        lit(tr, h.rowClass ? h.rowClass(Number(tr.getAttribute('data-i'))) : ''));
      tbody.querySelectorAll('tr[data-s]').forEach((tr) =>
        lit(tr, h.secClass ? h.secClass(Number(tr.getAttribute('data-s'))) : ''));
      if (h.drawn) h.drawn();
    }

    /** The part of the box in view, in px from its top. @returns {{ top: number, height: number }} */
    _view() {
      const b = this.box;
      return this.hooks.view ? this.hooks.view() : { top: b.scrollTop, height: b.clientHeight || 600 };
    }

    /** Draw the items around the scroll position, between two spacers. */
    _window() {
      const tbody = /** @type {HTMLTableElement} */ (this.table).tBodies[0];
      const items = this._items;
      const n = items.length;
      let start = 0;
      let end = n;
      if (n > VIRT_MIN && !this.flags.all) {
        const v = this._view();
        const vis = Math.max(1, Math.ceil(v.height / this.rowH));
        start = Math.max(0, Math.floor(v.top / this.rowH) - OVERSCAN);
        end = Math.min(n, start + vis + 2 * OVERSCAN);
      }
      const ncol = /** @type {TableModel} */ (this.model).cols.length || 1;
      const spacer = (/** @type {number} */ k) => '<tr class="vz-table-vspacer" aria-hidden="true">' +
        '<td colspan="' + ncol + '" style="padding:0;border:0;height:' +
        Math.round(k * this.rowH) + 'px"></td></tr>';
      /** @type {string[]} */
      const parts = [];
      if (start > 0) parts.push(spacer(start));
      for (let k = start; k < end; k++) parts.push(this._itemHtml(k));
      if (end < n) parts.push(spacer(n - end));
      tbody.innerHTML = parts.join('');
      this.win = [start, end];
      if (this.hooks.rows) this.hooks.rows(tbody);
      if (n > VIRT_MIN && !this._measured) {
        this._measured = true;
        const r0 = /** @type {HTMLElement | null} */ (tbody.querySelector('tr.vz-table-row'));
        if (r0 && r0.offsetHeight > 0 && Math.abs(r0.offsetHeight - this.rowH) > 0.5) {
          this.rowH = r0.offsetHeight;
          this._window();
        }
      }
    }

    /** Redraw the window once scrolling nears its edge. */
    _scrolled() {
      const n = this._items.length;
      if (!this.table || n <= VIRT_MIN) return;
      const v = this._view();
      const first = Math.floor(v.top / this.rowH);
      const last = first + Math.ceil(v.height / this.rowH);
      const [s, e] = this.win;
      if ((first - s < OVERSCAN / 2 && s > 0) || (e - last < OVERSCAN / 2 && e < n)) {
        this._window();
        if (this.hooks.windowed) this.hooks.windowed();
      }
    }

    /** @param {number} k Position in the item list. */
    _itemHtml(k) {
      const it = this._items[k];
      const m = /** @type {TableModel} */ (this.model);
      const h = this.hooks;
      if (it.s != null) {
        return H.section(m, it.s, { shut: this.shut.has(it.s), fold: this.flags.collapsible,
                                    extra: h.secClass ? h.secClass(it.s) : '' });
      }
      if (it.t != null) return H.title(m, /** @type {TableStructure} */ (this._st), it.t, this.flags.flush);
      const i = /** @type {number} */ (it.i);
      const next = this._items[k + 1];
      const last = !next || (next.s != null && m.sections != null &&
                             m.sections[next.s].level === 1);
      return H.row(m, /** @type {TableStructure} */ (this._st), i,
                   { last, shut: this.shutRows.has(i), extra: h.rowClass ? h.rowClass(i) : '',
                     head: h.head ? h.head(i) : undefined,
                     stubCls: this.flags.stubCls, flush: this.flags.flush, cell: h.cell });
    }

    /** The fold-all button follows the folds: open all, or fold all. */
    _foldAllButton() {
      const btn = this.host.querySelector('.vz-table-foldall');
      if (!btn) return;
      const open = this.shut.size === 0 && this.shutRows.size === 0;
      const act = open ? 'Collapse all groups' : 'Expand all groups';
      btn.setAttribute('aria-expanded', String(open));
      btn.setAttribute('aria-label', act);
      btn.setAttribute('data-blockr-tooltip', act);
    }

    _sortIcons() {
      if (!this.table) return;
      this.table.querySelectorAll('th.blockr-sortable').forEach((th) => {
        const on = this.sortCol != null && th.getAttribute('data-col-index') === String(this.sortCol);
        th.setAttribute('aria-sort', !on ? 'none' : this.sortDir === 1 ? 'ascending' : 'descending');
        const ic = th.querySelector('.blockr-sort-icon');
        if (!ic) return;
        ic.classList.toggle('blockr-sort-icon-asc', on && this.sortDir === 1);
        ic.classList.toggle('blockr-sort-icon-desc', on && this.sortDir === -1);
      });
    }

    /**
     * Next state of the sort cycle on a header: ascending, descending, off.
     * @param {Element} th
     */
    _cycle(th) {
      const col = parseInt(th.getAttribute('data-col-index') || '', 10);
      if (isNaN(col)) return;
      if (this.sortCol !== col) this.sort(col, 1);
      else this.sort(col, this.sortDir === 1 ? -1 : 0);
    }

    /**
     * The header cell an element is in, with its model entry.
     * @param {Element | null} el
     * @returns {{ th: Element, h: TableHeadCell } | null}
     */
    headAt(el) {
      const th = el && el.closest('th[data-h]');
      if (!th || !this.model) return null;
      const [r, c] = (th.getAttribute('data-h') || '').split(',').map(Number);
      const h = this.model.head[r] && this.model.head[r][c];
      return h ? { th, h } : null;
    }

    /** @param {MouseEvent} e */
    _click(e) {
      const t = /** @type {Element | null} */ (e.target);
      if (!t || !this.model || !t.closest) return;
      const drills = !!this.hooks.drills && this.hooks.drills();
      if (t.closest('.vz-table-foldall')) {
        this.foldAll(this.shut.size === 0 && this.shutRows.size === 0);
        return;
      }
      const ib = t.closest('.vz-table-indent-btn');
      if (ib) {
        const i = Number((/** @type {Element} */ (ib.closest('tr'))).getAttribute('data-i'));
        if (this.shutRows.has(i)) this.shutRows.delete(i);
        else this.shutRows.add(i);
        this.draw();
        return;
      }
      const sec = t.closest('tr.vz-table-section-header');
      if (sec && this.flags.collapsible) {
        const s = Number(sec.getAttribute('data-s'));
        const keyed = drills && !!(/** @type {TableSection[]} */ (this.model.sections)[s].keys);
        if (e.altKey) return this.foldAll(!this.shut.has(s));
        if (!(keyed && t.closest('.vz-table-section-value'))) return this.fold(s, !this.shut.has(s));
      }
      const hit = this.headAt(t);
      if (hit && hit.th.classList.contains('blockr-sortable') &&
          (!(drills && hit.h.keys) || t.closest('.blockr-sort-icon'))) {
        this._cycle(hit.th);
        return;
      }
      if (this.hooks.click) this.hooks.click(e);
    }

    /** @param {KeyboardEvent} e */
    _key(e) {
      if (e.key !== 'Enter' && e.key !== ' ') return;
      const t = /** @type {Element | null} */ (e.target);
      const th = t && t.closest && t.closest('th.blockr-sortable');
      if (!th) return;
      e.preventDefault();
      this._cycle(th);
    }
  }

  Object.assign(V.table, { Grid, rowText });
})();
// --- table/drill.js
// @ts-check
/**
 * The drill on a Grid: a click on a row, a cell, a keyed header or a keyed
 * section value sends a selection (`TableSelection`, table.d.ts) to R as
 * `<id>_select`, and the rows, sections and columns the current selection
 * claims are lit.
 *
 * What a click claims comes from the model: a row's `rows.keys`, or in a
 * flat table the row's raw value in the `drill` column; a cell adds its
 * column's `keys`; a header claims its `keys`; a section its `keys`. A
 * second click on what is already selected clears it. With a drill target
 * set (transient) every click is sent, the clicked part flashes and nothing
 * stays lit.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  const FLASH = ['vz-table-flash-row', 'vz-table-flash-col', 'vz-table-flash-colhead', 'vz-table-flash-cross'];
  const HOT = ['vz-table-hot-row', 'vz-table-hot-col', 'vz-table-hot-cell', 'vz-table-hot-soft'];

  /** @param {(string | null)[] | undefined} a @param {(string | null)[] | undefined} b */
  const sameValues = (a, b) => !!a && !!b && a.length === b.length &&
    a.every((v, k) => (v == null ? null : String(v)) === (b[k] == null ? null : String(b[k])));

  /**
   * Same columns, same values.
   * @param {TableClaim | null | undefined} a
   * @param {TableClaim | null | undefined} b
   */
  function equal(a, b) {
    const ka = Object.keys(a || {});
    const kb = Object.keys(b || {});
    return ka.length === kb.length &&
      ka.every((k) => sameValues((a || {})[k], (b || {})[k]));
  }

  /** Whether every value of `a` is among `b`'s. */
  const within = (/** @type {(string | null)[] | undefined} */ a,
                  /** @type {(string | null)[] | undefined} */ b) => {
    if (!a || !b || !a.length) return false;
    const has = new Set(b.map((v) => (v == null ? null : String(v))));
    return a.every((v) => has.has(v == null ? null : String(v)));
  };

  /**
   * Whether `keys` lies inside `claim`: every column of the claim is in the
   * keys, its values among theirs. A spanner's claim covers its leaves; a
   * colour split's segment (one level) covers its column (every level).
   * @param {TableClaim} claim
   * @param {TableClaim | null | undefined} keys
   */
  function covers(claim, keys) {
    const k = Object.keys(claim);
    return !!keys && k.length > 0 && k.every((c) => within(claim[c], keys[c]));
  }

  /** @param {TableSelection | null} a @param {TableSelection | null} b */
  function same(a, b) {
    if (!a || !b) return a === b;
    return equal(a.filters, b.filters) && equal(a.columns, b.columns);
  }

  /**
   * "Arm = Placebo, Sex = F": a selection in words, with column labels.
   * @param {TableSelection | null} sel
   * @param {Record<string, string>} labels
   */
  function label(sel, labels) {
    if (!sel) return '';
    const vals = (/** @type {(string | null)[]} */ v) =>
      v.map((x) => x == null ? '(Missing)' : x).join(', ');
    /** @param {TableClaim | undefined} c */
    const words = (c) => Object.keys(c || {}).map((k) => {
      const v = /** @type {TableClaim} */ (c)[k];
      // Annotation columns of a structured table: a group level reads as
      // its value, a Table 1 level as "<variable> = <level>".
      if (/^\.group\d+_level$/.test(k)) return vals(v);
      if (k === '.variable_level') {
        const name = String(((c || {})['.variable'] || [''])[0]);
        return (labels[name] || name) + ' = ' + vals(v);
      }
      return k.startsWith('.') ? '' : (labels[k] || k) + ' = ' + vals(v);
    }).filter(Boolean);
    return words(sel.filters).concat(words(sel.columns)).join(', ');
  }

  /**
   * What row `i` claims, or null.
   * @param {TableModel} m
   * @param {number} i
   * @returns {TableClaim | null}
   */
  function rowClaim(m, i) {
    if (m.rows && m.rows.keys) return m.rows.keys[i] || null;
    if (!m.drill) return null;
    const col = m.cols.find((c) => c.name === m.drill);
    const raw = col && col.raw ? col.raw[i] : null;
    return raw == null ? null : { [m.drill]: [raw] };
  }

  /** @param {TableModel | null} m */
  function clickable(m) {
    if (!m) return false;
    return !!(m.rows && m.rows.keys) || !!m.drill || m.cols.some((c) => !!c.keys) ||
      m.head.some((r) => r.some((h) => !!h.keys)) ||
      !!(m.sections && m.sections.some((x) => !!x.keys));
  }

  /** @param {TableClaim} filters @param {TableClaim | null | undefined} [columns] */
  const selection = (filters, columns) => /** @type {TableSelection} */ (
    Object.assign({ type: 'categorical', filters }, columns ? { columns } : {}));

  /**
   * The rows a row selection lights: rows with the same claim, or in a flat
   * table every row with the selected value in the drill column.
   * @param {TableModel} m
   * @param {TableClaim} f
   */
  function litRows(m, f) {
    /** @type {Set<number>} */
    const set = new Set();
    if (m.rows && m.rows.keys) {
      m.rows.keys.forEach((k, i) => { if (k && equal(k, f)) set.add(i); });
    } else if (m.drill && f[m.drill]) {
      const want = new Set(f[m.drill].map(String));
      const col = m.cols.find((c) => c.name === m.drill);
      ((col && col.raw) || []).forEach((v, i) => {
        if (v != null && want.has(String(v))) set.add(i);
      });
    }
    return set;
  }

  /**
   * The header cells, and the drawn body cells, a column claim covers.
   * @param {TableGrid} grid
   * @param {TableClaim | null | undefined} claim
   */
  function colEls(grid, claim) {
    /** @type {{ head: Element[], cells: Element[] }} */
    const out = { head: [], cells: [] };
    const m = grid.model;
    const table = grid.table;
    if (!claim || !m || !table || !Object.keys(claim).length) return out;
    const idx = m.cols.map((c, j) => covers(claim, c.keys) ? j : -1).filter((j) => j >= 0);
    table.querySelectorAll('th[data-h]').forEach((th) => {
      const hit = grid.headAt(th);
      if (hit && covers(claim, hit.h.keys)) out.head.push(th);
    });
    table.querySelectorAll('tbody tr.vz-table-row').forEach((tr) => {
      idx.forEach((j) => { if (tr.children[j]) out.cells.push(tr.children[j]); });
    });
    return out;
  }

  /**
   * Light what a transient click sent, until its animation ends.
   * @param {HTMLElement} box
   * @param {VizScope} scope
   * @param {Array<{ el: Element, cls: string }>} parts
   */
  function flash(box, scope, parts) {
    box.querySelectorAll('.' + FLASH.join(', .')).forEach((el) => el.classList.remove(...FLASH));
    for (const p of parts) {
      p.el.classList.add(p.cls);
      const off = scope.on(p.el, 'animationend', () => {
        p.el.classList.remove(...FLASH);
        off();
      });
    }
  }

  /**
   * Preview what a click on `at` would claim: a keyed header its columns, a
   * row its row, a keyed cell its row and column faintly and itself more.
   * @param {TableGrid} grid
   * @param {Element} at
   */
  function hover(grid, at) {
    const m = /** @type {TableModel} */ (grid.model);
    const hot = (/** @type {Element} */ el, /** @type {string[]} */ cls) => el.classList.add(...cls);
    if (at.matches('.vz-table-section-value')) {
      const sh = at.closest('tr.vz-table-section-header');
      if (sh && sh.classList.contains('vz-table-keyed')) hot(sh, ['vz-table-hot-row']);
      return;
    }
    const hit = grid.headAt(at);
    if (hit) {
      const ce = colEls(grid, hit.h.keys);
      ce.head.concat(ce.cells).forEach((c) => hot(c, ['vz-table-hot-col']));
      return;
    }
    const tr = at.closest('tr.vz-table-row');
    if (!tr || tr.classList.contains('vz-table-row-nodrill')) return;
    const j = /** @type {HTMLTableCellElement} */ (at).cellIndex;
    const keys = j !== m.stub && m.cols[j] && !tr.classList.contains('vz-table-heading') ? m.cols[j].keys : null;
    if (!keys) return hot(tr, ['vz-table-hot-row']);
    const ce = colEls(grid, keys);
    hot(tr, ['vz-table-hot-row', 'vz-table-hot-soft']);
    ce.head.concat(ce.cells).forEach((c) => { if (c !== at) hot(c, ['vz-table-hot-col', 'vz-table-hot-soft']); });
    hot(at, ['vz-table-hot-cell']);
  }

  /**
   * Hover classes on the grid, not :hover, since a column is not one element.
   * @param {TableGrid} grid
   * @param {VizScope} scope
   */
  function wireHover(grid, scope) {
    /** @type {Element | null} */
    let hotAt = null;
    const clear = () => grid.box.querySelectorAll('.' + HOT.join(', .'))
      .forEach((el) => el.classList.remove(...HOT));
    scope.on(grid.box, 'mouseover', (e) => {
      const t = /** @type {Element | null} */ (e.target);
      const at = t && t.closest ? (t.closest('.vz-table-section-value') || t.closest('td, th')) : null;
      if (at === hotAt) return;
      hotAt = at;
      clear();
      if (at && clickable(grid.model)) hover(grid, at);
    });
    scope.on(grid.box, 'mouseleave', () => {
      hotAt = null;
      clear();
    });
  }

  /**
   * What a click on `t` claims, and the row and cell it hit.
   * @param {TableGrid} grid
   * @param {Element} t
   * @returns {{ sel: TableSelection, tr?: Element, cell?: Element | null } | null}
   */
  function target(grid, t) {
    const m = /** @type {TableModel} */ (grid.model);
    const hit = grid.headAt(t);
    if (hit) {
      return hit.h.keys && !t.closest('.blockr-sort-icon') ? { sel: selection({}, hit.h.keys) } : null;
    }
    const sec = t.closest('tr.vz-table-section-header');
    if (sec) {
      const s = m.sections ? m.sections[Number(sec.getAttribute('data-s'))] : null;
      return s && s.keys && t.closest('.vz-table-section-value') ? { sel: selection(s.keys), tr: sec } : null;
    }
    const tr = t.closest('tr.vz-table-row');
    const claim = tr ? rowClaim(m, Number(tr.getAttribute('data-i'))) : null;
    if (!tr || !claim) return null;
    const td = /** @type {HTMLTableCellElement | null} */ (t.closest('td'));
    const col = td && td.cellIndex !== m.stub ? m.cols[td.cellIndex] : null;
    return { sel: selection(claim, col && segmentKeys(col, t)), tr, cell: td };
  }

  /**
   * A column's claim, narrowed to one level on a colour split's segment:
   * the column claims every level, the segment its own.
   * @param {TableCol} col
   * @param {Element} t
   * @returns {TableClaim | null | undefined}
   */
  function segmentKeys(col, t) {
    const m = col.mark;
    const seg = t.closest('[data-seg]');
    if (!col.keys || !seg || !m || m.kind !== 'barsplit') return col.keys;
    const level = m.names[Number(seg.getAttribute('data-seg'))];
    const all = JSON.stringify(m.names.slice().sort());
    /** @type {TableClaim} */
    const out = {};
    for (const [k, v] of Object.entries(col.keys)) {
      out[k] = level != null && JSON.stringify(v.slice().sort()) === all ? [level] : v;
    }
    return out;
  }

  class Drill {
    /**
     * @param {TableGrid} grid
     * @param {TableFrame} frame
     * @param {VizScope} scope
     * @param {TableDrillContext} ctx
     */
    constructor(grid, frame, scope, ctx) {
      this.grid = grid;
      this.frame = frame;
      this.scope = scope;
      this.ctx = ctx;
      /** Lit rows, worked out once per selection and model. */
      this.lit = { sel: /** @type {TableSelection | null} */ (null),
                   m: /** @type {TableModel | null} */ (null), rows: new Set() };
      wireHover(grid, scope);
      Object.assign(grid.hooks, {
        rowClass: (/** @type {number} */ i) => this.rowClass(i),
        secClass: (/** @type {number} */ s) => this.secClass(s),
        click: (/** @type {MouseEvent} */ e) => this.click(e),
        windowed: () => this.paint(),
        lit: () => ({ rows: this.rows(), sections: this.sections() }),
        drills: () => clickable(grid.model)
      });
    }

    rows() {
      const sel = this.ctx.selection();
      const m = this.ctx.model();
      if (this.lit.sel !== sel || this.lit.m !== m) {
        const f = sel && !this.ctx.transient() ? sel.filters : null;
        this.lit = { sel, m, rows: m && f && Object.keys(f).length ? litRows(m, f) : new Set() };
      }
      return this.lit.rows;
    }

    /** @param {number} i */
    rowClass(i) {
      const m = this.ctx.model();
      if (!m || !clickable(m)) return '';
      if (!rowClaim(m, i)) return 'vz-table-row-nodrill';
      return this.rows().has(i) ? 'vz-table-row-active' : '';
    }

    /** @param {number} s */
    secClass(s) {
      return this.secLit(s) ? 'vz-table-row-active' : '';
    }

    /** @param {number} s Whether section `s` is the selection. */
    secLit(s) {
      const sel = this.ctx.selection();
      const m = this.grid.model;
      const sec = m && m.sections ? m.sections[s] : null;
      return !!(sel && !this.ctx.transient() && sec && sec.keys && !sel.columns &&
        equal(sec.keys, sel.filters));
    }

    /** The section headers the selection lights. */
    sections() {
      const m = this.grid.model;
      const n = m && m.sections ? m.sections.length : 0;
      return Array.from({ length: n }, (_, s) => s).filter((s) => this.secLit(s));
    }

    /** Light the claimed columns; runs after every draw. */
    paint() {
      const table = this.grid.table;
      if (!table) return;
      table.querySelectorAll('.vz-table-col-active').forEach((el) => el.classList.remove('vz-table-col-active'));
      const sel = this.ctx.selection();
      if (!sel || this.ctx.transient()) return;
      const e = colEls(this.grid, sel.columns);
      e.head.concat(e.cells).forEach((el) => el.classList.add('vz-table-col-active'));
    }

    /**
     * Send a click's claim. Kept, it toggles: the same claim again clears.
     * @param {TableSelection} sel
     * @param {{ tr?: Element | null, cell?: Element | null }} at
     */
    choose(sel, at) {
      const ctx = this.ctx;
      if (ctx.transient()) {
        V.util.select(this.frame.id, /** @type {any} */ (sel));
        ctx.sent(sel);
        /** @type {Array<{ el: Element, cls: string }>} */
        const parts = at.tr ? [{ el: at.tr, cls: 'vz-table-flash-row' }] : [];
        const e = colEls(this.grid, sel.columns);
        e.head.forEach((el) => parts.push({ el, cls: 'vz-table-flash-colhead' }));
        e.cells.forEach((el) => parts.push({ el, cls: el === at.cell ? 'vz-table-flash-cross' : 'vz-table-flash-col' }));
        flash(this.frame.el, this.scope, parts);
        return;
      }
      const next = same(sel, ctx.selection()) ? null : sel;
      V.util.select(this.frame.id, /** @type {any} */ (next));
      ctx.select(next);
    }

    /** @param {MouseEvent} e */
    click(e) {
      const t = /** @type {Element} */ (e.target);
      if (!clickable(this.grid.model)) return;
      const hit = target(this.grid, t);
      if (hit) this.choose(hit.sel, hit);
    }

    /** @param {number} i @param {Element} el */
    clickRow(i, el) {
      const m = this.ctx.model();
      const claim = m ? rowClaim(m, i) : null;
      if (claim) this.choose(selection(claim), { tr: el });
    }
  }

  /**
   * @param {TableGrid} grid
   * @param {TableFrame} frame
   * @param {VizScope} scope
   * @param {TableDrillContext} ctx
   * @returns {TableDrill}
   */
  const wire = (grid, frame, scope, ctx) => new Drill(grid, frame, scope, ctx);

  V.table.drill = { same, covers, equal, label, wire, clickable };
})();
// --- table/chrome.js
// @ts-check
/**
 * The table block's frame, the same for every layout: the block frame
 * (core/frame.js) with a search box among the tools and the scroll box the
 * layout draws into. The search and download stay hidden until there is a
 * table to search or download. In a narrow block the tools take their own
 * row (table-view.css).
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /** Wait this long after the last key before searching. */
  const SEARCH_MS = 150;

  /** Can table.css move the header with a scroll timeline? */
  const FOLLOW = typeof CSS !== 'undefined' && typeof CSS.supports === 'function' &&
    CSS.supports('animation-timeline', '--a');

  /**
   * @param {string} tag
   * @param {string} cls
   * @param {HTMLElement} [parent]
   */
  function el(tag, cls, parent) {
    const e = document.createElement(tag);
    e.className = cls;
    if (parent) parent.appendChild(e);
    return e;
  }

  class Frame extends V.Frame {
    /**
     * @param {HTMLElement} host The output element; the frame fills it.
     * @param {VizScope} scope
     */
    constructor(host, scope) {
      super(host, scope, { cls: 'vz-table' });
      this.searchInput = /** @type {HTMLInputElement} */ (el('input', 'vz-table-search'));
      this.searchInput.type = 'search';
      this.searchInput.placeholder = 'Search…';
      this.searchInput.setAttribute('aria-label', 'Search table');
      this.tools.insertBefore(this.searchInput, this.downloadSlot);
      this.box = el('div', 'blockr-table-wrapper', this.body);
      this.box.style.overflow = 'auto';
      /** @type {Array<(q: string) => void>} */
      this._search = [];
      const run = scope.debounce(() => this._search.forEach((f) => f(this.query())), SEARCH_MS);
      scope.on(this.searchInput, 'input', run);
      // Where the panel around the frame was scrolled to, and the frame's
      // height, as the browser reports them: a layout switch reads neither
      // off the page, which would restyle the old layout's rows.
      /** @type {{ sc: Element, top: number } | null} */
      this._scrolled = null;
      scope.on(document, 'scroll', (e) => {
        const t = e.target === document ? document.scrollingElement : /** @type {Element} */ (e.target);
        if (t && t !== this.box && t.contains(this.el)) this._scrolled = { sc: t, top: t.scrollTop };
      }, { capture: true, passive: true });
      this.height = 0;
      scope.resize(this.el, (entries) => {
        const r = entries[entries.length - 1];
        const b = r.borderBoxSize && r.borderBoxSize[0];
        this.height = b ? b.blockSize : r.contentRect.height;
      });
      this.showTools({ search: false, download: false });
    }

    /** The search text, trimmed and lower case. */
    query() {
      return this.searchInput.hidden ? '' : this.searchInput.value.trim().toLowerCase();
    }

    /**
     * @param {(q: string) => void} fn Called after a pause in typing.
     * @returns {() => void} Stops calling it; a layout's scope owns it, so a
     *   layout switch lets go of the old view.
     */
    onSearch(fn) {
      this._search.push(fn);
      return () => {
        const k = this._search.indexOf(fn);
        if (k >= 0) this._search.splice(k, 1);
      };
    }

    /**
     * Search and download, each only when on and there is a table. `onOpen`
     * runs when the reader reaches for the download.
     * @param {{ search: boolean, download: boolean, downloads?: VizDownload[] | null,
     *           onOpen?: () => void }} o
     */
    showTools(o) {
      const was = !this.searchInput.hidden;
      this.searchInput.hidden = !o.search;
      if (was && !o.search && this.searchInput.value) {
        this.searchInput.value = '';
        this._search.forEach((f) => f(''));
      }
      this.downloads(o.download ? o.downloads : null, o.onOpen);
    }

    /**
     * The panel's scroll as its last scroll event left it, while the frame
     * is still in that panel; null when it has not scrolled.
     * @returns {{ sc: Element, top: number } | null}
     */
    scrollPlace() {
      const s = this._scrolled;
      return s && s.sc.isConnected && s.sc.contains(this.el) ? s : null;
    }

    /**
     * The nearest ancestor that scrolls up and down: a dock panel, the page.
     * One that only scrolls sideways computes overflow-y to auto as well, so
     * the style alone is not enough.
     * @returns {Element | null}
     */
    scroller() {
      for (let n = this.el.parentElement; n; n = n.parentElement) {
        const oy = getComputedStyle(n).overflowY;
        if ((oy === 'auto' || oy === 'scroll') && n.scrollHeight > n.clientHeight) return n;
      }
      return null;
    }

    /**
     * Keeps the header in view while the rows scroll. A box with a height
     * limit (interactive_table()'s `max_height`, the data table under a
     * view) scrolls itself, and table.css makes the header sticky in it.
     * Otherwise the table runs its full length and the panel around it
     * scrolls; the box still scrolls sideways, so a sticky header would
     * stick to the box and scroll away with the panel. Here the header moves
     * down by as much of the table as has scrolled out of view, all its rows
     * together, and stops at the last row. A scroll-driven animation in
     * table.css moves it in step with the scroll; this gives it the
     * table's measures, which change on a draw or a resize, not on a scroll.
     * A browser without scroll timelines gets the move from here, a frame
     * or more behind the scroll.
     */
    followHeader() {
      const box = this.box;
      const thead = /** @type {HTMLElement | null} */ (box.querySelector('table.blockr-table > thead'));
      const table = thead && thead.parentElement;
      if (!thead || !table) return;
      if (this.boxScrolls()) {
        box.classList.remove('vz-follow');
        thead.style.transform = '';
        box.classList.toggle('scrolled', box.scrollTop > 0);
        return;
      }
      const sp = this.scroller();
      const top = sp ? sp.getBoundingClientRect().top : 0;
      // offsetHeight, not a rect: a rect would include the transform.
      const max = table.offsetHeight - thead.offsetHeight;
      const tableTop = table.getBoundingClientRect().top;
      const y = Math.max(0, Math.min(top - tableTop, max));
      box.classList.toggle('scrolled', y > 0);
      if (FOLLOW) {
        const at = Math.round(tableTop - box.getBoundingClientRect().top) + 'px';
        const mx = Math.max(0, max) + 'px';
        const st = box.style;
        if (st.getPropertyValue('--vz-follow-at') !== at) st.setProperty('--vz-follow-at', at);
        if (st.getPropertyValue('--vz-follow-max') !== mx) st.setProperty('--vz-follow-max', mx);
        box.classList.add('vz-follow');
        return;
      }
      thead.style.transform = y > 0 ? 'translateY(' + y + 'px)' : '';
    }

    /** Does the box scroll up and down itself (a height limit)? */
    boxScrolls() {
      const b = this.box;
      return b.scrollTop > 0 || b.scrollHeight > b.clientHeight + 1;
    }

    /**
     * The part of the box in view, in px from its top, for the row window
     * (dom.js): the box's own scroll, or the part of the panel around it
     * (or of the page) that the box covers. 0.2 windowed rows only inside
     * the table block's own box; here the panel scrolls most tables.
     * @returns {{ top: number, height: number }}
     */
    inView() {
      const b = this.box;
      if (this.boxScrolls()) return { top: b.scrollTop, height: b.clientHeight || 600 };
      const sc = this.scroller();
      const r = sc ? sc.getBoundingClientRect() : { top: 0, height: root.innerHeight || 0 };
      return { top: Math.max(0, r.top - b.getBoundingClientRect().top), height: r.height || 600 };
    }

    /** @param {boolean} on A table with sections or keyed rows. */
    structured(on) {
      this.el.classList.toggle('vz-table-structured', on);
    }
  }

  V.table.Frame = Frame;
})();
// --- table/cards.js
// @ts-check
/**
 * The cards look: a TableCards model (table.d.ts) drawn as one row of cards
 * per table row, one card per column (records) or per summary (groups).
 * The layout calls `render()` with the rows to draw, in order, and the
 * classes of each row, so search, the drill and its highlight work on card
 * rows as on table rows. Groups passes `glyph` to draw its mark in a card.
 *
 * The grid follows the host's width:
 *   wide    a row head column, then one column per card, with column heads
 *   medium  each row on its own line, the row head above its cards
 *   narrow  one card per line, no legend
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const esc = V.util.esc;

  const NARROW = 320;
  const CARD_MIN = 150;
  const HEAD_W = 120;

  /** @param {TableCardCol['legend']} items */
  function legendHtml(items) {
    if (!items || !items.length) return '';
    return '<div class="vz-table-card-legend">' + items.map((it) =>
      '<span><i style="background:' + esc(it.color) + '"></i>' + esc(it.label) + '</span>')
      .join('') + '</div>';
  }

  /**
   * @param {TableCards} cards
   * @param {number} k Column.
   * @param {number} i Row.
   * @param {TableCardOpts['glyph']} glyph
   */
  function cardHtml(cards, k, i, glyph) {
    const c = cards.cols[k];
    const main = c.main ? c.main[i] : null;
    const sub = c.sub ? c.sub[i] : null;
    const g = c.glyph != null && glyph ? glyph(c.glyph, i) : '';
    let out = '<div class="vz-table-card' + (c.text ? ' is-text' : '') + '">' +
      '<div class="vz-table-card-lab" title="' + esc(c.label) + '">' + esc(c.label) + '</div>' +
      '<div class="vz-table-card-val">' + (main == null || main === ''
        ? '<span class="vz-table-card-na">—</span>' : esc(main)) + '</div>';
    if (c.sub) {
      // A single row's card names its statistic; a column of cards names it
      // once in its head.
      out += '<div class="vz-table-card-sub">' + (sub && c.word
        ? '<span class="vz-table-card-word">' + esc(c.word) + ': </span>' : '') + esc(sub || '') + '</div>';
    }
    if (g) out += '<div class="vz-table-card-glyph" data-k="' + c.glyph + '">' + g + '</div>';
    if (!cards.multi) out += legendHtml(c.legend);
    return out + '</div>';
  }

  /** @param {TableCards} cards */
  function headHtml(cards) {
    return '<div class="vz-table-card-corner"></div>' + cards.cols.map((c) => {
      const note = [c.word, c.range].filter(Boolean).join(' · ');
      return '<div class="vz-table-card-colhead"><b title="' + esc(c.label) + '">' + esc(c.label) +
        '</b>' + (note ? '<span>' + esc(note) + '</span>' : '') + legendHtml(c.legend) + '</div>';
    }).join('');
  }

  /**
   * @param {TableCards} cards
   * @param {number} i
   * @param {TableCardOpts} opts
   */
  function rowHtml(cards, i, opts) {
    const cls = opts.rowClass ? opts.rowClass(i) : '';
    let out = '<div class="vz-table-card-row' + (cls ? ' ' + cls : '') + '" data-i="' + i + '">';
    if (cards.multi) {
      const hex = cards.hex ? cards.hex[i] : null;
      const strip = !cards.hex ? ''
        : '<i class="vz-table-card-strip" style="' + (hex ? 'background:' + esc(hex) : 'visibility:hidden') +
          '"></i>';
      out += '<div class="vz-table-card-rowhead">' + strip + '<span class="vz-table-card-rowlab">' +
        esc(cards.label ? cards.label[i] : '') + '</span></div>';
    }
    for (let k = 0; k < cards.cols.length; k++) out += cardHtml(cards, k, i, opts.glyph);
    return out + '</div>';
  }

  /**
   * The layout mode for the host's width.
   * @param {HTMLElement} host
   * @param {TableCards} cards
   */
  function fit(host, cards) {
    const grid = host.querySelector('.vz-table-card-grid');
    if (!grid) return;
    const w = host.clientWidth || 0;
    const mode = w > 0 && w < NARROW ? 'narrow'
      : cards.multi && w > 0 && w < HEAD_W + cards.cols.length * CARD_MIN ? 'medium' : 'wide';
    if (grid.getAttribute('data-cards-mode') !== mode) grid.setAttribute('data-cards-mode', mode);
  }

  /** Hosts watched for width, with the model each shows. */
  const watched = new WeakMap();

  /**
   * Draw `cards` into `host`.
   * @param {HTMLElement} host
   * @param {TableCards} cards
   * @param {VizScope} scope Owns the width watch.
   * @param {TableCardOpts} [opts]
   */
  function render(host, cards, scope, opts) {
    const o = opts || {};
    const order = o.order || Array.from({ length: cards.n || 0 }, (_, i) => i);
    let out;
    if (!cards.cols.length) {
      out = '<div class="vz-table-card-empty">No summary to show as a card.</div>';
    } else {
      out = '<div class="vz-table-card-grid' + (cards.multi ? ' is-multi' : '') + '" style="--vz-table-card-k:' +
        cards.cols.length + '">' + (cards.multi ? headHtml(cards) : '') +
        order.map((i) => rowHtml(cards, i, o)).join('') + '</div>' +
        (cards.fold ? '<div class="vz-table-card-fold">' + esc(cards.fold) + '</div>' : '');
    }
    host.innerHTML = out;
    const w = watched.get(host);
    if (w && !w.scope.disposed) {
      w.cards = cards;
    } else {
      const entry = { scope, cards };
      watched.set(host, entry);
      scope.resize(host, () => fit(host, entry.cards));
    }
    fit(host, cards);
  }

  V.table.cards = { render };
})();
// --- table/gear.js
// @ts-check
/**
 * The table's gear: the core gear engine (`Blockr.viz.gear`, core/gear/)
 * in a tray under the frame's header, opened by the gear button with
 * `Blockr.viz.gear.tray`. The engine builds every row from the spec;
 * this file adds what the spec cannot say:
 *
 * - which rows a layout shows for its input (`visible`): a finished input
 *   picks no columns, the shadings and row colour need a flat table,
 *   folding needs sections, `missing` needs nesting;
 * - the `by` label, which differs per layout;
 * - the editors of keys the engine has no control for (`custom`): the
 *   shading rules here, a layout's own in its file (`Blockr.viz.table.editors`).
 *
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  const BY_LABEL = { records: 'Nest by', groups: 'One row per', variables: 'One column per' };
  /** Records rows a finished input does not offer: it is shown as it came. */
  const PICKS = ['by', 'rowname', 'columns', 'color', 'shadings', 'missing'];

  /**
   * Whether the records layout shows a row, given what it draws.
   * @param {string} key
   * @param {Record<string, unknown>} v
   * @param {TableInfo} info
   */
  function recordsRow(key, v, info) {
    const by = Array.isArray(v.by) && v.by.length > 0;
    if (info.finished && PICKS.includes(key)) return false;
    if (key === 'color' || key === 'shadings') return !by && !info.structured;
    if (key === 'format' || key === 'unit') return info.numbers;
    if (key === 'collapsible') return info.structured;
    if (key === 'missing') return by;
    return true;
  }

  /**
   * Whether "Group rows" applies: a nested table (two `by` columns) that is
   * not a matrix, with a summary that can draw on its outer rows (one
   * drawn as is never does).
   * @param {Record<string, unknown>} v
   */
  function groupRows(v) {
    if (!Array.isArray(v.by) || v.by.length !== 2 || v.look === 'compact') return false;
    const s = Array.isArray(v.summaries) ? v.summaries : [];
    return !s.length || s.some((x) => !x || (x.type || 'simple') !== 'simple' || x.func !== 'identity');
  }

  /**
   * @param {string} key
   * @param {Record<string, unknown>} v
   * @param {TableInfo} info
   */
  function visible(key, v, info) {
    if (key === 'drill_target' || key === 'drill_table') return v.drill != null && v.drill !== '';
    if (key === 'group_rows') return v.layout === 'groups' && groupRows(v);
    if (v.layout === 'records') return recordsRow(key, v, info);
    return true;
  }

  /** blockr.ui's controls; the table's dependency attaches them. */
  const ui = () => /** @type {VizUi} */ (root.Blockr);

  /**
   * A pick among `options`, with blockr.ui's Select.
   * @param {HTMLElement} el
   * @param {{ value: string, label: string }[]} options
   * @param {string} value '' for none: the placeholder shows.
   * @param {{ label: string, placeholder?: string }} o
   * @param {(v: string) => void} onPick
   * @param {VizScope} scope
   */
  function pick(el, options, value, o, onPick, scope) {
    // Labels, as the engine passes them: an object option shows its value.
    const labels = options.map((x) => x.label);
    const at = options.findIndex((x) => x.value === value);
    const inst = ui().Select.single(el, {
      options: labels, selected: at < 0 ? '' : labels[at],
      allowEmpty: !!o.placeholder, placeholder: o.placeholder, bordered: true, label: o.label,
      onChange: (/** @type {string} */ v) => {
        const k = labels.indexOf(v);
        if (k >= 0) onPick(options[k].value);
      }
    });
    scope.own({ dispose: () => inst.destroy() });
  }

  /**
   * A checkbox, blockr.ui's.
   * @param {HTMLElement} el
   * @param {string} label
   * @param {boolean} on
   * @param {(on: boolean) => void} onToggle
   */
  function check(el, label, on, onToggle) {
    el.appendChild(ui().checkbox(label, on, onToggle).el);
  }

  /**
   * A design-system button.
   * @param {string} cls Its own class and the blockr-btn modifiers.
   * @param {string} text
   * @param {string} label
   * @param {() => void} fn
   * @param {VizScope} scope
   */
  function button(cls, text, label, fn, scope) {
    const b = document.createElement('button');
    b.type = 'button';
    b.className = 'blockr-btn ' + cls;
    b.textContent = text;
    b.setAttribute('aria-label', label);
    scope.on(b, 'click', fn);
    return b;
  }

  /**
   * One shading rule: its mode and the numeric columns it paints (none:
   * every numeric column no earlier rule names).
   * @param {Record<string, unknown>} rule
   * @param {VizPayload['columns']} numeric
   * @param {(next: Record<string, unknown> | null) => void} put
   * @param {VizScope} scope
   */
  function ruleEl(rule, numeric, put, scope) {
    const el = document.createElement('div');
    el.className = 'vz-table-shade-rule';
    const mode = el.appendChild(document.createElement('div'));
    mode.className = 'vz-table-shade-mode';
    // The spec's own control: segments with the ramps' icons.
    const arg = /** @type {any} */ (V.spec.table).shadings.type.fields.mode;
    V.gear.controls.build(mode, {
      key: 'mode', arg, label: arg.gear.label, value: rule.mode ?? arg.default, values: rule,
      columns: [], required: false, scope, host: {}, titles: {},
      labelOf: (/** @type {string} */ k) => k,
      set: (/** @type {unknown} */ v) => put(Object.assign({}, rule, { mode: v }))
    });
    const on = new Set(/** @type {string[]} */ (rule.columns || []));
    const cols = el.appendChild(document.createElement('div'));
    cols.className = 'vz-table-shade-cols';
    for (const c of numeric) {
      check(cols, c.label || c.name, on.has(c.name), (now) => {
        if (now) on.add(c.name);
        else on.delete(c.name);
        const picked = numeric.map((x) => x.name).filter((n) => on.has(n));
        put(Object.assign({}, rule, { columns: picked.length ? picked : null }));
      });
    }
    el.appendChild(button('blockr-btn--quiet blockr-btn--xs vz-table-shade-remove', '×', 'Remove rule',
                          () => put(null), scope));
    return el;
  }

  /**
   * The shading rules: one row per rule, and a button that adds one. Every
   * key of a rule goes back as it came, `domain` included.
   * @param {HTMLElement} el
   * @param {VizCustomCtx} ctx
   */
  function shadings(el, ctx) {
    const rules = /** @type {Record<string, unknown>[]} */ (
      Array.isArray(ctx.values.shadings) ? ctx.values.shadings : []);
    const numeric = ctx.columns.filter(V.util.isNumeric);
    el.textContent = '';
    el.classList.add('vz-table-shadings');
    rules.forEach((rule, k) => {
      el.appendChild(ruleEl(rule, numeric, (next) => {
        const out = rules.slice();
        if (next) out[k] = next;
        else out.splice(k, 1);
        ctx.set(out);
      }, ctx.scope));
    });
    el.appendChild(button('blockr-btn--secondary blockr-btn--s vz-table-shade-add',
                          rules.length ? 'Add a rule' : 'Shade cells', 'Add a shading rule',
                          () => ctx.set(rules.concat([{ mode: 'diverging', columns: null, domain: null }])),
                          ctx.scope));
  }

  /** The groups layout's words that do not name a setting of their own (R's
   * groups_title_roles()): `{@by}` prints the innermost row column and `{@parent}` the
   * outer one; `{@value}`, `{@func}`, `{@facet}`, `{@fill}` and a set `{@color}` print the
   * first summary's column, statistic, facet, fill and colour. Each word opens the part
   * it prints, as 0.2.x's summarize table did (`{@group}` opened its one column): a
   * column word replaces its column, keeping the other level. Null for any other word.
   * @param {string} key @param {VizValues} v @returns {VizHostSlot | null} */
  function groupsSlot(key, v) {
    if (v.layout !== 'groups') return null;
    const by = (Array.isArray(v.by) ? v.by : (v.by == null ? [] : [v.by])).map(String);
    const column = (/** @type {string} */ label, /** @type {boolean} */ nullable) =>
      /** @type {VizArg} */ ({ type: { name: 'column' }, nullable, gear: { control: 'column', label } });
    const inner = key === 'by' || (key === 'group' && by.length === 1);
    const outer = (key === 'parent' || key === 'group') && by.length === 2;
    if (inner && by.length) {
      const keep = by.slice(0, -1);
      return { arg: column(BY_LABEL.groups, false), value: by[by.length - 1], label: BY_LABEL.groups,
               write: (x) => ({ by: x == null ? keep : keep.concat([String(x)]) }) };
    }
    if (outer) {
      return { arg: column('Outer rows', true), value: by[0], label: 'Outer rows',
               write: (x) => ({ by: x == null ? [by[1]] : [String(x), by[1]] }) };
    }
    const FIELD = /** @type {Record<string, string>} */ (
      { value: 'col', func: 'func', facet: 'facet', fill: 'fill', color: 'color' });
    const field = FIELD[key];
    const sums = Array.isArray(v.summaries) ? v.summaries : [];
    const s1 = /** @type {Record<string, unknown> | null} */ (
      sums[0] && typeof sums[0] === 'object' ? sums[0] : null);
    if (!field || !s1) return null;
    // An unset summary colour leaves `{@color}` to the table's own colour, as R prints it.
    if (key === 'color' && s1.color == null) return null;
    const spec = /** @type {any} */ (V.spec).table.summaries.type;
    const arg = (spec.variants[String(s1[spec.key] || 'simple')] || {})[field];
    if (!arg || !arg.gear) return null;
    return { arg, value: s1[field] ?? null, label: arg.gear.label,
             write: (x) => ({ summaries: [Object.assign({}, s1, { [field]: x })].concat(sums.slice(1)) }) };
  }

  /**
   * Mount the gear, or nothing without the engine.
   * @param {TableFrame} frame
   * @param {VizScope} scope
   * @param {(layout: string) => TableInfo} info What a layout draws.
   * @param {(layout: string) => void} [onLayout] A layout pick, before the
   *   tray shows the new layout's rows.
   * @returns {TableGearHandle | null}
   */
  function mount(frame, scope, info, onLayout) {
    /** What R resolved with the last payload: the drill targets. */
    /** @type {TableDerived} */
    let derived = {};
    /** @type {VizGearHost} */
    const host = {
      // The blocks a click can be sent to, as the chart offers them.
      choices: (key) => key === 'drill_target'
        ? (/** @type {VizChoice[]} */ ([{ value: '', label: 'None' }])).concat(derived.ctrl_choices || [])
        : null,
      // A Table 1 needs its variables (variables.js marks the pick).
      required: (key, v) => key === 'summaries' && v.layout === 'variables',
      visible: (key, v) => visible(key, v, info(String(v.layout))),
      label: (key, v) => key === 'by' ? BY_LABEL[/** @type {'records'} */ (v.layout)] : null,
      // The groups layout's words for parts of `by` and of the first summary.
      slot: groupsSlot,
      // What an empty picker means.
      placeholder: (key, v) => key === 'columns' ? 'All columns'
        : key === 'by' && v.layout === 'records' ? 'All' : null,
      // A layout's own editor for a key ("variables.summaries"), else the
      // key's ("shadings").
      custom: (key, el, ctx) => {
        const ed = V.table.editors[ctx.values.layout + '.' + key] || V.table.editors[key];
        if (ed) ed(el, ctx);
      },
      onChange: (changed) => {
        if (onLayout && typeof changed.layout === 'string') onLayout(changed.layout);
      }
    };
    const engine = frame.mountGear({ block: 'table', typeKey: 'layout', title: 'Table settings', host });
    return {
      engine, open: frame.openGear,
      update: (p) => {
        derived = /** @type {TableDerived} */ (p.derived || {});
        engine.update(p);
      }
    };
  }

  V.table.gear = { mount, visible, shadings, groupsSlot };
  V.table.ui = { pick, check, button };
  V.table.editors = Object.assign(V.table.editors || {}, { shadings });
})();
// --- table/summaries.js
// @ts-check
/**
 * The groups layout's Columns row in the gear: one entry per summary, its
 * fields built from the spec (`summaries`, one variant per `type`) with the
 * gear's own controls, so a field reads and edits as every other gear row.
 * An entry folds to its title; it moves up or down, or goes; the Add menu
 * appends an entry of any type. Every edit sends the whole list.
 *
 * A new entry whose type has required fields (a distribution's column, a
 * span's start and end) stays in the editor until they are set, then goes
 * with the list: the spec refuses an entry without them.
 *
 * Rules the spec's notes give and its fields cannot: an aggregate's column
 * only when it aggregates one, the stacking only for a bar split by
 * colour, the paint only for a tile, the outer range only for a mark.
 *
 * Each type has its drawing (core/gear/icons.js, `summary:<type>`): it
 * heads the entry and its row in the Add menu, under the type's one line.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  const TYPES = { simple: 'Summary', dist: 'Distribution', field: 'Values', sparkline: 'Sparkline',
                  spans: 'Spans', pair: 'Pair', expr: 'Expression', custom: 'Custom' };
  /** A type's line in the Add menu. */
  const ABOUT = { simple: 'Count, sum or mean: bar, number, dot', dist: 'Spread of a number: box or dot range',
                  field: 'The distinct values, as words', sparkline: 'A small line along an ordered column',
                  spans: 'Intervals in time, such as periods', pair: 'Two values joined, from and to',
                  expr: 'An R expression per row group', custom: 'A function of the rows, any mark' };
  /** The Add menu's groups: numbers and words; lines, intervals and pairs; code. */
  const BREAKS = ['sparkline', 'expr'];

  /** @typedef {Record<string, unknown>} Entry */

  /**
   * Whether an entry shows field `f`.
   * @param {Entry} s
   * @param {string} f
   */
  function shows(s, f) {
    if (s.type === 'simple') {
      if (f === 'col') return s.func != null && s.func !== 'count';
      if (f === 'position') return s.color != null && (s.show == null || s.show === 'bar');
      if (f === 'fill') return s.show === 'tile';
    }
    if (s.type === 'dist' && f === 'whiskers') return s.show !== 'text';
    return true;
  }

  const FUNC = { count_distinct: 'distinct count', mean: 'mean', median: 'median', sum: 'sum',
                 min: 'min', max: 'max', sd: 'sd' };
  /** @param {unknown} type */
  const glyph = (type) => (V.gear.glyphs || {})['summary:' + String(type)] || '';

  /**
   * An entry's title: its name, else its type.
   * @param {Entry} s
   */
  function title(s) {
    if (typeof s.name === 'string' && s.name) return s.name;
    return TYPES[/** @type {'simple'} */ (s.type)] || String(s.type);
  }

  /**
   * What an entry computes, in words: "count of rows, split by SEX".
   * @param {Entry} s
   */
  function describe(s) {
    const col = typeof s.col === 'string' ? s.col : '';
    /** @type {string} */
    let out;
    if (s.type === 'simple') {
      const f = String(s.func || 'count');
      out = f === 'count' ? 'count of rows' : (FUNC[/** @type {'mean'} */ (f)] || f) + ' of ' + col;
      if (s.color) out += ', split by ' + String(s.color);
    } else if (s.type === 'dist') {
      out = 'distribution of ' + col;
    } else if (s.type === 'field') {
      out = 'values of ' + col;
    } else if (s.type === 'expr') {
      out = String(s.expr || 'an expression');
    } else {
      out = (TYPES[/** @type {'simple'} */ (s.type)] || String(s.type)).toLowerCase() +
        (col ? ' of ' + col : '');
    }
    return s.facet ? out + ', by ' + String(s.facet) : out;
  }

  /**
   * An entry's head: its drawing, name, what it computes and a chevron.
   * @param {HTMLButtonElement} b
   * @param {Entry} s
   */
  function headText(b, s) {
    b.textContent = '';
    const part = (/** @type {string} */ cls, /** @type {string} */ text) => {
      const x = b.appendChild(document.createElement('span'));
      x.className = cls;
      x.textContent = text;
      return x;
    };
    part('vz-table-sum-icon', '').innerHTML = glyph(s.type);
    part('vz-table-sum-name', title(s));
    part('vz-table-sum-desc', describe(s));
    part('vz-table-sum-chev', '›');
  }

  /**
   * A small quiet button.
   * @param {string} cls
   * @param {string} text
   * @param {string} label
   * @param {() => void} fn
   * @param {VizScope} scope
   */
  function button(cls, text, label, fn, scope) {
    const b = V.table.ui.button('blockr-btn--quiet blockr-btn--xs vz-table-sum-ctl ' + cls, text, label,
                                fn, scope);
    b.setAttribute('data-blockr-tooltip', label);
    return b;
  }

  /**
   * The fields of entry `k`, built with the gear's controls.
   * @param {HTMLElement} body
   * @param {Entry} s
   * @param {Record<string, any>} variant
   * @param {VizCustomCtx} ctx
   * @param {(next: Entry) => void} put
   */
  function fields(body, s, variant, ctx, put) {
    const C = V.gear.controls;
    const keys = Object.keys(variant).filter((f) => variant[f].gear && shows(s, f));
    // The mark first, over the editor's width: it decides the fields that
    // follow. The name last.
    const order = [...keys.filter((x) => x === 'show'), ...keys.filter((x) => x !== 'show' && x !== 'name'),
                   ...keys.filter((x) => x === 'name')];
    for (const f of order) {
      const arg = variant[f];
      const el = document.createElement('div');
      el.className = 'vz-row vz-table-sum-field' + (f === 'show' ? ' vz-table-sum-field--wide' : '');
      const lab = document.createElement('label');
      lab.className = 'blockr-label';
      lab.textContent = arg.gear.label;
      el.appendChild(lab);
      const control = document.createElement('div');
      el.appendChild(control);
      body.appendChild(el);
      C.build(control, {
        key: f, arg, label: arg.gear.label, value: s[f] === undefined ? arg.default : s[f],
        values: Object.assign({}, ctx.values, s), columns: ctx.columns, required: false,
        scope: ctx.scope, host: {}, titles: {},
        labelOf: (/** @type {string} */ x) => x,
        set: (/** @type {unknown} */ v) => put(Object.assign({}, s, { [f]: v }))
      });
    }
  }

  /**
   * Whether an entry has every field its type requires.
   * @param {Entry} s
   * @param {Record<string, any>} variants
   */
  function complete(s, variants) {
    const variant = variants[/** @type {string} */ (s.type)] || {};
    return Object.keys(variant).every((f) => !variant[f].required || (s[f] != null && s[f] !== ''));
  }

  /**
   * One entry: its head, which opens it, and its fields with move and
   * remove.
   * @param {Entry[]} list
   * @param {number} k
   * @param {Record<string, any>} variants
   * @param {VizCustomCtx} ctx
   * @param {Set<number>} open
   * @param {(next: Entry[]) => void} send
   */
  function entry(list, k, variants, ctx, open, send) {
    const s = list[k];
    const el = document.createElement('div');
    el.className = 'vz-table-sum-row' + (open.has(k) ? ' is-open' : '');
    const head = document.createElement('div');
    head.className = 'vz-table-sum-head';
    const name = button('vz-table-sum-toggle', '', 'Edit ' + title(s), () => {
      if (open.has(k)) open.delete(k);
      else open.add(k);
      el.classList.toggle('is-open');
      body.hidden = !open.has(k);
      name.setAttribute('aria-expanded', String(open.has(k)));
    }, ctx.scope);
    name.removeAttribute('data-blockr-tooltip');
    name.setAttribute('aria-expanded', String(open.has(k)));
    headText(name, s);
    const move = (/** @type {number} */ d) => {
      const next = list.slice();
      next.splice(k + d, 0, next.splice(k, 1)[0]);
      send(next);
    };
    head.append(name);
    el.appendChild(head);
    const body = document.createElement('div');
    body.className = 'vz-table-sum-body';
    body.hidden = !open.has(k);
    const variant = variants[/** @type {string} */ (s.type)];
    if (variant) {
      fields(body, s, variant, ctx, (next) => send(list.map((x, j) => (j === k ? next : x))));
    }
    const acts = body.appendChild(document.createElement('div'));
    acts.className = 'vz-table-sum-actions';
    if (k > 0) acts.appendChild(button('vz-table-sum-up', 'Move up', 'Move up', () => move(-1), ctx.scope));
    if (k < list.length - 1) {
      acts.appendChild(button('vz-table-sum-down', 'Move down', 'Move down', () => move(1), ctx.scope));
    }
    acts.appendChild(button('vz-table-sum-rm', 'Remove', 'Remove this column',
                            () => send(list.filter((_, j) => j !== k)), ctx.scope));
    el.appendChild(body);
    return el;
  }

  /** Entries open in the tray, per element, across re-renders. */
  const opened = new WeakMap();
  /** A new entry still missing a required field, per element. */
  const drafts = new WeakMap();

  /**
   * The Columns row.
   * @param {HTMLElement} el
   * @param {VizCustomCtx} ctx
   */
  function editor(el, ctx) {
    const spec = /** @type {any} */ (V.spec.table);
    const variants = spec && spec.summaries && spec.summaries.type.variants;
    if (!variants) return;
    const list = /** @type {Entry[]} */ (Array.isArray(ctx.values.summaries) ? ctx.values.summaries : []);
    const box = /** @type {Element} */ (el.closest('.blockr-settings') || el);
    let open = opened.get(box);
    if (!open) {
      // A lone column opens: it is what the reader came to change.
      open = new Set(list.length === 1 ? [0] : []);
      opened.set(box, open);
    }
    el.textContent = '';
    el.classList.add('vz-table-summaries');
    list.forEach((_, k) => el.appendChild(entry(list, k, variants, ctx, open, ctx.set)));
    // The draft: sent with the list once complete, dropped by its Remove.
    const draft = (/** @type {Entry | null} */ s) => {
      if (s && complete(s, variants)) {
        drafts.delete(box);
        open.add(list.length);
        ctx.set(list.concat([s]));
        return;
      }
      if (s) drafts.set(box, s);
      else drafts.delete(box);
      editor(el, ctx);
    };
    const pending = drafts.get(box);
    if (pending) {
      const row = entry([pending], 0, variants, ctx, new Set([0]), (next) => draft(next[0] || null));
      row.classList.add('is-draft');
      el.appendChild(row);
    }
    const add = el.appendChild(document.createElement('div'));
    add.className = 'vz-table-sum-addcol';
    addMenu(add, Object.keys(variants), (t) => draft({ type: t }), ctx.scope);
  }

  /**
   * "+ Add a column": a button that opens blockr.ui's menu, a row per type
   * with its drawing, its name and its line. Without Blockr.menu, a pick of
   * the names.
   * @param {HTMLElement} el
   * @param {string[]} types
   * @param {(type: string) => void} onAdd
   * @param {VizScope} scope
   */
  function addMenu(el, types, onAdd, scope) {
    const ui = /** @type {VizUi} */ (root.Blockr);
    const name = (/** @type {string} */ t) => TYPES[/** @type {'simple'} */ (t)] || t;
    if (!ui.menu) {
      V.table.ui.pick(el, types.map((t) => ({ value: t, label: name(t) })), '',
                      { label: 'Add a column', placeholder: '+ Add a column' }, onAdd, scope);
      return;
    }
    /** @type {{ close(): void } | null} */
    let shown = null;
    const btn = V.table.ui.button('blockr-btn--secondary blockr-btn--s vz-table-sum-add', '+ Add a column',
                                  'Add a column', () => {
      if (shown) { shown.close(); return; }
      /** @type {VizMenuEntry[]} */
      const items = [];
      for (const t of types) {
        if (BREAKS.includes(t) && items.length) items.push({ divider: true });
        items.push({ label: name(t), icon: glyph(t) || undefined, onSelect: () => onAdd(t) });
      }
      const m = /** @type {NonNullable<VizUi['menu']>} */ (ui.menu)
        .call(ui, btn, { items, onClose: () => { shown = null; } });
      shown = m;
      m.el.classList.add('vz-table-sum-menu');
      // Each row's line under its name.
      const rows = m.el.querySelectorAll('.blockr-menu__item');
      types.forEach((t, i) => {
        const label = rows[i] && rows[i].querySelector('.blockr-menu__label');
        if (!label || !ABOUT[/** @type {'simple'} */ (t)]) return;
        const line = document.createElement('span');
        line.className = 'vz-table-sum-about';
        line.textContent = ABOUT[/** @type {'simple'} */ (t)];
        label.appendChild(line);
      });
    }, scope);
    btn.setAttribute('aria-haspopup', 'menu');
    el.appendChild(btn);
  }

  V.table.editors = Object.assign(V.table.editors || {}, { 'groups.summaries': editor });
  V.table.summaries = { shows, title, describe };
})();
// --- table/records.js
// @ts-check
/**
 * The records layout: the input rows as a table (dom.js), or as cards with
 * `look = "cards"` (cards.js), with the drill (drill.js).
 *
 * R sends the table in `payload.data` (a TableModel), so a payload whose
 * data revision is unchanged (a drill, a title edit) reuses the drawn table
 * and keeps its sort, folds and scroll. `config.selection` is R's record of
 * the drill; a click shows at once and R's next payload confirms it.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const T = V.table;

  /**
   * A selection from the config, if it has the shape of one.
   * @param {unknown} x
   * @returns {TableSelection | null}
   */
  function asSelection(x) {
    const s = /** @type {any} */ (x);
    if (!(s && typeof s === 'object' && s.filters && typeof s.filters === 'object')) return null;
    // R writes a click without a column half as `columns: {}`.
    const cols = s.columns && typeof s.columns === 'object' && Object.keys(s.columns).length;
    return cols ? s : { type: s.type, filters: s.filters };
  }

  /** @param {number} shown @param {number} total */
  function countText(shown, total) {
    if (!total) return '';
    const noun = total === 1 ? ' row' : ' rows';
    return shown === total ? total + noun : shown + ' of ' + total + noun;
  }

  /**
   * A structured table: R built it from an annotated frame (a Table 1, the
   * rows nested under `by`, a finished input), so it has sections, nested
   * rows or row claims. It takes the Table-1 typography and counts no rows.
   * @param {TableModel} m
   */
  const isStructured = (m) => !!(m.sections && m.sections.length) ||
    !!(m.rows && (m.rows.keys || m.rows.indent));

  class Records {
    /**
     * @param {TableFrame} frame
     * @param {VizScope} scope
     * @param {TableLayoutOpts} [opts] `still`: a picture or a page without
     *   Shiny, every row drawn and no tools.
     */
    constructor(frame, scope, opts = {}) {
      this.frame = frame;
      this.scope = scope;
      this.grid = new T.Grid(frame.box, scope);
      this.grid.hooks.view = () => frame.inView();
      this.cardsHost = scope.node(document.createElement('div'));
      this.cardsHost.className = 'vz-table-cards';
      this.cardsHost.hidden = true;
      frame.box.appendChild(this.cardsHost);
      /** @type {TableModel | null} */
      this.model = null;
      /** @type {VizPayload | null} */
      this.payload = null;
      /** The selection shown: R's, or a click R has not answered. */
      this.sel = /** @type {TableSelection | null} */ (null);
      this.drawnWith = '';
      this.still = !!opts.still;
      /** @type {Record<string, string>} */
      this.labels = {};
      this.drill = T.drill.wire(this.grid, frame, scope, {
        model: () => this.model,
        selection: () => this.sel,
        select: (s) => {
          this.sel = s;
          this.repaint();
        },
        sent: (s) => frame.flash('Drilled down to ' + T.drill.label(s, this.labels)),
        transient: () => this.transient()
      });
      // The header follows the scroll (chrome.js followHeader()). A fresh
      // table's header starts at the top, so a draw places it too, in the
      // next frame: placing it reads the layout, which a draw must not.
      // One capture listener on the document sees every scroller's scroll:
      // the dock moves a card between containers after it is drawn.
      let placing = false;
      const place = () => {
        if (placing) return;
        placing = true;
        scope.frame(() => {
          placing = false;
          frame.followHeader();
        });
      };
      if (!this.still) {
        const follow = () => frame.followHeader();
        scope.on(document, 'scroll', follow, { capture: true, passive: true });
        scope.on(window, 'resize', follow);
      }
      this.grid.hooks.drawn = () => {
        this.drill.paint();
        this.footer();
        if (!this.still) place();
      };
      scope.own({ dispose: frame.onSearch((q) => {
        if (this.cardsOn()) this.drawCards();
        else this.grid.search(q);
      }) });
      scope.on(this.cardsHost, 'click', (e) => {
        const t = /** @type {Element | null} */ (e.target);
        const row = t && t.closest ? t.closest('.vz-table-card-row[data-i]') : null;
        if (row) this.drill.clickRow(Number(row.getAttribute('data-i')), row);
      });
    }

    /** @returns {TableConfig} */
    config() {
      return /** @type {TableConfig} */ (/** @type {unknown} */ (
        this.payload ? this.payload.config : {}));
    }

    transient() {
      const t = this.config().drill_target;
      return !!(t && String(t).trim());
    }

    cardsOn() {
      return !!this.model && this.config().look === 'cards' && !!this.model.cards;
    }

    /** @param {VizPayload} p */
    render(p) {
      const before = this.payload ? asSelection(this.config().selection) : null;
      this.payload = p;
      const cfg = this.config();
      const d = /** @type {TableDerived} */ (p.derived || {});
      // No table is `data: {}`: a null `data` means "the rows you hold".
      const data = /** @type {any} */ (p.data);
      const m = /** @type {TableModel | null} */ (data && Array.isArray(data.cols) ? data : null);
      this.labels = Object.fromEntries((p.columns || []).map((c) => [c.name, c.label || c.name]));
      const shown = this.sel;
      this.sel = asSelection(cfg.selection);
      const f = this.frame;
      f.titles(d.titles);
      const has = !!m && !d.empty;
      f.showTools(this.tools(cfg, has, d));
      if (f.empty(d.empty) || !m) {
        this.model = null;
        this.drawnWith = '';
        this.grid.clear();
        this.cardsHost.hidden = true;
        f.el.classList.remove('vz-table-clickable');
        f.count('');
        f.status(null);
        return;
      }
      f.structured(isStructured(m));
      f.el.classList.toggle('vz-table-clickable', T.drill.clickable(m));
      const moved = !T.drill.same(before, this.sel);
      this.draw(m, cfg, moved ? (T.drill.same(shown, this.sel) ? 'shown' : 'moved') : null);
    }

    /**
     * The header's tools: search and download, when on and there is a
     * table.
     * @param {TableConfig} cfg
     * @param {boolean} has
     * @param {TableDerived} d
     * @returns {{ search: boolean, download: boolean, downloads?: VizDownload[] | null, onOpen?: () => void }}
     */
    tools(cfg, has, d) {
      if (this.still) return { search: false, download: false };
      return { search: has && cfg.search !== false, download: has && cfg.download !== false,
               downloads: d.downloads };
    }

    /**
     * How the Grid draws the model; a change of this redraws it.
     * @param {TableConfig} cfg
     * @param {TableModel} m
     * @returns {TableGridFlags}
     */
    flags(cfg, m) {
      // The compact look: value-column titles on their side (table.css).
      const attrs = cfg.look === 'compact' ? { 'data-dt-rotate-titles': 'on' } : undefined;
      return { sortable: cfg.sortable !== false, collapsible: cfg.collapsible !== false,
               all: this.still, attrs };
    }

    /** @returns {TableCardOpts} */
    cardOpts() {
      return {};
    }

    /**
     * The left of the footer: the row count of a flat table.
     * @param {TableModel} m
     * @param {{ shown: number, total: number }} c
     */
    countLine(m, c) {
      return isStructured(m) ? '' : countText(c.shown, c.total);
    }

    /**
     * Draw the model afresh when it or how it is drawn changed. Else a
     * payload that moved R's selection only lights it, and R's answer to a
     * click the rows already show only updates the footer; any other
     * payload redraws the rows.
     * @param {TableModel} m
     * @param {TableConfig} cfg
     * @param {'shown' | 'moved' | null} [sel] How R's selection changed.
     */
    draw(m, cfg, sel = null) {
      const flags = this.flags(cfg, m);
      const how = cfg.look + JSON.stringify(flags);
      if (m === this.model && how === this.drawnWith) {
        if (sel === 'shown') this.footer();
        else if (sel === 'moved') this.repaint();
        else if (this.cardsOn()) this.drawCards();
        else this.grid.draw();
        return;
      }
      this.model = m;
      this.drawnWith = how;
      if (this.cardsOn()) {
        this.grid.clear();
        this.cardsHost.hidden = false;
        this.drawCards();
        return;
      }
      this.cardsHost.hidden = true;
      this.cardsHost.innerHTML = '';
      this.grid.query = this.frame.query();
      this.grid.set(m, flags);
    }

    /** The search applied to the card rows. */
    cardOrder() {
      const m = /** @type {TableModel} */ (this.model);
      const n = Math.min(m.n, m.cards ? m.cards.n : 0);
      const q = this.frame.query();
      const all = Array.from({ length: n }, (_, i) => i);
      return q ? all.filter((i) => T.rowText(m, i).includes(q)) : all;
    }

    drawCards() {
      const m = /** @type {TableModel} */ (this.model);
      const order = this.cardOrder();
      T.cards.render(this.cardsHost, /** @type {TableCards} */ (m.cards), this.scope,
                     Object.assign({ order, rowClass: (/** @type {number} */ i) => this.drill.rowClass(i) },
                                   this.cardOpts()));
      this.footer();
    }

    /** Show the selection: lit rows and columns, and the footer. */
    repaint() {
      if (!this.model) return;
      if (!this.cardsOn()) {
        this.grid.relight();
        return;
      }
      this.cardsHost.querySelectorAll('.vz-table-card-row[data-i]').forEach((row) => {
        const cls = this.drill.rowClass(Number(row.getAttribute('data-i')));
        row.classList.toggle('vz-table-row-active', cls.split(' ').includes('vz-table-row-active'));
      });
      this.drill.paint();
      this.footer();
    }

    /** The row count and the status line: what the selection holds, in R's
     * words (`derived.receipt`), shown once R holds the selection the view
     * lights. Only R names the column a structured table's group level
     * stands for, so the browser writes no line of its own and a click's line
     * appears once, when R answers. A drill sent to another block flashes its
     * receipt instead (frame.flash). */
    footer() {
      const m = this.model;
      const f = this.frame;
      if (!m) return;
      const c = this.cardsOn() ? { shown: this.cardOrder().length, total: m.n }
        : this.grid.counts();
      // A printed table has no search to count against.
      f.count(this.still ? '' : this.countLine(m, c));
      const sel = this.transient() ? null : this.sel;
      const d = /** @type {TableDerived} */ (this.payload ? this.payload.derived : {});
      const held = !!sel && T.drill.same(sel, asSelection(this.config().selection));
      if (!held || !d.receipt) {
        f.status(null);
        return;
      }
      f.status(d.receipt, () => {
        V.util.select(f.id, null);
        this.sel = null;
        this.repaint();
      });
    }

    /** @returns {TableInfo} */
    info() {
      const m = this.model;
      const structured = !!m && isStructured(m);
      const by = this.config().by;
      return { structured, finished: structured && !(Array.isArray(by) && by.length),
               numbers: !m || m.cols.some((c) => !!c.num) };
    }

    dispose() {
      this.grid.clear();
    }
  }

  T.Records = Records;
  T.layouts.records = Records;
})();
// --- table/groups.js
// @ts-check
/**
 * The groups layout: one row per level of `by` (an outer level folds its
 * inner ones), one column per summary, each cell a mark (marks.js), with
 * the column's scale printed once under its header. The records view does
 * the rest (search, sort, drill, cards, footer); this adds the marks, the
 * legend, the fold note, and the picture the downloads take.
 *
 * Looks: `compact` is the matrix (square tile cells, titles on their side),
 * set on the table as data-summarize-compact; `cards` draws each row as
 * cards with the column's mark in each.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const T = V.table;
  const esc = V.util.esc;


  /**
   * The legend: one titled group per colour column.
   * @param {HTMLElement} el
   * @param {TableLegend | null | undefined} lg
   */
  function paintLegend(el, lg) {
    const groups = (lg && lg.groups) || [];
    el.hidden = !groups.length;
    el.innerHTML = groups.map((g) => '<span class="vz-table-groups-legend-group">' +
      '<span class="vz-table-groups-legend-title">' + esc(g.title) + '</span>' +
      (g.items || []).map((it) => '<span class="vz-table-groups-legend-item">' + (it.tag
        ? '<span class="vz-table-groups-legend-tag">' + esc(it.tag) + '</span>'
        : it.shape != null
          ? '<svg class="vz-table-groups-legend-shape" viewBox="-7 -7 14 14">' + V.table.marks.shape(Number(it.shape)) + '</svg>'
          : '<i style="background:' + esc(it.color) + '"></i>') +
        esc(it.label) + '</span>').join('') + '</span>').join('');
  }

  /**
   * A tile column as R sends it, back to one entry per row (R/table-groups-view.R,
   * table_groups_tile_wire()): the filled rows spread over the column, the text
   * colour looked up by fill. In place and once: the binding hands the same
   * model to every render.
   * @param {TableModel | null | undefined} m
   */
  function dense(m) {
    if (!m || !Array.isArray(m.cols)) return;
    for (const c of m.cols) {
      const at = c.at;
      if (Array.isArray(at)) {
        for (const k of /** @type {const} */ (['disp', 'key', 'bg', 'fg'])) {
          const held = /** @type {any[] | null | undefined} */ (c[k]);
          if (!Array.isArray(held)) continue;
          const full = new Array(m.n).fill(null);
          for (let j = 0; j < at.length; j++) full[at[j]] = held[j];
          /** @type {any} */ (c)[k] = full;
        }
        c.at = null;
      }
      const of = c.ink;
      if (of && Array.isArray(c.bg) && !c.fg) {
        c.fg = c.bg.map((b) => (b != null && of[b]) || null);
        c.ink = null;
      }
      const mk = /** @type {TableMarkInterval | null | undefined} */ (c.mark);
      if (mk && mk.kind === 'interval' && !Array.isArray(c.disp)) {
        c.disp = laneTexts(mk, m.n);
      }
    }
  }

  /**
   * A swimlane column's text per row, which the search reads: its segments'
   * and events' tooltips, each once, joined by " | ", as R's cells_text()
   * writes it. R leaves it out of the payload, since the tooltips are there
   * (R/table-lane-json.R).
   * @param {TableMarkInterval} mk
   * @param {number} n
   * @returns {(string | null)[]}
   */
  function laneTexts(mk, n) {
    /** @type {(string | null)[]} */
    const out = new Array(n);
    for (let i = 0; i < n; i++) {
      const tips = (mk.tips && mk.tips[i]) || [];
      const ev = (mk.eventTips && mk.eventTips[i]) || [];
      const t = Array.from(new Set(tips.concat(ev))).join(' | ');
      out[i] = t === '' ? null : t;
    }
    return out;
  }

  class Groups extends /** @type {any} */ (T.Records) {
    /**
     * @param {TableFrame} frame
     * @param {VizScope} scope
     * @param {TableLayoutOpts} [opts]
     */
    constructor(frame, scope, opts = {}) {
      super(frame, scope, opts);
      frame.el.classList.add('vz-table-groups');
      scope.own({ dispose: () => frame.el.classList.remove('vz-table-groups') });
      this.legend = scope.node(document.createElement('div'));
      this.legend.className = 'vz-table-groups-legend';
      frame.box.before(this.legend);
      const grid = /** @type {TableGrid} */ (this.grid);
      const drillClass = /** @type {(i: number) => string} */ (grid.hooks.rowClass);
      grid.hooks.rowClass = (i) => [drillClass(i), this.rowKind(i)].filter(Boolean).join(' ');
      grid.hooks.cell = (c, i) => T.marks.cell(c, i, { labels: this.labelsOn() });
      // Bar labels need the drawn lane's width to pick their place.
      grid.hooks.rows = (body) => {
        T.marks.fitLabels(body);
        const table = body.closest('table');
        if (table) T.axis.fit(table);
      };
      // A table drawn while hidden (a closed dock panel) has no width yet; a
      // resize places the labels again.
      if (typeof ResizeObserver !== 'undefined') {
        let w = 0;
        scope.resize(frame.box, () => {
          const now = frame.box.clientWidth;
          if (now && now !== w) {
            T.marks.fitLabels(frame.box, w !== 0);
            T.axis.fit(frame.box);
          }
          w = now;
        });
      }
      grid.hooks.head = (i) => this.headKind(i);
      T.tooltip.wire(frame.box, scope, () => this.model);
      /** What the last picture was of. */
      this.pictured = '';
      /** The picture of `pictured` on its way to R. @type {Promise<unknown> | null} */
      this.sending = null;
    }

    labelsOn() {
      return this.config().value_labels !== false;
    }

    /** @returns {boolean} */
    grouped() {
      const by = this.config().by;
      return Array.isArray(by) && by.length > 0;
    }

    /**
     * A row's place in the nesting: a parent of inner rows, an inner row,
     * the last inner row before the next parent.
     * @param {number} i
     */
    rowKind(i) {
      const m = /** @type {TableModel | null} */ (this.model);
      const ind = m && m.rows && m.rows.indent;
      if (!ind) return 'vz-table-groups-row';
      const next = i + 1 < ind.length ? ind[i + 1] : null;
      if (next != null && next > ind[i]) return 'vz-table-groups-row is-parent';
      if (ind[i] > 0) return 'vz-table-groups-row is-child' + (next == null || next < ind[i] ? ' is-group-end' : '');
      return 'vz-table-groups-row';
    }

    /**
     * A parent row with no values of its own (group_rows = "name", the
     * compact look's empty tiles) is one cell across the row. One with
     * values (group_rows = "summary") is a plain row, its name in the label
     * column like any other.
     * @param {number} i
     * @returns {'' | 'span'}
     */
    headKind(i) {
      const m = /** @type {TableModel | null} */ (this.model);
      const ind = m && m.rows && m.rows.indent;
      if (!m || !ind || !(i + 1 < ind.length && ind[i + 1] > ind[i])) return '';
      const empty = m.cols.every((c, j) => j === m.stub ||
        ((c.disp[i] == null || c.disp[i] === '') && !(c.bg && c.bg[i])));
      return empty ? 'span' : '';
    }

    /** @param {VizPayload} p */
    render(p) {
      dense(/** @type {TableModel | null} */ (p.data));
      super.render(p);
      const d = /** @type {TableDerived} */ (p.derived || {});
      paintLegend(this.legend, this.model && !this.cardsOn() ? d.legend : null);
      const grid = /** @type {TableGrid} */ (this.grid);
      const fonts = root.document.fonts && root.document.fonts.ready
        ? root.document.fonts.ready : Promise.resolve();
      fonts.then(() => tipCutTitles(grid.table));
    }

    /**
     * Search and sorting need groups; a picture has no tools.
     * @param {TableConfig} cfg
     * @param {boolean} has
     * @param {TableDerived} d
     */
    tools(cfg, has, d) {
      if (this.still) return { search: false, download: false };
      return { search: has && this.grouped() && cfg.search !== false,
               download: has && cfg.download !== false, downloads: d.downloads,
               onOpen: () => this.sendPicture() };
    }

    /**
     * @param {TableConfig} cfg
     * @param {TableModel} m
     * @returns {TableGridFlags}
     */
    flags(cfg, m) {
      const labels = this.labelsOn();
      const axis = cfg.column_axis !== false;
      /** @type {TableGrid} */ (this.grid).hooks.axis = axis ? (c) => T.marks.axis(c, labels) : undefined;
      /** @type {Record<string, string>} */
      const attrs = { 'data-summarize-values': labels ? 'on' : 'off',
                      'data-summarize-axis': axis ? 'on' : 'off' };
      if (cfg.bar_width && cfg.bar_width !== 'fit') attrs['data-summarize-width'] = cfg.bar_width;
      if (cfg.look === 'compact') attrs['data-summarize-compact'] = '1';
      if (m.rows && m.rows.indent && m.rows.indent.some((x) => x > 0)) attrs['data-summarize-nested'] = '1';
      return { sortable: cfg.sortable !== false && this.grouped(), collapsible: true, all: this.still,
               cls: 'vz-table-groups-table', stubCls: 'vz-table-groups-label-col', flush: true, attrs };
    }

    /** A card draws its column's mark, without the value it prints itself. */
    cardOpts() {
      const m = /** @type {TableModel} */ (this.model);
      return { glyph: (/** @type {number} */ k, /** @type {number} */ i) =>
        T.marks.glyph(m.cols[k] ? m.cols[k].mark : null, i, false) };
    }

    /**
     * The fold note in place of a row count.
     * @param {TableModel} m
     */
    countLine(m) {
      return m.note || '';
    }

    footer() {
      if (this.still) {
        if (this.model) this.frame.count(this.countLine(this.model));
        return;
      }
      super.footer();
    }

    /** @returns {TableInfo} */
    info() {
      return { structured: false, finished: false, numbers: true };
    }

    /**
     * Send R the picture of the table as drawn, once per payload.
     * @returns {Promise<unknown> | null} The sending, which a click on a
     *   download format waits for; null without a table.
     */
    sendPicture() {
      const p = /** @type {VizPayload | null} */ (this.payload);
      if (!p || !this.model) return null;
      const key = p.state_rev + ':' + p.data_rev;
      if (key === this.pictured) return this.sending;
      this.pictured = key;
      const width = Math.round(this.frame.el.getBoundingClientRect().width) || 900;
      this.sending = picture(Object.assign({}, p, { data: this.model }), width).then(
        (pic) => V.util.send(this.frame.id, 'capture', pic),
        // Without a picture R paints the table itself.
        () => { this.pictured = ''; });
      return this.sending;
    }
  }

  /**
   * A compact matrix's rotated titles stop at their cap (max-height) with an
   * ellipsis; each one cut gets its full text as the tooltip. The text is
   * measured on a canvas, so a table drawn in a hidden panel gets them too
   * (blockr.ui's own cut-off check reads widths, and these overflow upwards).
   * @param {HTMLTableElement | null | undefined} table
   */
  function tipCutTitles(table) {
    if (!table || !table.isConnected || table.getAttribute('data-summarize-compact') !== '1') return;
    const canvas = table.ownerDocument.createElement('canvas');
    const ctx = canvas.getContext ? canvas.getContext('2d') : null;
    if (!ctx) return;
    table.querySelectorAll('thead th.vz-table-col-header .blockr-col-name').forEach((n) => {
      const th = n.closest('th');
      const cs = root.getComputedStyle(n);
      const cap = parseFloat(cs.maxHeight);
      if (!th || !isFinite(cap)) return;
      ctx.font = cs.font || [cs.fontStyle, cs.fontWeight, cs.fontSize, cs.fontFamily].join(' ');
      const text = (n.textContent || '').trim();
      if (ctx.measureText(text).width > cap) th.setAttribute('data-blockr-tooltip', text);
      else th.removeAttribute('data-blockr-tooltip');
    });
  }

  /**
   * Wait for the fonts and two frames, so the table is laid out.
   * @param {VizScope} scope
   * @returns {Promise<void>}
   */
  function settled(scope) {
    const fonts = root.document.fonts && root.document.fonts.ready
      ? root.document.fonts.ready : Promise.resolve();
    return fonts.then(() => new Promise((r) => scope.frame(() => scope.frame(() => r(undefined)))));
  }

  /**
   * The picture's pixel ratio: R's (`derived.capture_ratio`, the board's
   * option), 2 on a payload without one.
   * @param {VizPayload} p
   */
  const pictureRatio = (p) =>
    Number(/** @type {TableDerived} */ (p.derived || {}).capture_ratio) || 2;

  /**
   * A picture of a groups table drawn off screen from `payload`: every row,
   * the titles, legend and caption, no tools. For the Image and PowerPoint
   * downloads, at the width the export asks for.
   * @param {VizPayload} payload Its `data` is the table model.
   * @param {number} width CSS px.
   * @param {number} [ratio] Pixel ratio of the PNG.
   * @returns {Promise<VizPicture>}
   */
  function picture(payload, width, ratio) {
    return snapdom().then((snap) => V.capture({ width, height: 20000, draw: (host) => {
      const scope = new V.Scope();
      const el = host.appendChild(root.document.createElement('div'));
      const frame = new T.Frame(el, scope);
      frame.el.classList.add('vz-table-groups-capture');
      const view = new Groups(frame, scope, { still: true });
      view.render(payload);
      return settled(scope)
        .then(() => snap.toCanvas(frame.el, { scale: ratio || pictureRatio(payload), dpr: 1,
                                              embedFonts: true,
                                              backgroundColor: '#ffffff' }))
        .then((/** @type {HTMLCanvasElement} */ canvas) => ({
          png: canvas.toDataURL('image/png'),
          width: Math.ceil(frame.el.offsetWidth), height: Math.ceil(frame.el.offsetHeight)
        }))
        .finally(() => scope.dispose());
    } }));
  }

  /**
   * SnapDOM, fetched from the package's js/vendor/snapdom.js the first time
   * a picture is taken; every picture after that, and one asked for while
   * it is on its way, waits for the same load. A page with no package folder
   * to fetch from fails at once.
   * @returns {Promise<any>}
   */
  const snapdom = () => {
    const ready = () => root.snapdom && typeof root.snapdom.toCanvas === 'function';
    const url = V.lazy.asset('js/vendor/snapdom.js');
    const loaded = ready() || !url ? Promise.resolve() : V.lazy.script(url);
    return loaded.then(() => {
      if (!ready()) throw new Error('snapdom is not loaded');
      return root.snapdom;
    });
  };

  T.layouts.groups = /** @type {TableLayoutClass} */ (/** @type {unknown} */ (Groups));
  T.picture = picture;
})();
// --- table/variables.js
// @ts-check
/**
 * The variables layout: a Table 1, one row per statistic or level of each
 * variable, one column per level of `by`. R builds it as a structured table
 * (sections per variable, the statistics indented), which the records view
 * draws; the cards look draws it as a table too.
 *
 * The variables are the `summaries` entries' columns (R:
 * table_summaries_vars()). The gear picks them as one list of columns, and
 * this file turns the pick back into entries: a numeric column a `dist`
 * entry, any other a `simple` count split by it. Entries kept keep every
 * key they had.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const T = V.table;

  /**
   * The variable an entry summarises, as R reads it.
   * @param {Record<string, unknown>} s
   * @returns {string | null}
   */
  function varOf(s) {
    const v = s.type === 'simple' ? (s.color != null ? s.color : s.col)
      : s.type === 'dist' || s.type === 'field' ? s.col : null;
    return typeof v === 'string' && v ? v : null;
  }

  /**
   * The entries for a pick of variables, in the pick's order.
   * @param {Record<string, unknown>[]} summaries
   * @param {string[]} vars
   * @param {VizPayload['columns']} columns
   */
  function withVars(summaries, vars, columns) {
    const numeric = new Set(columns.filter(V.util.isNumeric)
      .map((c) => c.name));
    return vars.map((v) => summaries.find((s) => varOf(s) === v) ||
      (numeric.has(v) ? { type: 'dist', col: v } : { type: 'simple', color: v }));
  }

  /**
   * The gear's Variables row: a column picker over the entries.
   * @param {HTMLElement} el
   * @param {VizCustomCtx} ctx
   */
  function picker(el, ctx) {
    const C = V.gear.controls;
    const summaries = /** @type {Record<string, unknown>[]} */ (
      Array.isArray(ctx.values.summaries) ? ctx.values.summaries : []);
    const vars = summaries.map(varOf).filter((v) => v != null);
    const by = /** @type {string[]} */ (Array.isArray(ctx.values.by) ? ctx.values.by : []);
    el.textContent = '';
    C.build(el, {
      key: 'summaries', label: 'Variables', value: vars, values: ctx.values,
      arg: { type: { name: 'columns' }, nullable: false,
             gear: { group: 'Variables', control: 'columns', label: 'Variables' } },
      columns: ctx.columns.filter((c) => !by.includes(c.name)),
      required: false, scope: ctx.scope, host: {}, titles: {},
      labelOf: (/** @type {string} */ k) => k,
      set: (/** @type {unknown} */ v) => ctx.set(withVars(summaries, /** @type {string[]} */ (v || []),
                                                         ctx.columns))
    });
    // The layout needs a variable; a row count names none.
    const ui = /** @type {VizUi} */ (root.Blockr);
    if (ui.setRequiredEmpty) ui.setRequiredEmpty(el, !vars.length);
  }

  class Variables extends /** @type {any} */ (T.Records) {
    /** A Table 1 has no card form; it draws as a table. */
    cardsOn() {
      return false;
    }

    /** @returns {TableInfo} */
    info() {
      return { structured: true, finished: false, numbers: false };
    }
  }

  T.layouts.variables = /** @type {TableLayoutClass} */ (/** @type {unknown} */ (Variables));
  T.editors = Object.assign(T.editors || {}, { 'variables.summaries': picker });
  T.variables = { varOf, withVars };
})();
// --- table/view.js
// @ts-check
/**
 * The table block's view, registered with the shared output binding as
 * `blockr.viz.table`. It builds the frame once and hands each payload to the
 * layout named in `config.layout`; a layout switch disposes the old
 * layout's view and everything it set up. Loaded last of the table scripts.
 *
 * The gear (gear.js) is the core gear engine; with it the titles carry
 * core/titles.js's slots. A view built with `gear: false` (a printed page)
 * has neither.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const T = V.table;

  /** How long a layout pick holds the panel when R does not answer it. */
  const HOLD_MS = 20000;

  class TableView {
    /**
     * @param {HTMLElement} el
     * @param {VizScope} scope
     * @param {TableViewOpts} [opts] `gear: false` mounts no gear and no
     *   title slots; `still` draws every row with no tools (a printed page).
     */
    constructor(el, scope, opts = {}) {
      this.scope = scope;
      this.el = el;
      this.still = !!opts.still;
      this.frame = new T.Frame(el, scope);
      /** @type {string | null} */
      this.layout = null;
      /** @type {TableLayoutView | null} */
      this.view = null;
      /** @type {VizScope | null} */
      this.viewScope = null;
      /** A layout pick R has not drawn yet: the panel's scroll at the
       * click, and the layout picked. */
      /** @type {{ sc: Element, top: number, to: string, release: () => void } | null} */
      this.held = null;
      /** The script error the tray last opened for. @type {string | null} */
      this.failed = null;
      this.gear = opts.gear === false ? null
        : T.gear.mount(this.frame, scope, (layout) => this.info(layout), (to) => this.hold(to));
      // The prepare script's controls, under the header.
      this.script = this.gear ? V.scriptBand(this.frame.el, scope, this.gear.engine) : null;
      if (this.script) this.frame.body.before(this.script.el);
    }

    /** @param {VizPayload} p */
    render(p) {
      const layout = String((p.config && p.config.layout) || 'records');
      // A switch takes the old layout's rows away before the new one draws,
      // which would pull a scrolled panel up; it keeps its place, the place
      // at the click when the gear picked the layout. The place comes from
      // the panel's scroll events: reading it here would restyle every row
      // of the old layout first.
      const switching = layout !== this.layout;
      const at = switching ? this.held || this.frame.scrollPlace() : null;
      if (switching) this.switchTo(layout);
      // The layouts paint the titles before the gear's update; their words need the
      // payload's settings, columns and script controls now.
      const engine = this.gear && this.gear.engine;
      if (engine && engine.prime) engine.prime(p);
      if (this.view) {
        this.view.render(p);
        if (this.held && (switching || layout === this.held.to)) this.unhold();
        if (at) {
          const { sc, top } = at;
          sc.scrollTop = top;
          this.scope.frame(() => { sc.scrollTop = top; });
        }
      } else {
        this.frame.empty({ kind: 'error', message: 'The ' + layout + ' layout cannot be drawn here.' });
      }
      // After the layout: the gear's rules read what it now draws.
      if (this.gear) this.gear.update(p);
      if (this.script) this.script.update(/** @type {TableDerived} */ (p.derived || {}).script);
      this.openOnError(/** @type {TableDerived} */ (p.derived || {}));
      this.frame.busyDone();
    }

    /**
     * A block's own gear opens at its `open` tab when the prepare script fails
     * (a table or column it needs is missing), once per failure.
     * @param {TableDerived} d
     */
    openOnError(d) {
      const tab = d.gear && d.gear.open;
      const err = (d.script && d.script.error) || null;
      if (!tab || !this.gear || err === this.failed) return;
      this.failed = err;
      if (!err) return;
      this.gear.engine.showTab(tab);
      this.gear.open();
    }

    /**
     * A layout pick in the gear. The tray shows the new layout's rows at
     * once, and a shorter tray would pull the scrolled panel up before R
     * sends the new layout: the frame keeps its height until it draws.
     * @param {string} to
     */
    hold(to) {
      const h = this.held;
      if (h) {
        h.release();
        h.to = to;
      } else {
        const at = this.frame.scrollPlace();
        if (!at) return;
        if (this.frame.height > 0) this.frame.el.style.minHeight = this.frame.height + 'px';
        this.held = { sc: at.sc, top: at.top, to, release: () => {} };
      }
      // R may refuse the pick; the frame lets go after a while.
      /** @type {NonNullable<TableView['held']>} */ (this.held).release =
        this.scope.timeout(() => this.unhold(), HOLD_MS);
    }

    unhold() {
      if (!this.held) return;
      this.held.release();
      this.held = null;
      this.frame.el.style.minHeight = '';
    }

    /** @param {string} layout */
    switchTo(layout) {
      if (this.viewScope) this.viewScope.dispose();
      this.view = null;
      this.layout = layout;
      this.frame.el.setAttribute('data-layout', layout);
      this.frame.empty(null);
      const Layout = T.layouts[layout];
      if (!Layout) return;
      this.viewScope = new V.Scope();
      this.view = this.viewScope.own(new Layout(this.frame, this.viewScope, { still: this.still }));
    }

    /** @param {string} [layout] Nothing special until R draws it. @returns {TableInfo} */
    info(layout) {
      const v = layout == null || layout === this.layout ? this.view : null;
      return v && v.info ? v.info() : { structured: false, finished: false, numbers: true };
    }

    /** @param {string} message */
    error(message) {
      this.frame.empty({ kind: 'error', message });
    }

    dispose() {
      this.unhold();
      if (this.viewScope) this.viewScope.dispose();
      this.viewScope = null;
      this.view = null;
    }
  }

  T.View = TableView;
  V.binding('blockr.viz.table', 'table', TableView, { busy: 'drawing' });
})();
// --- table/static.js
// @ts-check
/**
 * The table view without the block's output binding: a Web page download, a
 * knitted document, interactive_table(). R writes an element of class
 * `blockr-viz-static` with `data-viz-view="table"` holding the payload as
 * JSON (`<script type="application/json">`), in the shape the block sends.
 *
 * - `renderStatic(el, payload, opts)` draws `payload` on `el`; again on the
 *   same element, it re-renders through the same view, so search, sort,
 *   folds and scroll stay, and `data: null` keeps the rows of the same
 *   `data_rev`. `opts.gear === false`: no gear, plain titles.
 *   `opts.still`: every row and no tools, as for a printed page.
 * - Every such element is drawn on DOM ready and when one is added later
 *   (renderUI). No gear unless `data-viz-gear="on"`;
 *   `data-viz-still="off"` keeps the row window and the tools.
 * - R's `blockr-viz-static` message `{id, payload}` re-renders the element
 *   with that id.
 *
 * A click goes out as `<id>_select`, as from the block; without Shiny it
 * goes nowhere. The page's tables live as long as the page, under one Scope.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const T = V.table;
  const SEL = '.blockr-viz-static[data-viz-view="table"]';

  const page = new V.Scope();
  /** @type {WeakMap<Element, { view: InstanceType<VizViewClass>, scope: VizScope,
   *   rows?: { rev: number, data: unknown } }>} */
  const mounts = new WeakMap();

  /**
   * Draw `payload` on `el`, or redraw it there.
   * @param {HTMLElement} el
   * @param {VizPayload} payload
   * @param {TableViewOpts} [opts]
   */
  function renderStatic(el, payload, opts = {}) {
    let m = mounts.get(el);
    if (!m) {
      const scope = page.own(new V.Scope());
      const View = /** @type {new (el: HTMLElement, scope: VizScope, o: TableViewOpts) => InstanceType<VizViewClass>} */ (
        /** @type {unknown} */ (T.View));
      m = { scope, view: scope.own(new View(el, scope, opts)) };
      mounts.set(el, m);
      scope.own({ dispose: () => mounts.delete(el) });
    }
    // No rows: the rows of the same data revision, as the binding keeps
    // them; a payload whose rows the element does not hold draws nothing.
    if (payload.data != null) {
      m.rows = { rev: payload.data_rev, data: payload.data };
    } else if (m.rows && m.rows.rev === payload.data_rev) {
      payload = Object.assign({}, payload, { data: m.rows.data });
    } else {
      return;
    }
    m.view.render(payload);
  }

  /**
   * The options an element asks for.
   * @param {Element} el
   * @returns {TableViewOpts}
   */
  const optsOf = (el) => ({ gear: el.getAttribute('data-viz-gear') === 'on',
                            still: el.getAttribute('data-viz-still') !== 'off' });

  /** @param {Element} el */
  function fromJson(el) {
    const json = el.querySelector(':scope > script[type="application/json"]');
    if (mounts.has(el) || !json) return;
    let p;
    try {
      p = /** @type {VizPayload} */ (JSON.parse(json.textContent || '{}'));
    } catch (e) {
      return;
    }
    renderStatic(/** @type {HTMLElement} */ (el), p, optsOf(el));
  }

  /**
   * Draw every table under `node` not drawn yet.
   * @param {ParentNode} [node]
   */
  function mount(node) {
    const at = node || root.document;
    if (at instanceof root.Element && at.matches(SEL)) fromJson(at);
    for (const el of Array.from(at.querySelectorAll(SEL))) fromJson(/** @type {Element} */ (el));
  }

  function start() {
    mount();
    page.mutate(root.document.body, (/** @type {MutationRecord[]} */ recs) => {
      for (const r of recs) {
        for (const n of Array.from(r.addedNodes)) if (n instanceof root.Element) mount(/** @type {Element} */ (n));
      }
    }, { childList: true, subtree: true });
  }

  if (root.document.readyState === 'loading') page.on(root.document, 'DOMContentLoaded', start);
  else start();

  const shiny = root.Shiny;
  if (shiny && typeof shiny.addCustomMessageHandler === 'function') {
    shiny.addCustomMessageHandler('blockr-viz-static',
      (/** @type {{ id: string, payload: VizPayload }} */ msg) => {
        const el = root.document.getElementById(msg.id);
        if (el) renderStatic(el, msg.payload, optsOf(el));
      });
  }

  T.renderStatic = renderStatic;
  T.mount = mount;
})();
//# sourceMappingURL=table.js.map
