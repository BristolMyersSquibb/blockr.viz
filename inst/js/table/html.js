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

  const INDENT_PX = 16;
  const CHEV = '<svg class="blockr-chev" viewBox="0 0 24 24" fill="none" ' +
    'stroke="currentColor" stroke-width="2.4" stroke-linecap="round" ' +
    'stroke-linejoin="round" aria-hidden="true"><path d="M6 9l6 6 6-6"/></svg>';

  /**
   * The text of a header cell: name lines, then the quiet line and the sort
   * arrow in one row under them.
   * @param {TableHeadCell} h
   * @param {boolean} sortable
   */
  function headText(h, sortable) {
    const name = '<span class="blockr-col-name arm__name">' +
      String(h.text == null ? '' : h.text).split('\n').map(esc).join('<br>') + '</span>';
    const icon = sortable ? '<span class="blockr-sort-icon"></span>' : '';
    if (h.sub) {
      return name + '<div class="dt-th-subrow"><span class="blockr-col-label arm__n">' +
        esc(h.sub) + '</span>' + icon + '</div>';
    }
    return '<div class="dt-th-namerow">' + name + icon + '</div>';
  }

  /**
   * One header cell; `r` and `c` find it again in `model.head`.
   * @param {TableModel} model
   * @param {number} r
   * @param {number} c
   * @param {{ sortable: boolean, foldAll: boolean }} f
   */
  function th(model, r, c, f) {
    const h = model.head[r][c];
    const col = h.col != null ? model.cols[h.col] : null;
    const sortable = f.sortable && h.col != null && (h.span || 1) === 1;
    const cls = [h.stub ? 'blockr-stub-header' : 'blockr-col-header',
      h.stub ? '' : (h.col != null ? 'leaf' : 'group'),
      !h.stub && h.col == null && r > 0 ? 'group-inner' : '',
      col ? (col.num ? 'dt-col-num' : 'dt-col-txt') : '',
      sortable ? 'blockr-sortable' : '', h.keys ? 'dt-keyed' : ''];
    let inner = headText(h, sortable);
    if (h.stub && f.foldAll) {
      cls.push('dt-has-foldall');
      inner = '<div class="dt-foldall-wrap"><button type="button" class="dt-foldall" ' +
        'aria-expanded="true" aria-label="Collapse all groups">' + CHEV +
        (h.text ? '' : '<span class="dt-foldall-all">All</span>') + '</button>' +
        (h.text ? '<span class="dt-stub-title">' + inner + '</span>' : '') + '</div>';
    }
    return '<th class="' + cls.filter(Boolean).join(' ') + '" data-h="' + r + ',' + c + '"' +
      ((h.span || 1) > 1 ? ' colspan="' + h.span + '"' : '') +
      ((h.rows || 1) > 1 ? ' rowspan="' + h.rows + '"' : '') +
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
    const base = 'blockr-data ' + (c.num ? 'dt-num' : 'dt-txt');
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
        : '<td class="' + base + ' blockr-dash">&mdash;</td>';
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
    const label = sec.label ? '<span class="blockr-section-label">' + esc(sec.label + ': ') +
      '</span>' : '';
    const btn = o.fold
      ? '<button class="blockr-section-btn" type="button" aria-expanded="' + !o.shut + '">' + CHEV
      : '<span class="blockr-section-btn blockr-section-btn-static">' +
        '<span class="blockr-chev" aria-hidden="true"></span>';
    return '<tr class="blockr-section-header' + (o.shut ? ' collapsed' : '') +
      (sec.keys ? ' dt-keyed' : '') + (o.extra ? ' ' + o.extra : '') + '" data-s="' + s +
      '" data-level="' + sec.level + '"><td class="blockr-section-cell level-' + sec.level +
      ' blockr-section-nest" colspan="' + (m.cols.length || 1) + '">' + btn + label +
      '<span class="blockr-section-value">' + esc(sec.value) + '</span>' +
      (o.fold ? '</button>' : '</span>') + '</td></tr>';
  }

  /**
   * The stub cell: indented under its sections and its parent rows, with
   * the fold button of an indented group and the row tint.
   * @param {TableModel} m
   * @param {TableStructure} st
   * @param {number} i
   * @param {boolean} shut
   */
  function stub(m, st, i, shut) {
    const c = m.cols[/** @type {number} */ (m.stub)];
    const pad = (st.indent ? st.indent[i] - st.base : 0) + Math.max(0, st.levels - 1);
    /** @type {string[]} */
    const style = [];
    if (pad > 0) {
      style.push('padding-left:calc(' + (24 + pad * INDENT_PX) +
                 'px + var(--blockr-card-inset, 0px))');
    }
    const tint = m.rows && m.rows.tint ? m.rows.tint[i] : null;
    if (tint) style.push('box-shadow:inset 3px 0 0 0 ' + tint);
    const d = c.disp[i];
    const btn = st.toggle[i]
      ? '<button class="blockr-indent-btn" type="button" tabindex="-1" aria-expanded="' +
        !shut + '">' + CHEV + '</button>'
      : '';
    return '<td class="blockr-stub' + (st.toggle[i] ? ' blockr-has-toggle' : '') +
      (tint ? ' blockr-row-bar' : '') + '"' +
      (style.length ? ' style="' + esc(style.join(';')) + '"' : '') + '>' + btn +
      (d == null ? '&mdash;' : esc(d)) + '</td>';
  }

  /**
   * One row.
   * @param {TableModel} m
   * @param {TableStructure} st
   * @param {number} i
   * @param {{ last: boolean, shut: boolean, extra: string }} o
   */
  function row(m, st, i, o) {
    const r = m.rows || {};
    const cls = ['blockr-data-row'];
    if (r.strong && r.strong[i]) cls.push('blockr-bold');
    if (r.emph && r.emph[i]) cls.push('blockr-italic');
    if (o.last) cls.push('blockr-group-last');
    if (st.toggle[i]) cls.push('blockr-indent-toggle');
    if (st.toggle[i] && o.shut) cls.push('collapsed');
    if (o.extra) cls.push(o.extra);
    const ind = st.indent ? st.indent[i] : 0;
    let out = '<tr class="' + cls.join(' ') + '" data-i="' + i + '" data-indent="' + ind + '">';
    m.cols.forEach((c, j) => {
      out += j === m.stub ? stub(m, st, i, o.shut) : td(c, i);
    });
    return out + '</tr>';
  }

  V.table = V.table || /** @type {VizTableNamespace} */ ({ layouts: {} });
  V.table.html = { th, td, section, row };
})();
