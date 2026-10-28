// @ts-check
/**
 * The hover card of the groups marks, in the chart's card style (the
 * .dd-tt-* rules): the row as the headline, the column's context under it,
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
  const sw = (/** @type {unknown} */ c) => c ? '<span class="dd-tt-sw" style="background:' + esc(c) + '"></span>' : '';
  const head = (/** @type {unknown} */ t, /** @type {unknown} */ c = null) =>
    '<div class="dd-tt-head">' + sw(c) + '<span>' + esc(t) + '</span></div>';
  /** @param {unknown} label @param {unknown} value @param {unknown} [c] @param {boolean | null} [hit] */
  const row = (label, value, c, hit) => '<div class="dd-tt-row' +
    (hit === true ? ' is-hit' : hit === false ? ' is-dim' : '') + '">' + sw(c) +
    '<span class="dd-tt-label">' + esc(label) + '</span><span class="dd-tt-value">' + esc(value) +
    '</span></div>';
  const note = (/** @type {unknown} */ t) => '<div class="dd-tt-row dd-tt-label">' + esc(t) + '</div>';
  const SEP = '<div class="dd-tt-sep"></div>';
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
      const mark = t.closest('.vz-table-lane-wh, .vz-table-lane-cap, .vz-table-lane-box, .vz-table-lane-med, .vz-table-lane-fence, .vz-table-lane-rng, .vz-table-lane-ctr');
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
      const seg = t.closest('.vz-table-lane-seg');
      if (seg) return { text: seg.getAttribute('data-tip') || '', color: /** @type {HTMLElement} */ (seg).style.background };
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

  /** @param {{ text: string, color?: string }} l */
  function laneCard(l) {
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
      return l && l.text ? laneCard(l) : '';
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
