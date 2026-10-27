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

  /** Above this many items only a window of them is in the DOM. */
  const VIRT_MIN = 400;
  /** Items drawn beyond each edge of the visible part. */
  const OVERSCAN = 40;
  /** Row height until a drawn row is measured. */
  const ROW_H = 30;

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
    const base = indent && n ? Math.min(...indent) : 0;
    const levels = secs.length ? Math.max(...secs.map((x) => x.level)) : 0;
    return { blk, path, indent, toggle, base, levels };
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
      this.host.className = 'dt-table-slot';
      box.appendChild(this.host);
      /** @type {TableGridHooks} */
      this.hooks = hooks || {};
      /** @type {HTMLTableElement | null} */
      this.table = null;
      /** @type {TableModel | null} */
      this.model = null;
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
      scope.on(box, 'scroll', () => {
        if (pending) return;
        pending = true;
        scope.frame(() => {
          pending = false;
          this._scrolled();
        });
      }, { passive: true });
    }

    /**
     * Draw a new model. Sort and folds start over; the search stays.
     * @param {TableModel} model
     * @param {TableGridFlags} [flags]
     */
    set(model, flags) {
      this.model = model;
      this.flags = { sortable: !flags || flags.sortable !== false,
                     collapsible: !flags || flags.collapsible !== false };
      this.sortCol = null;
      this.sortDir = 0;
      this._st = structure(model, this.flags.collapsible);
      this._text = null;
      this._keys = new Map();
      this._measured = false;
      this.shut = new Set();
      this.shutRows = new Set();
      if (model.collapsed && this.flags.collapsible) this._shutAll(true);
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

    /** Rows that pass the search, of all rows. */
    counts() {
      return { shown: this._shown, total: this.model ? this.model.n : 0 };
    }

    /** The drawn item list. */
    items() {
      return this._items;
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
      const f = { sortable: this.flags.sortable, foldAll: this.folds() };
      const widths = m.widths || null;
      const cols = widths
        ? '<colgroup>' + widths.map((w) => '<col style="width:' + Number(w) + 'px">').join('') +
          '</colgroup>'
        : '';
      const head = m.head.map((row, r) =>
        '<tr>' + row.map((h, c) => H.th(m, r, c, f)).join('') + '</tr>').join('');
      this.host.hidden = false;
      this.host.innerHTML = '<table class="blockr-table' + (widths ? ' dt-fixed' : '') + '">' +
        cols + '<thead>' + head + '</thead><tbody></tbody></table>';
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
      } else {
        view = Array.from({ length: m.n }, (_, i) => i);
      }
      this._shown = view.length;
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
      return view.slice().sort((a, b) => {
        if (st.blk[a] !== st.blk[b]) return st.blk[a] - st.blk[b];
        const na = num[a];
        const nb = num[b];
        return dir * (na !== null && nb !== null ? na - nb : str[a].localeCompare(str[b]));
      });
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
      let msg = this.host.querySelector('.blockr-search-empty');
      if (empty && !msg) {
        msg = document.createElement('div');
        msg.className = 'blockr-search-empty';
        msg.setAttribute('role', 'status');
        msg.textContent = 'No matching rows';
        this.host.appendChild(msg);
      } else if (!empty && msg) {
        msg.remove();
      }
      if (this.hooks.drawn) this.hooks.drawn();
    }

    /** Draw the items around the scroll position, between two spacers. */
    _window() {
      const tbody = /** @type {HTMLTableElement} */ (this.table).tBodies[0];
      const items = this._items;
      const n = items.length;
      let start = 0;
      let end = n;
      if (n > VIRT_MIN) {
        const vis = Math.max(1, Math.ceil((this.box.clientHeight || 600) / this.rowH));
        start = Math.max(0, Math.floor(this.box.scrollTop / this.rowH) - OVERSCAN);
        end = Math.min(n, start + vis + 2 * OVERSCAN);
      }
      const ncol = /** @type {TableModel} */ (this.model).cols.length || 1;
      const spacer = (/** @type {number} */ k) => '<tr class="dt-vspacer" aria-hidden="true">' +
        '<td colspan="' + ncol + '" style="padding:0;border:0;height:' +
        Math.round(k * this.rowH) + 'px"></td></tr>';
      /** @type {string[]} */
      const parts = [];
      if (start > 0) parts.push(spacer(start));
      for (let k = start; k < end; k++) parts.push(this._itemHtml(k));
      if (end < n) parts.push(spacer(n - end));
      tbody.innerHTML = parts.join('');
      this.win = [start, end];
      if (n > VIRT_MIN && !this._measured) {
        this._measured = true;
        const r0 = /** @type {HTMLElement | null} */ (tbody.querySelector('tr.blockr-data-row'));
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
      const first = Math.floor(this.box.scrollTop / this.rowH);
      const last = first + Math.ceil(this.box.clientHeight / this.rowH);
      const [s, e] = this.win;
      if ((first - s < OVERSCAN / 2 && s > 0) || (e - last < OVERSCAN / 2 && e < n)) {
        this._window();
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
      const i = /** @type {number} */ (it.i);
      const next = this._items[k + 1];
      const last = !next || (next.s != null && m.sections != null &&
                             m.sections[next.s].level === 1);
      return H.row(m, /** @type {TableStructure} */ (this._st), i,
                   { last, shut: this.shutRows.has(i), extra: h.rowClass ? h.rowClass(i) : '' });
    }

    /** The fold-all button follows the folds: open all, or fold all. */
    _foldAllButton() {
      const btn = this.host.querySelector('.dt-foldall');
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
      if (t.closest('.dt-foldall')) {
        this.foldAll(this.shut.size === 0 && this.shutRows.size === 0);
        return;
      }
      const ib = t.closest('.blockr-indent-btn');
      if (ib) {
        const i = Number((/** @type {Element} */ (ib.closest('tr'))).getAttribute('data-i'));
        if (this.shutRows.has(i)) this.shutRows.delete(i);
        else this.shutRows.add(i);
        this.draw();
        return;
      }
      const sec = t.closest('tr.blockr-section-header');
      if (sec && this.flags.collapsible) {
        const s = Number(sec.getAttribute('data-s'));
        const keyed = drills && !!(/** @type {TableSection[]} */ (this.model.sections)[s].keys);
        if (e.altKey) return this.foldAll(!this.shut.has(s));
        if (!(keyed && t.closest('.blockr-section-value'))) return this.fold(s, !this.shut.has(s));
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

  Object.assign(V.table, { Grid, parseNum, rowText });
})();
