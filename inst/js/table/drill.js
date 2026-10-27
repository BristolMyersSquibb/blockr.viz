// @ts-check
/**
 * The drill on a Grid: a click on a row, a cell, a keyed header or a keyed
 * section value sends a selection (design.md section 2) to R as
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

  const FLASH = ['dt-flash-row', 'dt-flash-col', 'dt-flash-colhead', 'dt-flash-cross'];
  const HOT = ['dt-hot-row', 'dt-hot-col', 'dt-hot-cell', 'dt-hot-soft'];

  /** Distinguishes two clicks on the same thing; counts for the page. */
  let nonce = 0;

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

  /**
   * Whether `keys` lies inside `claim`: every column of the claim is in the
   * keys with the same values. A spanner's claim covers its leaves.
   * @param {TableClaim} claim
   * @param {TableClaim | null | undefined} keys
   */
  function covers(claim, keys) {
    const k = Object.keys(claim);
    return !!keys && k.length > 0 && k.every((c) => sameValues(claim[c], keys[c]));
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
    table.querySelectorAll('tbody tr.blockr-data-row').forEach((tr) => {
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
    if (at.matches('.blockr-section-value')) {
      const sh = at.closest('tr.blockr-section-header');
      if (sh && sh.classList.contains('dt-keyed')) hot(sh, ['dt-hot-row']);
      return;
    }
    const hit = grid.headAt(at);
    if (hit) {
      const ce = colEls(grid, hit.h.keys);
      ce.head.concat(ce.cells).forEach((c) => hot(c, ['dt-hot-col']));
      return;
    }
    const tr = at.closest('tr.blockr-data-row');
    if (!tr || tr.classList.contains('dt-row-nodrill')) return;
    const j = /** @type {HTMLTableCellElement} */ (at).cellIndex;
    const keys = j !== m.stub && m.cols[j] ? m.cols[j].keys : null;
    if (!keys) return hot(tr, ['dt-hot-row']);
    const ce = colEls(grid, keys);
    hot(tr, ['dt-hot-row', 'dt-hot-soft']);
    ce.head.concat(ce.cells).forEach((c) => { if (c !== at) hot(c, ['dt-hot-col', 'dt-hot-soft']); });
    hot(at, ['dt-hot-cell']);
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
      const at = t && t.closest ? (t.closest('.blockr-section-value') || t.closest('td, th')) : null;
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
    const sec = t.closest('tr.blockr-section-header');
    if (sec) {
      const s = m.sections ? m.sections[Number(sec.getAttribute('data-s'))] : null;
      return s && s.keys && t.closest('.blockr-section-value') ? { sel: selection(s.keys), tr: sec } : null;
    }
    const tr = t.closest('tr.blockr-data-row');
    const claim = tr ? rowClaim(m, Number(tr.getAttribute('data-i'))) : null;
    if (!tr || !claim) return null;
    const td = /** @type {HTMLTableCellElement | null} */ (t.closest('td'));
    const col = td && td.cellIndex !== m.stub ? m.cols[td.cellIndex] : null;
    return { sel: selection(claim, col && col.keys), tr, cell: td };
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
      if (!rowClaim(m, i)) return 'dt-row-nodrill';
      return this.rows().has(i) ? 'dt-row-active' : '';
    }

    /** @param {number} s */
    secClass(s) {
      const sel = this.ctx.selection();
      const m = this.grid.model;
      const sec = m && m.sections ? m.sections[s] : null;
      return sel && !this.ctx.transient() && sec && sec.keys && !sel.columns &&
        equal(sec.keys, sel.filters) ? 'dt-row-active' : '';
    }

    /** Light the claimed columns; runs after every draw. */
    paint() {
      const table = this.grid.table;
      if (!table) return;
      table.querySelectorAll('.dt-col-active').forEach((el) => el.classList.remove('dt-col-active'));
      const sel = this.ctx.selection();
      if (!sel || this.ctx.transient()) return;
      const e = colEls(this.grid, sel.columns);
      e.head.concat(e.cells).forEach((el) => el.classList.add('dt-col-active'));
    }

    /**
     * Send a click's claim. Kept, it toggles: the same claim again clears.
     * @param {TableSelection} sel
     * @param {{ tr?: Element | null, cell?: Element | null }} at
     */
    choose(sel, at) {
      const ctx = this.ctx;
      if (ctx.transient()) {
        V.util.send(this.frame.id, 'select', { selection: sel, nonce: ++nonce });
        ctx.sent(sel);
        /** @type {Array<{ el: Element, cls: string }>} */
        const parts = at.tr ? [{ el: at.tr, cls: 'dt-flash-row' }] : [];
        const e = colEls(this.grid, sel.columns);
        e.head.forEach((el) => parts.push({ el, cls: 'dt-flash-colhead' }));
        e.cells.forEach((el) => parts.push({ el, cls: el === at.cell ? 'dt-flash-cross' : 'dt-flash-col' }));
        flash(this.frame.el, this.scope, parts);
        return;
      }
      const next = same(sel, ctx.selection()) ? null : sel;
      V.util.send(this.frame.id, 'select', { selection: next, nonce: ++nonce });
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
