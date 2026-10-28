// @ts-check
/**
 * The groups layout's Columns row in the gear: one entry per summary, its
 * fields built from the spec (`summaries`, one variant per `type`) with the
 * gear's own controls, so a field reads and edits as every other gear row.
 * An entry folds to its title; it moves up or down, or goes; the Add menu
 * appends an entry of any type. Every edit sends the whole list.
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
   * One entry: its head, which opens it, and its fields with move and
   * remove.
   * @param {Entry[]} list
   * @param {number} k
   * @param {Record<string, any>} variants
   * @param {VizCustomCtx} ctx
   * @param {Set<number>} open
   */
  function entry(list, k, variants, ctx, open) {
    const s = list[k];
    const send = (/** @type {Entry[]} */ next) => ctx.set(next);
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
    list.forEach((_, k) => el.appendChild(entry(list, k, variants, ctx, open)));
    const add = el.appendChild(document.createElement('div'));
    add.className = 'vz-table-sum-addcol';
    addMenu(add, Object.keys(variants), (t) => {
      open.add(list.length);
      ctx.set(list.concat([{ type: t }]));
    }, ctx.scope);
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
