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
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  const TYPES = { simple: 'Summary', dist: 'Distribution', field: 'Values', sparkline: 'Sparkline',
                  spans: 'Spans', pair: 'Pair', expr: 'Expression', custom: 'Custom' };

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

  /** The gear's drawing for each type (core/gear/tiles.js). */
  const ICON = { simple: 'type:bar', dist: 'type:boxplot', sparkline: 'type:line',
                 spans: 'type:gantt', pair: 'type:pointrange' };
  const FUNC = { count_distinct: 'distinct count', mean: 'mean', median: 'median', sum: 'sum',
                 min: 'min', max: 'max', sd: 'sd' };

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
    const icons = /** @type {any} */ (V).gear && /** @type {any} */ (V).gear.icons;
    const icon = icons && icons[ICON[/** @type {'simple'} */ (s.type)]];
    b.textContent = '';
    const part = (/** @type {string} */ cls, /** @type {string} */ text) => {
      const x = b.appendChild(document.createElement('span'));
      x.className = cls;
      x.textContent = text;
      return x;
    };
    part('lane-sum-icon', '').innerHTML = icon || '';
    part('lane-sum-name', title(s));
    part('lane-sum-desc', describe(s));
    part('lane-sum-chev', '›');
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
    const b = V.table.ui.button('blockr-btn--quiet blockr-btn--xs lane-sum-ctl ' + cls, text, label,
                                fn, scope);
    b.setAttribute('data-blockr-tooltip', label);
    return b;
  }

  /**
   * The fields of entry `k`, built with the gear's controls.
   * @param {HTMLElement} body
   * @param {Entry} s
   * @param {Record<string, any>} variant
   * @param {TableGearCtx} ctx
   * @param {(next: Entry) => void} put
   */
  function fields(body, s, variant, ctx, put) {
    const C = /** @type {any} */ (V).gear.controls;
    const keys = Object.keys(variant).filter((f) => variant[f].gear && shows(s, f));
    for (const f of keys.filter((x) => x !== 'name').concat(keys.includes('name') ? ['name'] : [])) {
      const arg = variant[f];
      const el = document.createElement('div');
      el.className = 'dd-form-row lane-sum-field';
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
        scope: ctx.scope, host: {}, titles: {}, labelOf: (/** @type {string} */ x) => x,
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
   * @param {TableGearCtx} ctx
   * @param {Set<number>} open
   */
  function entry(list, k, variants, ctx, open) {
    const s = list[k];
    const send = (/** @type {Entry[]} */ next) => ctx.set(next);
    const el = document.createElement('div');
    el.className = 'lane-sum-row' + (open.has(k) ? ' is-open' : '');
    const head = document.createElement('div');
    head.className = 'lane-sum-head';
    const name = button('lane-sum-toggle', '', 'Edit ' + title(s), () => {
      if (open.has(k)) open.delete(k);
      else open.add(k);
      el.classList.toggle('is-open');
      body.hidden = !open.has(k);
    }, ctx.scope);
    name.removeAttribute('data-blockr-tooltip');
    headText(name, s);
    const move = (/** @type {number} */ d) => {
      const next = list.slice();
      next.splice(k + d, 0, next.splice(k, 1)[0]);
      send(next);
    };
    head.append(name);
    el.appendChild(head);
    const body = document.createElement('div');
    body.className = 'lane-sum-body';
    body.hidden = !open.has(k);
    const variant = variants[/** @type {string} */ (s.type)];
    if (variant) {
      fields(body, s, variant, ctx, (next) => send(list.map((x, j) => (j === k ? next : x))));
    }
    const acts = body.appendChild(document.createElement('div'));
    acts.className = 'lane-sum-actions';
    if (k > 0) acts.appendChild(button('lane-sum-up', 'Move up', 'Move up', () => move(-1), ctx.scope));
    if (k < list.length - 1) {
      acts.appendChild(button('lane-sum-down', 'Move down', 'Move down', () => move(1), ctx.scope));
    }
    acts.appendChild(button('lane-sum-rm', 'Remove', 'Remove this column',
                            () => send(list.filter((_, j) => j !== k)), ctx.scope));
    el.appendChild(body);
    return el;
  }

  /** Entries open in the tray, per element, across re-renders. */
  const opened = new WeakMap();

  /**
   * The Columns row.
   * @param {HTMLElement} el
   * @param {TableGearCtx} ctx
   */
  function editor(el, ctx) {
    const spec = /** @type {any} */ (V.spec.table);
    const variants = spec && spec.summaries && spec.summaries.type.variants;
    if (!variants || !(/** @type {any} */ (V).gear && /** @type {any} */ (V).gear.controls)) return;
    const list = /** @type {Entry[]} */ (Array.isArray(ctx.values.summaries) ? ctx.values.summaries : []);
    const box = /** @type {Element} */ (el.closest('.blockr-settings') || el);
    let open = opened.get(box);
    if (!open) {
      // A lone column opens: it is what the reader came to change.
      open = new Set(list.length === 1 ? [0] : []);
      opened.set(box, open);
    }
    el.textContent = '';
    el.classList.add('lane-summaries');
    list.forEach((_, k) => el.appendChild(entry(list, k, variants, ctx, open)));
    const add = el.appendChild(document.createElement('div'));
    add.className = 'lane-sum-addcol';
    V.table.ui.pick(add, Object.keys(variants).map((t) => ({
      value: t, label: TYPES[/** @type {'simple'} */ (t)] || t
    })), '', { label: 'Add a column', placeholder: '+ Add a column' }, (t) => {
      open.add(list.length);
      ctx.set(list.concat([{ type: t }]));
    }, ctx.scope);
  }

  V.table.editors = Object.assign(V.table.editors || {}, { 'groups.summaries': editor });
  V.table.summaries = { shows, title, describe };
})();
