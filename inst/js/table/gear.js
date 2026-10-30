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
   * @param {string} key
   * @param {Record<string, unknown>} v
   * @param {TableInfo} info
   */
  function visible(key, v, info) {
    if (key === 'drill_target' || key === 'drill_table') return v.drill != null && v.drill !== '';
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
