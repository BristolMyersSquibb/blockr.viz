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
  const MODES = [['diverging', 'Diverging'], ['sequential', 'Sequential'], ['bar', 'Bar']];
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
    pick(mode, MODES.map(([value, label]) => ({ value, label })), String(rule.mode || ''),
         { label: 'Mode' }, (v) => put(Object.assign({}, rule, { mode: v })), scope);
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

  /**
   * Mount the gear, or nothing without the engine.
   * @param {TableFrame} frame
   * @param {VizScope} scope
   * @param {() => TableInfo} info What the current layout draws.
   * @returns {TableGearHandle | null}
   */
  function mount(frame, scope, info) {
    /** What R resolved with the last payload: the drill targets. */
    /** @type {TableDerived} */
    let derived = {};
    /** @type {VizGearHost} */
    const host = {
      // The blocks a click can be sent to, as the chart offers them.
      choices: (key) => key === 'drill_target'
        ? (/** @type {VizChoice[]} */ ([{ value: '', label: 'None' }])).concat(derived.ctrl_choices || [])
        : null,
      visible: (key, v) => visible(key, v, info()),
      label: (key, v) => key === 'by' ? BY_LABEL[/** @type {'records'} */ (v.layout)] : null,
      // What an empty picker means.
      placeholder: (key, v) => key === 'columns' ? 'All columns'
        : key === 'by' && v.layout === 'records' ? 'All' : null,
      // A layout's own editor for a key ("variables.summaries"), else the
      // key's ("shadings").
      custom: (key, el, ctx) => {
        const ed = V.table.editors[ctx.values.layout + '.' + key] || V.table.editors[key];
        if (ed) ed(el, ctx);
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

  V.table.gear = { mount, visible, shadings };
  V.table.ui = { pick, check, button };
  V.table.editors = Object.assign(V.table.editors || {}, { shadings });
})();
