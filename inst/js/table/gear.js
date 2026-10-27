// @ts-check
/**
 * The table's gear: the core gear engine (`Blockr.viz.gear`, gear-api.md)
 * in a tray under the frame's header, opened by the gear button with
 * blockr.ui's `Blockr.gearTray`. The engine builds every row from the spec;
 * this file adds what the spec cannot say:
 *
 * - which rows a layout shows for its input (`visible`): a finished input
 *   picks no columns, the shadings and row colour need a flat table,
 *   folding needs sections, `missing` needs nesting;
 * - the `by` label, which differs per layout;
 * - the editors of keys the engine has no control for (`custom`): the
 *   shading rules here, a layout's own in its file (`Blockr.viz.table.editors`).
 *
 * Without the engine the view has no gear.
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

  /**
   * One shading rule: its mode and the numeric columns it paints (none:
   * every numeric column no earlier rule names).
   * @param {Record<string, unknown>} rule
   * @param {VizPayload['columns']} numeric
   * @param {(next: Record<string, unknown> | null) => void} put
   */
  function ruleEl(rule, numeric, put) {
    const el = document.createElement('div');
    el.className = 'vz-shade-rule';
    const mode = document.createElement('select');
    mode.className = 'vz-shade-mode';
    mode.setAttribute('aria-label', 'Mode');
    mode.innerHTML = MODES.map(([value, label]) => '<option value="' + value + '"' +
      (rule.mode === value ? ' selected' : '') + '>' + label + '</option>').join('');
    mode.onchange = () => put(Object.assign({}, rule, { mode: mode.value }));
    el.appendChild(mode);
    const on = new Set(/** @type {string[]} */ (rule.columns || []));
    const cols = document.createElement('div');
    cols.className = 'vz-shade-cols';
    for (const c of numeric) {
      const lab = document.createElement('label');
      const box = document.createElement('input');
      box.type = 'checkbox';
      box.checked = on.has(c.name);
      box.onchange = () => {
        if (box.checked) on.add(c.name);
        else on.delete(c.name);
        const picked = numeric.map((x) => x.name).filter((n) => on.has(n));
        put(Object.assign({}, rule, { columns: picked.length ? picked : null }));
      };
      lab.append(box, ' ' + (c.label || c.name));
      cols.appendChild(lab);
    }
    el.appendChild(cols);
    const del = document.createElement('button');
    del.type = 'button';
    del.className = 'vz-shade-remove';
    del.setAttribute('aria-label', 'Remove rule');
    del.textContent = '×';
    del.onclick = () => put(null);
    el.appendChild(del);
    return el;
  }

  /**
   * The shading rules: one row per rule, and a button that adds one. Every
   * key of a rule goes back as it came, `domain` included.
   * @param {HTMLElement} el
   * @param {TableGearCtx} ctx
   */
  function shadings(el, ctx) {
    const rules = /** @type {Record<string, unknown>[]} */ (
      Array.isArray(ctx.values.shadings) ? ctx.values.shadings : []);
    const numeric = ctx.columns.filter((c) => /^(numeric|integer|double)$/.test(String(c.class)));
    el.textContent = '';
    rules.forEach((rule, k) => {
      el.appendChild(ruleEl(rule, numeric, (next) => {
        const out = rules.slice();
        if (next) out[k] = next;
        else out.splice(k, 1);
        ctx.set(out);
      }));
    });
    const add = document.createElement('button');
    add.type = 'button';
    add.className = 'vz-shade-add';
    add.textContent = rules.length ? 'Add a rule' : 'Shade cells';
    add.onclick = () => ctx.set(rules.concat([{ mode: 'diverging', columns: null, domain: null }]));
    el.appendChild(add);
  }

  /**
   * The gear button and its tray, in the frame's header. Fallback without
   * blockr.ui: the button toggles the tray.
   * @param {TableFrame} frame
   * @param {VizScope} scope
   */
  function tray(frame, scope) {
    const btn = document.createElement('button');
    btn.type = 'button';
    btn.className = 'blockr-gear-btn';
    btn.innerHTML = root.Blockr.icons ? root.Blockr.icons.gear : '⚙';
    const band = document.createElement('div');
    band.className = 'blockr-settings blockr-settings--beak';
    frame.head.append(btn, band);
    const B = root.Blockr;
    const handle = B.gearTray ? B.gearTray(band, btn, { label: 'Table settings' }) : {
      open: false,
      /** @param {boolean} on */
      set(on) {
        this.open = on;
        band.classList.toggle('blockr-settings--open', on);
        btn.setAttribute('aria-expanded', String(on));
      }
    };
    if (!B.gearTray) scope.on(btn, 'click', () => handle.set(!handle.open));
    // The header's layout follows the tray: open, the gear sits alone on top.
    const sync = () => frame.head.classList.toggle('vz-head--open',
                                                   btn.getAttribute('aria-expanded') === 'true');
    scope.mutate(btn, sync, { attributes: true, attributeFilter: ['aria-expanded'] });
    // A tray left open is a layer on blockr.ui's dismiss stack: close it.
    scope.own({ dispose: () => handle.set(false) });
    return { band, open: () => handle.set(true) };
  }

  /**
   * Mount the gear, or nothing without the engine.
   * @param {TableFrame} frame
   * @param {VizScope} scope
   * @param {() => TableInfo} info What the current layout draws.
   * @returns {TableGearHandle | null}
   */
  function mount(frame, scope, info) {
    const G = /** @type {any} */ (V).gear;
    if (!G || typeof G.mount !== 'function') return null;
    const t = tray(frame, scope);
    /** @type {TableGearHost} */
    const host = {
      visible: (key, v) => visible(key, v, info()),
      label: (key, v) => key === 'by' ? BY_LABEL[/** @type {'records'} */ (v.layout)] : null,
      // A layout's own editor for a key ("variables.summaries"), else the
      // key's ("shadings").
      custom: (key, el, ctx) => {
        const ed = V.table.editors[ctx.values.layout + '.' + key] || V.table.editors[key];
        if (ed) ed(el, ctx);
      }
    };
    const engine = scope.own(G.mount(t.band, {
      block: 'table', id: frame.id, scope, typeKey: 'layout', title: 'Table settings', host
    }));
    return { engine, open: t.open, update: (p) => engine.update(p) };
  }

  V.table.gear = { mount, visible, shadings };
  V.table.editors = Object.assign(V.table.editors || {}, { shadings });
})();
