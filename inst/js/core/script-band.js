// @ts-check
/**
 * The prepare script's band above a view (`derived.script`). One control per
 * value the script declares, drawn by the gear's own control builder, and
 * "Script failed: ..." when the script threw. A change writes
 * `script_values`, keyed by the variable's name, through the gear, which
 * holds an edit R has not echoed yet.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /** @param {string} tag @param {string} cls @param {HTMLElement} parent */
  const el = (tag, cls, parent) => {
    const e = document.createElement(tag);
    e.className = cls;
    parent.appendChild(e);
    return e;
  };

  /** A declared control as a spec argument the gear can draw (R's dd_script_roles(): kind select,
   * multi, segmented, number, date, text). @param {VizScriptControl} sp @returns {VizArg} */
  const argOf = (sp) => {
    const gear = { label: sp.label };
    const bounds = { min: sp.min ?? null, max: sp.max ?? null, minOpen: false };
    switch (sp.kind) {
      case 'select': return /** @type {VizArg} */ ({ type: { name: 'enum' }, values: sp.options || [],
                                                     gear: { ...gear, control: 'select' } });
      case 'multi': return /** @type {VizArg} */ ({ type: { name: 'enums' }, values: sp.options || [],
                                                    gear: { ...gear, control: 'select' } });
      case 'segmented': return /** @type {VizArg} */ ({ type: { name: 'bool' },
                                                        gear: { ...gear, control: 'toggle' } });
      case 'number': return /** @type {VizArg} */ ({ type: { name: 'number' }, bounds,
                                                     gear: { ...gear, control: 'number' } });
      default: return /** @type {VizArg} */ ({ type: { name: 'string' },
                                               gear: { ...gear, control: 'text' } });
    }
  };

  /** The control's value as R holds it for the band ("on"/"off" for a flag).
   * @param {VizScriptControl} sp @param {unknown} v */
  const valueOf = (sp, v) => (sp.kind === 'segmented' ? v === 'on' || v === true : v);

  /**
   * The band, appended to `parent` and hidden while the script declares
   * nothing and did not fail.
   * @param {HTMLElement} parent @param {VizScope} scope @param {VizGear} gear
   * @returns {VizScriptBand}
   */
  V.scriptBand = (parent, scope, gear) => {
    const band = el('div', 'dd-mapping-band', parent);
    band.hidden = true;
    /** @type {string | null} */
    let last = null;
    /** Owns the controls of the band as drawn. @type {VizScope | null} */
    let drawn = null;

    /** @param {VizScriptControl} sp @param {unknown} value @param {VizScope} s */
    const control = (sp, value, s) => {
      const row = el('div', 'dd-form-row dd-band-row', band);
      row.setAttribute('data-role', sp.key);
      const arg = argOf(sp);
      if (arg.gear?.control !== 'toggle') {
        el('span', 'blockr-label', el('div', 'dd-row-head', row)).textContent = sp.label;
      }
      const set = (/** @type {unknown} */ v) => gear.set({
        script_values: { .../** @type {object} */ (gear.values().script_values || {}), [sp.name]: v }
      });
      V.gear.controls.build(el('div', 'dd-row-controls', row), {
        key: sp.key, arg, label: sp.label, value: valueOf(sp, value), values: {},
        columns: [], required: true, scope: s, host: {}, titles: {},
        labelOf: (/** @type {string} */ k) => k, set
      });
    };

    // A declaration that could not become a control, named with its reason.
    /** @param {VizScriptFailed} sp */
    const failed = (sp) => {
      const row = el('div', 'dd-form-row dd-band-row dd-script-input-error', band);
      el('span', 'blockr-label', el('div', 'dd-row-head', row)).textContent = sp.label;
      el('span', 'dd-form-help', row).textContent = sp.error || 'no control';
    };

    /** Redrawn only when the script's inputs, values or error changed.
     * @param {VizScript | null | undefined} script */
    const update = (script) => {
      const s = /** @type {Partial<VizScript>} */ (script || {});
      const key = JSON.stringify(s);
      if (key === last) return;
      last = key;
      if (drawn) drawn.dispose();
      const own = drawn = scope.own(new V.Scope());
      band.textContent = '';
      const inputs = Array.isArray(s.inputs) ? s.inputs : [];
      const values = /** @type {Record<string, unknown>} */ (s.values || {});
      for (const sp of inputs) {
        if (sp.kind === 'error') failed(sp);
        else control(sp, values[sp.key], own);
      }
      if (s.error) {
        el('div', 'dd-form-row dd-band-row dd-script-failed', band).textContent =
          'Script failed: ' + s.error;
      }
      band.hidden = !(inputs.length || s.error);
    };

    return { el: band, update };
  };
})();
