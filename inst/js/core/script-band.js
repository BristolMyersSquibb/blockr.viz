// @ts-check
/**
 * The prepare script's band (derived.script), for every block view: one
 * control per value the script declares, drawn by the gear's own control
 * builder, a row naming each declaration that could not become a control,
 * and "Script failed: ..." when the script threw. A change writes
 * `script_values`, keyed by the variable's name, through the gear. The band
 * is hidden while there is nothing to show.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /**
   * A declared control as a spec argument the gear can draw (R's
   * dd_script_roles(): kind select, multi, segmented, number, date, text).
   * @param {VizScriptInput} sp
   * @returns {VizArg}
   */
  function argOf(sp) {
    const gear = { label: sp.label };
    switch (sp.kind) {
      case 'select': return /** @type {VizArg} */ ({ type: { name: 'enum' }, values: sp.options || [],
                                                     gear: { ...gear, control: 'select' } });
      case 'multi': return /** @type {VizArg} */ ({ type: { name: 'enums' }, values: sp.options || [],
                                                    gear: { ...gear, control: 'select' } });
      case 'segmented': return /** @type {VizArg} */ ({ type: { name: 'bool' },
                                                        gear: { ...gear, control: 'toggle' } });
      case 'number': return /** @type {VizArg} */ ({
        type: { name: 'number' }, gear: { ...gear, control: 'number' },
        bounds: { min: sp.min ?? null, max: sp.max ?? null, minOpen: false }
      });
      default: return /** @type {VizArg} */ ({ type: { name: 'string' }, gear: { ...gear, control: 'text' } });
    }
  }

  /**
   * @param {string} tag
   * @param {string} cls
   * @param {HTMLElement} parent
   */
  function el(tag, cls, parent) {
    const e = parent.appendChild(root.document.createElement(tag));
    e.className = cls;
    return e;
  }

  /**
   * The band in `host`.
   * @param {HTMLElement} host The band element (class `dd-mapping-band`).
   * @param {VizScope} scope The view's Scope.
   * @param {VizGear} gear The view's gear engine: its values and set().
   * @returns {{ update(script: VizScriptState | null | undefined): void }}
   */
  function scriptBand(host, scope, gear) {
    host.style.display = 'none';
    let key = '';
    /** @type {VizScope | null} */
    let rows = null;

    /** @param {VizScriptInput} sp @param {unknown} value @param {VizScope} sc */
    const control = (sp, value, sc) => {
      const row = el('div', 'dd-form-row dd-band-row', host);
      row.setAttribute('data-role', sp.key);
      const arg = argOf(sp);
      if (arg.gear && arg.gear.control !== 'toggle') {
        el('span', 'blockr-label', el('div', 'dd-row-head', row)).textContent = sp.label;
      }
      // The gear's values hold an edit R has not echoed yet.
      const set = (/** @type {unknown} */ v) => gear.set({
        script_values: { ...(/** @type {Record<string, unknown>} */ (gear.values().script_values) || {}),
                         [sp.name]: v }
      });
      V.gear.controls.build(el('div', 'dd-row-controls', row), /** @type {any} */ ({
        key: sp.key, arg, label: sp.label, values: {}, columns: [], required: true,
        scope: sc, host: {}, titles: V.titles, labelOf: (/** @type {string} */ k) => k, set,
        value: sp.kind === 'segmented' ? value === 'on' || value === true : value
      }));
    };

    /** @param {VizScriptInput} sp */
    const failed = (sp) => {
      const row = el('div', 'dd-form-row dd-band-row dd-script-input-error', host);
      el('span', 'blockr-label', el('div', 'dd-row-head', row)).textContent = sp.label;
      el('span', 'dd-form-help', row).textContent = sp.error || 'no control';
    };

    return {
      update(script) {
        const s = script || {};
        const next = JSON.stringify(s);
        if (next === key) return;
        key = next;
        if (rows) rows.dispose();
        const sc = rows = scope.own(new V.Scope());
        host.textContent = '';
        const inputs = Array.isArray(s.inputs) ? s.inputs : [];
        for (const sp of inputs) {
          if (sp.kind === 'error') failed(sp);
          else control(sp, (s.values || {})[sp.key], sc);
        }
        if (s.error) {
          el('div', 'dd-form-row dd-band-row dd-script-failed', host).textContent =
            'Script failed: ' + s.error;
        }
        host.style.display = inputs.length || s.error ? '' : 'none';
      }
    };
  }

  V.scriptBand = scriptBand;
})();
