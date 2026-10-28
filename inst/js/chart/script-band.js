// @ts-check
/**
 * Chart block: the prepare script's band above the chart (derived.script).
 * One control per value the script declares, drawn by the gear's own
 * control builder, and "Script failed: ..." when the script threw. A change
 * writes `script_values`, keyed by the variable's name.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {any} */ (root.Blockr.viz);
  const NS = /** @type {any} */ (V.chart);
  const mk = NS.mk;

  /** A declared control as a spec argument the gear can draw (R's dd_script_roles(): kind select,
   * multi, segmented, number, date, text). @param {any} sp @returns {any} */
  const argOf = (sp) => {
    const gear = { label: sp.label };
    const bounds = { min: sp.min ?? null, max: sp.max ?? null, minOpen: false };
    switch (sp.kind) {
      case 'select': return { type: { name: 'enum' }, values: sp.options || [],
                              gear: { ...gear, control: 'select' } };
      case 'multi': return { type: { name: 'enums' }, values: sp.options || [],
                             gear: { ...gear, control: 'select' } };
      case 'segmented': return { type: { name: 'bool' }, gear: { ...gear, control: 'toggle' } };
      case 'number': return { type: { name: 'number' }, bounds, gear: { ...gear, control: 'number' } };
      default: return { type: { name: 'string' }, gear: { ...gear, control: 'text' } };
    }
  };

  /** The control's value as R holds it for the band ("on"/"off" for a flag).
   * @param {any} sp @param {any} v */
  const valueOf = (sp, v) => (sp.kind === 'segmented' ? v === 'on' || v === true : v);

  const scriptBand = {
    /** @this {any} @param {{ inputs?: any[], values?: Record<string, any>,
     *  error?: string | null } | null | undefined} script */
    _updateScriptBand(script) {
      const s = script || {};
      const key = JSON.stringify(s);
      if (key === this._scriptKey) return;
      this._scriptKey = key;
      if (this._bandScope) this._bandScope.dispose();
      const scope = this._bandScope = this.scope.own(new V.Scope());
      const el = this.bandEl;
      el.innerHTML = '';
      const inputs = Array.isArray(s.inputs) ? s.inputs : [];
      for (const sp of inputs) {
        if (sp.kind === 'error') this._scriptInputError(sp);
        else this._scriptControl(sp, (s.values || {})[sp.key], scope);
      }
      if (s.error) {
        mk('div', 'dd-form-row dd-band-row dd-script-failed', el).textContent =
          'Script failed: ' + s.error;
      }
      el.style.display = inputs.length || s.error ? '' : 'none';
    },

    /** @this {any} @param {any} sp @param {any} value @param {VizScope} scope */
    _scriptControl(sp, value, scope) {
      const row = mk('div', 'dd-form-row dd-band-row', this.bandEl);
      row.setAttribute('data-role', sp.key);
      const arg = argOf(sp);
      if (arg.gear.control !== 'toggle') {
        mk('span', 'blockr-label', mk('div', 'dd-row-head', row)).textContent = sp.label;
      }
      // The gear's values hold an edit R has not echoed yet.
      const set = (/** @type {any} */ v) => this._gear.set({
        script_values: { ...(this._gear.values().script_values || {}), [sp.name]: v }
      });
      V.gear.controls.build(mk('div', 'dd-row-controls', row), {
        key: sp.key, arg, label: sp.label, value: valueOf(sp, value), values: {},
        columns: [], required: true, scope, host: {}, titles: V.titles,
        labelOf: (/** @type {string} */ k) => k, set
      });
    },

    // A declaration that could not become a control, named with its reason.
    /** @this {any} @param {any} sp */
    _scriptInputError(sp) {
      const row = mk('div', 'dd-form-row dd-band-row dd-script-input-error', this.bandEl);
      mk('span', 'blockr-label', mk('div', 'dd-row-head', row)).textContent = sp.label;
      mk('span', 'dd-form-help', row).textContent = sp.error || 'no control';
    }
  };

  NS.scriptBand = scriptBand;
})();
