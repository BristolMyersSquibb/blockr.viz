// @ts-check
/**
 * The gear's range control: two fields, from and to, for axis limits. Each
 * is blank for auto; either commits the pair, both blank is null. A log axis
 * starts above 0, so an end at or below 0 is not sent while the axis is log
 * (`<role>_transform` is "log10"); the note under the fields says why. The
 * sentence's words for the same setting ask limitsNote() too (engine.js).
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const C = V.gear.controls;

  /** Why a pair cannot be set on its axis, or null.
   * @param {VizValues} values @param {string} key @param {unknown} pair @returns {string | null} */
  const limitsNote = (values, key, pair) => {
    const tk = key.replace(/_limits$/, '_transform');
    const arg = /** @type {any} */ (V.spec.chart || {})[tk];
    const only = arg && arg.applies && arg.applies.only;
    const log = values[tk] === 'log10' && (!only || only.includes(values.type));
    const low = Array.isArray(pair) && pair.some((v) => typeof v === 'number' && v <= 0);
    return log && low ? 'A log axis starts above 0. Leave the field empty for auto.' : null;
  };

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function range(el, ctx) {
    const cur = Array.isArray(ctx.value) ? ctx.value : [null, null];
    const box = el.appendChild(Object.assign(document.createElement('div'), { className: 'vz-range' }));
    const note = el.appendChild(Object.assign(document.createElement('div'), { className: 'vz-range-note' }));
    note.hidden = true;
    /** @type {HTMLInputElement[]} */
    const inputs = [];
    const commit = () => {
      const v = C.fromText(ctx.arg, inputs.map((inp) => inp.value.trim() || 'auto').join(' to '));
      const why = limitsNote(ctx.values, ctx.key, v);
      note.textContent = why || '';
      note.hidden = !why;
      if (!why && !C.same(v, ctx.value)) ctx.set(v);
    };
    ['from', 'to'].forEach((end, i) => {
      if (i) box.appendChild(Object.assign(document.createElement('span'), { className: 'vz-range-sep', textContent: 'to' }));
      const arg = /** @type {VizArg} */ ({ ...ctx.arg, type: { name: 'string' }, gear: { ...ctx.arg.gear, parse: undefined } });
      const inp = C.field(box, { ...ctx, arg, key: ctx.key + '_' + end, label: ctx.label + ' ' + end,
                                 value: cur[i] == null ? '' : String(cur[i]) }, { commit });
      inp.placeholder = 'auto';
      inputs.push(inp);
    });
  }

  V.gear.more = { ...(V.gear.more || {}), range };
  V.gear.limitsNote = limitsNote;
})();
