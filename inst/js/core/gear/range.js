// @ts-check
/**
 * The gear's range control: two fields, from and to, for axis limits. Each
 * is blank for auto; either commits the pair, both blank is null. A log axis
 * starts above 0, so an end at or below 0 is not sent while the axis is log
 * (`<role>_transform` is "log10"); the note under the fields says why. The
 * sentence's words for the same setting come from limitsSlot(), which asks
 * limitsNote() too.
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

  /** Axis limits on the face: one word per end. An end typed over keeps the
   * other as the gear holds it now (`values()`); a pair whose from is not
   * below its to is not sent (R refuses it).
   * @param {VizArg} arg @param {string} key @param {unknown} value @param {string} label
   * @param {(v: unknown) => void} set @param {() => VizValues} values
   * @returns {VizSlotOpener} */
  function limitsSlot(arg, key, value, label, set, values) {
    const now = (/** @type {unknown} */ v) => (Array.isArray(v) ? v : [null, null]);
    const pair = now(value);
    const num = (/** @type {unknown} */ v) => (typeof v === 'number' ? v : Date.parse(String(v)));
    const end = (/** @type {number} */ i) => /** @type {VizEndOpener} */ ({
      kind: 'entry', value: pair[i], label: label + (i ? ' to' : ' from'),
      toText: (v) => (v == null ? '' : String(v)),
      fromText: (t) => { const p = /** @type {unknown[] | null} */ (C.fromText(arg, t)); return p ? p[0] : null; },
      set: (v) => {
        const next = now(values()[key]).slice();
        next[i] = v;
        if (next[0] != null && next[1] != null && !(num(next[0]) < num(next[1]))) return;
        if (limitsNote(values(), key, next)) return;
        set(next.every((x) => x == null) ? null : next);
      }
    });
    return { kind: 'pair', value, ends: [end(0), end(1)], set, label };
  }

  V.gear.more = { ...(V.gear.more || {}), range };
  V.gear.limitsNote = limitsNote;
  V.gear.limitsSlot = limitsSlot;
})();
