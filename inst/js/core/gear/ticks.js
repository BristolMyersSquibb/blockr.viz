// @ts-check
/**
 * The gear's ticks control, for `x_ticks` and `y_ticks`: Auto | Every | At,
 * then the fields of that form. Every takes a number and, on a date axis, a
 * unit; At takes a comma list. Picking a form starts from the ticks the axis
 * draws now (V.ticks.start), so nothing has to be typed to see it work. A
 * log axis takes a list only: Every is off and a set step says why. The
 * sentence's words for the same setting come from ticksSlot(), which the
 * engine asks for a `ticks` word.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const C = V.gear.controls;
  const T = V.ticks;
  const ui = () => /** @type {VizUi} */ (root.Blockr);

  const FORMS = [{ value: 'auto', label: 'Auto' }, { value: 'every', label: 'Every' },
                 { value: 'at', label: 'At' }];

  /** The sentence's words for a ticks setting, one per part as the gear has a
   * control per part: the form ("auto", "every", "at", a menu), the step ("7",
   * typed over) and its unit on a date axis ("days", a menu), or the list ("0,
   * 25, 50", typed over). A form picked starts from the ticks drawn now.
   * @param {VizGearHost} host @param {string} key @param {unknown} value @param {string} label
   * @param {(v: unknown) => void} set @param {VizValues} values @returns {VizSlotOpener} */
  function ticksSlot(host, key, value, label, set, values) {
    const axis = (host.ticks && host.ticks(key, values)) || { type: 'value', drawn: [] };
    const log = axis.type === 'log';
    const p = T.parse(value);
    const form = p ? p.form : 'auto';
    /** @param {unknown} v */
    const send = (v) => { if (!T.note(v, log)) set(v); };
    /** @param {VizChoice[]} options @param {unknown} selected @param {string} lab
     * @param {(v: unknown) => void} pick @returns {VizWordPart['how']} */
    const menu = (options, selected, lab, pick) => ({ kind: 'menu', options, selected, multi: false,
                                                      label: lab, set: pick });
    const forms = ['auto', 'every', 'at'].filter((f) => !(log && f === 'every'))
      .map((f) => ({ value: f, label: f }));
    /** @type {VizWordPart[]} */
    const words = [{ text: form, how: menu(forms, form, label, (f) => {
      if (f === form) return;
      // The ticks drawn now, read at the pick: the words may predate the draw.
      const now = (host.ticks && host.ticks(key, values)) || axis;
      send(f === 'auto' ? null : T.start(/** @type {'every' | 'at'} */ (f), now.drawn, now.type));
    }) }];
    if (p && p.form === 'every') {
      words.push({ text: String(p.n), how: { kind: 'entry', value: p.n, label: label + ' step',
        toText: (v) => (v == null ? '' : String(v)),
        fromText: (t) => { const n = Number(String(t).trim()); return Number.isFinite(n) && n > 0 ? n : null; },
        set: (n) => { if (typeof n === 'number') send(T.step(n, p.unit)); } } });
      if (p.unit) {
        const units = T.UNITS.map((u) => ({ value: u, label: T.unitWord(u, p.n) }));
        words.push({ text: T.unitWord(p.unit, p.n),
                     how: menu(units, p.unit, label + ' unit', (u) => send(T.step(p.n, String(u)))) });
      }
    }
    if (p && p.form === 'at') {
      words.push({ text: p.list.join(', '), how: { kind: 'entry', value: p.list, label,
        toText: (v) => (Array.isArray(v) ? v.join(', ') : ''),
        fromText: (t) => T.parseList(t),
        set: (l) => { if (Array.isArray(l) && l.length >= 2) send(l); } } });
    }
    return { kind: 'words', value, words, set, label };
  }

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function ticks(el, ctx) {
    const axis = (ctx.host.ticks && ctx.host.ticks(ctx.key, ctx.values)) || { type: 'value', drawn: [] };
    const log = axis.type === 'log';
    const p = T.parse(ctx.value);
    const form = p ? p.form : 'auto';
    const box = el.appendChild(Object.assign(document.createElement('div'), { className: 'vz-ticks' }));
    const note = el.appendChild(Object.assign(document.createElement('div'), { className: 'vz-range-note' }));
    const say = (/** @type {string | null} */ why) => { note.textContent = why || ''; note.hidden = !why; };
    say(T.note(ctx.value, log));
    /** @param {unknown} v */
    const send = (v) => {
      const why = T.note(v, log);
      say(why);
      if (!why && !C.same(v, ctx.value)) ctx.set(v);
    };

    const seg = ui().segmented(FORMS, form, (/** @type {string} */ f) => {
      // The ticks drawn now, read at the click: the row may predate the draw.
      const now = (ctx.host.ticks && ctx.host.ticks(ctx.key, ctx.values)) || axis;
      send(f === 'auto' ? null : T.start(/** @type {'every' | 'at'} */ (f), now.drawn, now.type));
    }, { label: ctx.label });
    box.appendChild(seg.el);
    if (log) {
      const every = /** @type {HTMLButtonElement | undefined} */ (seg.el.children[1]);
      if (every) {
        every.disabled = true;
        every.title = T.LOG_NOTE;
      }
    }

    /** A text field in the row, holding `text`, committing through `commit`.
     * @param {string} key @param {string} label @param {string} text
     * @param {(t: string) => void} commit @param {string} cls */
    const field = (key, label, text, commit, cls) => {
      const arg = { ...ctx.arg, type: { name: 'string' }, gear: { ...ctx.arg.gear, parse: undefined } };
      const inp = C.field(box, /** @type {VizRowCtx} */ ({ ...ctx, arg, key: ctx.key + '_' + key,
                                                           label: ctx.label + ' ' + label, value: text }),
                          { commit });
      inp.classList.add(cls);
      return inp;
    };

    if (p && p.form === 'every') {
      const step = p;
      const inp = field('step', 'step', String(step.n), (t) => {
        const n = Number(String(t).trim());
        if (Number.isFinite(n) && n > 0) send(T.step(n, step.unit));
        else say('A step is a number above 0.');
      }, 'vz-ticks-step');
      inp.inputMode = 'decimal';
      if (axis.type === 'time' || step.unit) {
        const unit = step.unit || 'day';
        const host = box.appendChild(Object.assign(document.createElement('div'), { className: 'vz-ticks-unit' }));
        const opts = T.UNITS.map((u) => ({ value: u, label: T.unitWord(u, step.n) }));
        const inst = ui().Select.single(host, {
          options: opts.map((o) => o.label), selected: T.unitWord(unit, step.n), bordered: true,
          label: ctx.label + ' unit',
          onChange: (/** @type {string} */ v) => {
            const o = opts.find((x) => x.label === v);
            if (o) send(T.step(step.n, o.value));
          }
        });
        ctx.scope.own({ dispose: () => inst.destroy() });
      }
    }
    if (p && p.form === 'at') {
      field('list', 'values', p.list.join(', '), (t) => {
        const list = T.parseList(t);
        if (list.length < 2) { say(T.LIST_NOTE); return; }
        send(list);
      }, 'vz-ticks-list');
    }
  }

  V.gear.more = { ...(V.gear.more || {}), ticks };
  V.gear.ticksSlot = ticksSlot;
})();
