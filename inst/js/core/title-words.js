// @ts-check
/**
 * The sentence on the block's face: its words open their setting's control,
 * a menu or a field typed over the word, and the settings a dropped clause
 * would name are offered after it. Reads the parts R resolved, as the gear's
 * template rows do (titles.js).
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const ui = () => /** @type {VizUi} */ (root.Blockr);

  /** @param {VizScope} scope @param {HTMLElement} el @param {() => void} fn */
  const wire = (scope, el, fn) => {
    el.setAttribute('role', 'button');
    el.setAttribute('tabindex', '0');
    scope.on(el, 'click', (e) => { e.stopPropagation(); fn(); });
    scope.on(el, 'keydown', (ev) => {
      const e = /** @type {KeyboardEvent} */ (ev);
      if (e.key !== 'Enter' && e.key !== ' ') return;
      e.preventDefault();
      fn();
    });
  };

  /** The setting's help as a muted line: at the foot of a word's menu
   * (`panel`), or hung under the field typed over a word.
   * @param {string | undefined} help
   * @param {{ panel?: Element | null, under?: HTMLElement, scope?: VizScope }} at
   *   `scope` takes the floating line away when the field goes. */
  function helpLine(help, at) {
    if (!help) return;
    const line = document.createElement('div');
    line.className = 'vz-slot-help';
    line.textContent = help;
    if (at.panel) { at.panel.appendChild(line); return; }
    if (!at.under || !at.scope) return;
    line.classList.add('vz-slot-help--floating');
    document.body.appendChild(line);
    const place = ui().place;
    const placed = place ? place(line, at.under, { width: { min: 180, max: 320 } }) : null;
    at.scope.own({ dispose: () => { if (placed) placed.stop(); line.remove(); } });
  }

  /** Type a value over its word: Enter or leaving commits, Escape puts the
   * word back. A number is held in its range and steps on the arrow keys
   * (by ten with Shift); an emptied number keeps its value, unless it can be
   * empty (`clear`).
   * @param {VizScope} scope @param {HTMLElement} anchor
   * @param {VizSlotOpener & { kind: 'number' | 'entry' }} how */
  function edit(scope, anchor, how) {
    const es = scope.own(new V.Scope());
    const num = how.kind === 'number' ? how : null;
    const cur = how.value;
    const before = how.kind === 'number' ? (cur == null ? '' : String(cur)) : how.toText(cur);
    const input = document.createElement('input');
    input.type = num ? 'number' : 'text';
    input.className = 'blockr-slot-input';
    input.value = before;
    input.setAttribute('aria-label', how.label);
    // The box grows with its text, 4 to 34 characters; past that the
    // sentence wraps it to a line of its own. CSS field-sizing does the
    // same where the browser has it.
    const fit = () => { input.size = Math.min(34, Math.max(4, input.value.length + 1)); };
    fit();
    /** @param {number} v */
    const fix = (v) => (num ? Math.min(num.max, Math.max(num.min, v)) : v);
    let done = false;
    const finish = (/** @type {boolean} */ commit) => {
      if (done) return;
      done = true;
      const text = input.value.trim();
      es.dispose();
      input.replaceWith(anchor);
      if (!commit || text === before) { anchor.focus(); return; }
      if (num) {
        const v = parseFloat(text);
        if (Number.isFinite(v)) how.set(fix(v));
        else if (text === '' && num.clear) how.set(null);
      } else if (how.kind === 'entry') {
        how.set(how.fromText(text));
      }
    };
    es.on(input, 'input', fit);
    es.on(input, 'click', (e) => e.stopPropagation());
    es.on(input, 'keydown', (ev) => {
      const e = /** @type {KeyboardEvent} */ (ev);
      e.stopPropagation();
      if (num && (e.key === 'ArrowUp' || e.key === 'ArrowDown')) {
        e.preventDefault();
        const v = parseFloat(input.value);
        const by = num.step * (e.shiftKey ? 10 : 1) * (e.key === 'ArrowUp' ? 1 : -1);
        input.value = String(Number(fix((Number.isFinite(v) ? v : 0) + by).toFixed(10)));
      }
      if (e.key === 'Enter') { e.preventDefault(); finish(true); }
      if (e.key === 'Escape') { e.preventDefault(); finish(false); }
    });
    es.on(input, 'blur', () => finish(true));
    anchor.replaceWith(input);
    helpLine(how.help, { under: input, scope: es });
    input.focus();
    input.select();
  }

  /** Fill a menu under a word with its setting's choices; null when the
   * page's blockr.ui has no menu for it.
   * @param {HTMLElement} anchor
   * @param {VizSlotOpener & { kind: 'menu' }} how @param {string | undefined} by
   * @param {() => void} onClose */
  function openMenu(anchor, how, by, onClose) {
    const Sel = ui().Select;
    if (!Sel || !Sel.menu || (how.multi && !Sel.menuMulti)) return null;
    const C = V.gear.controls;
    const { options, keys } = C.uiOptions(how.options);
    const at = (/** @type {unknown} */ v) => keys[how.options.findIndex((o) => C.same(o.value, v))];
    const back = (/** @type {string} */ k) => how.options[keys.indexOf(k)];
    anchor.classList.add('blockr-slot--open');
    const menu = Sel.menu(anchor, {
      options, mode: how.multi ? 'multi' : 'single', title: how.label,
      selected: how.multi ? (/** @type {unknown[]} */ (how.selected || [])).map(at) : at(how.selected),
      labelFirst: by === 'label', searchPlaceholder: 'Filter',
      onChange: (/** @type {string | string[]} */ v) => {
        const val = Array.isArray(v) ? v.map((k) => back(k)).filter(Boolean).map((c) => c.value)
          : (back(v) || { value: how.selected }).value;
        how.set(val);
      },
      onClose: () => {
        anchor.classList.remove('blockr-slot--open');
        onClose();
      }
    });
    // The menu's panel is the last one blockr.ui hung on <body>; it goes with
    // the menu, and the line with it.
    const panels = document.querySelectorAll('body > .blockr-select__dropdown--menu');
    helpLine(how.help, { panel: panels[panels.length - 1] });
    return menu;
  }

  /** The settings a sentence would name if they were set, three at most.
   * @param {HTMLElement} el @param {string[]} offers @param {VizScope} ps
   * @param {VizGear} gear @param {(key: string, chip: HTMLElement) => void} openSlot
   * @param {() => void} openGear */
  function paintOffers(el, offers, ps, gear, openSlot, openGear) {
    const MAX = 3;
    const keys = offers.filter((k) => typeof k === 'string');
    const shown = keys.length > MAX ? keys.slice(0, MAX - 1) : keys;
    for (const key of shown) {
      const c = document.createElement('span');
      c.className = 'blockr-slot-offer';
      const how = gear.slot(key);
      c.textContent = '+ ' + (how ? how.label : gear.label(key));
      wire(ps, c, () => openSlot(key, c));
      el.appendChild(c);
    }
    if (keys.length <= MAX) return;
    const more = document.createElement('span');
    more.className = 'blockr-slot-offer blockr-slot-offer--more';
    more.textContent = 'More settings';
    wire(ps, more, openGear);
    el.appendChild(more);
  }

  /** The sentence on the block's face. A word R marked with an argument is
   * a control when the gear has one for it: a flag toggles in place, a
   * number or a list of values is typed over, anything else opens its list
   * (blockr.ui's Blockr.Select.menu). Offers name settings the sentence
   * would show if they were set; past three the last reads "More settings".
   * @param {{ gear: VizGear, scope: VizScope, openGear?: () => void }} opts
   * @returns {VizSlots} */
  function slots(opts) {
    const { gear } = opts;
    const own = opts.scope.own(new V.Scope());
    /** @type {Map<HTMLElement, VizScope>} */
    const painted = new Map();
    /** @type {{ key: string, anchor: HTMLElement, menu: { close(): void } } | null} */
    let open = null;
    const openGear = () => { if (opts.openGear) opts.openGear(); };

    const close = () => {
      const o = open;
      open = null;
      if (!o) return;
      o.anchor.classList.remove('blockr-slot--open');
      o.menu.close();
    };

    /** `given` opens one word of a setting drawn as several (axis ticks).
     * @param {string} key @param {HTMLElement} anchor @param {VizScope} ps
     * @param {string} [by] @param {VizSlotOpener} [given] */
    const openSlot = (key, anchor, ps, by, given) => {
      const was = open && open.key;
      close();
      if (was === key) return;
      const how = given || gear.slot(key);
      if (!how) { openGear(); return; }
      if (how.kind === 'flag') { how.set(!how.value); return; }
      if (how.kind === 'entry' || how.kind === 'number') { edit(ps, anchor, how); return; }
      if (how.kind === 'pair') { edit(ps, anchor, how.ends[0]); return; }
      if (how.kind === 'words') { openSlot(key + '#0', anchor, ps, by, how.words[0].how); return; }
      const menu = openMenu(anchor, how, by, () => {
        if (open && open.anchor === anchor) open = null;
      });
      if (!menu) { openGear(); return; }
      open = { key, anchor, menu };
    };

    /** @param {HTMLElement} el @param {VizTitleBand | null | undefined} band */
    const paint = (el, band) => {
      const old = painted.get(el);
      if (old) old.dispose();
      const ps = new V.Scope();
      painted.set(el, ps);
      el.textContent = '';
      if (!band) return;
      const parts = band.parts || [];
      if (!parts.length && band.text) el.textContent = band.text;
      for (const p of parts) {
        if (!p || !p.text) continue;
        const how = p.arg ? gear.slot(p.arg) : null;
        if (!how) { el.appendChild(document.createTextNode(p.text)); continue; }
        if (how.kind === 'pair') { V.slotPair(el, ps, how.ends); continue; }
        const arg = /** @type {string} */ (p.arg);
        if (how.kind === 'words') { V.slotWords(el, ps, how.words, (i, w) => openSlot(arg + '#' + i, w, ps, p.by, how.words[i].how)); continue; }
        const w = document.createElement('span');
        w.className = 'blockr-slot';
        w.textContent = p.text;
        wire(ps, w, () => openSlot(arg, w, ps, p.by));
        el.appendChild(w);
      }
      paintOffers(el, band.offers || [], ps, gear, (key, c) => openSlot(key, c, ps), openGear);
    };

    own.own({ dispose: () => {
      close();
      for (const ps of painted.values()) ps.dispose();
      painted.clear();
    } });
    return { paint, close, dispose: () => own.dispose() };
  }

  V.titleWords = { slots, edit, wire };
})();
