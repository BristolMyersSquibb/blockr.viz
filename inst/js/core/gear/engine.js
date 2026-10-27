// @ts-check
/**
 * The gear engine: one per block view. It draws the gear tray from the
 * block's generated spec (Blockr.viz.spec.<block>), holds the settings the
 * tray shows, and sends a change as `{rev, values}` with only the keys that
 * changed (`<id>_set`, read in R by viz_read_set()).
 *
 * The view mounts it once and hands it every payload:
 *
 *   const gear = scope.own(Blockr.viz.gear.mount(trayEl, { block, id, scope }));
 *   gear.update(payload);
 *
 * Everything it sets up goes through Scopes: the view's for the engine's
 * life, a fresh child per render for the controls of that render.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  V.gear = V.gear || /** @type {VizGearNamespace} */ ({});
  const S = V.gear.sections;
  const C = V.gear.controls;

  /** A local edit not yet echoed by R: its value, the value R had, and how
   * many more payloads may still predate it. */
  /** @typedef {{ value: unknown, before: unknown, left: number }} Pending */

  class Gear {
    /** @param {HTMLElement} el @param {VizGearOptions} opts */
    constructor(el, opts) {
      const spec = opts.spec || /** @type {VizSpec | undefined} */ (V.spec[opts.block]);
      if (!spec) throw new Error('blockr.viz: no spec for block "' + opts.block + '"');
      this.el = el;
      this.spec = spec;
      this.block = opts.block;
      this.id = opts.id;
      this.scope = opts.scope;
      this.host = opts.host || {};
      this.title = opts.title || 'Settings';
      this.typeKey = opts.typeKey || (spec.type ? 'type' : (spec.layout ? 'layout' : ''));
      /** @type {VizValues} */
      this._cfg = {};
      /** @type {Map<string, Pending>} */
      this._pending = new Map();
      /** @type {VizColumn[]} */
      this._cols = [];
      /** @type {VizTitles} */
      this._titles = {};
      this._rev = 0;
      this._dataRev = 0;
      /** Optional mappings added from the Add menu, shown while unset. */
      /** @type {Set<string>} */
      this._added = new Set();
      /** @type {string | null} */
      this._group = null;
      /** @type {VizScope | null} */
      this._rs = null;
      this._drawn = '';
      this.disposed = false;
    }

    /** @param {VizPayload} p */
    update(p) {
      if (this.disposed) return;
      const cfg = p.config || {};
      for (const [key, pend] of this._pending) {
        const now = cfg[key];
        pend.left -= 1;
        if (C.same(now, pend.value) || !C.same(now, pend.before) || pend.left <= 0) {
          this._pending.delete(key);
        }
      }
      this._cfg = cfg;
      this._cols = p.columns || [];
      this._titles = /** @type {VizTitles} */ ((p.derived && p.derived.titles) || {});
      this._rev = p.state_rev || 0;
      this._dataRev = p.data_rev || 0;
      if (this._key() !== this._drawn) this.render();
    }

    /** The settings as R sent them, with the local edits on top. */
    values() {
      /** @type {VizValues} */
      const out = Object.assign({}, this._cfg);
      for (const [key, pend] of this._pending) out[key] = pend.value;
      return out;
    }

    /** Write settings and send the ones that changed.
     * @param {VizValues} changes */
    set(changes) {
      if (this.disposed) return;
      const now = this.values();
      /** @type {VizValues} */
      const changed = {};
      for (const [key, v] of Object.entries(changes)) {
        if (!this.spec[key] || C.same(now[key], v)) continue;
        changed[key] = v;
        this._pending.set(key, { value: v, before: this._cfg[key], left: 2 });
      }
      if (!Object.keys(changed).length) return;
      const msg = { rev: this._rev, values: changed };
      if (this.host.send) this.host.send(msg);
      else V.util.send(this.id, 'set', msg, this.block);
      if (this.host.onChange) this.host.onChange(changed, this.values());
      this.render();
    }

    /** What decides the drawing; a payload that leaves it alone keeps the
     * tray as it is, open lists included. */
    _key() {
      return JSON.stringify([this.values(), this._cols, this._titles, this._dataRev,
                             Array.from(this._added)]);
    }

    /** @param {string} key @param {VizValues} [values] */
    label(key, values) {
      const vals = values || this.values();
      const own = this.host.label && this.host.label(key, vals);
      if (own) return own;
      const arg = this.spec[key];
      if (arg && arg.gear) return arg.gear.label;
      const words = String(key).replace(/[_.]+/g, ' ').trim();
      return words.charAt(0).toUpperCase() + words.slice(1);
    }

    /** Is the row for `key` drawn now?
     * @param {string} key @param {VizValues} values */
    _shown(key, values) {
      const arg = this.spec[key];
      if (!arg || !arg.gear) return false;
      if (arg.gear.control === 'none' && !this.host.custom) return false;
      if (this.typeKey && !S.applies(arg, values[this.typeKey])) return false;
      if (arg.gear.when && !C.isSet(values[arg.gear.when])) return false;
      return !this.host.visible || this.host.visible(key, values) !== false;
    }

    /** @param {string} key @param {VizValues} values @param {VizScope} scope
     * @returns {VizRowCtx} */
    _ctx(key, values, scope) {
      const required = !!(this.host.required && this.host.required(key, values));
      return {
        key, arg: this.spec[key], label: this.label(key, values), value: values[key], values,
        columns: this._cols, required, scope, host: this.host, titles: this._titles,
        labelOf: (k) => this.label(k, values),
        set: (v) => this.set({ [key]: v })
      };
    }

    render() {
      if (this.disposed) return;
      const focus = this._saveFocus();
      if (this._rs) this._rs.dispose();
      const rs = this._rs = new V.Scope();
      const values = this.values();
      this._drawn = this._key();
      this.el.textContent = '';
      this.el.setAttribute('role', 'dialog');
      this.el.setAttribute('aria-label', this.title);
      const plan = S.plan(this.spec, (k) => this._shown(k, values),
        (k) => C.isSet(values[k]) || this._added.has(k));
      const built = plan.map((p) => {
        const sec = S.sectionEl(p.title);
        for (const key of p.keys) this._row(sec.body, p.title, this._ctx(key, values, rs));
        if (p.adds.length) this._addMenu(sec.body, p.adds, values, rs);
        return { plan: p, el: sec.el, titleEl: sec.titleEl };
      });
      S.assemble(this.el, built, this._group, (g) => { this._group = g; }, this.title);
      this._restoreFocus(focus);
    }

    /** One row: its label over its controls. The label is left out where
     * it repeats the section's title or where a checkbox carries it.
     * @param {HTMLElement} body @param {string} section @param {VizRowCtx} ctx */
    _row(body, section, ctx) {
      const row = document.createElement('div');
      row.className = 'dd-form-row';
      row.setAttribute('data-role', ctx.key);
      const g = /** @type {VizGearField} */ (ctx.arg.gear);
      const boxed = g.control === 'toggle' ||
        (g.control === 'segmented' && ctx.arg.type.name === 'bool');
      if (!boxed && ctx.label !== section) {
        const head = document.createElement('div');
        head.className = 'dd-row-head';
        const lbl = document.createElement('span');
        lbl.className = 'blockr-label';
        lbl.textContent = ctx.label + (ctx.required ? ' *' : '');
        head.appendChild(lbl);
        row.appendChild(head);
      }
      const controls = document.createElement('div');
      controls.className = 'dd-row-controls';
      row.appendChild(controls);
      body.appendChild(row);
      C.build(controls, ctx);
    }

    /** The optional mappings not shown, as a menu that adds a row.
     * @param {HTMLElement} body @param {string[]} keys @param {VizValues} values
     * @param {VizScope} scope */
    _addMenu(body, keys, values, scope) {
      const row = document.createElement('div');
      row.className = 'dd-form-row dd-add-mapping';
      const sel = document.createElement('select');
      sel.className = 'dd-select dd-add-select';
      sel.setAttribute('aria-label', 'Add a mapping');
      sel.setAttribute('data-gear-input', '.add');
      const first = document.createElement('option');
      first.value = '';
      first.textContent = '+ Add';
      sel.appendChild(first);
      for (const key of keys) {
        const o = document.createElement('option');
        o.value = key;
        o.textContent = this.label(key, values);
        sel.appendChild(o);
      }
      scope.on(sel, 'change', () => {
        if (!sel.value) return;
        this._added.add(sel.value);
        this.render();
      });
      row.appendChild(sel);
      body.appendChild(row);
    }

    /** A field being typed in, so a re-render can put it back. */
    _saveFocus() {
      const a = /** @type {HTMLInputElement | null} */ (document.activeElement);
      if (!a || !this.el.contains(a)) return null;
      const key = a.getAttribute('data-gear-input');
      if (!key) return null;
      const typed = a.tagName === 'INPUT' || a.tagName === 'TEXTAREA';
      return { key, text: typed ? a.value : null,
               start: typed ? a.selectionStart : null, end: typed ? a.selectionEnd : null };
    }

    /** @param {{ key: string, text: string | null, start: number | null,
     *            end: number | null } | null} f */
    _restoreFocus(f) {
      if (!f) return;
      const el = /** @type {HTMLInputElement | null} */ (
        this.el.querySelector('[data-gear-input="' + f.key + '"]'));
      if (!el) return;
      if (f.text != null && el.value !== f.text) {
        el.value = f.text;
        el.dispatchEvent(new Event('input'));
      }
      el.focus();
      if (f.start != null && typeof el.setSelectionRange === 'function') {
        try { el.setSelectionRange(f.start, f.end); } catch (e) { /* not a text field */ }
      }
    }

    /** What a click on the sentence's word for `key` opens.
     * @param {string} key @returns {VizSlotOpener | null} */
    slot(key) {
      const arg = this.spec[key];
      if (!arg || !arg.gear || arg.gear.control === 'none') return null;
      const values = this.values();
      const name = arg.type.name;
      const listed = this._listed(key);
      if (name === 'bool') return { kind: 'flag', value: values[key] === true };
      if ((name === 'number' || name === 'integer') && !listed) {
        const b = arg.bounds || { min: null, max: null, minOpen: false };
        return { kind: 'number', min: b.min == null ? -Infinity : b.min,
                 max: b.max == null ? Infinity : b.max, step: name === 'integer' ? 1 : 0.1 };
      }
      if ((name === 'numbers' || name === 'values') && !listed) return { kind: 'entry' };
      if (['string', 'template', 'map', 'records', 'records_by', 'named'].includes(name)) {
        return null;
      }
      const ctx = this._ctx(key, values, this.scope);
      const empty = name === 'columns' || name === 'enums' ? null : C.emptyChoice(ctx);
      let options = /** @type {VizChoice[]} */ (empty ? [empty] : []).concat(C.options(ctx));
      if (listed) {
        const cur = values[key];
        options = options.filter((o) => o.value == null || listed.includes(String(o.value)) ||
          C.same(o.value, cur));
      }
      if (!options.length) return null;
      return { kind: 'menu', options, selected: values[key] ?? null,
               multi: name === 'columns' || name === 'enums' };
    }

    /** The values a sentence lists for a word (`{@func in mean, median}`).
     * @param {string} key @returns {string[] | null} */
    _listed(key) {
      const all = this._titles.choices;
      const v = all && all[key];
      if (v == null) return null;
      const list = (Array.isArray(v) ? v : [v]).map(String).filter((x) => x);
      return list.length ? list : null;
    }

    dispose() {
      if (this.disposed) return;
      this.disposed = true;
      if (this._rs) this._rs.dispose();
      this._rs = null;
      this._pending.clear();
      this.el.textContent = '';
    }
  }

  /**
   * A gear button and its tray, appended to a view's header `head`: blockr.ui's
   * Blockr.gearTray when the page has it, else the button toggles the tray.
   * The header gets `vz-head--open` while the tray is open; a tray left open
   * is closed when `scope` goes, which takes it off blockr.ui's dismiss
   * stack.
   * @param {HTMLElement} head @param {VizScope} scope @param {string} label
   * @returns {{ band: HTMLElement, button: HTMLButtonElement, set(on: boolean): void,
   *             isOpen(): boolean }}
   */
  V.gear.tray = (head, scope, label) => {
    const btn = document.createElement('button');
    btn.type = 'button';
    btn.className = 'blockr-gear-btn';
    btn.innerHTML = root.Blockr.icons && root.Blockr.icons.gear ? root.Blockr.icons.gear : '⚙';
    const band = document.createElement('div');
    band.className = 'blockr-settings blockr-settings--beak dd-popover';
    head.append(btn, band);
    const ui = root.Blockr;
    let open = false;
    const own = ui.gearTray ? ui.gearTray(band, btn, { label }) : null;
    const set = (/** @type {boolean} */ on) => {
      if (own) { own.set(on); return; }
      open = on;
      band.classList.toggle('blockr-settings--open', on);
      btn.classList.toggle('blockr-gear-active', on);
      btn.setAttribute('aria-expanded', String(on));
    };
    if (!own) {
      btn.setAttribute('aria-label', label);
      btn.setAttribute('aria-expanded', 'false');
      scope.on(btn, 'click', () => set(!open));
    }
    const sync = () => head.classList.toggle('vz-head--open',
                                             btn.getAttribute('aria-expanded') === 'true');
    scope.mutate(btn, sync, { attributes: true, attributeFilter: ['aria-expanded'] });
    scope.own({ dispose: () => set(false) });
    return { band, button: btn, set, isOpen: () => (own ? own.isOpen() : open) };
  };

  /**
   * Mount a gear into the tray element `el`.
   * @param {HTMLElement} el @param {VizGearOptions} opts @returns {VizGear}
   */
  V.gear.mount = (el, opts) => new Gear(el, opts);
})();
