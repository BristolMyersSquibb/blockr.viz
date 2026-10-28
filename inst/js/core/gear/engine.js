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

  /**
   * How wide a row is in the section's grid (gear.css): tiles, editors,
   * templates and multi-line text take the whole width; a control with a
   * second one beside it (drill, highlight) two cells; a checkbox sits on
   * the controls' line.
   * @param {VizRowCtx} ctx @param {boolean} boxed
   */
  const rowClass = (ctx, boxed) => {
    const control = ctx.arg.gear ? ctx.arg.gear.control : '';
    if (control === 'tiles' || control === 'custom' || control === 'template') return ' vz-row--wide';
    if (control === 'code' || control === 'script') return ' vz-row--code';
    if ((control === 'toggle' && ctx.arg.type.name !== 'bool') || ctx.arg.type.name === 'named') {
      return ' vz-row--paired';
    }
    return boxed ? ' vz-row--check' : '';
  };

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
      /** Optional mappings added from the Add menu, shown while unset. */
      /** @type {Set<string>} */
      this._added = new Set();
      /** @type {string | null} */
      this._group = null;
      /** @type {VizScope | null} */
      this._renderScope = null;
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
     * tray as it is, open lists included, R's echo of an edit among them.
     * The rows reach the tray only through the host's choices, so those
     * count, not the data revision a new table layout brings. The titles'
     * arguments, which differ by layout, show only in a template row's
     * warning; the `@` menu reads them when it opens (ctx.titles is live). */
    _key() {
      const values = this.values();
      const host = this.host;
      const t = this._titles;
      const shown = Object.keys(this.spec).filter((k) => this._shown(k, values)).map((k) => [
        k, this.label(k, values), !!(host.required && host.required(k, values)),
        (host.choices && host.choices(k, values, this._cols)) || null,
        /** @type {VizGearField} */ (this.spec[k].gear).control === 'template'
          ? V.titles.unknownTokens(String(values[k] ?? ''), t, this._cols) : null
      ]);
      return JSON.stringify([values, this._cols, [t.title, t.subtitle, t.caption],
                             Array.from(this._added), shown]);
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
      if (arg.gear.control === 'custom' && !this.host.custom) return false;
      if (this.typeKey && !S.applies(arg, values[this.typeKey])) return false;
      if (arg.gear.when && !C.isSet(values[arg.gear.when])) return false;
      return !this.host.visible || this.host.visible(key, values) !== false;
    }

    /** @param {string} key @param {VizValues} values @param {VizScope} scope
     * @returns {VizRowCtx} */
    _ctx(key, values, scope) {
      const required = !!(this.host.required && this.host.required(key, values));
      const gear = this;
      return {
        key, arg: this.spec[key], label: this.label(key, values), value: values[key], values,
        columns: this._cols, required, scope, host: this.host,
        // The latest payload's, as a payload may leave the row drawn.
        get titles() { return gear._titles; },
        labelOf: (k) => this.label(k, values),
        set: (v) => this.set({ [key]: v })
      };
    }

    render() {
      if (this.disposed) return;
      const focus = this._saveFocus();
      if (this._renderScope) this._renderScope.dispose();
      const rs = this._renderScope = new V.Scope();
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
      const g = /** @type {VizGearField} */ (ctx.arg.gear);
      const boxed = g.control === 'toggle' ||
        (g.control === 'segmented' && ctx.arg.type.name === 'bool');
      const row = document.createElement('div');
      row.className = 'vz-row' + rowClass(ctx, boxed);
      row.setAttribute('data-role', ctx.key);
      if (!boxed && ctx.label !== section) {
        const head = document.createElement('div');
        head.className = 'vz-row-head';
        const lbl = document.createElement('span');
        lbl.className = 'blockr-label';
        lbl.textContent = ctx.label + (ctx.required ? ' *' : '');
        head.appendChild(lbl);
        row.appendChild(head);
      }
      const controls = document.createElement('div');
      controls.className = 'vz-row-controls';
      row.appendChild(controls);
      body.appendChild(row);
      C.build(controls, ctx);
      // An empty required pick takes blockr.ui's amber cue. A custom editor
      // knows what empty means for its value and sets the cue itself.
      const ui = /** @type {VizUi} */ (root.Blockr);
      if (ctx.required && g.control !== 'custom' && ui.setRequiredEmpty) {
        ui.setRequiredEmpty(row, !C.isSet(ctx.value));
      }
    }

    /** The optional mappings not shown, as a menu that adds a row.
     * @param {HTMLElement} body @param {string[]} keys @param {VizValues} values
     * @param {VizScope} scope */
    _addMenu(body, keys, values, scope) {
      const row = document.createElement('div');
      row.className = 'vz-row vz-add vz-add-mapping';
      body.appendChild(row);
      const add = (/** @type {string} */ key) => {
        if (!key) return;
        this._added.add(key);
        this.render();
      };
      const labels = keys.map((k) => this.label(k, values));
      const inst = /** @type {VizUi} */ (root.Blockr).Select.single(row, {
        options: labels, selected: '', allowEmpty: true, placeholder: '+ Add a mapping',
        label: 'Add a mapping', onChange: (/** @type {string} */ v) => add(keys[labels.indexOf(v)])
      });
      scope.own({ dispose: () => inst.destroy() });
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
      if (!arg || !arg.gear || arg.gear.control === 'custom') return null;
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
      if (['map', 'records', 'records_by', 'named'].includes(name)) return null;
      // Free text stays in the gear; a string picked from a list is a menu.
      if ((name === 'string' || name === 'template') && arg.gear.control !== 'select') return null;
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
      if (this._renderScope) this._renderScope.dispose();
      this._renderScope = null;
      this._pending.clear();
      this.el.textContent = '';
    }
  }

  /**
   * A gear button and its tray, appended to a view's header `head`, opened
   * and closed by blockr.ui's Blockr.gearTray. The header gets
   * `vz-head--open` while the tray is open; a tray left open is closed when
   * `scope` goes, which takes it off blockr.ui's dismiss stack.
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
    band.className = 'blockr-settings blockr-settings--beak vz-tray';
    head.append(btn, band);
    const tray = /** @type {VizUi} */ (root.Blockr).gearTray(band, btn, { label });
    const set = (/** @type {boolean} */ on) => tray.set(on);
    const sync = () => head.classList.toggle('vz-head--open',
                                             btn.getAttribute('aria-expanded') === 'true');
    scope.mutate(btn, sync, { attributes: true, attributeFilter: ['aria-expanded'] });
    scope.own({ dispose: () => set(false) });
    return { band, button: btn, set, isOpen: () => tray.isOpen() };
  };

  /**
   * Mount a gear into the tray element `el`.
   * @param {HTMLElement} el @param {VizGearOptions} opts @returns {VizGear}
   */
  V.gear.mount = (el, opts) => new Gear(el, opts);
})();
