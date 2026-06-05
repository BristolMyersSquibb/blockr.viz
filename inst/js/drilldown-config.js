/**
 * DrilldownConfig — the shared gear-popover config engine for blockr drilldown
 * blocks (chart, table, …). Host-agnostic: it renders a grouped, role-spec
 * driven popover (Mapping / Encoding / Presentation + a Drill-down section)
 * and calls back into a host for everything block-specific.
 *
 * A host provides (see DrilldownChart for the chart implementation):
 *   popoverEl()      -> the <body>-portaled popover element
 *   roles            -> the ROLES dict (key -> {label,kind,colType,...})
 *   config()         -> the mutable config object (mutated in place)
 *   columns()        -> column metadata array [{name,type,n_unique,label?}]
 *   context()        -> string key for colTypeBy/optionsBy/hintBy (chart=family)
 *   currentType()    -> the chart-type value (for type-conditional rows) or null
 *   sections()       -> {requiredMap,optionalMap,encoding,presentation} (current)
 *   sectionsForFamily(fam) -> the same for a specific family (carry-over)
 *   secondary        -> Set of paired-tail role keys (skipped in section loops)
 *   typeKey          -> the config key the type picker writes (e.g. 'chart_type')
 *   typeGroups       -> [{label, types:[...]}] for the type picker, or null
 *   familyFor(type)  -> family string for a type, or null (no families)
 *   entryRequired(role) -> mark a section role required (chart: metric in aggregated)
 *   drillAutoLabel() -> label for the drill "Auto" option, or null (no drill section)
 *   title            -> popover title string
 *   onChange()       -> a config value changed: re-render output + send to R
 *   onMults()        -> a slider changed (separate transport)
 *   onClearFilter()  -> clear the emitted filter
 *   ensureDefaults() -> fill block defaults after a type change
 *   afterTypeChange()-> e.g. update family CSS classes (optional)
 *   isOpen()         -> is the popover open
 *   reopen()         -> reopen the popover (keep it open across a re-render)
 *
 * Exposed as Blockr.DrilldownConfig (and window.DrilldownConfig).
 */
(() => {
  'use strict';

  class DrilldownConfig {
    constructor(host) {
      this.h = host;
      this._selects = {};
      this._added = new Set();      // optional roles the user added this session
      this._roleMemory = {};        // role key -> last chosen column (sticky)
    }

    // -- small helpers --------------------------------------------------------
    get _SECONDARY() { return this.h.secondary; }
    _hasVal(v) { return v !== null && v !== undefined && v !== '' && v !== '(none)'; }
    _cols() { return this.h.columns() || []; }
    _cfg() { return this.h.config(); }
    _role(key) { return this.h.roles[key]; }
    _colExists(name) { return this._cols().some(c => c.name === name); }

    _colFits(key, name) {
      const role = this._role(key);
      const c = this._cols().find(x => x.name === name);
      if (!c) return false;
      const ct = role.colTypeBy ? role.colTypeBy[this.h.context()] : role.colType;
      if (ct === 'num') return c.type === 'numeric';
      if (ct === 'cat') return c.type === 'categorical' || c.n_unique <= 50;
      return true;
    }

    _rememberRole(key, val) {
      const role = this._role(key);
      if (role && role.kind === 'column' && this._hasVal(val)) {
        this._roleMemory[key] = val;
      }
    }

    _colOptionsFor(key, { required }) {
      const role = this._role(key);
      const ct = role.colTypeBy ? role.colTypeBy[this.h.context()] : role.colType;
      let cols = this._cols();
      if (ct === 'num') cols = cols.filter(c => c.type === 'numeric');
      else if (ct === 'cat') cols = cols.filter(c => c.type === 'categorical' || c.n_unique <= 50);
      if (role.maxUnique) cols = cols.filter(c => c.n_unique <= role.maxUnique);
      const opts = cols.map(c => c.label ? { value: c.name, label: c.label } : c.name);
      if (role.allowCount) opts.unshift('.count');
      else if (!required) opts.unshift('(none)');
      return opts;
    }

    _selectOptionsFor(key) {
      const role = this._role(key);
      const raw = role.optionsBy ? (role.optionsBy[this.h.context()] || []) : (role.options || []);
      const out = [];
      for (const o of raw) {
        if (o === '#num') {
          for (const c of this._cols().filter(c => c.type === 'numeric')) {
            out.push(c.label ? { value: c.name, label: c.label } : c.name);
          }
        } else { out.push(o); }
      }
      return out;
    }

    _entryApplicable(key) {
      const spec = this.h.sections();
      const all = [...spec.encoding, ...spec.presentation];
      const e = all.find(x => (typeof x === 'string' ? x : x.role) === key);
      if (!e) return false;
      return typeof e === 'string' || !e.types || e.types.includes(this.h.currentType());
    }

    _fieldHelp(key, value) {
      const col = this._cols().find(c => c.name === value);
      if (col && col.label && col.label !== col.name) {
        return col.name + ' (' + col.label + ')';
      }
      if (this._hasVal(value)) return '';
      const role = this._role(key);
      if (!role) return '';
      return (role.hintBy && role.hintBy[this.h.context()]) || role.ph || '';
    }

    // -- render ---------------------------------------------------------------
    render() {
      const cfg = this._cfg();
      const spec = this.h.sections();
      const pop = this.h.popoverEl();

      for (const s of Object.values(this._selects)) {
        if (s && typeof s.destroy === 'function') s.destroy();
      }
      this._selects = {};

      pop.innerHTML = '';

      // a11y: the gear popover is a configuration dialog. Label it so screen
      // readers announce it; the title element is its accessible name.
      pop.setAttribute('role', 'dialog');
      pop.setAttribute('aria-label', (this.h.title || 'Settings'));

      const title = document.createElement('div');
      title.className = 'blockr-popover-label dd-popover-title';
      title.id = (pop.id || 'dd-pop') + '-title';
      title.textContent = this.h.title || 'Settings';
      pop.setAttribute('aria-labelledby', title.id);
      pop.appendChild(title);

      // Type picker (optional — chart only)
      if (this.h.typeGroups && this.h.typeGroups.length) {
        const typesRow = document.createElement('div');
        typesRow.className = 'blockr-popover-row dd-popover-types';
        for (const g of this.h.typeGroups) {
          const group = document.createElement('div');
          group.className = 'dd-type-group';
          const glabel = document.createElement('span');
          glabel.className = 'dd-type-group-label';
          glabel.textContent = g.label;
          group.appendChild(glabel);
          const btns = document.createElement('div');
          btns.className = 'dd-cfg-types';
          for (const t of g.types) {
            const btn = document.createElement('button');
            btn.className = 'dd-type-btn' + (t === cfg[this.h.typeKey] ? ' dd-type-active' : '');
            btn.textContent = t;
            btn.addEventListener('click', () => this._onType(t));
            btns.appendChild(btn);
          }
          group.appendChild(btns);
          typesRow.appendChild(group);
        }
        pop.appendChild(typesRow);
      }

      // Mapping: required rows, shown-optional rows, add menu (skipped whole
      // if the block has no mapping roles — e.g. the table).
      const shownOpt = spec.optionalMap.filter(k => this._hasVal(cfg[k]) || this._added.has(k));
      const remaining = spec.optionalMap.filter(k => !shownOpt.includes(k));
      if (spec.requiredMap.length || shownOpt.length || remaining.length) {
        const mapSec = this._sectionEl('Mapping');
        for (const key of spec.requiredMap) this._renderRole(mapSec, key, { required: true });
        for (const key of shownOpt) this._renderRole(mapSec, key, { removable: true });
        if (remaining.length) this._addMappingMenu(mapSec, remaining);
      }

      // Encoding / Presentation
      this._renderSection('Encoding', spec.encoding);
      this._renderSection('Presentation', spec.presentation);

      // Drill-down (optional — host opts out by returning null from drillAutoLabel)
      if (this.h.drillAutoLabel) this._renderDrillSection();

      if (this.h.afterTypeChange) this.h.afterTypeChange();
    }

    _sectionEl(titleText) {
      const sec = document.createElement('div');
      sec.className = 'dd-section';
      const h = document.createElement('div');
      h.className = 'dd-section-title';
      h.textContent = titleText;
      sec.appendChild(h);
      this.h.popoverEl().appendChild(sec);
      return sec;
    }

    _renderSection(titleText, entries) {
      const ct = this.h.currentType();
      const list = entries
        .map(e => (typeof e === 'string' ? { role: e } : e))
        .filter(e => !e.types || e.types.includes(ct))
        .filter(e => !this._SECONDARY.has(e.role));
      if (!list.length) return;
      const sec = this._sectionEl(titleText);
      for (const e of list) {
        const required = this.h.entryRequired ? this.h.entryRequired(e.role) : false;
        this._renderRole(sec, e.role, { required });
      }
    }

    _renderRole(container, key, opts = {}) {
      const role = this._role(key);
      const paired = !!(role.pairedWith && this._entryApplicable(role.pairedWith));
      const row = document.createElement('div');
      row.className = 'blockr-popover-row dd-form-row dd-role-' + key +
        (paired ? ' dd-role-paired' : '');

      const head = document.createElement('div');
      head.className = 'dd-row-head';
      const lbl = document.createElement('span');
      lbl.className = 'blockr-popover-label';
      lbl.textContent = role.label + (opts.required ? ' *' : '');
      head.appendChild(lbl);
      if (opts.removable) {
        const rm = document.createElement('button');
        rm.type = 'button';
        rm.className = 'dd-role-remove';
        rm.title = 'Remove ' + role.label;
        rm.innerHTML = '✕';
        rm.addEventListener('click', (e) => { e.stopPropagation(); this._removeRole(key); });
        head.appendChild(rm);
      }
      row.appendChild(head);

      const controls = document.createElement('div');
      controls.className = 'dd-row-controls';
      const helpEl = document.createElement('span');
      helpEl.className = 'dd-form-help';

      const markRequired = () => {
        row.classList.toggle('dd-role-required-empty',
          !!opts.required && !this._hasVal(this._cfg()[key]));
      };
      const setHelp = () => {
        if (role.kind !== 'column') { helpEl.style.display = 'none'; return; }
        const txt = this._fieldHelp(key, this._cfg()[key]);
        helpEl.textContent = txt;
        helpEl.style.display = txt ? '' : 'none';
      };

      this._buildControl(controls, key, { required: opts.required, onChange: () => { setHelp(); markRequired(); } });
      if (paired) this._buildControl(controls, role.pairedWith, { onChange: () => {} });
      row.appendChild(controls);
      row.appendChild(helpEl);
      setHelp();
      markRequired();
      container.appendChild(row);
    }

    _renderDrillSection() {
      const cfg = this._cfg();
      const d = cfg.drill;
      const on = this._hasVal(d);
      const sec = this._sectionEl('Drill-down');

      const tRow = document.createElement('div');
      tRow.className = 'blockr-popover-row dd-form-row';
      const tHead = document.createElement('div');
      tHead.className = 'dd-row-head';
      const tLbl = document.createElement('span');
      tLbl.className = 'blockr-popover-label';
      tLbl.textContent = 'Filter on selection';
      tHead.appendChild(tLbl);
      tRow.appendChild(tHead);
      const seg = document.createElement('div');
      seg.className = 'dd-segmented';
      for (const o of [{ v: 'off', l: 'Off' }, { v: 'on', l: 'On' }]) {
        const b = document.createElement('button');
        b.type = 'button';
        b.className = 'dd-seg-btn' + (((o.v === 'on') === on) ? ' dd-seg-active' : '');
        b.textContent = o.l;
        b.addEventListener('click', () => {
          if (o.v === 'off') cfg.drill = '';
          else if (!this._hasVal(cfg.drill)) cfg.drill = 'auto';
          const wasOpen = this.h.isOpen();
          this.render();
          if (wasOpen) setTimeout(() => this.h.reopen(), 0);
          this.h.onChange('drill'); this.h.onClearFilter();
        });
        seg.appendChild(b);
      }
      tRow.appendChild(seg);
      sec.appendChild(tRow);

      if (on) {
        const autoLabel = this.h.drillAutoLabel();
        const row = document.createElement('div');
        row.className = 'blockr-popover-row dd-form-row';
        const head = document.createElement('div');
        head.className = 'dd-row-head';
        const lbl = document.createElement('span');
        lbl.className = 'blockr-popover-label';
        lbl.textContent = 'Filter on';
        head.appendChild(lbl);
        row.appendChild(head);
        const controls = document.createElement('div');
        controls.className = 'dd-row-controls';
        const wrap = document.createElement('div');
        wrap.className = 'dd-picker-wrap';
        const colOpt = (c) => c.label ? { value: c.name, label: c.label } : c.name;
        const opts = [{ value: 'auto', label: autoLabel }, ...this._cols().map(colOpt)];
        const sel = (this._hasVal(cfg.drill) && cfg.drill !== 'auto') ? cfg.drill : 'auto';
        const onSel = (val) => { cfg.drill = val; this.h.onChange('drill'); this.h.onClearFilter(); };
        if (typeof Blockr !== 'undefined' && Blockr.Select) {
          this._selects['drill'] = Blockr.Select.single(wrap, { options: opts, selected: sel, onChange: onSel });
        } else {
          const s = document.createElement('select');
          s.className = 'dd-cfg-select';
          for (const o of opts) {
            const val = (typeof o === 'object' && o) ? o.value : o;
            const txt = (typeof o === 'object' && o && o.label) ? o.label : val;
            const op = document.createElement('option');
            op.value = val; op.textContent = txt;
            if (val === sel) op.selected = true;
            s.appendChild(op);
          }
          s.addEventListener('change', () => onSel(s.value));
          wrap.appendChild(s);
        }
        controls.appendChild(wrap);
        row.appendChild(controls);
        sec.appendChild(row);
      }
    }

    _buildControl(parent, key, { required, onChange } = {}) {
      const role = this._role(key);
      const cb = onChange || (() => {});
      const cfg = this._cfg();
      if (role.kind === 'column') {
        const opts = this._colOptionsFor(key, { required });
        const wrap = document.createElement('div');
        wrap.className = 'dd-picker-wrap';
        const sel = (cfg[key] && cfg[key] !== '(none)') ? cfg[key] : (required ? '' : '(none)');
        const onSel = (val) => {
          cfg[key] = (val === '(none)') ? '' : val;
          this._rememberRole(key, cfg[key]);
          cb();
          this.h.onChange(key);
          this.h.onClearFilter();
        };
        this._mkSelect(wrap, opts, sel, onSel, key, true);
        parent.appendChild(wrap);
      } else if (role.kind === 'select') {
        const opts = this._selectOptionsFor(key);
        const wrap = document.createElement('div');
        wrap.className = 'dd-picker-wrap';
        const cur = cfg[key];
        const selv = this._hasVal(cur) ? cur : ((typeof opts[0] === 'object' && opts[0]) ? opts[0].value : opts[0]);
        const onSel = (val) => { cfg[key] = val; cb(); this.h.onChange(key); };
        this._mkSelect(wrap, opts, selv, onSel, key, false);
        parent.appendChild(wrap);
      } else if (role.kind === 'segmented') {
        const seg = document.createElement('div');
        seg.className = 'dd-segmented';
        const cur = this._hasVal(cfg[key]) ? cfg[key] : role.options[0].value;
        for (const o of role.options) {
          const b = document.createElement('button');
          b.type = 'button';
          b.className = 'dd-seg-btn' + (o.value === cur ? ' dd-seg-active' : '');
          b.textContent = o.label;
          b.addEventListener('click', () => {
            cfg[key] = o.value;
            seg.querySelectorAll('.dd-seg-btn').forEach(x => x.classList.remove('dd-seg-active'));
            b.classList.add('dd-seg-active');
            cb(); this.h.onChange(key);
          });
          seg.appendChild(b);
        }
        parent.appendChild(seg);
      } else if (role.kind === 'slider') {
        this._buildSlider(parent, key);
      }
    }

    // Build a Blockr.Select (or native fallback). `decorate` shows
    // `name (label)` option text (column pickers); else just the label.
    _mkSelect(wrap, opts, selected, onSel, key, decorate) {
      if (typeof Blockr !== 'undefined' && Blockr.Select) {
        this._selects[key] = Blockr.Select.single(wrap, { options: opts, selected, onChange: onSel });
      } else {
        const s = document.createElement('select');
        s.className = 'dd-cfg-select';
        for (const o of opts) {
          const val = (typeof o === 'object' && o) ? o.value : o;
          const txt = (typeof o === 'object' && o && o.label)
            ? (decorate ? `${o.value} (${o.label})` : o.label) : val;
          const op = document.createElement('option');
          op.value = val; op.textContent = txt;
          if (val === selected) op.selected = true;
          s.appendChild(op);
        }
        s.addEventListener('change', () => onSel(s.value));
        wrap.appendChild(s);
      }
    }

    _buildSlider(parent, key) {
      const cfg = this._cfg();
      const init = cfg[key];
      const v0 = (typeof init === 'number' && isFinite(init)) ? init : 1.0;
      const wrap = document.createElement('div');
      wrap.className = 'dd-slider-wrap';
      const input = document.createElement('input');
      input.type = 'range';
      input.min = '0.5'; input.max = '3.0'; input.step = '0.1';
      input.value = String(v0);
      input.className = 'dd-slider';
      wrap.appendChild(input);
      const value = document.createElement('span');
      value.className = 'dd-slider-value';
      value.textContent = v0.toFixed(1) + '×';
      wrap.appendChild(value);
      let debounce;
      input.addEventListener('input', () => {
        const v = parseFloat(input.value);
        value.textContent = v.toFixed(1) + '×';
        cfg[key] = v;
        clearTimeout(debounce);
        debounce = setTimeout(() => this.h.onMults(), 150);
      });
      parent.appendChild(wrap);
    }

    _addMappingMenu(container, remaining) {
      const wrap = document.createElement('div');
      wrap.className = 'dd-add-wrap';
      const bar = document.createElement('div');
      bar.className = 'blockr-add-row';
      const btn = document.createElement('button');
      btn.type = 'button';
      btn.className = 'blockr-add-link dd-add-trigger';
      const plus = (typeof Blockr !== 'undefined' && Blockr.icons) ? Blockr.icons.plus : '+';
      btn.innerHTML = `<span class="blockr-add-icon">${plus}</span> Add mapping`;
      const menu = document.createElement('div');
      menu.className = 'dd-add-menu';
      menu.style.display = 'none';
      for (const key of remaining) {
        const item = document.createElement('button');
        item.type = 'button';
        item.className = 'dd-add-item';
        item.textContent = this._role(key).label;
        item.addEventListener('click', (e) => {
          e.stopPropagation();
          menu.style.display = 'none';
          this._addRole(key);
        });
        menu.appendChild(item);
      }
      btn.addEventListener('click', (e) => {
        e.stopPropagation();
        menu.style.display = (menu.style.display === 'none') ? '' : 'none';
      });
      bar.appendChild(btn);
      wrap.appendChild(bar);
      wrap.appendChild(menu);
      container.appendChild(wrap);
    }

    _addRole(key) {
      const cfg = this._cfg();
      this._added.add(key);
      if (!this._hasVal(cfg[key]) && this._roleMemory[key] &&
          this._colExists(this._roleMemory[key]) && this._colFits(key, this._roleMemory[key])) {
        cfg[key] = this._roleMemory[key];
        this.h.onChange(key);
      }
      const wasOpen = this.h.isOpen();
      this.render();
      if (wasOpen) setTimeout(() => this.h.reopen(), 0);
    }

    _removeRole(key) {
      const cfg = this._cfg();
      cfg[key] = '';
      this._added.delete(key);
      delete this._roleMemory[key];
      const wasOpen = this.h.isOpen();
      this.render();
      if (wasOpen) setTimeout(() => this.h.reopen(), 0);
      this.h.onChange(key);
      this.h.onClearFilter();
    }

    // Type/family switch (chart). No-op-ish when there are no families.
    _onType(t) {
      const cfg = this._cfg();
      const oldFam = this.h.familyFor ? this.h.familyFor(cfg[this.h.typeKey]) : null;
      cfg[this.h.typeKey] = t;
      const newFam = this.h.familyFor ? this.h.familyFor(t) : null;
      if (this.h.familyFor && oldFam !== newFam) {
        this._carryRoles(newFam);
        this.h.onClearFilter();
      }
      if (this.h.ensureDefaults) this.h.ensureDefaults();
      const wasOpen = this.h.isOpen();
      this.render();
      if (wasOpen) setTimeout(() => this.h.reopen(), 0);
      this.h.onChange(this.h.typeKey);
    }

    // Identity-carry + sticky memory across a family switch.
    _carryRoles(newFam) {
      const spec = this.h.sectionsForFamily(newFam);
      const cfg = this._cfg();
      const keep = new Set([...spec.requiredMap, ...spec.optionalMap]);
      for (const key of Object.keys(this.h.roles)) {
        if (this.h.roles[key].kind !== 'column') continue;
        if (this.h.carryKeep && this.h.carryKeep.includes(key)) continue;
        const v = cfg[key];
        if (!this._hasVal(v)) continue;
        if (!(keep.has(key) && this._colFits(key, v))) {
          this._roleMemory[key] = v;
          cfg[key] = '';
        }
      }
      for (const key of spec.requiredMap) {
        const mem = this._roleMemory[key];
        if (!this._hasVal(cfg[key]) && mem && this._colExists(mem) && this._colFits(key, mem)) {
          cfg[key] = mem;
        }
      }
    }
  }

  const ns = (typeof Blockr !== 'undefined') ? Blockr : (window.Blockr = window.Blockr || {});
  ns.DrilldownConfig = DrilldownConfig;
  window.DrilldownConfig = DrilldownConfig;
})();
