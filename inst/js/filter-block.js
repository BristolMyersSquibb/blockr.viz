/**
 * FilterBlock — minimal value filter for data frames.
 *
 * Main UI (card body): one row per active column, each with a Blockr.Select
 *   (single or multi depending on the column's mode).
 * Gear popover:
 *   - "Fields" multi-select: which columns are active.
 *   - Per-column mode toggle: click-through pill Single <-> Multi.
 *
 * Depends on: blockr-core.js, blockr-select.js (from blockr.dplyr).
 */
(() => {
  'use strict';

  const optValueOf = (o) => (typeof o === 'object' && o !== null ? o.value : o);

  class FilterBlock {
    constructor(el) {
      this.el = el;
      this.columns = [];     // [{value, label}]  (all columns of incoming df)
      this.colLabels = {};   // value -> label
      this.colValues = {};   // col -> options array ([string] or [{value,label}])
      this.active = [];      // active column names, ordered
      this.modes = {};       // col -> "single" | "multi"
      this.values = {};      // col -> array of selected string values

      this._callback = null;
      this._submitted = false;
      this._debounceTimer = null;
      this._popoverOpen = false;

      this._fieldSelect = null;
      this._modePills = {};
      this._bodySelects = {};

      this._buildDOM();
      this._renderBody();
    }

    _autoSubmit() {
      clearTimeout(this._debounceTimer);
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'bi-filter-card';
      this.card.style.position = 'relative';
      this.el.appendChild(this.card);

      // Gear header (top-right) — identical markup to blockr.dplyr blocks.
      const gearHeader = document.createElement('div');
      gearHeader.className = 'blockr-gear-header';
      this.gearBtn = document.createElement('button');
      this.gearBtn.type = 'button';
      this.gearBtn.className = 'blockr-gear-btn';
      this.gearBtn.innerHTML = Blockr.icons.gear;
      this.gearBtn.title = 'Advanced settings';
      this.gearBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        this._togglePopover();
      });
      gearHeader.appendChild(this.gearBtn);
      this.card.appendChild(gearHeader);

      this._buildPopover();

      this.bodyEl = document.createElement('div');
      this.bodyEl.className = 'bi-filter-body';
      this.card.appendChild(this.bodyEl);

      // Click-outside closes popover.
      document.addEventListener('click', (e) => {
        if (this._popoverOpen && this.popoverEl &&
            !this.popoverEl.contains(e.target) &&
            !this.gearBtn.contains(e.target)) {
          this._closePopover();
        }
      });
    }

    _buildPopover() {
      this.popoverEl = document.createElement('div');
      this.popoverEl.className = 'blockr-popover';
      this.popoverEl.style.display = 'none';

      // Fields multi-select
      const fieldsRow = document.createElement('div');
      fieldsRow.className = 'blockr-popover-row';
      const fieldsLabel = document.createElement('label');
      fieldsLabel.className = 'blockr-popover-label';
      fieldsLabel.textContent = 'Fields';
      fieldsRow.appendChild(fieldsLabel);

      const fieldsWrap = document.createElement('div');
      fieldsWrap.className = 'blockr-popover-select-wrap';
      fieldsRow.appendChild(fieldsWrap);

      this._fieldSelect = Blockr.Select.multi(fieldsWrap, {
        options: this.columns,
        selected: this.active.slice(),
        placeholder: 'Select columns\u2026',
        reorderable: false,
        onChange: (selected) => {
          this._setActive(selected);
          this._autoSubmit();
        }
      });

      this.popoverEl.appendChild(fieldsRow);

      // Per-column mode toggles
      this.modesContainer = document.createElement('div');
      this.modesContainer.className = 'bi-filter-modes';
      this.popoverEl.appendChild(this.modesContainer);

      this.card.appendChild(this.popoverEl);
    }

    _rebuildModes() {
      this.modesContainer.innerHTML = '';
      this._modePills = {};

      this.active.forEach((col) => {
        const row = document.createElement('div');
        row.className = 'blockr-popover-row bi-filter-mode-row';

        const label = document.createElement('label');
        label.className = 'blockr-popover-label';
        label.textContent = this.colLabels[col] || col;
        row.appendChild(label);

        const mode = this.modes[col] || 'single';
        const pill = document.createElement('button');
        pill.type = 'button';
        pill.className = 'blockr-pill blockr-popover-toggle';
        this._stylePill(pill, mode);
        pill.title = 'Toggle between single- and multi-select';
        pill.addEventListener('click', () => {
          const newMode = (this.modes[col] === 'multi') ? 'single' : 'multi';
          this.modes[col] = newMode;
          this._stylePill(pill, newMode);
          if (newMode === 'single') {
            const v = this.values[col] || [];
            if (v.length !== 1) {
              const first = this._firstValueOf(col);
              this.values[col] = first != null ? [String(first)] : [];
            }
          }
          this._renderBody();
          this._autoSubmit();
        });

        row.appendChild(pill);
        this._modePills[col] = pill;
        this.modesContainer.appendChild(row);
      });
    }

    _stylePill(pill, mode) {
      pill.textContent = mode === 'multi' ? 'Multi' : 'Single';
      pill.classList.toggle('blockr-popover-toggle-active', mode === 'multi');
    }

    _firstValueOf(col) {
      const opts = this.colValues[col] || [];
      if (opts.length === 0) return null;
      return optValueOf(opts[0]);
    }

    _setActive(newActive) {
      const newSet = new Set(newActive);
      Object.keys(this.modes).forEach((c) => {
        if (!newSet.has(c)) delete this.modes[c];
      });
      Object.keys(this.values).forEach((c) => {
        if (!newSet.has(c)) delete this.values[c];
      });
      newActive.forEach((col) => {
        if (!this.modes[col]) this.modes[col] = 'single';
        if (!this.values[col] || this.values[col].length === 0) {
          if (this.modes[col] === 'single') {
            const first = this._firstValueOf(col);
            this.values[col] = first != null ? [String(first)] : [];
          } else {
            this.values[col] = [];
          }
        }
      });
      this.active = newActive.slice();
      this._rebuildModes();
      this._renderBody();
    }

    _renderBody() {
      Object.values(this._bodySelects).forEach((s) => s && s.destroy && s.destroy());
      this._bodySelects = {};
      this.bodyEl.innerHTML = '';

      if (this.active.length === 0) {
        const empty = document.createElement('div');
        empty.className = 'bi-filter-empty';
        empty.textContent = 'No filters. Click the gear to add fields.';
        this.bodyEl.appendChild(empty);
        return;
      }

      this.active.forEach((col) => {
        const row = document.createElement('div');
        row.className = 'blockr-row bi-filter-row';

        const labelWrap = document.createElement('div');
        labelWrap.className = 'bi-filter-label-wrap';
        const label = document.createElement('label');
        label.className = 'blockr-label';
        label.appendChild(document.createTextNode(col));
        const sub = this.colLabels[col];
        if (sub) {
          const subEl = document.createElement('span');
          subEl.className = 'bi-filter-label-sublabel';
          subEl.textContent = sub;
          label.appendChild(subEl);
        }
        labelWrap.appendChild(label);
        row.appendChild(labelWrap);

        const wrap = document.createElement('div');
        wrap.className = 'bi-filter-select-wrap';
        row.appendChild(wrap);

        const mode = this.modes[col] || 'single';
        const opts = this.colValues[col] || [];
        const sel = this.values[col] || [];

        if (mode === 'single') {
          this._bodySelects[col] = Blockr.Select.single(wrap, {
            options: opts,
            selected: sel[0] != null ? sel[0] : null,
            placeholder: 'Select value\u2026',
            onChange: (v) => {
              if (v == null || v === '') {
                const first = this._firstValueOf(col);
                this.values[col] = first != null ? [String(first)] : [];
              } else {
                this.values[col] = [String(v)];
              }
              this._autoSubmit();
            }
          });
        } else {
          this._bodySelects[col] = Blockr.Select.multi(wrap, {
            options: opts,
            selected: sel.slice(),
            placeholder: 'Select values\u2026',
            reorderable: false,
            onChange: (vals) => {
              this.values[col] = vals.map(String);
              this._autoSubmit();
            }
          });
        }

        this.bodyEl.appendChild(row);
      });
    }

    _togglePopover() {
      this._popoverOpen ? this._closePopover() : this._openPopover();
    }

    _openPopover() {
      this.popoverEl.style.display = 'block';
      this._popoverOpen = true;
      this.gearBtn.classList.add('blockr-gear-active');
    }

    _closePopover() {
      this.popoverEl.style.display = 'none';
      this._popoverOpen = false;
      this.gearBtn.classList.remove('blockr-gear-active');
    }

    _compose() {
      const valuesOut = {};
      Object.keys(this.values).forEach((k) => {
        valuesOut[k] = this.values[k].slice();
      });
      return {
        columns: this.active.slice(),
        modes: Object.assign({}, this.modes),
        values: valuesOut
      };
    }

    _submit() {
      this._submitted = true;
      if (this._callback) this._callback(true);
    }

    getValue() {
      if (!this._submitted) return null;
      return this._compose();
    }

    setState(state) {
      const colsRaw = (state && state.columns) || [];
      const cols = Array.isArray(colsRaw)
        ? colsRaw
        : (colsRaw != null && colsRaw !== '' ? [colsRaw] : []);
      const modes = (state && state.modes) || {};
      const vals = (state && state.values) || {};
      this.active = cols.slice();
      this.modes = Object.assign({}, modes);
      this.values = {};
      Object.keys(vals).forEach((k) => {
        const v = vals[k];
        this.values[k] = Array.isArray(v) ? v.map(String) : (v != null ? [String(v)] : []);
      });
      this.active.forEach((col) => {
        if (!this.modes[col]) this.modes[col] = 'single';
        if (!this.values[col]) this.values[col] = [];
      });
      if (this._fieldSelect) {
        this._fieldSelect.setOptions(this.columns, this.active);
      }
      this._rebuildModes();
      this._renderBody();
    }

    updateColumns(payload) {
      this.columns = (payload && payload.columns) || [];
      this.colValues = (payload && payload.values) || {};
      this.colLabels = {};
      this.columns.forEach((c) => {
        const v = optValueOf(c);
        const l = (typeof c === 'object' && c !== null && c.label) ? c.label : '';
        this.colLabels[v] = l;
      });

      const valid = new Set(this.columns.map(optValueOf));
      const activeArr = Array.isArray(this.active)
        ? this.active
        : (this.active != null && this.active !== '' ? [this.active] : []);
      this.active = activeArr.filter((c) => valid.has(c));
      Object.keys(this.modes).forEach((c) => {
        if (!valid.has(c)) delete this.modes[c];
      });
      Object.keys(this.values).forEach((c) => {
        if (!valid.has(c)) delete this.values[c];
      });

      // Drop stale values not present in column's options.
      this.active.forEach((col) => {
        const validVals = new Set((this.colValues[col] || []).map(optValueOf).map(String));
        this.values[col] = (this.values[col] || []).filter((v) => validVals.has(String(v)));
        const mode = this.modes[col] || 'single';
        if (mode === 'single' && this.values[col].length === 0) {
          const first = this._firstValueOf(col);
          if (first != null) this.values[col] = [String(first)];
        }
      });

      if (this._fieldSelect) {
        this._fieldSelect.setOptions(this.columns, this.active);
      }
      this._rebuildModes();
      this._renderBody();
    }
  }

  // --- Shiny input binding ---

  const binding = new Shiny.InputBinding();

  Object.assign(binding, {
    find: (scope) => $(scope).find('.bi-filter-container'),
    getId: (el) => el.id || null,
    getValue: (el) => (el._block ? el._block.getValue() : null),
    setValue: (el, value) => { if (el._block) el._block.setState(value); },
    subscribe: (el, callback) => {
      if (el._block) el._block._callback = () => callback(true);
    },
    unsubscribe: (el) => {
      if (el._block) el._block._callback = null;
    },
    initialize: (el) => {
      el._block = new FilterBlock(el);
      if (el._pendingColumns) {
        el._block.updateColumns(el._pendingColumns);
        delete el._pendingColumns;
      }
      if (el._pendingState) {
        el._block.setState(el._pendingState);
        delete el._pendingState;
      }
    },
    receiveMessage: (el, data) => {
      if (data && data.state && el._block) el._block.setState(data.state);
    }
  });

  Shiny.inputBindings.register(binding, 'blockr.bi.filter');

  const pumpColumns = (id, payload) => {
    const el = document.getElementById(id);
    if (el && el._block) {
      el._block.updateColumns(payload);
    } else if (el) {
      el._pendingColumns = payload;
    } else {
      let attempts = 0;
      const t = setInterval(() => {
        attempts++;
        const el2 = document.getElementById(id);
        if (el2 && el2._block) { el2._block.updateColumns(payload); clearInterval(t); }
        else if (el2) { el2._pendingColumns = payload; clearInterval(t); }
        if (attempts > 50) clearInterval(t);
      }, 100);
    }
  };

  Shiny.addCustomMessageHandler('bi-filter-columns', (msg) => {
    pumpColumns(msg.id, { columns: msg.columns, values: msg.values });
  });

  Shiny.addCustomMessageHandler('bi-filter-update', (msg) => {
    const el = document.getElementById(msg.id);
    if (el && el._block) {
      el._block.setState(msg.state);
    } else if (el) {
      el._pendingState = msg.state;
    }
  });
})();
