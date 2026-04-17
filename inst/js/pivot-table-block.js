/**
 * PivotTableBlock — JS-driven input binding for blockr.bi::pivot_table_block.
 *
 * Main: rows (multi), cols (multi), measures (multi), agg_fun (single).
 * Gear: round-to-digits text input.
 *
 * Depends on: blockr-core.js, blockr-select.js (from blockr.dplyr), blockr-blocks.css.
 */
(() => {
  'use strict';

  const AGG_OPTIONS = [
    { value: 'sum',    label: 'Sum' },
    { value: 'mean',   label: 'Mean' },
    { value: 'median', label: 'Median' },
    { value: 'min',    label: 'Min' },
    { value: 'max',    label: 'Max' },
    { value: 'n',      label: 'Count' }
  ];

  class PivotTableBlock {
    constructor(el) {
      this.el = el;
      this._dimensions = [];
      this._measures = [];
      this._callback = null;
      this._submitted = false;
      this._debounceTimer = null;

      this._state = {
        rows: [],
        cols: [],
        measures: [],
        agg_fun: 'sum',
        digits: ''
      };

      this._selects = {};
      this._buildDOM();
    }

    _autoSubmit() {
      clearTimeout(this._debounceTimer);
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'ptb-card';
      this.card.style.position = 'relative';
      this.el.appendChild(this.card);

      // Gear button (top-right)
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

      // Primary grid
      const grid = document.createElement('div');
      grid.className = 'ptb-grid';
      this.card.appendChild(grid);

      this._selects.rows = this._buildMultiField(grid, {
        label: 'Rows',
        placeholder: 'Row dimensions\u2026',
        stateKey: 'rows'
      });

      this._selects.cols = this._buildMultiField(grid, {
        label: 'Columns',
        placeholder: 'Column dimensions\u2026',
        stateKey: 'cols'
      });

      this._selects.measures = this._buildMultiField(grid, {
        label: 'Measures',
        placeholder: 'Numeric columns\u2026',
        stateKey: 'measures',
        usesMeasures: true
      });

      // Aggregation (single)
      const aggWrap = document.createElement('div');
      aggWrap.className = 'ptb-field';
      const aggLabel = document.createElement('label');
      aggLabel.className = 'blockr-label';
      aggLabel.textContent = 'Aggregation';
      aggWrap.appendChild(aggLabel);
      this._selects.agg_fun = Blockr.Select.single(aggWrap, {
        options: AGG_OPTIONS,
        selected: this._state.agg_fun,
        placeholder: 'Function\u2026',
        onChange: (value) => {
          this._state.agg_fun = value || 'sum';
          this._autoSubmit();
        }
      });
      this._selects.agg_fun.el.classList.add('blockr-select--bordered');
      grid.appendChild(aggWrap);

      // Aggregating-over hint (set from R via custom message)
      this.hintEl = document.createElement('div');
      this.hintEl.className = 'ptb-hint text-muted';
      this.hintEl.style.fontSize = '0.75rem';
      this.hintEl.style.color = '#9ca3af';
      this.hintEl.style.marginTop = '8px';
      this.hintEl.textContent = '';
      this.card.appendChild(this.hintEl);

      // Gear popover
      this.popover = document.createElement('div');
      this.popover.className = 'blockr-popover';
      this.popover.style.display = 'none';

      const popTitle = document.createElement('div');
      popTitle.className = 'blockr-popover-label';
      popTitle.style.fontWeight = '600';
      popTitle.style.color = '#374151';
      popTitle.style.marginBottom = '10px';
      popTitle.textContent = 'Advanced';
      this.popover.appendChild(popTitle);

      this._addDigitsRow();

      this.card.appendChild(this.popover);

      document.addEventListener('click', (e) => {
        if (!this.card.contains(e.target)) {
          this.popover.style.display = 'none';
          this.gearBtn.classList.remove('blockr-gear-active');
        }
      });
    }

    _buildMultiField(grid, { label, placeholder, stateKey, usesMeasures }) {
      const wrap = document.createElement('div');
      wrap.className = 'ptb-field';
      const lbl = document.createElement('label');
      lbl.className = 'blockr-label';
      lbl.textContent = label;
      wrap.appendChild(lbl);
      const sel = Blockr.Select.multi(wrap, {
        options: [],
        selected: [],
        placeholder,
        onChange: (selected) => {
          this._state[stateKey] = selected || [];
          this._autoSubmit();
        }
      });
      sel.el.classList.add('blockr-select--bordered');
      sel._usesMeasures = !!usesMeasures;
      grid.appendChild(wrap);
      return sel;
    }

    _addDigitsRow() {
      const row = document.createElement('div');
      row.className = 'blockr-popover-row';
      row.style.display = 'flex';
      row.style.alignItems = 'center';
      row.style.gap = '10px';
      row.style.marginBottom = '8px';

      const label = document.createElement('span');
      label.className = 'blockr-popover-label';
      label.textContent = 'Round to:';
      label.style.marginBottom = '0';
      label.style.flexShrink = '0';
      row.appendChild(label);

      const input = document.createElement('input');
      input.type = 'text';
      input.className = 'blockr-popover-input';
      input.placeholder = 'digits (empty = no rounding)';
      input.value = this._state.digits || '';
      input.style.flex = '1';
      input.addEventListener('input', () => {
        this._state.digits = input.value;
        this._autoSubmit();
      });
      input.addEventListener('click', (e) => e.stopPropagation());
      row.appendChild(input);

      this.popover.appendChild(row);
      this._digitsInput = input;
    }

    _togglePopover() {
      const showing = this.popover.style.display === 'none';
      this.popover.style.display = showing ? 'block' : 'none';
      this.gearBtn.classList.toggle('blockr-gear-active', showing);
    }

    _submit() {
      this._submitted = true;
      this._callback?.(true);
    }

    getValue() {
      if (!this._submitted) return null;
      return {
        rows: this._state.rows,
        cols: this._state.cols,
        measures: this._state.measures,
        agg_fun: this._state.agg_fun,
        digits: this._state.digits
      };
    }

    setState(state) {
      if (!state) return;
      const arrayKeys = ['rows', 'cols', 'measures'];
      for (const k of arrayKeys) {
        if (Array.isArray(state[k])) this._state[k] = state[k].slice();
      }
      if (typeof state.agg_fun === 'string') this._state.agg_fun = state.agg_fun;
      if (typeof state.digits === 'string' || typeof state.digits === 'number') {
        this._state.digits = String(state.digits);
        if (this._digitsInput) this._digitsInput.value = this._state.digits;
      }

      if (this._selects.rows) {
        this._selects.rows.setOptions(this._dimensions, this._state.rows);
      }
      if (this._selects.cols) {
        this._selects.cols.setOptions(this._dimensions, this._state.cols);
      }
      if (this._selects.measures) {
        this._selects.measures.setOptions(this._measures, this._state.measures);
      }
      if (this._selects.agg_fun) {
        this._selects.agg_fun.setOptions(AGG_OPTIONS, this._state.agg_fun);
      }
    }

    updateColumns(msg) {
      this._dimensions = Array.isArray(msg.dimensions) ? msg.dimensions : [];
      this._measures = Array.isArray(msg.measures) ? msg.measures : [];
      if (this._selects.rows) {
        this._selects.rows.setOptions(this._dimensions, this._state.rows);
        this._state.rows = this._selects.rows.getValue();
      }
      if (this._selects.cols) {
        this._selects.cols.setOptions(this._dimensions, this._state.cols);
        this._state.cols = this._selects.cols.getValue();
      }
      if (this._selects.measures) {
        this._selects.measures.setOptions(this._measures, this._state.measures);
        this._state.measures = this._selects.measures.getValue();
      }
      this._autoSubmit();
    }

    setHint(text) {
      if (this.hintEl) this.hintEl.textContent = text || '';
    }
  }

  // --- Shiny input binding ---
  const binding = new Shiny.InputBinding();
  Object.assign(binding, {
    find: (scope) => $(scope).find('.pivot-table-block-container'),
    getId: (el) => el.id || null,
    getValue: (el) => el._block?.getValue() ?? null,
    setValue: (el, value) => el._block?.setState(value),
    subscribe: (el, callback) => {
      if (el._block) el._block._callback = () => callback(true);
    },
    unsubscribe: (el) => {
      if (el._block) el._block._callback = null;
    },
    initialize: (el) => {
      el._block = new PivotTableBlock(el);
      if (el._pendingColumns) {
        el._block.updateColumns(el._pendingColumns);
        delete el._pendingColumns;
      }
      if (el._pendingState) {
        el._block.setState(el._pendingState);
        delete el._pendingState;
      }
      if (el._pendingHint) {
        el._block.setHint(el._pendingHint);
        delete el._pendingHint;
      }
    }
  });
  Shiny.inputBindings.register(binding, 'blockr.pivot_table');

  const waitForEl = (id, cb) => {
    const el = document.getElementById(id);
    if (el) return cb(el);
    let attempts = 0;
    const t = setInterval(() => {
      attempts++;
      const found = document.getElementById(id);
      if (found) { clearInterval(t); cb(found); }
      if (attempts > 50) clearInterval(t);
    }, 100);
  };

  Shiny.addCustomMessageHandler('pivot-table-columns', (msg) => {
    waitForEl(msg.id, (el) => {
      if (el._block) el._block.updateColumns(msg);
      else el._pendingColumns = msg;
    });
  });

  Shiny.addCustomMessageHandler('pivot-table-update', (msg) => {
    waitForEl(msg.id, (el) => {
      if (el._block) el._block.setState(msg.state);
      else el._pendingState = msg.state;
    });
  });

  Shiny.addCustomMessageHandler('pivot-table-aggregating-over', (msg) => {
    waitForEl(msg.id, (el) => {
      if (el._block) el._block.setHint(msg.text);
      else el._pendingHint = msg.text;
    });
  });
})();
