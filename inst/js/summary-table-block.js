/**
 * SummaryTableBlock — JS-driven input binding for blockr.bi::summary_table_block.
 *
 * Main: three Blockr.Select widgets (vars multi / sections multi / by multi, max 2).
 * Gear: stats preset, overall toggle + label, indent_details, nest_hierarchies.
 *
 * Depends on: blockr-core.js, blockr-select.js (from blockr.dplyr), blockr-blocks.css.
 */
(() => {
  'use strict';

  class SummaryTableBlock {
    constructor(el) {
      this.el = el;
      this._varCols = [];
      this._catCols = [];
      this._callback = null;
      this._submitted = false;
      this._debounceTimer = null;

      this._state = {
        vars: [],
        sections: [],
        by: [],
        stats: 'compact',
        add_overall: false,
        overall_label: 'Total',
        indent_details: true,
        nest_hierarchies: false
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
      this.card.className = 'stb-card';
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
      grid.className = 'stb-grid';
      this.card.appendChild(grid);

      // Vars (full-width)
      const varsWrap = document.createElement('div');
      varsWrap.className = 'stb-field';
      varsWrap.style.gridColumn = '1 / -1';
      const varsLabel = document.createElement('label');
      varsLabel.className = 'blockr-label';
      varsLabel.textContent = 'Variables';
      varsWrap.appendChild(varsLabel);
      this._selects.vars = Blockr.Select.multi(varsWrap, {
        options: [],
        selected: [],
        placeholder: 'Columns to summarise\u2026',
        onChange: (selected) => {
          this._state.vars = selected || [];
          this._autoSubmit();
        }
      });
      this._selects.vars.el.classList.add('blockr-select--bordered');
      grid.appendChild(varsWrap);

      // Sections
      const sectionsWrap = document.createElement('div');
      sectionsWrap.className = 'stb-field';
      const sectionsLabel = document.createElement('label');
      sectionsLabel.className = 'blockr-label';
      sectionsLabel.textContent = 'Sections (outer grouping)';
      sectionsWrap.appendChild(sectionsLabel);
      this._selects.sections = Blockr.Select.multi(sectionsWrap, {
        options: [],
        selected: [],
        placeholder: 'Optional outer section columns\u2026',
        onChange: (selected) => {
          this._state.sections = selected || [];
          this._autoSubmit();
        }
      });
      this._selects.sections.el.classList.add('blockr-select--bordered');
      grid.appendChild(sectionsWrap);

      // By (max 2)
      const byWrap = document.createElement('div');
      byWrap.className = 'stb-field';
      const byLabel = document.createElement('label');
      byLabel.className = 'blockr-label';
      byLabel.textContent = 'By (column split, up to 2)';
      byWrap.appendChild(byLabel);
      this._selects.by = Blockr.Select.multi(byWrap, {
        options: [],
        selected: [],
        placeholder: 'Up to 2 categorical columns\u2026',
        onChange: (selected) => {
          const sel = (selected || []).slice(0, 2);
          if ((selected || []).length > 2) {
            // Enforce max 2 — truncate and redraw.
            this._selects.by.setOptions(this._catCols, sel);
          }
          this._state.by = sel;
          this._autoSubmit();
        }
      });
      this._selects.by.el.classList.add('blockr-select--bordered');
      grid.appendChild(byWrap);

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

      this._addStatsRow();
      this._addOverallRow();
      this._addOverallLabelRow();
      this._addBooleanRow('indent_details',
        { on: 'Indent details', off: 'Flat rows' },
        'Indent the detail rows within each variable section');
      this._addBooleanRow('nest_hierarchies',
        { on: 'Nested', off: 'Independent' },
        'Nest hierarchy columns (sections) visually');

      this.card.appendChild(this.popover);

      document.addEventListener('click', (e) => {
        if (!this.card.contains(e.target)) {
          this.popover.style.display = 'none';
          this.gearBtn.classList.remove('blockr-gear-active');
        }
      });
    }

    _addPopoverRow(children, description) {
      const row = document.createElement('div');
      row.className = 'blockr-popover-row';
      row.style.display = 'flex';
      row.style.alignItems = 'center';
      row.style.gap = '10px';
      row.style.marginBottom = '8px';

      (Array.isArray(children) ? children : [children]).forEach(ch => row.appendChild(ch));

      if (description) {
        const muted = document.createElement('span');
        muted.textContent = description;
        muted.style.fontSize = '0.75rem';
        muted.style.color = '#9ca3af';
        muted.style.flex = '1';
        row.appendChild(muted);
      }

      this.popover.appendChild(row);
      return row;
    }

    _addBooleanRow(key, labels, description) {
      const btn = document.createElement('button');
      btn.type = 'button';
      btn.className = 'blockr-pill blockr-popover-toggle';
      const update = () => {
        const active = !!this._state[key];
        btn.textContent = active ? labels.on : labels.off;
        btn.classList.toggle('blockr-popover-toggle-active', active);
      };
      update();
      btn.addEventListener('click', (e) => {
        e.stopPropagation();
        this._state[key] = !this._state[key];
        update();
        this._autoSubmit();
      });
      btn._update = update;
      this['_toggle_' + key] = btn;
      this._addPopoverRow(btn, description);
    }

    _addStatsRow() {
      const btn = document.createElement('button');
      btn.type = 'button';
      btn.className = 'blockr-pill blockr-popover-toggle';
      const labels = { compact: 'Compact', expanded: 'Expanded' };
      const update = () => {
        const isExpanded = this._state.stats === 'expanded';
        btn.textContent = isExpanded ? labels.expanded : labels.compact;
        btn.classList.toggle('blockr-popover-toggle-active', isExpanded);
      };
      update();
      btn.addEventListener('click', (e) => {
        e.stopPropagation();
        this._state.stats = this._state.stats === 'expanded' ? 'compact' : 'expanded';
        update();
        this._autoSubmit();
      });
      btn._update = update;
      this._toggle_stats = btn;
      this._addPopoverRow(btn, 'Compact = Mean (SD). Expanded = N/Mean/SD/Median/Q1,Q3/Min,Max.');
    }

    _addOverallRow() {
      const btn = document.createElement('button');
      btn.type = 'button';
      btn.className = 'blockr-pill blockr-popover-toggle';
      const labels = { on: 'With overall', off: 'No overall' };
      const update = () => {
        const active = !!this._state.add_overall;
        btn.textContent = active ? labels.on : labels.off;
        btn.classList.toggle('blockr-popover-toggle-active', active);
        if (this._overallLabelRow) {
          this._overallLabelRow.style.display = active ? 'flex' : 'none';
        }
      };
      update();
      btn.addEventListener('click', (e) => {
        e.stopPropagation();
        this._state.add_overall = !this._state.add_overall;
        update();
        this._autoSubmit();
      });
      btn._update = update;
      this._toggle_add_overall = btn;
      this._addPopoverRow(btn, 'Append an overall column across all groups');
    }

    _addOverallLabelRow() {
      const input = document.createElement('input');
      input.type = 'text';
      input.className = 'blockr-popover-input';
      input.placeholder = 'Overall column label';
      input.value = this._state.overall_label || '';
      input.style.flex = '1';
      input.addEventListener('input', () => {
        this._state.overall_label = input.value;
        this._autoSubmit();
      });
      input.addEventListener('click', (e) => e.stopPropagation());

      const label = document.createElement('span');
      label.className = 'blockr-popover-label';
      label.textContent = 'Label:';
      label.style.marginBottom = '0';
      label.style.flexShrink = '0';

      const row = this._addPopoverRow([label, input]);
      row.style.display = this._state.add_overall ? 'flex' : 'none';
      this._overallLabelRow = row;
      this._overallLabelInput = input;
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
        vars: this._state.vars,
        sections: this._state.sections,
        by: this._state.by,
        stats: this._state.stats,
        add_overall: this._state.add_overall,
        overall_label: this._state.overall_label,
        indent_details: this._state.indent_details,
        nest_hierarchies: this._state.nest_hierarchies
      };
    }

    setState(state) {
      if (!state) return;
      const arrayKeys = ['vars', 'sections', 'by'];
      for (const k of arrayKeys) {
        if (Array.isArray(state[k])) this._state[k] = state[k].slice();
      }
      if (typeof state.stats === 'string') this._state.stats = state.stats;
      if (typeof state.add_overall === 'boolean') this._state.add_overall = state.add_overall;
      if (typeof state.overall_label === 'string') {
        this._state.overall_label = state.overall_label;
        if (this._overallLabelInput) this._overallLabelInput.value = state.overall_label;
      }
      if (typeof state.indent_details === 'boolean') this._state.indent_details = state.indent_details;
      if (typeof state.nest_hierarchies === 'boolean') this._state.nest_hierarchies = state.nest_hierarchies;

      for (const key of ['stats', 'add_overall', 'indent_details', 'nest_hierarchies']) {
        const btn = this['_toggle_' + key];
        if (btn && btn._update) btn._update();
      }

      if (this._selects.vars) {
        this._selects.vars.setOptions(this._varCols, this._state.vars);
      }
      if (this._selects.sections) {
        this._selects.sections.setOptions(this._catCols, this._state.sections);
      }
      if (this._selects.by) {
        this._selects.by.setOptions(this._catCols, this._state.by);
      }
    }

    updateColumns(msg) {
      this._varCols = Array.isArray(msg.var_cols) ? msg.var_cols : [];
      this._catCols = Array.isArray(msg.cat_cols) ? msg.cat_cols : [];
      if (this._selects.vars) {
        this._selects.vars.setOptions(this._varCols, this._state.vars);
        this._state.vars = this._selects.vars.getValue();
      }
      if (this._selects.sections) {
        this._selects.sections.setOptions(this._catCols, this._state.sections);
        this._state.sections = this._selects.sections.getValue();
      }
      if (this._selects.by) {
        this._selects.by.setOptions(this._catCols, this._state.by);
        this._state.by = this._selects.by.getValue();
      }
      this._autoSubmit();
    }
  }

  // --- Shiny input binding ---
  const binding = new Shiny.InputBinding();
  Object.assign(binding, {
    find: (scope) => $(scope).find('.summary-table-block-container'),
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
      el._block = new SummaryTableBlock(el);
      if (el._pendingColumns) {
        el._block.updateColumns(el._pendingColumns);
        delete el._pendingColumns;
      }
      if (el._pendingState) {
        el._block.setState(el._pendingState);
        delete el._pendingState;
      }
    }
  });
  Shiny.inputBindings.register(binding, 'blockr.summary_table');

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

  Shiny.addCustomMessageHandler('summary-table-columns', (msg) => {
    waitForEl(msg.id, (el) => {
      if (el._block) el._block.updateColumns(msg);
      else el._pendingColumns = msg;
    });
  });

  Shiny.addCustomMessageHandler('summary-table-update', (msg) => {
    waitForEl(msg.id, (el) => {
      if (el._block) el._block.setState(msg.state);
      else el._pendingState = msg.state;
    });
  });
})();
