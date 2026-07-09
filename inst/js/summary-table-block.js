// @ts-check
/**
 * SummaryTableBlock — JS-driven input binding for blockr.viz::summary_table_block.
 *
 * Main: three Blockr.Select widgets (vars multi / sections multi / by multi, max 2).
 * Gear: an in-flow settings band (stat checkboxes, overall checkbox + label,
 * nest_hierarchies checkbox, group-by select, count-distinct select).
 *
 * Depends on: blockr-core.js, blockr-select.js (from blockr.dplyr),
 * blockr-blocks.css, settings-band.js/.css (Blockr.checkbox + .blockr-settings).
 */
(() => {
  'use strict';

  /**
   * @typedef {Object} SummaryTableState
   * @property {string[]} vars
   * @property {string[]} sections
   * @property {string[]} by
   * @property {string[]} stats
   * @property {boolean} add_overall
   * @property {string} overall_label
   * @property {boolean} indent_details
   * @property {boolean} nest_hierarchies
   * @property {string} id_var
   */

  /** A toggle <button> carrying its redraw closure as an expando. */
  /** @typedef {HTMLButtonElement & { _update?: () => void }} ToggleButton */

  /**
   * Numeric stat vocabulary — must mirror SUMMARY_STATS_CATALOG in
   * R/summary-table.R. Array order is the canonical row order; the
   * selection is kept in this order regardless of click order.
   */
  const STAT_OPTIONS = [
    { key: 'n',            label: 'N' },
    { key: 'n_pct',        label: 'n (%)' },
    { key: 'mean',         label: 'Mean' },
    { key: 'sd',           label: 'SD' },
    { key: 'mean_sd',      label: 'Mean (SD)' },
    { key: 'median',       label: 'Median' },
    { key: 'median_q1_q3', label: 'Median (Q1, Q3)' },
    { key: 'q1_q3',        label: 'Q1, Q3' },
    { key: 'min_max',      label: 'Min, Max' }
  ];
  const STAT_KEYS = STAT_OPTIONS.map(o => o.key);
  /** Legacy preset values from boards saved before `stats` was a vector.
   *  @type {Record<string, string[]>} */
  const STAT_LEGACY = {
    compact: ['mean_sd'],
    expanded: ['n', 'mean', 'sd', 'median', 'q1_q3', 'min_max']
  };

  /** @param {any} stats @returns {string[] | null} */
  const normalizeStats = (stats) => {
    if (typeof stats === 'string') {
      if (STAT_LEGACY[stats]) return STAT_LEGACY[stats].slice();
      stats = [stats];
    }
    if (!Array.isArray(stats)) return null;
    const keys = STAT_KEYS.filter(k => stats.indexOf(k) >= 0);
    return keys.length ? keys : null;
  };

  /** The binding's container element, with the block instance + pending state. */
  /** @typedef {HTMLElement & { _block?: SummaryTableBlock, _pendingColumns?: any, _pendingState?: any }} STBHost */

  class SummaryTableBlock {
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      /** @type {BlockrSelectOption[]} */
      this._varCols = [];
      /** @type {BlockrSelectOption[]} */
      this._catCols = [];
      /** @type {((submit: boolean) => void) | null} */
      this._callback = null;
      this._submitted = false;
      /** @type {ReturnType<typeof setTimeout> | null} */
      this._debounceTimer = null;

      /** @type {{ vars?: BlockrSelectMultiHandle, sections?: BlockrSelectMultiHandle, by?: BlockrSelectMultiHandle }} */
      this._selects = {};

      /** @type {SummaryTableState} */
      this._state = {
        vars: [],
        sections: [],
        by: [],
        stats: ['mean_sd'],
        add_overall: false,
        overall_label: 'Total',
        indent_details: true,
        nest_hierarchies: false,
        id_var: ''
      };

      // Created here (not in _buildDOM) so their types are definite for the
      // methods/closures below — _buildDOM only configures and appends them.
      this.card = document.createElement('div');
      this.gearBtn = document.createElement('button');
      this.popover = document.createElement('div');
      this._bandGrid = document.createElement('div');

      /** @type {((value: string) => void) | null} */
      this._overallLabelSync = null;

      this._buildDOM();
    }

    _autoSubmit() {
      clearTimeout(this._debounceTimer ?? undefined);
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _buildDOM() {
      const BSelect = /** @type {BlockrSelectStatic} */ (Blockr.Select);
      this.card.className = 'stb-card';
      this.card.style.position = 'relative';
      this.el.appendChild(this.card);

      // Gear button (top-right)
      const gearHeader = document.createElement('div');
      gearHeader.className = 'blockr-gear-header';
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

      // Summarize (full-width row)
      const varsWrap = document.createElement('div');
      varsWrap.className = 'stb-field stb-field--full';
      const varsLabel = document.createElement('label');
      varsLabel.className = 'blockr-label';
      varsLabel.textContent = 'Summarize';
      varsWrap.appendChild(varsLabel);
      this._selects.vars = BSelect.multi(varsWrap, {
        options: [],
        selected: [],
        placeholder: 'Columns to summarize\u2026',
        onChange: (selected) => {
          this._state.vars = selected || [];
          this._setVarsAttention();
          this._autoSubmit();
        }
      });
      this._selects.vars.el.classList.add('blockr-select--bordered');
      grid.appendChild(varsWrap);
      // Summarize is the one required field: highlight it amber while empty
      // (design-system "needs a value" affordance, mirroring the chart block's
      // .dd-role-required-empty) instead of letting the block read as broken.
      this._varsWrap = varsWrap;
      this._setVarsAttention();

      // Split by (max 2)
      const byWrap = document.createElement('div');
      byWrap.className = 'stb-field stb-field--full';
      const byLabel = document.createElement('label');
      byLabel.className = 'blockr-label';
      byLabel.textContent = 'Split by';
      byWrap.appendChild(byLabel);
      this._selects.by = BSelect.multi(byWrap, {
        options: [],
        selected: [],
        placeholder: 'Column dimension (up to 2)\u2026',
        onChange: (selected) => {
          const sel = (selected || []).slice(0, 2);
          if ((selected || []).length > 2) {
            // Enforce max 2 — truncate and redraw.
            this._selects.by?.setOptions(this._catCols, sel);
          }
          this._state.by = sel;
          this._autoSubmit();
        }
      });
      this._selects.by.el.classList.add('blockr-select--bordered');
      grid.appendChild(byWrap);

      // Settings band (design-system pilot \u2014 blockr.ui/dev/
      // gear-panel-proposals.html variant B): a full-width, in-flow panel
      // between the gear header and the main grid, built from the standard
      // controls; on/off options are .blockr-checkbox (see
      // boolean-controls-proposals.html), not self-labeling pills.
      // --beak: gear connector T1 (settings-band.css) — the open band grows
      // a notch pointing at the gear that opened it.
      this.popover.className = 'blockr-settings blockr-settings--beak';

      const popTitle = document.createElement('div');
      popTitle.className = 'blockr-settings__title';
      popTitle.textContent = 'Settings';
      this.popover.appendChild(popTitle);

      this._bandGrid.className = 'blockr-settings__grid';
      this.popover.appendChild(this._bandGrid);

      this._addStatsRow();
      this._addOverallRow();
      this._addOverallLabelRow();
      this._addBooleanRow('nest_hierarchies',
        { on: 'Nest hierarchy', off: 'Flat' },
        'Auto-nest adjacent categorical columns (e.g. SOC \u2192 PT)');
      this._addSectionsRow();
      this._addIdVarRow();

      // In flow: opening pushes the main grid down; the gear is the only
      // toggle (a panel, not a menu \u2014 no outside-click dismissal).
      this.card.insertBefore(this.popover, grid);
    }

    /** @param {HTMLElement | HTMLElement[]} children @param {string} [description] */
    _addPopoverRow(children, description) {
      const row = document.createElement('div');
      row.className = 'blockr-settings__field blockr-settings__field--full';
      row.style.display = 'flex';
      row.style.flexDirection = 'row';
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

      this._bandGrid.appendChild(row);
      return row;
    }

    /**
     * @param {'indent_details' | 'nest_hierarchies'} key
     * @param {{ on: string, off: string }} labels
     * @param {string} [description]
     */
    _addBooleanRow(key, labels, description) {
      // Boolean data option -> checkbox (design-system rule); the label
      // carries the affirmative meaning, the box carries the state.
      const box = Blockr.checkbox(labels.on, !!this._state[key], (checked) => {
        this._state[key] = checked;
        this._autoSubmit();
      });
      const update = () => { box.set(!!this._state[key]); };
      (/** @type {Record<string, any>} */ (this))['_toggle_' + key] = { _update: update };
      this._addPopoverRow(box.el, description);
    }

    _addStatsRow() {
      const wrap = document.createElement('div');
      wrap.className = 'blockr-settings__field blockr-settings__field--full';
      wrap.style.marginBottom = '8px';

      const label = document.createElement('span');
      label.className = 'blockr-label';
      label.textContent = 'Statistics';
      label.style.display = 'block';
      label.style.marginBottom = '4px';
      wrap.appendChild(label);

      const hint = document.createElement('span');
      hint.textContent = 'Rows per numeric variable — one selected = a single row, several = one row each.';
      hint.style.fontSize = '0.75rem';
      hint.style.color = '#9ca3af';
      hint.style.display = 'block';
      hint.style.marginBottom = '6px';
      wrap.appendChild(hint);

      // One checkbox per stat (independent on/off options — design-system
      // rule: data options are checkboxes, not self-labeling pills).
      const boxRow = document.createElement('div');
      boxRow.className = 'blockr-checkbox-row';
      wrap.appendChild(boxRow);

      const boxes = STAT_OPTIONS.map((opt) => {
        const box = Blockr.checkbox(
          opt.label,
          this._state.stats.indexOf(opt.key) >= 0,
          (/** @type {boolean} */ checked) => {
            if (!checked) {
              // Never deselect the last stat — the engine needs at least one.
              if (this._state.stats.length === 1) { box.set(true); return; }
              const idx = this._state.stats.indexOf(opt.key);
              if (idx >= 0) this._state.stats.splice(idx, 1);
            } else {
              // Keep canonical catalog order regardless of click order.
              this._state.stats = STAT_KEYS.filter(
                k => k === opt.key || this._state.stats.indexOf(k) >= 0
              );
            }
            this._autoSubmit();
          });
        return box;
      });
      boxes.forEach(b => boxRow.appendChild(b.el));

      const update = () => {
        boxes.forEach((box, i) => {
          box.set(this._state.stats.indexOf(STAT_OPTIONS[i].key) >= 0);
        });
      };

      // Same `{ _update }` shape as the toggle handles so setState's
      // refresh loop can repaint it uniformly.
      this._toggle_stats = /** @type {any} */ ({ _update: update });
      this._bandGrid.appendChild(wrap);
    }

    _addOverallRow() {
      const update = () => {
        box.set(!!this._state.add_overall);
        if (this._overallLabelRow) {
          this._overallLabelRow.style.display = this._state.add_overall ? 'flex' : 'none';
        }
      };
      const box = Blockr.checkbox('Overall column', !!this._state.add_overall,
        (/** @type {boolean} */ checked) => {
          this._state.add_overall = checked;
          update();
          this._autoSubmit();
        });
      this._toggle_add_overall = /** @type {any} */ ({ _update: update });
      this._addPopoverRow(box.el, 'Append an overall column across all groups');
    }

    _addOverallLabelRow() {
      const input = document.createElement('input');
      input.type = 'text';
      // Standard 42px text input — the band uses the same controls as the
      // main UI (no popover-specific input recipe).
      input.className = 'blockr-text-input';
      input.placeholder = 'Overall column label';
      input.value = this._state.overall_label || '';
      input.style.flex = '1';
      // Commit model (design-system §5.5): typing never submits — the value
      // commits on Enter/blur/chip, the chip fades to ✓, Escape reverts.
      const chip = document.createElement('button');
      chip.type = 'button';
      chip.className = 'blockr-expr-confirm';
      chip.title = 'Apply (Enter)';
      chip.setAttribute('aria-label', 'Apply (Enter)');
      chip.style.display = 'none';
      let committed = input.value;
      let everCommitted = false;
      const syncChip = () => {
        if (input.value !== committed) {
          chip.style.display = '';
          chip.classList.remove('confirmed');
          chip.innerHTML = 'Enter <span class="blockr-kbd">↵</span>';
        } else if (everCommitted) {
          chip.style.display = '';
          chip.classList.add('confirmed');
          chip.innerHTML = (Blockr.icons && Blockr.icons.confirm) || '✓';
        } else {
          chip.style.display = 'none';
        }
      };
      const commit = () => {
        if (input.value === committed) return;
        committed = input.value;
        everCommitted = true;
        this._state.overall_label = input.value;
        this._submit();
        syncChip();
      };
      input.addEventListener('input', syncChip);
      input.addEventListener('keydown', (e) => {
        if (e.key === 'Enter') { e.preventDefault(); commit(); }
        else if (e.key === 'Escape') { input.value = committed; syncChip(); }
      });
      input.addEventListener('blur', commit);
      chip.addEventListener('mousedown', (e) => e.preventDefault());
      chip.addEventListener('click', commit);
      // Programmatic restores (setState) reset the committed baseline so a
      // restored value never shows an armed chip.
      this._overallLabelSync = (/** @type {string} */ value) => {
        input.value = value;
        committed = value;
        everCommitted = false;
        syncChip();
      };

      const label = document.createElement('span');
      label.className = 'blockr-label';
      label.textContent = 'Label:';
      label.style.marginBottom = '0';
      label.style.flexShrink = '0';

      const row = this._addPopoverRow([label, input, chip]);
      row.style.display = this._state.add_overall ? 'flex' : 'none';
      this._overallLabelRow = row;
      this._overallLabelInput = input;
    }

    _addSectionsRow() {
      const BSelect = /** @type {BlockrSelectStatic} */ (Blockr.Select);
      const wrap = document.createElement('div');
      wrap.className = 'blockr-settings__field blockr-settings__field--full';
      wrap.style.marginBottom = '8px';

      const label = document.createElement('span');
      label.className = 'blockr-label';
      label.textContent = 'Group by';
      label.style.display = 'block';
      label.style.marginBottom = '4px';
      wrap.appendChild(label);

      const hint = document.createElement('span');
      hint.textContent = 'Outer row hierarchy above the summarized variables';
      hint.style.fontSize = '0.75rem';
      hint.style.color = '#9ca3af';
      hint.style.display = 'block';
      hint.style.marginBottom = '6px';
      wrap.appendChild(hint);

      this._selects.sections = BSelect.multi(wrap, {
        options: [],
        selected: [],
        placeholder: 'Optional grouping columns\u2026',
        onChange: (selected) => {
          this._state.sections = selected || [];
          this._autoSubmit();
        }
      });
      this._selects.sections.el.classList.add('blockr-select--bordered');
      this._bandGrid.appendChild(wrap);
    }

    _addIdVarRow() {
      const select = document.createElement('select');
      // Standard 42px control (same inputs as the main UI).
      select.className = 'blockr-text-input';
      select.style.flex = '1';
      select.style.width = 'auto';

      const empty = document.createElement('option');
      empty.value = '';
      empty.textContent = '— row count —';
      select.appendChild(empty);

      select.addEventListener('change', () => {
        this._state.id_var = select.value;
        this._autoSubmit();
      });

      const label = document.createElement('span');
      label.className = 'blockr-label';
      label.textContent = 'Count distinct by:';
      label.style.marginBottom = '0';
      label.style.flexShrink = '0';

      this._addPopoverRow([label, select],
        'Compute N values and percentages over distinct values of this column.');
      this._idVarSelect = select;
    }

    _refreshIdVarOptions() {
      const sel = this._idVarSelect;
      if (!sel) return;
      while (sel.options.length > 1) sel.remove(1);
      for (const col of this._varCols) {
        // col is either a bare name string or a {value, label} object.
        const val = (col && typeof col === 'object') ? col.value : col;
        const lbl = (col && typeof col === 'object' && col.label) ? col.label : '';
        const opt = document.createElement('option');
        opt.value = val;
        opt.textContent = lbl ? `${val} (${lbl})` : val;
        sel.appendChild(opt);
      }
      const want = this._state.id_var || '';
      sel.value = want;
      // If the desired column is no longer present, fall back to row count.
      if (sel.value !== want) this._state.id_var = '';
    }

    /** Amber-highlight the Summarize field whenever no variable is chosen. */
    _setVarsAttention() {
      if (this._varsWrap) {
        this._varsWrap.classList.toggle(
          'stb-field--required-empty', this._state.vars.length === 0);
      }
    }

    _togglePopover() {
      const showing = !this.popover.classList.contains('blockr-settings--open');
      this.popover.classList.toggle('blockr-settings--open', showing);
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
        nest_hierarchies: this._state.nest_hierarchies,
        id_var: this._state.id_var || ''
      };
    }

    /** @param {any} state */
    setState(state) {
      if (!state) return;
      const arrayKeys = /** @type {Array<'vars' | 'sections' | 'by'>} */ (['vars', 'sections', 'by']);
      for (const k of arrayKeys) {
        if (Array.isArray(state[k])) this._state[k] = state[k].slice();
      }
      const stats = normalizeStats(state.stats);
      if (stats) this._state.stats = stats;
      if (typeof state.add_overall === 'boolean') this._state.add_overall = state.add_overall;
      if (typeof state.overall_label === 'string') {
        this._state.overall_label = state.overall_label;
        if (this._overallLabelSync) this._overallLabelSync(state.overall_label);
        else if (this._overallLabelInput) this._overallLabelInput.value = state.overall_label;
      }
      if (typeof state.indent_details === 'boolean') this._state.indent_details = state.indent_details;
      if (typeof state.nest_hierarchies === 'boolean') this._state.nest_hierarchies = state.nest_hierarchies;
      if (typeof state.id_var === 'string') {
        this._state.id_var = state.id_var;
        this._refreshIdVarOptions();
      }

      for (const key of ['stats', 'add_overall', 'nest_hierarchies']) {
        const btn = (/** @type {Record<string, any>} */ (this))['_toggle_' + key];
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
      this._setVarsAttention();
    }

    /** @param {any} msg */
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
      this._refreshIdVarOptions();
      this._setVarsAttention();
      this._autoSubmit();
    }
  }

  // --- Shiny input binding ---
  const binding = new Shiny.InputBinding();
  Object.assign(binding, {
    find: (/** @type {Document | HTMLElement} */ scope) => $(scope).find('.summary-table-block-container'),
    getId: (/** @type {STBHost} */ el) => el.id || null,
    getValue: (/** @type {STBHost} */ el) => el._block?.getValue() ?? null,
    setValue: (/** @type {STBHost} */ el, /** @type {any} */ value) => el._block?.setState(value),
    subscribe: (/** @type {STBHost} */ el, /** @type {(v: boolean) => void} */ callback) => {
      if (el._block) el._block._callback = () => callback(true);
    },
    unsubscribe: (/** @type {STBHost} */ el) => {
      if (el._block) el._block._callback = null;
    },
    initialize: (/** @type {STBHost} */ el) => {
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

  const waitForEl = (/** @type {string} */ id, /** @type {(el: STBHost) => void} */ cb) => {
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
