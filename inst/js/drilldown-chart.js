/**
 * Drill-Down Chart — configurable chart that acts as a filter.
 *
 * Three families:
 *   Aggregated (bar, pie, treemap, boxplot): group-by + metric, click to filter.
 *   Individual (scatter, line): x/y columns, brush-drag to filter. Clicking a
 *     line emits a categorical filter on its series (typically USUBJID).
 *   Timeline (gantt): interval rows with start + optional end, categorical y
 *     axis. Clicking a bar emits a USUBJID filter.
 */
(() => {
  'use strict';

  const AGGREGATED_TYPES = ['bar', 'pie', 'treemap', 'boxplot'];
  const INDIVIDUAL_TYPES = ['scatter', 'line'];
  const TIMELINE_TYPES = ['gantt'];

  // Individual-family render baselines. Multipliers (`line_width_mult`,
  // `dot_size_mult`) scale these so slider 1.0× matches echarts' default
  // look. Values mirror the literals previously hardcoded in mkSeries.
  const BASE_LINE_WIDTH   = 1.4;
  const BASE_SCATTER_SIZE = 6;
  const BASE_LINE_MARKER  = 4;

  // Trajectory overlay degradation thresholds (spec §2)
  const TRAJ_FULL_MAX = 50;
  const TRAJ_REDUCED_MAX = 500;
  const TRAJ_HARD_CAP = 1000;

  // blockr echarts design system. Okabe-Ito categorical palette —
  // colorblind-safe and well-tested for general categorical encoding.
  const BLOCKR_PALETTE = ['#0072B2', '#D55E00', '#F0E442', '#009E73', '#56B4E9', '#E69F00', '#CC79A7'];
  const BLOCKR_FONT = "'Open Sans', system-ui, sans-serif";
  const AXIS_LABEL_COLOR = '#666';
  const AXIS_LINE_COLOR = '#ccc';
  const SPLIT_LINE_COLOR = '#f3f4f6';

  // Always-on, small, muted toolbox shared by every chart family.
  //
  // `feature.brush` MUST be registered for the line/scatter family —
  // `takeGlobalCursor({key:'brush'})` (which activates brush mode by default)
  // requires the toolbox brush feature to be present, otherwise the cursor
  // mode silently fails to engage and drag-to-filter doesn't work. The
  // brush icons only render on charts that actually have a `brush`
  // component, so this is inert for bar/pie/treemap/boxplot/gantt.
  const TOOLBOX = {
    show: true,
    right: 8,
    top: 4,
    itemSize: 11,
    feature: {
      saveAsImage: { title: 'Save', pixelRatio: 2 },
      brush: { type: ['rect', 'lineX', 'clear'] }
    },
    iconStyle: { borderColor: '#bbb' }
  };

  const AGG_FNS = [
    { value: 'count', label: 'Count' },
    { value: 'count_distinct', label: 'Count distinct' },
    { value: 'mean', label: 'Mean' },
    { value: 'median', label: 'Median' },
    { value: 'sum', label: 'Sum' },
    { value: 'min', label: 'Min' },
    { value: 'max', label: 'Max' }
  ];

  class DrilldownChart {
    constructor(el) {
      this.el = el;
      this.data = [];
      this.columns = [];
      this.config = {};
      this.charts = [];
      this._selected = null;
      this._selects = {};
      this.theme = null;  // null -> echarts default theme
      this._buildDOM();
    }

    setTheme(theme) {
      const normalized = (theme && theme !== 'default') ? theme : null;
      if (this.theme === normalized) return;
      this.theme = normalized;
      if (this.data && this.data.length) this._render();
    }

    _family() {
      if (AGGREGATED_TYPES.includes(this.config.chart_type)) return 'aggregated';
      if (TIMELINE_TYPES.includes(this.config.chart_type)) return 'timeline';
      return 'individual';
    }

    // Pick an echarts axis type from column metadata. Returns 'category',
    // 'value', or 'time'. Date columns are detected by name ending in "DT"
    // plus numeric ms values (the convention used by the AE gantt R code).
    _axisTypeFor(colName) {
      if (!colName) return 'value';
      const meta = (this.columns || []).find(c => c.name === colName);
      if (meta && meta.type === 'categorical') return 'category';
      if (/DT$/i.test(colName)) {
        const sample = (this.data || []).find(r => r[colName] != null);
        if (sample && typeof sample[colName] === 'number' && sample[colName] > 1e11) {
          return 'time';
        }
      }
      return 'value';
    }

    // Categories (sorted by companion numeric column if present, e.g.
    // AVISITN for AVISIT). Used by any chart family that puts a categorical
    // column on an axis.
    _orderedCategories(colName, rows) {
      const src = rows || this.data || [];
      const companions = { AVISIT: 'AVISITN' };
      const companion = companions[colName];
      if (companion && src.length && src[0][companion] !== undefined) {
        const mins = {};
        for (const r of src) {
          const k = String(r[colName] ?? '');
          const v = Number(r[companion]);
          if (!isNaN(v) && (mins[k] == null || v < mins[k])) mins[k] = v;
        }
        return Object.keys(mins).sort((a, b) => mins[a] - mins[b]);
      }
      // First-occurrence order
      const seen = new Set();
      const out = [];
      for (const r of src) {
        const k = String(r[colName] ?? '');
        if (!seen.has(k)) { seen.add(k); out.push(k); }
      }
      return out;
    }

    _buildDOM() {
      this.el.innerHTML = '';

      // Card wrapper (for popover positioning)
      this.card = document.createElement('div');
      this.card.className = 'dd-card';
      this.el.appendChild(this.card);

      // Gear header (top-right, same as blockr.dplyr)
      const gearHeader = document.createElement('div');
      gearHeader.className = 'blockr-gear-header';
      this.gearBtn = document.createElement('button');
      this.gearBtn.type = 'button';
      this.gearBtn.className = 'blockr-gear-btn';
      this.gearBtn.innerHTML = (typeof Blockr !== 'undefined' && Blockr.icons)
        ? Blockr.icons.gear : '\u2699';
      this.gearBtn.title = 'Advanced settings';
      this.gearBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        this._togglePopover();
      });
      gearHeader.appendChild(this.gearBtn);
      this.card.appendChild(gearHeader);

      // Main config bar (always visible)
      this.configBar = document.createElement('div');
      this.configBar.className = 'dd-config-bar';
      this.card.appendChild(this.configBar);

      // Popover (right-anchored, same as blockr.dplyr)
      this.popoverEl = document.createElement('div');
      this.popoverEl.className = 'blockr-popover';
      this.popoverEl.style.display = 'none';
      this.card.appendChild(this.popoverEl);

      // Close popover on outside click
      document.addEventListener('click', (e) => {
        if (this._popoverOpen && this.popoverEl &&
            !this.popoverEl.contains(e.target) &&
            !this.gearBtn.contains(e.target)) {
          this._closePopover();
        }
      });

      // Chart area
      this.chartGrid = document.createElement('div');
      this.chartGrid.className = 'dd-chart-grid';
      this.card.appendChild(this.chartGrid);

      // Status footer (always present, below chart)
      this.statusEl = document.createElement('div');
      this.statusEl.className = 'dd-status-footer';
      this.card.appendChild(this.statusEl);
      this._updateStatus();
    }

    // -- Config UI ------------------------------------------------------------

    _addSelect(container, label, key, options, selected, cssClass) {
      const wrapper = document.createElement('div');
      // Single-bordered "key/value field" — same shape as .blockr-row in
      // rename/filter blocks. The wrapper carries the border; the inner
      // Blockr.Select stays unbordered (don't add .blockr-select--bordered
      // here, that would double-border).
      wrapper.className = 'dd-picker-wrap' +
        (cssClass ? ' ' + cssClass : '');

      const lbl = document.createElement('label');
      lbl.className = 'blockr-label';
      lbl.textContent = label;
      wrapper.appendChild(lbl);

      const useBlockrSelect = typeof Blockr !== 'undefined' && Blockr.Select;

      const onSelect = (val) => {
        this.config[key] = (val === '(none)') ? '' : val;
        this._selected = null;
        this._render();
        this._sendConfig();
        this._sendClearFilter();
      };

      if (useBlockrSelect) {
        const sel = Blockr.Select.single(wrapper, {
          options: options,
          selected: selected || (typeof options[0] === 'object' && options[0] !== null ? options[0].value : options[0]) || '',
          onChange: onSelect
        });
        this._selects[key] = sel;
      } else {
        const sel = document.createElement('select');
        sel.className = 'dd-cfg-select';
        for (const o of options) {
          const val = typeof o === 'object' && o !== null ? o.value : o;
          const txt = typeof o === 'object' && o !== null && o.label ? `${o.value} (${o.label})` : val;
          const opt = document.createElement('option');
          opt.value = val;
          opt.textContent = txt;
          if (val === selected) opt.selected = true;
          sel.appendChild(opt);
        }
        sel.addEventListener('change', () => onSelect(sel.value));
        wrapper.appendChild(sel);
      }

      container.appendChild(wrapper);
    }

    _renderConfig() {
      const cats = this.columns.filter(c => c.type === 'categorical' || c.n_unique <= 50);
      const nums = this.columns.filter(c => c.type === 'numeric');
      const allCols = this.columns;
      const cfg = this.config;

      // Destroy old selects
      for (const s of Object.values(this._selects)) {
        if (s && typeof s.destroy === 'function') s.destroy();
      }
      this._selects = {};

      // Helper: column metadata to option (with label if available)
      const colOpt = (c) => c.label ? { value: c.name, label: c.label } : c.name;

      // ===== MAIN BAR =====
      this.configBar.innerHTML = '';

      // Aggregated: Group by
      this._addSelect(this.configBar, 'Group', 'group_by',
        ['(none)', ...allCols.map(colOpt)], cfg.group_by || '(none)', 'dd-cfg-aggregated');

      // Individual + Timeline: X
      // For timeline, y_col may be categorical (AE term); the filter below
      // allows that.
      this._addSelect(this.configBar, 'X', 'x_col',
        allCols.map(colOpt), cfg.x_col, 'dd-cfg-xy');
      const yOpts = this._family() === 'timeline' ? allCols : nums;
      this._addSelect(this.configBar, 'Y', 'y_col',
        yOpts.map(colOpt), cfg.y_col, 'dd-cfg-xy');

      // Individual + Timeline: Series. On individual it splits rows into
      // separate lines/scatter series. On timeline it labels individual
      // bars (tooltip + on-bar text), letting color_by stay low-cardinality
      // for a clean legend. High cardinality is fine in both families.
      this._addSelect(this.configBar, 'Series', 'series_by',
        ['(none)', ...allCols.map(colOpt)], cfg.series_by || '(none)',
        'dd-cfg-individual');
      this._addSelect(this.configBar, 'Series', 'series_by',
        ['(none)', ...allCols.map(colOpt)], cfg.series_by || '(none)',
        'dd-cfg-timeline');

      // Timeline only: X end (interval end column)
      this._addSelect(this.configBar, 'X end', 'x_end_col',
        ['(none)', ...allCols.map(colOpt)], cfg.x_end_col || '(none)',
        'dd-cfg-timeline');

      // Shared: Color, Facet
      this._addSelect(this.configBar, 'Color', 'color_by',
        ['(none)', ...allCols.map(colOpt)], cfg.color_by || '(none)');
      this._addSelect(this.configBar, 'Facet', 'facet_by',
        ['(none)', ...cats.filter(x => x.n_unique <= 10).map(colOpt)], cfg.facet_by || '(none)');

      // Individual scatter only: smoother overlay (none / lm / loess)
      this._addSelect(this.configBar, 'Smoother', 'smoother',
        ['none', 'lm', 'loess'], cfg.smoother || 'none', 'dd-cfg-xy');

      // Individual line: optional error-band columns. Presence of both is
      // the on/off — same pattern as X end for timelines. Numeric only.
      this._addSelect(this.configBar, 'Lo', 'lo_col',
        ['(none)', ...nums.map(colOpt)], cfg.lo_col || '(none)',
        'dd-cfg-xy');
      this._addSelect(this.configBar, 'Hi', 'hi_col',
        ['(none)', ...nums.map(colOpt)], cfg.hi_col || '(none)',
        'dd-cfg-xy');

      // ===== POPOVER CONTENT =====
      this.popoverEl.innerHTML = '';

      // Chart type selector (two groups)
      const typesRow = document.createElement('div');
      typesRow.className = 'blockr-popover-row dd-popover-types';

      const buildTypeGroup = (label, types) => {
        const group = document.createElement('div');
        group.className = 'dd-type-group';

        const glabel = document.createElement('span');
        glabel.className = 'dd-type-group-label';
        glabel.textContent = label;
        group.appendChild(glabel);

        const btns = document.createElement('div');
        btns.className = 'dd-cfg-types';
        for (const t of types) {
          const btn = document.createElement('button');
          btn.className = 'dd-type-btn' + (t === cfg.chart_type ? ' dd-type-active' : '');
          btn.textContent = t;
          btn.addEventListener('click', () => {
            const oldFamily = this._family();
            this.el.querySelectorAll('.dd-type-btn').forEach(b => b.classList.remove('dd-type-active'));
            btn.classList.add('dd-type-active');
            this.config.chart_type = t;
            const newFamily = this._family();
            if (oldFamily !== newFamily) {
              this._selected = null;
              this._sendClearFilter();
              const wasOpen = this._popoverOpen;
              this._renderConfig();
              if (wasOpen) setTimeout(() => this._openPopover(), 0);
            }
            this._updateFamilyClass();
            this._render();
            this._sendConfig();
          });
          btns.appendChild(btn);
        }
        group.appendChild(btns);
        return group;
      };

      typesRow.appendChild(buildTypeGroup('Aggregated', AGGREGATED_TYPES));
      typesRow.appendChild(buildTypeGroup('Individual', INDIVIDUAL_TYPES));
      typesRow.appendChild(buildTypeGroup('Timeline', TIMELINE_TYPES));
      this.popoverEl.appendChild(typesRow);

      // Metric + Agg (aggregated only)
      const metricRow = document.createElement('div');
      metricRow.className = 'blockr-popover-row dd-cfg-aggregated';
      this._addSelect(metricRow, 'Metric', 'metric',
        ['.count', ...nums.map(colOpt)], cfg.metric || '.count');
      this._addSelect(metricRow, 'Agg', 'agg_fn',
        AGG_FNS.map(a => a.value), cfg.agg_fn || 'count');
      this.popoverEl.appendChild(metricRow);

      // Sort + Dir (aggregated: value / alpha / column-min ordering)
      const sortAggRow = document.createElement('div');
      sortAggRow.className = 'blockr-popover-row dd-cfg-aggregated';
      this._addSelect(sortAggRow, 'Sort', 'sort_by',
        ['value', 'alpha', ...nums.map(colOpt)], cfg.sort_by || 'value');
      this._addSelect(sortAggRow, 'Dir', 'sort_dir',
        ['asc', 'desc'], cfg.sort_dir || 'desc');
      this.popoverEl.appendChild(sortAggRow);

      // Sort + Dir (timeline: onset / alpha / column-min ordering)
      const sortTimeRow = document.createElement('div');
      sortTimeRow.className = 'blockr-popover-row dd-cfg-timeline';
      this._addSelect(sortTimeRow, 'Sort', 'sort_by',
        ['onset', 'alpha', ...allCols.map(colOpt)], cfg.sort_by || 'onset');
      this._addSelect(sortTimeRow, 'Dir', 'sort_dir',
        ['asc', 'desc'], cfg.sort_dir || 'asc');
      this.popoverEl.appendChild(sortTimeRow);

      // Line width + dot size multipliers (individual only)
      const themeRow = document.createElement('div');
      themeRow.className = 'blockr-popover-row dd-cfg-individual';
      this._addMultSlider(themeRow, 'Line width', 'line_width_mult',
        cfg.line_width_mult);
      this._addMultSlider(themeRow, 'Dot size', 'dot_size_mult',
        cfg.dot_size_mult);
      this.popoverEl.appendChild(themeRow);

      this._updateFamilyClass();
    }

    _addMultSlider(container, label, key, initial) {
      const v0 = (typeof initial === 'number' && isFinite(initial)) ? initial : 1.0;

      const wrap = document.createElement('div');
      wrap.className = 'dd-slider-wrap';

      const lbl = document.createElement('label');
      lbl.className = 'blockr-label';
      lbl.textContent = label;
      wrap.appendChild(lbl);

      const input = document.createElement('input');
      input.type = 'range';
      input.min = '0.5';
      input.max = '3.0';
      input.step = '0.1';
      input.value = String(v0);
      input.className = 'dd-slider';
      wrap.appendChild(input);

      const value = document.createElement('span');
      value.className = 'dd-slider-value';
      value.textContent = v0.toFixed(1) + '\u00D7';
      wrap.appendChild(value);

      let debounce;
      input.addEventListener('input', () => {
        const v = parseFloat(input.value);
        value.textContent = v.toFixed(1) + '\u00D7';
        this.config[key] = v;
        this._render();
        clearTimeout(debounce);
        debounce = setTimeout(() => this._sendMults(), 150);
      });

      container.appendChild(wrap);
    }

    _updateFamilyClass() {
      const family = this._family();
      this.el.classList.remove(
        'dd-family-aggregated', 'dd-family-individual', 'dd-family-timeline'
      );
      this.el.classList.add('dd-family-' + family);
    }

    // -- Data + rendering entry point -----------------------------------------

    setData(columns, data, config) {
      this.columns = columns || [];
      this.config = config || {};

      // Convert column-oriented data to row-oriented array
      // Data may arrive as: JSON string (pre-encoded), column object, or row array
      if (typeof data === 'string') {
        data = JSON.parse(data);
      }
      if (data && !Array.isArray(data)) {
        const keys = Object.keys(data);
        const n = keys.length > 0 ? (Array.isArray(data[keys[0]]) ? data[keys[0]].length : 1) : 0;
        this.data = new Array(n);
        for (let i = 0; i < n; i++) {
          const row = {};
          for (const k of keys) row[k] = Array.isArray(data[k]) ? data[k][i] : data[k];
          this.data[i] = row;
        }
      } else {
        this.data = data || [];
      }

      if (!this.config.chart_type) this.config.chart_type = 'bar';

      const fam = this._family();
      if (fam === 'aggregated') {
        if (!this.config.group_by && this.columns.length > 0) {
          const cat = this.columns.find(c => c.type === 'categorical' && c.n_unique <= 30);
          this.config.group_by = cat ? cat.name : this.columns[0].name;
        }
        if (!this.config.metric) this.config.metric = '.count';
        if (!this.config.agg_fn) this.config.agg_fn = 'count';
        if (!this.config.sort_by) this.config.sort_by = 'value';
        if (!this.config.sort_dir) this.config.sort_dir = 'desc';
      } else if (fam === 'timeline') {
        if (!this.config.x_col && this.columns.length > 0) {
          const num = this.columns.find(c => c.type === 'numeric');
          this.config.x_col = num ? num.name : this.columns[0].name;
        }
        if (!this.config.y_col) {
          const cat = this.columns.find(c => c.type === 'categorical' && c.n_unique > 1);
          this.config.y_col = cat ? cat.name : this.columns[0]?.name;
        }
        if (!this.config.sort_by) this.config.sort_by = 'onset';
        if (!this.config.sort_dir) this.config.sort_dir = 'asc';
      } else {
        if (!this.config.x_col && this.columns.length > 0) {
          const num = this.columns.find(c => c.type === 'numeric');
          this.config.x_col = num ? num.name : this.columns[0].name;
        }
        if (!this.config.y_col) {
          const nums = this.columns.filter(c => c.type === 'numeric');
          const other = nums.find(c => c.name !== this.config.x_col);
          this.config.y_col = other ? other.name : (nums[0] ? nums[0].name : this.columns[0]?.name);
        }
      }

      // Restore filter state if provided (e.g., from saved board)
      if (config?.filter_values && config?.filter_column) {
        this._selected = config.filter_values.length === 1
          ? config.filter_values[0] : config.filter_values;
      }

      this._renderConfig();
      this._render();
    }

    // -- Aggregation ----------------------------------------------------------

    _aggregate() {
      const { group_by, color_by, facet_by, metric, agg_fn } = this.config;
      if (this.data.length === 0) return [];

      const groups = {};
      for (const row of this.data) {
        const gv = group_by ? String(row[group_by] ?? '') : 'Total';
        const cv = color_by ? String(row[color_by] ?? '') : '__all__';
        const fv = facet_by ? String(row[facet_by] ?? '') : '__all__';
        const key = fv + '|||' + gv + '|||' + cv;
        if (!groups[key]) groups[key] = { facet: fv, group: gv, color: cv, values: [], rows: [] };
        groups[key].rows.push(row);
        if (metric !== '.count' && row[metric] != null) groups[key].values.push(Number(row[metric]));
      }

      const result = [];
      for (const g of Object.values(groups)) {
        let value;
        if (agg_fn === 'count') value = g.rows.length;
        else if (agg_fn === 'count_distinct') { const s = new Set(); for (const r of g.rows) s.add(r[metric]); value = s.size; }
        else if (agg_fn === 'mean') value = g.values.length ? g.values.reduce((a, b) => a + b, 0) / g.values.length : 0;
        else if (agg_fn === 'median') { const s = g.values.slice().sort((a, b) => a - b); const m = Math.floor(s.length / 2); value = s.length ? (s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2) : 0; }
        else if (agg_fn === 'sum') value = g.values.reduce((a, b) => a + b, 0);
        else if (agg_fn === 'min') value = g.values.length ? Math.min.apply(null, g.values) : 0;
        else if (agg_fn === 'max') value = g.values.length ? Math.max.apply(null, g.values) : 0;
        result.push({ facet: g.facet, group: g.group, color: g.color, value: Math.round(value * 100) / 100 });
      }
      return result;
    }

    // -- Chart rendering ------------------------------------------------------

    _render() {
      for (const c of this.charts) c.dispose();
      this.charts = [];
      this.chartGrid.innerHTML = '';

      if (this.data.length === 0) {
        this.chartGrid.innerHTML = '<div class="vd-empty-state"><p class="vd-empty-text">No data to chart</p></div>';
        return;
      }

      const fam = this._family();

      // Guard: color_by with too many distinct levels renders an unreadable
      // legend. color_by means "map column values to colors" — a small
      // palette has ~7 readable colors, 15 is a hard ceiling. For splitting
      // data into many series (one per patient), use `series_by` instead.
      const MAX_COLOR_LEVELS = 15;
      if (this.config.color_by) {
        const nColors = new Set(this.data.map(r => r[this.config.color_by])).size;
        if (nColors > MAX_COLOR_LEVELS) {
          // Aggregated: no series_by escape hatch, hard stop.
          // Individual/timeline: nudge the user toward series_by (or a
          // lower-cardinality grouping column like arm).
          const hint = fam === 'aggregated'
            ? `Pick a column with \u2264${MAX_COLOR_LEVELS} categories.`
            : `Use <code>series_by</code> to split into series (e.g. USUBJID); keep <code>color_by</code> for low-cardinality grouping (e.g. ARM).`;
          this.chartGrid.innerHTML = `<div class="vd-empty-state"><p class="vd-empty-text">Too many color levels (${nColors}). ${hint}</p></div>`;
          return;
        }
      }
      if (fam === 'aggregated') this._renderAggregated();
      else if (fam === 'timeline') this._renderTimeline();
      else this._renderIndividual();

      // Resize after render + watch for container becoming visible (dock tab switch)
      setTimeout(() => { for (const c of this.charts) c.resize(); }, 300);
      this._observeResize();
    }

    // -- Aggregated rendering -------------------------------------------------

    _renderAggregated() {
      const agg = this._aggregate();
      if (agg.length === 0) {
        this.chartGrid.innerHTML = '<div class="vd-empty-state"><p class="vd-empty-text">No data to chart</p></div>';
        return;
      }

      const facets = [...new Set(agg.map(a => a.facet))].sort();
      const colors = [...new Set(agg.map(a => a.color))].filter(c => c !== '__all__').sort();
      const palette = BLOCKR_PALETTE;
      const singleFacet = facets.length === 1;

      // Switch grid off for single facet
      this.chartGrid.classList.toggle('dd-chart-grid-single', singleFacet);

      const sortBy = this.config.sort_by || 'alpha';
      const sortDir = this.config.sort_dir === 'desc' ? -1 : 1;

      // Ordering for the category axis. "alpha" = group name;
      // "value" = total of the computed metric across color stacks;
      // otherwise, a raw-data column whose minimum per group orders the axis.
      const orderGroups = (facetData) => {
        const groups = [...new Set(facetData.map(a => a.group))];
        if (sortBy === 'alpha') {
          return groups.sort((a, b) => a.localeCompare(b) * sortDir);
        }
        if (sortBy === 'value') {
          const totals = {};
          for (const a of facetData) totals[a.group] = (totals[a.group] || 0) + a.value;
          return groups.sort((a, b) => (totals[a] - totals[b]) * sortDir);
        }
        // Column name: look up each group's min over the raw data.
        const groupCol = this.config.group_by;
        if (!groupCol) return groups.sort((a, b) => a.localeCompare(b) * sortDir);
        const mins = {};
        for (const r of this.data) {
          const g = String(r[groupCol] ?? '');
          const v = Number(r[sortBy]);
          if (!isNaN(v) && (mins[g] == null || v < mins[g])) mins[g] = v;
        }
        return groups.sort((a, b) => {
          const av = mins[a], bv = mins[b];
          if (av == null && bv == null) return a.localeCompare(b) * sortDir;
          if (av == null) return 1;
          if (bv == null) return -1;
          return (av - bv) * sortDir;
        });
      };

      for (const facet of facets) {
        const facetData = agg.filter(a => a.facet === facet);
        const groups = orderGroups(facetData);

        let container;
        if (singleFacet) {
          container = this.chartGrid;
        } else {
          container = document.createElement('div');
          container.className = 'dd-facet';
          if (facet !== '__all__') {
            const label = document.createElement('div');
            label.className = 'dd-facet-label';
            label.textContent = facet;
            container.appendChild(label);
          }
          this.chartGrid.appendChild(container);
        }

        const chartDiv = document.createElement('div');
        chartDiv.className = 'dd-chart';
        const ct = this.config.chart_type;
        chartDiv.style.height = (ct === 'pie' || ct === 'treemap')
          ? '350px'
          : Math.max(350, groups.length * 28 + 60) + 'px';
        container.appendChild(chartDiv);

        const option = this._buildAggregatedOption(facetData, groups, colors, palette);
        if (!option) {
          chartDiv.innerHTML = '<div class="vd-empty-state"><p class="vd-empty-text">Boxplot needs a numeric metric</p></div>';
          continue;
        }
        const chart = echarts.init(chartDiv, this.theme || undefined);
        this.charts.push(chart);
        chart.setOption(option, true);

        chart.on('click', (params) => {
          const clickedGroup = params.name || (params.value && params.value[0]);
          if (!clickedGroup) return;
          this._selected = this._selected === clickedGroup ? null : clickedGroup;
          this._updateHighlight();
          this._sendCategoricalFilter();
        });
      }
      this._updateHighlight();
    }

    _buildAggregatedOption(facetData, groups, colors, palette) {
      const ct = this.config.chart_type;
      const ax = { labelColor: AXIS_LABEL_COLOR, fontSize: 11, splitLineColor: SPLIT_LINE_COLOR };

      if (ct === 'pie') return this._buildPie(facetData, groups, palette);
      if (ct === 'boxplot') return this._buildBoxplot(groups, palette, ax);
      if (ct === 'treemap') return this._buildTreemap(facetData, groups, palette);

      const series = [];
      if (colors.length === 0) {
        series.push({ type: 'bar', data: groups.map(g => { const d = facetData.find(a => a.group === g); return d ? d.value : 0; }), itemStyle: { color: palette[0] }, barWidth: '60%', emphasis: { focus: 'self' } });
      } else {
        for (let ci = 0; ci < colors.length; ci++) {
          const color = colors[ci];
          series.push({
            type: 'bar', name: color,
            // Use null (not 0) for missing (group, color) combos. Stacked
            // bars skip nulls — this ensures a patient on a single arm
            // renders as one colored segment, even if the other arm series
            // get constructed for the rest of the cohort.
            data: groups.map(g => {
              const d = facetData.find(a => a.group === g && a.color === color);
              return d ? d.value : null;
            }),
            stack: 'stack',
            itemStyle: { color: palette[ci % palette.length] },
            barWidth: '60%',
            emphasis: { focus: 'self' }
          });
        }
      }

      return {
        ...(this.theme ? {} : { backgroundColor: 'transparent' }),
        textStyle: { fontFamily: BLOCKR_FONT },
        tooltip: { trigger: 'axis', axisPointer: { type: 'shadow' }, confine: true },
        toolbox: TOOLBOX,
        legend: colors.length > 0 ? { show: true, bottom: 0, textStyle: { fontSize: 11 } } : undefined,
        grid: { left: 160, right: 5, top: 30, bottom: colors.length > 0 ? 55 : 20 },
        xAxis: { type: 'value', axisLabel: { color: ax.labelColor, fontSize: ax.fontSize }, axisLine: { lineStyle: { color: AXIS_LINE_COLOR } }, splitLine: { lineStyle: { color: ax.splitLineColor, type: 'dashed' } } },
        yAxis: { type: 'category', data: groups, inverse: true, axisLabel: { color: ax.labelColor, fontSize: ax.fontSize, align: 'left', margin: 150, width: 145, overflow: 'truncate', ellipsis: '\u2026' }, axisLine: { show: false }, axisTick: { show: false } },
        series
      };
    }

    _buildPie(facetData, groups, palette) {
      const pieData = groups.map((g, i) => {
        const total = facetData.filter(a => a.group === g).reduce((s, a) => s + a.value, 0);
        return { name: g, value: total, itemStyle: { color: palette[i % palette.length] } };
      }).filter(d => d.value > 0);
      return { ...(this.theme ? {} : { backgroundColor: 'transparent' }), textStyle: { fontFamily: BLOCKR_FONT }, toolbox: TOOLBOX, tooltip: { trigger: 'item', formatter: '{b}: {c} ({d}%)' }, series: [{ type: 'pie', radius: ['30%', '70%'], data: pieData, label: { show: true, fontSize: 10, formatter: '{b}' }, emphasis: { itemStyle: { shadowBlur: 10, shadowColor: 'rgba(0,0,0,0.2)' } } }] };
    }

    _buildTreemap(facetData, groups, palette) {
      const tmData = groups.map((g, i) => {
        const total = facetData.filter(a => a.group === g).reduce((s, a) => s + a.value, 0);
        return { name: g, value: total, itemStyle: { color: palette[i % palette.length] } };
      }).filter(d => d.value > 0);
      return { ...(this.theme ? {} : { backgroundColor: 'transparent' }), textStyle: { fontFamily: BLOCKR_FONT }, toolbox: TOOLBOX, tooltip: { trigger: 'item', formatter: '{b}: {c}' }, series: [{ type: 'treemap', data: tmData, width: '100%', height: '100%', roam: false, nodeClick: false, breadcrumb: { show: false }, label: { show: true, fontSize: 12, formatter: '{b}\n{c}' }, itemStyle: { borderColor: '#fff', borderWidth: 2, gapWidth: 2 }, emphasis: { itemStyle: { shadowBlur: 10, shadowColor: 'rgba(0,0,0,0.15)' } } }] };
    }

    _buildBoxplot(groups, palette, ax) {
      const groupBy = this.config.group_by;
      const metric = this.config.metric;
      if (metric === '.count') return null;
      const boxData = groups.map(g => {
        const vals = this.data.filter(r => String(r[groupBy]) === g && r[metric] != null).map(r => Number(r[metric])).sort((a, b) => a - b);
        if (vals.length === 0) return [0, 0, 0, 0, 0];
        const q = (p) => { const i = p * (vals.length - 1); const lo = Math.floor(i); return lo === i ? vals[lo] : vals[lo] + (vals[lo + 1] - vals[lo]) * (i - lo); };
        const q1 = q(0.25), q3 = q(0.75), iqr = q3 - q1;
        const lo = Math.max(vals[0], q1 - 1.5 * iqr);
        const hi = Math.min(vals[vals.length - 1], q3 + 1.5 * iqr);
        return [lo, q1, q(0.5), q3, hi];
      });
      return { ...(this.theme ? {} : { backgroundColor: 'transparent' }), textStyle: { fontFamily: BLOCKR_FONT }, toolbox: TOOLBOX, tooltip: { trigger: 'item', confine: true }, grid: { left: 160, right: 5, top: 30, bottom: 20 }, xAxis: { type: 'value', axisLabel: { color: ax.labelColor, fontSize: ax.fontSize }, axisLine: { lineStyle: { color: AXIS_LINE_COLOR } } }, yAxis: { type: 'category', data: groups, inverse: true, axisLabel: { color: ax.labelColor, fontSize: ax.fontSize, align: 'left', margin: 150, width: 145, overflow: 'truncate', ellipsis: '\u2026' }, axisLine: { show: false } }, series: [{ type: 'boxplot', data: boxData, itemStyle: { color: palette[0] + '22', borderColor: palette[0] } }] };
    }

    // -- Individual rendering -------------------------------------------------

    _renderIndividual() {
      const { x_col, y_col, color_by, facet_by, series_by } = this.config;
      if (!x_col || !y_col) {
        this.chartGrid.innerHTML = '<div class="vd-empty-state"><p class="vd-empty-text">Select X and Y columns</p></div>';
        return;
      }

      const ct = this.config.chart_type;
      const isLine = ct === 'line';
      const palette = BLOCKR_PALETTE;
      const ax = { labelColor: AXIS_LABEL_COLOR, fontSize: 11, splitLineColor: SPLIT_LINE_COLOR };

      const facets = facet_by
        ? [...new Set(this.data.map(r => String(r[facet_by] ?? '')))].sort()
        : ['__all__'];

      // series_by is the primary per-entity splitter. If not set, fall back
      // to the legacy behaviour: one series per distinct color_by value.
      // This keeps existing configs (e.g. aggregated charts that rely on
      // color_by stacking) working without change.
      const splitCol = series_by || color_by;
      let seriesLevels = splitCol
        ? [...new Set(this.data.map(r => String(r[splitCol] ?? '')))].sort()
        : [];
      const singleFacet = facets.length === 1;

      // Hard-cap and messaging for trajectory overlays at very high series
      // count (spec §2). Only applies to line charts — scatter stays uncapped.
      let capMessage = null;
      if (isLine && seriesLevels.length > TRAJ_HARD_CAP) {
        capMessage =
          `Showing first ${TRAJ_HARD_CAP} of ${seriesLevels.length} series` +
          ` (` + (splitCol || 'series') + `) — filter upstream to narrow.`;
        seriesLevels = seriesLevels.slice(0, TRAJ_HARD_CAP);
      }

      // Color resolver: when series_by is set separately from color_by, look
      // up each series' color_by value to share a palette color across
      // series that belong to the same color group (e.g. all patients on
      // ARM A share one color). When series_by equals color_by or only
      // color_by is set, index by series level.
      const colorForLevel = (level, index) => {
        if (!color_by) return palette[0];
        if (series_by && series_by !== color_by) {
          const rep = this.data.find(r => String(r[series_by]) === level);
          const cv = rep ? String(rep[color_by] ?? '') : '';
          if (!this._colorLookup) this._colorLookup = {};
          if (!(cv in this._colorLookup)) {
            this._colorLookup[cv] = palette[Object.keys(this._colorLookup).length % palette.length];
          }
          return this._colorLookup[cv];
        }
        return palette[index % palette.length];
      };
      this._colorLookup = null;

      // Line opacity / marker thresholds by series count
      const seriesCount = seriesLevels.length || 1;
      let lineOpacity = 1, symbol = 'circle';
      if (isLine) {
        if (seriesCount > TRAJ_REDUCED_MAX) { lineOpacity = 0.15; symbol = 'none'; }
        else if (seriesCount > TRAJ_FULL_MAX) { lineOpacity = 0.35; symbol = 'none'; }
      }

      // Axis types from column metadata
      const xAxisType = this._axisTypeFor(x_col);
      const yAxisType = this._axisTypeFor(y_col);
      const xCats = xAxisType === 'category' ? this._orderedCategories(x_col) : null;
      const yCats = yAxisType === 'category' ? this._orderedCategories(y_col) : null;

      const encodeX = (v) => xAxisType === 'category' ? String(v ?? '') : Number(v);
      const encodeY = (v) => yAxisType === 'category' ? String(v ?? '') : Number(v);

      this.chartGrid.classList.toggle('dd-chart-grid-single', singleFacet);

      for (const facet of facets) {
        const rows = facet === '__all__' ? this.data : this.data.filter(r => String(r[facet_by]) === facet);

        let container;
        if (singleFacet) {
          container = this.chartGrid;
        } else {
          container = document.createElement('div');
          container.className = 'dd-facet';
          if (facet !== '__all__') {
            const label = document.createElement('div');
            label.className = 'dd-facet-label';
            label.textContent = facet;
            container.appendChild(label);
          }
          this.chartGrid.appendChild(container);
        }

        const chartDiv = document.createElement('div');
        chartDiv.className = 'dd-chart';
        chartDiv.style.height = '400px';
        container.appendChild(chartDiv);

        const chart = echarts.init(chartDiv, this.theme || undefined);
        this.charts.push(chart);

        const dm = this.config.dot_size_mult   ?? 1.0;
        const lm = this.config.line_width_mult ?? 1.0;
        const lineMarkerPx = (symbol === 'none'
          ? BASE_LINE_MARKER
          : BASE_LINE_MARKER * dm);
        const lineBaseW  = BASE_LINE_WIDTH * lm;
        const lineHoverW = BASE_LINE_WIDTH * 1.7 * lm;
        const scatterPx  = BASE_SCATTER_SIZE * dm;

        const stepMode = this.config.step;  // null | 'start' | 'end' | 'middle'
        const smoother = this.config.smoother || 'none';
        const smootherSeries = this.config.smoother_series || null;
        const loCol = this.config.lo_col;
        const hiCol = this.config.hi_col;
        const refX = Array.isArray(this.config.ref_x) ? this.config.ref_x : [];
        const refY = Array.isArray(this.config.ref_y) ? this.config.ref_y : [];

        const mkSeries = (name, data, color) => ({
          type: isLine ? 'line' : 'scatter',
          name: name,
          data: data,
          step: isLine && stepMode ? stepMode : undefined,
          // `triggerLineEvent: true` makes click/hover fire when the
          // cursor is on the line itself, not only on symbols. Without
          // it, click on a symbol-less thin line never registers.
          triggerLineEvent: isLine ? true : undefined,
          symbol: isLine ? symbol : 'circle',
          symbolSize: isLine ? lineMarkerPx : scatterPx,
          itemStyle: { color: color, cursor: 'pointer' },
          lineStyle: isLine ? { width: lineBaseW, opacity: lineOpacity } : undefined,
          emphasis: isLine ? { focus: 'series', lineStyle: { width: lineHoverW, opacity: 1 } } : { focus: 'self' },
          blur: isLine ? { lineStyle: { opacity: 0.05 } } : undefined
        });

        // R precomputes the smoother (lm or loess) per group and sends the
        // line points via config.smoother_series. JS just renders.
        const smootherLine = (groupName) => {
          if (smoother === 'none' || !smootherSeries) return null;
          const key = groupName != null ? String(groupName) : '__all__';
          const s = smootherSeries[key];
          if (!s || !s.x || !s.y) return null;
          const out = [];
          for (let i = 0; i < s.x.length; i++) {
            if (Number.isFinite(s.x[i]) && Number.isFinite(s.y[i])) {
              out.push([s.x[i], s.y[i]]);
            }
          }
          return out.length >= 2 ? out : null;
        };

        // Helper: error-bar custom series builder. Renders vertical segments
        // (loCol, hiCol) at each x for one group.
        const mkErrBarSeries = (name, errPts, color) => ({
          type: 'custom',
          name: name + ' (CI)',
          silent: true,
          z: 1,
          data: errPts,  // [[x, lo, hi], ...]
          renderItem: (params, api) => {
            const x = api.value(0), lo = api.value(1), hi = api.value(2);
            const pLo = api.coord([x, lo]);
            const pHi = api.coord([x, hi]);
            const w = 4;
            return {
              type: 'group',
              children: [
                { type: 'line', shape: { x1: pLo[0], y1: pLo[1], x2: pHi[0], y2: pHi[1] }, style: { stroke: color, lineWidth: 1 } },
                { type: 'line', shape: { x1: pLo[0]-w, y1: pLo[1], x2: pLo[0]+w, y2: pLo[1] }, style: { stroke: color, lineWidth: 1 } },
                { type: 'line', shape: { x1: pHi[0]-w, y1: pHi[1], x2: pHi[0]+w, y2: pHi[1] }, style: { stroke: color, lineWidth: 1 } }
              ]
            };
          }
        });

        const series = [];
        const pushOverlays = (name, pts, color, rawRows) => {
          // Smoother overlay (scatter charts only) — uses R-precomputed
          // points from config.smoother_series.
          if (smoother !== 'none' && !isLine) {
            const ln = smootherLine(name);
            if (ln) series.push({
              type: 'line',
              name: (name || 'fit') + ' (' + smoother + ')',
              data: ln,
              silent: true,
              showSymbol: false,
              lineStyle: { color: color, width: 2, type: 'solid', opacity: 0.9 },
              z: 2
            });
          }
          // Error-bar overlay (line charts with lo_col/hi_col)
          if (isLine && loCol && hiCol && rawRows && rawRows.length) {
            const errPts = rawRows
              .filter(r => r[x_col] != null && r[loCol] != null && r[hiCol] != null)
              .map(r => [encodeX(r[x_col]), Number(r[loCol]), Number(r[hiCol])]);
            if (errPts.length) series.push(mkErrBarSeries(name || 'errbar', errPts, color));
          }
        };

        if (seriesLevels.length === 0) {
          const grpRows = rows.filter(r => r[x_col] != null && r[y_col] != null);
          const pts = grpRows.map(r => [encodeX(r[x_col]), encodeY(r[y_col])]);
          if (isLine && xAxisType !== 'category') pts.sort((a, b) => a[0] - b[0]);
          series.push(mkSeries(undefined, pts, palette[0]));
          pushOverlays(undefined, pts, palette[0], grpRows);
        } else {
          for (let ci = 0; ci < seriesLevels.length; ci++) {
            const cl = seriesLevels[ci];
            const grpRows = rows.filter(r => String(r[splitCol]) === cl && r[x_col] != null && r[y_col] != null);
            const pts = grpRows.map(r => [encodeX(r[x_col]), encodeY(r[y_col])]);
            if (isLine && xAxisType !== 'category') pts.sort((a, b) => a[0] - b[0]);
            const color = colorForLevel(cl, ci);
            series.push(mkSeries(cl, pts, color));
            pushOverlays(cl, pts, color, grpRows);
          }
        }

        // Reference-line overlays (ref_x vertical, ref_y horizontal)
        if (series.length > 0 && (refX.length || refY.length)) {
          const refData = [];
          for (const v of refX) refData.push({ xAxis: Number(v) });
          for (const v of refY) refData.push({ yAxis: Number(v) });
          series[0].markLine = {
            silent: true,
            symbol: 'none',
            lineStyle: { color: '#dc2626', type: 'dashed', width: 1.5 },
            label: { show: false },
            data: refData
          };
        }

        // Brush config: lineX for line charts, rect for scatter
        const brushType = isLine ? ['lineX'] : ['rect'];

        // Legend only when distinct *colors* are few enough to read. With
        // series_by ≠ color_by, legend reflects color_by cardinality, not
        // series_by. If color_by is unset, no legend at all.
        const legendCardinality = color_by
          ? new Set(this.data.map(r => String(r[color_by] ?? ''))).size
          : 0;
        const showLegend = color_by && legendCardinality > 0 && legendCardinality <= 15;

        // When series_by ≠ color_by, build an explicit legend over the
        // color_by levels (e.g. "F", "M") rather than letting echarts
        // enumerate series names (e.g. every USUBJID). The legend item
        // colors come from this._colorLookup populated inside
        // colorForLevel above. We still map clicks to toggle every series
        // that shares that color_by value.
        const useColorByLegend = showLegend && color_by && series_by &&
          series_by !== color_by;
        let colorByLegendData = null;
        let seriesByColorByVal = null;
        if (useColorByLegend) {
          const lookup = this._colorLookup || {};
          const cbLevels = [...new Set(this.data.map(
            r => String(r[color_by] ?? '')
          ))].sort();
          colorByLegendData = cbLevels.map((lvl, i) => ({
            name: lvl,
            itemStyle: { color: lookup[lvl] || palette[i % palette.length] }
          }));
          // Precompute series-name → color_by-value map used by the
          // legend click handler to toggle all series in a color group.
          seriesByColorByVal = {};
          for (const lvl of cbLevels) seriesByColorByVal[lvl] = [];
          for (const sl of seriesLevels) {
            const rep = this.data.find(r => String(r[series_by]) === sl);
            const cv = rep ? String(rep[color_by] ?? '') : '';
            if (cv in seriesByColorByVal) seriesByColorByVal[cv].push(sl);
          }
          // Echarts only renders legend items whose `name` matches a
          // real series. Inject one empty series per color level so the
          // legend has something to bind to. These series hold no data
          // so they don't paint, but they make the legend chip appear.
          for (const { name, itemStyle } of colorByLegendData) {
            series.push({
              type: isLine ? 'line' : 'scatter',
              name,
              data: [],
              itemStyle,
              lineStyle: { color: itemStyle.color },
              showSymbol: false,
              silent: true
            });
          }
        }

        // Axis names omitted: the X / Y pickers above the chart already
        // show the selected column, so echoing it on the axis is redundant
        // and competes with the legend for the bottom margin.
        const xAxisSpec = {
          type: xAxisType,
          axisLabel: { color: ax.labelColor, fontSize: ax.fontSize },
          axisLine: { lineStyle: { color: AXIS_LINE_COLOR } },
          splitLine: { lineStyle: { color: ax.splitLineColor, type: 'dashed' } },
          scale: true
        };
        if (xCats) xAxisSpec.data = xCats;

        const yAxisSpec = {
          type: yAxisType,
          axisLabel: { color: ax.labelColor, fontSize: ax.fontSize },
          axisLine: { lineStyle: { color: AXIS_LINE_COLOR } },
          splitLine: { lineStyle: { color: ax.splitLineColor, type: 'dashed' } },
          scale: true
        };
        if (yCats) yAxisSpec.data = yCats;

        // Brushing is skipped when the x-axis is categorical (echarts' brush
        // needs continuous coords), for line charts — on a per-patient
        // trajectory overlay, clicking a line to filter by USUBJID is the
        // expected drill-down, and an active brush cursor would consume
        // those clicks before they reach the series — and for scatter when
        // `series_by` is set. With `series_by` the user's intent is "click
        // a dot to filter to that series" (e.g. one dot per policy → click
        // → filter to that policy); leaving brush active fires a 1-pixel
        // brushSelected on the click point that races the click handler
        // and overwrites the categorical filter with a range filter on
        // (x == click_x & y == click_y), matching at most one row.
        const brushable = xAxisType !== 'category' && !isLine && !series_by;

        const option = {
          ...(this.theme ? {} : { backgroundColor: 'transparent' }),
          textStyle: { fontFamily: BLOCKR_FONT },
          tooltip: { trigger: 'item', formatter: (p) => `${x_col}: ${p.value[0]}<br>${y_col}: ${p.value[1]}` + (p.seriesName ? `<br>${color_by || 'series'}: ${p.seriesName}` : ''), confine: true },
          // Always set explicitly; leaving legend undefined lets echarts
          // auto-render one per series, which eats the plot area when
          // series_by is high-cardinality (e.g. USUBJID).
          legend: useColorByLegend
            ? { show: true, bottom: 0, textStyle: { fontSize: 11 },
                data: colorByLegendData }
            : showLegend
              ? { show: true, bottom: 0, textStyle: { fontSize: 11 } }
              : { show: false },
          grid: { left: 50, right: 5, top: 30, bottom: showLegend ? 55 : 30 },
          xAxis: xAxisSpec,
          yAxis: yAxisSpec,
          toolbox: TOOLBOX,
          // brush.toolbox lists the brush types REGISTERED for use. Must
          // include the types we activate (rect for scatter, lineX for line)
          // and the types referenced by toolbox.feature.brush.type, otherwise
          // clicking an icon (or takeGlobalCursor) is a no-op.
          brush: brushable ? { toolbox: ['rect', 'lineX', 'clear'], xAxisIndex: 0, yAxisIndex: isLine ? undefined : 0, brushStyle: { color: 'rgba(0, 114, 178, 0.1)', borderColor: 'rgba(0, 114, 178, 0.5)', borderWidth: 1 }, throttleDelay: 300 } : undefined,
          series
        };

        chart.setOption(option, true);

        // When the legend shows color_by levels (not series), intercept
        // legend clicks and fan them out to every series that belongs to
        // the clicked color group. Otherwise a click on "F" would do
        // nothing (no series is named "F") and the user couldn't toggle.
        if (useColorByLegend && seriesByColorByVal) {
          chart.on('legendselectchanged', (params) => {
            const cv = params.name;
            const targets = seriesByColorByVal[cv];
            if (!targets || targets.length === 0) return;
            const show = !!(params.selected && params.selected[cv]);
            const action = show ? 'legendSelect' : 'legendUnSelect';
            for (const sn of targets) {
              chart.dispatchAction({ type: action, name: sn });
            }
          });
        }

        // Click → categorical filter on the series. Filter column is
        // series_by when set (e.g. USUBJID), otherwise color_by, otherwise
        // USUBJID as a sensible default.
        chart.on('click', (params) => {
          if (params.componentType !== 'series') return;
          const col = series_by || color_by || 'USUBJID';
          const val = params.seriesName;
          if (!val) return;
          this._selectedColumn = col;
          this._selected = val;
          this._sendCategoricalFilter(col, [val]);
          this._updateStatus();
        });

        // Activate brush mode by default (no need to click toolbox first)
        if (brushable) {
          chart.dispatchAction({
            type: 'takeGlobalCursor',
            key: 'brush',
            brushOption: { brushType: isLine ? 'lineX' : 'rect', brushMode: 'single' }
          });
        }

        chart.on('brushSelected', (params) => {
          if (!params.batch || params.batch.length === 0) return;

          // Collect all selected data indices across all areas/series
          const batch = params.batch[0];
          if (!batch?.selected) {
            this._hasBrushFilter = false;
            this._updateStatus();
            this._sendClearFilter();
            return;
          }

          // Gather all selected indices from all series
          let allIndices = [];
          for (const s of batch.selected) {
            if (s.dataIndex && s.dataIndex.length > 0) {
              allIndices = allIndices.concat(s.dataIndex);
            }
          }
          allIndices = [...new Set(allIndices)]; // dedupe

          if (allIndices.length === 0) {
            this._hasBrushFilter = false;
            this._updateStatus();
            this._sendClearFilter();
            return;
          }

          // Clear brush on other facet charts
          for (const other of this.charts) {
            if (other !== chart) other.dispatchAction({ type: 'brush', areas: [] });
          }

          // Compute x/y range from selected points' actual values
          const opt = chart.getOption();
          const seriesData = [];
          for (const s of (opt.series || [])) {
            if (s.data) seriesData.push(...s.data);
          }

          let xVals = [], yVals = [];
          for (const idx of allIndices) {
            const pt = seriesData[idx];
            if (pt) {
              const x = Array.isArray(pt) ? pt[0] : pt.value?.[0];
              const y = Array.isArray(pt) ? pt[1] : pt.value?.[1];
              if (x != null) xVals.push(x);
              if (y != null) yVals.push(y);
            }
          }

          this._hasBrushFilter = true;
          this._updateStatus();

          if (xVals.length > 0) {
            const xRange = [Math.min(...xVals), Math.max(...xVals)];
            if (isLine) {
              this._sendRangeFilter(xRange, null);
            } else {
              const yRange = yVals.length > 0 ? [Math.min(...yVals), Math.max(...yVals)] : null;
              this._sendRangeFilter(xRange, yRange);
            }
          }
        });

        chart.on('brush', (params) => {
          if (!params.areas || params.areas.length === 0) {
            this._hasBrushFilter = false;
            this._updateStatus();
            this._sendClearFilter();
          }
        });
      }

      this._capMessage = capMessage;
      this._updateStatus();
    }

    // -- Timeline (gantt) rendering ------------------------------------------

    _renderTimeline() {
      const { x_col, x_end_col, y_col, color_by, facet_by, sort_by, series_by } = this.config;
      const sortDir = this.config.sort_dir === 'desc' ? -1 : 1;
      if (!x_col || !y_col) {
        this.chartGrid.innerHTML = '<div class="vd-empty-state"><p class="vd-empty-text">Select X (start) and Y (term) columns</p></div>';
        return;
      }

      const ax = { labelColor: AXIS_LABEL_COLOR, fontSize: 11, splitLineColor: SPLIT_LINE_COLOR };
      const palette = BLOCKR_PALETTE;
      const xAxisType = this._axisTypeFor(x_col);
      const xCats = xAxisType === 'category' ? this._orderedCategories(x_col) : null;

      const facets = facet_by
        ? [...new Set(this.data.map(r => String(r[facet_by] ?? '')))].sort()
        : ['__all__'];
      const singleFacet = facets.length === 1;
      this.chartGrid.classList.toggle('dd-chart-grid-single', singleFacet);

      // Distinct color levels (sorted). With one named series per level
      // below, echarts assigns a color to each series from option.color
      // (BLOCKR_PALETTE) — no manual lookup needed.
      const colorLevels = color_by
        ? [...new Set(this.data.map(r => String(r[color_by] ?? '')))].sort()
        : [];

      // Convert an x-axis value to a numeric/time coord, or to a category
      // index when the axis is categorical.
      const xCoord = (v, cats) => {
        if (xAxisType === 'category') {
          const i = cats.indexOf(String(v ?? ''));
          return i < 0 ? 0 : i;
        }
        return Number(v);
      };

      // Sort helper — ascending min of the sort column per category
      const sortTerms = (rows) => {
        const sb = sort_by || 'onset';
        if (sb === 'alpha') {
          return [...new Set(rows.map(r => String(r[y_col] ?? '')))]
            .sort((a, b) => a.localeCompare(b) * sortDir);
        }
        const sortCol = (sb === 'onset') ? x_col : sb;
        const mins = {};
        for (const r of rows) {
          const k = String(r[y_col] ?? '');
          let v = r[sortCol];
          if (xAxisType === 'category' && sortCol === x_col) {
            v = xCoord(v, xCats);
          } else {
            v = Number(v);
          }
          if (!isNaN(v) && (mins[k] == null || v < mins[k])) mins[k] = v;
        }
        return Object.keys(mins).sort((a, b) => {
          const av = mins[a], bv = mins[b];
          if (av == null && bv == null) return a.localeCompare(b) * sortDir;
          if (av == null) return 1;
          if (bv == null) return -1;
          return (av - bv) * sortDir;
        });
      };

      for (const facet of facets) {
        const rows = facet === '__all__' ? this.data
          : this.data.filter(r => String(r[facet_by]) === facet);
        if (rows.length === 0) continue;

        let container;
        if (singleFacet) {
          container = this.chartGrid;
        } else {
          container = document.createElement('div');
          container.className = 'dd-facet';
          if (facet !== '__all__') {
            const label = document.createElement('div');
            label.className = 'dd-facet-label';
            label.textContent = facet;
            container.appendChild(label);
          }
          this.chartGrid.appendChild(container);
        }

        const terms = sortTerms(rows);

        const chartDiv = document.createElement('div');
        chartDiv.className = 'dd-chart';
        // Extra 20px when the legend is on, to keep the legend visually
        // separated from the x-axis labels.
        const heightExtra = (color_by && colorLevels.length > 0) ? 100 : 80;
        chartDiv.style.height = Math.max(200, terms.length * 28 + heightExtra) + 'px';
        container.appendChild(chartDiv);

        const chart = echarts.init(chartDiv, this.theme || undefined);
        this.charts.push(chart);

        const barData = [];
        for (const r of rows) {
          if (r[x_col] == null) continue;
          const term = String(r[y_col] ?? '');
          const lane = terms.indexOf(term);
          if (lane < 0) continue;
          const s = xCoord(r[x_col], xCats);
          let e;
          if (x_end_col && r[x_end_col] != null && !Number.isNaN(Number(r[x_end_col]))) {
            e = xCoord(r[x_end_col], xCats);
          } else {
            // Single-day event — render a narrow dot.
            e = s;
          }
          barData.push({
            value: [s, e, lane, term, r[color_by] ?? '', r['USUBJID'] ?? '',
                    r[series_by] ?? '']
          });
        }

        const xAxisSpec = {
          type: xAxisType,
          axisLabel: { color: ax.labelColor, fontSize: ax.fontSize },
          axisLine: { lineStyle: { color: AXIS_LINE_COLOR } },
          splitLine: { lineStyle: { color: ax.splitLineColor, type: 'dashed' } },
          scale: true
        };
        if (xCats) xAxisSpec.data = xCats;

        // Auto-legend: show whenever color_by is set. The legend is
        // scroll-type, so high cardinality (AETERM with 200+ values)
        // scrolls instead of being suppressed. Series below are split
        // per color level so echarts has a real series to bind each
        // legend chip to — without that, legend items never paint.
        const showLegend = !!color_by && colorLevels.length > 0;
        // Names only — echarts derives each chip's color from the matching
        // series via option.color cycling.
        const legendData = showLegend ? colorLevels.slice() : null;

        const renderItemFn = (params, api) => {
          const start = api.coord([api.value(0), api.value(2)]);
          const end = api.coord([api.value(1), api.value(2)]);
          const h = api.size([0, 1])[1] * 0.6;
          const barW = Math.max(end[0] - start[0], 4);
          const rect = echarts.graphic.clipRectByRect(
            { x: start[0], y: start[1] - h / 2, width: barW, height: h },
            { x: params.coordSys.x, y: params.coordSys.y,
              width: params.coordSys.width, height: params.coordSys.height }
          );
          if (!rect) return;
          // On-bar text picks the most specific label available so the
          // bar isn't redundant with the y-axis: series_by (per-bar
          // detail, e.g. AETERM) > color_by (e.g. AEBODSYS) > y_col.
          const label = series_by
            ? String(api.value(6) ?? '')
            : color_by
              ? String(api.value(4) ?? '')
              : String(api.value(3) ?? '');
          const children = [{
            type: 'rect',
            shape: Object.assign({}, rect, { r: 3 }),
            style: api.style()
          }];
          if (barW > 50 && label) {
            children.push({
              type: 'text',
              style: {
                text: label,
                x: rect.x + 6,
                y: rect.y + rect.height / 2,
                fill: '#fff',
                fontSize: 10,
                fontWeight: 500,
                fontFamily: BLOCKR_FONT,
                textVerticalAlign: 'middle',
                truncate: { outerWidth: barW - 12 }
              }
            });
          }
          return { type: 'group', children: children };
        };

        // Build series. When color_by is set, split barData by level so
        // each color group is its own named series — that's what makes
        // legend chips render and click-to-toggle work. Otherwise fall
        // back to a single anonymous series.
        let seriesArray;
        if (showLegend) {
          const buckets = new Map(colorLevels.map(lvl => [lvl, []]));
          for (const d of barData) {
            const lvl = String(d.value[4] ?? '');
            if (buckets.has(lvl)) buckets.get(lvl).push(d);
          }
          seriesArray = colorLevels.map(lvl => ({
            type: 'custom',
            name: lvl,
            data: buckets.get(lvl) || [],
            encode: { x: [0, 1], y: 2 },
            renderItem: renderItemFn
          }));
        } else {
          seriesArray = [{
            type: 'custom',
            data: barData,
            encode: { x: [0, 1], y: 2 },
            renderItem: renderItemFn
          }];
        }

        const option = {
          ...(this.theme ? {} : { backgroundColor: 'transparent' }),
          color: palette,
          textStyle: { fontFamily: BLOCKR_FONT },
          tooltip: {
            trigger: 'item',
            confine: true,
            formatter: (p) => {
              const v = p.value;
              const term = v[3] || '';
              const colorVal = v[4] || '';
              const subj = v[5] || '';
              const detail = v[6] || '';
              // Headline is the most specific label available: series_by
              // (per-bar detail) wins, else fall back to y_col (lane).
              const headline = detail || term;
              let html = `<div style="min-width:180px"><div style="font-weight:700;margin-bottom:4px">${headline}</div>`;
              if (detail && term && term !== detail) {
                html += `<div style="font-size:11px;color:#666">${term}</div>`;
              }
              if (colorVal) html += `<div style="font-size:11px;color:#666">${colorVal}</div>`;
              if (subj) html += `<div style="font-size:11px;color:#666">${subj}</div>`;
              html += '</div>';
              return html;
            }
          },
          legend: showLegend
            ? { show: true, bottom: 8, type: 'scroll', textStyle: { fontSize: 11 }, data: legendData }
            : { show: false },
          toolbox: TOOLBOX,
          grid: { left: 160, right: 10, top: 20, bottom: showLegend ? 60 : 30 },
          xAxis: xAxisSpec,
          yAxis: {
            type: 'category',
            data: terms,
            inverse: true,
            axisLabel: {
              color: ax.labelColor, fontSize: ax.fontSize,
              align: 'left', margin: 150, width: 145,
              overflow: 'truncate', ellipsis: '\u2026'
            },
            axisLine: { show: false },
            axisTick: { show: false },
            splitLine: { show: false }
          },
          series: seriesArray
        };

        chart.setOption(option, true);

        chart.on('click', (params) => {
          if (params.componentType !== 'series') return;
          const v = params.value;
          if (!v) return;
          const subj = v[5];
          if (!subj) return;
          this._selectedColumn = 'USUBJID';
          this._selected = String(subj);
          this._sendCategoricalFilter('USUBJID', [String(subj)]);
          this._updateStatus();
        });
      }

      this._capMessage = null;
      this._updateStatus();
    }

    // -- Highlight / selection ------------------------------------------------

    _updateHighlight() {
      const sel = this._selected;
      this._updateStatus();

      for (const chart of this.charts) {
        const option = chart.getOption();
        if (!option || !option.series) continue;

        // ECharts' setOption merges series by position, so a partial array
        // would overwrite series[0] for every iteration. Build the full
        // array so each series is updated in place.
        const cats = option.xAxis?.[0]?.data || option.yAxis?.[0]?.data || [];
        const newSeries = option.series.map((s) => {
          if (s.type === 'pie') {
            return { data: (s.data || []).map(d => d == null ? d : ({ ...d, itemStyle: { ...d.itemStyle, opacity: sel && d.name !== sel ? 0.2 : 1 } })) };
          }
          if (s.type === 'treemap') {
            return { data: (s.data || []).map(d => d == null ? d : ({ ...d, itemStyle: { ...d.itemStyle, opacity: sel && d.name !== sel ? 0.3 : 1 } })) };
          }
          // Individual/timeline families use echarts emphasis/blur states
          // for highlighting, driven natively by hover. Skip the manual
          // category-mask path — it would mis-apply to non-category data
          // (USUBJID on a trajectory, term on a gantt).
          if (s.type === 'boxplot' || s.type === 'line' || s.type === 'scatter' || s.type === 'custom') {
            return {};
          }
          if (cats.length === 0) return {};
          const newData = (s.data || []).map((v, i) => {
            // typeof null === 'object', so guard v before reading v.value;
            // this series data legitimately contains null gap values for
            // missing groups (see _renderAggregated). Without the `v &&`
            // check, null.value throws and aborts the whole render.
            const val = (v && typeof v === 'object' && !Array.isArray(v)) ? v.value : v;
            return { value: val, itemStyle: { opacity: sel ? (cats[i] === sel ? 1 : 0.15) : 1 } };
          });
          return { data: newData };
        });
        chart.setOption({ series: newSeries }, false);
      }
    }

    _clearBrush() {
      for (const chart of this.charts) chart.dispatchAction({ type: 'brush', areas: [] });
    }

    // -- Popover toggle -------------------------------------------------------

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

    // -- Status footer --------------------------------------------------------

    _updateStatus() {
      if (!this.statusEl) return;
      this.statusEl.innerHTML = '';

      const hasFilter = this._selected || this._hasBrushFilter;
      let text = 'No filter active';

      if (this._selected) {
        const col = this._selectedColumn;
        text = 'Filtered: ' + (col ? col + ' = ' : '') + this._selected;
      } else if (this._hasBrushFilter) {
        text = 'Brush filter active';
      }

      const span = document.createElement('span');
      span.className = 'dd-status-text';
      span.textContent = text;
      this.statusEl.appendChild(span);

      if (this._capMessage) {
        const cap = document.createElement('span');
        cap.className = 'dd-status-cap';
        cap.textContent = this._capMessage;
        this.statusEl.appendChild(cap);
      }

      if (hasFilter) {
        const resetBtn = document.createElement('button');
        resetBtn.className = 'dd-status-reset';
        resetBtn.textContent = 'Reset';
        resetBtn.addEventListener('click', () => {
          this._selected = null;
          this._selectedColumn = null;
          this._hasBrushFilter = false;
          this._clearBrush();
          this._updateHighlight();
          this._sendClearFilter();
        });
        this.statusEl.appendChild(resetBtn);
      }
    }

    // -- Communication --------------------------------------------------------

    // Emits a categorical filter. If `col` and `values` are provided, they
    // override the default (group_by + current _selected) — used by
    // click-to-filter on trajectory series and gantt bars, where the column
    // is USUBJID rather than group_by.
    _sendCategoricalFilter(col, values) {
      if (!this.el.id) return;
      const column = col !== undefined ? col : this.config.group_by;
      const vals = values !== undefined ? values
        : (this._selected ? [this._selected] : null);
      Shiny.setInputValue(this.el.id + '_action', {
        action: 'filter', filter_type: 'categorical',
        column: column,
        values: vals
      }, { priority: 'event' });
    }

    _sendRangeFilter(xRange, yRange) {
      if (!this.el.id) return;
      Shiny.setInputValue(this.el.id + '_action', {
        action: 'filter', filter_type: 'range',
        x_col: this.config.x_col, y_col: yRange ? this.config.y_col : null,
        x_range: xRange, y_range: yRange
      }, { priority: 'event' });
    }

    _sendClearFilter() {
      if (!this.el.id) return;
      Shiny.setInputValue(this.el.id + '_action', {
        action: 'filter',
        filter_type: this._family() === 'aggregated' ? 'categorical' : 'range',
        column: null, values: null,
        x_col: null, y_col: null, x_range: null, y_range: null
      }, { priority: 'event' });
    }

    _sendConfig() {
      if (!this.el.id) return;
      Shiny.setInputValue(this.el.id + '_action', {
        action: 'config',
        group_by: this.config.group_by,
        color_by: this.config.color_by || '',
        facet_by: this.config.facet_by || '',
        metric: this.config.metric,
        agg_fn: this.config.agg_fn,
        chart_type: this.config.chart_type,
        x_col: this.config.x_col || '',
        y_col: this.config.y_col || '',
        x_end_col: this.config.x_end_col || '',
        sort_by: this.config.sort_by || '',
        sort_dir: this.config.sort_dir || 'asc',
        series_by: this.config.series_by || '',
        smoother: this.config.smoother || 'none',
        lo_col: this.config.lo_col || '',
        hi_col: this.config.hi_col || ''
      }, { priority: 'event' });
    }

    _sendMults() {
      if (!this.el.id) return;
      Shiny.setInputValue(this.el.id + '_action', {
        action: 'set_mults',
        line_width_mult: this.config.line_width_mult ?? 1.0,
        dot_size_mult: this.config.dot_size_mult ?? 1.0
      }, { priority: 'event' });
    }

    _observeResize() {
      if (this._resizeObserver) this._resizeObserver.disconnect();
      this._resizeObserver = new ResizeObserver(() => {
        for (const c of this.charts) c.resize();
      });
      this._resizeObserver.observe(this.chartGrid);
    }

    resize() { for (const c of this.charts) c.resize(); }
    dispose() {
      if (this._resizeObserver) this._resizeObserver.disconnect();
      for (const c of this.charts) c.dispose();
      this.charts = [];
    }
  }

  // -- Shiny binding ----------------------------------------------------------

  const binding = new Shiny.InputBinding();
  Object.assign(binding, {
    find: (scope) => $(scope).find('.drilldown-chart-container'),
    getId: (el) => el.id || null,
    getValue: () => null,
    subscribe: () => {},
    unsubscribe: () => {},
    initialize: (el) => {
      el._block = new DrilldownChart(el);
      if (el._pendingTheme !== undefined) {
        el._block.setTheme(el._pendingTheme);
        delete el._pendingTheme;
      }
      if (el._pendingData) {
        const p = el._pendingData;
        el._block.setData(p.columns, p.data, p.config);
        delete el._pendingData;
      }
    }
  });
  Shiny.inputBindings.register(binding, 'blockr.drilldown');

  Shiny.addCustomMessageHandler('drilldown-data', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.setData(msg.columns, msg.data, msg.config);
    } else if (el) {
      el._pendingData = msg;
    } else {
      let n = 0;
      const t = setInterval(() => {
        n++;
        const el2 = document.getElementById(msg.id);
        if (el2?._block) { el2._block.setData(msg.columns, msg.data, msg.config); clearInterval(t); }
        else if (el2) { el2._pendingData = msg; clearInterval(t); }
        if (n > 50) clearInterval(t);
      }, 100);
    }
  });

  Shiny.addCustomMessageHandler('drilldown-theme', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.setTheme(msg.theme);
    } else if (el) {
      el._pendingTheme = msg.theme;
    } else {
      let n = 0;
      const t = setInterval(() => {
        n++;
        const el2 = document.getElementById(msg.id);
        if (el2?._block) { el2._block.setTheme(msg.theme); clearInterval(t); }
        else if (el2) { el2._pendingTheme = msg.theme; clearInterval(t); }
        if (n > 50) clearInterval(t);
      }, 100);
    }
  });

})();
