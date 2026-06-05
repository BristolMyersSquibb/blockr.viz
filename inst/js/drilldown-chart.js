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
  // Display-only number formatting. Data is now sent at full precision
  // (so click-to-filter equality round-trips), so trim noisy decimals
  // for tooltips / status text without touching the underlying value.
  const ddNum = (v) => {
    if (typeof v !== 'number' || !isFinite(v)) return v;
    if (Number.isInteger(v)) return v.toLocaleString();
    return Number(v.toPrecision(6)).toLocaleString(undefined, {
      maximumFractionDigits: 4
    });
  };
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
  // Base toolbox WITHOUT brush — bar/pie/treemap/boxplot/gantt and any
  // non-brushable scatter/line (categorical x, or series set) get no
  // brush icon, so the button is never a dead control.
  const TOOLBOX = {
    show: true,
    right: 8,
    top: 4,
    itemSize: 11,
    feature: {
      saveAsImage: { title: 'Save', pixelRatio: 2 }
    },
    iconStyle: { borderColor: '#bbb' }
  };

  // Toolbox WITH the brush feature — only used when the chart actually
  // wires a `brush` component + brushSelected handler (brushable).
  const mkToolbox = (withBrush) => withBrush
    ? {
        ...TOOLBOX,
        feature: { ...TOOLBOX.feature,
          brush: { type: ['rect', 'lineX', 'clear'] } }
      }
    : TOOLBOX;

  const AGG_FNS = [
    { value: 'count', label: 'Count' },
    { value: 'count_distinct', label: 'Count distinct' },
    { value: 'mean', label: 'Mean' },
    { value: 'median', label: 'Median' },
    { value: 'sum', label: 'Sum' },
    { value: 'min', label: 'Min' },
    { value: 'max', label: 'Max' }
  ];

  // ===== Role spec ==========================================================
  // The config popover is a pure function of these two structures plus the
  // current (columns, config). See blockr.design/open/block-config-ui.
  //
  // ROLES — keyed by the existing config key (no persisted-key renames).
  //   kind: 'column' | 'select' | 'segmented' | 'slider'
  //   colType: 'cat' (categorical or n_unique<=50) | 'num' | 'any'
  //   allowCount: prepend '.count' to a numeric column picker (metric)
  //   maxUnique: cap a categorical picker (facet)
  //   pairedWith: render this role's control beside its pair in one row
  //   optionsBy / colTypeBy / hintBy: per-family overrides (key = family)
  // Inlined here for v1; extract to a shared module when the table/ggplot
  // blocks adopt it (follow-up specs).
  const ROLES = {
    group:  { label: 'Group',  kind: 'column', colType: 'cat', ph: 'category column…' },
    x:      { label: 'X',      kind: 'column', colType: 'any',
              hintBy: { individual: 'numeric column…', timeline: 'time / sequence…' } },
    y:      { label: 'Y',      kind: 'column', colTypeBy: { individual: 'num', timeline: 'any' } },
    xend:   { label: 'X end',  kind: 'column', colType: 'any', ph: 'interval end…' },
    series: { label: 'Series', kind: 'column', colType: 'any' },
    color:  { label: 'Color',  kind: 'column', colType: 'any' },
    facet:  { label: 'Facet',  kind: 'column', colType: 'cat', maxUnique: 10 },
    drill:  { label: 'Drill',  kind: 'column', colType: 'any' },
    label:  { label: 'Label',  kind: 'column', colType: 'any' },
    metric: { label: 'Metric', kind: 'column', colType: 'num', allowCount: true, pairedWith: 'agg_fn' },
    agg_fn: { label: 'Agg',    kind: 'select', options: AGG_FNS },
    sort_by:  { label: 'Sort', kind: 'select', pairedWith: 'sort_dir',
                optionsBy: { aggregated: ['value', 'alpha', '#num'],
                             timeline: ['onset', 'alpha', '#num'] } },
    sort_dir: { label: 'Dir',  kind: 'select', options: ['asc', 'desc'] },
    orientation: { label: 'Orientation', kind: 'segmented',
                   options: [{ value: 'horizontal', label: 'Horizontal' },
                             { value: 'vertical', label: 'Vertical' }] },
    smoother: { label: 'Smoother', kind: 'select', options: ['none', 'lm', 'loess'] },
    lo:       { label: 'Lo', kind: 'column', colType: 'num' },
    hi:       { label: 'Hi', kind: 'column', colType: 'num' },
    line_width_mult: { label: 'Line width', kind: 'slider' },
    dot_size_mult:   { label: 'Dot size',   kind: 'slider' }
  };

  // FAMILY_ROLES — per family, ordered. A section entry is either a role key
  // (always shown for the family) or { role, types:[...] } (shown only for
  // those chart types). requiredMap rows render immediately; optionalMap rows
  // are added on demand from the "+ Add mapping" menu. `metric` is required
  // for aggregated but lives in the Encoding section (carries its own marker).
  const FAMILY_ROLES = {
    aggregated: {
      requiredMap: ['group'],
      optionalMap: ['color', 'facet', 'label'],
      encoding: ['metric', { role: 'agg_fn', types: ['bar', 'pie', 'treemap'] }],
      // orientation: bar only for v1 (boxplot swap is a follow-up)
      presentation: ['sort_by', 'sort_dir', { role: 'orientation', types: ['bar'] }]
    },
    individual: {
      requiredMap: ['x', 'y'],
      optionalMap: ['series', 'color', 'facet', 'label'],
      encoding: [],
      presentation: [
        { role: 'smoother', types: ['scatter'] },
        { role: 'lo', types: ['line'] },
        { role: 'hi', types: ['line'] },
        'line_width_mult', 'dot_size_mult'
      ]
    },
    timeline: {
      requiredMap: ['x', 'xend', 'y'],
      optionalMap: ['series', 'color', 'facet', 'label'],
      encoding: [],
      presentation: ['sort_by', 'sort_dir']
    }
  };

  class DrilldownChart {
    constructor(el) {
      this.el = el;
      this.data = [];
      this.columns = [];
      this.config = {};
      this.argHelp = {};
      this.charts = [];
      this._selected = null;
      this._selects = {};
      this._added = new Set();      // optional roles the user added this session
      this._roleMemory = {};        // role key -> last chosen column (sticky)
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
      // The popover is portaled to <body> (see below); a re-render of the
      // widget would otherwise orphan the old one. Remove it first.
      if (this.popoverEl && this.popoverEl.parentNode) {
        this.popoverEl.parentNode.removeChild(this.popoverEl);
      }
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
      this.gearBtn.title = 'Chart settings';
      this.gearBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        this._togglePopover();
      });
      gearHeader.appendChild(this.gearBtn);
      this.card.appendChild(gearHeader);

      // All configuration (mapping + presentation) lives behind the gear.
      // The card itself shows only the result + its direct interactions.
      // See blockr.docs design-system/components/blockr-popover.md.

      // Popover. Portaled to <body> so it escapes Dockview's transformed
      // panels (a transformed ancestor traps position:fixed, and the
      // panel's overflow:auto clips an absolute popover). Same pattern as
      // blockr-select's dropdown. The `dd-popover` class carries all the
      // styling that used to be scoped under .drilldown-chart-container.
      this.popoverEl = document.createElement('div');
      this.popoverEl.className = 'blockr-popover dd-popover';
      this.popoverEl.style.display = 'none';
      document.body.appendChild(this.popoverEl);

      // Close popover on outside click. The handler is stored so dispose()
      // (and a re-_buildDOM()) can remove it — otherwise each widget
      // instance leaks one document-level listener that closes over a stale
      // popoverEl. Remove any previously registered handler first so only
      // one is ever active per instance.
      if (this._outsideClick) {
        document.removeEventListener('click', this._outsideClick);
      }
      this._outsideClick = (e) => {
        if (this._popoverOpen && this.popoverEl &&
            !this.popoverEl.contains(e.target) &&
            !this.gearBtn.contains(e.target)) {
          this._closePopover();
        }
      };
      document.addEventListener('click', this._outsideClick);

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

    // One settings row: label on top, full-width control, muted help text
    // below (sourced from the block's _arguments() metadata). See
    // blockr.docs design-system/components/blockr-popover.md.
    _addSelect(container, label, key, options, selected, cssClass) {
      const row = document.createElement('div');
      row.className = 'blockr-popover-row dd-form-row' +
        (cssClass ? ' ' + cssClass : '');

      const lbl = document.createElement('span');
      lbl.className = 'blockr-popover-label';
      lbl.textContent = label;
      row.appendChild(lbl);

      // The bordered control box (no inline label now — label is on top).
      const wrapper = document.createElement('div');
      wrapper.className = 'dd-picker-wrap';

      // Muted help below the control. For a column picker this shows the
      // selected variable in the usual `name (label)` convention so it is
      // clear which column is mapped; otherwise the static role text.
      const helpEl = document.createElement('span');
      helpEl.className = 'dd-form-help';
      const setHelp = (v) => {
        const txt = this._fieldHelp(key, v);
        helpEl.textContent = txt;
        helpEl.style.display = txt ? '' : 'none';
      };
      setHelp(selected);

      const useBlockrSelect = typeof Blockr !== 'undefined' && Blockr.Select;

      const onSelect = (val) => {
        this.config[key] = (val === '(none)') ? '' : val;
        this._selected = null;
        setHelp(val);
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

      row.appendChild(wrapper);
      row.appendChild(helpEl);

      container.appendChild(row);
    }

    // Help text under a field. For a column picker: the selected
    // variable's name and label in the usual `name (label)` convention,
    // so it is clear which column is mapped. Falls back to the static
    // _arguments() role description when the value is not a column
    // (e.g. "(none)", ".count", an aggregation function) or the column
    // carries no label.
    // Help under a column row: the selected column's `name (label)` when a
    // column is chosen, else the role's placeholder/hint. No longer sourced
    // from the registry _arguments() prose — UI help is a UI-layer concern
    // (see blockr.design/open/block-config-ui).
    _fieldHelp(key, value) {
      const col = (this.columns || []).find(c => c.name === value);
      if (col && col.label && col.label !== col.name) {
        return col.name + ' (' + col.label + ')';
      }
      if (this._hasVal(value)) return '';
      const role = ROLES[key];
      if (!role) return '';
      return (role.hintBy && role.hintBy[this._family()]) || role.ph || '';
    }

    // Axis title for a mapped column: its variable label when present,
    // else the column name. The popover help shows `name (label)`; an
    // axis is tighter, so just the human label (or the name).
    _axisTitle(col) {
      if (!col) return '';
      const c = (this.columns || []).find(x => x.name === col);
      if (c && c.label && c.label !== c.name) return c.label;
      return col;
    }

    // Roles whose control renders inside its primary's row (the pair tail),
    // so they are skipped when a section iterates its entries.
    static get _SECONDARY() {
      return new Set(Object.values(ROLES).map(r => r.pairedWith).filter(Boolean));
    }

    _hasVal(v) { return v !== null && v !== undefined && v !== '' && v !== '(none)'; }
    _colExists(name) { return (this.columns || []).some(c => c.name === name); }

    // Does column `name` satisfy role `key`'s column-type filter for the
    // active family? Used by identity-carry and sticky-memory restore.
    _colFits(key, name) {
      const role = ROLES[key];
      const c = (this.columns || []).find(x => x.name === name);
      if (!c) return false;
      const ct = role.colTypeBy ? role.colTypeBy[this._family()] : role.colType;
      if (ct === 'num') return c.type === 'numeric';
      if (ct === 'cat') return c.type === 'categorical' || c.n_unique <= 50;
      return true;
    }

    _rememberRole(key, val) {
      if (ROLES[key] && ROLES[key].kind === 'column' && this._hasVal(val)) {
        this._roleMemory[key] = val;
      }
    }

    // Column-picker options for a role: type-filtered, label-decorated,
    // with '.count' / '(none)' sentinels as the role allows.
    _colOptionsFor(key, { required }) {
      const role = ROLES[key];
      const ct = role.colTypeBy ? role.colTypeBy[this._family()] : role.colType;
      let cols = this.columns || [];
      if (ct === 'num') cols = cols.filter(c => c.type === 'numeric');
      else if (ct === 'cat') cols = cols.filter(c => c.type === 'categorical' || c.n_unique <= 50);
      if (role.maxUnique) cols = cols.filter(c => c.n_unique <= role.maxUnique);
      const opts = cols.map(c => c.label ? { value: c.name, label: c.label } : c.name);
      if (role.allowCount) opts.unshift('.count');
      else if (!required) opts.unshift('(none)');
      return opts;
    }

    // Resolve a select role's options for the active family; '#num' expands
    // to the numeric column names.
    _selectOptionsFor(key) {
      const role = ROLES[key];
      const raw = role.optionsBy ? (role.optionsBy[this._family()] || []) : (role.options || []);
      const out = [];
      for (const o of raw) {
        if (o === '#num') {
          for (const c of this.columns.filter(c => c.type === 'numeric')) {
            out.push(c.label ? { value: c.name, label: c.label } : c.name);
          }
        } else { out.push(o); }
      }
      return out;
    }

    // Is `key`'s section entry applicable to the current chart type? Used to
    // decide whether a paired tail (agg_fn for boxplot) should render.
    _entryApplicable(key) {
      const fam = FAMILY_ROLES[this._family()];
      const all = [...fam.encoding, ...fam.presentation];
      const e = all.find(x => (typeof x === 'string' ? x : x.role) === key);
      if (!e) return false;
      return typeof e === 'string' || !e.types || e.types.includes(this.config.chart_type);
    }

    _renderConfig() {
      const cfg = this.config;
      const fam = this._family();
      const spec = FAMILY_ROLES[fam];

      for (const s of Object.values(this._selects)) {
        if (s && typeof s.destroy === 'function') s.destroy();
      }
      this._selects = {};
      if (!this._added) this._added = new Set();
      if (!this._roleMemory) this._roleMemory = {};

      this.popoverEl.innerHTML = '';

      const title = document.createElement('div');
      title.className = 'blockr-popover-label dd-popover-title';
      title.textContent = 'Chart settings';
      this.popoverEl.appendChild(title);

      // Chart-type picker (gates everything below)
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
          btn.addEventListener('click', () => this._onChartType(t));
          btns.appendChild(btn);
        }
        group.appendChild(btns);
        return group;
      };
      typesRow.appendChild(buildTypeGroup('Aggregated', AGGREGATED_TYPES));
      typesRow.appendChild(buildTypeGroup('Individual', INDIVIDUAL_TYPES));
      typesRow.appendChild(buildTypeGroup('Timeline', TIMELINE_TYPES));
      this.popoverEl.appendChild(typesRow);

      // ----- Mapping: required rows, shown-optional rows, add menu -----
      const mapSec = this._sectionEl('Mapping');
      for (const key of spec.requiredMap) {
        this._renderRole(mapSec, key, { required: true });
      }
      const shownOpt = spec.optionalMap.filter(
        k => this._hasVal(cfg[k]) || this._added.has(k));
      for (const key of shownOpt) this._renderRole(mapSec, key, { removable: true });
      const remaining = spec.optionalMap.filter(k => !shownOpt.includes(k));
      if (remaining.length) this._addMappingMenu(mapSec, remaining);

      // ----- Encoding / Presentation: always-shown applicable rows -----
      this._renderSection('Encoding', spec.encoding);
      this._renderSection('Presentation', spec.presentation);

      // ----- Drill-down: capability toggle + optional target override -----
      this._renderDrillSection();

      this._updateFamilyClass();
    }

    _sectionEl(titleText) {
      const sec = document.createElement('div');
      sec.className = 'dd-section';
      const h = document.createElement('div');
      h.className = 'dd-section-title';
      h.textContent = titleText;
      sec.appendChild(h);
      this.popoverEl.appendChild(sec);
      return sec;
    }

    _renderSection(titleText, entries) {
      const ct = this.config.chart_type;
      const list = entries
        .map(e => (typeof e === 'string' ? { role: e } : e))
        .filter(e => !e.types || e.types.includes(ct))
        .filter(e => !DrilldownChart._SECONDARY.has(e.role));
      if (!list.length) return;
      const sec = this._sectionEl(titleText);
      for (const e of list) {
        const required = e.role === 'metric' && this._family() === 'aggregated';
        this._renderRole(sec, e.role, { required });
      }
    }

    // One labelled row for a role; dispatches the control(s) by kind and
    // draws a paired secondary (agg beside metric, dir beside sort) inline.
    _renderRole(container, key, opts = {}) {
      const role = ROLES[key];
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
          !!opts.required && !this._hasVal(this.config[key]));
      };
      const setHelp = () => {
        if (role.kind !== 'column') { helpEl.style.display = 'none'; return; }
        const txt = this._fieldHelp(key, this.config[key]);
        helpEl.textContent = txt;
        helpEl.style.display = txt ? '' : 'none';
      };

      this._buildControl(controls, key, { required: opts.required, onChange: () => { setHelp(); markRequired(); } });
      if (paired) {
        this._buildControl(controls, role.pairedWith, { onChange: () => {} });
      }
      row.appendChild(controls);
      row.appendChild(helpEl);
      setHelp();
      markRequired();
      container.appendChild(row);
    }

    // Drill-down is a capability, not an aesthetic: its own section with an
    // on/off toggle (off by default). On -> a "Filter on" picker whose first
    // option is the family's natural target ("auto"); pick a column to
    // override. Tri-state `drill`: ''=off, 'auto'=on-natural, column=override.
    _renderDrillSection() {
      const cfg = this.config;
      const on = this._drillState() !== 'off';
      const sec = this._sectionEl('Drill-down');

      // Toggle row
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
          const wasOpen = this._popoverOpen;
          this._renderConfig();
          if (wasOpen) setTimeout(() => this._openPopover(), 0);
          this._render(); this._sendConfig(); this._sendClearFilter();
        });
        seg.appendChild(b);
      }
      tRow.appendChild(seg);
      sec.appendChild(tRow);

      // When on: "Filter on" picker — Auto (natural target) + columns
      if (on) {
        const fam = this._family();
        const autoLabel = fam === 'aggregated' ? 'Auto — the clicked group'
          : fam === 'timeline' ? 'Auto — the clicked lane'
          : cfg.chart_type === 'line' ? 'Auto — the clicked series'
          : 'Auto — the selected point';
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
        const opts = [{ value: 'auto', label: autoLabel }, ...(this.columns || []).map(colOpt)];
        const sel = (this._hasVal(cfg.drill) && cfg.drill !== 'auto') ? cfg.drill : 'auto';
        const onSel = (val) => {
          cfg.drill = val; this._render(); this._sendConfig(); this._sendClearFilter();
        };
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

    // Build a single control (no label/row chrome — that's _renderRole's job).
    _buildControl(parent, key, { required, onChange } = {}) {
      const role = ROLES[key];
      const cb = onChange || (() => {});
      if (role.kind === 'column') {
        const opts = this._colOptionsFor(key, { required });
        const wrap = document.createElement('div');
        wrap.className = 'dd-picker-wrap';
        const sel = (this.config[key] && this.config[key] !== '(none)') ? this.config[key] : (required ? '' : '(none)');
        const onSel = (val) => {
          this.config[key] = (val === '(none)') ? '' : val;
          this._rememberRole(key, this.config[key]);
          this._selected = null;
          cb();
          this._render();
          this._sendConfig();
          this._sendClearFilter();
        };
        if (typeof Blockr !== 'undefined' && Blockr.Select) {
          this._selects[key] = Blockr.Select.single(wrap, { options: opts, selected: sel, onChange: onSel });
        } else {
          const s = document.createElement('select');
          s.className = 'dd-cfg-select';
          for (const o of opts) {
            const val = (typeof o === 'object' && o) ? o.value : o;
            const txt = (typeof o === 'object' && o && o.label) ? `${o.value} (${o.label})` : val;
            const op = document.createElement('option');
            op.value = val; op.textContent = txt;
            if (val === sel) op.selected = true;
            s.appendChild(op);
          }
          s.addEventListener('change', () => onSel(s.value));
          wrap.appendChild(s);
        }
        parent.appendChild(wrap);
      } else if (role.kind === 'select') {
        const opts = this._selectOptionsFor(key);
        const wrap = document.createElement('div');
        wrap.className = 'dd-picker-wrap';
        const cur = this.config[key];
        const sel = this._hasVal(cur) ? cur : ((typeof opts[0] === 'object' && opts[0]) ? opts[0].value : opts[0]);
        const onSel = (val) => { this.config[key] = val; cb(); this._render(); this._sendConfig(); };
        if (typeof Blockr !== 'undefined' && Blockr.Select) {
          this._selects[key] = Blockr.Select.single(wrap, { options: opts, selected: sel, onChange: onSel });
        } else {
          const s = document.createElement('select');
          s.className = 'dd-cfg-select';
          for (const o of opts) {
            const val = (typeof o === 'object' && o) ? o.value : o;
            const txt = (typeof o === 'object' && o && o.label) ? `${o.value} (${o.label})` : val;
            const op = document.createElement('option');
            op.value = val; op.textContent = txt;
            if (val === sel) op.selected = true;
            s.appendChild(op);
          }
          s.addEventListener('change', () => onSel(s.value));
          wrap.appendChild(s);
        }
        parent.appendChild(wrap);
      } else if (role.kind === 'segmented') {
        const seg = document.createElement('div');
        seg.className = 'dd-segmented';
        const cur = this._hasVal(this.config[key]) ? this.config[key] : role.options[0].value;
        for (const o of role.options) {
          const b = document.createElement('button');
          b.type = 'button';
          b.className = 'dd-seg-btn' + (o.value === cur ? ' dd-seg-active' : '');
          b.textContent = o.label;
          b.addEventListener('click', () => {
            this.config[key] = o.value;
            seg.querySelectorAll('.dd-seg-btn').forEach(x => x.classList.remove('dd-seg-active'));
            b.classList.add('dd-seg-active');
            cb(); this._render(); this._sendConfig();
          });
          seg.appendChild(b);
        }
        parent.appendChild(seg);
      } else if (role.kind === 'slider') {
        this._buildSlider(parent, key);
      }
    }

    _buildSlider(parent, key) {
      const init = this.config[key];
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
        this.config[key] = v;
        this._render();
        clearTimeout(debounce);
        debounce = setTimeout(() => this._sendMults(), 150);
      });
      parent.appendChild(wrap);
    }

    // "+ Add mapping" — a trigger that reveals the optional roles not yet
    // shown; picking one adds its row (restoring a remembered column if any).
    _addMappingMenu(container, remaining) {
      const wrap = document.createElement('div');
      wrap.className = 'dd-add-wrap';
      // Match blockr.dplyr's add affordance: a subtle grey text link with a
      // plus icon (blockr-add-row / blockr-add-link / blockr-add-icon, from
      // the shared blockr-blocks.css + Blockr.icons this block already loads).
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
        item.textContent = ROLES[key].label;
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
      this._added.add(key);
      if (!this._hasVal(this.config[key]) && this._roleMemory[key] &&
          this._colExists(this._roleMemory[key]) && this._colFits(key, this._roleMemory[key])) {
        this.config[key] = this._roleMemory[key];
        this._render();
        this._sendConfig();
      }
      const wasOpen = this._popoverOpen;
      this._renderConfig();
      if (wasOpen) setTimeout(() => this._openPopover(), 0);
    }

    // Explicit removal forgets the role's remembered column (vs a family
    // switch, which keeps it for switch-back).
    _removeRole(key) {
      this.config[key] = '';
      this._added.delete(key);
      delete this._roleMemory[key];
      const wasOpen = this._popoverOpen;
      this._renderConfig();
      if (wasOpen) setTimeout(() => this._openPopover(), 0);
      this._render();
      this._sendConfig();
      this._sendClearFilter();
    }

    _onChartType(t) {
      const oldFam = this._family();
      this.config.chart_type = t;
      const newFam = this._family();
      if (oldFam !== newFam) {
        this._carryRoles(newFam);
        this._selected = null;
        this._sendClearFilter();
      }
      this._ensureFamilyDefaults();
      this._updateFamilyClass();
      const wasOpen = this._popoverOpen;
      this._renderConfig();
      if (wasOpen) setTimeout(() => this._openPopover(), 0);
      this._render();
      this._sendConfig();
    }

    // Identity-carry + sticky memory across a family switch. A column role
    // keeps its value only if the role exists in the new family and the
    // column still fits its type; otherwise the column is remembered (for
    // switch-back) and cleared. Then required roles are restored from memory
    // when a fitting column is remembered.
    _carryRoles(newFam) {
      const spec = FAMILY_ROLES[newFam];
      const keep = new Set([...spec.requiredMap, ...spec.optionalMap]);
      for (const key of Object.keys(ROLES)) {
        if (ROLES[key].kind !== 'column') continue;
        if (key === 'drill') continue;  // capability, not a mapping — persists
        // `metric` is a column-kind role but is NOT in the block's
        // allow_empty_state: clearing it to '' violates that constraint and
        // WEDGES the block's reactive evaluation — downstream filtering then
        // freezes after a family switch. It is a required-for-init slot, not a
        // positional mapping; never clear it here.
        if (key === 'metric') continue;
        const v = this.config[key];
        if (!this._hasVal(v)) continue;
        if (!(keep.has(key) && this._colFits(key, v))) {
          this._roleMemory[key] = v;
          this.config[key] = '';
        }
      }
      for (const key of spec.requiredMap) {
        const mem = this._roleMemory[key];
        if (!this._hasVal(this.config[key]) && mem && this._colExists(mem) && this._colFits(key, mem)) {
          this.config[key] = mem;
        }
      }
    }

    // Establish sensible defaults for the active family. Crucially this also
    // picks the default MAPPING columns (group / x / y) when unset — and it
    // runs in the family-switch path (_onChartType) BEFORE _sendConfig, so R
    // learns the new mapping and ships those columns. Without this, switching
    // family clears the positional roles, R ships no columns, and the chart
    // renders empty (the data-pump only sends columns the current mapping
    // names). Mirrors the column-picking in setData.
    _ensureFamilyDefaults() {
      const cfg = this.config;
      const fam = this._family();
      const cols = this.columns || [];
      if (fam === 'aggregated') {
        if (!this._hasVal(cfg.group) && cols.length) {
          const cat = cols.find(c => c.type === 'categorical' && c.n_unique <= 30);
          cfg.group = cat ? cat.name : cols[0].name;
        }
        if (!cfg.metric) cfg.metric = '.count';
        if (!cfg.agg_fn) cfg.agg_fn = 'count';
        if (!this._hasVal(cfg.sort_by)) cfg.sort_by = 'value';
        if (!this._hasVal(cfg.sort_dir)) cfg.sort_dir = 'desc';
        if (!this._hasVal(cfg.orientation)) cfg.orientation = 'horizontal';
      } else if (fam === 'timeline') {
        if (!this._hasVal(cfg.x) && cols.length) {
          const num = cols.find(c => c.type === 'numeric');
          cfg.x = num ? num.name : cols[0].name;
        }
        if (!this._hasVal(cfg.y) && cols.length) {
          const cat = cols.find(c => c.type === 'categorical' && c.n_unique > 1);
          cfg.y = cat ? cat.name : cols[0].name;
        }
        if (!this._hasVal(cfg.sort_by)) cfg.sort_by = 'onset';
        if (!this._hasVal(cfg.sort_dir)) cfg.sort_dir = 'asc';
      } else {
        if (!this._hasVal(cfg.x) && cols.length) {
          const num = cols.find(c => c.type === 'numeric');
          cfg.x = num ? num.name : cols[0].name;
        }
        if (!this._hasVal(cfg.y) && cols.length) {
          const nums = cols.filter(c => c.type === 'numeric');
          const other = nums.find(c => c.name !== cfg.x);
          cfg.y = other ? other.name : (nums[0] ? nums[0].name : cols[0].name);
        }
      }
    }

    _updateFamilyClass() {
      const family = this._family();
      const fams = ['dd-family-aggregated', 'dd-family-individual',
        'dd-family-timeline'];
      this.el.classList.remove(...fams);
      this.el.classList.add('dd-family-' + family);
      if (this.popoverEl) {
        this.popoverEl.classList.remove(...fams);
        this.popoverEl.classList.add('dd-family-' + family);
      }
    }

    // -- Data + rendering entry point -----------------------------------------

    setData(columns, data, config, args) {
      this.columns = columns || [];
      this.config = config || {};
      if (args) this.argHelp = args;

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
        if (!this.config.group && this.columns.length > 0) {
          const cat = this.columns.find(c => c.type === 'categorical' && c.n_unique <= 30);
          this.config.group = cat ? cat.name : this.columns[0].name;
        }
        if (!this.config.metric) this.config.metric = '.count';
        if (!this.config.agg_fn) this.config.agg_fn = 'count';
        if (!this.config.sort_by) this.config.sort_by = 'value';
        if (!this.config.sort_dir) this.config.sort_dir = 'desc';
      } else if (fam === 'timeline') {
        if (!this.config.x && this.columns.length > 0) {
          const num = this.columns.find(c => c.type === 'numeric');
          this.config.x = num ? num.name : this.columns[0].name;
        }
        if (!this.config.y) {
          const cat = this.columns.find(c => c.type === 'categorical' && c.n_unique > 1);
          this.config.y = cat ? cat.name : this.columns[0]?.name;
        }
        if (!this.config.sort_by) this.config.sort_by = 'onset';
        if (!this.config.sort_dir) this.config.sort_dir = 'asc';
      } else {
        if (!this.config.x && this.columns.length > 0) {
          const num = this.columns.find(c => c.type === 'numeric');
          this.config.x = num ? num.name : this.columns[0].name;
        }
        if (!this.config.y) {
          const nums = this.columns.filter(c => c.type === 'numeric');
          const other = nums.find(c => c.name !== this.config.x);
          this.config.y = other ? other.name : (nums[0] ? nums[0].name : this.columns[0]?.name);
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
      const { group, color, facet, metric, agg_fn } = this.config;
      if (this.data.length === 0) return [];

      const groups = {};
      for (const row of this.data) {
        const gv = group ? String(row[group] ?? '') : 'Total';
        const cv = color ? String(row[color] ?? '') : '__all__';
        const fv = facet ? String(row[facet] ?? '') : '__all__';
        const key = fv + '|||' + gv + '|||' + cv;
        if (!groups[key]) groups[key] = { facet: fv, group: gv, color: cv, values: [], rows: [] };
        groups[key].rows.push(row);
        // Collect numeric metric values for mean/median/sum/min/max. Skip
        // entries that coerce to NaN (non-numeric text, empty strings) so a
        // single bad cell can't poison a mean/sum into NaN.
        if (metric !== '.count' && row[metric] != null) {
          const n = Number(row[metric]);
          if (!Number.isNaN(n)) groups[key].values.push(n);
        }
      }

      const result = [];
      for (const g of Object.values(groups)) {
        let value;
        if (agg_fn === 'count') value = g.rows.length;
        else if (agg_fn === 'count_distinct') { const s = new Set(); for (const r of g.rows) { const v = r[metric]; if (v != null && !(typeof v === 'number' && Number.isNaN(v))) s.add(v); } value = s.size; }
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

      // Required-role gate: a required mapping with no value can't draw — show
      // an inline prompt instead of a blank canvas (retires the silent empty
      // plot). xend is "required" only as an always-shown row (a gantt with no
      // end renders dots at x), so it never gates rendering.
      const gate = FAMILY_ROLES[fam].requiredMap.filter(k => k !== 'xend');
      const unset = gate.filter(k => !this._hasVal(this.config[k]));
      if (unset.length) {
        this.chartGrid.innerHTML =
          '<div class="vd-empty-state"><p class="vd-empty-text">Pick ' +
          unset.map(k => ROLES[k].label).join(' and ') + ' to plot.</p></div>';
        return;
      }

      // Guard: color with too many distinct levels renders an unreadable
      // legend. color means "map column values to colors" — a small
      // palette has ~7 readable colors, 15 is a hard ceiling. For splitting
      // data into many series (one per patient), use `series` instead.
      const MAX_COLOR_LEVELS = 15;
      if (this.config.color) {
        const nColors = new Set(this.data.map(r => r[this.config.color])).size;
        if (nColors > MAX_COLOR_LEVELS) {
          // Aggregated: no series escape hatch, hard stop.
          // Individual/timeline: nudge the user toward series (or a
          // lower-cardinality grouping column like arm).
          const hint = fam === 'aggregated'
            ? `Pick a column with \u2264${MAX_COLOR_LEVELS} categories.`
            : `Use <code>series</code> to split into series (e.g. USUBJID); keep <code>color</code> for low-cardinality grouping (e.g. ARM).`;
          this.chartGrid.innerHTML = `<div class="vd-empty-state"><p class="vd-empty-text">Too many color levels (${nColors}). ${hint}</p></div>`;
          return;
        }
      }

      // Guard (restored — present pre-refactor): a mapped column not in
      // the data means nothing to draw; bail cheaply instead of running
      // the full (expensive) render.
      const cfg = this.config;
      const colSet = new Set((this.columns || []).map(c => c.name));
      const req = fam === 'aggregated'
        ? [['Group', cfg.group],
           ['Metric', cfg.metric !== '.count' ? cfg.metric : null]]
        : [['X', cfg.x], ['Y', cfg.y]];
      const missing = req
        .filter(([, v]) => v && !colSet.has(v))
        .map(([lbl, v]) => `${lbl} = "${v}"`);
      if (missing.length) {
        this.chartGrid.innerHTML =
          '<div class="vd-empty-state"><p class="vd-empty-text">' +
          'Mapped column not in data: ' + missing.join(', ') +
          '. Check the block feeding this chart (a rename, flatten or ' +
          'pivot upstream may have changed the column name).</p></div>';
        return;
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
        const groupCol = this.config.group;
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
        chart.setOption(this._applyDrillEmphasis(option), true);

        chart.on('click', (params) => {
          if (this._drillState() === 'off') return;
          const clickedGroup = params.name || (params.value && params.value[0]);
          if (!clickedGroup) return;
          this._selected = this._selected === clickedGroup ? null : clickedGroup;
          this._updateHighlight();
          if (this._selected == null) { this._sendClearFilter(); return; }
          // The clicked group's source rows -> _emitDrill. With drill 'auto'
          // the target is the group column; with an override, that column.
          const g = this.config.group, fc = this.config.facet;
          const rows = (this.data || []).filter(r =>
            String(r[g]) === String(clickedGroup) &&
            (facet === '__all__' || !fc || String(r[fc]) === String(facet)));
          this._emitDrill(rows);
        });
      }
      this._updateHighlight();
    }

    _buildAggregatedOption(facetData, groups, colors, palette) {
      const ct = this.config.chart_type;
      const ax = { labelColor: AXIS_LABEL_COLOR, fontSize: 11, splitLineColor: SPLIT_LINE_COLOR };

      // Value-axis title (the numeric axis on bar / boxplot): the metric's
      // variable label, or "Count" for a row count. Same rationale as the
      // line/scatter axis titles — the mapping moved into the gear, so the
      // chart must say what it shows.
      const valueTitle = this.config.metric === '.count'
        ? 'Count' : this._axisTitle(this.config.metric);

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

      // Orientation is a presentation property (the ggplot coord_flip model):
      // horizontal (default) keeps the category on the y-axis \u2014 best for long
      // labels (AE terms, arms); vertical puts it on the x-axis. The mapping
      // is unchanged (Group=category, Metric=value) \u2014 flipping re-maps nothing.
      const vertical = this.config.orientation === 'vertical';
      const catAxis = vertical
        ? { type: 'category', data: groups, axisLabel: { color: ax.labelColor, fontSize: ax.fontSize, rotate: 30, overflow: 'truncate', width: 90, ellipsis: '\u2026' }, axisLine: { lineStyle: { color: AXIS_LINE_COLOR } }, axisTick: { show: false } }
        : { type: 'category', data: groups, inverse: true, axisLabel: { color: ax.labelColor, fontSize: ax.fontSize, align: 'left', margin: 150, width: 145, overflow: 'truncate', ellipsis: '\u2026' }, axisLine: { show: false }, axisTick: { show: false } };
      const valAxis = {
        type: 'value', name: valueTitle, nameLocation: 'middle',
        nameGap: vertical ? 45 : 30,
        nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize },
        axisLabel: { color: ax.labelColor, fontSize: ax.fontSize },
        axisLine: { lineStyle: { color: AXIS_LINE_COLOR } },
        splitLine: { lineStyle: { color: ax.splitLineColor, type: 'dashed' } }
      };
      const legendOn = colors.length > 0;
      return {
        ...(this.theme ? {} : { backgroundColor: 'transparent' }),
        textStyle: { fontFamily: BLOCKR_FONT },
        tooltip: { trigger: 'axis', axisPointer: { type: 'shadow' }, confine: true },
        toolbox: TOOLBOX,
        legend: legendOn ? { show: true, bottom: 0, textStyle: { fontSize: 11 } } : undefined,
        grid: vertical
          ? { left: 55, right: 10, top: 30, bottom: (legendOn ? 55 : 40) + 26 }
          : { left: 160, right: 5, top: 30, bottom: (legendOn ? 55 : 20) + 26 },
        xAxis: vertical ? catAxis : valAxis,
        yAxis: vertical ? valAxis : catAxis,
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
      const groupBy = this.config.group;
      const metric = this.config.metric;
      if (metric === '.count') return null;
      const boxData = groups.map(g => {
        const vals = this.data.filter(r => String(r[groupBy]) === g && r[metric] != null).map(r => Number(r[metric])).filter(v => !Number.isNaN(v)).sort((a, b) => a - b);
        // Empty / all-NA group: return null so ECharts draws nothing for
        // this category slot (index alignment with `groups` / yAxis.data is
        // preserved). A fake [0,0,0,0,0] summary would render a misleading
        // flat box at zero.
        if (vals.length === 0) return null;
        const q = (p) => { const i = p * (vals.length - 1); const lo = Math.floor(i); return lo === i ? vals[lo] : vals[lo] + (vals[lo + 1] - vals[lo]) * (i - lo); };
        const q1 = q(0.25), q3 = q(0.75), iqr = q3 - q1;
        const lo = Math.max(vals[0], q1 - 1.5 * iqr);
        const hi = Math.min(vals[vals.length - 1], q3 + 1.5 * iqr);
        return [lo, q1, q(0.5), q3, hi];
      });
      return { ...(this.theme ? {} : { backgroundColor: 'transparent' }), textStyle: { fontFamily: BLOCKR_FONT }, toolbox: TOOLBOX, tooltip: { trigger: 'item', confine: true }, grid: { left: 160, right: 5, top: 30, bottom: 46 }, xAxis: { type: 'value', name: this._axisTitle(this.config.metric), nameLocation: 'middle', nameGap: 30, nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize }, axisLabel: { color: ax.labelColor, fontSize: ax.fontSize }, axisLine: { lineStyle: { color: AXIS_LINE_COLOR } } }, yAxis: { type: 'category', data: groups, inverse: true, axisLabel: { color: ax.labelColor, fontSize: ax.fontSize, align: 'left', margin: 150, width: 145, overflow: 'truncate', ellipsis: '\u2026' }, axisLine: { show: false } }, series: [{ type: 'boxplot', data: boxData, itemStyle: { color: palette[0] + '22', borderColor: palette[0] } }] };
    }

    // -- Individual rendering -------------------------------------------------

    _renderIndividual() {
      // `series` (the config column) is aliased to `seriesCol` so it cannot
      // be shadowed by the local ECharts `const series = []` array built
      // below. The collision previously turned `r[series]` into
      // `r[<echarts-array-stringified>]` (always undefined) in the
      // useColorByLegend loop, forcing a full data scan per series level.
      const { x, y, color, facet, series: seriesCol } = this.config;
      if (!x || !y) {
        this.chartGrid.innerHTML = '<div class="vd-empty-state"><p class="vd-empty-text">Select X and Y columns</p></div>';
        return;
      }

      const ct = this.config.chart_type;
      const isLine = ct === 'line';
      const isScatter = ct === 'scatter';
      const palette = BLOCKR_PALETTE;
      const ax = { labelColor: AXIS_LABEL_COLOR, fontSize: 11, splitLineColor: SPLIT_LINE_COLOR };

      const facets = facet
        ? [...new Set(this.data.map(r => String(r[facet] ?? '')))].sort()
        : ['__all__'];

      // series is the primary per-entity splitter. If not set, fall back
      // to the legacy behaviour: one series per distinct color value.
      // This keeps existing configs (e.g. aggregated charts that rely on
      // color stacking) working without change.
      const splitCol = seriesCol || color;
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

      // Color resolver: when series is set separately from color, look
      // up each series' color value to share a palette color across
      // series that belong to the same color group (e.g. all patients on
      // ARM A share one color). When series equals color or only
      // color is set, index by series level.
      const colorForLevel = (level, index) => {
        if (!color) return palette[0];
        if (seriesCol && seriesCol !== color) {
          const rep = this.data.find(r => String(r[seriesCol]) === level);
          const cv = rep ? String(rep[color] ?? '') : '';
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
      const xAxisType = this._axisTypeFor(x);
      const yAxisType = this._axisTypeFor(y);
      const xCats = xAxisType === 'category' ? this._orderedCategories(x) : null;
      const yCats = yAxisType === 'category' ? this._orderedCategories(y) : null;
      // Axis-position index for a categorical x. Line series must be drawn
      // in axis order, not raw row order, or the polyline zig-zags back
      // and forth between visits (e.g. AVISIT ordered by AVISITN).
      const xOrder = xCats
        ? new Map(xCats.map((c, i) => [String(c), i]))
        : null;
      const sortLinePts = (pts) => {
        if (!isLine) return;
        if (xOrder) {
          pts.sort((a, b) =>
            (xOrder.get(String(a[0])) ?? 0) - (xOrder.get(String(b[0])) ?? 0));
        } else if (xAxisType !== 'category') {
          pts.sort((a, b) => a[0] - b[0]);
        }
      };

      const encodeX = (v) => xAxisType === 'category' ? String(v ?? '') : Number(v);
      const encodeY = (v) => yAxisType === 'category' ? String(v ?? '') : Number(v);

      this.chartGrid.classList.toggle('dd-chart-grid-single', singleFacet);

      for (const facet of facets) {
        const rows = facet === '__all__' ? this.data : this.data.filter(r => String(r[facet]) === facet);

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
        const loCol = this.config.lo;
        const hiCol = this.config.hi;
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
        // CI and smoother overlays share their parent line's `name` (not
        // "<name> (CI)" / "<name> (lm)") so ECharts collapses them into a
        // single legend entry per series — one click toggles line +
        // whiskers + fit together. legendHoverLink off so legend hover
        // doesn't try to emphasize these (silent) overlay series.
        const mkErrBarSeries = (name, errPts, color) => ({
          type: 'custom',
          name: name,
          legendHoverLink: false,
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
              name: name || 'fit',
              legendHoverLink: false,
              data: ln,
              silent: true,
              showSymbol: false,
              lineStyle: { color: color, width: 2, type: 'solid', opacity: 0.9 },
              z: 2
            });
          }
          // Error-bar overlay: CI whiskers for line charts AND scatter
          // (a coefficient / forest plot is a scatter with lo/hi whiskers).
          if ((isLine || isScatter) && loCol && hiCol &&
              rawRows && rawRows.length) {
            const errPts = rawRows
              .filter(r => r[x] != null && r[loCol] != null && r[hiCol] != null)
              .map(r => [encodeX(r[x]), Number(r[loCol]), Number(r[hiCol])]);
            if (errPts.length) series.push(mkErrBarSeries(name || 'errbar', errPts, color));
          }
        };

        if (seriesLevels.length === 0) {
          const grpRows = rows.filter(r => r[x] != null && r[y] != null);
          const pts = grpRows.map(r => [encodeX(r[x]), encodeY(r[y])]);
          sortLinePts(pts);
          series.push(mkSeries(undefined, pts, palette[0]));
          pushOverlays(undefined, pts, palette[0], grpRows);
        } else {
          for (let ci = 0; ci < seriesLevels.length; ci++) {
            const cl = seriesLevels[ci];
            const grpRows = rows.filter(r => String(r[splitCol]) === cl && r[x] != null && r[y] != null);
            const pts = grpRows.map(r => [encodeX(r[x]), encodeY(r[y])]);
            sortLinePts(pts);
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
        // series ≠ color, legend reflects color cardinality, not
        // series. If color is unset, no legend at all.
        const legendCardinality = color
          ? new Set(this.data.map(r => String(r[color] ?? ''))).size
          : 0;
        const showLegend = color && legendCardinality > 0 && legendCardinality <= 15;

        // When series ≠ color, build an explicit legend over the
        // color levels (e.g. "F", "M") rather than letting echarts
        // enumerate series names (e.g. every USUBJID). The legend item
        // colors come from this._colorLookup populated inside
        // colorForLevel above. We still map clicks to toggle every series
        // that shares that color value.
        const useColorByLegend = showLegend && color && seriesCol &&
          seriesCol !== color;
        let colorByLegendData = null;
        let seriesByColorByVal = null;
        if (useColorByLegend) {
          const lookup = this._colorLookup || {};
          const cbLevels = [...new Set(this.data.map(
            r => String(r[color] ?? '')
          ))].sort();
          colorByLegendData = cbLevels.map((lvl, i) => ({
            name: lvl,
            itemStyle: { color: lookup[lvl] || palette[i % palette.length] }
          }));
          // Precompute series-name → color-value map used by the
          // legend click handler to toggle all series in a color group.
          seriesByColorByVal = {};
          for (const lvl of cbLevels) seriesByColorByVal[lvl] = [];
          for (const sl of seriesLevels) {
            const rep = this.data.find(r => String(r[seriesCol]) === sl);
            const cv = rep ? String(rep[color] ?? '') : '';
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

        // Axis titles: the X / Y mapping moved into the gear popover, so
        // the chart itself is now the only place the reader can see what
        // the axes stand for. Use the variable label (else the column
        // name); the grid margins below leave room for them.
        const xAxisSpec = {
          type: xAxisType,
          name: this._axisTitle(x),
          nameLocation: 'middle',
          nameGap: 28,
          nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize },
          axisLabel: { color: ax.labelColor, fontSize: ax.fontSize },
          axisLine: { lineStyle: { color: AXIS_LINE_COLOR } },
          splitLine: { lineStyle: { color: ax.splitLineColor, type: 'dashed' } },
          scale: true
        };
        if (xCats) xAxisSpec.data = xCats;

        const yAxisSpec = {
          type: yAxisType,
          name: this._axisTitle(y),
          nameLocation: 'middle',
          nameGap: 46,
          nameRotate: 90,
          nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize },
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
        // `series` is set. With `series` the user's intent is "click
        // a dot to filter to that series" (e.g. one dot per policy → click
        // → filter to that policy); leaving brush active fires a 1-pixel
        // brushSelected on the click point that races the click handler
        // and overwrites the categorical filter with a range filter on
        // (x == click_x & y == click_y), matching at most one row.
        const brushable = xAxisType !== 'category' && !isLine && !seriesCol;


        const option = {
          ...(this.theme ? {} : { backgroundColor: 'transparent' }),
          textStyle: { fontFamily: BLOCKR_FONT },
          tooltip: { trigger: 'item', formatter: (p) => `${x}: ${ddNum(p.value[0])}<br>${y}: ${ddNum(p.value[1])}` + (p.seriesName ? `<br>${color || 'series'}: ${p.seriesName}` : ''), confine: true },
          // Always set explicitly; leaving legend undefined lets echarts
          // auto-render one per series, which eats the plot area when
          // series is high-cardinality (e.g. USUBJID).
          legend: useColorByLegend
            ? { show: true, bottom: 0, textStyle: { fontSize: 11 },
                data: colorByLegendData }
            : showLegend
              ? { show: true, bottom: 0, textStyle: { fontSize: 11 } }
              : { show: false },
          // left / bottom widened so the rotated Y title and the X title
          // (nameGap above) clear the tick labels and the legend.
          grid: { left: 66, right: 5, top: 30, bottom: showLegend ? 78 : 52 },
          xAxis: xAxisSpec,
          yAxis: yAxisSpec,
          toolbox: mkToolbox(brushable),
          // brush.toolbox lists the brush types REGISTERED for use. Must
          // include the types we activate (rect for scatter, lineX for line)
          // and the types referenced by toolbox.feature.brush.type, otherwise
          // clicking an icon (or takeGlobalCursor) is a no-op.
          brush: brushable ? { toolbox: ['rect', 'lineX', 'clear'], xAxisIndex: 0, yAxisIndex: isLine ? undefined : 0, brushStyle: { color: 'rgba(0, 114, 178, 0.1)', borderColor: 'rgba(0, 114, 178, 0.5)', borderWidth: 1 }, throttleDelay: 300 } : undefined,
          series
        };

        chart.setOption(this._applyDrillEmphasis(option), true);

        // When the legend shows color levels (not series), intercept
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

        // Click identifies a mark -> a selection (a click is a one-point
        // selection). With drill 'off' it's inert. With a drill column (auto
        // for line = series, or an override) it emits a categorical filter;
        // for a scatter under 'auto' (no categorical key) it filters the exact
        // observation (point filter on x&y). Brush is the same rule at range.
        chart.on('click', (params) => {
          if (this._drillState() === 'off') return;
          if (params.componentType !== 'series') return;
          // Clicking a point in brush mode also fires brushSelected/brush
          // "cleared" events that would call _sendClearFilter and wipe this
          // click's filter. Suppress those for a short window after a click.
          this._suppressBrushClear = true;
          setTimeout(() => { this._suppressBrushClear = false; }, 150);
          const splitCol = seriesCol || color;
          let rows, pointVal = null;
          if (splitCol && params.seriesName) {
            rows = (this.data || []).filter(
              r => String(r[splitCol]) === String(params.seriesName));
            this._selected = params.seriesName;
          } else {
            const v = params.value;
            if (!Array.isArray(v) || v.length < 2 || !x || !y) return;
            rows = (this.data || []).filter(
              r => String(r[x]) === String(v[0]) &&
                   String(r[y]) === String(v[1]));
            this._selected = `${ddNum(v[0])}, ${ddNum(v[1])}`;
            pointVal = v;
          }
          this._updateStatus();
          if (this._drillColumn()) {
            this._emitDrill(rows);
          } else if (pointVal) {
            // scatter auto: a click is a one-point selection -> a zero-width
            // x/y range (between(x,v,v) & between(y,v,v) = the observation).
            this._hasBrushFilter = true;
            this._sendRangeFilter([pointVal[0], pointVal[0]], [pointVal[1], pointVal[1]]);
          }
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
          if (this._drillState() === 'off') return;
          if (!params.batch || params.batch.length === 0) return;

          // Collect all selected data indices across all areas/series
          const batch = params.batch[0];
          if (!batch?.selected) {
            if (this._suppressBrushClear) return;
            this._hasBrushFilter = false;
            this._updateStatus();
            this._sendClearFilter();
            return;
          }

          // Gather selected (seriesIndex, dataIndex) pairs. A brushSelected
          // dataIndex is RELATIVE TO ITS OWN SERIES, so it must be looked up
          // in that series' data array — not in a flattened concatenation of
          // all series (which mis-maps once there is more than one series).
          const selected = [];
          for (const s of batch.selected) {
            if (s.dataIndex && s.dataIndex.length > 0) {
              for (const di of s.dataIndex) {
                selected.push({ seriesIndex: s.seriesIndex, dataIndex: di });
              }
            }
          }

          if (selected.length === 0) {
            if (this._suppressBrushClear) return;
            this._hasBrushFilter = false;
            this._updateStatus();
            this._sendClearFilter();
            return;
          }

          // Clear brush on other facet charts
          for (const other of this.charts) {
            if (other !== chart) other.dispatchAction({ type: 'brush', areas: [] });
          }

          // Compute x/y range from selected points' actual values. Index
          // into each point's own series array and bounds-check so an
          // out-of-range index can neither throw nor pull the wrong point.
          const opt = chart.getOption();
          const allSeries = opt.series || [];

          // With a drill column (an override, or 'auto' resolving to a
          // categorical key), the brush filters downstream on that column's
          // values for the brushed points — the same rule as click-drill, so a
          // selection filters consistently whether click or brush. Build a
          // (x|y) -> rows index once to map brushed points back to source rows.
          // With no drill column (scatter 'auto'), the brush does a geometric
          // x/y range filter.
          const drillCol = this._drillColumn();
          let rowIndex = null;
          if (drillCol) {
            rowIndex = new Map();
            const xc = this.config.x, yc = this.config.y;
            for (const r of (this.data || [])) {
              const k = String(r[xc]) + '|||' + String(r[yc]);
              if (!rowIndex.has(k)) rowIndex.set(k, []);
              rowIndex.get(k).push(r);
            }
          }

          let xVals = [], yVals = [], brushedRows = [];
          for (const sel of selected) {
            const sData = allSeries[sel.seriesIndex]?.data;
            if (!sData || sel.dataIndex < 0 || sel.dataIndex >= sData.length) continue;
            const pt = sData[sel.dataIndex];
            if (!pt) continue;
            const x = Array.isArray(pt) ? pt[0] : pt.value?.[0];
            const y = Array.isArray(pt) ? pt[1] : pt.value?.[1];
            if (x != null) xVals.push(x);
            if (y != null) yVals.push(y);
            if (rowIndex) {
              const hit = rowIndex.get(String(x) + '|||' + String(y));
              if (hit) brushedRows.push(...hit);
            }
          }

          this._hasBrushFilter = true;
          this._updateStatus();

          if (drillCol && brushedRows.length) {
            this._emitDrill(brushedRows);
          } else if (xVals.length > 0) {
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
            if (this._suppressBrushClear) return;
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
      const { x, xend, y, color, facet, sort_by, series, label } = this.config;
      // Effective drill column (tri-state): null when off, the lane (y) for
      // 'auto', or an override column. Packed at tuple slot 7 for the click.
      const drill = this._drillColumn();
      const sortDir = this.config.sort_dir === 'desc' ? -1 : 1;
      if (!x || !y) {
        this.chartGrid.innerHTML = '<div class="vd-empty-state"><p class="vd-empty-text">Select X (start) and Y (term) columns</p></div>';
        return;
      }

      const ax = { labelColor: AXIS_LABEL_COLOR, fontSize: 11, splitLineColor: SPLIT_LINE_COLOR };
      const palette = BLOCKR_PALETTE;
      const xAxisType = this._axisTypeFor(x);
      const xCats = xAxisType === 'category' ? this._orderedCategories(x) : null;

      const facets = facet
        ? [...new Set(this.data.map(r => String(r[facet] ?? '')))].sort()
        : ['__all__'];
      const singleFacet = facets.length === 1;
      this.chartGrid.classList.toggle('dd-chart-grid-single', singleFacet);

      // Distinct color levels (sorted). With one named series per level
      // below, echarts assigns a color to each series from option.color
      // (BLOCKR_PALETTE) — no manual lookup needed.
      const colorLevels = color
        ? [...new Set(this.data.map(r => String(r[color] ?? '')))].sort()
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
          return [...new Set(rows.map(r => String(r[y] ?? '')))]
            .sort((a, b) => a.localeCompare(b) * sortDir);
        }
        const sortCol = (sb === 'onset') ? x : sb;
        const mins = {};
        for (const r of rows) {
          const k = String(r[y] ?? '');
          let v = r[sortCol];
          if (xAxisType === 'category' && sortCol === x) {
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
          : this.data.filter(r => String(r[facet]) === facet);
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
        const heightExtra = (color && colorLevels.length > 0) ? 100 : 80;
        chartDiv.style.height = Math.max(200, terms.length * 28 + heightExtra) + 'px';
        container.appendChild(chartDiv);

        const chart = echarts.init(chartDiv, this.theme || undefined);
        this.charts.push(chart);

        const barData = [];
        for (const r of rows) {
          if (r[x] == null) continue;
          const term = String(r[y] ?? '');
          const lane = terms.indexOf(term);
          if (lane < 0) continue;
          const s = xCoord(r[x], xCats);
          let e;
          if (xend && r[xend] != null && !Number.isNaN(Number(r[xend]))) {
            e = xCoord(r[xend], xCats);
          } else {
            // Single-day event — render a narrow dot.
            e = s;
          }
          barData.push({
            // Slot 5: `label` column value (on-bar text). Slot 7: the
            // `drill` column value for this row (a click filters on it).
            // Scalars only — never pack the whole row object: the AE
            // swim-lane has thousands of bars and retaining full rows in
            // the echarts series froze the AE tab.
            value: [s, e, lane, term, r[color] ?? '',
                    (label != null ? (r[label] ?? '') : ''),
                    r[series] ?? '',
                    (drill != null ? (r[drill] ?? '') : '')]
          });
        }

        const xAxisSpec = {
          type: xAxisType,
          name: this._axisTitle(x),
          nameLocation: 'middle',
          nameGap: 28,
          nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize },
          axisLabel: { color: ax.labelColor, fontSize: ax.fontSize },
          axisLine: { lineStyle: { color: AXIS_LINE_COLOR } },
          splitLine: { lineStyle: { color: ax.splitLineColor, type: 'dashed' } },
          scale: true
        };
        if (xCats) xAxisSpec.data = xCats;

        // Auto-legend: show whenever color is set. The legend is
        // scroll-type, so high cardinality (AETERM with 200+ values)
        // scrolls instead of being suppressed. Series below are split
        // per color level so echarts has a real series to bind each
        // legend chip to — without that, legend items never paint.
        const showLegend = !!color && colorLevels.length > 0;
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
          // On-bar text is the `label` column's value (slot 5) only.
          // Unset -> no text. label is its own role, never series/color.
          const barLabel = this.config.label
            ? String(api.value(5) ?? '') : '';
          const children = [{
            type: 'rect',
            shape: Object.assign({}, rect, { r: 3 }),
            style: api.style()
          }];
          if (barW > 50 && barLabel) {
            children.push({
              type: 'text',
              style: {
                text: barLabel,
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

        // Build series. When color is set, split barData by level so
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
              const detail = v[6] || '';
              // Headline is the most specific label available: series
              // (per-bar detail) wins, else fall back to y (lane).
              const headline = detail || term;
              let html = `<div style="min-width:180px"><div style="font-weight:700;margin-bottom:4px">${headline}</div>`;
              if (detail && term && term !== detail) {
                html += `<div style="font-size:11px;color:#666">${term}</div>`;
              }
              if (colorVal) html += `<div style="font-size:11px;color:#666">${colorVal}</div>`;
              html += '</div>';
              return html;
            }
          },
          legend: showLegend
            ? { show: true, bottom: 8, type: 'scroll', textStyle: { fontSize: 11 }, data: legendData }
            : { show: false },
          toolbox: TOOLBOX,
          grid: { left: 160, right: 10, top: 20, bottom: showLegend ? 78 : 48 },
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

        chart.setOption(this._applyDrillEmphasis(option), true);

        chart.on('click', (params) => {
          if (this._drillState() === 'off') return;
          if (params.componentType !== 'series') return;
          // The clicked bar's drill value is packed at slot 7 (the effective
          // drill column: lane for 'auto', or an override).
          const v = params.value;
          if (!v) return;
          this._selected = String(v[3] || '');
          this._updateStatus();
          const dv = v[7];
          if (drill && dv != null && dv !== '') {
            this._emitDrill([{ [drill]: dv }]);
          }
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
      // Anchor in viewport coords with position:fixed. The shared CSS
      // anchors the popover absolute/right:0 to the card; in a narrow
      // dock tile a 680px popover then overflows ~500px off-screen left
      // and the panel's overflow:auto clips it. position:fixed is NOT
      // clipped by overflow ancestors and (unlike portaling to body)
      // keeps the element inside .drilldown-chart-container so the
      // scoped popover/family-visibility CSS still applies.
      this._positionPopover();
      // Blockr.Select components reflow after the first paint and grow the
      // popover; reposition on the next frame so the clamp uses the final
      // height.
      requestAnimationFrame(() => {
        if (this._popoverOpen) this._positionPopover();
      });
      this._popReposition = () => {
        if (this._popoverOpen) this._positionPopover();
      };
      window.addEventListener('scroll', this._popReposition, true);
      window.addEventListener('resize', this._popReposition);
    }
    _positionPopover() {
      const g = this.gearBtn.getBoundingClientRect();
      const pop = this.popoverEl;
      const vw = window.innerWidth;
      const vh = window.innerHeight;
      pop.style.position = 'fixed';
      pop.style.right = 'auto';
      // Never let the popover exceed the viewport; it scrolls internally
      // (overflow-y:auto in the stylesheet) when content is taller.
      pop.style.maxHeight = (vh - 16) + 'px';
      const pw = pop.offsetWidth;
      const ph = pop.offsetHeight;
      // Right edge aligns under the gear; clamp within the viewport.
      let left = Math.min(g.right, vw - 8) - pw;
      left = Math.max(8, Math.min(left, vw - pw - 8));
      // Prefer just below the gear; if it would overflow the bottom,
      // lift it up so the whole popover stays on screen.
      let top = g.bottom + 6;
      if (top + ph > vh - 8) top = Math.max(8, vh - 8 - ph);
      pop.style.left = left + 'px';
      pop.style.top = top + 'px';
      // Final guard: tighten max-height to the space actually available
      // from the chosen top, so the bottom edge never leaves the screen.
      pop.style.maxHeight = (vh - top - 8) + 'px';
    }
    _closePopover() {
      this.popoverEl.style.display = 'none';
      this._popoverOpen = false;
      this.gearBtn.classList.remove('blockr-gear-active');
      if (this._popReposition) {
        window.removeEventListener('scroll', this._popReposition, true);
        window.removeEventListener('resize', this._popReposition);
        this._popReposition = null;
      }
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
    // override the default (group + current _selected) — used by
    // click-to-filter on trajectory series and gantt bars, where the column
    // is series/color rather than group.
    _sendCategoricalFilter(col, values) {
      if (!this.el.id) return;
      const column = col !== undefined ? col : this.config.group;
      const vals = values !== undefined ? values
        : (this._selected ? [this._selected] : null);
      Shiny.setInputValue(this.el.id + '_action', {
        action: 'filter', filter_type: 'categorical',
        column: column,
        values: vals
      }, { priority: 'event' });
    }

    // The one drill rule (see blockr.design/open/drilldown-chart-roles).
    // A click identifies a mark; the mark maps to one or more source
    // rows; if `drill` is set, filter downstream on
    // `drill %in% distinct(drill over those rows)`. Unset drill -> inert.
    // Drill-down capability state from the tri-state `drill` config:
    //   ''/null -> 'off'   "auto" -> 'auto' (natural target)   column -> that col
    _drillState() {
      const d = this.config.drill;
      if (!d || d === '') return 'off';
      return d === 'auto' ? 'auto' : 'column';
    }

    // The effective categorical column a selection filters on, or null when the
    // selection should filter geometrically (a scatter point's x&y / brush box).
    //   explicit override -> that column
    //   auto -> the family's natural key: aggregated=group, timeline=lane(y),
    //           line=series/color, scatter=null (point/range)
    _drillColumn() {
      const d = this.config.drill;
      if (!d || d === '') return null;
      if (d !== 'auto') return d;
      const fam = this._family();
      if (fam === 'aggregated') return this.config.group || null;
      if (fam === 'timeline') return this.config.y || null;
      // individual: a series/color split is the natural categorical key (line
      // always; scatter when split). Without one, a scatter filters
      // geometrically (the selected point's x&y).
      return this.config.series || this.config.color || null;
    }

    // When drill is off the chart is a pure display — disable ECharts hover
    // emphasis on marks so there is no interactive-looking effect. Mutates and
    // returns the option for use inline at setOption.
    _applyDrillEmphasis(option) {
      if (this._drillState() === 'off' && option && Array.isArray(option.series)) {
        for (const s of option.series) { if (s) s.emphasis = { disabled: true }; }
      }
      return option;
    }

    _emitDrill(rows) {
      const c = this._drillColumn();
      if (!c || !rows || !rows.length) return;
      const vals = [...new Set(
        rows.map(r => (r ? r[c] : null)).filter(v => v != null)
      )].map(String);
      if (vals.length) this._sendCategoricalFilter(c, vals);
    }

    // Emits a point filter: the single observation(s) at an exact (x, y).
    // Retained for the brush-race guard path; not used by click drill.
    _sendPointFilter(xCol, yCol, xVal, yVal) {
      if (!this.el.id) return;
      Shiny.setInputValue(this.el.id + '_action', {
        action: 'filter', filter_type: 'point',
        x_col: xCol, y_col: yCol, x_val: xVal, y_val: yVal
      }, { priority: 'event' });
    }

    _sendRangeFilter(xRange, yRange) {
      if (!this.el.id) return;
      Shiny.setInputValue(this.el.id + '_action', {
        action: 'filter', filter_type: 'range',
        x_col: this.config.x, y_col: yRange ? this.config.y : null,
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
        group: this.config.group,
        color: this.config.color || '',
        facet: this.config.facet || '',
        metric: this.config.metric,
        agg_fn: this.config.agg_fn,
        chart_type: this.config.chart_type,
        x: this.config.x || '',
        y: this.config.y || '',
        xend: this.config.xend || '',
        sort_by: this.config.sort_by || '',
        sort_dir: this.config.sort_dir || 'asc',
        orientation: this.config.orientation || 'horizontal',
        series: this.config.series || '',
        label: this.config.label || '',
        drill: this.config.drill || '',
        smoother: this.config.smoother || 'none',
        lo: this.config.lo || '',
        hi: this.config.hi || ''
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
      // Remove the document-level outside-click listener (otherwise it
      // accumulates one stale closure per widget instance).
      if (this._outsideClick) {
        document.removeEventListener('click', this._outsideClick);
        this._outsideClick = null;
      }
      // Remove the popover that was portaled to <body> — if the widget
      // element is torn down without dispose() reaching here it would
      // orphan the popover in the DOM.
      if (this.popoverEl && this.popoverEl.parentNode) {
        this.popoverEl.parentNode.removeChild(this.popoverEl);
      }
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
        el._block.setData(p.columns, p.data, p.config, p.arguments);
        delete el._pendingData;
      }
    }
  });
  Shiny.inputBindings.register(binding, 'blockr.drilldown');

  Shiny.addCustomMessageHandler('drilldown-data', (msg) => {
    const el = document.getElementById(msg.id);
    if (el?._block) {
      el._block.setData(msg.columns, msg.data, msg.config, msg.arguments);
    } else if (el) {
      el._pendingData = msg;
    } else {
      let n = 0;
      const t = setInterval(() => {
        n++;
        const el2 = document.getElementById(msg.id);
        if (el2?._block) { el2._block.setData(msg.columns, msg.data, msg.config, msg.arguments); clearInterval(t); }
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
