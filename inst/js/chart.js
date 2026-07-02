// @ts-check
/**
 * Drill-Down Chart — configurable chart that acts as a filter.
 *
 * Three families:
 *   Aggregated (bar, pie, treemap, boxplot, radar): group-by + metric, click
 *     to filter. Radar puts the group levels on the spokes and draws one
 *     shape per color level; a click on a shape filters on its color value.
 *   Individual (scatter, line): x/y columns, brush-drag to filter. Clicking a
 *     line emits a categorical filter on its series (typically USUBJID).
 *   Timeline (gantt): interval rows with start + optional end, categorical y
 *     axis. Clicking a bar emits a USUBJID filter.
 */
(() => {
  'use strict';

  const AGGREGATED_TYPES = ['bar', 'waterfall', 'pie', 'treemap', 'boxplot', 'radar'];
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
  /** @param {any} v */
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

  // Waterfall sign colors (mirrors new_waterfall_block's defaults): increase =
  // Okabe-Ito green, decrease = red, total/subtotal = grey. Semantic, so they
  // override palette cycling for this mode.
  const WATERFALL_COLORS = { increase: '#009E73', decrease: '#dc2626', total: '#bbbbbb' };

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
  /** @param {boolean} withBrush */
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
    // Color-split bar layout. Only meaningful with a `color` mapping; without
    // one all three render identically (a single series). "percent" is stacked
    // + per-group normalization (see _buildAggregatedOption).
    bar_mode: { label: 'Stacking', kind: 'segmented',
                options: [{ value: 'stacked', label: 'Stacked' },
                          { value: 'grouped', label: 'Grouped' },
                          { value: 'percent', label: '100%' }] },
    // A waterfall is a bar with a cumulative baseline — exposed here as a bar
    // option, not its own chart_type. "Waterfall" sets baseline='cumulative'.
    baseline: { label: 'Bars', kind: 'segmented',
                options: [{ value: 'zero', label: 'Standard' },
                          { value: 'cumulative', label: 'Waterfall' }] },
    smoother: { label: 'Smoother', kind: 'select', options: ['none', 'lm', 'loess'] },
    lo:       { label: 'Lo', kind: 'column', colType: 'num' },
    hi:       { label: 'Hi', kind: 'column', colType: 'num' },
    line_width_mult: { label: 'Line width', kind: 'slider' },
    dot_size_mult:   { label: 'Dot size',   kind: 'slider' }
  };

  // FAMILY_ROLES — per family, ordered. A section entry is either a role key
  // (always shown for the family) or { role, types:[...] } (shown only for
  // those chart types). requiredMap rows render immediately; optionalMap rows
  // are added on demand from the "+ Add mapping" menu. `mapping` holds always-on
  // controls shown under the Mapping header after the role rows — for the chart
  // that is `metric` (required, carries its own marker) + the `agg_fn`: a bar IS
  // a sum-of-group, so the aggregation is part of the mapping, not a separate
  // "encoding" step.
  const FAMILY_ROLES = {
    aggregated: {
      requiredMap: ['group'],
      optionalMap: ['color', 'facet', 'label'],
      mapping: ['metric', { role: 'agg_fn', types: ['bar', 'waterfall', 'pie', 'treemap', 'radar'] }],
      // orientation: bar only for v1 (boxplot swap is a follow-up). Waterfall
      // is vertical-only (a bridge reads left-to-right along the value axis), so
      // it does not expose orientation.
      presentation: ['sort_by', 'sort_dir', { role: 'orientation', types: ['bar'] },
        { role: 'bar_mode', types: ['bar'] },
        { role: 'baseline', types: ['bar'] }]
    },
    individual: {
      requiredMap: ['x', 'y'],
      optionalMap: ['series', 'color', 'facet', 'label'],
      mapping: [],
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
      mapping: [],
      presentation: ['sort_by', 'sort_dir']
    }
  };

  // Roles whose control renders inside its primary's row (the paired tail), so
  // a section loop skips them. Passed to the shared config engine.
  const DD_SECONDARY = new Set(
    Object.values(ROLES).map(r => /** @type {any} */ (r).pairedWith).filter(Boolean));

  class DrilldownChart {
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      /** @type {any[]} */
      this.data = [];
      /** @type {VizColumn[]} */
      this.columns = [];
      /** @type {Record<string, any>} */
      this.config = {};
      /** @type {Record<string, any>} */
      this.argHelp = {};
      /** @type {any[]} */
      this.charts = [];
      /** @type {any} */
      this._selected = null;
      /** @type {any} */
      this.theme = null;  // null -> echarts default theme
      // DOM fields are populated by _buildDOM() (called below); declared here so
      // their types are definite for the type-checker. These statements are
      // no-op reads at runtime.
      /** @type {HTMLDivElement} */
      this.card;
      /** @type {HTMLButtonElement} */
      this.gearBtn;
      /** @type {HTMLDivElement} */
      this.popoverEl;
      /** @type {HTMLDivElement} */
      this.chartGrid;
      /** @type {HTMLDivElement} */
      this.statusEl;
      /** @type {((e: MouseEvent) => void) | null | undefined} */
      this._outsideClick;
      /** @type {boolean | undefined} */
      this._popoverOpen;
      /** @type {any} */
      this._cfg;
      /** @type {any} */
      this._selectedColumn;
      /** @type {Record<string, any> | null} */
      this._colorLookup;
      this._buildDOM();
      // The gear-popover config engine (shared with the table block). It owns
      // the popover rendering, role rows, add-as-needed, sticky memory and the
      // family/type switch; this chart supplies the block-specific hooks.
      this._cfg = this._makeConfig();
    }

    _makeConfig() {
      const DCfg = /** @type {typeof VizDrilldownConfig} */ ((typeof Blockr !== 'undefined' && Blockr.DrilldownConfig) || window.DrilldownConfig);
      return new DCfg({
        popoverEl: () => this.popoverEl,
        roles: ROLES,
        config: () => this.config,
        columns: () => this.columns,
        context: () => this._family(),
        currentType: () => this.config.chart_type,
        sections: () => FAMILY_ROLES[this._family()],
        sectionsForFamily: (/** @type {string} */ fam) => /** @type {Record<string, any>} */ (FAMILY_ROLES)[fam],
        secondary: DD_SECONDARY,
        typeKey: 'chart_type',
        typeGroups: [
          // Waterfall is intentionally NOT a picker button — it is a bar option
          // (the `baseline` toggle). It stays in AGGREGATED_TYPES so saved boards
          // with chart_type="waterfall" still classify/render as aggregated bars.
          { label: 'Aggregated', types: AGGREGATED_TYPES.filter(function (/** @type {string} */ t) { return t !== 'waterfall'; }) },
          { label: 'Individual', types: INDIVIDUAL_TYPES },
          { label: 'Timeline', types: TIMELINE_TYPES }
        ],
        familyFor: (/** @type {string} */ t) => AGGREGATED_TYPES.includes(t) ? 'aggregated'
          : TIMELINE_TYPES.includes(t) ? 'timeline' : 'individual',
        // `drill` and `metric` must persist across a family switch — drill is a
        // capability; metric is a required-for-init slot (clearing it wedges the
        // block, the family-switch freeze bug).
        carryKeep: ['drill', 'metric'],
        entryRequired: (/** @type {string} */ role) => role === 'metric' && this._family() === 'aggregated',
        drillAutoLabel: () => {
          const fam = this._family();
          if (this.config.chart_type === 'radar') return 'Auto — the clicked shape';
          return fam === 'aggregated' ? 'Auto — the clicked group'
            : fam === 'timeline' ? 'Auto — the clicked lane'
            : this.config.chart_type === 'line' ? 'Auto — the clicked series'
            : 'Auto — the selected point';
        },
        title: 'Chart settings',
        onChange: () => { this._render(); this._sendConfig(); },
        onMults: () => this._sendMults(),
        onClearFilter: () => { this._selected = null; this._sendClearFilter(); },
        ensureDefaults: () => this._ensureFamilyDefaults(),
        afterTypeChange: () => this._updateFamilyClass(),
        isOpen: () => this._popoverOpen,
        reopen: () => this._openPopover()
      });
    }

    // Thin delegators so external callers (tests / harness) and setData keep
    // working after the engine moved into DrilldownConfig.
    _renderConfig() { this._cfg.render(); }
    /** @param {string} t */
    _onChartType(t) { this._cfg._onType(t); }
    /** @param {string} key */
    _addRole(key) { this._cfg._addRole(key); }
    /** @param {string} key */
    _removeRole(key) { this._cfg._removeRole(key); }
    /** @param {string} key @param {any} val */
    _rememberRole(key, val) { this._cfg._rememberRole(key, val); }

    /** @param {any} theme */
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

    // The bar baseline mode: "zero" (a plain bar, every bar starts at 0) or
    // "cumulative" (a waterfall/bridge — each bar floats from the running
    // cumulative of the bars before it). chart_type "waterfall" is sugar for
    // bar + baseline="cumulative"; an explicit config.baseline on a bar also
    // works (the general model). Anything else is "zero".
    _baselineMode() {
      if (this.config.chart_type === 'waterfall') return 'cumulative';
      if (this.config.chart_type === 'bar' &&
          this.config.baseline === 'cumulative') return 'cumulative';
      return 'zero';
    }

    // Pick an echarts axis type from column metadata. Returns 'category',
    // 'value', or 'time'. Date columns are detected by name ending in "DT"
    // plus numeric ms values (the convention used by the AE gantt R code).
    // Board scale map (config.scales = { var, color: {level: hex},
    // order: [...] }, resolved in R). Returns it when it targets `varName`,
    // else null — unregistered variables keep palette cycling.
    /** @param {string} varName */
    _scaleFor(varName) {
      const sc = this.config && this.config.scales;
      return (sc && varName && sc.var === varName) ? sc : null;
    }

    // Order a set of levels: by the scale's order when one applies, else by
    // the column's factor levels (column metadata), else alphabetically —
    // the pre-scale-map behavior.
    /** @param {any[]} levels @param {any} scale @param {string} colName */
    _orderLevels(levels, scale, colName) {
      /** @param {any[]} ref */
      const by = (ref) => {
        /** @type {Map<string, number>} */
        const idx = new Map(ref.map((/** @type {any} */ l, /** @type {number} */ i) => /** @type {[string, number]} */ ([String(l), i])));
        return levels.slice().sort((/** @type {any} */ a, /** @type {any} */ b) =>
          ((idx.has(a) ? /** @type {number} */ (idx.get(a)) : 1e9) - (idx.has(b) ? /** @type {number} */ (idx.get(b)) : 1e9))
          || a.localeCompare(b));
      };
      if (scale && Array.isArray(scale.order) && scale.order.length) {
        return by(scale.order);
      }
      const meta = /** @type {any} */ ((this.columns || []).find(c => c.name === colName));
      if (meta && Array.isArray(meta.levels) && meta.levels.length) {
        return by(meta.levels);
      }
      return levels.slice().sort();
    }

    /** @param {string} colName */
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
    /** @param {string} colName @param {any[]} [rows] */
    _orderedCategories(colName, rows) {
      const src = rows || this.data || [];
      // Factor level order from column metadata wins (the data-level order
      // contract); values outside the declared levels append in
      // first-occurrence order.
      const meta = /** @type {any} */ ((this.columns || []).find(c => c.name === colName));
      if (meta && Array.isArray(meta.levels) && meta.levels.length) {
        const present = new Set(src.map((/** @type {any} */ r) => String(r[colName] ?? '')));
        const out = meta.levels.map(String).filter((/** @type {string} */ l) => present.has(l));
        for (const k of present) if (!out.includes(k)) out.push(k);
        return out;
      }
      /** @type {Record<string, string>} */
      const companions = { AVISIT: 'AVISITN' };
      const companion = companions[colName];
      if (companion && src.length && src[0][companion] !== undefined) {
        /** @type {Record<string, number>} */
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
      // The settings band lives inside the card, so clearing the element
      // removes it along with everything else.
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
      this.gearBtn.setAttribute('aria-label', 'Chart settings');
      this.gearBtn.setAttribute('aria-haspopup', 'dialog');
      this.gearBtn.setAttribute('aria-expanded', 'false');
      this.gearBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        this._togglePopover();
      });
      gearHeader.appendChild(this.gearBtn);
      this.card.appendChild(gearHeader);

      // All configuration (mapping + presentation) lives behind the gear.
      // The card itself shows only the result + its direct interactions.

      // In-flow settings band (design-system pilot — blockr.ui/dev/
      // gear-panel-proposals.html, variant B): inside the card between the
      // gear header and the chart, full block width. No <body> portal, no
      // fixed positioning, no outside-click dismissal — it is a panel, not a
      // menu; opening pushes the chart down so the result stays visible.
      this.popoverEl = document.createElement('div');
      this.popoverEl.className = 'blockr-settings dd-popover';
      this.card.appendChild(this.popoverEl);
      // A widget re-render rebuilds the DOM; restore the band's open state.
      if (this._popoverOpen) {
        this.popoverEl.classList.add('blockr-settings--open');
        this.gearBtn.classList.add('blockr-gear-active');
        this.gearBtn.setAttribute('aria-expanded', 'true');
      }

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

    // Axis title for a mapped column: its variable label when present,
    // else the column name. The settings-band select shows `name  label`;
    // an axis is tighter, so just the human label (or the name).
    /** @param {string} col */
    _axisTitle(col) {
      if (!col) return '';
      const c = (this.columns || []).find(x => x.name === col);
      if (c && c.label && c.label !== c.name) return c.label;
      return col;
    }

    /** @param {any} v */
    _hasVal(v) { return v !== null && v !== undefined && v !== '' && v !== '(none)'; }

    // Size the horizontal category-label gutter to the actual labels rather
    // than a fixed 150px column. ECharts' grid doesn't auto-fit to category
    // label width, so the family used a fixed wide gutter to give long terms
    // (AETERM) a stable truncating column. The cost: short codes (701, arm
    // names) and narrow dock panels got a giant empty left margin. This
    // measures the widest label and caps it at LABEL_CAP — content-fitting for
    // short labels, still truncating for long ones. Returns the three coupled
    // numbers the axis/grid need (axisLabel.width + .margin and grid.left),
    // preserving the original 5px label->axis gap and 10px container inset.
    /** @param {any[]} labels */
    _yGutter(labels) {
      const LABEL_CAP = 145; // matches the historical truncate width
      // Slack added to the measured width: the canvas may measure with the
      // fallback font before 'Open Sans' loads (ECharts then paints the wider
      // real font), and an exactly-fitting box truncates on sub-pixel rounding.
      // Without it short labels like "701" render as "7…".
      const PAD = 10;
      const DC = /** @type {any} */ (DrilldownChart);
      const ctx = DC._measureCtx ||
        (DC._measureCtx = document.createElement('canvas').getContext('2d'));
      ctx.font = `11px ${BLOCKR_FONT}`;
      let w = 0;
      for (const l of labels) {
        const tw = ctx.measureText(String(l ?? '')).width;
        if (tw > w) w = tw;
        if (w >= LABEL_CAP) break;
      }
      w = Math.min(Math.ceil(w) + PAD, LABEL_CAP);
      return { width: w, margin: w + 5, gridLeft: w + 15 };
    }

    // Category labels for an x-axis (vertical bars, waterfall) — the transpose
    // of _yGutter. Always shows every label (interval:0; decimation would drop
    // steps of a waterfall). Renders labels HORIZONTAL when each fits its
    // per-category column, else VERTICAL (90deg) — never diagonal. Long
    // vertical labels truncate with an ellipsis at a height cap, recoverable
    // via the bar/axis tooltip (which carries the full category name). Returns
    // { axisLabel, bottom } where `bottom` is the extra grid gutter the rotated
    // text needs (0 when horizontal).
    /** @param {any[]} labels @param {number} availW plot width minus grid margins */
    _xAxisLabels(labels, availW) {
      const DC = /** @type {any} */ (DrilldownChart);
      const ctx = DC._measureCtx ||
        (DC._measureCtx = document.createElement('canvas').getContext('2d'));
      ctx.font = `11px ${BLOCKR_FONT}`;
      let widest = 0;
      for (const l of labels) {
        const tw = ctx.measureText(String(l ?? '')).width;
        if (tw > widest) widest = tw;
      }
      const PAD = 8;
      const n = Math.max(1, labels.length);
      // Width is unknown when the panel is hidden (clientWidth 0); fall back to
      // a typical panel width so we don't wrongly rotate short labels.
      const slot = (availW > 0 ? availW : 600) / n;
      const base = { color: AXIS_LABEL_COLOR, fontSize: 11, interval: 0 };
      if (widest + PAD <= slot) {
        // Fits horizontally — keep flat, truncate only as a safety net.
        return {
          axisLabel: { ...base, rotate: 0, overflow: 'truncate',
            width: Math.max(10, Math.floor(slot - PAD)), ellipsis: '…' },
          bottom: 0
        };
      }
      // Doesn't fit — rotate to vertical and truncate long labels at a cap.
      const CAP = 120;
      const len = Math.min(Math.ceil(widest) + PAD, CAP);
      return {
        axisLabel: { ...base, rotate: 90, overflow: 'truncate',
          width: len, ellipsis: '…' },
        bottom: len
      };
    }

    // Bottom-anchored legends wrap onto extra rows when the chip labels
    // exceed the chart width, and ECharts grows them UPWARD from bottom:0 —
    // every grid.bottom in the option builders reserves a single legend row,
    // so row two lands on the x-axis title. This predicts the wrap the same
    // way the label helpers above predict text width (shared canvas,
    // ECharts' horizontal-legend layout constants: 25px chip + ~5px
    // chip-to-text gap per item, 10px itemGap between items and rows, 14px
    // itemHeight, 5px viewport padding per side) and returns
    //   { extra, scroll }
    // extra  = px to ADD to the one-row reservation (0 for a single row).
    // scroll = true when the legend would exceed MAX_ROWS; the caller should
    //          set legend type:'scroll' (the timeline chart's standing policy
    //          for high cardinality) and reserve nothing extra, instead of
    //          letting the legend eat the plot.
    // The +2px per-item slack biases toward predicting a wrap: over-reserving
    // costs whitespace, under-reserving is the overlap this exists to fix.
    /** @param {any[]} items legend entries (strings or {name} objects)
     *  @param {number} plotW chart container width in px (0 = unknown) */
    _legendRows(items, plotW) {
      const ONE_ROW = { extra: 0, scroll: false };
      // Width unknown (hidden panel): assume the row the grid already
      // reserves — a resize re-renders with a real width anyway.
      if (!items || items.length < 2 || !(plotW > 0)) return ONE_ROW;
      const MAX_ROWS = 4;
      const ROW_ADVANCE = 24; // itemHeight 14 + 10 itemGap between rows
      const DC = /** @type {any} */ (DrilldownChart);
      const ctx = DC._measureCtx ||
        (DC._measureCtx = document.createElement('canvas').getContext('2d'));
      ctx.font = `11px ${BLOCKR_FONT}`;
      const availW = plotW - 10;
      let x = 0;
      let rows = 1;
      for (const it of items) {
        const name = it != null && typeof it === 'object' ? it.name : it;
        const w = 25 + 5 + ctx.measureText(String(name ?? '')).width + 2;
        if (x > 0 && x + w > availW) { rows += 1; x = 0; }
        x += w + 10;
      }
      if (rows > MAX_ROWS) return { extra: 0, scroll: true };
      return { extra: (rows - 1) * ROW_ADVANCE, scroll: false };
    }

    // Radar companion to the grid.bottom reservation: the radar canvas is a
    // fixed 350px tall and has no grid, so extra legend rows instead lift the
    // polygon's center and shrink its radius by half the extra each, clearing
    // both the top edge and the legend block. Base numbers are the '62%' /
    // '46%' defaults in px. Shared by the builder and _refitLegend.
    /** @param {number} extra @param {number} plotW @param {boolean} showLegend */
    _radarLayout(extra, plotW, showLegend) {
      const H = 350;
      const baseRadius = 0.62 * Math.min(plotW > 0 ? plotW : H, H) / 2;
      return {
        radius: extra ? Math.max(60, Math.round(baseRadius - extra / 2)) : '62%',
        center: ['50%', extra
          ? Math.round(0.46 * H - extra / 2)
          : (showLegend ? '46%' : '50%')]
      };
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
          const cat = cols.find((/** @type {any} */ c) => c.type === 'categorical' && c.n_unique <= 30);
          cfg.group = cat ? cat.name : cols[0].name;
        }
        if (!cfg.metric) cfg.metric = '.count';
        if (!cfg.agg_fn) cfg.agg_fn = 'count';
        if (!this._hasVal(cfg.sort_by)) cfg.sort_by = 'value';
        if (!this._hasVal(cfg.sort_dir)) cfg.sort_dir = 'desc';
        if (!this._hasVal(cfg.orientation)) cfg.orientation = 'horizontal';
        if (!this._hasVal(cfg.bar_mode)) cfg.bar_mode = 'stacked';
      } else if (fam === 'timeline') {
        if (!this._hasVal(cfg.x) && cols.length) {
          const num = cols.find(c => c.type === 'numeric');
          cfg.x = num ? num.name : cols[0].name;
        }
        if (!this._hasVal(cfg.y) && cols.length) {
          const cat = cols.find((/** @type {any} */ c) => c.type === 'categorical' && c.n_unique > 1);
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

    /** @param {any} columns @param {any} data @param {any} config @param {any} args */
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
          /** @type {Record<string, any>} */
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
          const cat = this.columns.find((/** @type {any} */ c) => c.type === 'categorical' && c.n_unique <= 30);
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
          const cat = this.columns.find((/** @type {any} */ c) => c.type === 'categorical' && c.n_unique > 1);
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

      /** @type {Record<string, any>} */
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
        else if (agg_fn === 'mean') value = g.values.length ? g.values.reduce((/** @type {number} */ a, /** @type {number} */ b) => a + b, 0) / g.values.length : 0;
        else if (agg_fn === 'median') { const s = g.values.slice().sort((/** @type {number} */ a, /** @type {number} */ b) => a - b); const m = Math.floor(s.length / 2); value = s.length ? (s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2) : 0; }
        else if (agg_fn === 'sum') value = g.values.reduce((/** @type {number} */ a, /** @type {number} */ b) => a + b, 0);
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
          unset.map(k => /** @type {Record<string, any>} */ (ROLES)[k].label).join(' and ') + ' to plot.</p></div>';
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
        const avail = (this.columns || []).map(c => c.name);
        const availTxt = avail.length
          ? ' Columns available here: ' + avail.slice(0, 30).join(', ') +
            (avail.length > 30 ? ', …' : '') + '.'
          : '';
        this.chartGrid.innerHTML =
          '<div class="vd-empty-state"><p class="vd-empty-text">' +
          'Mapped column not in data: ' + missing.join(', ') + '.' + availTxt +
          ' A rename, flatten or pivot upstream may have changed the column ' +
          'name — re-pick it in the gear.</p></div>';
        return;
      }

      if (fam === 'aggregated') this._renderAggregated();
      else if (fam === 'timeline') this._renderTimeline();
      else this._renderIndividual();

      // Resize after render + watch for container becoming visible (dock tab switch)
      setTimeout(() => { this._resizeCharts(); }, 300);
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
      const colorScale = this._scaleFor(this.config.color);
      const colors = this._orderLevels(
        [...new Set(agg.map(a => a.color))].filter(c => c !== '__all__'),
        colorScale, this.config.color);
      const palette = BLOCKR_PALETTE;
      const singleFacet = facets.length === 1;

      // Switch grid off for single facet
      this.chartGrid.classList.toggle('dd-chart-grid-single', singleFacet);

      const sortBy = this.config.sort_by || 'alpha';
      const sortDir = this.config.sort_dir === 'desc' ? -1 : 1;

      // Waterfall (baseline=cumulative) is a running bridge — the step order
      // IS the data order (or the factor levels of the step column), NEVER a
      // value sort, which would scramble the cumulative path. Honor data order:
      // factor levels if the step column is a factor, else first-seen order in
      // the raw data. This overrides sort_by for this mode.
      const cumulative = this._baselineMode() === 'cumulative';

      // Ordering for the category axis. "alpha" = group name;
      // "value" = total of the computed metric across color stacks;
      // otherwise, a raw-data column whose minimum per group orders the axis.
      /** @param {any[]} facetData */
      const orderGroups = (facetData) => {
        const groups = [...new Set(facetData.map(a => a.group))];
        if (cumulative) {
          const meta = /** @type {any} */ ((this.columns || []).find(c => c.name === this.config.group));
          if (meta && Array.isArray(meta.levels) && meta.levels.length) {
            const idx = new Map(meta.levels.map((/** @type {any} */ l, /** @type {number} */ i) => [String(l), i]));
            const present = new Set(groups);
            return meta.levels.map(String).filter((/** @type {string} */ l) => present.has(l))
              .concat(groups.filter(g => !idx.has(g)));
          }
          // First-seen order in the raw data (stable bridge ordering).
          /** @type {string[]} */
          const seen = [];
          const seenSet = new Set();
          const groupCol = this.config.group;
          for (const r of this.data) {
            const g = groupCol ? String(r[groupCol] ?? '') : 'Total';
            if (!seenSet.has(g)) { seenSet.add(g); seen.push(g); }
          }
          const present = new Set(groups);
          return seen.filter(g => present.has(g))
            .concat(groups.filter(g => !seenSet.has(g)));
        }
        if (sortBy === 'alpha') {
          // Factor columns: "alpha" means level order (data-level contract).
          const meta = /** @type {any} */ ((this.columns || []).find(c => c.name === this.config.group));
          if (meta && Array.isArray(meta.levels) && meta.levels.length) {
            /** @type {Map<string, number>} */
            const idx = new Map(meta.levels.map((/** @type {any} */ l, /** @type {number} */ i) => /** @type {[string, number]} */ ([String(l), i])));
            return groups.sort((a, b) =>
              (((idx.has(a) ? /** @type {number} */ (idx.get(a)) : 1e9) - (idx.has(b) ? /** @type {number} */ (idx.get(b)) : 1e9))
                || a.localeCompare(b)) * sortDir);
          }
          return groups.sort((a, b) => a.localeCompare(b) * sortDir);
        }
        if (sortBy === 'value') {
          /** @type {Record<string, number>} */
          const totals = {};
          for (const a of facetData) totals[a.group] = (totals[a.group] || 0) + a.value;
          return groups.sort((a, b) => (totals[a] - totals[b]) * sortDir);
        }
        // Column name: look up each group's min over the raw data.
        const groupCol = this.config.group;
        if (!groupCol) return groups.sort((a, b) => a.localeCompare(b) * sortDir);
        /** @type {Record<string, number>} */
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

        /** @type {HTMLElement} */
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
        // Waterfall is vertical (category on the x-axis), so it takes the fixed
        // height like pie/radar — not the per-row horizontal-bar height.
        chartDiv.style.height = (ct === 'pie' || ct === 'treemap' ||
            ct === 'radar' || this._baselineMode() === 'cumulative')
          ? '350px'
          : Math.max(350, groups.length * 28 + 60) + 'px';
        container.appendChild(chartDiv);

        const option = this._buildAggregatedOption(facetData, groups, colors, palette, chartDiv.clientWidth);
        if (!option) {
          chartDiv.innerHTML = '<div class="vd-empty-state"><p class="vd-empty-text">Boxplot needs a numeric metric</p></div>';
          continue;
        }
        const chart = echarts.init(chartDiv, this.theme || undefined);
        this.charts.push(chart);
        // Legend-fit metadata rides on the option (the builders have no chart
        // handle); move it onto the instance for _refitLegend before ECharts
        // sees the option.
        const anyOption = /** @type {any} */ (option);
        /** @type {any} */ (chart).__legendFit = anyOption.__legendFit;
        delete anyOption.__legendFit;
        chart.setOption(this._applyDrillEmphasis(option), true);

        chart.on('click', (params) => {
          if (this._drillState() === 'off') return;
          // Radar: the clickable mark is the shape (one per color level), so
          // the selection identifies a color value, not a group. Without a
          // color mapping there is a single all-rows shape — nothing to
          // select, the click is inert.
          const isRadar = ct === 'radar';
          if (isRadar && !this.config.color) return;
          const clickedGroup = params.name || (params.value && params.value[0]);
          if (!clickedGroup) return;
          this._selected = this._selected === clickedGroup ? null : clickedGroup;
          this._updateHighlight();
          if (this._selected == null) { this._sendClearFilter(); return; }
          // The clicked mark's source rows -> _emitDrill. With drill 'auto'
          // the target is the group column (radar: the color column); with
          // an override, that column.
          const g = isRadar ? this.config.color : this.config.group;
          const fc = this.config.facet;
          const rows = (this.data || []).filter(r =>
            String(r[g]) === String(clickedGroup) &&
            (facet === '__all__' || !fc || String(r[fc]) === String(facet)));
          this._emitDrill(rows);
        });
      }
      this._updateHighlight();
    }

    /** @param {any[]} facetData @param {any[]} groups @param {any[]} colors @param {any[]} palette @param {number} [plotW] container width in px */
    _buildAggregatedOption(facetData, groups, colors, palette, plotW) {
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
      if (ct === 'radar') return this._buildRadar(facetData, groups, colors, palette, valueTitle, plotW || 0);
      // Waterfall = a bar with baseline "cumulative": each bar floats from the
      // running cumulative of the bars before it. Sugar for bar +
      // baseline="cumulative" (see _baselineMode). It reuses the same aggregated
      // contract (group=step axis, metric+agg_fn=value) and the same bar option
      // shell — only the bars' baseline is shifted.
      if (this._baselineMode() === 'cumulative') {
        return this._buildWaterfall(facetData, groups, valueTitle, ax, plotW);
      }

      // Color-split bar layout. "grouped" drops the shared stack so ECharts
      // dodges the series side-by-side; "percent" keeps the stack but feeds
      // each segment as a share (0..1) of its group total (ECharts has no
      // native percent stack). "stacked" (default) is absolute stacking.
      const barMode = this.config.bar_mode || 'stacked';
      const isGrouped = barMode === 'grouped';
      const isPercent = barMode === 'percent';

      // Bar sizing. Stacked/percent/single = one bar per category at a fixed
      // 60% width. Grouped = several bars per category: DON'T force a per-series
      // width (two 60% bars overflow the band and let the default barGap wedge a
      // gap inside the group). Instead let ECharts auto-size the series equally
      // (same width for every color) with barGap:0 so they touch, and a category
      // gap so groups stay separated.
      const barLayout = isGrouped
        ? { barGap: 0, barCategoryGap: '30%' }
        : { barWidth: '60%' };

      /** @type {any[]} */
      const series = [];
      if (colors.length === 0) {
        series.push({ type: 'bar', data: groups.map(g => { const d = facetData.find(a => a.group === g); return d ? d.value : 0; }), itemStyle: { color: palette[0] }, barWidth: '60%', emphasis: { focus: 'self' } });
      } else {
        const colorScale = this._scaleFor(this.config.color);
        // Per-group total across colors, for percent normalization only.
        /** @type {Record<string, number>} */
        const groupTotals = {};
        if (isPercent) {
          for (const g of groups) {
            groupTotals[g] = facetData
              .filter(a => a.group === g && a.value != null)
              .reduce((s, a) => s + a.value, 0);
          }
        }
        for (let ci = 0; ci < colors.length; ci++) {
          const color = colors[ci];
          series.push({
            type: 'bar', name: color,
            // Use null (not 0) for missing (group, color) combos. Stacked
            // bars skip nulls — this ensures a patient on a single arm
            // renders as one colored segment, even if the other arm series
            // get constructed for the rest of the cohort. In percent mode
            // the datum is an object {value: share, raw} so the tooltip can
            // show both the percentage and the underlying value.
            data: groups.map(g => {
              const d = facetData.find(a => a.group === g && a.color === color);
              const raw = d ? d.value : null;
              if (raw == null) return null;
              if (isPercent) {
                const tot = groupTotals[g];
                return tot ? { value: raw / tot, raw } : null;
              }
              return raw;
            }),
            // Percent is still stacked (shares sum to 1); only "grouped" dodges.
            ...(isGrouped ? {} : { stack: 'stack' }),
            itemStyle: {
              color: (colorScale && colorScale.color && colorScale.color[color])
                || palette[ci % palette.length]
            },
            ...barLayout,
            emphasis: { focus: 'self' }
          });
        }
      }

      // Orientation is a presentation property (the ggplot coord_flip model):
      // horizontal (default) keeps the category on the y-axis \u2014 best for long
      // labels (AE terms, arms); vertical puts it on the x-axis. The mapping
      // is unchanged (Group=category, Metric=value) \u2014 flipping re-maps nothing.
      const vertical = this.config.orientation === 'vertical';
      const gut = this._yGutter(groups);
      // Vertical bars put categories on the x-axis: horizontal-or-vertical
      // labels (never diagonal), all shown. Grid bottom grows for rotated text.
      const xlab = vertical ? this._xAxisLabels(groups, (plotW || 0) - 65) : null;
      const catAxis = vertical
        ? { type: 'category', data: groups, axisLabel: xlab?.axisLabel, axisLine: { lineStyle: { color: AXIS_LINE_COLOR } }, axisTick: { show: false } }
        : { type: 'category', data: groups, inverse: true, axisLabel: { color: ax.labelColor, fontSize: ax.fontSize, align: 'left', margin: gut.margin, width: gut.width, overflow: 'truncate', ellipsis: '\u2026' }, axisLine: { show: false }, axisTick: { show: false } };
      // Percent display only applies when a color split is actually present
      // (a single series is trivially 100% of itself).
      const showPercent = isPercent && colors.length > 0;
      const valAxis = {
        type: 'value', name: showPercent ? '% of group total' : valueTitle,
        nameLocation: 'middle',
        nameGap: vertical ? 45 : 30,
        nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize },
        axisLabel: {
          color: ax.labelColor, fontSize: ax.fontSize,
          ...(showPercent
            ? { formatter: (/** @type {number} */ v) => Math.round(v * 100) + '%' }
            : {})
        },
        ...(showPercent ? { max: 1 } : {}),
        axisLine: { lineStyle: { color: AXIS_LINE_COLOR } },
        splitLine: { lineStyle: { color: ax.splitLineColor, type: 'dashed' } }
      };
      // Percent tooltip shows both the share and the raw value (carried on the
      // datum as {value, raw}); default axis tooltip otherwise.
      const fmtRaw = (/** @type {number} */ n) =>
        Number.isInteger(n) ? n : Math.round(n * 100) / 100;
      /** @type {Record<string, any>} */
      const tooltip = { trigger: 'axis', axisPointer: { type: 'shadow' }, confine: true };
      if (showPercent) {
        tooltip.formatter = (/** @type {any[]} */ ps) => {
          if (!ps || !ps.length) return '';
          const head = ps[0].axisValueLabel || ps[0].name || '';
          const rows = ps
            .filter(p => p && p.value != null)
            .map(p => {
              const pct = Math.round((Number(p.value) || 0) * 100);
              const raw = p.data && p.data.raw != null ? fmtRaw(p.data.raw) : null;
              return p.marker + p.seriesName + ': ' + pct + '%' +
                (raw != null ? ' (' + raw + ')' : '');
            });
          return head + '<br/>' + rows.join('<br/>');
        };
      }
      const legendOn = colors.length > 0;
      const leg = legendOn ? this._legendRows(colors, plotW || 0) : { extra: 0, scroll: false };
      const bottomBase = vertical
        ? (legendOn ? 55 : 40) + 26 + (xlab ? xlab.bottom : 0)
        : (legendOn ? 55 : 20) + 26;
      return {
        __legendFit: legendOn
          ? { items: colors, base: bottomBase, key: leg.extra + (leg.scroll ? 'S' : '') }
          : undefined,
        ...(this.theme ? {} : { backgroundColor: 'transparent' }),
        textStyle: { fontFamily: BLOCKR_FONT },
        tooltip,
        toolbox: TOOLBOX,
        legend: legendOn
          ? { show: true, bottom: 0, textStyle: { fontSize: 11 }, ...(leg.scroll ? { type: 'scroll' } : {}) }
          : undefined,
        grid: vertical
          ? { left: 55, right: 10, top: 30, bottom: bottomBase + leg.extra }
          : { left: gut.gridLeft, right: 5, top: 30, bottom: bottomBase + leg.extra },
        xAxis: vertical ? catAxis : valAxis,
        yAxis: vertical ? valAxis : catAxis,
        series
      };
    }

    // Waterfall / bridge: a bar chart with baseline="cumulative". Each step's
    // aggregated value is its DELTA; the bar floats from the running cumulative
    // before it to running cumulative + delta. A "total" step resets the
    // baseline to 0 and draws the absolute running cumulative (a subtotal /
    // grand total marker). Sign-colored (green up, red down, grey total).
    //
    // Implementation mirrors waterfall-block.R::build_waterfall_data but in two
    // ECharts stacked bar series: a transparent "base" series carrying the
    // floating offset, and a visible "delta" series carrying the bar height,
    // each datum colored per sign. (vertical orientation only — a bridge reads
    // left-to-right.)
    /** @param {any[]} facetData @param {any[]} groups @param {any} valueTitle @param {any} ax @param {number} [plotW] container width in px */
    _buildWaterfall(facetData, groups, valueTitle, ax, plotW) {
      // One value per step (sum across any color split — waterfall is a single
      // series along the step axis, color does not stack here).
      /** @param {any} g */
      const valOf = (g) => facetData.filter(a => a.group === g)
        .reduce((/** @type {number} */ s, /** @type {any} */ a) => s + a.value, 0);

      // Which steps are "total" bars (baseline reset to 0). Optional; threaded
      // from R as config.waterfall_totals (array of step/group names). Default
      // none -> every bar is relative.
      const totals = new Set((this.config.waterfall_totals || []).map(String));

      /** @type {any[]} */
      const base = [];     // transparent offset (the floating baseline)
      /** @type {any[]} */
      const delta = [];    // visible bar height, sign-colored per datum
      let cum = 0;
      for (let i = 0; i < groups.length; i++) {
        const g = groups[i];
        const v = valOf(g);
        const isTotal = totals.has(g);
        if (isTotal) {
          // Total / subtotal bar: from 0 up to the running cumulative.
          base.push(0);
          delta.push({ value: cum, itemStyle: { color: WATERFALL_COLORS.total } });
          // A total bar does not advance the cumulative (it restates it).
        } else if (v >= 0) {
          base.push(cum);
          delta.push({ value: v, itemStyle: { color: WATERFALL_COLORS.increase } });
          cum += v;
        } else {
          // Negative delta: the bar hangs down from the prior cumulative.
          base.push(cum + v);
          delta.push({ value: -v, itemStyle: { color: WATERFALL_COLORS.decrease } });
          cum += v;
        }
      }

      // Step labels on the x-axis: horizontal-or-vertical (never diagonal),
      // every step shown (a waterfall with a hidden step label is unreadable).
      const xlab = this._xAxisLabels(groups, (plotW || 0) - 65);
      const catAxis = {
        type: 'category', data: groups,
        axisLabel: xlab.axisLabel,
        axisLine: { lineStyle: { color: AXIS_LINE_COLOR } },
        axisTick: { show: false }, splitLine: { show: false }
      };
      const valAxis = {
        type: 'value', name: valueTitle, nameLocation: 'middle', nameGap: 45,
        nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize },
        axisLabel: { color: ax.labelColor, fontSize: ax.fontSize },
        axisLine: { lineStyle: { color: AXIS_LINE_COLOR } },
        splitLine: { lineStyle: { color: ax.splitLineColor, type: 'dashed' } }
      };
      return {
        ...(this.theme ? {} : { backgroundColor: 'transparent' }),
        textStyle: { fontFamily: BLOCKR_FONT },
        tooltip: {
          trigger: 'axis', axisPointer: { type: 'shadow' }, confine: true,
          // Only report the visible delta series (the transparent base is an
          // implementation detail).
          formatter: (/** @type {any} */ params) => {
            const p = (params || []).find((/** @type {any} */ x) => x.seriesName === 'delta');
            if (!p) return '';
            return '<b>' + p.name + '</b><br>' + ddNum(p.value);
          }
        },
        toolbox: TOOLBOX,
        legend: { show: false },
        grid: { left: 55, right: 10, top: 30, bottom: 40 + 26 + xlab.bottom },
        xAxis: catAxis,
        yAxis: valAxis,
        series: [
          { name: 'base', type: 'bar', stack: 'waterfall', barWidth: '60%',
            itemStyle: { color: 'transparent', borderColor: 'transparent' },
            emphasis: { disabled: true }, silent: true,
            tooltip: { show: false }, data: base },
          // No explicit borderRadius: bars follow the default/registered theme
          // like every other bar series (was a lone hardcoded override that made
          // the waterfall the only rounded chart).
          { name: 'delta', type: 'bar', stack: 'waterfall', barWidth: '60%',
            emphasis: { focus: 'self' }, data: delta }
        ]
      };
    }

    /** @param {any[]} facetData @param {any[]} groups @param {any[]} palette */
    _buildPie(facetData, groups, palette) {
      const gScale = this._scaleFor(this.config.group);
      const pieData = groups.map((g, i) => {
        const total = facetData.filter(a => a.group === g).reduce((s, a) => s + a.value, 0);
        return { name: g, value: total, itemStyle: { color: (gScale && gScale.color && gScale.color[g]) || palette[i % palette.length] } };
      }).filter(d => d.value > 0);
      return { ...(this.theme ? {} : { backgroundColor: 'transparent' }), textStyle: { fontFamily: BLOCKR_FONT }, toolbox: TOOLBOX, tooltip: { trigger: 'item', formatter: '{b}: {c} ({d}%)' }, series: [{ type: 'pie', radius: ['30%', '70%'], data: pieData, label: { show: true, fontSize: 10, formatter: '{b}' }, emphasis: { itemStyle: { shadowBlur: 10, shadowColor: 'rgba(0,0,0,0.2)' } } }] };
    }

    /** @param {any[]} facetData @param {any[]} groups @param {any[]} palette */
    _buildTreemap(facetData, groups, palette) {
      const gScale = this._scaleFor(this.config.group);
      const tmData = groups.map((g, i) => {
        const total = facetData.filter(a => a.group === g).reduce((s, a) => s + a.value, 0);
        return { name: g, value: total, itemStyle: { color: (gScale && gScale.color && gScale.color[g]) || palette[i % palette.length] } };
      }).filter(d => d.value > 0);
      return { ...(this.theme ? {} : { backgroundColor: 'transparent' }), textStyle: { fontFamily: BLOCKR_FONT }, toolbox: TOOLBOX, tooltip: { trigger: 'item', formatter: '{b}: {c}' }, series: [{ type: 'treemap', data: tmData, width: '100%', height: '100%', roam: false, nodeClick: false, breadcrumb: { show: false }, label: { show: true, fontSize: 12, formatter: '{b}\n{c}' }, itemStyle: { borderColor: '#fff', borderWidth: 2, gapWidth: 2 }, emphasis: { itemStyle: { shadowBlur: 10, shadowColor: 'rgba(0,0,0,0.15)' } } }] };
    }

    // Radar = an aggregated chart in polar coords: the group levels are the
    // spokes (indicators), each color level draws one shape, and each vertex
    // is agg_fn(metric) for that (group, color) cell — the same _aggregate()
    // output the bar chart stacks. Multi-column spokes (the blockr.echarts
    // radar's `metrics`) are expressed by pivoting longer upstream and
    // mapping the name column to `group`.
    /** @param {any[]} facetData @param {any[]} groups @param {any[]} colors @param {any[]} palette @param {any} valueTitle @param {number} plotW */
    _buildRadar(facetData, groups, colors, palette, valueTitle, plotW) {
      const colorScale = this._scaleFor(this.config.color);
      // One shared max across spokes keeps shapes comparable (same contract
      // as the blockr.echarts radar). Guard 0/negative-only data with 1.
      const maxVal = Math.max(...facetData.map(a => a.value), 0) || 1;
      const indicator = groups.map(g => ({ name: g, max: maxVal }));
      // A missing (group, color) cell is a true zero for counting
      // aggregations; for mean/median/min/max there is no value — null
      // leaves a gap instead of faking a 0.
      const zeroWhenMissing = ['count', 'count_distinct', 'sum']
        .includes(this.config.agg_fn);
      const gapVal = zeroWhenMissing ? 0 : null;
      /** @param {any} g @param {any} c */
      const cellVal = (g, c) => {
        const d = c == null
          ? facetData.find(a => a.group === g)
          : facetData.find(a => a.group === g && a.color === c);
        return d ? d.value : gapVal;
      };
      /** @param {any} name @param {any} vals @param {any} col */
      const mkShape = (name, vals, col) => ({
        name: name,
        value: vals,
        itemStyle: { color: col },
        lineStyle: { color: col, width: 2 },
        areaStyle: { color: col, opacity: 0.15 }
      });
      const data = colors.length === 0
        ? [mkShape(valueTitle, groups.map(g => cellVal(g, null)), palette[0])]
        : colors.map((c, ci) => mkShape(
            c, groups.map(g => cellVal(g, c)),
            (colorScale && colorScale.color && colorScale.color[c])
              || palette[ci % palette.length]));
      const showLegend = colors.length > 0;
      const leg = showLegend ? this._legendRows(colors, plotW) : { extra: 0, scroll: false };
      const rl = this._radarLayout(leg.extra, plotW, showLegend);
      return {
        __legendFit: showLegend
          ? { items: colors, radar: true, key: leg.extra + (leg.scroll ? 'S' : '') }
          : undefined,
        ...(this.theme ? {} : { backgroundColor: 'transparent' }),
        textStyle: { fontFamily: BLOCKR_FONT },
        toolbox: TOOLBOX,
        tooltip: {
          trigger: 'item',
          confine: true,
          formatter: (/** @type {any} */ p) => '<b>' + p.name + '</b><br>' +
            groups.map((g, i) => {
              const v = p.value ? p.value[i] : null;
              return g + ': ' + (v == null ? '–' : ddNum(v));
            }).join('<br>')
        },
        legend: showLegend
          ? { show: true, bottom: 0, textStyle: { fontSize: 11 }, ...(leg.scroll ? { type: 'scroll' } : {}) }
          : { show: false },
        radar: {
          indicator,
          radius: rl.radius,
          center: rl.center,
          axisName: {
            color: AXIS_LABEL_COLOR, fontSize: 11,
            overflow: 'truncate', width: 90
          },
          axisLine: { lineStyle: { color: AXIS_LINE_COLOR } },
          splitLine: { lineStyle: { color: SPLIT_LINE_COLOR } },
          splitArea: { show: false }
        },
        series: [{
          type: 'radar',
          data,
          symbol: 'circle',
          symbolSize: 4,
          emphasis: { focus: 'self' }
        }]
      };
    }

    /** @param {any[]} groups @param {any[]} palette @param {any} ax */
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
        const q = (/** @type {number} */ p) => { const i = p * (vals.length - 1); const lo = Math.floor(i); return lo === i ? vals[lo] : vals[lo] + (vals[lo + 1] - vals[lo]) * (i - lo); };
        const q1 = q(0.25), q3 = q(0.75), iqr = q3 - q1;
        const lo = Math.max(vals[0], q1 - 1.5 * iqr);
        const hi = Math.min(vals[vals.length - 1], q3 + 1.5 * iqr);
        // Datum object (not bare array) so the tooltip can report the
        // group's observation count alongside the five-number summary.
        return { value: [lo, q1, q(0.5), q3, hi], n: vals.length };
      });
      const gut = this._yGutter(groups);
      // p.data is our {value, n} datum; whiskers are 1.5*IQR-capped, so
      // they're labeled as whiskers rather than min/max.
      const boxTooltipFmt = (/** @type {any} */ p) => {
        const d = p.data;
        if (!d || !Array.isArray(d.value)) return '';
        // ECharts normalizes boxplot datum values to
        // [dataIndex, lo, q1, med, q3, hi] — read the last five.
        const [lo, q1, med, q3, hi] = d.value.slice(-5);
        return (p.name ? p.name + '<br/>' : '') +
          'n: ' + d.n +
          '<br/>Median: ' + ddNum(med) +
          '<br/>Q1, Q3: ' + ddNum(q1) + ', ' + ddNum(q3) +
          '<br/>Whiskers: ' + ddNum(lo) + ' \u2013 ' + ddNum(hi);
      };
      return { ...(this.theme ? {} : { backgroundColor: 'transparent' }), textStyle: { fontFamily: BLOCKR_FONT }, toolbox: TOOLBOX, tooltip: { trigger: 'item', confine: true, formatter: boxTooltipFmt }, grid: { left: gut.gridLeft, right: 5, top: 30, bottom: 46 }, xAxis: { type: 'value', name: this._axisTitle(this.config.metric), nameLocation: 'middle', nameGap: 30, nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize }, axisLabel: { color: ax.labelColor, fontSize: ax.fontSize }, axisLine: { lineStyle: { color: AXIS_LINE_COLOR } } }, yAxis: { type: 'category', data: groups, inverse: true, axisLabel: { color: ax.labelColor, fontSize: ax.fontSize, align: 'left', margin: gut.margin, width: gut.width, overflow: 'truncate', ellipsis: '\u2026' }, axisLine: { show: false } }, series: [{ type: 'boxplot', data: boxData, itemStyle: { color: palette[0] + '22', borderColor: palette[0] } }] };
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
      const colorScale = this._scaleFor(color);
      let seriesLevels = splitCol
        ? this._orderLevels(
            [...new Set(this.data.map(r => String(r[splitCol] ?? '')))],
            this._scaleFor(splitCol), splitCol)
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
      const colorForLevel = (/** @type {any} */ level, /** @type {number} */ index) => {
        if (!color) return palette[0];
        if (seriesCol && seriesCol !== color) {
          const rep = this.data.find(r => String(r[seriesCol]) === level);
          const cv = rep ? String(rep[color] ?? '') : '';
          if (colorScale && colorScale.color && colorScale.color[cv] != null) {
            return colorScale.color[cv];
          }
          if (!this._colorLookup) this._colorLookup = {};
          if (!(cv in this._colorLookup)) {
            this._colorLookup[cv] = palette[Object.keys(this._colorLookup).length % palette.length];
          }
          return this._colorLookup[cv];
        }
        if (colorScale && colorScale.color && colorScale.color[level] != null) {
          return colorScale.color[level];
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
        ? new Map(xCats.map((/** @type {any} */ c, /** @type {number} */ i) => [String(c), i]))
        : null;
      const sortLinePts = (/** @type {any[]} */ pts) => {
        if (!isLine) return;
        if (xOrder) {
          pts.sort((a, b) =>
            (xOrder.get(String(a[0])) ?? 0) - (xOrder.get(String(b[0])) ?? 0));
        } else if (xAxisType !== 'category') {
          pts.sort((a, b) => a[0] - b[0]);
        }
      };

      const encodeX = (/** @type {any} */ v) => xAxisType === 'category' ? String(v ?? '') : Number(v);
      const encodeY = (/** @type {any} */ v) => yAxisType === 'category' ? String(v ?? '') : Number(v);

      this.chartGrid.classList.toggle('dd-chart-grid-single', singleFacet);

      for (const facet of facets) {
        const rows = facet === '__all__' ? this.data : this.data.filter(r => String(r[facet]) === facet);

        /** @type {HTMLElement} */
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

        const mkSeries = (/** @type {any} */ name, /** @type {any} */ data, /** @type {any} */ color) => ({
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
        const smootherLine = (/** @type {any} */ groupName) => {
          if (smoother === 'none' || !smootherSeries) return null;
          const key = groupName != null ? String(groupName) : '__all__';
          const s = smootherSeries[key];
          if (!s || !s.x || !s.y) return null;
          /** @type {any[]} */
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
        const mkErrBarSeries = (/** @type {any} */ name, /** @type {any} */ errPts, /** @type {any} */ color) => ({
          type: 'custom',
          name: name,
          legendHoverLink: false,
          silent: true,
          z: 1,
          data: errPts,  // [[x, lo, hi], ...]
          renderItem: (/** @type {any} */ params, /** @type {any} */ api) => {
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

        /** @type {any[]} */
        const series = [];
        const pushOverlays = (/** @type {any} */ name, /** @type {any} */ pts, /** @type {any} */ color, /** @type {any} */ rawRows) => {
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
              .filter((/** @type {any} */ r) => r[x] != null && r[loCol] != null && r[hiCol] != null)
              .map((/** @type {any} */ r) => [encodeX(r[x]), Number(r[loCol]), Number(r[hiCol])]);
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
          /** @type {any[]} */
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
        /** @type {any[] | null} */
        let colorByLegendData = null;
        /** @type {Record<string, any[]> | null} */
        let seriesByColorByVal = null;
        if (useColorByLegend) {
          /** @type {Record<string, any>} */
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
        // Categorical x (visits): reuse the bar-chart label policy — every
        // label shown (no auto-interval decimation), horizontal when it
        // fits its per-category slot, else vertical with an ellipsis cap.
        // The x title drops below the rotated text via nameGap.
        const xlab = xCats
          ? this._xAxisLabels(xCats, chartDiv.clientWidth - 71)
          : null;
        const xAxisSpec = {
          type: xAxisType,
          name: this._axisTitle(x),
          nameLocation: 'middle',
          nameGap: xlab && xlab.bottom ? xlab.bottom + 16 : 28,
          nameTextStyle: { color: ax.labelColor, fontSize: ax.fontSize },
          axisLabel: xlab
            ? xlab.axisLabel
            : { color: ax.labelColor, fontSize: ax.fontSize },
          axisLine: { lineStyle: { color: AXIS_LINE_COLOR } },
          splitLine: { lineStyle: { color: ax.splitLineColor, type: 'dashed' } },
          scale: true
        };
        if (xCats) /** @type {any} */ (xAxisSpec).data = xCats;

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
        if (yCats) /** @type {any} */ (yAxisSpec).data = yCats;

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


        // Line charts hover per x-position (axis trigger): item trigger
        // only fires on symbols, and high series counts render with
        // symbol:'none' — no hover box at all. The formatter lists each
        // line's value at the hovered x, capped so a trajectory overlay
        // (hundreds of patients) doesn't produce an unreadable tower.
        // When the cursor is ON a line (series mouseover via
        // triggerLineEvent), the box narrows to that series only,
        // matching the emphasis/blur highlight.
        const TT_ROW_CAP = 12;
        const hover = { si: /** @type {number | null} */ (null) };
        const lineTooltip = {
          trigger: 'axis',
          axisPointer: { type: 'line' },
          confine: true,
          formatter: (/** @type {any} */ ps) => {
            if (!Array.isArray(ps)) ps = [ps];
            // Only real line series: drop error-bar overlays (custom) and
            // the empty legend-binding series (no data -> never present).
            let rows = ps.filter((/** @type {any} */ p) =>
              p && p.seriesType === 'line' && Array.isArray(p.value));
            if (!rows.length) return '';
            if (hover.si != null) {
              const own = rows.filter((/** @type {any} */ p) =>
                p.seriesIndex === hover.si);
              if (own.length) rows = own;
            }
            // One row per series: data with several observations per x
            // (replicates) yields one param per datum — keep the first.
            const seen = new Set();
            rows = rows.filter((/** @type {any} */ p) => {
              if (seen.has(p.seriesIndex)) return false;
              seen.add(p.seriesIndex);
              return true;
            });
            // Category x: the visit label is the header. Numeric x: name
            // the value ("Day: 30"), else it reads as a bare number.
            const head = xCats
              ? (rows[0].axisValueLabel ?? String(rows[0].value[0]))
              : this._axisTitle(x) + ': ' + ddNum(rows[0].value[0]);
            const lines = rows.slice(0, TT_ROW_CAP).map((/** @type {any} */ p) => {
              // Without a series split there is one unnamed series and
              // ECharts invents "series0" — label the y column instead.
              const nm = splitCol ? p.seriesName : this._axisTitle(y);
              return p.marker + nm + ': ' + ddNum(p.value[1]);
            });
            if (rows.length > TT_ROW_CAP) {
              lines.push('… +' + (rows.length - TT_ROW_CAP) + ' more');
            }
            return head + '<br/>' + lines.join('<br/>');
          }
        };
        // Legend entries are the color levels either way (explicit data when
        // series ≠ color, series names — one per color level — otherwise).
        const legendItems = useColorByLegend
          ? colorByLegendData
          : showLegend
            ? [...new Set(this.data.map(r => String(r[color] ?? '')))]
            : null;
        const leg = legendItems
          ? this._legendRows(legendItems, chartDiv.clientWidth)
          : { extra: 0, scroll: false };
        /** @type {any} */ (chart).__legendFit = legendItems
          ? { items: legendItems,
              base: (showLegend ? 78 : 52) + (xlab ? xlab.bottom : 0),
              key: leg.extra + (leg.scroll ? 'S' : '') }
          : undefined;

        const option = {
          ...(this.theme ? {} : { backgroundColor: 'transparent' }),
          textStyle: { fontFamily: BLOCKR_FONT },
          tooltip: isLine
            ? lineTooltip
            : { trigger: 'item', formatter: (/** @type {any} */ p) => `${x}: ${ddNum(p.value[0])}<br>${y}: ${ddNum(p.value[1])}` + (p.seriesName ? `<br>${color || 'series'}: ${p.seriesName}` : ''), confine: true },
          // Always set explicitly; leaving legend undefined lets echarts
          // auto-render one per series, which eats the plot area when
          // series is high-cardinality (e.g. USUBJID).
          legend: useColorByLegend
            ? { show: true, bottom: 0, textStyle: { fontSize: 11 },
                data: colorByLegendData,
                ...(leg.scroll ? { type: 'scroll' } : {}) }
            : showLegend
              ? { show: true, bottom: 0, textStyle: { fontSize: 11 },
                  ...(leg.scroll ? { type: 'scroll' } : {}) }
              : { show: false },
          // left / bottom widened so the rotated Y title and the X title
          // (nameGap above) clear the tick labels and the legend; rotated
          // categorical x labels add their text height on top.
          grid: { left: 66, right: 5, top: 30, bottom: (showLegend ? 78 : 52) + leg.extra + (xlab ? xlab.bottom : 0) },
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

        // Track which line the cursor is on (triggerLineEvent makes bare
        // line segments fire series mouseover/mouseout) so the axis
        // tooltip above can narrow to the hovered series.
        if (isLine) {
          chart.on('mouseover', (/** @type {any} */ p) => {
            if (p.componentType === 'series' && p.seriesType === 'line') {
              hover.si = p.seriesIndex;
            }
          });
          chart.on('mouseout', (/** @type {any} */ p) => {
            if (p.componentType === 'series' && p.seriesType === 'line') {
              hover.si = null;
            }
          });
          chart.on('globalout', () => { hover.si = null; });
        }

        // When the legend shows color levels (not series), intercept
        // legend clicks and fan them out to every series that belongs to
        // the clicked color group. Otherwise a click on "F" would do
        // nothing (no series is named "F") and the user couldn't toggle.
        if (useColorByLegend && seriesByColorByVal) {
          chart.on('legendselectchanged', (/** @type {any} */ params) => {
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

      // Distinct color levels (scale/factor/alpha order). With one named
      // series per level below, echarts assigns a color to each series from
      // option.color — built per level when a board scale applies, palette
      // cycling otherwise.
      const colorScale = this._scaleFor(color);
      const colorLevels = color
        ? this._orderLevels(
            [...new Set(this.data.map(r => String(r[color] ?? '')))],
            colorScale, color)
        : [];

      // Convert an x-axis value to a numeric/time coord, or to a category
      // index when the axis is categorical.
      const xCoord = (/** @type {any} */ v, /** @type {any[]} */ cats) => {
        if (xAxisType === 'category') {
          const i = cats.indexOf(String(v ?? ''));
          return i < 0 ? 0 : i;
        }
        return Number(v);
      };

      // Sort helper — ascending min of the sort column per category
      const sortTerms = (/** @type {any[]} */ rows) => {
        const sb = sort_by || 'onset';
        if (sb === 'alpha') {
          return [...new Set(rows.map((/** @type {any} */ r) => String(r[y] ?? '')))]
            .sort((a, b) => a.localeCompare(b) * sortDir);
        }
        const sortCol = (sb === 'onset') ? x : sb;
        /** @type {Record<string, number>} */
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
        if (xCats) /** @type {any} */ (xAxisSpec).data = xCats;

        // Auto-legend: show whenever color is set. The legend is
        // scroll-type, so high cardinality (AETERM with 200+ values)
        // scrolls instead of being suppressed. Series below are split
        // per color level so echarts has a real series to bind each
        // legend chip to — without that, legend items never paint.
        const showLegend = !!color && colorLevels.length > 0;
        // Names only — echarts derives each chip's color from the matching
        // series via option.color cycling.
        const legendData = showLegend ? colorLevels.slice() : null;

        const renderItemFn = (/** @type {any} */ params, /** @type {any} */ api) => {
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
          /** @type {any[]} */
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
          /** @type {Map<any, any[]>} */
          const buckets = new Map(colorLevels.map((/** @type {any} */ lvl) => /** @type {[any, any[]]} */ ([lvl, []])));
          for (const d of barData) {
            const lvl = String(d.value[4] ?? '');
            if (buckets.has(lvl)) { const b = buckets.get(lvl); if (b) b.push(d); }
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

        const gut = this._yGutter(terms);
        const option = {
          ...(this.theme ? {} : { backgroundColor: 'transparent' }),
          color: (colorScale && colorScale.color && colorLevels.length)
            ? colorLevels.map((/** @type {any} */ lvl, /** @type {number} */ i) =>
                colorScale.color[lvl] || palette[i % palette.length])
            : palette,
          textStyle: { fontFamily: BLOCKR_FONT },
          tooltip: {
            trigger: 'item',
            confine: true,
            formatter: (/** @type {any} */ p) => {
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
          grid: { left: gut.gridLeft, right: 10, top: 20, bottom: showLegend ? 78 : 48 },
          xAxis: xAxisSpec,
          yAxis: {
            type: 'category',
            data: terms,
            inverse: true,
            axisLabel: {
              color: ax.labelColor, fontSize: ax.fontSize,
              align: 'left', margin: gut.margin, width: gut.width,
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
        const newSeries = option.series.map((/** @type {any} */ s) => {
          if (s.type === 'pie') {
            return { data: (s.data || []).map((/** @type {any} */ d) => d == null ? d : ({ ...d, itemStyle: { ...d.itemStyle, opacity: sel && d.name !== sel ? 0.2 : 1 } })) };
          }
          if (s.type === 'treemap') {
            return { data: (s.data || []).map((/** @type {any} */ d) => d == null ? d : ({ ...d, itemStyle: { ...d.itemStyle, opacity: sel && d.name !== sel ? 0.3 : 1 } })) };
          }
          if (s.type === 'radar') {
            // Dim the non-selected shapes (one data item per color level).
            return { data: (s.data || []).map((/** @type {any} */ d) => d == null ? d : ({
              ...d,
              lineStyle: { ...d.lineStyle, opacity: sel && d.name !== sel ? 0.15 : 1 },
              itemStyle: { ...d.itemStyle, opacity: sel && d.name !== sel ? 0.15 : 1 },
              areaStyle: { ...d.areaStyle, opacity: sel && d.name !== sel ? 0.04 : 0.15 }
            })) };
          }
          // Individual/timeline families use echarts emphasis/blur states
          // for highlighting, driven natively by hover. Skip the manual
          // category-mask path — it would mis-apply to non-category data
          // (USUBJID on a trajectory, term on a gantt).
          if (s.type === 'boxplot' || s.type === 'line' || s.type === 'scatter' || s.type === 'custom') {
            return {};
          }
          if (cats.length === 0) return {};
          const newData = (s.data || []).map((/** @type {any} */ v, /** @type {number} */ i) => {
            // typeof null === 'object', so guard v before reading v.value;
            // this series data legitimately contains null gap values for
            // missing groups (see _renderAggregated). Without the `v &&`
            // check, null.value throws and aborts the whole render.
            const isObj = (v && typeof v === 'object' && !Array.isArray(v));
            const val = isObj ? v.value : v;
            // Preserve any per-datum itemStyle (e.g. the waterfall delta's
            // sign color) and only layer the selection opacity on top. Also
            // carry the percent-mode `raw` value (used by the tooltip) — the
            // rebuild below would otherwise drop it on the first highlight pass.
            const baseStyle = isObj ? v.itemStyle : undefined;
            return {
              value: val,
              ...(isObj && v.raw != null ? { raw: v.raw } : {}),
              itemStyle: { ...baseStyle, opacity: sel ? (cats[i] === sel ? 1 : 0.15) : 1 }
            };
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
      // The band is in flow: opening is a class toggle. No positioning, no
      // scroll/resize listeners, nothing to clamp to the viewport.
      this.popoverEl.classList.add('blockr-settings--open');
      this._popoverOpen = true;
      this.gearBtn.classList.add('blockr-gear-active');
      this.gearBtn.setAttribute('aria-expanded', 'true');
      // The chart shares vertical space with the band now; re-measure.
      this._resizeCharts();
    }
    _closePopover() {
      this.popoverEl.classList.remove('blockr-settings--open');
      this._popoverOpen = false;
      this.gearBtn.classList.remove('blockr-gear-active');
      this.gearBtn.setAttribute('aria-expanded', 'false');
      this._resizeCharts();
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
    /** @param {any} [col] @param {any} [values] */
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
      // radar's clickable mark is the per-color shape, so its natural key is
      // the color column (no color mapping -> nothing to drill on).
      if (this.config.chart_type === 'radar') return this.config.color || null;
      if (fam === 'aggregated') return this.config.group || null;
      if (fam === 'timeline') return this.config.y || null;
      // individual: a series/color split is the natural categorical key (line
      // always; scatter when split). Without one, a scatter filters
      // geometrically (the selected point's x&y).
      return this.config.series || this.config.color || null;
    }

    // When drill is off the chart is a pure display — disable ECharts hover
    // emphasis on marks AND drop the pointer ("hand") cursor so there is no
    // interactive-looking effect on a chart that can't be clicked. ECharts
    // defaults series items to cursor:'pointer'; some series (line/scatter)
    // also set itemStyle.cursor explicitly, which overrides the series-level
    // cursor, so reset both. Mutates and returns the option for use inline at
    // setOption.
    /** @param {any} option */
    _applyDrillEmphasis(option) {
      if (this._drillState() === 'off' && option && Array.isArray(option.series)) {
        for (const s of option.series) {
          if (!s) continue;
          s.emphasis = { disabled: true };
          s.cursor = 'default';
          if (s.itemStyle) s.itemStyle.cursor = 'default';
          if (s.lineStyle) s.lineStyle.cursor = 'default';
        }
      }
      return option;
    }

    /** @param {any[]} rows */
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
    /** @param {any} xCol @param {any} yCol @param {any} xVal @param {any} yVal */
    _sendPointFilter(xCol, yCol, xVal, yVal) {
      if (!this.el.id) return;
      Shiny.setInputValue(this.el.id + '_action', {
        action: 'filter', filter_type: 'point',
        x_col: xCol, y_col: yCol, x_val: xVal, y_val: yVal
      }, { priority: 'event' });
    }

    /** @param {any} xRange @param {any} yRange */
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
        bar_mode: this.config.bar_mode || 'stacked',
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

    // Resize every chart, but skip when the container is hidden or has no
    // layout box. Dock panels are kept mounted (defaultRenderer "always") even
    // when off-screen, so resizing those canvases is pure waste; when the panel
    // is revealed its size changes and the ResizeObserver fires again.
    _resizeCharts() {
      if (!this.chartGrid || this.chartGrid.offsetParent === null) return;
      if (!this.chartGrid.clientWidth || !this.chartGrid.clientHeight) return;
      for (const c of this.charts) { c.resize(); this._refitLegend(c); }
    }

    // Re-fit the bottom-legend reservation after a container resize.
    // chart.resize() re-lays-out with the option BUILT AT THE OLD WIDTH, so a
    // legend that wrapped to 2 rows at build time can wrap to 4 once the dock
    // narrows the panel — climbing back into the axis title. Builders stash
    // the legend items + the one-row grid bottom on the instance
    // (__legendFit); this recomputes the wrap at the new width and merges the
    // corrected reservation in. Also covers charts built while hidden
    // (clientWidth 0 → one-row assumption): the reveal resize lands here.
    /** @param {any} chart */
    _refitLegend(chart) {
      const fit = chart.__legendFit;
      if (!fit) return;
      const leg = this._legendRows(fit.items, chart.getWidth());
      const key = leg.extra + (leg.scroll ? 'S' : '');
      if (fit.key === key) return;
      fit.key = key;
      const legendPatch = { type: leg.scroll ? 'scroll' : 'plain' };
      chart.setOption(fit.radar
        ? { legend: legendPatch,
            radar: this._radarLayout(leg.extra, chart.getWidth(), true) }
        : { legend: legendPatch, grid: { bottom: fit.base + leg.extra } });
    }

    _observeResize() {
      if (this._resizeObserver) this._resizeObserver.disconnect();
      // Coalesce bursts of resize ticks into one redraw per animation frame.
      // A dock relayout fires many size changes while it settles; without this
      // each tick triggers a full synchronous ECharts redraw per chart.
      this._resizeObserver = new ResizeObserver(() => {
        if (this._resizeRaf) return;
        this._resizeRaf = requestAnimationFrame(() => {
          this._resizeRaf = null;
          this._resizeCharts();
        });
      });
      this._resizeObserver.observe(this.chartGrid);
    }

    resize() { this._resizeCharts(); }
    dispose() {
      if (this._resizeObserver) this._resizeObserver.disconnect();
      if (this._resizeRaf) { cancelAnimationFrame(this._resizeRaf); this._resizeRaf = null; }
      for (const c of this.charts) c.dispose();
      this.charts = [];
      // The settings band lives inside the widget element, so it is torn
      // down with the card — no portaled popover or document-level
      // outside-click listener to clean up anymore.
    }
  }

  // -- Shiny binding ----------------------------------------------------------

  const binding = new Shiny.InputBinding();
  Object.assign(binding, {
    find: (/** @type {any} */ scope) => $(scope).find('.drilldown-chart-container'),
    getId: (/** @type {any} */ el) => el.id || null,
    getValue: () => null,
    subscribe: () => {},
    unsubscribe: () => {},
    initialize: (/** @type {any} */ el) => {
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

  Shiny.addCustomMessageHandler('drilldown-data', (/** @type {any} */ msg) => {
    const el = /** @type {any} */ (document.getElementById(msg.id));
    if (el?._block) {
      el._block.setData(msg.columns, msg.data, msg.config, msg.arguments);
    } else if (el) {
      el._pendingData = msg;
    } else {
      let n = 0;
      const t = setInterval(() => {
        n++;
        const el2 = /** @type {any} */ (document.getElementById(msg.id));
        if (el2?._block) { el2._block.setData(msg.columns, msg.data, msg.config, msg.arguments); clearInterval(t); }
        else if (el2) { el2._pendingData = msg; clearInterval(t); }
        if (n > 50) clearInterval(t);
      }, 100);
    }
  });


  Shiny.addCustomMessageHandler('drilldown-theme', (/** @type {any} */ msg) => {
    const el = /** @type {any} */ (document.getElementById(msg.id));
    if (el?._block) {
      el._block.setTheme(msg.theme);
    } else if (el) {
      el._pendingTheme = msg.theme;
    } else {
      let n = 0;
      const t = setInterval(() => {
        n++;
        const el2 = /** @type {any} */ (document.getElementById(msg.id));
        if (el2?._block) { el2._block.setTheme(msg.theme); clearInterval(t); }
        else if (el2) { el2._pendingTheme = msg.theme; clearInterval(t); }
        if (n > 50) clearInterval(t);
      }, 100);
    }
  });

})();
