// @ts-check
/**
 * Chart block v2: the gear's role spec.
 *
 * What the settings band offers per chart family: ROLES (one entry per
 * config key: its label, control kind and column filter) and FAMILY_ROLES
 * (which roles each family shows, in which section). The gear engine,
 * drilldown-config.js, renders from these; it is shared with the table,
 * tile and heatmap blocks and used as it is.
 *
 * Copied from chart.js, which keeps its own copy until v1 is deleted.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});
  const { DISTRIBUTION_TYPES } = NS;

  /** @type {Array<{value: string, label: string, center: string, range: string}>} */
  const SUMMARY_STATS = [
    { value: 'median_q1_q3', label: 'Median · Q1–Q3', center: 'Median', range: 'Q1–Q3' },
    { value: 'mean_sd',      label: 'Mean ± SD',           center: 'Mean',   range: '±1 SD' },
    { value: 'mean_2sd',     label: 'Mean ± 2 SD',         center: 'Mean',   range: '±2 SD' },
    { value: 'mean_se',      label: 'Mean ± SE',           center: 'Mean',   range: '±1 SE' },
    { value: 'p5_p95',       label: '5th–95th percentile', center: 'Median', range: 'P5–P95' },
    { value: 'p10_p90',      label: '10th–90th percentile', center: 'Median', range: 'P10–P90' },
    { value: 'min_max',      label: 'Min–Max',             center: 'Median', range: 'Min–Max' }
  ];
  // Whisker rules = the same vocabulary plus Tukey fences (whisker-only:
  // fences are defined off the data's quartiles, whatever the body shows).
  const WHISKER_STATS = [
    { value: 'tukey', label: 'Tukey (1.5×IQR)', center: '', range: '1.5×IQR' },
    ...SUMMARY_STATS
  ];
  /** @param {string} stat */
  const statMeta = (stat) =>
    WHISKER_STATS.find(s => s.value === stat) || WHISKER_STATS[0];

  // Subtle inline chart-type glyphs for the tile picker (design-system
  // type-picker proposal B; hand-drawn 14px, currentColor, dimmed via CSS
  // so the text label stays primary — no icon-font dependency).
  /** @type {Record<string, string>} */
  const TYPE_ICONS = {
    bar:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor">' +
      '<rect x="2" y="8" width="3" height="6"/><rect x="6.5" y="4" width="3" height="10"/>' +
      '<rect x="11" y="10" width="3" height="4"/></svg>',
    pie:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="1.4">' +
      '<circle cx="8" cy="8" r="6"/><path d="M8 8 V2 M8 8 L13 11"/></svg>',
    treemap:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor">' +
      '<rect x="2" y="2" width="7" height="12" rx="1"/><rect x="10" y="2" width="4" height="6" rx="1"/>' +
      '<rect x="10" y="9" width="4" height="5" rx="1"/></svg>',
    boxplot:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="1.4">' +
      '<rect x="4" y="5" width="8" height="6"/>' +
      '<path d="M4 8 h8 M8 2 v3 M8 11 v3 M6 2 h4 M6 14 h4"/></svg>',
    pointrange:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="1.4">' +
      '<path d="M5 3 v10 M3.5 3 h3 M3.5 13 h3 M11 5.5 v7 M9.5 5.5 h3 M9.5 12.5 h3"/>' +
      '<circle cx="5" cy="8" r="1.7" fill="currentColor" stroke="none"/>' +
      '<circle cx="11" cy="9" r="1.7" fill="currentColor" stroke="none"/></svg>',
    radar:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="1.2">' +
      '<path d="M8 2 L14 6.5 L11.7 13.5 L4.3 13.5 L2 6.5 Z"/>' +
      '<path d="M8 8 L8 2 M8 8 L14 6.5 M8 8 L11.7 13.5 M8 8 L4.3 13.5 M8 8 L2 6.5"/></svg>',
    scatter:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor">' +
      '<circle cx="4" cy="11" r="1.6"/><circle cx="8" cy="6" r="1.6"/>' +
      '<circle cx="12" cy="9" r="1.6"/><circle cx="13" cy="3" r="1.6"/></svg>',
    line:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="1.6" stroke-linecap="round">' +
      '<path d="M2 12 L6 7 L10 9 L14 3"/></svg>',
    band:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="1.3" stroke-linejoin="round">' +
      '<path d="M2 5 L6 3.5 L10 6 L14 4" opacity="0.55"/>' +
      '<path d="M2 11 L6 9.5 L10 12 L14 10.5" opacity="0.55"/>' +
      '<path d="M2 8 L6 6.5 L10 9 L14 7.2" stroke-width="1.8"/></svg>',
    gantt:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor">' +
      '<rect x="2" y="3" width="8" height="2.6" rx="1"/><rect x="5" y="6.8" width="9" height="2.6" rx="1"/>' +
      '<rect x="3" y="10.6" width="6" height="2.6" rx="1"/></svg>'
  };

  // Aggregation vocabulary + the group/value/func role triple + the
  // value-follows-agg reconcile now live in the shared drilldown-agg.js so the
  // table and tile render the identical control. Loaded before this file.
  const DAgg = /** @type {any} */ (
    (typeof Blockr !== 'undefined' && Blockr.DrilldownAgg) || window.DrilldownAgg);

  // ===== Role spec ==========================================================
  // The config popover is a pure function of these two structures plus the
  // current (columns, config). See blockr.design/open/block-config-ui.
  //
  // ROLES — keyed by the existing config key (no persisted-key renames).
  //   kind: 'column' | 'select' | 'segmented' | 'slider'
  //   colType: 'cat' (categorical or n_unique<=50) | 'num' | 'any' | 'none'
  //            (no data columns), or a function of the current config
  //   allowCount: prepend '.count' to the column picker (value); may also
  //               be a function of the current config
  //   maxUnique: cap a categorical picker (facet)
  //   pairedWith: render this role's control beside its pair in one row
  //   when: (cfg) => bool — hide the row while the rest of the config makes
  //         it inert (facet_scales without a facet)
  //   optionsBy / colTypeBy / phBy: per-family overrides (key = family)
  // Inlined here for v1; extract to a shared module when the table/ggplot
  // blocks adopt it (follow-up specs).
  const ROLES = {
    // group / value / func are shared with the table + tile so all three
    // render the identical aggregation control — see drilldown-agg.js.
    ...DAgg.aggRoles(),
    // `x` accepts any column, but the useful one differs by family, and the
    // picker cannot show that. It belongs in the placeholder (the empty slot's
    // voice), not in a help line beneath the control.
    x:      { label: (/** @type {any} */ cfg) =>
                (cfg.chart_type === 'band' ? 'Timeline' : 'X'),
              kind: 'column',
              // The band slides a window along its x, so a categorical column
              // (AVISIT, PARAM, ...) cannot work -- offering one only lets the
              // reader pick a mapping that renders nothing. Narrow the picker
              // instead; band_empty_reason() still covers a saved board whose
              // column changed type underneath it.
              colType: (/** @type {any} */ cfg) =>
                (cfg.chart_type === 'band' ? 'num' : 'any'),
              // A band slides a window along a numeric x, everything else
              // plots time, a measure, or a category on it.
              kinds: ['time', 'value', 'group'],
              phBy: { individual: 'numeric column…', timeline: 'time / sequence…' } },
    y:      { label: (/** @type {any} */ cfg) =>
                (cfg.chart_type === 'band' ? 'Value' : 'Y'),
              // `id` for the timeline family, whose y IS the lane (one row
              // per subject); a value everywhere else.
              kind: 'column', colTypeBy: { individual: 'num', timeline: 'any' },
              kinds: ['value', 'id'] },
    xend:   { label: 'X end',  kind: 'column', colType: 'any', kinds: ['time', 'value'],
              ph: 'interval end…' },
    series: { label: 'Series', kind: 'column', colType: 'any', kinds: ['id', 'group'] },
    color:  { label: 'Color',  kind: 'column', colType: 'any', kinds: ['group'] },
    facet:  { label: 'Facet',  kind: 'column', colType: 'cat', maxUnique: 10,
              kinds: ['group'] },
    // `drill` is a capability, not an aesthetic: it names whatever column a
    // click should filter downstream on, which is a board wiring decision and
    // not something the marks can predict. Left unnarrowed on purpose.
    drill:  { label: 'Drill',  kind: 'column', colType: 'any' },
    label:  { label: 'Label',  kind: 'column', colType: 'any', kinds: ['group', 'id'] },
    // Extra columns to append to each mark's hover tooltip (beyond the mapped
    // roles). Multi-select; empty = none. Values are shipped and packed
    // per-bar, so this is bounded to the picked columns, never the whole row.
    tt_fields: { label: 'Tooltip fields', kind: 'columns', colType: 'any',
                 placeholder: 'add columns…' },
    // Sort keys keep their internal values ('value' / 'alpha' / 'onset' —
    // saved state, R round-trip) but display human labels; bare internals
    // in the picker read as jargon. '#num' expands to numeric columns,
    // which label themselves (name + variable label).
    sort_by:  { label: 'Sort', kind: 'select',
                optionsBy: {
                  aggregated: [
                    { value: 'value', label: 'By value' },
                    // Factor levels, else first-appearance in the rows — the
                    // order the data itself carries (visits, dose groups).
                    { value: 'data', label: 'Data order' },
                    { value: 'alpha', label: 'A–Z' },
                    '#num'
                  ],
                  timeline: [
                    { value: 'onset', label: 'By onset' },
                    { value: 'alpha', label: 'A–Z' },
                    '#num'
                  ]
                } },
    // Two values, so a segmented control beside Sort (design system,
    // "Segmented control"). Descending leads, as the mock-up draws it; an
    // unset order sorts ascending, which `dflt` says.
    sort_dir: { label: 'Order', kind: 'segmented', dflt: 'asc',
                options: [{ value: 'desc', label: 'Descending' },
                          { value: 'asc',  label: 'Ascending' }] },
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
    // Bar value labels (bar + waterfall). Boolean on/off segmented -> a
    // checkbox, "on"/"off" wire format like identity_line below; R stores a
    // logical (bool_state). Drawn by _barValueLabels.
    value_labels: { label: 'Values', kind: 'segmented',
                    options: [{ value: 'on', label: 'Show values' },
                              { value: 'off', label: 'Off' }] },
    // A waterfall is a bar with a cumulative baseline — exposed here as a bar
    // option, not its own chart_type. "Waterfall" sets baseline='cumulative'.
    baseline: { label: 'Bars', kind: 'segmented',
                options: [{ value: 'zero', label: 'Standard' },
                          { value: 'cumulative', label: 'Waterfall' }] },
    smoother: { label: 'Smoother', kind: 'select', options: ['none', 'lm', 'loess'] },
    // Helper lines. `identity_line` is the computed diagonal; x_lines /
    // value_lines are fixed positions the user types. All three draw a dashed guide, so the
    // gear groups them in one "Helper lines" section (see SECTIONS below).
    //
    // Boolean on/off segmented -> rendered as a checkbox (see _isBoolSegmented).
    // The "on"/"off" strings are THIS CONTROL's wire format, not the block's
    // API: R stores a logical and converts at the boundary (bool_state), the
    // same split the table block's sortable/collapsible already use.
    identity_line: { label: 'Identity', kind: 'segmented',
                     options: [{ value: 'on', label: 'Identity line (y = x)' },
                               { value: 'off', label: 'Off' }] },
    // Free numeric entry: one line per number, comma-separated. `text` (not a
    // bespoke numeric kind) because the engine's text control already has the
    // commit model we want (Enter/blur to apply, Escape to revert) and R parses
    // the string with num_vec_state() -- which drops junk, so a typo yields no
    // line rather than a line at NA.
    // `slot: 'entry'`: named in the block's sentence, the word is typed over
    // in place (a line at 32.5 is not in any list). See SentenceSlots._edit.
    x_lines: { label: 'Vertical', kind: 'text', ph: 'e.g. 3 or 2, 5', slot: 'entry' },
    // Across the value axis. On scatter/line that is y, so the pair reads
    // Vertical / Horizontal; on the aggregated family it follows
    // `orientation` and is the only helper line, so it is named by its job.
    value_lines: { label: (/** @type {any} */ cfg) =>
                     (cfg.chart_type === 'scatter' || cfg.chart_type === 'line'
                       ? 'Horizontal' : 'Reference lines'),
                   kind: 'text', ph: 'e.g. 2 or 1, 4', slot: 'entry' },
    // Boxplot observation overlay. "none" = box only; "outliers" = only the
    // points beyond the whisker extent. (A former "all" strip was dropped:
    // on a category axis every point sat on the box's centre line, an opaque
    // smear at clinical N that stopped discriminating — saved boards carrying
    // it degrade to "none".)
    box_points: { label: 'Points', kind: 'segmented',
                  options: [{ value: 'none', label: 'None' },
                            { value: 'outliers', label: 'Outliers' }] },
    // Distribution statistic (boxplot + pointrange, browser-computed from the
    // raw value column). One shared vocabulary, picked once or twice: the
    // point range's single interval, or the box's body — with `whiskers` as
    // the box's second, outer pick. Defaults per mark (mean_se / Tukey box)
    // resolve in _ensureDistributionMetric, so the roles carry none.
    summary: { label: (/** @type {any} */ cfg) =>
                 cfg.chart_type === 'boxplot' ? 'Box' : 'Interval',
               kind: 'select',
               options: SUMMARY_STATS.map(s => ({ value: s.value, label: s.label })) },
    whiskers: { label: 'Whiskers', kind: 'select',
                options: WHISKER_STATS.map(s => ({ value: s.value, label: s.label })) },
    // Line through the point-range centers in slot order (the over-visits
    // reading). Boolean segmented -> checkbox, "on"/"off" wire format like
    // identity_line; R stores a logical (bool_state).
    connect_centers: { label: 'Centers', kind: 'segmented',
                       options: [{ value: 'on', label: 'Connect centers' },
                                 { value: 'off', label: 'Off' }] },
    // How the line connects its points (line charts only) -- one control that
    // replaces the old separate `step` + `smooth` selects, since they were two
    // ways of saying the same thing (how to get from point to point) and
    // "straight" appeared in both. `monotone` (default) draws monotone-smoothed
    // lines (smoothMonotone 'x': no overshoot, so the curve never implies values
    // outside the measured range) at every density; `straight` draws plain
    // segments; `step-start`/`step-middle`/`step-end` hold the value between
    // observations and place the vertical jump along the interval (dose levels,
    // on/off states). Legacy boards that saved `step` + `smooth` map to this on
    // construction (see new_chart_block).
    connect: { label: 'Line', kind: 'select', ph: 'Monotone',
               options: [{ value: 'monotone',    label: 'Monotone' },
                         { value: 'straight',    label: 'Straight' },
                         { value: 'step-start',  label: 'Step at start' },
                         { value: 'step-middle', label: 'Step at middle' },
                         { value: 'step-end',    label: 'Step at end' }] },
    // Observation counts appended to labels ("Female (12)"). `count_on` picks
    // the surface: the category axis, the facet strip, or both. `count_col` is
    // the id column counted DISTINCT per label group (blank = raw row count) —
    // distinct counts CANNOT sum from the per-cell n (a subject can appear in
    // several colour/facet cells), so _labelCounts recomputes over the raw
    // rows. `rerender:true` so clearing count_on hides the id picker live.
    count_on: { label: 'Group counts', kind: 'select', rerender: true,
                options: [{ value: 'off',   label: 'Off' },
                          { value: 'axis',  label: 'On category axis' },
                          { value: 'facet', label: 'On facet labels' },
                          { value: 'both',  label: 'On axis + facets' }] },
    // Aggregating funcs count DISTINCT ids; identity ("None (as is)") plots
    // pre-summarised rows, so the picked column is shown AS-IS (the group's
    // row value) — relabel accordingly. Blank = row count either way.
    count_col: { label: (/** @type {any} */ cfg) =>
                   cfg.func === 'identity' ? 'Count column' : 'Count distinct of',
                 kind: 'column', colType: 'any',
                 ph: 'column to count (blank = row count)' },
    // Panel scales for a facet grid. "fixed" (the default) gives every panel
    // ONE numeric domain and one category set/order, so a position means the
    // same thing in every panel — the premise of small multiples, and what
    // static_chart()'s facet_wrap() has always done. "free" lets each panel
    // size itself off its own subset (ggplot's free-scale semantics, which
    // also drop unused discrete levels): the honest reading when the panels
    // do not share a unit, e.g. faceting by PARAM, where ALT, bilirubin and
    // heart rate cannot meaningfully share a value axis.
    //
    // Only the individual family offers "free_y". Elsewhere the value axis
    // follows `orientation`, so a "free y" that silently meant "free x" on a
    // horizontal bar would be a lie — those families get fixed/free, and a
    // saved free_y reads as free (see _facetScales).
    //
    // `when` hides the control until a facet column is mapped: with one panel
    // there is nothing to share a scale WITH.
    // One toggle for downloads, the same decision the table and summarize
    // blocks take: "can people take this chart away" is one question, and
    // which file the reader wants is theirs.
    download: { label: 'Download', kind: 'segmented', options: [
                  { value: 'on', label: 'Downloads' },
                  { value: 'off', label: 'No downloads' }] },
    facet_scales: { label: 'Panel scales', kind: 'select',
                    when: (/** @type {any} */ cfg) => !!cfg.facet,
                    optionsBy: {
                      aggregated: [
                        { value: 'fixed', label: 'Shared across panels' },
                        { value: 'free',  label: 'Free per panel' }],
                      individual: [
                        { value: 'fixed',  label: 'Shared across panels' },
                        { value: 'free_y', label: 'Free y per panel' },
                        { value: 'free',   label: 'Free x and y per panel' }],
                      timeline: [
                        { value: 'fixed', label: 'Shared across panels' },
                        { value: 'free',  label: 'Free per panel' }]
                    } },
    // Panels per row in the facet grid. '' (the default) is auto: the
    // stylesheet's `repeat(auto-fit, minmax(300px, 1fr))` fits as many panels
    // per row as the card is wide, which is what a faceted chart has always
    // done. A number pins the row, and the SAME number reaches
    // facet_wrap(ncol = ) in the report / deck render, where ggplot picks a
    // square-ish grid of its own — so the picture on screen and the one in
    // the deck stop disagreeing about their shape.
    //
    // Labelled "Panel columns", never "Columns": everywhere else in this gear
    // a column is a column of the DATA.
    //
    // `when` hides it until a facet is mapped, like facet_scales: one panel
    // has no row to lay out.
    //
    // '' is not a picked value, so the closed control shows the PLACEHOLDER,
    // which is where auto has to be said (`ph`) — a select that renders blank
    // reads as a setting that failed to load. The list still carries the
    // Auto row, because going back to auto is a pick like any other.
    facet_cols: { label: 'Panel columns', kind: 'select',
                  when: (/** @type {any} */ cfg) => !!cfg.facet,
                  ph: 'Auto (fits the width)',
                  // A fixed set shows its labels alone, so each label is the
                  // whole reading.
                  options: [{ value: '',  label: 'Auto (fits the width)' },
                            { value: '1', label: '1 per row' },
                            { value: '2', label: '2 per row' },
                            { value: '3', label: '3 per row' },
                            { value: '4', label: '4 per row' },
                            { value: '6', label: '6 per row' }] },
    // Distribution band. `band_window` picks how the window is sized;
    // `band_size` reads as subjects (adaptive) or x units (fixed), so its
    // label follows the mode rather than lying in one of them.
    band_window: { label: 'Window', kind: 'select', rerender: true,
                   options: [{ value: 'adaptive', label: 'Adaptive (by subjects)' },
                             { value: 'fixed', label: 'Fixed (x units)' }] },
    band_size: { label: (/** @type {any} */ cfg) =>
                   (cfg.band_window === 'fixed' ? 'Half-width (x units)'
                                                : 'Subjects per window'),
                 // `label` may be a function (see count_col); `ph` may not --
                 // drilldown-config.js assigns role.ph straight to
                 // input.placeholder, so a function would print its source.
                 kind: 'text', ph: 'e.g. 45' },
    band_min_n: { label: 'Cut below n', kind: 'text', ph: 'e.g. 12' },
    band_id: { label: 'Subject id', kind: 'column', colType: 'any',
               ph: 'id counted distinct (blank = rows)' },
    ref_hi: { label: 'Upper limit', kind: 'column', colType: 'num',
              ph: 'column, e.g. ANRHI (blank = none)' },
    ref_lo: { label: 'Lower limit', kind: 'column', colType: 'num',
              ph: 'column, e.g. ANRLO (blank = none)' },
    lo:       { label: 'Lo', kind: 'column', colType: 'num' },
    hi:       { label: 'Hi', kind: 'column', colType: 'num' },
    line_width_mult: { label: 'Line width', kind: 'slider' },
    dot_size_mult:   { label: 'Dot size',   kind: 'slider' },
    // Chart text (three-tier contract, R side R/title-template.R): null =
    // auto — the title inherits the data frame's label attribute; "" =
    // explicitly none; other text renders with {...} tokens resolved against
    // the current data BY R ({ARM}, {label(value)}, {n}, {n_distinct(col)},
    // and {filters} for what the upstream filters applied).
    // `autoValue` surfaces the inherited auto title as the input's value, so
    // clearing the field is how the auto title is turned OFF (commits "").
    title:    { label: 'Title', kind: 'text', ph: 'e.g. AEs by {ARM}',
                autoValue: (/** @type {any} */ cfg) =>
                  (cfg.title == null && cfg.title_resolved) ? cfg.title_resolved : '' },
    subtitle: { label: 'Subtitle', kind: 'text', ph: 'e.g. {func} of {label(@y)}[, by {@color}]',
                hint: 'A setting becomes a control on the block. [ ] drops its clause when the setting is empty.',
                autoValue: (/** @type {any} */ cfg) =>
                  (cfg.subtitle == null && cfg.subtitle_resolved) ? cfg.subtitle_resolved : '' },
    caption:  { label: 'Caption', kind: 'text', ph: 'e.g. {filters} or N = {n} records',
                autoValue: (/** @type {any} */ cfg) =>
                  (cfg.caption == null && cfg.caption_resolved) ? cfg.caption_resolved : '' }
  };

  // The value is one shared slot across every chart: the numeric variable the
  // chart is about. Bar/pie/etc. wrap it in an aggregation, so it reads as part
  // of "mean of AGE" under the Aggregation section. The distribution marks do
  // NOT aggregate — they show the raw distribution of that same variable — so
  // there the very same slot is just the "Value" to plot. Same argument,
  // different framing. (Label is a function of config; the engine resolves it.)
  // "% of panel" counts the distinct values of any column, as "Count
  // distinct" does, so its picker offers every column, the current value
  // column included (B6).
  ROLES.value = /** @type {any} */ ({
    ...ROLES.value,
    label: (/** @type {any} */ cfg) =>
      DISTRIBUTION_TYPES.includes(cfg.chart_type) ? 'Value' : 'Aggregate',
    colType: (/** @type {any} */ cfg) =>
      (cfg.func === 'count_distinct' || cfg.func === 'pct_distinct') ? 'any'
        : (!cfg.func || cfg.func === 'count') ? 'none' : 'num'
  });

  // "None (as is)" — a chart-only aggregation that plots the value column
  // directly (one bar per row when the category is unique), for data whose
  // bar heights are already computed upstream (a summary table, a precomputed
  // metric). Appended HERE, to the chart's func picker, rather than to the
  // shared DrilldownAgg.AGG_FNS: the chart aggregates client-side, so identity
  // can live entirely on this side and leave the table/tile pickers — and the
  // R AGG_FNS drift + golden cross-tests — untouched. The engine branch that
  // reads it is in drilldown-agg.js aggregate() (dormant for table/tile, which
  // never send func='identity').
  // "% of panel" — the share of the panel's distinct values, not of the bar
  // (bar_mode 'percent' already does that) and not of the grid. Chart-only for
  // the same reason as identity: the denominator is a FACET population, which
  // a table has no equivalent of, so pinning its meaning there wants composer's
  // make_denom as the reference rather than an invention. Engine branch lives
  // in drilldown-agg.js aggregate().
  ROLES.func = /** @type {any} */ ({
    ...ROLES.func,
    options: [...ROLES.func.options,
              { value: 'pct_distinct', label: '% of panel' },
              { value: 'identity', label: 'None (as is)' }]
  });

  // Missing group keys. Sits in Mapping, after Group and before the
  // aggregation, because it decides which rows are counted: with
  // pct_distinct, "drop" is what lets a row count toward the denominator
  // without drawing a bar.
  // Which roles a pct_distinct denominator is taken WITHIN. The chart cannot
  // infer this: it would have to know whether a column is a population split
  // (arm, sex, country) or an event attribute (grade), and dividing grade-2
  // subjects by subjects-with-grade-2 is circular. Only the three mapped roles
  // are offered, because a cell knows no other coordinate.
  // A single role here, not a combination: the gear has no multi-select kind,
  // and no case so far needs one (facet+group is the cell itself, i.e. always
  // 100%). The ENGINE takes a vector, so widening this to combinations later
  // is a control change and not a data one.
  ROLES.pct_of = /** @type {any} */ ({
    label: 'Percent within', kind: 'select',
    when: (/** @type {any} */ cfg) => cfg.func === 'pct_distinct',
    options: [{ value: 'facet', label: 'Facet' },
              { value: 'group', label: 'Group' },
              { value: 'color', label: 'Colour' }]
  });

  ROLES.na_group = /** @type {any} */ ({
    label: 'Missing group', kind: 'select',
    options: [{ value: 'level', label: 'Own category' },
              { value: 'drop',  label: 'Not a category' }]
  });

  // FAMILY_ROLES — per family, ordered. A section entry is either a role key
  // (always shown for the family) or { role, types:[...] } (shown only for
  // those chart types). requiredMap rows render immediately; optionalMap rows
  // are added on demand from the "+ Add mapping" menu. `mapping` holds always-on
  // controls, drawn under Mapping after the required rows. For the aggregated
  // family that is what decides which rows are counted and how: the missing
  // group, the aggregation (`value` paired with `func`, drawn as "Aggregate"
  // and "Of") and the percent denominator. There is no separate Aggregation
  // section (mock-up: blockr.design open/summarize-table/mock-summary-mark/
  // chart.html, B and E).
  const FAMILY_ROLES = {
    aggregated: {
      requiredMap: ['group'],
      // `label` (on-mark text) is timeline-only: only the gantt custom
      // renderer consumes it. Not offered here — pie/treemap label their
      // marks by group automatically. `color` is offered only where the
      // renderer consumes it: pie/treemap/waterfall draw ONE measure per
      // group (their builders sum across color cells — the mapping would be
      // inert and misleading).
      optionalMap: [
        { role: 'color', types: ['bar', 'boxplot', 'pointrange', 'radar'] },
        'facet',
        // Bar only: a bar is a GROUP of rows, so an extra tooltip column has no
        // single value — the bar tooltip shows the group's representative (its
        // first row). Unambiguous for a "None (as is)" bar (one row per bar);
        // for a real aggregation it is a representative, not a per-row value.
        // Offered only on bar because only the bar tooltip formatter consumes
        // it (pie/treemap/boxplot/radar/waterfall have their own).
        { role: 'tt_fields', types: ['bar'] }
      ],
      // Missing group sits next to Group: it decides whether rows without a
      // group are counted as their own category. Bar only, like the percent
      // denominator after the aggregation: both exist for the
      // population-as-rows pattern, which is a bar idea (a pie of "everyone,
      // including the ones with no category" is not a chart anyone wants).
      mapping: [{ role: 'na_group', types: ['bar'] },
        'value', { role: 'func', types: ['bar', 'waterfall', 'pie', 'treemap', 'radar'] },
        { role: 'pct_of', types: ['bar'] }],
      // orientation: bar + the distribution marks (which default vertical —
      // groups on x — while bar defaults horizontal; see
      // _ensureDistributionMetric). Waterfall is vertical-only (a bridge
      // reads left-to-right along the value axis), so it does not expose
      // orientation.
      presentation: ['sort_by', 'sort_dir',
        { role: 'orientation', types: ['bar', 'boxplot', 'pointrange'] },
        { role: 'bar_mode', types: ['bar'] },
        { role: 'value_labels', types: ['bar', 'waterfall'] },
        { role: 'baseline', types: ['bar'] },
        // Distribution statistics: the shared interval pick (box body /
        // point-range interval), the box's outer whisker rule, and the
        // point-range center line. See SUMMARY_STATS.
        { role: 'summary', types: ['boxplot', 'pointrange'] },
        { role: 'whiskers', types: ['boxplot'] },
        { role: 'connect_centers', types: ['pointrange'] },
        { role: 'box_points', types: ['boxplot'] },
        // Across the value axis, whichever way the chart lies.
        { role: 'value_lines', types: ['bar', 'waterfall', 'boxplot', 'pointrange'] },
        // Count labels: the axis surface applies to the category-axis charts
        // (bar and waterfall per group/step; boxplot per drawn box slot);
        // facet applies to any faceted family. Pie/treemap/radar have no
        // category axis, so "axis" no-ops there (the tooltip n covers it).
        'count_on', 'count_col',
        // Facet-grid shape; both hidden until a facet is mapped (role
        // `when`).
        'facet_scales', 'facet_cols', 'download'],
      titles: ['title', 'subtitle', 'caption']
    },
    individual: {
      requiredMap: ['x', 'y'],
      // `label` is timeline-only (gantt on-bar text); scatter/line have no
      // on-mark text renderer. `tt_fields` surfaces extra columns on hover:
      // one row per mark (scatter) / per series at the hovered x (line), so
      // each value is unambiguous (unlike an aggregated bar, a group of rows).
      optionalMap: ['series', 'color', 'facet', 'tt_fields'],
      mapping: [],
      presentation: [
        { role: 'smoother', types: ['scatter'] },
        // Band: the window is the only new idea; `summary` / `whiskers`
        // are the SAME distribution vocabulary the boxplot uses, so the
        // two marks cannot disagree about what an interval means.
        { role: 'band_window', types: ['band'] },
        { role: 'band_size', types: ['band'] },
        { role: 'band_min_n', types: ['band'] },
        { role: 'band_id', types: ['band'] },
        { role: 'summary', types: ['band'] },
        { role: 'whiskers', types: ['band'] },
        { role: 'box_points', types: ['band'] },
        { role: 'ref_hi', types: ['band'] },
        { role: 'ref_lo', types: ['band'] },
        // Helper lines, kept adjacent so they read as one group: the computed
        // diagonal, then the fixed positions. They are NOT a titled section --
        // `presentation` is a flat list and a real header needs a change to the
        // shared engine (drilldown-config.js), which is a follow-up.
        // x_lines/value_lines are offered on line charts too: a normal-range
        // or threshold marker on a trajectory is the same need as on a scatter.
        { role: 'identity_line', types: ['scatter'] },
        { role: 'x_lines', types: ['scatter', 'line'] },
        { role: 'value_lines', types: ['scatter', 'line'] },
        // Line-connect mode (straight / monotone / step-*): line-only, so it
        // sits with the other line presentation options.
        { role: 'connect', types: ['line'] },
        { role: 'lo', types: ['line'] },
        { role: 'hi', types: ['line'] },
        'line_width_mult', 'dot_size_mult',
        // Count labels: scatter/line have numeric axes, so only facet counts
        // apply here (the "axis" choice no-ops); shown for faceted charts.
        'count_on', 'count_col',
        // Facet-grid shape; both hidden until a facet is mapped (role
        // `when`).
        'facet_scales', 'facet_cols', 'download'
      ],
      titles: ['title', 'subtitle', 'caption']
    },
    timeline: {
      requiredMap: ['x', 'xend', 'y'],
      optionalMap: ['series', 'color', 'facet', 'label', 'tt_fields'],
      mapping: [],
      // Count labels: "axis" counts events (or distinct count_col) per lane.
      presentation: ['sort_by', 'sort_dir', 'count_on', 'count_col',
        // Facet-grid shape; both hidden until a facet is mapped (role
        // `when`).
        'facet_scales', 'facet_cols', 'download'],
      titles: ['title', 'subtitle', 'caption']
    }
  };

  // Roles whose control renders inside its primary's row (the paired tail), so
  // a section loop skips them. Passed to the shared config engine.
  const DD_SECONDARY = new Set(
    Object.values(ROLES).map(r => /** @type {any} */ (r).pairedWith).filter(Boolean));

  NS.roles = { ROLES, FAMILY_ROLES, DD_SECONDARY, TYPE_ICONS, SUMMARY_STATS, WHISKER_STATS,
               statMeta, DAgg };
})();
