// @ts-check
/**
 * Chart block: the model rules every family shares.
 *
 * The defaults a config gets when R sends it or when the gear switches the
 * chart type, and the gate in front of every draw: the empty states for no
 * rows, an unset required role, too many colour levels and a mapped column
 * the rows do not carry.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const NS = /** @type {any} */ (B.chart = B.chart || {});
  const model = NS.model = NS.model || {};

  /**
   * Box and point range need a numeric value and never the row count, and
   * default to vertical, data order. True when `cfg` is one of them.
   * @param {Record<string, any>} cfg
   */
  const ensureDistributionMetric = (cfg) => {
    if (!NS.DISTRIBUTION_TYPES.includes(cfg.chart_type)) return false;
    if (!cfg.func || cfg.func === 'count') cfg.func = 'mean';
    // An empty "Value *" asks for a real choice; the first numeric column is
    // often a code that plots as a flat line.
    if (cfg.value === '.count') cfg.value = '';
    if (!cfg.summary) cfg.summary = cfg.chart_type === 'boxplot' ? 'median_q1_q3' : 'mean_se';
    if (cfg.chart_type === 'boxplot' && !cfg.whiskers) cfg.whiskers = 'tukey';
    if (!cfg.orientation) cfg.orientation = 'vertical';
    if (!cfg.sort_by) cfg.sort_by = 'data';
    if (!cfg.sort_dir) cfg.sort_dir = 'asc';
    return true;
  };

  /**
   * The defaults a config from R gets before the first draw: the mapping
   * columns a family cannot draw without, and its sort. Mutates `cfg`.
   * @param {Record<string, any>} cfg @param {VizColumn[]} columns
   */
  const defaults = (cfg, columns) => {
    if (!cfg.chart_type) cfg.chart_type = 'bar';
    const fam = NS.familyOf(cfg.chart_type);
    if (fam === 'aggregated') {
      if (!cfg.group && columns.length > 0) {
        const cat = columns.find((/** @type {any} */ c) => c.type === 'categorical' && c.n_unique <= 30);
        cfg.group = cat ? cat.name : columns[0].name;
      }
      if (!ensureDistributionMetric(cfg)) {
        if (!cfg.func) cfg.func = 'count';
        if (!cfg.value && cfg.func === 'count') cfg.value = '.count';
      }
      if (!cfg.sort_by) cfg.sort_by = 'value';
      if (!cfg.sort_dir) cfg.sort_dir = 'desc';
    } else if (fam === 'timeline') {
      if (!cfg.x && columns.length > 0) {
        const num = columns.find((c) => c.type === 'numeric');
        cfg.x = num ? num.name : columns[0].name;
      }
      if (!cfg.y) {
        const cat = columns.find((/** @type {any} */ c) => c.type === 'categorical' && c.n_unique > 1);
        cfg.y = cat ? cat.name : columns[0]?.name;
      }
      if (!cfg.sort_by) cfg.sort_by = 'onset';
      if (!cfg.sort_dir) cfg.sort_dir = 'asc';
    } else {
      if (!cfg.x && columns.length > 0) {
        const num = columns.find((c) => c.type === 'numeric');
        cfg.x = num ? num.name : columns[0].name;
      }
      if (!cfg.y) {
        const nums = columns.filter((c) => c.type === 'numeric');
        const other = nums.find((c) => c.name !== cfg.x);
        cfg.y = other ? other.name : (nums[0] ? nums[0].name : columns[0]?.name);
      }
    }
  };

  /**
   * The defaults after the gear switched the chart type, so R learns the
   * new mapping (and ships its columns) with the config it is sent next.
   * Mutates `cfg`.
   * @param {Record<string, any>} cfg @param {VizColumn[]} columns
   */
  const familyDefaults = (cfg, columns) => {
    const fam = NS.familyOf(cfg.chart_type);
    const cols = columns || [];
    const has = NS.hasVal;
    if (fam === 'aggregated') {
      if (!has(cfg.group) && cols.length) {
        const cat = cols.find((/** @type {any} */ c) => c.type === 'categorical' && c.n_unique <= 30);
        cfg.group = cat ? cat.name : cols[0].name;
      }
      if (!ensureDistributionMetric(cfg)) {
        if (!cfg.func) cfg.func = 'count';
        if (!cfg.value && cfg.func === 'count') cfg.value = '.count';
      }
      if (!has(cfg.sort_by)) cfg.sort_by = 'value';
      if (!has(cfg.sort_dir)) cfg.sort_dir = 'desc';
      if (!has(cfg.orientation)) cfg.orientation = 'horizontal';
      if (!has(cfg.bar_mode)) cfg.bar_mode = 'stacked';
    } else if (fam === 'timeline') {
      if (!has(cfg.x) && cols.length) {
        const num = cols.find((c) => c.type === 'numeric');
        cfg.x = num ? num.name : cols[0].name;
      }
      if (!has(cfg.y) && cols.length) {
        const cat = cols.find((/** @type {any} */ c) => c.type === 'categorical' && c.n_unique > 1);
        cfg.y = cat ? cat.name : cols[0].name;
      }
      if (!has(cfg.sort_by)) cfg.sort_by = 'onset';
      if (!has(cfg.sort_dir)) cfg.sort_dir = 'asc';
    } else {
      if (!has(cfg.x) && cols.length) {
        const num = cols.find((c) => c.type === 'numeric');
        cfg.x = num ? num.name : cols[0].name;
      }
      if (!has(cfg.y) && cols.length) {
        const nums = cols.filter((c) => c.type === 'numeric');
        const other = nums.find((c) => c.name !== cfg.x);
        cfg.y = other ? other.name : (nums[0] ? nums[0].name : cols[0].name);
      }
    }
  };

  // Above this many colour levels a legend cannot be read.
  const MAX_COLOR_LEVELS = 15;

  /** @param {string} text */
  const emptyState = (text) =>
    '<div class="vd-empty-state"><p class="vd-empty-text">' + text + '</p></div>';

  /**
   * The empty state a config draws instead of a chart, as HTML, or null when
   * it can draw.
   * @param {any[]} rows @param {VizColumn[]} columns @param {Record<string, any>} cfg
   * @returns {string | null}
   */
  const gate = (rows, columns, cfg) => {
    if (rows.length === 0) return emptyState('No data to chart');
    const fam = NS.familyOf(cfg.chart_type);

    // xend is required only as an always-shown row: a gantt without an end
    // draws dots.
    const required = NS.roles.FAMILY_ROLES[fam].requiredMap
      .filter((/** @type {string} */ k) => k !== 'xend');
    const unset = required.filter((/** @type {string} */ k) => !NS.hasVal(cfg[k]));
    if (unset.length) {
      return emptyState('Pick ' + unset.map((/** @type {string} */ k) => {
        const l = NS.roles.ROLES[k].label;
        return (typeof l === 'function') ? l(cfg) : l;
      }).join(' and ') + ' to plot.');
    }

    // A band whose series R could not compute says why.
    if (cfg.chart_type === 'band' && !cfg.band_series && cfg.band_note) {
      return emptyState(NS.esc(cfg.band_note));
    }

    if (cfg.color) {
      const nColors = new Set(rows.map((r) => r[cfg.color])).size;
      if (nColors > MAX_COLOR_LEVELS) {
        const hint = fam === 'aggregated'
          ? `Pick a column with ≤${MAX_COLOR_LEVELS} categories.`
          : 'Use <code>series</code> to split into series (e.g. USUBJID); keep ' +
            '<code>color</code> for low-cardinality grouping (e.g. ARM).';
        return emptyState(`Too many color levels (${nColors}). ${hint}`);
      }
    }

    const colSet = new Set((columns || []).map((c) => c.name));
    const req = fam === 'aggregated'
      ? [['Group', cfg.group], ['Metric', cfg.value !== '.count' ? cfg.value : null]]
      : [['X', cfg.x], ['Y', cfg.y]];
    const missing = req
      .filter(([, v]) => v && !colSet.has(v))
      .map(([lbl, v]) => `${lbl} = "${v}"`);
    if (missing.length) {
      const avail = (columns || []).map((c) => c.name);
      const availTxt = avail.length
        ? ' Columns available here: ' + avail.slice(0, 30).join(', ') +
          (avail.length > 30 ? ', …' : '') + '.'
        : '';
      return emptyState('Mapped column not in data: ' + missing.join(', ') + '.' + availTxt +
        ' A rename, flatten or pivot upstream may have changed the column ' +
        'name — re-pick it in the gear.');
    }
    return null;
  };

  /**
   * Facet value -> its strip label, with "(n)" when count_on covers facets.
   * @param {any[]} rows @param {Record<string, any>} cfg @param {any[]} facets
   * @returns {Map<string, any>}
   */
  const facetLabels = (rows, cfg, facets) => {
    /** @type {Map<string, any>} */
    const m = new Map();
    const counts = NS.countOn(cfg, 'facet') ? NS.labelCounts(rows, cfg, cfg.facet, null, null) : null;
    for (const f of facets) m.set(f, counts ? NS.withCount(f, counts) : f);
    return m;
  };

  Object.assign(model, { ensureDistributionMetric, defaults, familyDefaults, gate, emptyState,
                         facetLabels, MAX_COLOR_LEVELS });
})();
