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
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});
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
   * The default order of a bar, pie or treemap category, from its column:
   * a date, number or factor keeps its own order (time, size, levels), any
   * other column sorts by value, largest first.
   * @param {Record<string, any>} cfg @param {VizColumn[]} columns
   * @returns {{ sort_by: string, sort_dir: string }}
   */
  const naturalSort = (cfg, columns) => {
    const meta = /** @type {any} */ ((columns || []).find((c) => c.name === cfg.group));
    const ordered = !!meta && (meta.type === 'numeric' || !!meta.temporal ||
      (Array.isArray(meta.levels) && meta.levels.length > 0));
    return ordered ? { sort_by: 'data', sort_dir: 'asc' } : { sort_by: 'value', sort_dir: 'desc' };
  };

  /**
   * Fills an unset sort of an aggregated chart. An unset direction follows
   * the sort: largest first by value, ascending otherwise, so a `sort_by =
   * "data"` without a direction does not draw the data backwards.
   * @param {Record<string, any>} cfg @param {VizColumn[]} columns
   */
  const fillSort = (cfg, columns) => {
    const has = NS.hasVal;
    if (!has(cfg.sort_by)) cfg.sort_by = naturalSort(cfg, columns).sort_by;
    if (!has(cfg.sort_dir)) cfg.sort_dir = cfg.sort_by === 'value' ? 'desc' : 'asc';
  };

  /**
   * The defaults a config gets before a draw: the distribution statistics
   * and an aggregated chart's sort. A mapping is never picked here: an unset
   * one asks for a pick (gate()). Mutates `cfg`.
   * @param {Record<string, any>} cfg @param {VizColumn[]} columns
   */
  const defaults = (cfg, columns) => {
    if (!cfg.chart_type) cfg.chart_type = 'bar';
    const fam = NS.familyOf(cfg.chart_type);
    if (fam === 'aggregated') {
      if (!ensureDistributionMetric(cfg)) {
        if (!cfg.func) cfg.func = 'count';
        if (!cfg.value && cfg.func === 'count') cfg.value = '.count';
      }
      fillSort(cfg, columns);
    } else if (fam === 'timeline') {
      if (!cfg.sort_by) cfg.sort_by = 'onset';
      if (!cfg.sort_dir) cfg.sort_dir = 'asc';
    }
  };

  /** The settings a family cannot draw without, as [setting, spec key].
   * @param {Record<string, any>} cfg @returns {[string, string][]} */
  const requiredOf = (cfg) => {
    const fam = NS.familyOf(cfg.chart_type);
    if (cfg.chart_type === 'histogram') return [['x', 'x']];
    if (fam === 'aggregated') return [['group', cfg.cat_key || 'x']];
    return [['x', 'x'], ['y', 'y']];
  };

  /** The gear's label of a spec argument. @param {string} key */
  const specLabel = (key) => {
    const spec = V.spec && V.spec.chart;
    const arg = spec && spec[key];
    return arg && arg.gear ? arg.gear.label : key;
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
    const fam = NS.familyOf(cfg.chart_type);
    const unset = requiredOf(cfg).filter(([k]) => !NS.hasVal(cfg[k]));
    if (unset.length) {
      return emptyState('Pick ' + unset.map(([, key]) => specLabel(key)).join(' and ') +
                        ' to plot.');
    }

    const colSet = new Set((columns || []).map((c) => c.name));
    // A histogram's category is the bin column R adds beside x.
    const req = cfg.chart_type === 'histogram' ? [[specLabel('x'), cfg.x]]
      : fam === 'aggregated'
        ? [[specLabel(cfg.cat_key || 'x'), cfg.group],
           [specLabel(cfg.val_key || 'y'), cfg.value !== '.count' ? cfg.value : null]]
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

    if (rows.length === 0) return emptyState('No data to chart');

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

  Object.assign(model, { ensureDistributionMetric, naturalSort, defaults, gate, emptyState,
                         facetLabels, MAX_COLOR_LEVELS });
})();
