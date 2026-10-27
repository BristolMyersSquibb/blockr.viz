// @ts-check
/**
 * Chart block: the model rules every family shares.
 *
 * The settings the builders read, from the payload's config, and the gate
 * in front of every draw: the empty states for no rows, an unset required
 * role, too many colour levels and a mapped column the rows do not carry.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});
  const model = NS.model = NS.model || {};

  const FIXED = ['waterfall', 'pie', 'treemap'];
  /** @param {unknown} v */
  const set = (v) => v != null && v !== '' && !(Array.isArray(v) && !v.length);

  /**
   * Which way an oriented chart lies when `orientation` is not given, the
   * twin of R's chart_infer_lie(): a row count's single column is the
   * category, any other single column the value; a numeric x beside a
   * non-numeric y is horizontal; otherwise vertical, and a bar with neither
   * set horizontal.
   * @param {Record<string, any>} c @param {VizColumn[]} columns
   * @returns {'horizontal' | 'vertical'}
   */
  const inferLie = (c, columns) => {
    const type = c.type || 'bar';
    const hasX = set(c.x), hasY = set(c.y);
    const count = type === 'bar' && (c.func || 'count') === 'count';
    if (hasX !== hasY) return hasX === count ? 'vertical' : 'horizontal';
    if (!hasX) return type === 'bar' ? 'horizontal' : 'vertical';
    const numeric = (/** @type {string} */ n) => {
      const m = /** @type {any} */ ((columns || []).find((col) => col.name === n));
      return m ? m.type === 'numeric' : null;
    };
    return numeric(c.x) === true && numeric(c.y) === false ? 'horizontal' : 'vertical';
  };

  /**
   * An aggregated chart's axes: the category column, the column it
   * aggregates (`measure`, ".count" for a row count), the statistic, the
   * way it lies, and which of x and y holds each.
   * @param {Record<string, any>} c @param {VizColumn[]} columns
   */
  const axes = (c, columns) => {
    if (c.type === 'histogram') {
      return { category: set(c.x) ? c.x + '_bin' : null, measure: '.count', func: 'count',
               orientation: 'vertical', cat_key: 'x', val_key: null };
    }
    const fixed = FIXED.includes(c.type);
    const lie = fixed ? 'vertical' : (c.orientation || inferLie(c, columns));
    const onY = lie === 'horizontal';
    const func = NS.DISTRIBUTION_TYPES.includes(c.type) ? 'mean' : (c.func || 'count');
    const count = func === 'count';
    return { category: (onY ? c.y : c.x) || null,
             measure: count ? '.count' : ((onY ? c.x : c.y) || ''), func,
             orientation: fixed ? null : lie, cat_key: onY ? 'y' : 'x',
             val_key: count ? null : (onY ? 'x' : 'y') };
  };

  /**
   * The default order of a bar, pie or treemap category, from its column:
   * a date, number or factor keeps its own order (time, size, levels), any
   * other column sorts by value, largest first.
   * @param {Record<string, any>} cfg @param {VizColumn[]} columns
   * @returns {{ sort_by: string, sort_dir: string }}
   */
  const naturalSort = (cfg, columns) => {
    const meta = /** @type {any} */ ((columns || []).find((c) => c.name === cfg.category));
    const ordered = !!meta && (meta.type === 'numeric' || !!meta.temporal ||
      (Array.isArray(meta.levels) && meta.levels.length > 0));
    return ordered ? { sort_by: 'data', sort_dir: 'asc' } : { sort_by: 'value', sort_dir: 'desc' };
  };

  /**
   * The defaults that depend on the chart: the statistics of a box and a
   * point range, and the sort. An unset direction follows the sort: largest
   * first by value, ascending otherwise. A mapping is never picked here: an
   * unset one asks for a pick (gate()). Mutates `cfg`.
   * @param {Record<string, any>} cfg @param {VizColumn[]} columns
   */
  const defaults = (cfg, columns) => {
    const fam = NS.familyOf(cfg.type);
    if (NS.DISTRIBUTION_TYPES.includes(cfg.type)) {
      const box = cfg.type === 'boxplot';
      if (!cfg.summary) cfg.summary = box ? 'median_q1_q3' : 'mean_se';
      if (box && !cfg.whiskers) cfg.whiskers = 'tukey';
      if (!set(cfg.sort_by)) cfg.sort_by = 'data';
    } else if (fam === 'timeline' && !set(cfg.sort_by)) {
      cfg.sort_by = 'onset';
    }
    if (fam === 'individual') return;
    if (!set(cfg.sort_by)) cfg.sort_by = naturalSort(cfg, columns).sort_by;
    if (!set(cfg.sort_dir)) cfg.sort_dir = cfg.sort_by === 'value' ? 'desc' : 'asc';
  };

  /**
   * The settings the model and option builders read: each spec argument,
   * its default where the config leaves it out, R's `derived`, and an
   * aggregated chart's axes (axes()).
   * @param {Record<string, any>} config @param {Record<string, any>} [derived]
   * @param {VizColumn[]} [columns]
   * @returns {Record<string, any>}
   */
  const settings = (config, derived, columns) => {
    const spec = (V.spec && V.spec.chart) || {};
    /** @type {Record<string, any>} */
    const cfg = {};
    for (const k of Object.keys(spec)) cfg[k] = spec[k].default ?? null;
    for (const [k, v] of Object.entries(config || {})) if (v != null) cfg[k] = v;
    cfg.derived = derived || {};
    if (NS.familyOf(cfg.type) === 'aggregated') Object.assign(cfg, axes(cfg, columns || []));
    defaults(cfg, columns || []);
    return cfg;
  };

  /** The settings a family cannot draw without, as [setting, spec key].
   * @param {Record<string, any>} cfg @returns {[string, string][]} */
  const requiredOf = (cfg) => {
    const fam = NS.familyOf(cfg.type);
    if (cfg.type === 'histogram') return [['x', 'x']];
    if (fam === 'aggregated') return [['category', cfg.cat_key || 'x']];
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
    const fam = NS.familyOf(cfg.type);
    const unset = requiredOf(cfg).filter(([k]) => !NS.hasVal(cfg[k]));
    if (unset.length) {
      return emptyState('Pick ' + unset.map(([, key]) => specLabel(key)).join(' and ') +
                        ' to plot.');
    }

    const colSet = new Set((columns || []).map((c) => c.name));
    // A histogram's category is the bin column R adds beside x.
    const req = cfg.type === 'histogram' ? [[specLabel('x'), cfg.x]]
      : fam === 'aggregated'
        ? [[specLabel(cfg.cat_key || 'x'), cfg.category],
           [specLabel(cfg.val_key || 'y'), cfg.measure !== '.count' ? cfg.measure : null]]
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
    const band = cfg.derived && cfg.derived.band;
    if (cfg.type === 'band' && band && !band.series && band.note) {
      return emptyState(NS.esc(band.note));
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
   * Facet value -> its strip label, with "(n)" when show_n covers facets.
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

  Object.assign(model, { settings, inferLie, axes, FIXED, naturalSort, defaults, gate,
                         emptyState, facetLabels, MAX_COLOR_LEVELS });
})();
