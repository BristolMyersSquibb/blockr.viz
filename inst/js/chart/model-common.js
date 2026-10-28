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
   * non-numeric y is horizontal; otherwise, and with neither set, vertical.
   * @param {Record<string, any>} c @param {VizColumn[]} columns
   * @returns {'horizontal' | 'vertical'}
   */
  const inferLie = (c, columns) => {
    const type = c.type || 'bar';
    const hasX = set(c.x), hasY = set(c.y);
    const count = type === 'bar' && (c.func || 'count') === 'count';
    if (hasX !== hasY) return hasX === count ? 'vertical' : 'horizontal';
    if (!hasX) return 'vertical';
    const numeric = (/** @type {string} */ n) => {
      const m = columns.find((col) => col.name === n);
      return m ? m.type === 'numeric' : null;
    };
    return numeric(c.x) === true && numeric(c.y) === false ? 'horizontal' : 'vertical';
  };

  /** An aggregated chart's axes: the category column, the column it aggregates (`measure`, ".count"
   * for a row count), the statistic, the way it lies, and which of x and y holds each.
   * @param {Record<string, any>} c @param {VizColumn[]} columns */
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

  /** The defaults that depend on the chart: the statistics of a box and a point range, and the
   * sort. A date, number or factor category keeps its own order, any other sorts by value; an unset
   * direction is descending by value and ascending otherwise. A mapping is never picked here: an
   * unset one asks for a pick (gate()). Mutates `cfg`. @param {Record<string, any>} cfg
   * @param {VizColumn[]} columns */
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
    if (!set(cfg.sort_by)) {
      const meta = /** @type {any} */ (columns.find((c) => c.name === cfg.category));
      const ordered = !!meta && (meta.type === 'numeric' || !!meta.temporal ||
        (Array.isArray(meta.levels) && meta.levels.length > 0));
      cfg.sort_by = ordered ? 'data' : 'value';
    }
    if (!set(cfg.sort_dir)) cfg.sort_dir = cfg.sort_by === 'value' ? 'desc' : 'asc';
  };

  /** The settings the model and option builders read: each spec argument, its default where the
   * config leaves it out, R's `derived`, and an aggregated chart's axes (axes()).
   * @param {Record<string, any>} config @param {Record<string, any>} [derived]
   * @param {VizColumn[]} [columns] @returns {Record<string, any>} */
  const settings = (config, derived, columns = []) => {
    const spec = (V.spec && V.spec.chart) || {};
    /** @type {Record<string, any>} */
    const cfg = {};
    for (const k of Object.keys(spec)) cfg[k] = spec[k].default ?? null;
    for (const [k, v] of Object.entries(config || {})) if (v != null) cfg[k] = v;
    cfg.derived = derived || {};
    if (NS.familyOf(cfg.type) === 'aggregated') Object.assign(cfg, axes(cfg, columns));
    defaults(cfg, columns);
    return cfg;
  };

  /** The gear's label of a spec argument. @param {string} key */
  const specLabel = (key) => {
    const arg = V.spec && V.spec.chart && V.spec.chart[key];
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
    // The settings a family cannot draw without, as [setting, spec key].
    /** @type {[string, string][]} */
    const required = cfg.type === 'histogram' ? [['x', 'x']]
      : fam === 'aggregated' ? [['category', cfg.cat_key || 'x']]
      : [['x', 'x'], ['y', 'y']];
    const unset = required.filter(([k]) => !NS.hasVal(cfg[k]));
    if (unset.length) {
      return emptyState('Pick ' + unset.map(([, key]) => specLabel(key)).join(' and ') +
                        ' to plot.');
    }

    // A histogram's category is the bin column R adds beside x.
    const req = cfg.type === 'histogram' ? [[specLabel('x'), cfg.x]]
      : fam === 'aggregated'
        ? [[specLabel(cfg.cat_key || 'x'), cfg.category],
           [specLabel(cfg.val_key || 'y'), cfg.measure !== '.count' ? cfg.measure : null]]
        : [['X', cfg.x], ['Y', cfg.y]];
    const avail = columns.map((c) => c.name);
    const missing = req
      .filter(([, v]) => v && !avail.includes(v))
      .map(([lbl, v]) => `${lbl} = "${v}"`);
    if (missing.length) {
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
    const band = cfg.derived.band;
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
   * A panel's strip label: the facet value, with "(n)" when show_n covers
   * facets; null for a single panel.
   * @param {any[]} rows @param {Record<string, any>} cfg @param {number} nFacets
   * @returns {(facet: string) => string | null}
   */
  const facetLabels = (rows, cfg, nFacets) => {
    const counts = NS.countOn(cfg, 'facet') ? NS.labelCounts(rows, cfg, cfg.facet) : null;
    return (fv) => (nFacets === 1 || fv === '__all__' ? null
      : (counts ? NS.withCount(fv, counts) : fv));
  };

  /** The tooltip columns of a config, less `drop` (columns the tooltip shows anyway).
   * @param {Record<string, any>} cfg @param {string[]} [drop] @returns {string[]} */
  const tooltipFields = (cfg, drop = []) => [].concat(cfg.tooltip || []).map(String)
    .filter((c) => c && c !== 'null' && c !== '(none)' && !drop.includes(c));

  Object.assign(model, { settings, inferLie, FIXED, defaults, gate, emptyState, facetLabels,
                         tooltipFields });
})();
