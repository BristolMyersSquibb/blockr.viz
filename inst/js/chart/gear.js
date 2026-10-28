// @ts-check
/**
 * Chart block: what the chart tells the core gear engine (gear-api.md)
 * beyond the spec: the type tiles, which axis is required, choices drawn
 * from the data or the board (sort orders, column values, drill targets),
 * per-type labels and rows that wait for another setting.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});

  const TILES = [
    { label: 'Aggregated', values: ['bar', 'histogram', 'waterfall', 'pie', 'treemap',
                                    'boxplot', 'pointrange', 'band'] },
    { label: 'Individual', values: ['scatter', 'line', 'area', 'gantt'] }
  ];
  const NUMERIC_Y = ['scatter', 'line', 'area', 'band'];
  /** Distinct values offered in a picker of a column's values. */
  const MAX_LEVELS = 500;
  const FACET_COLS = [1, 2, 3, 4, 6];

  /** @param {string} type */
  const autoLabel = (type) => {
    const fam = NS.familyOf(type);
    if (type === 'histogram') return 'Auto, the clicked bin';
    if (fam === 'aggregated') return 'Auto, the clicked group';
    if (fam === 'timeline') return 'Auto, the clicked lane';
    return NS.isLineType(type) ? 'Auto, the clicked series' : 'Auto, the selected point';
  };

  /** The keys of an aggregated chart's category and aggregated column.
   * @param {Record<string, any>} v @param {VizColumn[]} meta */
  const roleKeys = (v, meta) => {
    const type = v.type || 'bar';
    if (NS.model.FIXED.includes(type) || type === 'histogram') return { cat: 'x', val: 'y' };
    const lie = v.orientation || NS.model.inferLie(v, meta);
    return lie === 'horizontal' ? { cat: 'y', val: 'x' } : { cat: 'x', val: 'y' };
  };

  /** @param {any} view The chart view: its rows, column meta and derived. @returns {VizGearHost} */
  function host(view) {
    const meta = () => view.columns;
    /** @param {string} col */
    const levels = (col) => (col
      ? view._ix.levels(view.data, col, 'nz').filter((/** @type {string} */ x) => x !== '')
        .slice(0, MAX_LEVELS).map((/** @type {string} */ x) => ({ value: x, label: x }))
      : []);
    /** @param {string} name */
    const numeric = (name) => {
      return V.util.isNumeric(meta().find((c) => c.name === name));
    };
    return {
      required(key, v) {
        const type = v.type || 'bar';
        if (NS.familyOf(type) !== 'aggregated') return key === 'x' || key === 'y';
        const r = roleKeys(v, meta());
        if (key === r.cat) return true;
        const counts = !NS.DISTRIBUTION_TYPES.includes(type) && (v.func || 'count') === 'count';
        return key === r.val && type !== 'histogram' && !counts;
      },
      label(key, v) {
        const type = v.type || 'bar';
        if (type === 'band' && key === 'x') return 'Timeline';
        if (type === 'band' && key === 'y') return 'Value';
        if (type === 'bar' && key === 'y_lines') return 'Reference lines';
        if (type === 'bar' && key === 'x_lines') return 'Category lines';
        if (key === 'band_size' && v.band_window === 'fixed') return 'Half-width (x units)';
        return null;
      },
      visible(key, v) {
        if (key === 'percent_of') return v.func === 'pct_distinct';
        if (key === 'drill_target' || key === 'drill_table') return !!v.drill;
        return true;
      },
      tiles: (key) => (key === 'type' ? TILES : null),
      columns(key, v, cols) {
        const type = v.type || 'bar';
        const num = ['ref_min', 'ref_max', 'ymin', 'ymax'].includes(key) ||
          (key === 'y' && NUMERIC_Y.includes(type)) || (key === 'x' && type === 'histogram');
        return num ? cols.filter((c) => numeric(c.name)) : cols;
      },
      choices(key, v) {
        const type = v.type || 'bar';
        if (key === 'drill') return [{ value: 'auto', label: autoLabel(type) }];
        if (key === 'drill_target') {
          const list = view.derived.ctrl_choices || [];
          return [{ value: '', label: 'None' }].concat(list);
        }
        if (key === 'facet_cols') return FACET_COLS.map((n) => ({ value: n, label: n + ' per row' }));
        if (key === 'sort_by') return sortChoices(type, meta());
        if (key === 'waterfall_totals') return levels(v.x);
        if (key === 'highlight') {
          const h = v.highlight && Object.keys(v.highlight)[0];
          return h ? levels(h) : [];
        }
        return null;
      },
      placeholder(key) {
        if (key === 'unit') return 'e.g. CHF';
        if (key === 'title') return 'e.g. AEs by {ARM}';
        if (key === 'subtitle') return 'e.g. {@func} of {label(@y)}[, by {label(@color)}]';
        if (key === 'caption') return 'e.g. {filters} or N = {n} records';
        return null;
      }
    };
  }

  /** @param {string} type @param {VizColumn[]} meta */
  function sortChoices(type, meta) {
    const fixed = type === 'gantt'
      ? [{ value: 'onset', label: 'Onset' }, { value: 'alpha', label: 'A–Z' }]
      : [{ value: 'value', label: 'By value' }, { value: 'data', label: 'Data order' },
         { value: 'alpha', label: 'A–Z' }];
    const cols = meta.filter(V.util.isNumeric)
      .map((c) => ({ value: c.name, label: c.name, note: c.label && c.label !== c.name ? c.label : '' }));
    return fixed.concat(cols);
  }

  NS.gear = { host };
})();
