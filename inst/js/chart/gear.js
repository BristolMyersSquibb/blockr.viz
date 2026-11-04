// @ts-check
/**
 * Chart block: what the chart tells the core gear engine (VizGearHost)
 * beyond the spec: the type tiles, which axis is required, choices drawn
 * from the data or the board (sort orders, column values, drill targets),
 * per-type labels and rows that wait for another setting.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  const TILES = [
    { label: 'Aggregated', values: ['bar', 'histogram', 'waterfall', 'pie', 'treemap',
                                    'boxplot', 'pointrange', 'band'] },
    { label: 'Individual', values: ['scatter', 'line', 'area', 'gantt'] }
  ];
  const NUMERIC_Y = ['scatter', 'line', 'area', 'band'];
  /** Distinct values offered in a picker of a column's values. */
  const MAX_LEVELS = 500;
  const FACET_COLS = [1, 2, 3, 4, 6];
  /** Distinct values a column may have to be offered as a facet, or, when it
   * holds numbers, as a category. */
  const MAX_FACET_LEVELS = 10;
  const MAX_FACET_ROW_LEVELS = 20;
  const MAX_CATEGORY_LEVELS = 50;
  /** The panel scale choices in a grid (facet_row). */
  const GRID_SCALES = [{ value: 'fixed', label: 'Shared across panels' },
                       { value: 'free', label: 'Free per row and column' },
                       { value: 'free_y', label: 'Free per row' }];

  /** @param {string} type */
  const autoLabel = (type) => {
    const fam = NS.familyOf(type);
    if (type === 'histogram') return 'Auto, the clicked bin';
    if (fam === 'aggregated') return 'Auto, the clicked group';
    if (fam === 'timeline') return 'Auto, the clicked lane';
    return NS.isLineType(type) ? 'Auto, the clicked series' : 'Auto, the selected point';
  };

  // Aggregations that read any column (they count distinct values); the others need numbers.
  const ANY_COLUMN = ['count_distinct', 'pct_distinct'];

  /** Whether an aggregated chart only counts rows, so `value` is unused.
   * @param {string} type @param {Record<string, any>} v */
  const counts = (type, v) => !NS.DISTRIBUTION_TYPES.includes(type) && (v.func || 'count') === 'count';

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
      return V.util.isNumeric(meta().find((/** @type {VizColumn} */ c) => c.name === name));
    };
    return {
      required(key, v) {
        const type = String(v.type || 'bar');
        if (type === 'histogram') return key === 'x';
        if (NS.familyOf(type) !== 'aggregated') return key === 'x' || key === 'y';
        return key === 'category' || (key === 'value' && !counts(type, v));
      },
      label(key, v) {
        const type = String(v.type || 'bar');
        if (type === 'band' && key === 'x') return 'Timeline';
        if (type === 'band' && key === 'y') return 'Value';
        // As 0.2: an aggregated chart or a histogram has "Reference lines". A bar
        // also takes them on its category axis, so its two rows name their axis.
        // Scatter, line and area keep the spec's Horizontal and Vertical.
        if ((key === 'value_lines' || key === 'x_lines') && NS.familyOf(type) === 'aggregated') {
          if (type !== 'bar') return 'Reference lines';
          return key === 'x_lines' ? 'Reference lines (category axis)' : 'Reference lines (value axis)';
        }
        if (key === 'band_size' && v.band_window === 'fixed') return 'Half-width (x units)';
        return null;
      },
      visible(key, v) {
        if (key === 'percent_of') return v.func === 'pct_distinct';
        if (key === 'value') return !counts(String(v.type || 'bar'), v);
        if (key === 'drill_target' || key === 'drill_table') return !!v.drill;
        // Panel scales apply to a wrap and a grid; a grid has its own columns.
        if (key === 'facet_scales') return !!(v.facet || v.facet_row);
        if (key === 'facet_cols') return !v.facet_row;
        return true;
      },
      tiles: (key) => (key === 'type' ? TILES : null),
      columns(key, v, cols) {
        const type = String(v.type || 'bar');
        const num = ['ref_min', 'ref_max', 'ymin', 'ymax'].includes(key) ||
          (key === 'y' && NUMERIC_Y.includes(type)) || (key === 'x' && type === 'histogram') ||
          (key === 'value' && !ANY_COLUMN.includes(String(v.func || 'count')));
        if (num) return cols.filter((c) => numeric(c.name));
        // As 0.2: a facet offers columns with at most 10 distinct values, a
        // category columns that are not numbers or have at most 50. That keeps
        // a prepare script's helper columns and the measures out of them.
        if (key === 'facet') return cols.filter((c) => c.n_unique <= MAX_FACET_LEVELS);
        // Grid rows stack down the card, so they take more levels.
        if (key === 'facet_row') return cols.filter((c) => c.n_unique <= MAX_FACET_ROW_LEVELS);
        if (key === 'category') {
          return cols.filter((c) => c.class !== 'number' || c.n_unique <= MAX_CATEGORY_LEVELS);
        }
        return cols;
      },
      choices(key, v) {
        const type = String(v.type || 'bar');
        if (key === 'drill') return [{ value: 'auto', label: autoLabel(type) }];
        if (key === 'drill_target') {
          const list = view.derived.ctrl_choices || [];
          return [{ value: '', label: 'None' }].concat(list);
        }
        if (key === 'facet_cols') return FACET_COLS.map((n) => ({ value: n, label: n + ' per row' }));
        // A grid frees its scales by row and by column, not by panel.
        if (key === 'facet_scales' && v.facet_row) return GRID_SCALES;
        if (key === 'sort_by') return sortChoices(type, meta());
        if (key === 'waterfall_totals') return levels(String(v.category ?? ''));
        if (key === 'highlight') {
          const h = v.highlight ? Object.keys(/** @type {object} */ (v.highlight))[0] : null;
          return h ? levels(h) : [];
        }
        return null;
      },
      placeholder(key) {
        if (key === 'unit') return 'e.g. CHF';
        if (key === 'title') return 'e.g. AEs by {ARM}';
        if (key === 'subtitle') return 'e.g. {@func} of {label(@value)}[, by {label(@color)}]';
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
