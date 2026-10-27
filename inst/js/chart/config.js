// @ts-check
/**
 * Chart block: the settings the drawing reads, from the payload.
 *
 * R sends the state under the spec's names (`config`) and what it computed
 * for the drawing (`derived`). The model and option builders read one flat
 * settings object; this file builds it. Where the builders still use their
 * own words for a setting (`group` and `value` for the category and the
 * aggregated column, `bar_mode`, `"on"`/`"off"`), the translation is here
 * and nowhere else.
 *
 * Which axis an aggregated chart's category lies on is R's rule
 * (chart_infer_lie() in R/chart-normalize.R); `inferLie()` is its twin for
 * a payload that does not carry it.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});

  const FIXED = ['waterfall', 'pie', 'treemap'];
  const ORIENTED = ['bar', 'boxplot', 'pointrange'];
  /** @type {Record<string, string>} */
  const BAR_MODE = { stack: 'stacked', dodge: 'grouped', fill: 'percent' };
  /** @type {Record<string, string>} */
  const COUNT_ON = { none: 'off', axis: 'axis', facet: 'facet', both: 'both' };
  /** @param {unknown} v */
  const onOff = (v) => (v === true ? 'on' : 'off');
  /** @param {unknown} v */
  const set = (v) => v != null && v !== '' && !(Array.isArray(v) && !v.length);
  /** @param {unknown} v @returns {{ value: any, label: string | null }[]} */
  const labelled = (v) => (Array.isArray(v) ? v : []);

  /**
   * Which way an oriented chart lies when `orientation` is not given:
   * a row count's single column is the category, any other single column
   * the value; a numeric x beside a non-numeric y is horizontal; otherwise
   * vertical, and a bar with neither set horizontal.
   * @param {Record<string, any>} c @param {VizColumn[]} columns
   * @returns {'horizontal' | 'vertical'}
   */
  function inferLie(c, columns) {
    const type = c.type || 'bar';
    const hasX = set(c.x);
    const hasY = set(c.y);
    const count = type === 'bar' && (c.func || 'count') === 'count';
    if (hasX !== hasY) return hasX === count ? 'vertical' : 'horizontal';
    if (hasX) {
      const kind = (/** @type {string} */ n) => {
        const m = /** @type {any} */ ((columns || []).find((col) => col.name === n));
        return m ? m.type === 'numeric' : null;
      };
      return kind(c.x) === true && kind(c.y) === false ? 'horizontal' : 'vertical';
    }
    return type === 'bar' ? 'horizontal' : 'vertical';
  }

  /**
   * The category, the aggregated column and the orientation of an
   * aggregated chart, from x, y and the way it lies.
   * @param {Record<string, any>} c @param {Record<string, any>} d @param {VizColumn[]} columns
   */
  function axes(c, d, columns) {
    const type = c.type || 'bar';
    if (type === 'histogram') {
      return { group: set(c.x) ? c.x + '_bin' : null, value: '.count', func: 'count',
               orientation: 'vertical', cat_key: 'x', val_key: null };
    }
    const fixed = FIXED.includes(type);
    const lie = fixed ? 'vertical' : (c.orientation || d.orientation || inferLie(c, columns));
    const onY = lie === 'horizontal';
    const group = onY ? c.y : c.x;
    const other = onY ? c.x : c.y;
    const dist = NS.DISTRIBUTION_TYPES.includes(type);
    const func = dist ? 'mean' : (c.func || 'count');
    const value = func === 'count' ? '.count' : (other || '');
    return { group: group || null, value, func, orientation: fixed ? null : lie,
             cat_key: onY ? 'y' : 'x', val_key: func === 'count' ? null : (onY ? 'x' : 'y') };
  }

  /**
   * The saved selection as the builders' filter fields.
   * @param {any} sel
   */
  function selection(sel) {
    if (!sel || typeof sel !== 'object') return {};
    if (sel.type === 'range') {
      return {
        filter_type: 'range', filters: sel.filters || null,
        filter_range: { x_col: sel.x, y_col: sel.y || null, x_range: sel.x_range,
                        y_range: sel.y_range || null }
      };
    }
    return { filter_type: 'categorical', filters: sel.filters || null };
  }

  /** @param {Record<string, any>} c */
  function highlight(c) {
    const h = c.highlight;
    const col = h && typeof h === 'object' ? Object.keys(h)[0] : null;
    return {
      highlight_by: col || '',
      highlight: col ? labelled(h[col]).map((x) => String(x.value)) : []
    };
  }

  /** @param {any} r */
  function ranges(r) {
    if (!r || typeof r !== 'object') return [];
    return Object.entries(r).map(([name, span]) => {
      const s = labelled(span);
      return { name, from: s[0] ? s[0].value : null, to: s[1] ? s[1].value : null };
    }).filter((x) => x.from != null && x.to != null);
  }

  /** What R computed for the drawing, under the builders' names.
   * @param {Record<string, any>} d */
  function computed(d) {
    const band = d.band || null;
    return {
      band_series: band ? band.series || null : null,
      band_refs: band ? band.refs || null : null,
      band_note: band ? band.note || null : null,
      smoother_series: d.smoother || null,
      scales: d.scales || null,
      palette: d.palette || null,
      capture_ratio: d.capture_ratio || null
    };
  }

  /**
   * The flat settings the model and option builders read.
   * @param {Record<string, any>} config The state, under the spec's names.
   * @param {Record<string, any>} [derived] What R computed for the drawing.
   * @param {VizColumn[]} [columns] The column meta.
   * @returns {Record<string, any>}
   */
  function settings(config, derived, columns) {
    const c = config || {};
    const d = derived || {};
    const type = c.type || 'bar';
    const fam = NS.familyOf(type);
    const yLines = labelled(c.y_lines);
    const xLines = labelled(c.x_lines);
    /** @type {Record<string, any>} */
    const out = {
      chart_type: type, x: c.x || null, y: c.y || null, xend: c.xend || null,
      color: c.color || '', facet: c.facet || '', series: c.series || '',
      label: c.label || '', linetype: c.linetype || '', drill: c.drill || '',
      sort_by: c.sort_by || null, sort_dir: c.sort_dir || null,
      bar_mode: BAR_MODE[c.position] || 'stacked', value_labels: onOff(c.value_labels),
      identity_line: onOff(c.identity_line), connect_centers: onOff(c.connect_centers),
      download: onOff(c.download), na_group: c.missing || 'level',
      pct_of: c.percent_of === 'category' ? 'group' : (c.percent_of || 'facet'),
      waterfall_totals: labelled(c.waterfall_totals).map((x) => String(x.value)),
      connect: String(c.connect || 'straight').replace('_', '-'),
      value_lines: yLines.map((x) => x.value), value_line_names: yLines.map((x) => x.label || ''),
      x_lines: xLines.map((x) => x.value), x_line_names: xLines.map((x) => x.label || ''),
      x_ranges: ranges(c.x_ranges), tt_fields: Array.isArray(c.tooltip) ? c.tooltip : [],
      lo: c.ymin || [], hi: c.ymax || [],
      line_width_mult: c.line_width ?? 1, dot_size_mult: c.point_size ?? 1,
      legend: c.legend || 'bottom', smoother: c.smoother || 'none',
      box_points: c.box_points || 'none', summary: c.summary || null, whiskers: c.whiskers || null,
      band_window: c.band_window || 'adaptive', band_size: c.band_size ?? 45,
      band_min_n: c.band_min_n ?? 12, bins: c.bins ?? 30, format: c.format ?? 'number',
      unit: c.unit ?? '', count_col: c.id || '', band_id: c.id || '',
      count_on: COUNT_ON[c.show_n] || 'off', ref_lo: c.ref_min || '', ref_hi: c.ref_max || '',
      y_scale: c.y_transform === 'log10' ? 'log' : 'linear',
      x_scale: c.x_transform === 'log10' ? 'log' : 'linear',
      facet_scales: c.facet_scales || 'fixed',
      facet_cols: c.facet_cols == null ? '' : String(c.facet_cols),
      ctrl_target: c.drill_target || '', ctrl_table: c.drill_table || '',
      title: c.title ?? null, subtitle: c.subtitle ?? null, caption: c.caption ?? null
    };
    if (fam === 'aggregated') Object.assign(out, axes(c, d, columns || []));
    return Object.assign(out, highlight(c), selection(c.selection), computed(d));
  }

  NS.config = { settings, inferLie, ORIENTED, FIXED };
})();
