/* The model and option builders' settings (their own words: `group`,
 * `value`, `bar_mode`, "on"/"off") as a payload under the spec's names,
 * `{config, derived}`: the inverse of chart/config.js settings(). For tests
 * that state a case in the builders' words and draw it through the view. */
'use strict';

const BAR_MODE = { stacked: 'stack', grouped: 'dodge', percent: 'fill' };
const set = (v) => v != null && v !== '' && !(Array.isArray(v) && !v.length);
const bool = (v) => v === 'on' || v === true;
const labelled = (vals, names) => (Array.isArray(vals) && vals.length
  ? vals.map((value, i) => ({ value, label: (names && names[i]) || null })) : null);
const DIST = ['boxplot', 'pointrange'];
const AGG = ['bar', 'waterfall', 'pie', 'treemap', 'boxplot', 'pointrange'];

function toPayload(o) {
  const type = o.chart_type === 'bar' && o.baseline === 'cumulative' ? 'waterfall' : o.chart_type;
  const c = { type };
  const derived = {};
  if (AGG.includes(type)) {
    const fixed = ['waterfall', 'pie', 'treemap'].includes(type);
    const lie = fixed ? 'vertical' : (o.orientation || (DIST.includes(type) ? 'vertical' : 'horizontal'));
    const value = o.value === '.count' || !set(o.value) ? null : o.value;
    const func = DIST.includes(type) ? null : (o.func || 'count');
    const val = func === 'count' ? null : value;
    if (lie === 'horizontal') { c.y = o.group || null; c.x = val; } else { c.x = o.group || null; c.y = val; }
    if (!fixed) c.orientation = lie;
    if (func) c.func = func;
  } else {
    for (const k of ['x', 'y', 'xend']) if (k in o) c[k] = set(o[k]) ? o[k] : null;
  }
  for (const k of ['color', 'facet', 'series', 'label', 'linetype', 'sort_by', 'sort_dir',
                   'smoother', 'box_points', 'summary', 'whiskers', 'legend', 'format', 'unit',
                   'facet_scales', 'band_window', 'band_size', 'band_min_n', 'bins']) {
    if (set(o[k])) c[k] = o[k];
  }
  if (set(o.drill)) c.drill = o.drill;
  if (o.bar_mode) c.position = BAR_MODE[o.bar_mode];
  for (const k of ['value_labels', 'identity_line', 'connect_centers', 'download']) {
    if (k in o) c[k] = bool(o[k]);
  }
  if (o.connect) c.connect = o.connect.replace('-', '_');
  if (o.na_group) c.missing = o.na_group;
  if (o.pct_of) c.percent_of = o.pct_of === 'group' ? 'category' : o.pct_of;
  if (o.count_on) c.show_n = o.count_on === 'off' ? 'none' : o.count_on;
  if (set(o.count_col) || set(o.band_id)) c.id = o.count_col || o.band_id;
  if (set(o.value_lines)) c.y_lines = labelled(o.value_lines, o.value_line_names);
  if (set(o.x_lines)) c.x_lines = labelled(o.x_lines, o.x_line_names);
  if (set(o.lo)) c.ymin = [].concat(o.lo);
  if (set(o.hi)) c.ymax = [].concat(o.hi);
  if (set(o.tt_fields)) c.tooltip = o.tt_fields;
  if (o.line_width_mult != null) c.line_width = o.line_width_mult;
  if (o.dot_size_mult != null) c.point_size = o.dot_size_mult;
  if (set(o.facet_cols)) c.facet_cols = Number(o.facet_cols);
  if (set(o.waterfall_totals)) c.waterfall_totals = labelled(o.waterfall_totals);
  if (set(o.ref_hi)) c.ref_max = o.ref_hi;
  if (set(o.ref_lo)) c.ref_min = o.ref_lo;
  if (o.y_scale) c.y_transform = o.y_scale === 'log' ? 'log10' : 'identity';
  if (o.x_scale) c.x_transform = o.x_scale === 'log' ? 'log10' : 'identity';
  if (set(o.highlight_by)) {
    c.highlight = { [o.highlight_by]: [].concat(o.highlight || []).map((value) => ({ value, label: null })) };
  }
  if (set(o.filter_column) && set(o.filter_values)) {
    c.selection = { type: 'categorical', filters: { [o.filter_column]: o.filter_values } };
  }
  if (o.filters) c.selection = { type: 'categorical', filters: o.filters };
  if (o.band_series || o.band_refs || o.band_note) {
    derived.band = { series: o.band_series || null, refs: o.band_refs || null,
                     note: o.band_note || null };
  }
  if (o.smoother_series) derived.smoother = o.smoother_series;
  if (o.scales) derived.scales = o.scales;
  if (o.palette) derived.palette = o.palette;
  const titles = {};
  for (const k of ['title', 'subtitle', 'caption']) {
    if (o[k + '_resolved']) titles[k] = { text: o[k + '_resolved'] };
  }
  if (Object.keys(titles).length) derived.titles = titles;
  return { config: c, derived };
}

module.exports = { toPayload };
