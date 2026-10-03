/* The characterization cases: one chart config each, drawn against the
 * fixture in fixtures/chart-data.json (see make-fixtures.R).
 *
 * The fixture has missing values in the group (AETERM), colour (AESEV, ARM),
 * series (USUBJID), facet (ARM) and value (AVAL, CHG) columns, a factor with
 * an unused level (SEX: M, F, U), a visit column whose order comes from a
 * companion (AVISIT / AVISITN) and numeric NA shipped as the string "NA",
 * the way chart_data_json() sends it.
 */
'use strict';

const F = require('./fixtures/chart-data.json');

// What build_chart_msg() sends for a block built with new_chart_block()'s
// defaults; each case overrides what it is about.
const BASE = {
  value: '.count', func: 'count', bar_mode: 'stacked', value_labels: 'off',
  line_width_mult: 1, dot_size_mult: 1, connect: 'monotone',
  value_lines: [], x_lines: [], smoother: 'none', identity_line: 'off',
  box_points: 'none', connect_centers: 'off', count_on: 'off',
  na_group: 'level', facet_scales: 'fixed', download: 'off',
  waterfall_totals: [], filter_values: []
};

const c = (name, config, opts = {}) => ({ name, config: { ...BASE, ...config }, ...opts });

const AE = { group: 'AETERM', drill: 'auto' };
const LAB = { x: 'ADY', y: 'AVAL', drill: 'auto' };
const GANTT = { chart_type: 'gantt', x: 'ASTDY', xend: 'AENDY', y: 'AETERM', drill: 'auto' };

const aggregated = [
  // bar
  c('bar-defaults', { chart_type: 'bar' }),
  c('bar-group', { chart_type: 'bar', ...AE }),
  c('bar-color-stacked', { chart_type: 'bar', ...AE, color: 'AESEV' }),
  c('bar-color-grouped', { chart_type: 'bar', ...AE, color: 'AESEV', bar_mode: 'grouped' }),
  c('bar-color-percent', { chart_type: 'bar', ...AE, color: 'AESEV', bar_mode: 'percent' }),
  c('bar-vertical', { chart_type: 'bar', ...AE, orientation: 'vertical' }),
  c('bar-vertical-narrow', { chart_type: 'bar', ...AE, orientation: 'vertical' }, { width: 320 }),
  c('bar-vertical-grouped', { chart_type: 'bar', ...AE, color: 'AESEV', bar_mode: 'grouped',
                              orientation: 'vertical' }),
  c('bar-mean', { chart_type: 'bar', group: 'AVISIT', value: 'AVAL', func: 'mean', drill: 'auto' }),
  c('bar-facet-fixed', { chart_type: 'bar', ...AE, color: 'AESEV', facet: 'ARM' }),
  c('bar-facet-free', { chart_type: 'bar', ...AE, facet: 'ARM', facet_scales: 'free' }),
  c('bar-facet-cols', { chart_type: 'bar', ...AE, facet: 'SEX', facet_cols: '1' }),
  c('bar-count-axis', { chart_type: 'bar', ...AE, color: 'AESEV', count_on: 'axis' }),
  c('bar-count-axis-distinct', { chart_type: 'bar', ...AE, count_on: 'axis',
                                 count_col: 'USUBJID' }),
  c('bar-count-both-facet', { chart_type: 'bar', ...AE, facet: 'SEX', count_on: 'both',
                              count_col: 'USUBJID' }),
  c('bar-sort-value-asc', { chart_type: 'bar', ...AE, sort_by: 'value', sort_dir: 'asc' }),
  c('bar-sort-alpha-desc', { chart_type: 'bar', ...AE, sort_by: 'alpha', sort_dir: 'desc' }),
  c('bar-sort-data', { chart_type: 'bar', group: 'AVISIT', sort_by: 'data', sort_dir: 'asc',
                       drill: 'auto' }),
  c('bar-sort-alpha-factor', { chart_type: 'bar', group: 'SEX', color: 'ARM', sort_by: 'alpha',
                               sort_dir: 'asc', drill: 'auto' }),
  c('bar-sort-column', { chart_type: 'bar', ...AE, sort_by: 'ASTDY', sort_dir: 'asc' }),
  c('bar-value-labels', { chart_type: 'bar', ...AE, color: 'AESEV', value_labels: 'on' }),
  c('bar-value-labels-vertical-grouped', { chart_type: 'bar', ...AE, color: 'AESEV',
                                           bar_mode: 'grouped', orientation: 'vertical',
                                           value_labels: 'on' }),
  c('bar-value-labels-percent', { chart_type: 'bar', ...AE, color: 'AESEV',
                                  bar_mode: 'percent', value_labels: 'on' }),
  c('bar-pct-distinct-facet', { chart_type: 'bar', ...AE, value: 'USUBJID',
                                func: 'pct_distinct', facet: 'SEX', pct_of: 'facet',
                                value_labels: 'on' }),
  c('bar-pct-distinct-color', { chart_type: 'bar', ...AE, color: 'ARM', value: 'USUBJID',
                                func: 'pct_distinct', pct_of: 'color' }),
  c('bar-na-group-drop', { chart_type: 'bar', ...AE, na_group: 'drop' }),
  c('bar-tt-fields', { chart_type: 'bar', ...AE, tt_fields: ['ARM', 'AVISIT'] }),
  c('bar-drill-off', { chart_type: 'bar', group: 'AETERM', color: 'AESEV', drill: '' }),
  c('bar-selected', { chart_type: 'bar', ...AE, color: 'AESEV', filter_column: 'AETERM',
                      filter_values: ['Nausea'] }),
  c('bar-identity-count-col', { chart_type: 'bar', group: 'USUBJID', value: 'AVAL',
                                func: 'identity', count_on: 'axis', count_col: 'ANRHI',
                                drill: 'auto' }),
  c('bar-value-lines', { chart_type: 'bar', ...AE, value_lines: [5, 12] }),
  c('bar-scales', { chart_type: 'bar', ...AE, color: 'AESEV',
                    scales: { var: 'AESEV', color: { MILD: '#00aa00', SEVERE: '#aa0000' },
                              order: ['SEVERE', 'MODERATE', 'MILD'] } }),
  c('bar-palette-overflow', { chart_type: 'bar', group: 'AVISIT', color: 'USUBJID',
                              palette: ['#111111', '#222222', '#333333'], drill: 'auto' }),
  c('bar-too-many-colors', { chart_type: 'bar', ...AE, color: 'ADY' }),
  c('bar-missing-column', { chart_type: 'bar', group: 'NOPE' }),
  c('bar-titles', { chart_type: 'bar', ...AE, title_resolved: 'Adverse events',
                    subtitle_resolved: 'Count of rows by Reported Term',
                    caption_resolved: 'Safety population\nN = 60 records' }),
  c('bar-theme-dark', { chart_type: 'bar', ...AE }, { theme: 'dark' }),
  c('bar-resend', { chart_type: 'bar', ...AE, color: 'AESEV' },
    { resend: { sort_dir: 'asc', count_on: 'axis' } }),
  // waterfall
  c('waterfall', { chart_type: 'waterfall', group: 'AVISIT', value: 'CHG', func: 'sum',
                   drill: 'auto' }),
  c('waterfall-totals-labels', { chart_type: 'bar', baseline: 'cumulative', group: 'AVISIT',
                                 value: 'CHG', func: 'sum', waterfall_totals: ['Week 12'],
                                 value_labels: 'on', count_on: 'axis', drill: 'auto' }),
  // pie, treemap
  c('pie', { chart_type: 'pie', group: 'AESEV', drill: 'auto' }),
  c('pie-facet-selected', { chart_type: 'pie', group: 'AESEV', facet: 'SEX', drill: 'auto',
                            filter_column: 'AESEV', filter_values: ['MILD'] }),
  c('treemap', { chart_type: 'treemap', group: 'AETERM', value: 'AVAL', func: 'sum',
                 drill: 'auto' }),
  // radar
  c('radar', { chart_type: 'radar', group: 'AVISIT', color: 'ARM', value: 'AVAL', func: 'mean',
               drill: 'auto' }),
  c('radar-nocolor', { chart_type: 'radar', group: 'AETERM', drill: 'auto' }),
  c('radar-facet', { chart_type: 'radar', group: 'AVISIT', color: 'AESEV', facet: 'SEX',
                     drill: 'auto' }),
  // boxplot, pointrange
  c('boxplot', { chart_type: 'boxplot', group: 'AVISIT', value: 'AVAL', drill: 'auto' }),
  c('boxplot-color-outliers', { chart_type: 'boxplot', group: 'AVISIT', color: 'ARM',
                                value: 'AVAL', box_points: 'outliers', drill: 'auto' }),
  c('boxplot-horizontal-count', { chart_type: 'boxplot', group: 'AETERM', value: 'AVAL',
                                  orientation: 'horizontal', count_on: 'axis',
                                  count_col: 'USUBJID', drill: 'auto' }),
  c('boxplot-facet-color', { chart_type: 'boxplot', group: 'AVISIT', color: 'ARM',
                             facet: 'SEX', value: 'AVAL', drill: 'auto' }),
  c('boxplot-stats', { chart_type: 'boxplot', group: 'AVISIT', value: 'AVAL',
                       summary: 'mean_sd', whiskers: 'min_max', value_lines: [30],
                       drill: 'auto' }),
  c('boxplot-no-value', { chart_type: 'boxplot', group: 'AVISIT' }),
  c('pointrange', { chart_type: 'pointrange', group: 'AVISIT', value: 'AVAL', drill: 'auto' }),
  c('pointrange-color-connect', { chart_type: 'pointrange', group: 'AVISIT', color: 'ARM',
                                  value: 'AVAL', connect_centers: 'on',
                                  orientation: 'horizontal', drill: 'auto' })
];

const individual = [
  c('scatter-defaults', { chart_type: 'scatter' }),
  c('scatter', { chart_type: 'scatter', ...LAB }),
  c('scatter-color', { chart_type: 'scatter', ...LAB, color: 'ARM' }),
  c('scatter-series', { chart_type: 'scatter', ...LAB, series: 'USUBJID' }),
  c('scatter-series-color', { chart_type: 'scatter', ...LAB, series: 'USUBJID', color: 'ARM' }),
  c('scatter-facet-fixed', { chart_type: 'scatter', ...LAB, color: 'AESEV', facet: 'ARM' }),
  c('scatter-facet-free-y', { chart_type: 'scatter', ...LAB, facet: 'SEX',
                              facet_scales: 'free_y', count_on: 'facet',
                              count_col: 'USUBJID' }),
  c('scatter-identity', { chart_type: 'scatter', x: 'AVAL', y: 'LO', identity_line: 'on',
                          drill: 'auto' }),
  c('scatter-smoother', { chart_type: 'scatter', ...LAB, smoother: 'lm',
                          smoother_series: F.smoother_series.plain }),
  c('scatter-smoother-color', { chart_type: 'scatter', ...LAB, color: 'ARM', smoother: 'lm',
                                smoother_series: F.smoother_series.color }),
  c('scatter-smoother-facet', { chart_type: 'scatter', ...LAB, facet: 'SEX', smoother: 'lm',
                                smoother_series: F.smoother_series.facet }),
  c('scatter-errorbars-refs', { chart_type: 'scatter', ...LAB, lo: 'LO', hi: 'HI',
                                x_lines: [28], value_lines: [25, 35] }),
  c('scatter-tt-fields', { chart_type: 'scatter', ...LAB, color: 'ARM',
                           tt_fields: ['USUBJID', 'AVISIT', 'AVAL'] }),
  c('scatter-categorical-x', { chart_type: 'scatter', x: 'AVISIT', y: 'AVAL', drill: 'auto' }),
  c('scatter-drill-off', { chart_type: 'scatter', ...LAB, color: 'ARM', drill: '' }),
  c('line', { chart_type: 'line', ...LAB, series: 'USUBJID' }),
  c('line-series-color', { chart_type: 'line', ...LAB, series: 'USUBJID', color: 'ARM' }),
  c('line-categorical-x', { chart_type: 'line', x: 'AVISIT', y: 'AVAL', series: 'USUBJID',
                            drill: 'auto' }, { width: 300 }),
  c('line-facet-counts', { chart_type: 'line', ...LAB, series: 'USUBJID', facet: 'ARM',
                           count_on: 'facet', count_col: 'USUBJID' }),
  c('line-single-step-errorbars', { chart_type: 'line', ...LAB, connect: 'step-end',
                                    lo: 'LO', hi: 'HI' }),
  c('line-color-only', { chart_type: 'line', ...LAB, color: 'SEX', tt_fields: ['USUBJID'] }),
  c('band', { chart_type: 'band', ...LAB, band_series: F.band_series.plain,
              whiskers: 'p10_p90' }),
  c('band-color', { chart_type: 'band', ...LAB, color: 'ARM', band_series: F.band_series.color,
                    whiskers: 'p10_p90' }),
  c('band-facet-refs', { chart_type: 'band', ...LAB, facet: 'SEX', box_points: 'outliers',
                         band_series: F.band_series.facet, band_refs: F.band_refs,
                         whiskers: 'p10_p90' }),
  c('band-note', { chart_type: 'band', ...LAB, band_note: 'Too few subjects per window.' })
];

const timeline = [
  c('gantt', { ...GANTT }),
  c('gantt-no-xend', { ...GANTT, xend: null }),
  c('gantt-color-label-series', { ...GANTT, color: 'AESEV', label: 'AETERM',
                                  series: 'USUBJID' }),
  c('gantt-patient-facet-counts', { ...GANTT, y: 'USUBJID', facet: 'ARM', sort_by: 'onset',
                                    count_on: 'both', count_col: 'AETERM' }),
  c('gantt-patient-facet-free', { ...GANTT, y: 'USUBJID', facet: 'ARM', facet_scales: 'free' }),
  c('gantt-sort-alpha-desc', { ...GANTT, sort_by: 'alpha', sort_dir: 'desc' }),
  c('gantt-sort-column', { ...GANTT, sort_by: 'ADY', sort_dir: 'asc' }),
  c('gantt-categorical-x', { ...GANTT, x: 'AVISIT', xend: null, y: 'USUBJID' }),
  c('gantt-tt-fields-drill-column', { ...GANTT, tt_fields: ['USUBJID', 'AESEV'],
                                      drill: 'USUBJID' }),
  c('gantt-defaults', { chart_type: 'gantt' })
];

const other = [
  c('empty-data', { chart_type: 'bar', ...AE },
    { data: { AETERM: [] } })
];

module.exports = { F, BASE, aggregated, individual, timeline, other };
