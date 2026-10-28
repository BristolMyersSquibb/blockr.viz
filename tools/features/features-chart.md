# Chart block: features and the tests that cover them

Phase 1.1 of `rewrite-plan.md`, chart part. One line per thing a user can
see or do with `new_chart_block()` on the reference (`feat/viz-0.3.0` @
`8651d92`), and the test that covers it. Argument names are the proposed
ones (`api-proposal.md`); the reference's name follows in brackets where it
differs.

Each row ends with one or more `[test: <file> :: <test name>]` links, which
`tools/check-features.R` checks against the test results: the test exists
and passes. The text before the links is where the coverage came from on the
reference.

How to read the "covered by" column:

- `rec:<name>` is a recording in `tests/testthat/test-chart-record.R`
  (scenario in `helper-chart-record.R`, fixture
  `fixtures/chart-record/<name>.rds`), branch `test/chart-record`. It pins
  the state, the emitted filter, the result, the drill sends, downloads and
  what the browser receives (rows, columns, settings as drawn), under the
  public names.
- `R:` is a testthat file, `JS:` a node test in `tests/js/` (drawing
  snapshots in `tests/js/__snapshots__/`).
- **browser** marks a test in `test-shinytest2.R`. Those never run in CI
  (they skip under `R CMD check`), so they count as untested there.
- **none** means no test found.

All tests below pass on the reference with blockr.core `origin/main`
(5,707 R expectations, 0 failures, 49 skips, all browser tests; 602 JS
tests).

## Chart types

| # | Capability | Covered by |
|---|---|---|
| 1 | Bar of counts from one column, orientation from which axis names it | rec:bar_count; R: test-chart-value-labels.R:46; JS: chart-aggregated.test.js (bar-defaults, bar-vertical) [test: test-chart-record.R :: chart recording: bar_count] [test: tests/js/run-case.js :: bar-defaults] [test: tests/js/run-case.js :: bar-vertical] |
| 2 | Bar of an aggregate of a value column | rec:bar_mean_dodge; JS: bar-mean, option-aggregated.test.js [test: test-chart-record.R :: chart recording: bar_mean_dodge] [test: tests/js/run-case.js :: bar-mean] |
| 3 | Histogram: x cut into `bins` with `pretty()` breaks, drawn as bars | rec:histogram; R: test-chart-histogram.R; JS: labels.test.js:119, gear-histogram-counts. Rewrite: drawn in chart/view.test.js and chart/script-band.test.js. [test: test-chart-record.R :: chart recording: histogram] [test: tests/js/chart/view.test.js :: a histogram stands its bins on x] [test: tests/js/chart/script-band.test.js :: a histogram draws every bin R's edges list, the empty ones too] [test: test-chart-derived.R :: chart_hist bins a numeric x and labels the bins as x] |
| 4 | Waterfall: bars float from the running total | rec:waterfall; R: test-chart-block.R:975; JS: waterfall, waterfall-totals-labels [test: test-chart-record.R :: chart recording: waterfall] [test: tests/js/run-case.js :: waterfall-totals-labels] |
| 5 | Pie | rec:pie; JS: pie, pie-facet-selected, click-pie [test: test-chart-record.R :: chart recording: pie] [test: tests/js/run-case.js :: pie-facet-selected] [test: tests/js/run-case.js :: pie] |
| 6 | Treemap | rec:treemap; JS: treemap, click-treemap [test: test-chart-record.R :: chart recording: treemap] [test: tests/js/run-case.js :: treemap] |
| 7 | Boxplot from raw values per category | rec:boxplot; R: test-distribution-marks.R:41; JS: boxplot, boxplot-stats [test: test-chart-record.R :: chart recording: boxplot] [test: tests/js/run-case.js :: boxplot-stats] |
| 8 | Point range from raw values per category | rec:pointrange; R: test-distribution-marks.R:28; JS: pointrange [test: test-chart-record.R :: chart recording: pointrange] |
| 9 | Scatter | rec:scatter_full; R: test-chart-block.R:191; JS: scatter, scatter-defaults [test: test-chart-record.R :: chart recording: scatter_full] [test: tests/js/run-case.js :: scatter-defaults] |
| 10 | Line | rec:line_full; R: test-chart-lines.R:34; JS: line, line-series-color [test: test-chart-record.R :: chart recording: line_full] [test: tests/js/run-case.js :: line-series-color] |
| 11 | Area: a line with its area filled, stacked by colour unless dodged | rec:area_dodge; R: test-chart-lines.R:89; JS: legend.test.js:143, log-axis.test.js:78, gear-area-interval. Rewrite: drawn in chart/option-individual.test.js. [test: test-chart-record.R :: chart recording: area_dodge] [test: tests/js/legend.test.js :: marks on an area name the bands at their end] [test: tests/js/chart/log-axis.test.js :: a line takes y_transform and ignores x_transform; an area ignores both] [test: tests/js/chart/option-individual.test.js :: an area fills and stacks a colour split, from zero, without dots] |
| 12 | Band: windowed median and range over x, outliers as points | rec:band, rec:band_default; R: test-band-series.R, test-chart-push.R:315; JS: band, band-color, band-facet-refs [test: test-chart-record.R :: chart recording: band] [test: test-chart-record.R :: chart recording: band_default] [test: test-band-series.R :: summarize_stat pins each statistic on a known vector] [test: tests/js/run-case.js :: band-color] [test: tests/js/run-case.js :: band-facet-refs] |
| 13 | Band too thin to draw: a note says why | rec:band_too_thin; JS: band-note [test: test-chart-record.R :: chart recording: band_too_thin] [test: tests/js/run-case.js :: band-note] |
| 14 | Gantt: intervals on lanes, open intervals as dots | rec:gantt; R: test-chart-block.R:6; JS: chart-timeline.test.js (gantt, gantt-no-xend) [test: test-chart-record.R :: chart recording: gantt] [test: tests/js/run-case.js :: gantt-no-xend] |
| 15 | Ordered visit axis: `AVISIT` sorted by `AVISITN` | rec:visit_order [test: test-chart-record.R :: chart recording: visit_order] |

## Mapping

| # | Capability | Covered by |
|---|---|---|
| 16 | `x`, `y` name screen axes on every type | rec:set_roles; R: test-chart-legacy.R:144, :163 [test: test-chart-record.R :: chart recording: set_roles] [test: test-chart-legacy.R :: the orientation is inferred from x and y] |
| 17 | `func`: count, count_distinct, pct_distinct, mean, median, sum, min, max | rec:bar_mean_dodge, rec:bar_distinct, rec:bar_pct_distinct, rec:waterfall; R: test-agg-golden.R:165, :224 (R and JS agree for every function); JS: model-aggregated.test.js [test: test-chart-record.R :: chart recording: bar_mean_dodge] [test: test-chart-record.R :: chart recording: bar_distinct] [test: test-chart-record.R :: chart recording: bar_pct_distinct] [test: test-chart-record.R :: chart recording: waterfall] [test: test-agg-golden.R :: golden: JS and R engines agree on the ungrouped grand total] [test: test-agg-golden.R :: na_group = 'drop' agrees across the other aggregations too] |
| 18 | `orientation`: inferred from x and y, set only when both are non-numeric | rec:bar_distinct, rec:set_roles; R: test-chart-legacy.R:144 [test: test-chart-record.R :: chart recording: bar_distinct] [test: test-chart-record.R :: chart recording: set_roles] [test: test-chart-legacy.R :: the orientation is inferred from x and y] |
| 19 | `color` splits marks into levels, at most 15 | rec:bar_mean_dodge, rec:scatter_full; JS: legend.test.js, bar-too-many-colors, bar-palette-overflow [test: test-chart-record.R :: chart recording: bar_mean_dodge] [test: test-chart-record.R :: chart recording: scatter_full] [test: tests/js/run-case.js :: bar-too-many-colors] [test: tests/js/run-case.js :: bar-palette-overflow] |
| 20 | `facet`: one panel per level | rec:bar_fill_facet, rec:scatter_full; JS: bar-facet-fixed, scatter-facet-fixed [test: test-chart-record.R :: chart recording: bar_fill_facet] [test: test-chart-record.R :: chart recording: scatter_full] [test: tests/js/run-case.js :: bar-facet-fixed] [test: tests/js/run-case.js :: scatter-facet-fixed] |
| 21 | `series`: separate lines or point groups, independent of colour | rec:line_full; JS: model-individual.test.js:64, line-series-color [test: test-chart-record.R :: chart recording: line_full] [test: tests/js/run-case.js :: line-series-color] |
| 22 | `xend`: interval end on a gantt | rec:gantt; R: test-chart-block.R:10 [test: test-chart-record.R :: chart recording: gantt] |
| 23 | `label`: text on marks, only where a mark's rows agree | rec:bar_distinct, rec:scatter_full; JS: mark-labels.test.js [test: test-chart-record.R :: chart recording: bar_distinct] [test: test-chart-record.R :: chart recording: scatter_full] |
| 24 | `linetype`: first value solid, others dashed | rec:line_full, rec:set_misc; R: test-chart-lines.R:35; JS: option-individual.test.js:353 [test: test-chart-record.R :: chart recording: line_full] [test: test-chart-record.R :: chart recording: set_misc] |
| 25 | `highlight`: one column's values in colour, the rest grey | rec:scatter_full, rec:set_aesthetics, rec:titles_highlight; R: test-chart-highlight.R; JS: highlight.test.js [test: test-chart-record.R :: chart recording: scatter_full] [test: test-chart-record.R :: chart recording: set_aesthetics] [test: test-chart-record.R :: chart recording: titles_highlight] |
| 26 | `tooltip` [`tt_fields`]: extra columns in each mark's tooltip | rec:bar_missing_drop, rec:scatter_full; JS: labels.test.js:142, bar-tt-fields [test: test-chart-record.R :: chart recording: bar_missing_drop] [test: test-chart-record.R :: chart recording: scatter_full] [test: tests/js/labels.test.js :: bar tooltip: the aggregation by label, the colour rows and tooltip fields too] [test: tests/js/run-case.js :: bar-tt-fields] |
| 27 | `ymin`, `ymax` [`lo`, `hi`]: error bars on a scatter, shaded bands on a line, several nested | rec:line_full, rec:set_misc; R: test-chart-lines.R:35; JS: line-single-step-errorbars, scatter-errorbars-refs [test: test-chart-record.R :: chart recording: line_full] [test: test-chart-record.R :: chart recording: set_misc] [test: tests/js/run-case.js :: line-single-step-errorbars] [test: tests/js/run-case.js :: scatter-errorbars-refs] |
| 28 | `id`: distinct individuals for the band and for counts | rec:band, rec:bar_fill_facet, rec:set_distribution; JS: bar-count-axis-distinct [test: test-chart-record.R :: chart recording: band] [test: test-chart-record.R :: chart recording: bar_fill_facet] [test: test-chart-record.R :: chart recording: set_distribution] [test: tests/js/run-case.js :: bar-count-axis-distinct] |
| 29 | `missing` [`na_group`]: a missing category is its own level, or dropped | rec:bar_missing_drop; R: test-chart-block.R:665, test-table-missing-drill.R; JS: model-aggregated.test.js:33 [test: test-chart-record.R :: chart recording: bar_missing_drop] [test: test-agg-golden.R :: na_group = 'drop' removes those rows from both engines] [test: tests/js/chart/model-aggregated.test.js :: a missing group is its own level; missing drop removes it] |
| 30 | `percent_of` [`pct_of`]: denominator of pct_distinct (facet, category, color, total) | rec:bar_fill_facet, rec:bar_pct_distinct; R: test-agg-pct-distinct.R; JS: format-total.test.js:59 [test: test-chart-record.R :: chart recording: bar_fill_facet] [test: test-chart-record.R :: chart recording: bar_pct_distinct] [test: test-agg-pct-distinct.R :: the denominator is the panel's subjects, not the bars'] [test: tests/js/format-total.test.js :: pct_of "total": one denominator for every panel] |
| 31 | A mapped column missing from the data: optional roles draw as unmapped, the stored pick is kept | rec:heal_missing_column; JS: bar-missing-column [test: tests/js/run-case.js :: bar-missing-column] |
| 32 | A subject in two overlapping groups is drawn in both | R: test-expand-groups.R [test: test-expand-groups.R :: overlap: one copy of each group's member rows, in group order] |

## Look

| # | Capability | Covered by |
|---|---|---|
| 33 | `position` [`bar_mode`]: stack, dodge, fill | rec:bar_mean_dodge, rec:bar_fill_facet, rec:area_dodge, rec:set_misc; JS: bar-color-stacked, -grouped, -percent [test: test-chart-record.R :: chart recording: bar_mean_dodge] [test: test-chart-record.R :: chart recording: bar_fill_facet] [test: test-chart-record.R :: chart recording: area_dodge] [test: test-chart-record.R :: chart recording: set_misc] [test: tests/js/run-case.js :: bar-color-stacked] |
| 34 | `sort_by`: value, data, alpha, onset, a column; default by category type | rec:bar_mean_dodge, rec:gantt, rec:histogram; JS: model-aggregated.test.js:51, :83 [test: test-chart-record.R :: chart recording: bar_mean_dodge] [test: test-chart-record.R :: chart recording: gantt] [test: test-chart-record.R :: chart recording: histogram] [test: tests/js/chart/model-aggregated.test.js :: group order: value, alpha, factor levels, data, a column] [test: tests/js/chart/model-aggregated.test.js :: the default sort follows the category column] |
| 35 | `sort_dir`: asc, desc, default by `sort_by` | rec:bar_mean_dodge, rec:data_changes; JS: bar-sort-value-asc, bar-sort-alpha-desc [test: test-chart-record.R :: chart recording: bar_mean_dodge] [test: test-chart-record.R :: chart recording: data_changes] [test: tests/js/run-case.js :: bar-sort-value-asc] [test: tests/js/run-case.js :: bar-sort-alpha-desc] |
| 36 | `value_labels`: totals on a stack, each bar when dodged, shares when filled | rec:bar_mean_dodge; R: test-chart-value-labels.R; JS: bar-value-labels* [test: test-chart-record.R :: chart recording: bar_mean_dodge] |
| 37 | `format`: number, compact, percent, 0 to 6 decimals | rec:bar_mean_dodge, rec:bar_missing_drop, rec:set_aesthetics; JS: format-total.test.js:11, :24 [test: test-chart-record.R :: chart recording: bar_mean_dodge] [test: test-chart-record.R :: chart recording: bar_missing_drop] [test: test-chart-record.R :: chart recording: set_aesthetics] [test: tests/js/format-total.test.js :: a whole number of decimals prints that many, from a number or its text] [test: tests/js/format-total.test.js :: "number" prints up to two decimals, as the table block] |
| 38 | `unit` after the number, once in the axis title | rec:bar_mean_dodge; JS: labels.test.js:78 [test: test-chart-record.R :: chart recording: bar_mean_dodge] [test: tests/js/labels.test.js :: axisName: the label with the unit; unlabelled, the unit alone, else the name] |
| 39 | `y_transform`, `x_transform` [`y_scale`, `x_scale`]: log10 axis, rows at or below zero left out and counted | rec:bar_missing_drop, rec:scatter_full, rec:set_aesthetics; R: test-chart-log-scale.R; JS: log-axis.test.js [test: test-chart-record.R :: chart recording: bar_missing_drop] [test: test-chart-record.R :: chart recording: scatter_full] [test: test-chart-record.R :: chart recording: set_aesthetics] |
| 40 | `legend`: bottom, top, marks, none; marks falls back where a type has no room | rec:pie, rec:scatter_full, rec:set_aesthetics; JS: legend.test.js [test: test-chart-record.R :: chart recording: pie] [test: test-chart-record.R :: chart recording: scatter_full] [test: test-chart-record.R :: chart recording: set_aesthetics] |
| 41 | `x_ranges`: named shaded stretches of x | rec:scatter_full, rec:set_aesthetics; R: test-chart-lines.R:36; JS: interactions-gear.test.js:344 [test: test-chart-record.R :: chart recording: scatter_full] [test: test-chart-record.R :: chart recording: set_aesthetics] [test: tests/js/chart/option-individual.test.js :: x_ranges shade x, direct labels name the lines and make room] [test: tests/js/core/gear.test.js :: text: lines and ranges parse into their wire forms and back] |
| 42 | `facet_scales`: fixed, free, free_y | rec:bar_fill_facet, rec:scatter_full; JS: bar-facet-free, scatter-facet-free-y, model-aggregated.test.js:133; **browser**: test-shinytest2.R:940, :956 [test: test-chart-record.R :: chart recording: bar_fill_facet] [test: test-chart-record.R :: chart recording: scatter_full] [test: tests/js/chart/model-aggregated.test.js :: facets: fixed scales share one group order, free scales keep their own] [test: tests/js/run-case.js :: bar-facet-free] [test: tests/js/run-case.js :: scatter-facet-free-y] |
| 43 | `facet_cols`: panels per row, auto by width; option `blockr.viz.facet_cols` | rec:bar_fill_facet, rec:set_aesthetics; R: test-facet-cols.R; JS: bar-facet-cols, interactions-lifecycle.test.js:239 [test: test-chart-record.R :: chart recording: bar_fill_facet] [test: test-chart-record.R :: chart recording: set_aesthetics] [test: tests/js/chart/view.test.js :: facet_cols sets the panels per row; unset, the grid fills by width] [test: test-chart-server.R :: the facet_cols option sets the panels per row of a new chart] [test: tests/js/run-case.js :: bar-facet-cols] |
| 44 | `waterfall_totals`: steps drawn as totals | rec:waterfall, rec:set_misc; R: test-chart-block.R:564; JS: interactions-gear.test.js:368 [test: test-chart-record.R :: chart recording: waterfall] [test: test-chart-record.R :: chart recording: set_misc] |
| 45 | Board scale map colours a column the same way everywhere | R: test-scale-map-resolve.R, test-scale-provenance.R; JS: bar-scales [test: test-scale-map-resolve.R :: dd_colored_var follows the per-chart-type table] [test: test-scale-provenance.R :: a picker copy inherits its source column's binding] [test: tests/js/run-case.js :: bar-scales] |
| 46 | Board theme and dark mode | JS: bar-theme-dark, interactions-lifecycle.test.js:146, :168. Rewrite: core/binding.test.js. [test: tests/js/chart/view.test.js :: the theme R names is the ECharts theme; a new one draws new instances] [test: tests/js/core/binding.test.js :: a light/dark switch draws the last value again; an unmounted view is left alone] [test: tests/js/run-case.js :: bar-theme-dark] |
| 47 | Long category labels wrap, turn, thin; a note says how many are hidden | JS: axes-labels.test.js:13, :41; **browser**: test-shinytest2.R:1054, :1062 [test: tests/js/chart/axes-labels.test.js :: x labels: flat when they fit, wrapped flat, then turned] [test: tests/js/chart/axes-labels.test.js :: a note when a category axis has fewer pixels than labels] |

## Marks

| # | Capability | Covered by |
|---|---|---|
| 48 | `smoother`: lm or loess per colour group and panel, on log axes in log space | rec:scatter_full, rec:scatter_loess; R: test-smoother-series.R, test-chart-log-scale.R:51; JS: scatter-smoother* [test: test-chart-record.R :: chart recording: scatter_full] [test: test-chart-record.R :: chart recording: scatter_loess] [test: test-smoother-series.R :: a faceted fit is per panel, not one pooled line] |
| 49 | `identity_line` on a scatter | rec:scatter_full; R: test-chart-block.R:385; JS: scatter-identity, log-axis.test.js:118 [test: test-chart-record.R :: chart recording: scatter_full] [test: tests/js/chart/log-axis.test.js :: the identity line: on log-log over one domain, gone when the scales differ] [test: tests/js/run-case.js :: scatter-identity] |
| 50 | `connect`: straight, monotone, step_start, step_middle, step_end [hyphenated] | rec:line_full, rec:set_aesthetics; JS: option-individual.test.js:54 (step_end only). [test: test-chart-record.R :: chart recording: line_full] [test: test-chart-record.R :: chart recording: set_aesthetics] [test: tests/js/chart/option-individual.test.js :: connect: straight, monotone and the three steps reach the line series] |
| 51 | `line_width`, `point_size` [`line_width_mult`, `dot_size_mult`] | rec:line_full, rec:scatter_full, rec:set_aesthetics; JS: option-individual.test.js:270 [test: test-chart-record.R :: chart recording: line_full] [test: test-chart-record.R :: chart recording: scatter_full] [test: test-chart-record.R :: chart recording: set_aesthetics] |
| 52 | `summary`: boxplot body, point range interval, band ribbon | rec:boxplot, rec:pointrange, rec:band, rec:set_distribution; R: test-distribution-marks.R; JS: model-aggregated.test.js:170, :182. [test: test-band-series.R :: summarize_stat pins each statistic on a known vector] [test: tests/js/chart/model-aggregated.test.js :: summarizeStat: mean_2sd is two sd around the mean, p5_p95 the 5th to 95th percentile] [test: test-chart-record.R :: chart recording: boxplot] [test: test-chart-record.R :: chart recording: pointrange] [test: test-chart-record.R :: chart recording: band] [test: test-chart-record.R :: chart recording: set_distribution] [test: tests/js/chart/model-aggregated.test.js :: summarizeStat: quartiles, Tukey fences clipped to the data, sd and se] [test: tests/js/chart/model-aggregated.test.js :: B8: the 10th-90th percentile draws that range around the median, not Q1-Q3] |
| 53 | `whiskers`: tukey or any summary; default by type | rec:boxplot, rec:band, rec:set_distribution; R: test-distribution-marks.R:152 [test: test-chart-record.R :: chart recording: boxplot] [test: test-chart-record.R :: chart recording: band] [test: test-chart-record.R :: chart recording: set_distribution] |
| 54 | `box_points`: outliers beyond the whiskers | rec:boxplot, rec:set_distribution; JS: boxplot-color-outliers [test: test-chart-record.R :: chart recording: boxplot] [test: test-chart-record.R :: chart recording: set_distribution] [test: tests/js/run-case.js :: boxplot-color-outliers] |
| 55 | `connect_centers` on a point range | rec:pointrange, rec:set_aesthetics; JS: pointrange-color-connect [test: test-chart-record.R :: chart recording: pointrange] [test: test-chart-record.R :: chart recording: set_aesthetics] [test: tests/js/run-case.js :: pointrange-color-connect] |
| 56 | `band_window`, `band_size`, `band_min_n` | rec:band, rec:band_default, rec:set_distribution; R: test-band-series.R:97 [test: test-chart-record.R :: chart recording: band] [test: test-chart-record.R :: chart recording: band_default] [test: test-chart-record.R :: chart recording: set_distribution] [test: test-band-series.R :: compute_band_series nests facet then series, both defaulting] |
| 57 | `ref_min`, `ref_max` [`ref_lo`, `ref_hi`]: reference lines on a band at the column's median | rec:band, rec:set_distribution; R: test-band-series.R; JS: band-facet-refs, labels.test.js:251 [test: test-chart-record.R :: chart recording: band] [test: test-chart-record.R :: chart recording: set_distribution] [test: test-band-series.R :: summarize_stat pins each statistic on a known vector] [test: tests/js/run-case.js :: band-facet-refs] |
| 58 | `y_lines` [`value_lines`], `x_lines`: dashed guide lines, named ones labelled; dates and categories on x | rec:scatter_full, rec:line_full, rec:set_aesthetics; R: test-chart-block.R:443, :447; JS: bar-value-lines, axes-labels.test.js:47 [test: test-chart-record.R :: chart recording: scatter_full] [test: test-chart-record.R :: chart recording: line_full] [test: test-chart-record.R :: chart recording: set_aesthetics] [test: tests/js/chart/axes-labels.test.js :: value lines: a markLine on the first series, the axis widened past it] [test: tests/js/run-case.js :: bar-value-lines] |
| 59 | `show_n` [`count_on`]: "(n)" on axis labels, facet strips or both | rec:bar_fill_facet, rec:set_distribution; JS: bar-count-axis, bar-count-both-facet, gantt-patient-facet-counts [test: test-chart-record.R :: chart recording: bar_fill_facet] [test: test-chart-record.R :: chart recording: set_distribution] [test: tests/js/run-case.js :: bar-count-axis] [test: tests/js/run-case.js :: bar-count-both-facet] [test: tests/js/run-case.js :: gantt-patient-facet-counts] |

## Text

| # | Capability | Covered by |
|---|---|---|
| 60 | Title, subtitle, caption default to the data's label, subtitle, caption attributes | rec:titles_auto; R: test-title-template.R [test: test-chart-record.R :: chart recording: titles_auto] [test: test-title-template.R :: resolve_title_template substitutes column-value tokens] |
| 61 | `""` shows no text | rec:titles_template [test: test-chart-record.R :: chart recording: titles_template] |
| 62 | Template tokens `{col}`, `{label(col)}`, `{n}`, `{n_distinct(col)}` | rec:titles_template; R: test-title-template.R [test: test-chart-record.R :: chart recording: titles_template] [test: test-title-template.R :: resolve_title_template substitutes column-value tokens] |
| 63 | `{filters}`: the filter trail of the data | rec:input_tbl; R: test-title-template.R [test: test-chart-record.R :: chart recording: input_tbl] [test: test-title-template.R :: resolve_title_template substitutes column-value tokens] |
| 64 | `{highlight}` | rec:titles_highlight [test: test-chart-record.R :: chart recording: titles_highlight] |
| 65 | `{@arg}` slots: a setting printed as a word the reader can click to change | rec:titles_template; R: test-chart-legacy.R:185; JS: interactions-sentence.test.js [test: test-chart-record.R :: chart recording: titles_template] [test: test-chart-legacy.R :: title slots take the public names, and a saved old one is renamed] |
| 66 | `[...]` clause drops when its setting is empty, and the setting is offered beside the band | rec:titles_offer; JS: interactions-sentence.test.js:107 [test: test-chart-record.R :: chart recording: titles_offer] [test: tests/js/core/titles.test.js :: the template row shows what the card shows, with its slots and offers] [test: test-title-template.R :: a dropped clause puts its argument on offer] |
| 67 | `{@arg in a, b}` lists the choices a word offers | rec:titles_template; JS: interactions-sentence.test.js:223 [test: test-chart-record.R :: chart recording: titles_template] [test: test-title-template.R :: a slot's choice list narrows its menu, not its word] [test: tests/js/core/titles.test.js :: slots: words open their setting; plain words stay text] |
| 68 | Option `blockr.viz.default_caption` captions every new chart | R: test-chart-block.R:811 [test: test-chart-server.R :: the default_caption option captions a new chart that sets none] |
| 69 | Axis titles and tooltips use column labels | JS: labels.test.js [test: tests/js/labels.test.js :: axisTitle: the label, else the name] [test: tests/js/labels.test.js :: boxplot and point range: the value axis by label] |

## Drill-down

| # | Capability | Covered by |
|---|---|---|
| 70 | `drill`: a click filters downstream on a column, `"auto"` on the type's natural target, `NULL` does nothing | rec:drill_categorical, rec:histogram; R: test-chart-filters.R:53; JS: interactions-click.test.js (click-*-drill-off, -drill-column) [test: test-chart-record.R :: chart recording: drill_categorical] [test: test-chart-record.R :: chart recording: histogram] |
| 71 | Several columns in one click are ANDed | rec:drill_categorical, rec:script_drill; R: test-chart-filters.R:68 [test: test-chart-record.R :: chart recording: drill_categorical] [test: test-chart-record.R :: chart recording: script_drill] |
| 72 | A missing value is a value to click | rec:drill_categorical; R: test-chart-filters.R:81; JS: chart-keys.test.js:104 [test: test-chart-record.R :: chart recording: drill_categorical] [test: tests/js/chart/chart-keys.test.js :: a missing value is a key too: sent as null, named (missing), matched by null] |
| 73 | Clicking the selected mark again, or an empty spot, clears | rec:drill_categorical, rec:drill_point; JS: click-bar-latched, click-bar-empty-click [test: test-chart-record.R :: chart recording: drill_categorical] [test: tests/js/chart/select.test.js :: a bar click selects its category; again, or a click beside the bars, clears] |
| 74 | A click on a histogram bin filters a range of x | rec:histogram; R: test-chart-histogram.R:48 [test: test-chart-record.R :: chart recording: histogram] |
| 75 | A click on a dot without a drill column filters that x and y, dates read from ms (the emitted code is open: behaviour-changes.md row 9) | rec:drill_point, rec:drill_point_date [test: test-chart-server.R :: a click on one point filters its x and y] |
| 76 | A brush on a scatter or line filters an x (and y) range, with its facet panel | rec:drill_range, rec:drill_range_date; R: test-chart-filters.R:121; JS: interactions-brush.test.js; **browser**: test-shinytest2.R:513 [test: test-chart-record.R :: chart recording: drill_range] [test: test-chart-record.R :: chart recording: drill_range_date] |
| 77 | A legend chip click filters its level | JS: legend.test.js:102, click-legend-* [test: tests/js/legend.test.js :: a chip still filters its level, and a second click sends again] |
| 78 | The footer names the filter in column labels | JS: chart-keys.test.js:53, labels.test.js:292 [test: tests/js/chart/chart-keys.test.js :: the footer names the filter in column labels (D4)] [test: tests/js/labels.test.js :: drawn: a click names its filter by label in the footer] |
| 79 | A click on a pooled group filters on its members | R: test-expand-groups.R [test: test-expand-groups.R :: a click on a pooled group filters the input on its members] [test: test-expand-groups.R :: a pooled click claims the source column as a set (overlap)] |
| 80 | A saved selection restores filtered and marked | rec:drill_restore; R: test-chart-filters.R:202; JS: interactions-restore.test.js; **browser**: test-shinytest2.R:859 [test: test-chart-record.R :: chart recording: drill_restore] |
| 81 | `drill_target` [`ctrl_target`], `drill_table` [`ctrl_table`]: a click is sent to a filter block, the chart keeps no selection, the same click sends again | rec:drill_transient, rec:set_misc; R: test-chart-filters.R:151, test-ctrl-*.R; JS: click-*-transient [test: test-chart-record.R :: chart recording: drill_transient] [test: test-chart-record.R :: chart recording: set_misc] |
| 82 | `drill_target = "auto"` finds the board's drill destination | rec:set_misc; R: test-ctrl-auto-target.R [test: test-chart-record.R :: chart recording: set_misc] [test: test-ctrl-auto-target.R :: auto takes the one drill filter block, over any value filters] |
| 83 | Real canvas clicks on bar, pie, scatter | JS (fake ECharts): interactions-click.test.js; **browser**: test-shinytest2.R:472, :491, :501 [test: tests/js/chart/select.test.js :: a bar click selects its category; again, or a click beside the bars, clears] [test: tests/js/chart/select.test.js :: a pie slice and a gantt lane select their level] [test: tests/js/chart/select.test.js :: a scatter point is a zero-width range; a brush its extent] |

## Downloads

| # | Capability | Covered by |
|---|---|---|
| 84 | `download`: the menu shows Excel, Web page, PowerPoint, Image; off hides it | rec:download_scatter, rec:set_aesthetics [test: test-chart-record.R :: chart recording: download_scatter] [test: test-chart-record.R :: chart recording: set_aesthetics] |
| 85 | Excel: one row per mark for aggregated types, the rows otherwise, with title and footer | rec:download_bar, rec:download_scatter; R: test-chart-xlsx.R [test: test-chart-record.R :: chart recording: download_bar] [test: test-chart-record.R :: chart recording: download_scatter] |
| 86 | Image, web page, PowerPoint from the browser's picture | rec:download_capture (a file is written; its content is not checked); JS: interactions-export.test.js [test: test-chart-record.R :: chart recording: download_capture] |
| 87 | No picture yet: the first click on a format waits for the picture; a chart R asks for is drawn offscreen | rec:download_capture; JS: interactions-export.test.js:255, :274 [test: test-chart-record.R :: chart recording: download_capture] [test: tests/js/chart/export.test.js :: capture: a picture once the panels finished rendering, one reply, the host removed] [test: tests/js/core/download.test.js :: a click on a format waits for the picture onOpen is sending] [test: tests/js/chart/view.test.js :: an empty state offers no Image: there is no picture to take] |
| 88 | Board download footer under the picture and in the sheet | R: test-download-footer.R [test: test-download-footer.R :: the footer fills in user, time, filters and provenance fields] |
| 89 | File name from the dataset and the chart's title | R: test-download-filename.R [test: test-download-filename.R :: the name is dataset, title, time] |

## States

| # | Capability | Covered by |
|---|---|---|
| 90 | Busy cue while upstream recomputes or the chart draws | rec (wire layer: `blockr-busy` messages); JS: busy-cue.test.js [test: tests/js/busy-cue.test.js :: a slow draw shows the cue, and the drawn picture ends it] [test: tests/js/busy-cue.test.js :: R ends the cue when no picture is coming] |
| 91 | Empty data draws an empty state, not stale marks | rec:data_changes; JS: empty-data, model-aggregated.test.js:46 [test: test-chart-record.R :: chart recording: data_changes] [test: tests/js/chart/model-aggregated.test.js :: no cells: the empty state] [test: tests/js/run-case.js :: empty-data] |
| 92 | Needs input: no x or y draws nothing and says so | JS: model-individual.test.js:42, boxplot-no-value [test: tests/js/chart/model-individual.test.js :: without x or y the model is empty] [test: tests/js/run-case.js :: boxplot-no-value] |
| 93 | A data change re-sends the rows; a settings change does not | rec:data_changes, rec:wire_transport; R: test-chart-push.R:27, :158 [test: test-chart-record.R :: chart recording: data_changes] |
| 94 | A chart in a hidden panel draws when shown; a late script still gets its rows | rec:wire_transport; JS: interactions-lifecycle.test.js:41, binding.test.js:80 [test: tests/js/core/binding.test.js :: rows missing for a revision are asked for once] [test: tests/js/core/binding.test.js :: a value without rows uses the rows held for its revision] [test: tests/js/busy-cue.test.js :: a chart with no picture counts from when its panel shows] |
| 95 | Input that is not a data frame (composer table) is coerced, annotation columns dropped | rec:input_tbl; R: test-input-contract.R [test: test-chart-record.R :: chart recording: input_tbl] [test: test-input-contract.R :: can_coerce_annotated_df is the one contract predicate] |

## Prepare script (experimental)

| # | Capability | Covered by |
|---|---|---|
| 96 | `script` runs on the input first; its plain-value assignments become controls | rec:script; R: test-chart-prepare.R:27 [test: test-chart-record.R :: chart recording: script] [test: test-chart-prepare.R :: a script transforms the frame and its declarations drive it] |
| 97 | `script_values` hold the controls' settings | rec:script; R: test-chart-prepare.R:190 [test: test-chart-record.R :: chart recording: script] [test: test-chart-prepare.R :: script and values round-trip through block state] |
| 98 | A failing script is reported, nothing is drawn | rec:script; R: test-chart-prepare.R:49 [test: test-chart-record.R :: chart recording: script] [test: test-chart-prepare.R :: every way a script can fail is reported rather than swallowed] |
| 99 | A click on a column the script makes filters after the script | rec:script_drill; R: test-chart-prepare.R:395 [test: test-chart-record.R :: chart recording: script_drill] [test: test-chart-prepare.R :: a click on a column the script makes filters after the script] |

## Gear

Each gear section is covered by the per-type outlines and drives in
`JS: interactions-gear.test.js` (`gear-<type>`, `gear-drive-*`) and, for what
reaches R, by `rec:set_roles`, `rec:set_aesthetics`, `rec:set_distribution`,
`rec:set_misc`.

| # | Capability | Covered by |
|---|---|---|
| 100 | Type picker; switching type keeps the mapping that still fits | rec:set_roles; JS: gear-drive-type-switch; **browser**: test-shinytest2.R:528 [test: test-chart-record.R :: chart recording: set_roles] |
| 101 | Every control writes its setting, and the gear's echo of unchanged values writes nothing | rec:set_*; rec:wire_gear_echoes; R: test-chart-push.R [test: test-chart-record.R :: chart recording: set_roles] [test: test-chart-record.R :: chart recording: set_aesthetics] [test: test-chart-record.R :: chart recording: set_distribution] [test: test-chart-record.R :: chart recording: set_misc] |
| 102 | Junk from a control (non-numeric bins, band size, unknown legend or format) keeps the setting | rec:histogram, rec:set_aesthetics, rec:set_distribution [test: test-chart-record.R :: chart recording: histogram] [test: test-chart-record.R :: chart recording: set_aesthetics] [test: test-chart-record.R :: chart recording: set_distribution] |
| 103 | The gear's column menus show labels | JS: labels.test.js:301 [test: tests/js/labels.test.js :: drawn: the gear's column menus show each label] |

## Constructor and saved boards

| # | Capability | Covered by |
|---|---|---|
| 104 | Invalid type, orientation, position, interval, legend, format, percent_of, connect, missing are errors | rec ctor-cases (test-chart-record.R) [test: test-chart-record.R :: chart recording: constructor cases fail and warn as before] |
| 105 | Saved-board shapes heal: `list()` for NULL, "on"/"off" logicals, script as lines | rec:ctor_normalize [test: test-chart-record.R :: chart recording: ctor_normalize] |
| 106 | Names from blockr.viz < 0.3.0 in code: translated with a warning | rec:ctor_legacy, rec:ctor_legacy_hi; R: test-chart-legacy.R:17 [test: test-chart-record.R :: chart recording: ctor_legacy] [test: test-chart-record.R :: chart recording: ctor_legacy_hi] [test: test-chart-legacy.R :: old names in code are translated, with a warning] |
| 107 | A board saved with 0.2.x restores silently, old names never come back on re-save | R: test-chart-legacy.R:105, :115, :135 [test: test-chart-legacy.R :: old names never come back on a re-save] [test: test-chart-legacy.R :: a board saved with direct_labels restores as legend = marks] |
| 108 | Starting state of an unserved block (what a board saves before it runs) | rec (each scenario's `initial`) [test: test-chart-record.R :: chart recording: bar_count] [test: test-chart-record.R :: chart recording: scatter_full] |
| 109 | Registry entry for the block list and the assistant | rec registry (test-chart-record.R) [test: test-chart-record.R :: chart recording: constructor cases fail and warn as before] |
| 110 | Description for the assistant | R: test-describe-chart.R [test: test-describe-chart.R :: the description names what is mapped, in chart vocabulary] |

## Without a test, or only in the browser suite

Items 1 to 3 of the reference's list (area and histogram drawing, `connect = "step_start"` and `"step_middle"`, `mean_2sd` and `p5_p95`) have tests on the rewrite; see rows 3, 11, 50 and 52.

4. **Download content.** Image, web page and PowerPoint downloads are checked only for being written.
5. **The nine chart tests in `test-shinytest2.R`** (real canvas clicks on bar, pie and scatter, brush, type switch in the gear, shared and free facet domains, boxplot slot centring, label overflow note, restored footer) never run in CI.
6. **`config_effect()` for blockr.ai** (`R/drilldown-ai-effect.R`): no test.
7. **Board theme option** `new_drilldown_theme_option()`: no R test; the JS side is tested.

Dead tests to drop with the rewrite: the JS radar cases (`radar*`,
`click-radar-*`, `gear-radar`, `restore-radar`, `model-aggregated.test.js:163`)
draw a type the constructor no longer accepts.

## Differences the rewrite may show

- **`connect` written when `connect_centers` changes.** The reference reads
  `msg$connect` with `$`, which partially matches `connect_centers`; a
  config message without a `connect` field then sets the line style to the
  centers' "on"/"off". The gear always sends `connect`, so no user sees it.
  The recordings avoid it (`rec:set_aesthetics` sends `connect` along); the
  wire scenario `rec:wire_gear_echoes` records it.
