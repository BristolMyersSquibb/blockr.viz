# Chart block: features and the tests that cover them

Phase 1.1 of `rewrite-plan.md`, chart part. One line per thing a user can
see or do with `new_chart_block()` on the reference (`feat/viz-0.3.0` @
`8651d92`), and the test that covers it. Argument names are the proposed
ones (`api-proposal.md`); the reference's name follows in brackets where it
differs.

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
| 1 | Bar of counts from one column, orientation from which axis names it | rec:bar_count; R: test-chart-value-labels.R:46; JS: chart-aggregated.test.js (bar-defaults, bar-vertical) |
| 2 | Bar of an aggregate of a value column | rec:bar_mean_dodge; JS: bar-mean, option-aggregated.test.js |
| 3 | Histogram: x cut into `bins` with `pretty()` breaks, drawn as bars | rec:histogram; R: test-chart-histogram.R; JS: labels.test.js:119, gear-histogram-counts. **No drawing snapshot.** |
| 4 | Waterfall: bars float from the running total | rec:waterfall; R: test-chart-block.R:975; JS: waterfall, waterfall-totals-labels |
| 5 | Pie | rec:pie; JS: pie, pie-facet-selected, click-pie |
| 6 | Treemap | rec:treemap; JS: treemap, click-treemap |
| 7 | Boxplot from raw values per category | rec:boxplot; R: test-distribution-marks.R:41; JS: boxplot, boxplot-stats |
| 8 | Point range from raw values per category | rec:pointrange; R: test-distribution-marks.R:28; JS: pointrange |
| 9 | Scatter | rec:scatter_full; R: test-chart-block.R:191; JS: scatter, scatter-defaults |
| 10 | Line | rec:line_full; R: test-chart-lines.R:34; JS: line, line-series-color |
| 11 | Area: a line with its area filled, stacked by colour unless dodged | rec:area_dodge; R: test-chart-lines.R:89; JS: legend.test.js:143, log-axis.test.js:78, gear-area-interval. **No drawing snapshot.** |
| 12 | Band: windowed median and range over x, outliers as points | rec:band, rec:band_default; R: test-band-series.R, test-chart-push.R:315; JS: band, band-color, band-facet-refs |
| 13 | Band too thin to draw: a note says why | rec:band_too_thin; JS: band-note |
| 14 | Gantt: intervals on lanes, open intervals as dots | rec:gantt; R: test-chart-block.R:6; JS: chart-timeline.test.js (gantt, gantt-no-xend) |
| 15 | Ordered visit axis: `AVISIT` sorted by `AVISITN` | rec:visit_order |

## Mapping

| # | Capability | Covered by |
|---|---|---|
| 16 | `x`, `y` name screen axes on every type | rec:set_roles; R: test-chart-legacy.R:144, :163 |
| 17 | `func`: count, count_distinct, pct_distinct, mean, median, sum, min, max | rec:bar_mean_dodge, rec:bar_distinct, rec:bar_pct_distinct, rec:waterfall; R: test-agg-golden.R:165, :224 (R and JS agree for every function); JS: model-aggregated.test.js |
| 18 | `orientation`: inferred from x and y, set only when both are non-numeric | rec:bar_distinct, rec:set_roles; R: test-chart-legacy.R:144 |
| 19 | `color` splits marks into levels, at most 15 | rec:bar_mean_dodge, rec:scatter_full; JS: legend.test.js, bar-too-many-colors, bar-palette-overflow |
| 20 | `facet`: one panel per level | rec:bar_fill_facet, rec:scatter_full; JS: bar-facet-fixed, scatter-facet-fixed |
| 21 | `series`: separate lines or point groups, independent of colour | rec:line_full; JS: model-individual.test.js:64, line-series-color |
| 22 | `xend`: interval end on a gantt | rec:gantt; R: test-chart-block.R:10 |
| 23 | `label`: text on marks, only where a mark's rows agree | rec:bar_distinct, rec:scatter_full; JS: mark-labels.test.js |
| 24 | `linetype`: first value solid, others dashed | rec:line_full, rec:set_misc; R: test-chart-lines.R:35; JS: option-individual.test.js:353 |
| 25 | `highlight`: one column's values in colour, the rest grey | rec:scatter_full, rec:set_aesthetics, rec:titles_highlight; R: test-chart-highlight.R; JS: highlight.test.js |
| 26 | `tooltip` [`tt_fields`]: extra columns in each mark's tooltip | rec:bar_missing_drop, rec:scatter_full; JS: labels.test.js:142, bar-tt-fields |
| 27 | `ymin`, `ymax` [`lo`, `hi`]: error bars on a scatter, shaded bands on a line, several nested | rec:line_full, rec:set_misc; R: test-chart-lines.R:35; JS: line-single-step-errorbars, scatter-errorbars-refs |
| 28 | `id`: distinct individuals for the band and for counts | rec:band, rec:bar_fill_facet, rec:set_distribution; JS: bar-count-axis-distinct |
| 29 | `missing` [`na_group`]: a missing category is its own level, or dropped | rec:bar_missing_drop; R: test-chart-block.R:665, test-table-missing-drill.R; JS: model-aggregated.test.js:33 |
| 30 | `percent_of` [`pct_of`]: denominator of pct_distinct (facet, category, color, total) | rec:bar_fill_facet, rec:bar_pct_distinct; R: test-agg-pct-distinct.R; JS: format-total.test.js:59 |
| 31 | A mapped column missing from the data: optional roles draw as unmapped, the stored pick is kept | rec:heal_missing_column; JS: bar-missing-column |
| 32 | A subject in two overlapping groups is drawn in both | R: test-expand-groups.R |

## Look

| # | Capability | Covered by |
|---|---|---|
| 33 | `position` [`bar_mode`]: stack, dodge, fill | rec:bar_mean_dodge, rec:bar_fill_facet, rec:area_dodge, rec:set_misc; JS: bar-color-stacked, -grouped, -percent |
| 34 | `sort_by`: value, data, alpha, onset, a column; default by category type | rec:bar_mean_dodge, rec:gantt, rec:histogram; JS: model-aggregated.test.js:51, :83 |
| 35 | `sort_dir`: asc, desc, default by `sort_by` | rec:bar_mean_dodge, rec:data_changes; JS: bar-sort-value-asc, bar-sort-alpha-desc |
| 36 | `value_labels`: totals on a stack, each bar when dodged, shares when filled | rec:bar_mean_dodge; R: test-chart-value-labels.R; JS: bar-value-labels* |
| 37 | `format`: number, compact, percent, 0 to 6 decimals | rec:bar_mean_dodge, rec:bar_missing_drop, rec:set_aesthetics; JS: format-total.test.js:11, :24 |
| 38 | `unit` after the number, once in the axis title | rec:bar_mean_dodge; JS: labels.test.js:78 |
| 39 | `y_transform`, `x_transform` [`y_scale`, `x_scale`]: log10 axis, rows at or below zero left out and counted | rec:bar_missing_drop, rec:scatter_full, rec:set_aesthetics; R: test-chart-log-scale.R; JS: log-axis.test.js |
| 40 | `legend`: bottom, top, marks, none; marks falls back where a type has no room | rec:pie, rec:scatter_full, rec:set_aesthetics; JS: legend.test.js |
| 41 | `x_ranges`: named shaded stretches of x | rec:scatter_full, rec:set_aesthetics; R: test-chart-lines.R:36; JS: interactions-gear.test.js:344 |
| 42 | `facet_scales`: fixed, free, free_y | rec:bar_fill_facet, rec:scatter_full; JS: bar-facet-free, scatter-facet-free-y, model-aggregated.test.js:133; **browser**: test-shinytest2.R:940, :956 |
| 43 | `facet_cols`: panels per row, auto by width; option `blockr.viz.facet_cols` | rec:bar_fill_facet, rec:set_aesthetics; R: test-facet-cols.R; JS: bar-facet-cols, interactions-lifecycle.test.js:239 |
| 44 | `waterfall_totals`: steps drawn as totals | rec:waterfall, rec:set_misc; R: test-chart-block.R:564; JS: interactions-gear.test.js:368 |
| 45 | Board scale map colours a column the same way everywhere | R: test-scale-map-resolve.R, test-scale-provenance.R; JS: bar-scales |
| 46 | Board theme and dark mode | JS: bar-theme-dark, interactions-lifecycle.test.js:146, :168 |
| 47 | Long category labels wrap, turn, thin; a note says how many are hidden | JS: axes-labels.test.js:13, :41; **browser**: test-shinytest2.R:1054, :1062 |

## Marks

| # | Capability | Covered by |
|---|---|---|
| 48 | `smoother`: lm or loess per colour group and panel, on log axes in log space | rec:scatter_full, rec:scatter_loess; R: test-smoother-series.R, test-chart-log-scale.R:51; JS: scatter-smoother* |
| 49 | `identity_line` on a scatter | rec:scatter_full; R: test-chart-block.R:385; JS: scatter-identity, log-axis.test.js:118 |
| 50 | `connect`: straight, monotone, step_start, step_middle, step_end [hyphenated] | rec:line_full, rec:set_aesthetics; JS: option-individual.test.js:54 (step_end only). **step_start and step_middle are not drawn in any test.** |
| 51 | `line_width`, `point_size` [`line_width_mult`, `dot_size_mult`] | rec:line_full, rec:scatter_full, rec:set_aesthetics; JS: option-individual.test.js:270 |
| 52 | `summary`: boxplot body, point range interval, band ribbon | rec:boxplot, rec:pointrange, rec:band, rec:set_distribution; R: test-distribution-marks.R; JS: model-aggregated.test.js:170, :182. **mean_2sd and p5_p95 are not tested.** |
| 53 | `whiskers`: tukey or any summary; default by type | rec:boxplot, rec:band, rec:set_distribution; R: test-distribution-marks.R:152 |
| 54 | `box_points`: outliers beyond the whiskers | rec:boxplot, rec:set_distribution; JS: boxplot-color-outliers |
| 55 | `connect_centers` on a point range | rec:pointrange, rec:set_aesthetics; JS: pointrange-color-connect |
| 56 | `band_window`, `band_size`, `band_min_n` | rec:band, rec:band_default, rec:set_distribution; R: test-band-series.R:97 |
| 57 | `ref_min`, `ref_max` [`ref_lo`, `ref_hi`]: reference lines on a band at the column's median | rec:band, rec:set_distribution; R: test-band-series.R; JS: band-facet-refs, labels.test.js:251 |
| 58 | `y_lines` [`value_lines`], `x_lines`: dashed guide lines, named ones labelled; dates and categories on x | rec:scatter_full, rec:line_full, rec:set_aesthetics; R: test-chart-block.R:443, :447; JS: bar-value-lines, axes-labels.test.js:47 |
| 59 | `show_n` [`count_on`]: "(n)" on axis labels, facet strips or both | rec:bar_fill_facet, rec:set_distribution; JS: bar-count-axis, bar-count-both-facet, gantt-patient-facet-counts |

## Text

| # | Capability | Covered by |
|---|---|---|
| 60 | Title, subtitle, caption default to the data's label, subtitle, caption attributes | rec:titles_auto; R: test-title-template.R |
| 61 | `""` shows no text | rec:titles_template |
| 62 | Template tokens `{col}`, `{label(col)}`, `{n}`, `{n_distinct(col)}` | rec:titles_template; R: test-title-template.R |
| 63 | `{filters}`: the filter trail of the data | rec:input_tbl; R: test-title-template.R |
| 64 | `{highlight}` | rec:titles_highlight |
| 65 | `{@arg}` slots: a setting printed as a word the reader can click to change | rec:titles_template; R: test-chart-legacy.R:185; JS: interactions-sentence.test.js |
| 66 | `[...]` clause drops when its setting is empty, and the setting is offered beside the band | rec:titles_offer; JS: interactions-sentence.test.js:107 |
| 67 | `{@arg in a, b}` lists the choices a word offers | rec:titles_template; JS: interactions-sentence.test.js:223 |
| 68 | Option `blockr.viz.default_caption` captions every new chart | R: test-chart-block.R:811 |
| 69 | Axis titles and tooltips use column labels | JS: labels.test.js |

## Drill-down

| # | Capability | Covered by |
|---|---|---|
| 70 | `drill`: a click filters downstream on a column, `"auto"` on the type's natural target, `NULL` does nothing | rec:drill_categorical, rec:histogram; R: test-chart-filters.R:53; JS: interactions-click.test.js (click-*-drill-off, -drill-column) |
| 71 | Several columns in one click are ANDed | rec:drill_categorical, rec:script_drill; R: test-chart-filters.R:68 |
| 72 | A missing value is a value to click | rec:drill_categorical; R: test-chart-filters.R:81; JS: chart-keys.test.js:104 |
| 73 | Clicking the selected mark again, or an empty spot, clears | rec:drill_categorical, rec:drill_point; JS: click-bar-latched, click-bar-empty-click |
| 74 | A click on a histogram bin filters a range of x | rec:histogram; R: test-chart-histogram.R:48 |
| 75 | A click on a dot without a drill column filters that x and y, dates read from ms | rec:drill_point, rec:drill_point_date |
| 76 | A brush on a scatter or line filters an x (and y) range, with its facet panel | rec:drill_range, rec:drill_range_date; R: test-chart-filters.R:121; JS: interactions-brush.test.js; **browser**: test-shinytest2.R:513 |
| 77 | A legend chip click filters its level | JS: legend.test.js:102, click-legend-* |
| 78 | The footer names the filter in column labels | JS: chart-keys.test.js:53, labels.test.js:292 |
| 79 | A click on a pooled group filters on its members | R: test-expand-groups.R |
| 80 | A saved selection restores filtered and marked | rec:drill_restore; R: test-chart-filters.R:202; JS: interactions-restore.test.js; **browser**: test-shinytest2.R:859 |
| 81 | `drill_target` [`ctrl_target`], `drill_table` [`ctrl_table`]: a click is sent to a filter block, the chart keeps no selection, the same click sends again | rec:drill_transient, rec:set_misc; R: test-chart-filters.R:151, test-ctrl-*.R; JS: click-*-transient |
| 82 | `drill_target = "auto"` finds the board's drill destination | rec:set_misc; R: test-ctrl-auto-target.R |
| 83 | Real canvas clicks on bar, pie, scatter | JS (fake ECharts): interactions-click.test.js; **browser**: test-shinytest2.R:472, :491, :501 |

## Downloads

| # | Capability | Covered by |
|---|---|---|
| 84 | `download`: the menu shows Excel, Web page, PowerPoint, Image; off hides it | rec:download_scatter, rec:set_aesthetics |
| 85 | Excel: one row per mark for aggregated types, the rows otherwise, with title and footer | rec:download_bar, rec:download_scatter; R: test-chart-xlsx.R |
| 86 | Image, web page, PowerPoint from the browser's picture | rec:download_capture (a file is written; its content is not checked); JS: interactions-export.test.js |
| 87 | No picture yet: the chart is drawn offscreen and the next click finds it | rec:download_capture; JS: interactions-export.test.js:255, :274 |
| 88 | Board download footer under the picture and in the sheet | R: test-download-footer.R |
| 89 | File name from the dataset and the chart's title | R: test-download-filename.R |

## States

| # | Capability | Covered by |
|---|---|---|
| 90 | Busy cue while upstream recomputes or the chart draws | rec (wire layer: `blockr-busy` messages); JS: busy-cue.test.js |
| 91 | Empty data draws an empty state, not stale marks | rec:data_changes; JS: empty-data, model-aggregated.test.js:46 |
| 92 | Needs input: no x or y draws nothing and says so | JS: model-individual.test.js:42, boxplot-no-value |
| 93 | A data change re-sends the rows; a settings change does not | rec:data_changes, rec:wire_transport; R: test-chart-push.R:27, :158 |
| 94 | A chart in a hidden panel draws when shown; a late script still gets its rows | rec:wire_transport; JS: interactions-lifecycle.test.js:41, binding.test.js:80 |
| 95 | Input that is not a data frame (composer table) is coerced, annotation columns dropped | rec:input_tbl; R: test-input-contract.R |

## Prepare script (experimental)

| # | Capability | Covered by |
|---|---|---|
| 96 | `script` runs on the input first; its plain-value assignments become controls | rec:script; R: test-chart-prepare.R:27 |
| 97 | `script_values` hold the controls' settings | rec:script; R: test-chart-prepare.R:190 |
| 98 | A failing script is reported, nothing is drawn | rec:script; R: test-chart-prepare.R:49 |
| 99 | A click on a column the script makes filters after the script | rec:script_drill; R: test-chart-prepare.R:395 |

## Gear

Each gear section is covered by the per-type outlines and drives in
`JS: interactions-gear.test.js` (`gear-<type>`, `gear-drive-*`) and, for what
reaches R, by `rec:set_roles`, `rec:set_aesthetics`, `rec:set_distribution`,
`rec:set_misc`.

| # | Capability | Covered by |
|---|---|---|
| 100 | Type picker; switching type keeps the mapping that still fits | rec:set_roles; JS: gear-drive-type-switch; **browser**: test-shinytest2.R:528 |
| 101 | Every control writes its setting, and the gear's echo of unchanged values writes nothing | rec:set_*; rec:wire_gear_echoes; R: test-chart-push.R |
| 102 | Junk from a control (non-numeric bins, band size, unknown legend or format) keeps the setting | rec:histogram, rec:set_aesthetics, rec:set_distribution |
| 103 | The gear's column menus show labels | JS: labels.test.js:301 |

## Constructor and saved boards

| # | Capability | Covered by |
|---|---|---|
| 104 | Invalid type, orientation, position, interval, legend, format, percent_of, connect, missing are errors | rec ctor-cases (test-chart-record.R) |
| 105 | Saved-board shapes heal: `list()` for NULL, "on"/"off" logicals, script as lines | rec:ctor_normalize |
| 106 | Names from blockr.viz < 0.3.0 in code: translated with a warning | rec:ctor_legacy, rec:ctor_legacy_hi; R: test-chart-legacy.R:17 |
| 107 | A board saved with 0.2.x restores silently, old names never come back on re-save | R: test-chart-legacy.R:105, :115, :135 |
| 108 | Starting state of an unserved block (what a board saves before it runs) | rec (each scenario's `initial`) |
| 109 | Registry entry for the block list and the assistant | rec registry (test-chart-record.R); R: test-block-arguments.R, test-block-arguments-drift.R |
| 110 | Description for the assistant | R: test-describe-chart.R |

## Without a test, or only in the browser suite

1. **Area and histogram drawing.** No drawing snapshot for either type; only legend, axis and gear tests touch them. Add `area` and `histogram` cases to `tests/js/cases.js`.
2. **`connect = "step_start"` and `"step_middle"`.** R passes them through (rec:line_full); no JS test draws them.
3. **`summary` / `whiskers` = `mean_2sd` and `p5_p95`.** The statistic is not computed in any test.
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
