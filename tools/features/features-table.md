# Feature catalogue: table block

What `new_table_block()` does for users on the reference, `feat/viz-0.3.0`
@ `8651d92`, and which test covers each capability. The table part of
`features.md` (rewrite plan, Phase 1). Written 2026-10-27.

The recordings are on local branch `test/table-record` (worktree
`_worktrees/viz-table-record`). Paths below are inside blockr.viz.

Each row ends with one or more `[test: <file> :: <test name>]` links to a
test on the rewrite, which `tools/check-features.R` checks against the test
results. `test-table-golden.R :: table recording: <name>` replays recording
`G:<name>`; the constructor and restore cases are one test each. The text
before the links is where the coverage came from on the reference.

How to read the "covered by" column:

- `G:name` is recording `name` in `tests/testthat/test-table-golden.R`
  (scenarios in `helper-table-golden.R`, fixtures in
  `tests/testthat/fixtures/table-golden/`). `G:ctor:name` and
  `G:restore:name` are the constructor and restore cases.
- `R:file` is an existing testthat file, `JS:file` a file in `tests/js/`.
- `R:file (browser)` is a shinytest2 test that skips without Chrome and under
  R CMD check. It did not run here.
- **none** means no test on the reference exercises it. Those lines are
  listed again at the end.

A recording holds, per step: the state under the constructor's argument
names, the expression sent downstream and its result, the block
conditions, every custom message to the browser (payloads parsed), the
rendered HTML outputs (status line, download menu, chrome), control-channel
sends, and a downloaded file where the step is a download. 69 scenarios with
154 steps, 27 constructor cases, 18 restored blocks, and the registry entry.
All green on the reference with blockr.core `origin/main` (`da3fcb4`) and
blockr.ui local `main` (`708ef52`), and identical across two fresh runs in
different order.

## Layouts

| Capability | Covered by |
|---|---|
| `layout = "records"`: the input rows as they are | G:rec_default, G:rec_columns; R:test-table-block-server.R [test: test-table-golden.R :: table recording: rec_default] [test: test-table-golden.R :: table recording: rec_columns] [test: tests/js/table/fixtures.test.js :: fixture rec_default] [test: test-table-model.R :: table_model_records() gives the recorded records cells] |
| records with `by`: rows nested under one header per level (`table_nest()`) | G:rec_by, G:rec_by_missing; R:test-table-gear-gaps.R [test: test-table-golden.R :: table recording: rec_by] [test: test-table-golden.R :: table recording: rec_by_missing] [test: test-table-tile.R :: the records layout nests by `by` and outputs the input rows] [test: tests/js/table/dom.test.js :: nested sections: both levels, search keeps the headers of what matches] |
| records with a finished table as input (annotated data frame): drawn with its structure | G:rec_structured, G:rec_source, G:rec_download_structured [test: test-table-golden.R :: table recording: rec_structured] [test: test-table-golden.R :: table recording: rec_source] [test: test-table-golden.R :: table recording: rec_download_structured] [test: test-table-structured.R :: table_structured_view() draws what the reference sent] |
| records with an object that has an `as_annotated_df()` method | G:rec_input_tbl; R:test-table-block-annotated.R [test: test-table-golden.R :: table recording: rec_input_tbl] [test: test-table-block-annotated.R :: block expr coerces a table-producing input to the annotated df] |
| `layout = "groups"`: one row per level of `by`, one column per summary | G:grp_default, G:grp_all_types [test: test-table-golden.R :: table recording: grp_default] [test: test-table-golden.R :: table recording: grp_all_types] [test: tests/js/table/fixtures.test.js :: fixture grp_default] [test: test-table-groups-view.R :: table_groups_view() draws what the reference sent] |
| groups without `by`: one Total row, no sort, search or drill | G:grp_no_by, G:grp_cards_no_by; R:test-table-groups-no-by.R; JS:table/groups.test.js ("without by there is no search or header sorting") [test: test-table-golden.R :: table recording: grp_no_by] [test: test-table-golden.R :: table recording: grp_cards_no_by] [test: tests/js/table/groups.test.js :: without by there is no search or header sorting] [test: test-table-cards.R :: without `by` the cards draw one row and no drill] |
| groups with two `by` columns: parent rows that fold | G:grp_two_by; JS:table/groups.test.js ("nested groups: parents fold their terms") [test: test-table-golden.R :: table recording: grp_two_by] [test: tests/js/table/groups.test.js :: nested groups: parents fold their terms; search keeps the parent] |
| `layout = "variables"`: Table 1, one column per level of `by` | G:var_default, G:var_options [test: test-table-golden.R :: table recording: var_default] [test: test-table-golden.R :: table recording: var_options] [test: test-table-model.R :: table_model_variables() builds the recorded Table 1] [test: tests/js/table/variables.test.js :: the variables layout draws its Table 1 with sections and the drill] |
| variables with no variable picked: the input passes through | G:var_none [test: test-table-golden.R :: table recording: var_none] [test: test-table-model.R :: table_model_variables() returns the data without variables] |
| layout switch keeps `by`, `summaries`, titles and toggles | G:layout_switch [test: test-table-golden.R :: table recording: layout_switch] [test: tests/js/table/records.test.js :: another layout replaces the records view and its table] |
| switching to groups with no summaries seeds one count column | G:layout_switch_seed [test: test-table-golden.R :: table recording: layout_switch_seed] [test: test-normalize.R :: a groups table always has a column] |
| an unknown layout value is ignored by the switch; in code it is an error now (behaviour-changes.md row 7) | G:layout_switch (last step), G:ctor:bad_layout [test: test-table-golden.R :: table recording: layout_switch] [test: test-table-golden.R :: table recording: constructor cases] |
| layout tiles in the gear follow the pushed layout | R:test-table-layout-switch.R (browser) [test: tests/js/core/gear.test.js :: the table follows its own spec: layout tiles and per-layout rows] [test: tests/js/table/records.test.js :: the gear engine, when loaded, is mounted in a tray and updated after the layout] |

## Looks

| Capability | Covered by |
|---|---|
| `look = "table"` | every scenario without `look` [test: test-table-golden.R :: table recording: rec_default] [test: test-table-golden.R :: table recording: grp_default] [test: test-table-golden.R :: table recording: var_default] |
| `look = "compact"`, records: rotated column titles | G:rec_compact, G:ctor:legacy_compact, G:ctor:legacy_rotate [test: test-table-golden.R :: table recording: rec_compact] [test: tests/js/table/records.test.js :: the compact look turns the column titles on their side] [test: tests/js/table/records.test.js :: table.css rotates the titles the compact look marks] [test: test-table-golden.R :: table recording: constructor cases] [test: test-table-cards.R :: a board saved with `compact` restores in the compact look] |
| `look = "compact"`, groups: square tile cells, a heatmap | G:grp_compact; R:test-table-tile.R, R:test-summarize-compact-width.R [test: test-table-golden.R :: table recording: grp_compact] [test: test-table-tile.R :: compact marks the table and the export carries the paint] [test: tests/js/table/groups.test.js :: a tile is painted with its value; numbers and text draw as they are] |
| `look = "cards"`, records: one card per column | G:rec_cards; R:test-table-cards.R [test: test-table-golden.R :: table recording: rec_cards] [test: test-table-cards.R :: the records view carries cards in the records layout only] [test: tests/js/table/records.test.js :: the cards look draws a card per column and drills on a card row] |
| `look = "cards"`, groups: one row of cards per level, spans left out | G:grp_cards, G:grp_cards_no_by; R:test-table-cards.R [test: test-table-golden.R :: table recording: grp_cards] [test: test-table-golden.R :: table recording: grp_cards_no_by] [test: test-table-cards.R :: the groups view carries one card per drawable summary] [test: tests/js/table/groups.test.js :: a card draws its column's mark without the value] |
| `look = "cards"` in the variables layout draws a table | G:var_cards [test: test-table-golden.R :: table recording: var_cards] [test: tests/js/table/variables.test.js :: the cards look draws a Table 1 as a table] |
| an unknown look from the gear keeps the setting (behaviour-changes.md row 14); in code it is an error (row 7) | G:ctor:bad_look, G:rec_gear [test: test-table-golden.R :: table recording: rec_gear] [test: test-table-golden.R :: table recording: constructor cases] |
| the cards' browser rendering (`inst/js/table/cards.js`) | JS:table/records.test.js ("the cards look draws a card per column"), JS:table/static.test.js [test: tests/js/table/records.test.js :: the cards look draws a card per column and drills on a card row] [test: tests/js/table/records.test.js :: search narrows the card rows] [test: tests/js/table/records.test.js :: a single row of cards names the statistic in the card] [test: tests/js/table/records.test.js :: the cards follow the width of their host] [test: tests/js/table/groups.test.js :: hover: a card glyph gives its column's card] |

## Arguments

| Argument and values | Covered by |
|---|---|
| `by`: records nesting, groups rows (one or two), variables columns | G:rec_by, G:grp_default, G:grp_two_by, G:var_default [test: test-table-golden.R :: table recording: rec_by] [test: test-table-golden.R :: table recording: grp_default] [test: test-table-golden.R :: table recording: grp_two_by] [test: test-table-golden.R :: table recording: var_default] |
| `rowname` | G:rec_columns, G:rec_gear [test: test-table-golden.R :: table recording: rec_columns] [test: test-table-golden.R :: table recording: rec_gear] |
| `columns`, `NULL` = all | G:rec_columns, G:rec_default [test: test-table-golden.R :: table recording: rec_columns] [test: test-table-golden.R :: table recording: rec_default] |
| `summaries`, type `simple`: `func` count, count_distinct, sum, mean; `show` bar, number, dot, tile | G:grp_all_types, G:grp_compact [test: test-table-golden.R :: table recording: grp_all_types] [test: test-table-golden.R :: table recording: grp_compact] [test: tests/js/table/groups.test.js :: a bar: the track, its fill and the value with its percentage] |
| `simple` with `func` median, min, max, identity | R:test-summarize-table.R, R:test-groups-summaries.R [test: test-groups-summaries.R :: simple rows take median, min and max of their column] [test: test-groups-summaries.R :: identity rides through simple rows: the value as-is] |
| `simple` colour split, `position` stack, dodge, fill | G:grp_color_split; R:test-table-vocabulary.R [test: test-table-golden.R :: table recording: grp_color_split] [test: test-table-vocabulary.R :: position sets a colour split's layout] [test: tests/js/table/groups.test.js :: a colour split: one segment per level with a value, its label inside] [test: tests/js/table/groups.test.js :: a grouped split with labels draws one labelled track per level] |
| `facet`, `facet_order`, `fill`; `facet_order` is dropped on the rewrite and fixed at count (spec-table.md) | G:grp_facet, G:grp_compact; R:test-table-tile.R [test: test-table-golden.R :: table recording: grp_facet] [test: test-table-golden.R :: table recording: grp_compact] [test: test-groups-summaries.R :: facet is the SUMMARY's mapping: only mapped columns repeat] [test: test-table-tile.R :: tile columns order by count and carry the worst level] [test: test-legacy-restore.R :: summary entries and shading rules take the new keys] |
| type `dist`: `show` box, pointrange, text; `summary`, `whiskers`, `style` | G:grp_all_types; R:test-groups-summaries.R, R:test-groups-stats.R [test: test-table-golden.R :: table recording: grp_all_types] [test: test-groups-summaries.R :: box and dot are ONE mark: style over centre + inner + outer] [test: test-groups-stats.R :: the table's summary and whiskers specs are the chart's enums] [test: tests/js/table/groups.test.js :: a box: whiskers, caps, box, median; nothing without a median] [test: tests/js/table/groups.test.js :: a point range: fence, range and centre] |
| type `field` | G:grp_all_types, G:grp_facet [test: test-table-golden.R :: table recording: grp_all_types] [test: test-table-golden.R :: table recording: grp_facet] [test: test-groups-summaries.R :: the field join is distinct values with a fold cap, never first()] |
| type `series`: `band`, `ref` | G:grp_all_types; R:test-groups-summaries.R [test: test-table-golden.R :: table recording: grp_all_types] [test: test-groups-summaries.R :: a series ref computes a pooled line, mean_sd adds the band] [test: tests/js/table/groups.test.js :: a sparkline: band, reference line, line and the last point] |
| type `spans`: `label`, `tooltip`, `size` | G:grp_all_types, G:grp_cards [test: test-table-golden.R :: table recording: grp_all_types] [test: test-table-golden.R :: table recording: grp_cards] [test: test-groups-summaries.R :: spans label/fields enrich tips and key the highlight] [test: tests/js/table/groups.test.js :: a swimlane: its segments in their level colours, the axis in the foot] |
| type `pair`: `x_func`, `xend_func`, `ref_lo`, `ref_hi`, `ref`, `dash` | G:grp_all_types; JS:table/groups.test.js ("hover: a box names its statistics; a pair its clauses") [test: test-table-golden.R :: table recording: grp_all_types] [test: tests/js/table/groups.test.js :: a pair: reference, link, from and an open to; dashed by its level] [test: tests/js/table/groups.test.js :: hover: a box names its statistics; a pair its clauses] |
| type `expr`, including a failing expression | G:grp_all_types, G:grp_custom_error; R:test-groups-summaries.R [test: test-table-golden.R :: table recording: grp_all_types] [test: test-table-golden.R :: table recording: grp_custom_error] [test: test-groups-summaries.R :: expr rows evaluate per group and fail as a cell, not a crash] |
| type `custom` | G:grp_custom_error; R:test-groups-custom.R [test: test-table-golden.R :: table recording: grp_custom_error] [test: test-groups-custom.R :: a bare body using `d` works like a function] [test: test-groups-custom.R :: a facet runs the function per facet level] |
| `denom` key | R:test-agg-pct-distinct.R [test: test-groups-custom.R :: a custom count with `denom` prints n (%) against the slice's N] [test: test-groups-custom.R :: a custom count with `denom` keeps N on its header] |
| old summary keys (`stat`, `from`/`to`, `lo`/`hi`, `fields`) | G:ctor:old_summary_keys, G:grp_gear; R:test-table-vocabulary.R [test: test-table-golden.R :: table recording: constructor cases] [test: test-table-golden.R :: table recording: grp_gear] [test: test-table-vocabulary.R :: old summary keys read as the public ones] [test: test-legacy-restore.R :: summary entries and shading rules take the new keys] |
| old aggregation list `list(func, cols)` under `summaries` | G:ctor:agg_summaries [test: test-table-golden.R :: table recording: constructor cases] [test: test-groups-summaries.R :: the ctor moves a ranked-bar grouping into `by`] |
| empty `summaries` in groups: one count column | G:ctor:groups_seed, G:grp_gear [test: test-table-golden.R :: table recording: constructor cases] [test: test-table-golden.R :: table recording: grp_gear] [test: test-normalize.R :: a groups table always has a column] |
| `id`: count distinct subjects in the variables layout | G:var_options, G:var_widget [test: test-table-golden.R :: table recording: var_options] [test: test-table-golden.R :: table recording: var_widget] [test: test-summary-table-distinct.R :: summary_table() distinct-subject denominator drops NA ids end-to-end] |
| `pct_of` facet, total | G:grp_facet; R:test-table-vocabulary.R; G:ctor:bad_pct_of [test: test-table-golden.R :: table recording: grp_facet] [test: test-table-vocabulary.R :: pct_of = total takes a facet copy's share of the whole table] [test: test-table-golden.R :: table recording: constructor cases] |
| `missing` level, drop: records headers, groups rows, variables levels | G:rec_by_missing, G:rec_by, G:grp_missing, G:var_missing; R:test-table-missing-drill.R; G:ctor:bad_missing [test: test-table-golden.R :: table recording: rec_by_missing] [test: test-table-golden.R :: table recording: rec_by] [test: test-table-golden.R :: table recording: grp_missing] [test: test-table-golden.R :: table recording: var_missing] [test: test-table-vocabulary.R :: missing = level shows a missing group as its own row] [test: test-table-golden.R :: table recording: constructor cases] |
| `color` a column (row tint), `""` off | G:rec_shadings, G:rec_color_off, G:rec_gear [test: test-table-golden.R :: table recording: rec_shadings] [test: test-table-golden.R :: table recording: rec_color_off] [test: test-table-golden.R :: table recording: rec_gear] [test: test-table-model.R :: the row tint follows the color setting] |
| `color = NULL` tints rows from the board's scale map; `color = "auto"` on the rewrite (behaviour-changes.md row 5) | R:test-table-model.R [test: test-table-model.R :: color = auto tints rows from the board's scale map] [test: test-legacy-restore.R :: the table's color = NULL means auto only before the rewrite] |
| `shadings` modes sequential, diverging, bar, `domain` | G:rec_shadings, G:rec_gear, G:rec_download; R:test-table-exhibit.R [test: test-table-golden.R :: table recording: rec_shadings] [test: test-table-golden.R :: table recording: rec_gear] [test: test-table-golden.R :: table recording: rec_download] [test: test-table-exhibit.R :: the download frame carries the screen's cell paint (#43)] [test: tests/js/table/dom.test.js :: cell fills, ink and data bars become styles] [test: tests/js/table/records.test.js :: the shading editor round-trips every key of a rule] |
| `shadings` with `source` | R:test-table-shading-source.R [test: test-table-shading-source.R :: plain-column source paints the rule's columns by that column] [test: test-table-shading-source.R :: template source pairs each column with its companion and hides it] |
| `format` number, compact, percent, 0 to 6; clamping, strings | G:rec_columns, G:rec_format_compact, G:rec_format_percent, G:grp_format, G:grp_format_digits, G:ctor:bad_format, G:ctor:format_clamp, G:ctor:format_string_number [test: test-table-golden.R :: table recording: rec_columns] [test: test-table-golden.R :: table recording: rec_format_compact] [test: test-table-golden.R :: table recording: rec_format_percent] [test: test-table-golden.R :: table recording: grp_format] [test: test-table-golden.R :: table recording: grp_format_digits] [test: test-table-golden.R :: table recording: constructor cases] [test: test-table-exhibit.R :: fixed decimals print exactly, number keeps small values] |
| `unit` | G:rec_columns, G:rec_cards, G:grp_format, G:grp_cards [test: test-table-golden.R :: table recording: rec_columns] [test: test-table-golden.R :: table recording: rec_cards] [test: test-table-golden.R :: table recording: grp_format] [test: test-table-golden.R :: table recording: grp_cards] [test: test-table-exhibit.R :: compact, percent and unit reach cells, cards and downloads] |
| `facet_layout` by_summary, by_level | G:grp_facet [test: test-table-golden.R :: table recording: grp_facet] [test: test-groups-summaries.R :: by_level reorders into level groups with a two-row header] |
| `sort_by` value, data, alpha, `"label"`, a summary name, a facet level | G:grp_default, G:grp_sort, G:grp_facet, G:ctor:sort_label [test: test-table-golden.R :: table recording: grp_default] [test: test-table-golden.R :: table recording: grp_sort] [test: test-table-golden.R :: table recording: grp_facet] [test: test-table-golden.R :: table recording: constructor cases] [test: test-groups-summaries.R :: sort_by a summary's name orders by that column] [test: test-groups-summaries.R :: sort_by data honors factor levels, else first appearance] |
| `sort_dir` desc, asc | G:grp_sort [test: test-table-golden.R :: table recording: grp_sort] [test: test-table-model.R :: the groups sort direction defaults as the chart's: by value down, else up] |
| `top_n` | G:grp_sort, G:grp_titles [test: test-table-golden.R :: table recording: grp_sort] [test: test-table-golden.R :: table recording: grp_titles] |
| `value_labels`, `axis` | G:grp_display [test: test-table-golden.R :: table recording: grp_display] [test: tests/js/table/groups.test.js :: values off: the marks draw without their numbers] [test: tests/js/table/groups.test.js :: the axis strip sits under its header, with the value slot] [test: test-groups-summaries.R :: axis = FALSE drops every strip] |
| `bar_width` narrow, medium, wide, fit, invalid | G:grp_display (narrow, wide, invalid), G:grp_default (fit) [test: test-table-golden.R :: table recording: grp_display] [test: test-table-golden.R :: table recording: grp_default] [test: tests/js/table/groups.test.js :: bar_width: narrow, medium and wide set the table width; fit leaves it] |
| `bar_width = "medium"` | JS:table/groups.test.js [test: tests/js/table/groups.test.js :: bar_width: narrow, medium and wide set the table width; fit leaves it] |
| `drill`: records column, `"auto"`, `"source"`; groups any value; variables | G:rec_drill, G:rec_structured, G:rec_source, G:grp_drill, G:var_drill [test: test-table-golden.R :: table recording: rec_drill] [test: test-table-golden.R :: table recording: rec_structured] [test: test-table-golden.R :: table recording: rec_source] [test: test-table-golden.R :: table recording: grp_drill] [test: test-table-golden.R :: table recording: var_drill] [test: test-table-expr.R :: a flat records click filters its column] |
| `drill_target`, `drill_table` | G:rec_drill_target, G:grp_drill_target; R:test-table-drill.R (browser) [test: test-table-golden.R :: table recording: rec_drill_target] [test: test-table-golden.R :: table recording: grp_drill_target] [test: tests/js/table/drill.test.js :: with a drill target every click is sent and nothing stays lit] |
| `search`, `sortable` | G:rec_toggles, G:grp_display; R:test-table-display-toggles.R [test: test-table-golden.R :: table recording: rec_toggles] [test: test-table-golden.R :: table recording: grp_display] [test: tests/js/table/dom.test.js :: search narrows the rows and the count, and says when nothing matches] [test: tests/js/table/dom.test.js :: without header sorting the headers do not sort] |
| `collapsible` | G:rec_toggles, G:rec_download_by; R:test-table-display-toggles.R; JS:table/dom.test.js ("without collapsible the sections are plain labels") [test: test-table-golden.R :: table recording: rec_toggles] [test: test-table-golden.R :: table recording: rec_download_by] [test: tests/js/table/dom.test.js :: without collapsible the sections are plain labels] [test: tests/js/table/dom.test.js :: sections head their rows; a click on the header folds it] |
| `max_height`, `NULL`, the old `"600px"` dropped; gone on the rewrite, the table scrolls with its panel (api-proposal.md section 2) | G:rec_toggles, G:grp_display, G:ctor:max_height_legacy [test: test-table-golden.R :: table recording: rec_toggles] [test: test-table-golden.R :: table recording: grp_display] [test: test-table-golden.R :: table recording: constructor cases] [test: tests/js/table/dom.test.js :: a long table draws only a window of rows and follows the scroll] |
| `download` | G:rec_toggles, G:rec_download, G:grp_download [test: test-table-golden.R :: table recording: rec_toggles] [test: test-table-golden.R :: table recording: rec_download] [test: test-table-golden.R :: table recording: grp_download] [test: tests/js/table/records.test.js :: download off hides the download slot only] |
| `title`, `subtitle`, `caption`: `NULL` auto, `""` none, template | G:rec_titles, G:rec_titles_auto, G:grp_titles; R:test-title-template.R [test: test-table-golden.R :: table recording: rec_titles] [test: test-table-golden.R :: table recording: rec_titles_auto] [test: test-table-golden.R :: table recording: grp_titles] [test: test-title-template.R :: resolve_block_title implements the three tiers] |
| `caption` default `getOption("blockr.viz.default_caption")` | G:rec_default_caption, G:grp_default_caption [test: test-table-golden.R :: table recording: rec_default_caption] [test: test-table-golden.R :: table recording: grp_default_caption] |
| `stats`, `sections`, `total`, `total_label`, `nest` | G:var_options, G:var_nest, G:var_widget; R:test-summary-table-stats.R [test: test-table-golden.R :: table recording: var_options] [test: test-table-golden.R :: table recording: var_nest] [test: test-table-golden.R :: table recording: var_widget] [test: tests/js/core/gear.test.js :: the variables layout: Group by, Section by, Individual id, Label and Nest send their keys] |
| `script`, `script_values`, script error | G:rec_script, G:grp_script; R:test-summarize-prepare.R [test: test-table-golden.R :: table recording: rec_script] [test: test-table-golden.R :: table recording: grp_script] [test: test-kit-server.R :: a browser script is dropped unless the option allows it] |
| `selection`: a saved click restores | G:rec_drill_restore, G:grp_drill_restore, G:restore:tile_by_region; R:test-summarize-drill-path.R [test: test-table-golden.R :: table recording: rec_drill_restore] [test: test-table-golden.R :: table recording: grp_drill_restore] [test: test-table-golden.R :: table recording: restored boards] [test: tests/js/table/drill.test.js :: R's selection lights its rows and writes R's receipt] |
| old names in `...` (`value`, `group`, `digits`, `compact`, `rotate_titles`, per-format downloads, `cell_color`, `ctrl_*`, `filter_*`) | G:ctor:legacy_*; R:test-table-legacy.R [test: test-table-golden.R :: table recording: constructor cases] [test: test-legacy-ctor.R :: old table names in code are translated, with one warning] |
| block attributes in `...` (`block_name`) | G:ctor:block_attrs [test: test-table-golden.R :: table recording: constructor cases] |
| an unknown argument (`colour =`) is accepted without a word; an error on the rewrite (behaviour-changes.md row 7) | G:ctor:unknown_arg [test: test-table-golden.R :: table recording: constructor cases] |
| `list()` slots from a pre-#144 paste heal to `NULL` | G:ctor:list_healing [test: test-table-golden.R :: table recording: constructor cases] [test: test-table-block.R :: empty-list state from a pre-#144 DAG paste heals to the defaults] |
| saved state round-trips (`blockr_ser`, `blockr_deser`) | every G:ctor case (`roundtrip`) [test: test-table-golden.R :: table recording: constructor cases] [test: test-table-block.R :: block state round-trips constructor args] |

## Gear controls

The recordings send what the reference's gear sends (`action = "config"`,
the key, the value) and record the state that results.

| Control | Covered by |
|---|---|
| records gear: Row names, Columns, Color by, shading rules, Numbers, Unit, Missing, Header sorting, Sections, Search, Download, Look, Filter on, Title, Subtitle, Caption | G:rec_gear; JS:table/records.test.js ("the gear hides what a finished input cannot change") [test: test-table-golden.R :: table recording: rec_gear] [test: tests/js/table/records.test.js :: the gear engine, when loaded, is mounted in a tray and updated after the layout] [test: tests/js/core/gear.test.js :: the table follows its own spec: layout tiles and per-layout rows] |
| records gear: Nest by (`group` key) and its Missing | G:rec_by [test: test-table-golden.R :: table recording: rec_by] |
| records gear: the old `digits` and `rotate_titles` keys | G:rec_gear [test: test-table-golden.R :: table recording: rec_gear] |
| records gear sections and field order | JS:core/gear.test.js, table/records.test.js [test: tests/js/core/gear.test.js :: sections come in tray order with spec labels] [test: tests/js/table/records.test.js :: the gear hides what a finished input cannot change] |
| groups gear: One row per, the columns list, Facet layout, Sort, Order, Top, Search, Header sorting, Column axis, Values, Bar width, Percent of, Missing, Numbers, Unit, Download, Look, Filter on, Title, Subtitle, Caption | G:grp_gear, G:grp_sort, G:grp_display, G:grp_facet, G:grp_missing, G:grp_drill [test: test-table-golden.R :: table recording: grp_gear] [test: test-table-golden.R :: table recording: grp_sort] [test: test-table-golden.R :: table recording: grp_display] [test: test-table-golden.R :: table recording: grp_facet] [test: test-table-golden.R :: table recording: grp_missing] [test: test-table-golden.R :: table recording: grp_drill] [test: tests/js/core/gear.test.js :: columns: several picks, empty to null where the spec allows it] |
| groups gear: unknown and empty keys are ignored | G:grp_gear [test: test-table-golden.R :: table recording: grp_gear] [test: test-kit-input.R :: a settings message is read against the spec] [test: test-kit-server.R :: a rejected key is logged once] |
| groups gear column editor in the browser: colour split, show, reorder, remove | JS:table/summaries.test.js ("move, remove, add and fold") [test: tests/js/table/summaries.test.js :: move, remove, add and fold] [test: tests/js/table/summaries.test.js :: a field edit sends the whole list with that entry changed] |
| groups gear column editor in the browser: `func`, `col`, `facet` and the type-specific fields | JS:table/summaries.test.js [test: tests/js/table/summaries.test.js :: an entry's fields come from its type, with the spec's rules] [test: tests/js/table/summaries.test.js :: one entry per summary: its name, what it computes, a chevron] |
| variables widget: Variables, Group by, Section by, Statistics, Total and label, Nest, Subject id | G:var_widget (R side); JS:table/variables.test.js ("the gear's Variables row picks among the columns not in by") [test: test-table-golden.R :: table recording: var_widget] [test: tests/js/table/variables.test.js :: a pick of variables becomes entries; kept entries keep their keys] [test: tests/js/core/gear.test.js :: enums in a columns control (table stats)] |
| variables widget pickers other than Statistics, in the browser | JS:core/gear.test.js [test: tests/js/core/gear.test.js :: the variables layout: Group by, Section by, Individual id, Label and Nest send their keys] [test: tests/js/table/variables.test.js :: the gear's Variables row picks among the columns not in by] |
| Send to filter: target and table | G:rec_drill_target, G:grp_drill_target [test: test-table-golden.R :: table recording: rec_drill_target] [test: test-table-golden.R :: table recording: grp_drill_target] |
| Send to filter: the list of target blocks on the board | R:test-ctrl-auto-target.R [test: test-ctrl-auto-target.R :: the table's payload lists the board's filter blocks to send to] |
| script section: the script and its value controls (`sv_*`) | G:rec_script, G:grp_script [test: test-table-golden.R :: table recording: rec_script] [test: test-table-golden.R :: table recording: grp_script] |
| Escape closes the gear | JS:core/gear-tray.test.js [test: tests/js/core/gear-tray.test.js :: Escape closes the gear tray of a chart or table view, and focus goes to the gear] |
| the layout tiles | G:layout_switch, G:layout_switch_seed [test: test-table-golden.R :: table recording: layout_switch] [test: test-table-golden.R :: table recording: layout_switch_seed] [test: tests/js/core/gear.test.js :: the table follows its own spec: layout tiles and per-layout rows] |

## Drill and clear

| Capability | Covered by |
|---|---|
| records: a click on a value filters downstream, `==` or `%in%` | G:rec_drill; R:test-table-block-server.R [test: test-table-golden.R :: table recording: rec_drill] [test: test-table-expr.R :: a flat records click filters its column] [test: tests/js/table/drill.test.js :: a row click claims the drill value and lights every row with it] |
| records: clear (null column) | G:rec_drill, G:rec_by [test: test-table-golden.R :: table recording: rec_drill] [test: test-table-golden.R :: table recording: rec_by] [test: test-table-expr.R :: no click keeps every row] |
| records: the row click in the browser and the second click that clears | R:test-table-drill.R (browser); R:test-drill-lifecycle.R (markup only) [test: tests/js/table/drill.test.js :: a row click claims the drill value and lights every row with it] [test: tests/js/table/drill.test.js :: a second click on a lit row clears the drill] [test: tests/js/table/drill.test.js :: the clear button on the receipt clears the drill] |
| records nested: a header click claims the group, `(Missing)` claims NA rows | G:rec_by, G:rec_by_missing; R:test-table-missing-drill.R [test: test-table-golden.R :: table recording: rec_by] [test: test-table-golden.R :: table recording: rec_by_missing] [test: tests/js/table/drill.test.js :: nested records: a row claims its groups and its drill value] [test: tests/js/table/drill.test.js :: a keyed section value claims the section; its chevron still folds] |
| structured input: a cell or header click with its column half | G:rec_structured, G:var_drill [test: test-table-golden.R :: table recording: rec_structured] [test: test-table-golden.R :: table recording: var_drill] [test: test-table-expr.R :: a structured click ANDs its identity keys] [test: tests/js/table/drill.test.js :: Table 1: a row claims its level, a cell adds its column] [test: tests/js/table/drill.test.js :: a keyed header claims its column; its arrow still sorts] |
| `drill = "source"`: the click resolves to source rows (`drill_source()`) | G:rec_source [test: test-table-golden.R :: table recording: rec_source] [test: test-table-expr.R :: the source mode wraps the filter and adds the column's arm] |
| groups: a row click, a parent path, clear, `clear_filter` | G:grp_drill, G:grp_two_by; JS:table/groups.test.js ("a row click claims the group and lights its row; again clears") [test: test-table-golden.R :: table recording: grp_drill] [test: test-table-golden.R :: table recording: grp_two_by] [test: test-table-expr.R :: a groups click is the filters as they are] [test: tests/js/table/groups.test.js :: a row click claims the group and lights its row; again clears] |
| groups: a click on `(Missing)` keeps the NA rows | G:grp_missing; R:test-table-missing-drill.R [test: test-table-golden.R :: table recording: grp_missing] |
| re-aiming or clearing the drill drops the filter; removing `by` drops it | G:grp_drill, G:rec_drill [test: test-table-golden.R :: table recording: grp_drill] [test: test-table-golden.R :: table recording: rec_drill] |
| a click with a prepare script filters before or after the script | G:grp_script, G:rec_script; R:test-summarize-prepare.R [test: test-table-golden.R :: table recording: grp_script] [test: test-table-golden.R :: table recording: rec_script] |
| status line "Filtered: ..." (records) | G:rec_drill, G:rec_by (output `records-dt_status`) [test: test-table-golden.R :: table recording: rec_drill] [test: test-table-golden.R :: table recording: rec_by] [test: tests/js/table/drill.test.js :: R's selection lights its rows and writes R's receipt] |
| status line (groups, drawn in the browser) | JS:table/drill.test.js ("the clear button on the receipt clears the drill") [test: test-table-golden.R :: table recording: grp_drill] [test: tests/js/table/drill.test.js :: the clear button on the receipt clears the drill] |
| transient drill: every click sends, the same row twice sends twice, a clear is inert, nothing filters here | G:rec_drill_target, G:grp_drill_target [test: test-table-golden.R :: table recording: rec_drill_target] [test: test-table-golden.R :: table recording: grp_drill_target] [test: test-kit-block-server.R :: with a drill target a click is an event, not a selection] [test: tests/js/table/drill.test.js :: with a drill target every click is sent and nothing stays lit] |
| pooled groups of an overlap definition | R:test-expand-groups.R [test: test-expand-groups.R :: summarize table counts per group, percent over subjects] [test: test-expand-groups.R :: a click on a pooled group filters the input on its members] |

## Downloads

| Capability | Covered by |
|---|---|
| menu offers Excel, Web page, PowerPoint (records) and Image (groups) while `download` is on, nothing when off | G:rec_download, G:grp_download (outputs `records-dt_download`, `groups-summarize_download`) [test: test-table-golden.R :: table recording: rec_download] [test: test-table-golden.R :: table recording: grp_download] [test: test-export.R :: viz_downloads() lists the formats and writes them on click] [test: tests/js/table/records.test.js :: the download control lists the formats R offers] |
| Excel, records: values, cell fills of the shadings, title, subtitle, caption, number format | G:rec_download; R:test-write-annotated-xlsx.R, R:test-table-exhibit.R [test: test-table-golden.R :: table recording: rec_download] [test: test-write-annotated-xlsx.R :: write_annotated_xlsx() writes subtitle and caption rows] [test: test-table-exhibit.R :: Excel keeps the full value under a rounding number format (#37)] |
| Excel, nested records, structured input, variables | G:rec_download_by, G:rec_download_structured, G:var_download [test: test-table-golden.R :: table recording: rec_download_by] [test: test-table-golden.R :: table recording: rec_download_structured] [test: test-table-golden.R :: table recording: var_download] |
| Excel, groups: the numbers behind the marks | G:grp_download; R:test-summarize-download.R [test: test-table-golden.R :: table recording: grp_download] |
| Excel, cards: the values | G:rec_download_cards, G:grp_download_cards [test: test-table-golden.R :: table recording: rec_download_cards] [test: test-table-golden.R :: table recording: grp_download_cards] |
| Web page for every layout and look; follows `collapsible` and `sortable` | G:rec_download, G:rec_download_by, G:rec_download_cards, G:rec_download_structured, G:grp_download, G:grp_download_cards, G:var_download; R:test-write-exhibit.R [test: test-table-golden.R :: table recording: rec_download] [test: test-table-golden.R :: table recording: rec_download_by] [test: test-table-golden.R :: table recording: rec_download_cards] [test: test-table-golden.R :: table recording: rec_download_structured] [test: test-table-golden.R :: table recording: grp_download] [test: test-table-golden.R :: table recording: grp_download_cards] [test: test-table-golden.R :: table recording: var_download] [test: test-write-exhibit.R :: write_exhibit_html() honours collapsible = FALSE] [test: test-write-exhibit.R :: write_exhibit_html() honours sortable = FALSE] |
| PowerPoint builds: records, nested, structured, cards, groups painted and captured, variables | same G scenarios ("written: pptx") [test: test-table-golden.R :: table recording: rec_download] [test: test-table-golden.R :: table recording: rec_download_by] [test: test-table-golden.R :: table recording: rec_download_structured] [test: test-table-golden.R :: table recording: rec_download_cards] [test: test-table-golden.R :: table recording: grp_download] [test: test-table-golden.R :: table recording: var_download] [test: test-table-cards.R :: the cards exhibit writes html with the model and a painted slide] |
| PowerPoint content: slides, fitting, carry-over | R:test-write-exhibit.R [test: test-write-exhibit.R :: write_exhibit_pptx() writes one slide holding a NATIVE table] [test: test-write-exhibit.R :: a table too tall for one slide is carried onto the next] [test: test-write-exhibit.R :: shrinking to fit beats splitting] |
| Image, groups: painted fallback, then the browser's capture | G:grp_download [test: test-table-golden.R :: table recording: grp_download] [test: tests/js/table/groups.test.js :: opening the download menu sends R the picture, once per payload] |
| Image, records: not offered; the handler errors | G:rec_download (step 5) [test: test-table-golden.R :: table recording: rec_download] |
| file name: dataset, title, time | every download step; R:test-download-filename.R [test: test-download-filename.R :: the name is dataset, title, time] |
| download footer and exhibit font options | R:test-download-footer.R, R:test-exhibit-font.R [test: test-download-footer.R :: the table block's downloads carry the footer in every format] [test: test-exhibit-font.R :: the floor resolves argument, then board, then option, then 11] |
| a failed download shows a notification | R:test-export.R [test: test-export.R :: a failed download shows a notification and still fails] |
| a deck asks the groups table for its picture through the capture service | JS:table/groups.test.js, core/capture.test.js [test: tests/js/table/groups.test.js :: the picture draws every row off screen without tools, and goes] [test: tests/js/core/capture.test.js :: a picture is drawn in an off-screen host that then goes] |

## Title slots

| Capability | Covered by |
|---|---|
| `{n}`, `{label(col)}`, `{n_distinct(col)}`, `[ ... ]` optional clauses | G:rec_titles, G:grp_titles; R:test-title-template.R [test: test-table-golden.R :: table recording: rec_titles] [test: test-table-golden.R :: table recording: grp_titles] [test: test-title-template.R :: n and n_distinct tokens count rows and values] [test: test-title-template.R :: an optional segment leaves with its token] |
| `NULL` takes the input's `label`, `subtitle`, `caption` attributes | G:rec_titles_auto, G:grp_titles [test: test-table-golden.R :: table recording: rec_titles_auto] [test: test-table-golden.R :: table recording: grp_titles] [test: test-title-template.R :: input_display_attrs reads label, subtitle and caption from raw input] |
| groups `{@by}`, `{@top_n}`, `{@func in ...}` | G:grp_titles [test: test-table-golden.R :: table recording: grp_titles] [test: test-title-template.R :: a slot's choice list narrows its menu, not its word] |
| groups `{@group}`, `{@parent}`, `{@value}`, `{@id_var}`, `{@color}`, `{@facet}`, `{@fill}`, `{@sort_by}`, `{@drill}` | R:test-title-template.R [test: test-title-template.R :: the groups layout's slots print its settings and its first summary] |
| prepare-script values as slots | R:test-summarize-exhibit.R, R:test-summarize-download.R [test: test-chart-prepare.R :: a script's values are title slots, at the script's default until set] |
| records and variables: `{@arg}` prints blank; resolves on the rewrite (behaviour-changes.md row 15) | G:rec_titles_slots, G:var_titles_slots [test: test-table-golden.R :: table recording: rec_titles_slots] [test: test-table-golden.R :: table recording: var_titles_slots] |
| the resolved titles in the downloads | G:rec_download, G:grp_download [test: test-table-golden.R :: table recording: rec_download] [test: test-table-golden.R :: table recording: grp_download] [test: test-summarize-exhibit.R :: the HTML renderer uses resolved exhibit titles] |
| the face's clickable title words and offers | G:grp_titles (payload `titles`) [test: test-table-golden.R :: table recording: grp_titles] [test: tests/js/core/titles.test.js :: slots: words open their setting; plain words stay text] [test: tests/js/table/records.test.js :: titles paint through the title slots when they are loaded] |

## Empty, error and needs-input states

| Capability | Covered by |
|---|---|
| no input connected | G:rec_no_input, G:grp_no_input, G:var_no_input [test: test-table-golden.R :: table recording: rec_no_input] [test: test-table-golden.R :: table recording: grp_no_input] [test: test-table-golden.R :: table recording: var_no_input] |
| input that is not a data frame: block error | G:rec_not_df; R:test-input-contract.R [test: test-table-golden.R :: table recording: rec_not_df] [test: test-table-block-annotated.R :: table block dat_valid accepts data frames and coercible objects] |
| zero rows | G:rec_transport, G:grp_transport [test: test-table-golden.R :: table recording: rec_transport] [test: test-table-golden.R :: table recording: grp_transport] |
| a column gone after an upstream change | G:rec_transport, G:grp_transport (`df_no_cat`) [test: test-table-golden.R :: table recording: rec_transport] [test: test-table-golden.R :: table recording: grp_transport] |
| prepare script error | G:rec_script, G:grp_script [test: test-table-golden.R :: table recording: rec_script] [test: test-table-golden.R :: table recording: grp_script] |
| failing `expr` or `custom` summary | G:grp_custom_error [test: test-table-golden.R :: table recording: grp_custom_error] [test: test-groups-custom.R :: a custom summary that fails only on the parent rows is one failed column] |
| a payload that fails to build shows a red bar in the table | JS:table/records.test.js; R:test-table-model.R [test: tests/js/table/records.test.js :: an error from R shows in the frame] [test: test-table-model.R :: table_model_groups() passes a preparation error on] [test: test-table-groups-view.R :: a failed preparation has no view] |
| an identical payload is not sent again; `_ready` re-sends | G:rec_transport, G:grp_transport [test: test-table-golden.R :: table recording: rec_transport] [test: test-table-golden.R :: table recording: grp_transport] [test: test-kit-output.R :: an identical payload is not sent again] [test: test-kit-output.R :: _need makes the next render send the rows again] |

## Restore of saved boards

Saved by blockr.viz 0.2.223 (`c79f5fb`, main before the table rewrite):
`fixtures/tile-board-main.json` and `fixtures/table-golden/old-boards.json`
(written by `dev/table-golden-old-boards.R`).

| Board | Covered by |
|---|---|
| tile: grouped summaries, totals, long frame, wide frame | G:restore:tile_*; R:test-tile-deser.R [test: test-table-golden.R :: table recording: restored boards] [test: test-tile-deser.R :: the whole saved board restores] |
| table block (records): plain; all settings, old colour args, per-format downloads, `rotate_titles`, saved click, `600px`; shadings; in-block aggregation with and without `group`; ctrl | G:restore:old_table_*; R:test-table-legacy.R [test: test-table-golden.R :: table recording: restored boards] [test: test-legacy-restore.R :: a table saved by the reference restores under the new names] |
| summarize table: ranked bar (mean with colour, count distinct with parent and facet, identity with fields); summaries with old keys and saved path click; script with values and ctrl | G:restore:old_summarize_*; R:test-groups-one-list.R [test: test-table-golden.R :: table recording: restored boards] [test: test-legacy-restore.R :: an old table with group and typed summaries keeps its summaries] |
| summary table: flat, with sections and nest, the interim `state` blob | G:restore:old_summary_* [test: test-table-golden.R :: table recording: restored boards] [test: test-legacy-restore.R :: a summary table block restores in the variables layout] |
| the superseded constructors `new_summarize_table_block()`, `new_summary_table_block()` | G:ctor:superseded_* [test: test-table-golden.R :: table recording: constructor cases] |
| gt table, `drilldown_table_block` | dropped (decision); not recorded [test: test-legacy-restore.R :: blocks dropped for good have no restore] [test: test-table-vocabulary.R :: gt_table is off the block list, the table block is on it] |
| AE heatmap | restored by blockr.pharma `feat/ae-heatmap-table` as `new_table_block(layout = "groups", look = "compact")`, which G:grp_compact covers; not recorded here [test: test-table-golden.R :: table recording: grp_compact] |

## Registry

| Capability | Covered by |
|---|---|
| `table_arguments()` and `table_guidance()` for the assistant | G:registry [test: test-spec-registry.R :: the table's registry entry gives the assistant its arguments and guidance] |

## Old to new names

The recordings use the reference's names. `table_golden_renames()` in
`helper-table-golden.R` holds the table below; `table_golden_translate()`
applies it to the scenarios and the recordings when the installed
blockr.viz has the new names (detected by the `percent_of` formal of
`new_table_block()` and the `total` formal of `summary_table()`).

| Where | Old | New | Note |
|---|---|---|---|
| argument | `pct_of` | `percent_of` | [test: test-legacy-ctor.R :: the table's pct_of reads as percent_of, in code and in a saved board] |
| argument | `axis` | `column_axis` | [test: test-legacy-ctor.R :: old table names in code are translated, with one warning] |
| pair summary | `dash` | `linetype` | [test: test-legacy-restore.R :: summary entries and shading rules take the new keys] |
| pair summary | `ref_lo`, `ref_hi` | `ref_min`, `ref_max` | [test: test-legacy-restore.R :: summary entries and shading rules take the new keys] |
| argument | `sort_dir` | `sort_dir` | default `"desc"` becomes `NULL`; recordings that rely on the default with alpha or data order need a decision [test: test-table-model.R :: the groups sort direction defaults as the chart's: by value down, else up] |
| argument | `color` | `color` | `NULL` (auto) becomes `"auto"`, `""` (off) becomes `NULL`; translated [test: test-legacy-restore.R :: the table's color = NULL means auto only before the rewrite] [test: test-table-block.R :: a pasted empty-list color from an old board restores as auto] |
| `summary_table()` in emitted code | `add_overall`, `overall_label`, `subject_var`, `nest_hierarchies` | `total`, `total_label`, `id`, `nest` | [test: test-table-golden.R :: table recording: var_options] [test: test-table-golden.R :: table recording: var_nest] |
| `summary_table()` | `missing` default `"drop"` | default `"level"` | not translated [test: test-table-vocabulary.R :: the variables layout's emitted code keeps missing levels] |
| `table_exhibit()` | `value`, `group`, `digits` | `columns`, `by`, gone | not emitted by the block [test: test-table-exhibit.R :: table_exhibit() is the block as a printed table] |

Browser messages and payloads are recorded as the reference sends them
(keys such as `group`, `rotate_titles`, `ctrl_target`). They are compared
in their own expectation (`messages`), so a rewrite that changes the
protocol fails there and nowhere else.

## Without a test

On the reference. Each item below has a test on the rewrite now; the rows above link it.

- records gear sections and field order in the browser
- `color = NULL` tinting rows from the board's scale map
- `bar_width = "medium"`
- groups gear column editor in the browser: `func`, `col`, `facet` and the type-specific fields
- variables widget pickers other than Statistics, in the browser
- Send to filter: the list of target blocks, for the table
- records row click and clear in the browser (only the skipped browser test)
- a failed download's notification
- a deck asking the groups table for its picture (capture service)
- groups title slots `{@group}`, `{@parent}`, `{@value}`, `{@id_var}`, `{@color}`, `{@facet}`, `{@fill}`, `{@sort_by}`, `{@drill}`
- a payload build failure (red bar in the table)
- records search, header sorting and scrolling in the browser (groups has JS tests for all three)
