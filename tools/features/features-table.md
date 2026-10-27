# Feature catalogue: table block

What `new_table_block()` does for users on the reference, `feat/viz-0.3.0`
@ `8651d92`, and which test covers each capability. The table part of
`features.md` (rewrite plan, Phase 1). Written 2026-10-27.

The recordings are on local branch `test/table-record` (worktree
`_worktrees/viz-table-record`). Paths below are inside blockr.viz.

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
| `layout = "records"`: the input rows as they are | G:rec_default, G:rec_columns; R:test-table-block-server.R |
| records with `by`: rows nested under one header per level (`table_nest()`) | G:rec_by, G:rec_by_missing; R:test-table-gear-gaps.R |
| records with a finished table as input (annotated data frame): drawn with its structure | G:rec_structured, G:rec_source, G:rec_download_structured |
| records with an object that has an `as_annotated_df()` method | G:rec_input_tbl; R:test-table-block-annotated.R |
| `layout = "groups"`: one row per level of `by`, one column per summary | G:grp_default, G:grp_all_types |
| groups without `by`: one Total row, no sort, search or drill | G:grp_no_by, G:grp_cards_no_by; R:test-table-groups-no-by.R; JS:summarize-table.test.js |
| groups with two `by` columns: parent rows that fold | G:grp_two_by; JS:summarize-table.test.js ("a parent row folds") |
| `layout = "variables"`: Table 1, one column per level of `by` | G:var_default, G:var_options |
| variables with no variable picked: the input passes through | G:var_none |
| layout switch keeps `by`, `summaries`, titles and toggles | G:layout_switch |
| switching to groups with no summaries seeds one count column | G:layout_switch_seed; JS:summarize-table.test.js ("a new groups table opens on its one count column") |
| an unknown layout value is ignored by the switch, healed to records by the constructor | G:layout_switch (last step), G:ctor:bad_layout |
| layout tiles in the gear follow the pushed layout | JS:summarize-table.test.js ("the layout tiles follow"); R:test-table-layout-switch.R (browser) |

## Looks

| Capability | Covered by |
|---|---|
| `look = "table"` | every scenario without `look` |
| `look = "compact"`, records: rotated column titles | G:rec_compact, G:ctor:legacy_compact, G:ctor:legacy_rotate |
| `look = "compact"`, groups: square tile cells, a heatmap | G:grp_compact; R:test-table-tile.R, R:test-summarize-compact-width.R |
| `look = "cards"`, records: one card per column | G:rec_cards; R:test-table-cards.R |
| `look = "cards"`, groups: one row of cards per level, spans left out | G:grp_cards, G:grp_cards_no_by; R:test-table-cards.R |
| `look = "cards"` in the variables layout draws a table | G:var_cards |
| an unknown look reads as `"table"` | G:ctor:bad_look, G:rec_gear |
| the cards' browser rendering (`inst/js/table-cards.js`) | **none** |

## Arguments

| Argument and values | Covered by |
|---|---|
| `by`: records nesting, groups rows (one or two), variables columns | G:rec_by, G:grp_default, G:grp_two_by, G:var_default |
| `rowname` | G:rec_columns, G:rec_gear |
| `columns`, `NULL` = all | G:rec_columns, G:rec_default |
| `summaries`, type `simple`: `func` count, count_distinct, sum, mean; `show` bar, number, dot, tile | G:grp_all_types, G:grp_compact |
| `simple` with `func` median, min, max, identity | R:test-summarize-table.R, R:test-lane-summaries.R |
| `simple` colour split, `position` stack, dodge, fill | G:grp_color_split; R:test-table-vocabulary.R |
| `facet`, `facet_order`, `fill` | G:grp_facet, G:grp_compact; R:test-table-tile.R |
| type `dist`: `show` box, pointrange, text; `summary`, `whiskers`, `style` | G:grp_all_types; R:test-lane-summaries.R, R:test-lane-stats.R |
| type `field` | G:grp_all_types, G:grp_facet |
| type `series`: `band`, `ref` | G:grp_all_types; R:test-lane-summaries.R |
| type `spans`: `label`, `tooltip`, `size` | G:grp_all_types, G:grp_cards |
| type `pair`: `x_func`, `xend_func`, `ref_lo`, `ref_hi`, `ref`, `dash` | G:grp_all_types; JS:summarize-table.test.js (pair tooltip) |
| type `expr`, including a failing expression | G:grp_all_types, G:grp_custom_error; R:test-lane-summaries.R |
| type `custom` | G:grp_custom_error; R:test-lane-custom.R |
| `denom` key | R:test-agg-pct-distinct.R |
| old summary keys (`stat`, `from`/`to`, `lo`/`hi`, `fields`) | G:ctor:old_summary_keys, G:grp_gear; R:test-table-vocabulary.R |
| old aggregation list `list(func, cols)` under `summaries` | G:ctor:agg_summaries |
| empty `summaries` in groups: one count column | G:ctor:groups_seed, G:grp_gear |
| `id`: count distinct subjects in the variables layout | G:var_options, G:var_widget |
| `pct_of` facet, total | G:grp_facet; R:test-table-vocabulary.R; G:ctor:bad_pct_of |
| `missing` level, drop: records headers, groups rows, variables levels | G:rec_by_missing, G:rec_by, G:grp_missing, G:var_missing; R:test-table-missing-drill.R; G:ctor:bad_missing |
| `color` a column (row tint), `""` off | G:rec_shadings, G:rec_color_off, G:rec_gear |
| `color = NULL` tints rows from the board's scale map | **none** (no scale map in the mock session) |
| `shadings` modes sequential, diverging, bar, `domain` | G:rec_shadings, G:rec_gear, G:rec_download; R:test-table-exhibit.R |
| `shadings` with `source` | R:test-table-shading-source.R |
| `format` number, compact, percent, 0 to 6; clamping, strings | G:rec_columns, G:rec_format_compact, G:rec_format_percent, G:grp_format, G:grp_format_digits, G:ctor:bad_format, G:ctor:format_clamp, G:ctor:format_string_number |
| `unit` | G:rec_columns, G:rec_cards, G:grp_format, G:grp_cards |
| `facet_layout` by_summary, by_level | G:grp_facet |
| `sort_by` value, data, alpha, `"label"`, a summary name, a facet level | G:grp_default, G:grp_sort, G:grp_facet, G:ctor:sort_label |
| `sort_dir` desc, asc | G:grp_sort |
| `top_n` | G:grp_sort, G:grp_titles |
| `value_labels`, `axis` | G:grp_display |
| `bar_width` narrow, medium, wide, fit, invalid | G:grp_display (narrow, wide, invalid), G:grp_default (fit) |
| `bar_width = "medium"` | **none** |
| `drill`: records column, `"auto"`, `"source"`; groups any value; variables | G:rec_drill, G:rec_structured, G:rec_source, G:grp_drill, G:var_drill |
| `drill_target`, `drill_table` | G:rec_drill_target, G:grp_drill_target; R:test-table-drill.R (browser) |
| `search`, `sortable` | G:rec_toggles, G:grp_display; R:test-table-display-toggles.R |
| `collapsible` | G:rec_toggles, G:rec_download_by; R:test-table-display-toggles.R; JS:table-collapse.test.js |
| `max_height`, `NULL`, the old `"600px"` dropped | G:rec_toggles, G:grp_display, G:ctor:max_height_legacy; JS:summarize-table.test.js ("a height makes the table scroll") |
| `download` | G:rec_toggles, G:rec_download, G:grp_download; R:test-table-gear-state.R |
| `title`, `subtitle`, `caption`: `NULL` auto, `""` none, template | G:rec_titles, G:rec_titles_auto, G:grp_titles; R:test-title-template.R |
| `caption` default `getOption("blockr.viz.default_caption")` | G:rec_default_caption, G:grp_default_caption |
| `stats`, `sections`, `total`, `total_label`, `nest` | G:var_options, G:var_nest, G:var_widget; R:test-summary-table-stats.R |
| `script`, `script_values`, script error | G:rec_script, G:grp_script; R:test-summarize-prepare.R |
| `selection`: a saved click restores | G:rec_drill_restore, G:grp_drill_restore, G:restore:tile_by_region; R:test-summarize-drill-path.R |
| old names in `...` (`value`, `group`, `digits`, `compact`, `rotate_titles`, per-format downloads, `cell_color`, `ctrl_*`, `filter_*`) | G:ctor:legacy_*; R:test-table-legacy.R |
| block attributes in `...` (`block_name`) | G:ctor:block_attrs |
| an unknown argument (`colour =`) is accepted without a word | G:ctor:unknown_arg |
| `list()` slots from a pre-#144 paste heal to `NULL` | G:ctor:list_healing |
| saved state round-trips (`blockr_ser`, `blockr_deser`) | every G:ctor case (`roundtrip`) |

## Gear controls

The recordings send what the reference's gear sends (`action = "config"`,
the key, the value) and record the state that results.

| Control | Covered by |
|---|---|
| records gear: Row names, Columns, Color by, shading rules, Numbers, Unit, Missing, Header sorting, Sections, Search, Download, Look, Filter on, Title, Subtitle, Caption | G:rec_gear; JS:table-gear.test.js |
| records gear: Nest by (`group` key) and its Missing | G:rec_by |
| records gear: the old `digits` and `rotate_titles` keys | G:rec_gear |
| records gear sections and field order | JS:table-gear.test.js |
| groups gear: One row per, the columns list, Facet layout, Sort, Order, Top, Search, Header sorting, Column axis, Values, Bar width, Percent of, Missing, Numbers, Unit, Download, Look, Filter on, Title, Subtitle, Caption | G:grp_gear, G:grp_sort, G:grp_display, G:grp_facet, G:grp_missing, G:grp_drill; JS:summarize-table.test.js |
| groups gear: unknown and empty keys are ignored | G:grp_gear |
| groups gear column editor in the browser: colour split, show, reorder, remove | JS:summarize-table.test.js |
| groups gear column editor in the browser: `func`, `col`, `facet` and the type-specific fields | **none** |
| variables widget: Variables, Group by, Section by, Statistics, Total and label, Nest, Subject id | G:var_widget (R side); JS:summary-table-block.test.js (Statistics only) |
| variables widget pickers other than Statistics, in the browser | **none** |
| Send to filter: target and table | G:rec_drill_target, G:grp_drill_target |
| Send to filter: the list of target blocks on the board | **none** for the table |
| script section: the script and its value controls (`sv_*`) | G:rec_script, G:grp_script |
| Escape closes the gear | JS:gear-escape.test.js |
| the layout tiles | G:layout_switch, G:layout_switch_seed |

## Drill and clear

| Capability | Covered by |
|---|---|
| records: a click on a value filters downstream, `==` or `%in%` | G:rec_drill; R:test-table-block-server.R |
| records: clear (null column) | G:rec_drill, G:rec_by |
| records: the row click in the browser and the second click that clears | R:test-table-drill.R (browser); R:test-drill-lifecycle.R (markup only) |
| records nested: a header click claims the group, `(Missing)` claims NA rows | G:rec_by, G:rec_by_missing; R:test-table-missing-drill.R |
| structured input: a cell or header click with its column half | G:rec_structured, G:var_drill |
| `drill = "source"`: the click resolves to source rows (`drill_source()`) | G:rec_source |
| groups: a row click, a parent path, clear, `clear_filter` | G:grp_drill, G:grp_two_by; JS:summarize-table.test.js ("a latched drill", "a nested drill") |
| groups: a click on `(Missing)` keeps the NA rows | G:grp_missing; R:test-table-missing-drill.R |
| re-aiming or clearing the drill drops the filter; removing `by` drops it | G:grp_drill, G:rec_drill |
| a click with a prepare script filters before or after the script | G:grp_script, G:rec_script; R:test-summarize-prepare.R |
| status line "Filtered: ..." (records) | G:rec_drill, G:rec_by (output `records-dt_status`) |
| status line (groups, drawn in the browser) | JS:summarize-table.test.js |
| transient drill: every click sends, the same row twice sends twice, a clear is inert, nothing filters here | G:rec_drill_target, G:grp_drill_target |
| pooled groups of an overlap definition | R:test-expand-groups.R |

## Downloads

| Capability | Covered by |
|---|---|
| menu offers Excel, Web page, PowerPoint (records) and Image (groups) while `download` is on, nothing when off | G:rec_download, G:grp_download (outputs `records-dt_download`, `groups-summarize_download`); R:test-table-gear-state.R |
| Excel, records: values, cell fills of the shadings, title, subtitle, caption, number format | G:rec_download; R:test-write-annotated-xlsx.R, R:test-table-exhibit.R |
| Excel, nested records, structured input, variables | G:rec_download_by, G:rec_download_structured, G:var_download |
| Excel, groups: the numbers behind the marks | G:grp_download; R:test-summarize-download.R |
| Excel, cards: the values | G:rec_download_cards, G:grp_download_cards |
| Web page for every layout and look; follows `collapsible` and `sortable` | G:rec_download, G:rec_download_by, G:rec_download_cards, G:rec_download_structured, G:grp_download, G:grp_download_cards, G:var_download; R:test-write-exhibit.R |
| PowerPoint builds: records, nested, structured, cards, groups painted and captured, variables | same G scenarios ("written: pptx") |
| PowerPoint content: slides, fitting, carry-over | R:test-write-exhibit.R |
| Image, groups: painted fallback, then the browser's capture | G:grp_download |
| Image, records: not offered; the handler errors | G:rec_download (step 5) |
| file name: dataset, title, time | every download step; R:test-download-filename.R |
| download footer and exhibit font options | R:test-download-footer.R, R:test-exhibit-font.R |
| a failed download shows a notification | **none** (the error is recorded, the notification is not) |
| a deck asks the groups table for its picture through the capture service | **none** |

## Title slots

| Capability | Covered by |
|---|---|
| `{n}`, `{label(col)}`, `{n_distinct(col)}`, `[ ... ]` optional clauses | G:rec_titles, G:grp_titles; R:test-title-template.R |
| `NULL` takes the input's `label`, `subtitle`, `caption` attributes | G:rec_titles_auto, G:grp_titles |
| groups `{@by}`, `{@top_n}`, `{@func in ...}` | G:grp_titles |
| groups `{@group}`, `{@parent}`, `{@value}`, `{@id_var}`, `{@color}`, `{@facet}`, `{@fill}`, `{@sort_by}`, `{@drill}` | **none** |
| prepare-script values as slots | R:test-summarize-exhibit.R, R:test-summarize-download.R |
| records and variables: `{@arg}` prints blank | G:rec_titles_slots, G:var_titles_slots |
| the resolved titles in the downloads | G:rec_download, G:grp_download |
| the face's clickable title words and offers | G:grp_titles (payload `titles`) |

## Empty, error and needs-input states

| Capability | Covered by |
|---|---|
| no input connected | G:rec_no_input, G:grp_no_input, G:var_no_input |
| input that is not a data frame: block error | G:rec_not_df; R:test-input-contract.R |
| zero rows | G:rec_transport, G:grp_transport |
| a column gone after an upstream change | G:rec_transport, G:grp_transport (`df_no_cat`) |
| prepare script error | G:rec_script, G:grp_script |
| failing `expr` or `custom` summary | G:grp_custom_error |
| a payload that fails to build shows a red bar in the table | **none** |
| an identical payload is not sent again; `_ready` re-sends | G:rec_transport, G:grp_transport |

## Restore of saved boards

Saved by blockr.viz 0.2.223 (`c79f5fb`, main before the table rewrite):
`fixtures/tile-board-main.json` and `fixtures/table-golden/old-boards.json`
(written by `dev/table-golden-old-boards.R`).

| Board | Covered by |
|---|---|
| tile: grouped summaries, totals, long frame, wide frame | G:restore:tile_*; R:test-tile-deser.R |
| table block (records): plain; all settings, old colour args, per-format downloads, `rotate_titles`, saved click, `600px`; shadings; in-block aggregation with and without `group`; ctrl | G:restore:old_table_*; R:test-table-legacy.R |
| summarize table: ranked bar (mean with colour, count distinct with parent and facet, identity with fields); summaries with old keys and saved path click; script with values and ctrl | G:restore:old_summarize_*; R:test-groups-one-list.R |
| summary table: flat, with sections and nest, the interim `state` blob | G:restore:old_summary_* |
| the superseded constructors `new_summarize_table_block()`, `new_summary_table_block()` | G:ctor:superseded_* |
| gt table, `drilldown_table_block` | dropped (decision); not recorded |
| AE heatmap | restored by blockr.pharma `feat/ae-heatmap-table`; not recorded here |

## Registry

| Capability | Covered by |
|---|---|
| `table_arguments()` and `table_guidance()` for the assistant | G:registry |

## Old to new names

The recordings use the reference's names. `table_golden_renames()` in
`helper-table-golden.R` holds the table below; `table_golden_translate()`
applies it to the scenarios and the recordings when the installed
blockr.viz has the new names (detected by the `percent_of` formal of
`new_table_block()` and the `total` formal of `summary_table()`).

| Where | Old | New | Note |
|---|---|---|---|
| argument | `pct_of` | `percent_of` | |
| argument | `axis` | `column_axis` | |
| pair summary | `dash` | `linetype` | |
| pair summary | `ref_lo`, `ref_hi` | `ref_min`, `ref_max` | |
| argument | `sort_dir` | `sort_dir` | default `"desc"` becomes `NULL`; recordings that rely on the default with alpha or data order need a decision |
| argument | `color` | `color` | `NULL` (auto) becomes `"auto"`, `""` (off) becomes `NULL`; translated |
| `summary_table()` in emitted code | `add_overall`, `overall_label`, `subject_var`, `nest_hierarchies` | `total`, `total_label`, `id`, `nest` | |
| `summary_table()` | `missing` default `"drop"` | default `"level"` | not translated |
| `table_exhibit()` | `value`, `group`, `digits` | `columns`, `by`, gone | not emitted by the block |

Browser messages and payloads are recorded as the reference sends them
(keys such as `group`, `rotate_titles`, `ctrl_target`). They are compared
in their own expectation (`messages`), so a rewrite that changes the
protocol fails there and nowhere else.

## Without a test

- the cards' browser rendering (`table-cards.js`)
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
