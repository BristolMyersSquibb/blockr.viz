# blockr.viz 0.3.0.9000

The first release for CRAN. The package is down to two blocks, the chart
and the table, rewritten on one specification of their arguments that the
constructors, the gear, saved boards, title slots and the assistant all
read.

## The blocks

* `new_chart_block()` draws bar, histogram, waterfall, pie, treemap, box,
  point range, scatter, line, area, band and timeline charts with ECharts.
* `new_table_block()` has three layouts, `"records"` (the input's rows),
  `"groups"` (one row per group, one column per summary) and
  `"variables"` (a Table 1), each drawn as a `"table"`, a `"compact"`
  matrix or `"cards"`. It replaces the tile, summarize, summary and gt
  table blocks.
* `vignette("chart")` and `vignette("table")` go through the arguments.

## Arguments

* One name per concept in both blocks, in US spelling, as in ggplot2 where
  the concept is the same: `type`, `x`, `y`, `position`, `y_transform`,
  `percent_of`, `drill_target`, `selection`, `columns`, `by`. The Legacy
  section lists the old names.
* In code, a value an argument does not take is an error. Old names still
  work, with a warning. Any other name is kept as a block attribute, so
  another package's attribute (blockr.dock's `visible`) survives a
  restore.
* A saved board is healed instead: a value the block does not take is reset
  to its default, with one warning naming it.

## Changes in behaviour

* The chart keeps `x`, `y` and `color` when its type changes; a setting a
  type does not use is hidden in the gear, not cleared. Picking
  `func = "count"` no longer clears `y`.
* Clearing a mapped column that the data lacks clears it.
* Data-driven defaults, such as the first text column on `x` or the sort
  order, are worked out when the chart is drawn and are not saved.
* The table's `color = NULL` now means no color; the default is `"auto"`.
  Boards saved before this version keep their old meaning.
* The table's default sort direction follows the chart's rule: descending
  for `sort_by = "value"`, else ascending.
* In the groups layout, a custom summary that fails only on the parent rows
  of a nested `by` shows as one failed column with its error; the table
  still draws.
* Title slots resolve in the records and variables layouts as well:
  `{@rowname}`, `{label(@rowname)}`, `{@look}` and `{@by}` were empty
  there.
* Unsetting a table's `drill_target` clears the selection at the old target
  once; it was cleared twice.
* A board saved with a click on an aggregated old table keeps that click
  when it restores in the groups layout: the table shows it and the output
  is filtered. Before, the restored output was unfiltered.

## Removed

* The gt table block (`new_gt_table_block()`, `gt_table()`); gt is no
  longer suggested.
* The drilldown chart, waterfall and drilldown table blocks of blockr.viz
  0.0.x. Their saved boards no longer restore. A waterfall is
  `new_chart_block(type = "waterfall")`.
* `new_tile_block()`. A tile is `new_table_block(look = "cards")`. Saved
  tile boards restore as table blocks in the cards look: a tile with
  `summaries` in the groups layout, any other in the records layout.
* `new_summary_table_block()` and `new_summarize_table_block()` are
  superseded: they translate their arguments and return a table block in
  the variables and the groups layout.

## Downloads

* Downloads go through one registry: `register_viz_exporter()` adds a
  format, and a block offers every registered format that can write it.
  blockr.viz registers Excel, a web page, an image and PowerPoint; a block
  hands its downloads an exhibit (`new_viz_exhibit()`).

## Security

* On a locked board (`options(blockr.locked = TRUE)`) the blocks take no
  settings from the browser; clicks still filter.
* The prepare script (`script`) runs R code on the server, so the browser
  can set it only with `options(blockr.viz.browser_script = TRUE)`. Code and
  saved boards set it as before.

## Legacy

`R/legacy.R` and `R/legacy-restore.R` keep what older code and boards need,
and nothing else does. They go when the support for blockr.viz before 0.3.0
goes.

* Chart arguments: `chart_type` and `baseline` are `type`, `group` is `x`,
  `value` and `metric` are `y`, `agg_fn` is `func`, `bar_mode` is
  `position`, `tt_fields` is `tooltip`, `line_width_mult` and
  `dot_size_mult` are `line_width` and `point_size`, `value_lines` is
  `y_lines`, `lo` and `hi` are `ymin` and `ymax`, `band_id` and `count_col`
  are `id`, `count_on` is `show_n`, `na_group` is `missing`, `pct_of` is
  `percent_of`, `ref_lo` and `ref_hi` are `ref_min` and `ref_max`,
  `y_scale` and `x_scale` are `y_transform` and `x_transform`,
  `direct_labels` is `legend = "marks"`, `ctrl_target` and `ctrl_table`
  are `drill_target` and `drill_table`, `values` is `script_values`, and
  the `filter_*` fields are `selection`. `func_toggle` is dropped.
* Chart values: `type = "radar"` becomes a bar chart, with a warning;
  `baseline = "cumulative"` on a bar is `type = "waterfall"`; `bar_mode`
  `"stacked"`, `"grouped"` and `"percent"` are `position` `"stack"`,
  `"dodge"` and `"fill"`; `pct_of = "group"` is `"category"`; `y_scale`
  and `x_scale` `"linear"` and `"log"` are `"identity"` and `"log10"`;
  `show_n = "off"` is `"none"`; `connect = "step-start"` and its siblings
  are `"step_start"`, and `connect = "off"` is `"straight"`;
  `box_points = "all"` is `"none"`; `"on"` and `"off"` are `TRUE` and
  `FALSE`. Several `percent_of` values keep the first.
* Table arguments: `value` and `values` are `columns`, `group` is `by`,
  `cell_color` is `shadings`, `row_color` is `color`, `rotate_titles` and
  `compact` are `look`, `digits` is `format`, the three `*_download`
  switches are `download`, `ctrl_target` and `ctrl_table` are
  `drill_target` and `drill_table`, the `filter_*` fields are `selection`,
  `pct_of` is `percent_of`, `axis` is `column_axis`, and the summary
  table's `vars`, `add_overall`, `overall_label`, `nest_hierarchies` and
  `id_var` are `summaries`, `total`, `total_label`, `nest` and `id`.
  `max_height`, `indent_details` and `state` are dropped.
* Table values: `sort_by = "label"` is `"alpha"`, `stats = "compact"` is
  `"mean_sd"`, and an old aggregation `list(func, cols)` in `summaries`
  becomes one number column per function and column.
* Summary keys: `band` is `ymin`, `ref` is `summary` or `x_lines`,
  `ref_lo`, `ref_hi`, `lo` and `hi` are `ref_min` and `ref_max`, `dash` is
  `linetype`, `from` and `to` are `x` and `xend`, `from_func` and
  `to_func` are `x_func` and `xend_func`, `fields` is `tooltip`, `stat`
  and `inner` are `summary`, `outer` is `whiskers`, `style` is `show`
  (`"dot"` is `"pointrange"`), `denom` is `id`, `type = "series"` is
  `"sparkline"`, `ref = "median_iqr"` is `"median_q1_q3"`, and `position`
  `"stacked"`, `"grouped"` and `"percent"` are `"stack"`, `"dodge"` and
  `"fill"`. `scope`, `parent` and `facet_order` are dropped.
* Shading keys: `cols` is `columns` and `type` is `mode`. `palette` and
  `source` are dropped.
* Saved boards: chart and table blocks restore under the new names, title
  templates included (`{@group}` becomes `{@x}` or `{@y}`). Tile, summary
  table and summarize table blocks restore as table blocks. gt,
  `drilldown_chart`, `waterfall` and `drilldown_table` blocks no longer
  restore.

# blockr.viz 0.3.0

* `y_scale = "log"` draws a base-10 y axis on a scatter, line or band, and
  `x_scale = "log"` the x axis of a scatter. Ticks run 1-2-5 up to three
  decades and at powers of ten beyond. Values and guide lines at or below
  zero are not drawn, and the footer counts them; an interval's low end
  there is cut at the bottom of the axis. A smoother is fitted on the log
  scale. The gear offers "Y scale" and "X scale" next to Unit.
* `highlight` makes a few marks of a bar, scatter, line or area stand out:
  `highlight = list(country = "Switzerland")` names one column and some of
  its values. Marks holding one of the values keep their colour, every other
  mark is grey and drawn behind them. With `color` the highlighted marks keep
  their colour level, the palette is spread over the highlighted levels
  only, and the legend lists only those. Highlighted marks are named: in
  semibold on a bar's axis, beside a point, at a line's end. A title says it
  with `{highlight}`: `"...[, {highlight} highlighted]"`. The gear offers it
  under "Add mapping" as a column and a value picker. A drill selection
  still dims by opacity on top.
* `type = "histogram"` cuts `x` into about `bins` bins (default 30) at
  readable breaks and draws them as touching bars, empty bins included. A
  click on a bin filters `x` to its range.
* `type = "area"`: a line with its area filled, stacked across a colour
  split unless `position = "dodge"`, measured from zero.
* `ymin` / `ymax` on a line draw a shaded band, and several columns draw
  nested bands, the first the outermost: `ymin = c("lo95", "lo80"), ymax =
  c("hi95", "hi80")` is a forecast fan. A single line's band is grey. On a
  scatter they stay error bars. A saved line chart with `lo` / `hi` now
  draws a band instead of error bars.
* `linetype` names a column whose values set a line's dash: the first value
  is solid, the others dashed, from the last point of the solid part on.
* `x_ranges` shades named stretches of x (`list(Recession = c("2008-01-01",
  "2009-06-30"))`). The gear edits them as one line, "Shaded":
  `Recession: 2008-01-01 to 2009-06-30; Covid: 2020-03-01 to 2021-06-30`.
* `x_lines` take dates and category values as well as numbers, as
  `x_ranges` does: `x_lines = c(Listed = "2026-10-21")`. A `Date` is placed
  on a date axis; before, it landed in 1970 and a date string was dropped.
  On a bar, `x_lines` and `x_ranges` mark the category axis either way
  round: a value names its bar, and on bars over dates a date marks the bar
  whose period holds it. The gear writes names the way the shaded ranges
  do, `Listed: 2026-10-21; 2026-11-02`, for `y_lines` too, so a name set in
  code survives an edit.
* `label` on a scatter, line or area writes the column's value beside each
  point that has one, and the gear offers it under "Add mapping". On a bar,
  pie or treemap it writes the value on each bar, slice or tile whose rows
  agree on one: at a bar's end, inside a stack's segments where it fits, in
  place of a slice's name. Waterfall, histogram, boxplot, pointrange and
  radar do not draw it.
* `format` also takes a whole number of decimals from 0 to 6 (`format = 2`
  prints 4.36 on the axis, in tooltips and value labels), as on the table
  block. The gear's "Numbers" select offers Auto, 0, 1 and 2 decimals beside
  compact and percent.
* `format = "number"` prints up to two decimals, trailing zeros dropped, and
  a value that would round to 0 keeps two significant digits (0.0034), as
  the table block. It printed up to four before ("4.3587 %"). Bar value
  labels print each value on its own (1.7 next to 2.14).
* `pct_of = "total"` takes a `pct_distinct` denominator over the whole
  chart, all panels together. `"facet"` stays the default.
* The gear has a "Totals" row on a waterfall (`waterfall_totals`, picked
  from the steps), "Interval low / high" on an area (`ymin` / `ymax`) and
  "Group counts" on a histogram (`show_n`).
* `{@type}` in a title, subtitle or caption opens a menu of chart types,
  narrowed by a list (`{@type in line, scatter}`).
* `legend` places the colour legend: `"bottom"` (default), `"top"`, `"marks"`
  or `"none"`. `"marks"` writes the names on the marks (at a line's or an
  area's end, beside the last bar of a stack, at a scatter group's centre,
  on a pie slice); dodged bars, histograms and boxplots keep the legend.
  Names of scatter groups whose centres sit close are moved apart a line at
  a time.
  The legend is left-aligned, its swatch has the mark's shape (a line, a
  dot or a square), and it has a title only when the colour column has a
  label. `direct_labels = TRUE`, from earlier builds of 0.3.0, is read as
  `legend = "marks"`.
* `format` (`"number"`, `"compact"`, `"percent"`) and `unit` print the values
  as the table block does: the ticks in the format, the unit once in the axis
  title, tooltips and value labels with both.
* Line charts follow the rules from the line chart review. A dense line
  draws no dots; a line whose points are at least 24 px apart draws ringed
  dots. A resize or a new filter moves the lines and the dots together,
  without animation. Lines are 2 px. A hairline crosshair finds the date and one tooltip
  lists every line, highest first, named by its level alone; nothing on the
  plot changes as the cursor moves. A Date or date-time x is a time axis
  whose ticks follow the zoom (years, months, days) and whose tooltip names
  the period ("Q4 2014", "Dec 1986"); a brush or point click taken there
  filters as a date. Gridlines are solid hairlines, and a line chart has no
  x grid and no y axis line.
* With one line per entity (`series`, a trajectory per subject), hover still
  picks the nearest line, without the gradient area and the ring of dots.
* A bar, pie or treemap without `sort_by` orders its categories by the
  category column: a date, number or factor keeps its own order (time, size,
  levels), and a text column sorts by value, largest first. Before, every
  category sorted by value, so quarters and weeks came out of order. When
  the gear switches the category, a sort still at one of these defaults
  follows the new column. `sort_by = "data"` puts a date or number in time
  or numeric order instead of first appearance, and without `sort_dir` it
  runs ascending (it ran descending before). A chart whose state sets both
  `sort_by` and `sort_dir` keeps its order.
* `connect` defaults to `"straight"` for new charts. A saved chart keeps the
  value it was saved with, so existing boards stay smoothed.
* `new_chart_block()` has new argument names, chosen to stay stable on CRAN
  and to match blockr.ggplot and ggplot2. `x` and `y` are the screen axes for
  every type: a horizontal bar of counts is `y = "class"`, a horizontal bar
  of sums `x = "sales", y = "region", func = "sum"`. `orientation` is
  inferred and only needed when both columns are non-numeric. The renames:
  `chart_type` -> `type`, `group`/`value` -> `x`/`y`, `bar_mode` ->
  `position` (`"stack"`, `"dodge"`, `"fill"`), `tt_fields` -> `tooltip`,
  `line_width_mult`/`dot_size_mult` -> `line_width`/`point_size`,
  `value_lines` -> `y_lines`, `lo`/`hi` -> `ymin`/`ymax`, `band_id` and
  `count_col` -> `id`, `count_on` -> `show_n`, `na_group` -> `missing`,
  `ctrl_target`/`ctrl_table` -> `drill_target`/`drill_table`, `values` ->
  `script_values`, and the four `filter_*` fields -> `selection`.
  `pct_of = "group"` is now `"category"`. `baseline = "cumulative"` is
  `type = "waterfall"`, and `type = "radar"` is gone. The old names still
  work, with a warning, and saved boards load unchanged.
* `{@arg}` slots in titles take the new names (`{@type}`, `{label(@y)}`). A
  saved template with an old name still resolves.
* The chart draws no toolbox icons. Drag in a line chart to zoom; a "Reset
  zoom" link appears while zoomed, and a double-click also resets. A scatter
  brushes on drag as before.
* The fallback palette, used without blockr.theme, is blockr.theme's
  "Blockr" palette. The old fallback's yellow was unreadable on white.
* The filter line under the chart's caption shows only while a filter is
  set; "No filter active" is gone. There is no Reset button: a second click
  on the selected mark, or a click on empty plot space, clears the
  selection.
* Captions are upright, on the chart and the table. Text on the chart canvas
  is 12px (blockr.ui's `--blockr-mark-font-size`; the fallback follows).
* An axis is named by the column's label and the unit, "Growth (%)"; a
  column without a label by the unit alone, else by its name. It has no name
  when the chart's title already contains it.
* An unnamed `y_lines` line at 0 has no label: the axis prints 0.
* A histogram's bins carry x's label, so the filter line reads "price =
  1000-1500".
* `interactive_table()` draws a table the way the table block does, for
  another block's UI, and `update_interactive_table()` replaces the table
  inside it from the server. It replaces `drilldown_table()`, which is no
  longer exported. `static_table()` is the print counterpart.
* The heatmap and tile blocks are gone. The table block draws heatmaps
  (the groups layout with `look = "compact"`) and tiles (`look = "cards"`).
  A saved tile block loads as a table block.
* The picker block is gone. Bind chart and table mappings to the source
  columns directly and put a group switch in the board's filter.
* The table block orders rows alphabetically with `sort_by = "alpha"`, as
  the chart block does. The old value `"label"` is read as `"alpha"`.
* The table block's `format` takes the chart block's values: `"number"`,
  `"compact"` (1.2K, 3.4M), `"percent"` (a fraction x100 with "%") or a
  whole number of decimals from 0 to 6. A new `unit` heads the numeric
  columns ("Revenue (CHF)") and follows the number on a card. Both apply to
  the records layout's cells, cards and downloads; the gear's "Numbers"
  select and "Unit" field set them. Excel keeps the values as numbers and
  shows them in the same format.
* A fixed number of decimals prints exactly that many (`format = 2` gives
  4.30), so a column lines up; Excel formats every numeric column with it.
  `"number"` prints up to two decimals, and a value that would round to 0
  keeps two significant digits (0.0034). An old `digits = 2`, the old
  default, reads as `"number"`.
* `format` and `unit` reach the groups layout too: its number cells, the
  value labels beside its bars and its cards. `unit` heads the columns that
  are not counts. The variables layout (Table 1) keeps its statistics' own
  formats. The groups gear has the same "Numbers" and "Unit" controls.
* A drill sent to a filter block (`drill_target`) from a missing bar, or
  from a table's "(Missing)" row, claims the rows with no value: the claim
  carries the value filter's `"<NA>"` / `"<empty>"` tokens, and with the
  board's subject column it is the subjects behind those rows. Before, it
  claimed nothing.
* A click on the "(Missing)" row of a table with `missing = "level"` gives
  the rows whose value is missing, in every layout. Before, it gave none.
  `summary_table()` and `table_nest()` take `missing`: the variables layout
  keeps a numeric variable numeric when it has missing values, and a nested
  records table names its missing group "(Missing)", a header a drill can
  claim.
* The table block's `missing` defaults to `"level"`, as on the chart block:
  missing values of a grouping, colour or facet column show as "(Missing)"
  unless the author drops them. A board saved by the new table block stored
  its `missing` and keeps it. Boards saved by the old table, summarize table
  and summary table blocks had no `missing`; they now show "(Missing)" rows
  and columns where their data has missing values.
* The table block offers its downloads by default (`download = TRUE`), as
  the chart block does. Boards saved by the new table block stored their
  choice and keep it. An old table board keeps downloads off when it stored
  its per-format download flags all off; one saved before those flags
  existed now offers downloads.
* A table summary's `summary` and `whiskers` are typed in the block's
  argument specs: the chart's statistics (`median_q1_q3`, `mean_sd`, ...,
  `whiskers` also `tukey`), plus `mean_ci95` and `"none"`, which only the
  table draws.
* The table block's footer says nothing about filters while none is set:
  "No filter active" and the Reset button are gone, in every layout and
  look, as under the chart. "Filtered: ..." shows while a row or card is
  selected; a second click on it clears the filter. The records layout keeps
  its row count.

# blockr.viz 0.2.210

* The table block's groups layout has one way to say what its columns are:
  the column list. The gear's Mapping section (Group, Aggregate, Add
  mapping) and the single ranked bar behind it are gone. Group by sits under
  the "Rows are" switch. A new groups table starts with one open "Count"
  column. Boards saved with the old summarize table's bar restore as that
  list: `group` and `parent` become `by`, and `func`, `value`, `id_var`,
  `color`, `facet` and `bar_mode` become the column's own settings.
* In the groups layout a faceted column ranks the rows by its value over all
  levels, not by its first level. A count's percent over pooled groups
  counts each input row once. An unnamed column is headed by its variable's
  label ("Mean Start Day", not "Mean ASTDY").
* A summarize table grouped by two `by` columns lists the inner column flat
  when the outer one has no value in any row. A prepare script can blank the
  outer column behind a flag (`soc <- FALSE`, `AEBODSYS = if (soc) AEBODSYS
  else NA`) and name the flag in the subtitle (`[ within {@soc}]`), so the
  reader switches the nesting on and off from the sentence. Before, such a
  table drew no rows.
* A prepare-script word offered beside the sentence ("+ system organ
  class") shows its label from the first paint, not the variable name.

# blockr.viz 0.2.207

* The summarize table has a Values switch in its gear (`value_labels`,
  default on). Off drops every number printed beside a mark.
* A colour-split column (a dumbbell, box or dot range per grade, or a bar
  whose levels sit side by side) prints one number per level, beside that
  level's lane.
* A stacked bar prints each segment's number inside it (a 100% bar its
  share), white or dark against the fill, and keeps the total at the end. A
  segment too narrow for its number shows none; the tooltip still has it. Before, the cell printed
  none. The row grows to hold them, on screen and on the slide, so a long
  table can take more slides.
* Value labels print one decimal finer than the column axis's ticks (ticks
  every 10 read "17.7"); whole numbers stay whole. A dumbbell's change only
  carries a "+" when the column also has a negative change.
* Value labels are grey.
* A nested summarize table has the table block's fold-all chevron in its
  top-left header cell: one click opens every group, the next folds them.
* Every colour level of a split cell draws at full size; a row with more
  levels grows instead of thinning the third level on.

# blockr.viz 0.2.205

* A number in a block's title or subtitle is typed over instead of opening a
  list of round values, so any value in its range can be set from the word
  (7, not only 5 or 10). The range shows beside the field ("0 to 50"), the
  arrow keys step by one (Shift: ten), and a value outside the range is held
  at the nearest end. Applies to the chart, heatmap and summarize table.
* The field a word turns into while typed over has the design system's
  accent edge and focus ring.

# blockr.viz 0.2.204

* A nested summarize table gives its label column 24px of left padding, and
  child rows 48px. The fold chevron sat 2px outside the cell, under the bar a
  clicked row draws at its left edge; it now has a gap to it.

# blockr.viz 0.2.203

* The summarize table's title sits on the left of the gear row again, without
  a line under it, and the shared table styles (cell padding, row hover, drill
  cursor, active row, drill flash, sticky header, column alignment) reach it
  again. table.css still named the classes from before the rank -> summarize
  rename (0.2.194).

# blockr.viz 0.2.202

* A grouped colour split in the summarize table draws no track for a level
  with no value in that row, as the lollipop draws no lane for it. A term
  with no SEVERE event shows two bars, not two bars and an empty track. The
  slide and image exports follow.

# blockr.viz 0.2.201

* A summarize table's colour split takes its levels from the rows it draws.
  A population join adds subjects with no record, which have no group but
  still carry their arm, so an arm with no events (Screen Failure) kept an
  empty bar slot in every row and a legend key. Lollipops drew nothing for it
  and looked right; bars drew the empty track.

# blockr.viz 0.2.200

* A custom summary that splits by a column its function returns (a dumbbell
  or an interval per grade) errors no more. 0.2.198 read the colour column of
  every split, and such a split has none.

# blockr.viz 0.2.199

* A row click on a nested summarize table filters on the row's grouping path.
  A parent row filters its outer column (`AEBODSYS = CARDIAC`), a child row
  both columns (`AEBODSYS = SKIN, AEDECOD = RASH`), for the downstream filter
  and for a drill sent to another block. Before, every row claimed the inner
  column, so a parent row matched nothing and a child label found under
  several parents claimed it under all of them. The gear's Drill-down section
  is an on/off switch now; its column picker is gone. Boards saved with a
  single-column click restore it as before.

# blockr.viz 0.2.198

* The summarize table no longer gives a colour level a slot when no row in
  the table carries it. A filter upstream keeps a factor's full level set, so
  grouped bars, lollipops and the legend showed an empty slot for every
  filtered-out level. Zeros in single rows keep their slot, and the remaining
  levels keep their colours.

# blockr.viz 0.2.197

* A chart opened for the first time shows its "drawing… 1.2 s" clock from
  the moment its panel shows. On a board that builds blocks lazily, R's own
  start message came a second or more after the click, and the chart sat
  blank and uncounted until then. A "done" that R sends while the block is
  still being built no longer ends the clock when a new start follows at
  once.

# blockr.viz 0.2.196

* A drill to another block (`ctrl_target`) now carries every value the click
  names. A click on a bar segment that covers nine patients drills the patient
  profile to those nine; before, a drill naming more than one value reached
  the target as nothing.

# blockr.viz 0.2.195

* The summarize table carries the classes `lane_chart_block` and `rank_block`
  again. Dropping them in 0.2.194 made every saved board with a summarize
  table fail to open ("Could not deserialize object: expected classes ...").

# blockr.viz 0.2.194

* Removed `new_rank_block()` and `new_lane_chart_block()`, the summarize
  table's former names. Use `new_summarize_table_block()`.
* `rank_table()` is no longer exported. The summarize table's static HTML is
  `html_exhibit(static_summarize_table(...))`.
* The summarize table's internals are renamed from "rank" to "summarize":
  files (`R/summarize-*.R`, `inst/js/summarize-table.js`), functions, CSS
  classes and variables (`blockr-summarize-*`), HTML attributes
  (`data-summarize-*`) and the custom message names. Custom CSS that targeted
  `.blockr-rank-*` needs the new names.

# blockr.viz 0.2.193

* Summarize table: turning the search bar, the drill-down or "Send to
  filter" on or off in the gear no longer closes the gear. These settings
  used to re-render the block's container; they now update it in place.

# blockr.viz 0.2.192

* The chart block's browser code is rewritten: `inst/js/chart.js` is
  replaced by separate model, option and view files in `inst/js/chart/`.
  Clicks follow the documented rules: a click always sends its filter and
  never toggles it off (Reset clears); a coloured segment, a facet panel
  and a scatter point's series filter what they show; a legend chip
  filters its level instead of hiding it; a brush takes the brushed points;
  the footer names the filter in column labels; a restored or latched
  filter, a range included, lights what holds filtered rows and dims the
  rest.

# blockr.viz 0.2.190

* The chart block's ggplot twin is gone: `static_chart()`, `chart_expr()`,
  `chart_code()` and the chart block's `report_call()` method. Chart
  downloads (png, web page, PowerPoint) carry the picture the browser drew.
  The Excel download writes the rows the chart is drawn from.
* The canvas capture kill switch is gone (`BLOCKR_CANVAS_CAPTURE`,
  `options(blockr.viz.canvas_capture)`). Chart, heatmap and rank blocks
  always export the captured picture.
