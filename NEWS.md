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
