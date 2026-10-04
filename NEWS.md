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
