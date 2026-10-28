# Chart block for CRAN: requirements

Branch `feat/chart-cran`. The goal is a chart block that works for general
data analysis (an assistant building reports from a chat, the way Power BI
is used), with argument names that can stay stable once blockr.viz is on
CRAN. The table and tile blocks follow afterwards. The target look and the
R calls are in the mockup (`_scratch/blockx/viz-mockup/index.html` in the
workspace).

## Decisions

- **ECharts stays the renderer.** Open-source BI tools in the same role use
  it (Superset, Metabase, Evidence). What needed work was the block's
  arguments and its default look, not the library.
- **Nothing moves to blockr.pharma.** Gantt (a timeline of intervals) and the
  band (a running quantile band across individuals) are general chart types.
  The clinical part is the examples in the docs (`USUBJID`, `ANRHI`), which
  get neutral data.
- **No toolbox icons.** The toolbox is still rendered with zero size outside
  the canvas, because ECharts builds the drag controllers only for a rendered
  toolbox. Drag in the plot to zoom (line) or brush (scatter). A zoomed panel
  shows a "Reset zoom" link; a double-click also resets.
- **Palette.** The fallback used when blockr.theme is not installed is
  blockr.theme's "Blockr" palette (Okabe-Ito without black and yellow). The
  old fallback's yellow `#F0E442` is 1.3:1 on white. Boards with blockr.theme
  are unchanged.
- **Public names follow ggplot2 and blockr.ggplot** where the concept is the
  same, use one name per concept across the viz blocks, and use no
  abbreviations.
- **Aggregated types map `category` and `value`** (changed on 2026-10-28;
  0.3.0 made `x` and `y` the screen axes for every type, which made the
  gear's orientation switch read the category as the value). This is the
  block-config-ui decision of June 2026 and what boards before 0.3.0 saved
  as `group` and `value`. `orientation` only turns the marks, `NULL` being
  horizontal for a bar and vertical for a boxplot or pointrange. `x` and
  `y` stay for scatter, line, area and band, the histogram (`x`) and the
  gantt (`x`, `xend`, `y`). `y_lines` is `value_lines` again.
- **The internals keep their names.** The R server and the JavaScript
  (`inst/js/chart/`) still use `chart_type`, `group`/`value`/`orientation`,
  `bar_mode` and so on. `R/chart-args.R` translates at the constructor, the
  saved state, the gear's config message and the `{@arg}` slots. This keeps
  the 7,600 lines of chart JavaScript and its 233 snapshots unchanged.
- **Subtitle and caption slots stay** (`{@arg}`, `{label(@arg in a, b)}`,
  `[...]`). They take the public names; a saved template with an internal
  name (`{@group}`) still resolves.

## Argument names

| Before 0.3.0 | Now | Note |
|---|---|---|
| `chart_type` | `type` | As in blockr.ggplot. |
| `group`, `value` | `category`, `value` | See above. `value = ".count"` is a row count, `value` unset. |
| `bar_mode` = stacked / grouped / percent | `position` = `"stack"` / `"dodge"` / `"fill"` | Values as in blockr.ggplot. |
| `tt_fields` | `tooltip` | |
| `line_width_mult`, `dot_size_mult` | `line_width`, `point_size` | Still relative, 1 = default. |
| `lo`, `hi` | `ymin`, `ymax` | Interval columns, drawn as error bars on a scatter or a line (a forest plot). |
| `band_id`, `count_col` | `id` | One column that identifies an individual. |
| `count_on` | `show_n` | |
| `na_group` | `missing` | `"level"` / `"drop"`. |
| `pct_of = "group"` | `pct_of = "category"` | Other values unchanged. |
| `ctrl_target`, `ctrl_table` | `drill_target`, `drill_table` | |
| `values` | `script_values` | Goes with `script`. |
| `filter_type`, `filters`, `filter_range`, `filter_point` | `selection` | One list the chart writes. A formal because blockr.core saves a block from its formals. |
| `baseline = "cumulative"` | `type = "waterfall"` | |
| `filter_column`, `filter_values`, `func_toggle` | removed | Read on load only. |
| `type = "radar"` | removed | Loads as a bar chart, with a warning. |

Unchanged: `func`, `color`, `facet`, `series`, `xend`, `label`, `drill`,
`sort_by`, `sort_dir`, `value_labels`, `connect`, `x_lines`, `smoother`,
`identity_line`, `box_points`, `summary`, `whiskers`, `connect_centers`,
`band_window`, `band_size`, `band_min_n`, `ref_lo`, `ref_hi`,
`waterfall_totals`, `facet_scales`, `facet_cols`, `title`, `subtitle`,
`caption`, `download`, `script`.

## Old boards and old code

- Saved boards: `blockr_deser.chart_block()` translates the old payload
  keys with `chart_legacy_args()`, silently. A bar saved without an
  orientation was drawn horizontal and a boxplot vertical, which is what
  `orientation = NULL` means now.
- Code: the constructor still accepts the old names through `...`,
  translates them with the same function and warns. The old names never
  reach the block's attributes, so a re-save writes the new names.
- Callers in other packages (blockr.pharma, blockr.ai) and the TypeScript
  port in blockx (`shared/chart.ts`, `server/chart.ts`) are updated to the
  new names in the same piece of work.

## New features (built)

- `ymin` / `ymax` on a line draw a shaded band, several pairs nesting (95 %
  outside 80 %) for a forecast fan. Grey for a single line, the line's
  colour with a colour split. On a scatter they stay error bars.
- `linetype`: a column whose values set the dash (history solid, forecast
  dashed from the last observed point on).
- `x_ranges`: named shaded ranges on x (`list(Pandemic = c(...))`).
- `legend`: `"bottom"` (default), `"top"`, `"marks"` (names on the marks: line and area ends, the last bar of a stack, a scatter group's centre, pie slices; other types keep the legend) or `"none"`. Replaces `direct_labels`.
- `format`, `unit`: number format of the value axis, tooltip and labels, as
  on the tile block.
- `type = "histogram"` with `bins` (default 30, as in blockr.ggplot): R cuts
  x at `pretty()` breaks into a `<x>_bin` column, the browser draws a bar
  with touching bars, a click on a bin filters x to its range.
- `type = "area"`: a line with its area filled, stacked across a colour
  split unless `position = "dodge"`.
- Line rules from `feat/line-rules` (merged): crosshair tooltip, a time axis
  for dates, no dots on dense lines, hairline grid, straight lines by
  default.

Not built: names in `x_lines` / `y_lines` as labels.

## Done means

- Every chart type draws as before: the option and interaction snapshots in
  `tests/js/__snapshots__/` pass, or a difference is explained (the toolbox
  and the palette so far).
- Boards saved with 0.2.x open with the same chart. Tested on saved boards,
  including one from a production deployment, before merging.
- `R CMD check` passes apart from the CRAN blockers below, and the docs and
  examples use neutral data.
- The assistant's argument spec and guidance (`R/chart-arguments.R`) use the
  new names.

## Not part of this

- The table and tile blocks. They take the same cross-block names later
  (`drill_target`, `selection`, `id`), and blockr.dplyr calls the group-by
  `by`, which the table and tile use as `group`.
- CRAN blockers outside the chart: blockr.ui is an Import and not on CRAN;
  blockr.theme, blockr.ai, blockr.assistant and blockr.extra are in
  Suggests and not on CRAN. How blockr.theme and viz relate (one palette and
  scale map consumed by viz and blockr.ggplot) is a separate design question.
