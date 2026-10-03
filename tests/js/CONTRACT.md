# The chart block's R <-> JS contract

What `inst/js/chart.js` (with `drilldown-config.js` and `busy-cue.js`) sends
to R and what it accepts from R. Derived from the code at viz cb5baef and
pinned by the snapshots in `__snapshots__/interactions/` (file names in
brackets). A rewrite has to keep the names, field sets and types below.

`<id>` is the chart element's id, `ns("drilldown_block")` in
`chart-block.R`, for example `block_3-expr-drilldown_block`. Every input is
sent with `{ priority: "event" }`, so an identical value sent twice reaches R
twice.

## Inputs the chart sends

### `<id>_action`, action `filter`, categorical

`{ action: "filter", filter_type: "categorical", column, values, nonce }`

- `column`: string, the drill column (the group, the colour for radar, the
  lane for gantt, series or colour for line and split scatter, or the drill
  override column).
- `values`: array of strings, the distinct non-null values of `column` over
  the clicked mark's rows, in row order, each through `String()`. Never empty
  (nothing is sent when it would be).
- `nonce`: integer, a per-chart counter that goes up by one with every
  categorical send (clicks and brushes, latched and transient). Not reset by
  clears or re-renders.
- When: a click on a mark with drill on (latched or with `ctrl_target`), a
  brush whose points map to rows with a drill column.
  [click-*, brush-scatter-color, brush-scatter-facet, brush-scatter-drill-column]

### `<id>_action`, action `filter`, range

`{ action: "filter", filter_type: "range", x_col, y_col, x_range, y_range }`

- `x_col`: string, `config.x`. `y_col`: `config.y`, or null when `y_range` is
  null.
- `x_range`, `y_range`: `[min, max]` numbers; `y_range` null for a brush event
  on a line chart.
- When: a scatter (or unsplit line, or band) click with drill `auto` and no
  split column sends a zero-width range at the clicked point; a brush with no
  drill column sends the brushed points' extent.
  [click-scatter-geometric, click-line-no-split, click-band-plain,
  brush-scatter-geometric, brush-line-event]

### `<id>_action`, action `filter`, clear

`{ action: "filter", filter_type, column: null, values: null, x_col: null,
y_col: null, x_range: null, y_range: null }`

- `filter_type`: `"categorical"` when the chart type is aggregated (bar,
  waterfall, pie, treemap, boxplot, pointrange, radar), else `"range"`
  (scatter, line, band and gantt), whatever the filter it clears was.
- When: a second click on the selected aggregated mark, Reset, a brush clear
  (outside the 150 ms / 450 ms window after a click), and in the gear after
  every column pick, role removal, drill change, and type switch across
  families. In the gear it follows the `config` message.
  [click-bar-latched, brush-*, gear-drive-*]

### `<id>_action`, action `filter`, point

`{ action: "filter", filter_type: "point", x_col, y_col, x_val, y_val }` is
built by `_sendPointFilter()` but nothing calls it.

### `<id>_action`, action `config`

Sent after every gear or sentence edit (and after a type switch), in full,
with these fields in this order:

| field | type, default when unset |
|---|---|
| `action` | `"config"` |
| `group` | string; left out of the message when the config has no group |
| `color`, `facet` | string, `""` |
| `value` | string as stored (may be `""`) |
| `func` | string as stored |
| `chart_type` | string |
| `x`, `y`, `xend` | string, `""` |
| `sort_by` | string, `""` |
| `sort_dir` | string, `"asc"` |
| `orientation` | string, `"horizontal"` |
| `bar_mode` | string, `"stacked"` |
| `value_labels` | `"on"` / `"off"` (a `true` in the config reads as on) |
| `na_group` | string, `"level"` |
| `pct_of` | string, `"facet"` |
| `baseline` | string, `"zero"` |
| `series`, `label`, `drill` | string, `""` |
| `smoother` | string, `"none"` |
| `connect` | string, `"monotone"` |
| `identity_line` | string, `"off"` |
| `value_lines`, `x_lines` | string: what was typed, or `String()` of the array R sent (`[5,12]` -> `"5,12"`, `[]` -> `""`) |
| `box_points` | string, `"none"` |
| `band_window` | string, `"adaptive"` |
| `band_size` | as stored, `45` |
| `band_min_n` | as stored, `12` |
| `band_id`, `ref_hi`, `ref_lo` | string, `""` |
| `count_on` | string, `"off"` |
| `count_col` | string, `""` |
| `facet_scales` | string, `"fixed"` |
| `facet_cols` | string, `""` (a number from R arrives as its string) |
| `download` | `"on"` unless the config holds `false` or `"off"` |
| `lo`, `hi` | string, `""` |
| `tt_fields` | array of strings, `[]` |
| `title`, `subtitle`, `caption` | string or null, verbatim (null = auto, `""` = none) |
| `ctrl_target`, `ctrl_table` | string, `""` |
| `script` | string, `""` |
| `sv_<name>` | one per `script_inputs` entry that is not an error: the value, `""` for null or an empty array |

`summary`, `whiskers`, `connect_centers`, `line_width_mult`, `dot_size_mult`,
`band_series`, `band_refs`, `filter_*` and `capture_*` are never sent back.
[gear-drive-*, sentence-slots]

### `<id>_action`, action `set_mults`

`{ action: "set_mults", line_width_mult, dot_size_mult }`, numbers (1 when
unset). Sent 150 ms after the last slider input. [gear-drive-scatter,
gear-drive-line]

### `<id>_need`

Value: the `data_rev` the chart lacks rows for. Sent once per rev when a
`drilldown-data` without `data` or a `drilldown-ready` names a rev the chart
does not hold. Reset when rows arrive. [msg-data-without-rows, msg-ready,
life-remount]

### `<id>_ready`

Value: `Date.now()`. Sent by the input binding's `initialize` when no
`drilldown-data` is waiting for that id (also on a re-mount and on a second
`initialize` of the same element). [life-bind-empty, life-remount]

### `<id>_capture`

`{ png, width, height }`: a PNG data URL and the composed size in CSS px.
Sent when `config.capture_export` is true and the hoisted download control
(`.blockr-action-menu__trigger` or `.dd-chart-dl .blockr-tool` in the gear
header) is clicked. Scale is `config.capture_ratio`, else 2.
[export-capture-input]

### `blockr_viz_capture_result`

`{ req, png, width, height }` or `{ req, error }`. Exactly one per
`drilldown-capture` request: the picture after the panels are drawn plus
1400 ms, an error at once when the payload throws, or
`"the chart did not compose in time"` after 20 s. [export-capture-*]

## Messages the chart handles

### `drilldown-data`

`{ id, columns, config, arguments, data_rev, data? }`

- `columns`: `[{ name, type: "numeric" | "categorical" | ..., n_unique,
  label?, levels?, kind? }]`.
- `data`: a column object, its JSON string, or a row array. A column may be
  dictionary encoded as `{ __enc__: "dict", levels, codes }`. Numeric NA may
  arrive as the string `"NA"`.
- `data_rev`: rows are re-parsed only when it changes (or, for a string, when
  the string changes). Left out `data` with a rev the chart holds: redraw with
  the held rows. With a rev it does not hold: `<id>_need`, no draw.
- Before the element is bound the message waits per id; a later message
  without rows for the same rev keeps the waiting rows, any other replaces it.
- `config`: the block's settings (see `build_chart_msg()`), plus what only
  the chart reads: `filter_column` + `filter_values` (a saved selection,
  restored when both are set and `ctrl_target` is empty), `ctrl_choices`,
  `title_resolved` / `_parts` / `_offers` (and subtitle, caption),
  `sentence_args`, `title_arg_values`, `script_inputs`, `script_error`,
  `sv_<name>`, `capture_export`, `capture_ratio`, `palette`, `scales`,
  `band_series`, `band_refs`, `band_note`, `smoother_series`.
- Effect: setData, redraw, and the busy cue ends two frames after a canvas
  exists. [msg-data-*, msg-before-mount, restore-*]

### `drilldown-ready`

`{ id, data_rev }`, R's answer to `<id>_ready`. Nothing when the chart holds
rows for that rev or the id is not bound; else `<id>_need`. [msg-ready]

### `drilldown-theme`

`{ id, theme }`. `"default"` and null mean no theme. A change of theme
re-creates the chart's echarts instances with it and redraws when there are
rows; the current theme again does nothing. Waits per id when the element is
not bound yet.
[life-theme-message, life-theme-before-data, msg-before-mount]

### `drilldown-capture`

`{ req, width, height, ratio, columns, data, data_rev, config }`. Draws the
chart in a hidden host (`.blockr-capture-host`, fixed at left -20000px, the
requested size) and replies on `blockr_viz_capture_result`. The host is
removed after a picture, and left in place after an error or the timeout.
[export-capture-*]

### `blockr-busy`, `blockr-busy-done` (busy-cue.js)

`{ id, label }` adds `blockr-busy` to the element after 300 ms and writes
`"<label>… <s> s"` into `.blockr-busy-line`; a second start while one is
pending is ignored. `{ id }` ends it, as does the chart's own draw.
[msg-busy, msg-busy-without-rows, busy-cue.test.js]

### data-bs-theme

Not a message: a change of `data-bs-theme` on `<html>` or any element under
it redraws every chart that holds rows. [life-bs-theme]

## v2 (`inst/js/chart-v2/`)

What v2 sends and reads differently from the above, by decision
(blockr.design `open/chart-block-v2/decisions.md`). Everything not listed
here is as above. R reads both engines' messages.

### `<id>_action`, action `filter`, categorical

`{ action: "filter", filter_type: "categorical", filters, nonce }`

- `filters`: object, column -> array of values, one entry per column; the
  columns AND together. A value is a string, or JSON `null` for a missing
  value (a mark keyed on a missing value, drill `auto` only). R matches a
  null with `is.na(col) | col %in% ""`. `column` / `values` are not sent.
- What it holds: with drill `auto`, the clicked mark's keys (group, colour
  where colour splits the mark, facet, series; D2, D3, D6, D7); with an
  explicit drill column, that column's values in the rows under the mark.
  A legend chip sends its level, `{<colour>: [level]}` (D8). A brush with
  a series column sends the series of the brushed points, plus the facet
  key in a facet panel (D9).
- `nonce`: as v1, one up per send, a re-click and a chip included (D1).
- [click-*, click-legend-*, brush-scatter-drill-column, restore-receipt]

### `<id>_action`, action `filter`, range

`{ action: "filter", filter_type: "range", x_col, y_col, x_range, y_range,
filters? }`

- As v1, plus `filters` when the point or brush was taken in a facet
  panel: `{<facet>: [value]}` (null for a missing facet value). R ANDs it
  with the range (D10). Left out otherwise, so a range outside a facet is
  v1's message.
- A brush without a series or an explicit drill column is always a range,
  whatever the colour (D9).
- [click-scatter-facet-geometric, brush-scatter-facet, brush-scatter-color]

### `<id>_action`, action `filter`, clear

As v1, with `filter_type` the type of the filter it clears (B1).

### `drilldown-data` config: the saved filter

R's block state is `filter_type`, `filters` (a named list of character
vectors, `NA` = missing), `filter_range` and `filter_point`;
`filter_column` / `filter_values` are legacy constructor arguments that
fold into `filters` and are never state. The config carries, isolated:

- `filter_type`: `"categorical"`, `"range"` or `"point"`.
- `filters`: the state's `filters`, each column a JSON array, NA as null;
  null when there is none.
- `filter_range`: `{ x_col, y_col, x_range, y_range }` when `filter_type`
  is `"range"`, else null.
- `filter_column` + `filter_values`: for v1, only when the filter is
  categorical, one column and has no missing value; else null and `[]`.

v2 restores `filters` (or, without it, `filter_column` + `filter_values`)
as a categorical selection, and a `"range"` with its `filter_range` and
the facet key in `filters` as a range selection; both dim what they do not
select (D5, D11). A transient chart (`ctrl_target` set) restores neither.
[restore-*, restore-bar-two-columns, restore-bar-missing,
restore-scatter-range-facet]

### Legend band

A chip is a filter control, not a visibility toggle (D8): no
`legendSelect` / `legendUnSelect`, no chip is ever `dd-legend-chip-off`;
the chip of the level the latched filter selects carries
`dd-legend-chip-on`. With drill off a chip does nothing.
