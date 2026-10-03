# The chart block's R <-> JS contract

What the chart scripts in `inst/js/chart-v2/` (with `drilldown-config.js` and
`busy-cue.js`) send to R and what they accept from R, pinned by the snapshots
in `__snapshots__/interactions/` (file names in brackets). The click rules
are the ones decided in blockr.design `open/chart-block-v2/decisions.md`.

`<id>` is the chart element's id, `ns("drilldown_block")` in
`chart-block.R`, for example `block_3-expr-drilldown_block`. Every input is
sent with `{ priority: "event" }`, so an identical value sent twice reaches R
twice.

## Inputs the chart sends

### `<id>_action`, action `filter`, categorical

`{ action: "filter", filter_type: "categorical", filters, nonce }`

- `filters`: object, column -> array of values, one entry per column; the
  columns AND together. A value is a string, or JSON `null` for a missing
  value (a mark keyed on a missing value, drill `auto` only). R matches a
  null with `is.na(col) | col %in% ""`.
- What it holds: with drill `auto`, the clicked mark's keys (group, colour
  where colour splits the mark, facet, series). In a gantt a mark's keys
  are lane and facet. With an explicit drill column, that column's distinct
  values in the rows under the mark. A legend chip sends its level,
  `{<colour>: [level]}`. A brush with a series column sends the series of
  the brushed points, plus the facet key in a facet panel. Nothing is sent
  when the filter would be empty.
- `nonce`: integer, a per-chart counter that goes up by one with every
  categorical send (clicks, chips and brushes, latched and transient, a
  re-click included). Not reset by clears or re-renders.
- When: a click on a mark with drill on (latched or with `ctrl_target`); a
  second click on the same mark sends again and never clears.
  [click-*, click-legend-*, brush-scatter-drill-column, restore-receipt]

### `<id>_action`, action `filter`, range

`{ action: "filter", filter_type: "range", x_col, y_col, x_range, y_range,
filters? }`

- `x_col`: string, `config.x`. `y_col`: `config.y`, or null when `y_range` is
  null.
- `x_range`, `y_range`: `[min, max]` numbers; `y_range` null for a brush event
  on a line chart.
- `filters`: `{<facet>: [value]}` when the point or brush was taken in a
  facet panel (null for a missing facet value); R ANDs it with the range.
  Left out otherwise.
- When: a scatter (or unsplit line, or band) click with drill `auto` and no
  series sends a zero-width range at the clicked point; a brush without a
  series or an explicit drill column sends the brushed points' extent,
  whatever the colour.
  [click-scatter-geometric, click-scatter-facet-geometric, click-line-no-split,
  click-band-plain, brush-scatter-geometric, brush-scatter-facet,
  brush-scatter-color, brush-line-event]

### `<id>_action`, action `filter`, clear

`{ action: "filter", filter_type, column: null, values: null, x_col: null,
y_col: null, x_range: null, y_range: null }`

- `filter_type`: the type of the filter it clears.
- When: Reset, a brush clear (outside the 150 ms / 450 ms window after a
  click), and in the gear after every column pick, role removal, drill
  change, and type switch across families. In the gear it follows the
  `config` message. An empty brush selection clears only a filter a brush
  made.
  [restore-*, brush-*, gear-drive-*]

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
`initialize` of the same element; either disposes the old view's echarts
instances first). [life-bind-empty, life-remount, life-initialize-twice]

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
  the chart reads: the saved filter (below), `ctrl_choices`,
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
removed after a picture, an error or the timeout.
[export-capture-*]

### `blockr-busy`, `blockr-busy-done` (busy-cue.js)

`{ id, label }` adds `blockr-busy` to the element after 300 ms and writes
`"<label>… <s> s"` into `.blockr-busy-line`; a second start while one is
pending is ignored. `{ id }` ends it, as does the chart's own draw.
[msg-busy, msg-busy-without-rows, busy-cue.test.js]

### data-bs-theme

Not a message: a change of `data-bs-theme` on `<html>` or any element under
it redraws every chart that holds rows. [life-bs-theme]

## The saved filter

R's block state is `filter_type`, `filters` (a named list of character
vectors, `NA` = missing), `filter_range` and `filter_point`;
`filter_column` / `filter_values` are legacy constructor arguments that
fold into `filters` and are never state. The `drilldown-data` config
carries, isolated:

- `filter_type`: `"categorical"`, `"range"` or `"point"`.
- `filters`: the state's `filters`, each column a JSON array, NA as null;
  null when there is none.
- `filter_range`: `{ x_col, y_col, x_range, y_range }` when `filter_type`
  is `"range"`, else null.
- `filter_column` + `filter_values`: the older one-column form, only when
  the filter is categorical, one column and has no missing value; else null
  and `[]`.

The chart restores `filters` (or, without it, `filter_column` +
`filter_values`) as a categorical selection, and a `"range"` with its
`filter_range` and the facet key in `filters` as a range selection. Both
light what holds filtered rows and dim the rest, a filter on a column the
chart does not draw included. A transient chart (`ctrl_target` set)
restores neither.
[restore-*, restore-bar-two-columns, restore-bar-missing,
restore-scatter-range-facet]

## Legend band

A chip is a filter control, not a visibility toggle: no `legendSelect` /
`legendUnSelect`, no chip is ever `dd-legend-chip-off`; the chip of the
level the latched filter selects carries `dd-legend-chip-on`. With drill
off a chip does nothing. [click-legend-*]
