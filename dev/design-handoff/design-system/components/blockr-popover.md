# Gear popover — advanced settings menu

The gear button in a block's top-right corner opens the `.blockr-popover`:
the block's settings form. This doc is the spec for **what goes in it** and
**how its rows are laid out**. For the bare class list see
[blockr-row.md](./blockr-row.md#gear--popover).

Reference implementation: `blockr.bi/inst/js/drilldown-table.js:140`
(`popRow`) and `:173` (`buildCogwheel`). The drilldown table block is the
pattern; the drilldown chart block (`blockr.bi/inst/js/drilldown-chart.js:259`)
is the anti-pattern this doc exists to correct.

## Principle 1 — the card shows the result, the gear configures it

A block's resting surface is its **output** plus **direct interactions with
that output**. Everything that defines *what the output is* lives behind the
gear.

| On the card | Behind the gear |
|---|---|
| The chart / table / value | Column mapping (group, x, y, series, color, facet) |
| Sort, search, paginate | Chart type, aggregation function, metric |
| Click-to-drill, brush, hover | Coloring, rounding, sort order, theme |
| The echarts zoom/save toolbox (small, muted, always on) | Anything sourced from the block's `_arguments()` metadata |

The test for any control: *am I interacting with the data shown, or
redefining what data is shown?* Redefining → gear.

Why this rule, for blockr specifically: blocks compose densely into
workspaces and must render from ~300px to full screen (see
[design-system-prompt.md](../design-system-prompt.md)). An always-on config
bar costs vertical space in every block in every workspace, permanently, to
expose controls that are set once and rarely touched. Pushing them behind the
gear keeps the workspace quiet (the design system's "dense, quiet,
editor-like, no card chrome") and gives the result the full card.

Note on framing: for the drilldown family the column mapping is the block's
**transform definition** (what to group/aggregate, or which x/y to plot), and
the click/brush is the **filter it emits**. Both are configuration. The
mapping belongs behind the gear; the click/brush interaction stays on the
card.

## Principle 2 — each field is a vertical unit; fields flow in a grid

A **field** is one labelled control, stacked top to bottom:

```
[ label                    ]   .blockr-popover-label
[ full-width control       ]   width:100%; box-sizing:border-box
[ muted help text below    ]   #9ca3af, 0.75rem, line-height 1.3
```

Exact values (field internals, from `popRow`, `drilldown-table.js:140`):

| Element | Spec |
|---|---|
| Field | `display:flex; flex-direction:column; align-items:stretch; gap:4px; margin-bottom:12px` |
| Label | `.blockr-popover-label`, `margin-bottom:0` |
| Control | `width:100%; box-sizing:border-box` |
| Help text | `font-size:0.75rem; color:#9ca3af; line-height:1.3` |
| Popover title | `.blockr-popover-label`, `font-weight:600; margin-bottom:10px`, text "Advanced" |

A one-control-per-row column wastes horizontal space and gets very tall once
a block has more than a handful of settings. So fields **flow in a
responsive wrapping grid**: the popover is a `flex-wrap` container, each
field `flex: 1 1 ~190px; min-width: 0`, so a wide screen fits about three
across and a narrow one collapses to one. Use `flex-wrap` + `min()`, never
media or container queries (the general blockr rule, see
[design-system-prompt.md](../design-system-prompt.md)).

| Element | Spec |
|---|---|
| Popover container | `width: min(680px, 92vw); display:flex; flex-wrap:wrap; align-items:flex-start; gap:0 14px; max-height:70vh; overflow-y:auto` |
| Field | `flex: 1 1 190px; min-width: 0` (plus the vertical internals above) |
| Title / mode selector | `flex: 1 1 100%` — always own a full row |

Fixing the popover width with `min(680px, 92vw)` (rather than letting it size
to content) also keeps its footprint **stable when conditional fields toggle**:
switching chart type changes which fields show, but the popover must not
visibly resize from that. Width stays constant; only the wrapped height
changes, and the grid keeps that change to a row or two instead of a tall
jump.

Do **not**: put the label beside the control, put two controls inside one
field, or put help text *above* the control. (The grid places whole fields
side by side; it never splits a field.)

Why help goes *below*: a side description column gets squished. Help above
separates the label from its control and reads as noise before the reader
knows what the control is. Help below is read after the control has been
seen, only when needed, and degrades gracefully because it is the
lowest-priority element in the stack.

## Principle 3 — help text comes from `_arguments()`, terse and muted

Block argument descriptions already exist in the block's `_arguments()`
metadata (e.g. `blockr.bi/R/drilldown-chart-arguments.R`). The popover row's
help text is sourced from there, so one string serves both the LLM-facing API
and the human-facing settings form. Keep it to one line. The popover is a
settings form, not documentation; link out if more is needed.

**Exception — column pickers show the selected variable, not the role
text.** When a field maps a data column (X, Y, Group, Color, ...), the
static "X-axis column ..." sentence does not tell the user *which* variable
is mapped. For these fields the help line instead shows the selected column
in the usual `name (label)` convention (the column name plus its
`attr(x, "label")`, matching how selects and table headers surface labels
elsewhere), updated live as the selection changes. Fall back to the
`_arguments()` role text when the value is not a column (`(none)`,
`.count`, an aggregation function) or the column carries no label.

## Principle 4 — order by causality, not registration

Controls in the popover read in the order one would set them, not the order
they appear in `external_ctrl`:

1. Type / mode selector first (it gates everything below it).
2. Mapping (group/x/y/series/color/facet — and, for a chart, the metric +
   aggregation: a bar *is* a sum-of-group, so the aggregation is part of the
   mapping, not a step beside it).
3. Presentation (value formatting, sort, rounding, theme, coloring, layout).

There are only **two** field sections, Mapping and Presentation (plus the
type picker above and the Drill-down section below). An earlier draft carried a
third "Encoding" section between them; it was dropped because "encoding" is the
grammar-of-graphics name for the field→channel binding — i.e. a synonym for
*Mapping* — so the two headers read as duplicates. What had been "Encoding"
split cleanly anyway: the chart's metric/aggregation are mapping; the tile's
style/good-when/number-format/unit are presentation.

Conditional field sets (the chart's aggregated / individual / timeline
families, toggled via `dd-cfg-*` classes) stay conditional, but the
conditional logic moves *with the fields* into the popover. The popover stays
open across changes so re-encoding is not a repeated open/close.

## Principle 5 — what explicitly stays on the card

State the positive rule, not just "move things off":

- The echarts toolbox (zoom / restore / save) — the design system's "small,
  muted, always-on toolbox shared by every chart family"
  (`drilldown-chart.js:38`).
- The table search box and column sort headers.
- Click / brush / hover drill interaction.

These are interactions with the shown result, not configuration.

**Corollary — the result must self-describe.** Once mapping moves behind
the gear there is no always-visible bar saying which column is X, Y, group,
etc. The result itself becomes the only place that information lives, so it
must carry it: axis titles from the mapped column's label (chart), labelled
column headers (table), a legend for the color/series split, and a block
name that states the subject. A chart with bare numeric axes and no titles
is now a defect, not a clean look. (This was the concrete regression when
the drilldown chart's pickers first moved into the popover: the axes lost
their only labels — fixed by deriving `xAxis.name` / `yAxis.name` from the
column label, `drilldown-chart.js` `_axisTitle`.)

## Known trade-off

Moving mapping behind the gear means the current encoding is not visible at a
glance and re-encoding is a two-click action. Mitigations: keep the popover
open across changes (the chart already reopens it on family switch,
`drilldown-chart.js:354`), and let the block name carry the semantics
("AE max grade by subject"). The drilldown table block demonstrates the
trade-off is acceptable in practice. Accept it knowingly rather than
re-litigate per block.

## Status

The drilldown **table** block follows this spec today. The drilldown
**chart** block does not (mapping is an always-on card bar; popover rows are
horizontal and label-only; `_arguments()` help is unused). Aligning the chart
block is the tracked follow-up; this doc is the target.
