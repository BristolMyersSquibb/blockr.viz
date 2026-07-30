# static-chart parity harness

Three ways to look at the deck-side chart renderers next to the interactive
canvas chart. All of them read the same state matrix, `states.R`, so the sides
cannot drift apart. Run everything from the workspace root.

## 1. The blockr workflow (deck path, end to end)

```sh
Rscript blockr.viz/dev/parity/workflow.R          # http://127.0.0.1:3838/
BLOCKR_REPORT_STYLE=static Rscript blockr.viz/dev/parity/workflow.R
```

A dock board carrying every state as a chart block, plus an outline
extension. Front the **Outline** tab and switch it to **Output** (the eye next
to `<>`): each exhibit is the deck render of a state, and Output evaluates the
generated script, so exhibits do not wait for the chart panels. Front a chart
tab on the right for the canvas version of the same state; both panes are on
screen together.

`BLOCKR_REPORT_STYLE` picks the printed form: `code` (the default) compiles
the state to a plain dplyr + ggplot2 pipeline via `chart_expr()`; `static`
emits `static_chart()`, the renderer built to match the canvas.

## 2. The side-by-side app (strict A/B)

```sh
Rscript blockr.viz/dev/parity/preview.R           # http://127.0.0.1:3838/
```

One scrolling page, one row per state: the live canvas block, `static_chart()`
and the `chart_expr()` pipeline, each pane the same width. The radio at the
top drops a pane so the remaining two get half the page each. Both deck panes
go through `report_call()`, so they are the real printed forms; the emitted
code for both styles is in the collapsed panel under each row.

Compare at equal width -- the canvas sizes itself from the row count and picks
its facet columns from the space it has, so a narrow pane changes its layout,
not just its scale.

## 3. Screenshot pairs (pixel comparison)

```sh
Rscript blockr.viz/dev/parity/app.R 4271 &        # canvas board
Rscript blockr.viz/dev/parity/drive.R 4271        # canvas-<id>.png + sizes.csv
Rscript blockr.viz/dev/parity/static.R            # static-<id>.png
```

`drive.R` screenshots each chart card through chromote and records its pixel
size; `static.R` renders the same states at that size and at 96 dpi (the
canvas draws in CSS pixels), so the pairs in
`_scratch/static-chart-parity/` can be flipped between. This is the route for
judging spacing and type size, which the live app cannot settle.

## Notes

- Port 3838 is the only port the devcontainer forwards; every script takes a
  port argument (or `BLOCKR_PORT`) if something else already has it.
- The board feeding the outline uses dataset blocks, not `new_static_block()`:
  Output evaluates the generated script, and a static block emits
  `x <- get("data", envir = <environment>)`, which does not parse.
