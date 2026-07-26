# Rank block — ranked horizontal bars as an HTML table

Landed 2026-07-26. Mockups that led to it: `_scratch/rankbar-mockups/index.html`
(six variants, A–F).

## Why

Every ranked bar chart we ship is really a table that grew a bar. The bar is the
only part echarts is needed for, and using a chart there costs us what a table
gives away for free: search, click-to-sort, exact values in their own columns,
arbitrary row count, printable output. The trigger was a CDEx user pointing at
"Most Frequent AEs" (`blockr.cdex/dev/ae-bms-view.R`, a `new_chart_block(chart_type
= "bar", drill = "AEDECOD")`).

What the table lacked was colour mapping and faceting. Both are addable, and
faceting is where the table form wins outright: **a facet becomes a column**,
which a table does natively and a chart grid does badly.

## Shape

A third sibling, not a mode on an existing block:

- chart-block **vocabulary**: `group` / `color` / `facet` / `bar_mode` /
  `sort_by` / `drill`, plus `parent` and `compare`
- table-block **rendering**: the shared html-table chrome and CSS, sticky
  header, scroll at `max_height`, client-side search and sort

Rejected: extending `new_table_block()` (it grows a second personality and an
unscannable gear band) and a second renderer inside the chart block (every
downstream question becomes "is this an echarts?").

**Horizontal bars only.** That constraint is the whole reason the arg surface
stays small enough to be a good block. Anything else is the chart block.

## Files

| File | What |
|---|---|
| `R/rank-table.R` | the data half: `rank_prepare()` → rows + a column plan |
| `R/rank-table-html.R` | the HTML half: chrome, marks, `rank_table()` |
| `R/rank-table-css.R` | the CSS delta on top of `html_table_shared_css_fallback()` |
| `R/rank-block.R` | `new_rank_block()`, arg specs, guidance |
| `inst/js/rank-table.js` | search / sort / expand / row-click drill |
| `dev/verify-rank-block.R` | the five shapes on real ADAE, drill wired to a table |
| `tests/testthat/test-rank-table.R` | prepare + HTML contract |

`rank_prepare()` returns a **column plan** (`list(kind = "bar" | "barsplit" |
"bardiv" | "num", ...)`) and the renderer walks it. The renderer knows nothing
about group / facet / compare — which is what keeps five bar shapes in one
code path.

## Decisions worth remembering

**Large tables follow the table block, full stop.** Every row rendered, scroll
at `max_height` (600px), sticky header, search and sort client-side. No
pagination, no default cap, no virtualisation. `top_n` is opt-in and exists for
report exhibits only (a pptx slide has no scrollbar), and always draws a visible
fold row — never a silent truncation. If we ever need virtualisation both blocks
want it at the same moment and it belongs in `html-table.R`, once.

**The bar scale comes from the whole column, server-side.** Never from the
visible or filtered rows, or scrolling and searching silently rescale bars.

**A parent is not the sum of its children.** Each level is aggregated in its own
pass (`rank_aggregate()` twice), so distinct-subject counts stay correct: one
subject reporting three preferred terms in a class counts once for the class.

**Percentages use a per-facet denominator.** An arm's percentage is over that
arm's own N, never the pooled total.

**Facet beats colour when both are set**, and says so in the footer note.
Inventing a two-way (segments × columns) layout was not worth the arg surface.

**Every bad config is a message, not an error.** A missing column names itself
and tells the user to re-pick it; no red banner, no stack trace.

## Borrowed from the table block, not re-invented

Christoph, 2026-07-26: "borrow from table block: header style, show labels,
sorting, should look the same" — so the header cells come from the table
block's own `dt_th()`: same classes, same name-over-label two-tier cell (a
faceted column's sub-line is its `N = k`), same `.blockr-sort-icon` arrow. Every
column is a sort hook, including the bar columns: each cell carries `data-v`
with the raw number, so sorting a bar column sorts its value rather than
(absent) cell text.

**Labels are real data labels.** The label column's sub-line is the group
column's own `label` attribute (`AEDECOD` → "Dictionary-Derived Term", both
levels when nested), the measure column's says what it counts ("distinct
USUBJID"), a faceted column's is its `N = k`, the percentage column's names its
denominator ("of 225"), and a reduced measure reads "Mean: <label>" the way
`dd_metric_plan()` writes it. Sub-lines are kept SHORT on purpose: a long one
wraps inside a narrow column and inflates the whole header row.

**The gear is the shared engine.** `Blockr.DrilldownConfig`
(`inst/js/drilldown-config.js`) is host-agnostic; `rank-table.js` registers as a
third host beside `chart.js` and `table.js`, so the menu has the chart's exact
structure: Mapping (required `Rank by`, then `Measure` / `Of column` /
`Count distinct` conditional on the measure, then `+ Add mapping` offering
`Group into` / `Color by` / `One column per` as add-as-needed rows),
Presentation (`Order` pill, `Search bar`, `Split layout` when a colour split is
on), Titles, and a Drill-down section. Keys ARE the R config params, so
`onChange(key)` round-trips through `input$rank_block_action` (action `config`)
to the matching reactiveVal. The gear reads its state off `data-rank-cfg` on the
rendered `<table>` — one JSON attribute, because the three text slots must carry
null (auto) vs "" (none), which an HTML attribute cannot say.

**One control row, then the title band.** The search box moves up into the gear
row and sits LEFT of the gear, which keeps its canonical top-right spot; the
emptied chrome row is hidden. Same hoist the table block does. Title and
subtitle then use the canonical `.dd-table-titles` band BETWEEN that row and the
column headers (`.dd-table-caption` below the table), which is what the chart
and table blocks do. Hoisting the title into the control row instead cost 15px
of row height (45px against the table's 30px) — measured, not eyeballed. The
legend gets its own row under the title band, so a long legend can never push
the search box around.

**The chrome CSS is one definition, not two.** `inst/css/table.css` was scoped
to `.drilldown-table-container` only, so the rank table silently missed every
header-cell rule: its sub-labels ran 19px instead of 11px and wrapped, giving an
84px header row against the table's 59px. The selectors now read
`:is(.drilldown-table-container, .blockr-rank-container)` — one set of rules for
both containers, since they ARE the same chrome. `.drilldown-table-structured`
rules stay table-only. After that: control row 30px, header 59px, row 42px, cell
padding identical in both blocks.

**The gear structure mirrors the chart's, section for section.** Mapping
(`Rank by *`, `+ Add mapping`) → Aggregation (`Aggregate`, and `Of column` /
`Count distinct` as the measure needs them) → Presentation (`Sort`, `Order`,
`Search bar`, `Split layout`) → Titles → Drill-down. Two traps: the measure rows
belong in a trailing `aggTitle` section (leaving them in `mapping` puts them
under the Mapping header, which the chart does not do), and a host-level
`drillHint` renders a SECOND, empty Drill-down heading on top of the one
`drillToggle` already draws — the hint belongs on the sections spec.

**The chevron is the table block's.** `section_chevron_svg()` inside a
`.blockr-indent-btn`, with the row carrying `blockr-indent-toggle` /`collapsed`
so the shared rotation rule applies. Those four CSS rules live in
`html_table_delta_css()`, which the rank chrome deliberately does NOT inject (it
is the structured Table-1 typography and would restyle every cell), so they are
mirrored in `rank_table_css()` with a keep-in-sync note.

## The bar mark is the chart's, not a local invention

Christoph asked whether the rounded bar end should align with the charts. It
should, and the codebase had already decided: `inst/js/chart.js` sets no
`borderRadius` on any bar series (echarts default 0 = square both ends), stacks
with `stack: 'stack'` and no separating border, and groups with `barGap: 0` so a
group's bars touch. There is even a comment recording that a lone rounded
override was REMOVED because it "made the waterfall the only rounded chart". So
the rank table's 4px rounded data-end and 2px surface gaps — both taken from the
generic data-viz guidance — were the odd one out, not the house style.

Now: square ends, segments touch, grouped bars touch, and a single-series bar
takes `dd_palette(1L)` (the chart's first palette colour) rather than a CSS
token, so a rank table and a bar chart of the same data are the same blue and
follow a themed board's palette together.

**One deliberate divergence: the grey track stays.** echarts has no track behind
a bar because a chart has an axis; a table cell has neither, so the track is
what says "share of the column max" and keeps short bars comparable. The
crossfilter block in blockr.dm already draws track + fill, so there is
precedent. Drop it only if a bar-in-a-cell should read as a chart mark rather
than as a cell.

## Left open

**An ordered split gets categorical colours.** `color = "AESEV"` draws MILD /
MODERATE / SEVERE from `DD_BLOCKR_PALETTE` (blue / orange / yellow), because
matching the charts and the board scale map matters more than any local choice.
But severity is *ordered*, and an ordered variable reads better as one hue
stepped light→dark; the house yellow is also sub-3:1 on white, so it leans on
the adjacent numbers for legibility. The clean fix is a scale-map binding for
ordered factors in blockr.theme, not a divergent palette here. Flagged, not
decided.
