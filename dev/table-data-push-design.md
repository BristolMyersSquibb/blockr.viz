# Table block: data-push transport + windowed rendering

Status: in progress (feat/table-data-push). Motivation, protocol and phases
for moving the table block's body off `renderUI` onto the chart-style
custom-message transport, with client-side windowed rendering for large flat
tables.

## Why

The body render is `output$dt_table <- renderUI(...)`: every upstream data
change rebuilds the whole `<table>` as HTML (5.2 MB at 10k x 13), ships it,
Shiny swaps the subtree, and table.js re-wires sort/click/collapse on the
fresh DOM. Measured cost split at 10k rows: R build 43% / browser
insert+layout 45% / JS init 12% — the browser half is structural (every row
in the DOM) and untouchable from R. The chart block already has the right
shape: persistent shell, `sendCustomMessage` with column JSON, content-rev
guard, in-place updates.

## Architecture

The chrome (search bar, gear, scroll wrapper, status/download slots) stays a
one-shot `renderUI` — unchanged. The body becomes:

- R: one plain `shiny::observe` builds a payload and
  `session$sendCustomMessage("blockr-viz-table-data", ...)`. A single observe,
  NOT observeEvent + channels: that exact shape is what the blockr.dock
  lazy-eval card-probe pairing suspends for hidden panels (see the comment
  at chart-block.R "Push data + config to JS"). The whole payload is
  serialized once with jsonlite (dodges the auto_unbox trap) and cached: an
  identical JSON string is not re-sent (chart's last_msg guard), and `rev`
  ticks only on real change so JS can skip re-parsing.
- JS: `Shiny.addCustomMessageHandler("blockr-table-data", ...)` finds the
  container via `[data-dt-elem-id]`. Payloads are kept in a persistent
  per-elemId store (not a one-shot queue): a payload arriving before the
  chrome exists waits there, and a container re-created later (dock panel
  re-mount / view switch) re-renders instantly from the store with no R
  round trip.

## Payload kinds

Two kinds, chosen by what dominates:

- `kind: "html"` — structured ("Table 1") tables, message tables, and error
  states. These are small; the payload carries the full `<table>` HTML
  rendered by the EXISTING builders (`dt_table_tag_structured`,
  `dt_message_table` + `dt_table_attrs`). JS injects it and runs the
  existing `wireTable` path (DOM sort / collapse / search) untouched. Zero
  behavioral change, zero markup duplication.
- `kind: "flat"` — the scale path. Column-oriented cell model; JS assembles
  rows and renders only a window.

Flat model (see types.d.ts `VizTablePayload`):

- `head`: HTML string of the `<table ...data-dt-*><colgroup/><thead/><tbody
  (empty)/></table>` — built by the same R tag builders (`dt_th`,
  `dt_colgroup`, `dt_table_attrs`, `stamp_ctrl`), so the gear keeps reading
  its state off the table's data attributes exactly as today.
- `cols`: array of `{cls, disp, raw?, style?}`; entry 0 is the stub.
  `disp` are PLAIN (unescaped) display strings, `null` for NA (JS renders
  the em-dash cell and skips raw/style, byte-matching the R renderer);
  `raw` plain values only for drill/group columns; `style` pre-built
  ` style="..."` chunks (shading gradients / heatmap colors — generated,
  no user content). JS escapes text and attributes with the same &<>(")
  rules htmltools uses.
- `nodrill`: sparse row-index list for `dt-row-nodrill`.
- `bar`: per-row stub box-shadow chunks when row coloring is on.

Payload size: disp strings ≈ the data itself; the per-cell tag overhead
(~30 bytes x n_cells) that dominated the HTML payload is gone.

## Windowed rendering (flat only)

Above ~500 rows, JS renders only the rows near the viewport (window ≈ 120,
re-rendered as scrolling approaches an edge) between two spacer `<tr>`s
whose heights are `k * avg_row_height` (measured off the first window;
fixed table-layout means spacers never affect column widths). Row striping
is class-based (no nth-child rules), so windowing cannot shift stripes.
Under the threshold the same code path renders all rows.

Search and sort operate on the model, not the DOM: sort keys come from
`parseNum(disp)` (same semantics as today's textContent sort — displayed
values, not raw), search matches a lazily built lowercased per-row concat
of `disp`. Both produce an index order/filter that the window renders
through. Active-row highlight is applied at window-render time (matching
`data-dt-active` against the model's raw vectors), so it survives scrolling
and re-renders.

Click drill stays a delegated tbody listener reading `data-raw` off the
rendered cells — unchanged contract with the R `_action` observer.

## The one hard-won gotcha: the dep must ship with the STATIC ui

The block's `ui =` must include `drilldown_table_dep()` directly (chart-block
parity), not rely on the chrome renderUI to bring it. The first payload rides
the same flush batch as the chrome HTML; a handler registered by an
async-loading script (injected with the renderUI output) arrives too late,
and **Shiny silently drops custom messages that have no registered handler**.
Symptom: every table blank on first load, while a later config edit (which
triggers a re-send) renders fine. The JS-side payload store cannot save you
here — the store never sees the dropped message either.

## What does NOT change

- `drilldown_table()` (the exported standalone renderer) and
  `dt_table_tag()` keep rendering full HTML — vignettes, tests, static use.
  The flat cell computation is factored so the tag builder and the payload
  builder consume the same vectors (parity by construction).
- Chrome outputs `dt_status`, `dt_download`, the xlsx handler, the `_action`
  observer, gear engine, ctrl-send: untouched.
- Structured-table markup, collapse, drill keys: untouched (html kind).

## Perf expectations vs the renderUI path (10k x 13)

- payload: ~5.2 MB HTML -> roughly the data size as JSON (~1-1.5 MB)
- browser: full-DOM swap + full re-wire -> ~120 rows in DOM, O(window)
- R build: unchanged (already vectorized) minus the renderTags walk
- sort/search: O(rows) DOM walks -> array ops + O(window) render
- dock view switch: re-render from the client store, no R round trip
