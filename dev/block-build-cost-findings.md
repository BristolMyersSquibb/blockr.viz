# Block build-up cost — findings & fix ideas

_2026-07-10. Investigation prompted by the table block feeling slow to build/
appear on a view switch. Question asked: "are we doing too much on the R side
(loops)? profile the build-up cost of the blocks."_

## TL;DR

- The table block's **R render is cheap** — 0.77 ms for the structured summary
  it renders in production. There are **no expensive data loops**; the render
  path is already vectorised.
- The real, repeated cost is **`htmltools::htmlDependency()` rebuilding the same
  dependency objects on every render and every block construction**, each doing
  disk I/O (`packageVersion()` → `read.dcf`, plus `system.file`/`find.package`).
- This is **memoizable**. Measured: caching the table chrome's dependency list
  drops `dt_chrome()` from **4.70 ms → 0.16 ms (29×)**. The same pattern inflates
  dplyr/chart/summary block **construction** (~6–9 ms each), which is the cost
  that scales with block count at startup.

## How to reproduce

Two harnesses were added (`blockr.viz/dev/`):

- `profile-blocks-r.R` — R-side micro-profiler. Times block **construction**
  (`new_*_block()`) and **render** (the HTML/expr build), median of N reps, with
  an Rprof deep-dive on the table's structured & flat render.
  `REPS=30 SIZE=prod Rscript blockr.viz/dev/profile-blocks-r.R`
  (`SIZE=prod` = 300-subject adsl / `large` = 20k rows).
- `profile-blocks-e2e.R` — chromote + CPU/network throttle, per-view
  "time to appear" (dormant → wake). **Caveat:** the multi-block board dies
  under `pkgload::load_all` with `addResourcePath ... blockr-core-js,
  directoryPath=''` — a load_all `system.file`-shim artifact, **not** a prod bug
  (installed packages resolve fine). Table-block E2E numbers below came from the
  `preview-view-switch-latency.R` board, which renders under load_all.

## Findings

### 1. Build cost per block (prod data, warm, median of 30)

| pkg | block | stage | median | kind |
|---|---|---|---:|---|
| blockr.viz | summary_table | compute (the summarise) | 10.95 ms | real work |
| blockr.ggplot | ggplot | ggplot_build (300 pts) | 8.47 ms | real work |
| blockr.dplyr | mutate | construct | 8.77 ms | **dep rebuild** |
| blockr.dplyr | filter | construct | 8.59 ms | **dep rebuild** |
| blockr.viz | chart | construct | 6.90 ms | **dep rebuild** |
| blockr.dplyr | select | construct | 6.36 ms | **dep rebuild** |
| blockr.viz | summary_table | construct | 5.94 ms | **dep rebuild** |
| blockr.viz | tile | render | 5.81 ms | mixed |
| blockr.viz | table | render:structured:chrome | 4.80 ms | **dep rebuild** |
| blockr.viz | table | render:flat:body (300×7) | 2.55 ms | render |
| blockr.viz | table | render:structured:body (3×9) | 0.77 ms | render (prod case) |
| blockr.viz | table | construct | 0.15 ms | — |
| blockr.viz | tile | construct | 0.14 ms | — |
| blockr.viz | table | is_structured | 0.01 ms | — |

The table's actual render sits at the bottom. Everything marked "dep rebuild" is
the same underlying cost.

### 2. Root cause: htmlDependency rebuild = repeated disk I/O

Rprof of `dt_chrome()`: ~95 % of the time is `drilldown_table_dep()` →
`htmltools::htmlDependency` → `utils::packageVersion()` (`read.dcf`, ~45 % of
total) + `system.file()`/`find.package()`. Rprof of `new_filter_block()`: ~95 %
is the dependency list its `expr_ui` builds, same disk-read breakdown.

Every dependency builder has this shape (`drilldown_table_dep`,
`blockr.viz/R/table-block.R:1095`):

```r
drilldown_table_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name    = "blockr-blocks-css",
      version = paste0(utils::packageVersion("blockr.dplyr"), ".2"), # read.dcf
      src     = system.file("css", package = "blockr.dplyr"),         # disk stat
      stylesheet = c("blockr-blocks.css", "blockr-select.css")
    ),
    ... 5 more htmlDependency() calls, each hitting disk ...
  )
}
```

It takes no arguments and returns the same object every call within a running
process — `packageVersion()`/`system.file()` cannot change while the app runs.
So the work is pure repetition.

Single-call costs (warm): `packageVersion()` ≈ 0.29 ms, `system.file()` ≈
0.42 ms. `drilldown_table_dep()` makes ~4 of each → the ~4.8 ms chrome.

### 3. What is NOT the problem

- **Table render loops.** The render is vectorised end-to-end; a comment at
  `table-block.R:170` notes a past per-cell `vapply` (72 % of build) that was
  already removed. Flat-body render is 2.55 ms for 300×7, structured 0.77 ms.
- **`summary_table` compute (10.95 ms)** and **`ggplot_build` (8.47 ms)** are
  genuine computation, not rebuild waste. Leave them alone.

### 4. E2E "time to appear" (6× CPU, 150 ms latency)

- Pre chrome-fix: chrome 1089 ms, body 1201 ms.
- Post chrome-fix (control section no longer gated on `data()`, shipped on
  `fix/table-chrome-immediate-render`): chrome ~2–300 ms, body ~300 ms.

The chrome fix removed a client round trip; dep memoization would additionally
cut the chrome's own build (~4.8 ms → ~0.16 ms).

## Fix ideas

### A. Memoize the dependency builders (primary, high leverage)

Add a one-line helper and wrap each nullary `*_dep()` builder in a lazy
process-level cache:

```r
# blockr.viz/R/viz-block-dep.R (and mirror in blockr.dplyr)
memoise_dep <- function(build) {
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- build()
    cache
  }
}

drilldown_table_dep <- memoise_dep(function() {
  htmltools::tagList( htmltools::htmlDependency(...), ... )
})
```

First call pays the disk reads once; every call after is free.

- **Builders in blockr.viz** (~8): `drilldown_table_dep` (table-block.R:1095),
  `drilldown_chart_dep`/`drilldown_echarts_themes_dep` (chart-dep.R),
  `summary_table_block_dep` (summary-table-block.R:277),
  `viz_block_css_dep`/`settings_band_dep`/`drilldown_shared_dep`/`viz_echarts_dep`
  (viz-block-dep.R), `tile_block_dep` (tile-block.R:320).
- **Builders in blockr.dplyr** (~6, called from `js-block.R:203–209` in
  `expr_ui`): `blockr_core_js_dep`, `blockr_blocks_css_dep`, `settings_band_dep`,
  `blockr_select_dep`, `blockr_input_dep`, and `js_block_dep(name)`. The last
  takes an argument → use a keyed cache (`cache[[name]]`) instead of nullary.

**Why it's safe.** `htmlDependency` objects are immutable value lists; htmltools
already shares them across sessions/users when de-duping by name/version. Nothing
writes to them, so a process-level cache shared across Shiny sessions is correct.
The cache lives for the process — a dev reinstall or app restart is a new process
and rebuilds, so no stale-path risk in practice.

**Expected win.**
- Table chrome render: ~4.8 ms → ~0.16 ms, per block, every render.
- dplyr/chart/summary **construction**: ~6–9 ms → near-zero, which scales with
  block count at startup (the ~3.1 s `board_ui` rebuild for 100 blocks).
- No behaviour change; no effect on the genuine-work items.

Delivery: add helper, wrap builders, bump both package versions, reinstall,
re-run `profile-blocks-r.R` to show before/after in the same table.

### B. Render chrome before data (already done for table; tile still open)

Separate from build cost but same felt-latency theme. The block's control-pane
UI should not be gated on `data()`.

- **table** — FIXED (`fix/table-chrome-immediate-render`): `output$dt_result`
  renders the chrome immediately instead of `req`-ing `dt_is_structured(data())`.
- **tile** — STILL AFFECTED. `ui` is a bare `uiOutput("tile_result")` and the
  sole `renderUI` does `d <- plain_data(); req(is.data.frame(d))`
  (tile-block.R:202–203) before `tile_html()` builds the whole card. Needs the
  same treatment: render a static container immediately, defer only the inner
  body.
- **chart** — not affected (static `div` + `sendCustomMessage`).
- **cdisc DM badge** (blockr.dm) and **patient-profile gear popover**
  (blockr.pharma) — minor, cosmetic; primary controls already paint immediately.

## Suggested order

1. **A — dep memoization** (viz + dplyr): biggest, cheapest, board-wide win.
2. **B — tile chrome-before-data**: matches the table fix, removes the tile's
   wake-up lag.
3. Optional: get `profile-blocks-e2e.R` running against an installed, consistent
   package set so every block type gets a real time-to-appear number (the
   devcontainer's installed set is currently stale — core 0.1.3 vs source
   0.2.0.9002 — so this needs a fresh install first).
