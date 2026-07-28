# Summarize table: what crosses the wire

_2026-07-28. Question asked: "does the summarize table send only data, no HTML
formatting, from R to JS? Is it efficient — compare with the chart block and
the table block?"_

Harness: `dev/profile-payload-bytes.R` (`REPS=6 Rscript
blockr.viz/dev/profile-payload-bytes.R`). Shiny's websocket is never
compressed, so **raw** bytes are what the browser waits on; gzip columns are
shown only to expose repetition.

## TL;DR

- **Yes, it is a data push.** 98.3% of the payload is numbers and plain
  strings. The only markup is `head` (the `<table>` open tag + `<thead>`),
  5.3 KB and **O(ncol), not O(nrow)** — plus a `kind = "html"` error path that
  only fires for non-renderable states.
- **The bar path is the best of the three blocks** — 95 bytes per table row,
  **12.8× smaller than the equivalent server-rendered HTML**, and 5.7× smaller
  than what the chart block ships for the same input, because it aggregates in
  R and the chart ships raw rows.
- **The distribution path is 7× heavier than the bar path** (698 vs 95
  bytes/row) and **47% of it is pre-rendered prose tooltips** — not HTML, but
  still presentation built in R.
- **Bytes are not the bottleneck; compute is.** At prod scale the dist config
  spends **556 ms in `rank_prepare`** against **32 ms** building and
  serializing the payload. `quantile()` runs once per group per facet.

## Measured

AE-shaped long frame, `count_distinct(USUBJID)` by preferred term, faceted by
arm. "3 cols + dist" = a count bar + a box column + a dot/mean±SD column.

### prod (4,800 input rows → 400 terms × 3 arms)

| payload | raw | gzip | bytes/row | build |
|---|---:|---:|---:|---:|
| summarize (bar + facet) **JSON** | **37.0 KB** | 4.9 KB | 94.8 | 28.8 ms |
| summarize (bar + facet) HTML | 472.3 KB | 17.7 KB | 1209.1 | 44.8 ms |
| summarize (3 cols + dist) **JSON** | **272.5 KB** | 77.8 KB | 697.6 | 556.0 ms |
| summarize (3 cols + dist) HTML | 1525.1 KB | 140.3 KB | 3904.3 | 640.9 ms |
| table block, same aggregated rows | 18.5 KB | 3.2 KB | 47.4 | 4.1 ms |
| chart block, raw input rows | 211.0 KB | 22.3 KB | 45.0 | 1.3 ms |
| table block, raw input rows | 371.1 KB | 53.1 KB | 79.2 | 15.2 ms |

Same shape at 1,800 rows (12.4 KB bar / 87.4 KB dist) and at 12,000 rows
(72.8 KB bar / 541.2 KB dist). Per-row cost is flat, so the encoding scales
linearly with **table** rows, not input rows.

### Cross-block comparison

The three blocks sit at three points on one axis:

| block | what it ships | per-row | scales with |
|---|---|---:|---|
| chart | raw values of the mapped columns, no formatting at all | 45 B | **input** rows |
| table | `cls` + `disp` per cell (display strings, CSS class) | 47 B | table rows |
| summarize, bar | widths + sort values + label strings | 95 B | table rows |
| summarize, dist | 8 geometry vectors + prose tooltip + label | 698 B | table rows |

The chart block is the purest data push (45 B/row is just the numbers) but it
has no aggregation seam, so it ships every input row: **211 KB where the
summarize table ships 37 KB for the same dataset.** The table block's cell
model is the same architecture as the summarize table's — 55% of its bytes are
`disp` display strings, per-cell `cls` is only 0.4% because it repeats.

### Where the dist bytes go (272.5 KB, prod)

| field | KB | % |
|---|---:|---:|
| `tip` prose strings (6 columns) | 128.4 | **47.1** |
| box geometry `wl w1 bl bw bc b2 w2 wh` (3 cols) | 45.9 | 16.8 |
| `pointrange` geometry `c l rw` (3 cols) | 20.1 | 7.4 |
| `disp` + `v` (all columns) | 33.0 | 12.1 |
| `label` (row stubs) | 8.2 | 3.0 |
| `head` markup | 5.3 | **1.9** |
| `nn` (never read by the client) | 4.8 | 1.8 |
| `chrome` | 0.1 | 0.0 |

A tip reads `n=10 · Median 53.47 · Q1–Q3 43.85–58.58 · 1.5×IQR 24.99–76.36`
(~69 bytes). Roughly 35 of those bytes are the labels `n=`, `Median`, `Q1–Q3`,
`1.5×IQR` — identical on every row of the column.

## Findings

### 1. The markup claim holds

`rank_flat_payload()` (`R/rank-push.R:972`) emits `head` as the only HTML
string; a scan of the serialized payload with `head` removed finds **zero
angle brackets**. `label`, `parent`, `fold`, legend labels are plain text
escaped **client-side** (`rank-table.js:541`). The gear config rides inside
`head` as `data-rank-cfg` — 0.8 KB, re-sent on every push although the gear
only re-reads it on popover open.

### 2. `tip` is presentation built in R, and it is half the payload

The tooltip is the only carrier of the **raw** statistics: the geometry
vectors are scaled percentages, and the domain (`dmin`/`dmax`) is never
shipped. So the numbers ride twice — once as positions, once as prose — and
the per-row prose repeats the column's constant labels.

Shipping the 5–6 raw stats per row plus a per-column template scalar would
replace `tip` (128 KB) **and** the 4 derived geometry vectors that are
differences of the others (`w1`, `bw`, `b2`, `w2` for box; `rw`, `ow` for
pointrange). Estimated: **272 KB → ~90 KB, a 3× cut**, with the client
formatting the tooltip from data.

The comment at `R/rank-push.R:85-89` explains why the differences are computed
in R (`String(24.13 - 10.5)` is not `"13.63"` in JS). That is a real float
printing concern, but it applies to *display* strings, not to CSS percentages
where a trailing digit is invisible.

### 3. Dead payload fields

- **`nn`** (box, pointrange, sparkline) is serialized on every push and
  **never read** — `grep -o "[.]nn[^a-zA-Z]" inst/js/rank-table.js` returns
  nothing. It exists only to build `tip` in R. 4.8 KB at prod scale, free to
  drop.
- **`chrome$foot$reset`** is shipped and never read; the Reset button lives in
  the one-shot chrome shell (`rank_footer_tag`, `R/rank-table-html.R:306`).
- A **text/field column** ships `v` and `disp` as the *same string*
  (`R/rank-push.R:410`) — exactly 2× that column's bytes.
- **`sub`** is `rows$.level > 0L` (`R/rank-push.R:127`), which the client
  already derives from the shipped `level` vector (`rank-table.js:535`).

### 4. The real cost was `rank_prepare`, not the wire — FIXED

Prod, 3 cols + dist, per render, before:

| stage | ms |
|---|---:|
| `rank_prepare` (the aggregation) | **680.7** |
| `rank_cells` | 102.7 |
| `rank_flat_payload` | 24.0 |
| `toJSON` | 7.6 |

Rprof put 34% of `rank_prepare` in `stats::quantile` → `sort.int` and 40% in
`as.data.frame.list`. Three costs were stacked in `lane_stat_agg`'s per-group
`f()` (`R/lane-stats.R:130`):

1. Each `lane_summarize()` call ran `stats::quantile()` once **per
   probability**, and each call re-sorts. A box column asks for two statistics
   (body + whiskers) = 6 sorts of the same group's values.
2. `out$.n` came from a whole extra `lane_summarize(v, "min_max")` — a 7th
   sort, to reach a `length()`.
3. `as.data.frame()` ran once per group on a list of scalars, spending more
   time validating names than the statistics took to compute.

**Fix (applied).** `lane_stat_basis()` sorts once per group and carries
`n`/`mean`/`sd`; `lane_q()` reads quantiles off it by index; `.n` is read
straight off the basis; the one-row frame is built directly.
`lane_summarize()` keeps its signature as a thin wrapper, so its callers and
tests are untouched.

| | before | after | |
|---|---:|---:|---|
| `lane_stat_agg` in isolation | 342 ms | **28 ms** | 12.2× |
| dist render, small | 178.7 ms | **63.6 ms** | 2.8× |
| dist render, prod | 556.0 ms | **142.0 ms** | 3.9× |
| dist render, stress | 1107.0 ms | **279.9 ms** | 4.0× |

Payload bytes are unchanged, by design. The first attempt wrote the
interpolation as `lo + h * (hi - lo)`, which is algebraically equal to
`quantile.default`'s `(1 - h) * lo + h * hi` but **not** float-equal: ~2.5% of
the AE table's values moved by ~1e-12, and 61 of them sat on a rounding
boundary, flipping a displayed digit (`31.59` → `31.6`). `lane_q()` now
reproduces `quantile.default`'s expression and its two guards exactly. Pinned
two ways: a bit-identity test against `stats::quantile()`
(`test-lane-stats.R`, `expect_identical` not `expect_equal`), and a
byte-for-byte diff of the whole prod payload against the old implementation
(279,329 bytes, identical).

After the fix the `rank_prepare` profile is flat — no single hot spot above
6%, the remainder being dplyr's grouping machinery.

## Suggested order

1. ~~Vectorise the dist aggregation in `rank_prepare`.~~ **Done**, 3.9× at
   prod scale.
2. **Drop `nn`, `foot$reset`, `sub`, and the text-column `disp`/`v`
   duplication.** Pure deletion, no design change, ~7% of the payload.
3. **Replace `tip` with raw stats + a per-column format template**, and ship
   `dmin`/`dmax` so the client derives `w1`/`bw`/`b2`/`w2`/`rw`/`ow`.
   ~3× payload cut on the dist path. Bigger change — it moves the tooltip
   wording into JS, so the static/report HTML consumer
   (`rank_cells_html()`) keeps its own copy and the drift guard in
   `test-rank-push.R` needs a matching pair.
4. **Move `data-rank-cfg` out of the per-push `head`** into the one-shot
   chrome, or into its own rarely-sent message.

Nothing here argues against the current architecture — the cell model already
beats server-rendered HTML by 12.8× on the bar path and 5.6× on the dist path.
These are refinements inside it.
