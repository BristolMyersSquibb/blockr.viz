# Playwright canvas-drill e2e (manual)

The shinytest2 suite (`tests/testthat/test-shinytest2.R`) drives every
interaction blockr.viz exposes **except** a real click on the ECharts chart:
ECharts renders to a `<canvas>` and zrender hit-tests *native* mouse events, so
a synthesized DOM event won't trigger the drill. This directory holds a small,
opt-in Playwright test that fires a genuine `page.mouse.click()` at a bar's
pixel coordinates (located via the live ECharts instance's `convertToPixel()`).

It is **not** part of `R CMD check` / CI — it needs Node + a browser and a
running app. Run it by hand when touching chart click-to-filter.

## Run

```sh
# 1. Serve the fixture board (data -> chart[drill] -> downstream table).
Rscript dev/e2e/chart-drill-app.R          # http://127.0.0.1:3838
# (override the port if 3838 is busy: VIZ_E2E_PORT=7901 Rscript dev/e2e/chart-drill-app.R)

# 2. Install the test deps once.
cd dev/e2e && npm install

# 3. Run against the app.
npx playwright test                         # uses :3838
# VIZ_E2E_URL=http://127.0.0.1:7901 npx playwright test    # if you overrode the port
```

The test uses the container's system chromium (`/usr/bin/chromium`,
`--no-sandbox`); override with `PLAYWRIGHT_CHROMIUM=/path/to/chrome`.

## What it asserts

Clicking the **North** bar drills `region == "North"`, filtering the chart's
downstream output to North's two rows (products A and B) — the `out` table goes
from 6 rows to 2. If the drill contract or the canvas hit-test regresses, the
row count stays at 6 and the test fails.
