import { test, expect } from '@playwright/test';

// The one interaction shinytest2 can't drive: a genuine ECharts canvas click.
// ECharts renders bars to a <canvas>; zrender hit-tests native mouse events, so
// a synthesized DOM event won't trigger the drill. We locate the target bar via
// the ECharts instance's convertToPixel(), then fire a real page.mouse.click().
//
// Board (dev/e2e/chart-drill-app.R): data -> chart(drill="region") -> out table.
// Clicking the "North" bar must filter the chart's downstream output to North's
// rows, which the `out` table renders (6 rows -> 2: products A and B).
//
//   Rscript dev/e2e/chart-drill-app.R                       # serves :3838
//   cd dev/e2e && npm install                               # once
//   npx playwright test chart-drill.spec.mjs                # against :3838
//   # or VIZ_E2E_URL=http://127.0.0.1:7901 npx playwright test chart-drill.spec.mjs

const CHART = '#board-block_chart-expr-drilldown_block';
const OUT_ROWS = '#board-block_out-expr-dt_result table tbody tr';

test('clicking the North bar drills and filters the downstream table', async ({ page }) => {
  await page.goto('/');

  // Chart lazy-renders on scroll into view; wait for its canvas.
  await page.locator(`${CHART} canvas`).scrollIntoViewIfNeeded();
  await page.waitForSelector(`${CHART} canvas`, { timeout: 30_000 });
  await page.waitForTimeout(1500); // let the bar geometry settle

  // Baseline: the downstream table shows every row.
  await expect(page.locator(OUT_ROWS)).toHaveCount(6);

  // Pixel center of the North bar (value axis 75 of 150, category "North"),
  // mapped through the live ECharts instance and offset by the canvas origin.
  const pt = await page.evaluate((sel) => {
    const div = document.querySelector(`${sel} .dd-chart-grid`).querySelector('div');
    const inst = window.echarts.getInstanceByDom(div);
    const local = inst.convertToPixel({ gridIndex: 0 }, [75, 'North']);
    const rect = div.getBoundingClientRect();
    return { x: rect.left + local[0], y: rect.top + local[1] };
  }, CHART);

  await page.mouse.click(pt.x, pt.y);

  // The drill filters region == "North" -> products A and B (2 rows).
  await expect(page.locator(OUT_ROWS)).toHaveCount(2, { timeout: 10_000 });
});
