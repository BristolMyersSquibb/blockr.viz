// Standalone Playwright screenshotter for the tile demo board. Uses the system
// chromium with its own user-data-dir so it doesn't collide with the MCP
// browser lock. Run:
//   node blockr.bi/dev/tile-shot.mjs
import { chromium } from 'playwright-core';

const OUT = '/tmp/tile-shots';
const URL = process.env.TILE_URL || 'http://localhost:3838';
const PROFILE = process.env.TILE_PROFILE || '/tmp/tile-pw-profile';

const ctx = await chromium.launchPersistentContext(PROFILE, {
  headless: true,
  executablePath: '/usr/bin/chromium',
  args: ['--no-sandbox', '--disable-dev-shm-usage'],
  viewport: { width: 1280, height: 2200 },
});
const page = ctx.pages()[0] || await ctx.newPage();
const logs = [];
page.on('console', m => logs.push(`[${m.type()}] ${m.text()}`));
page.on('pageerror', e => logs.push(`[pageerror] ${e.message}`));

await page.goto(URL, { waitUntil: 'networkidle' });
// Wait for the dock + at least one tile card to render (blockr boards build via JS).
await page.waitForSelector('.tk-block', { timeout: 30000 }).catch(() => {});
await page.waitForTimeout(2500);

const nTiles = await page.locator('.tk-block').count();
const nCards = await page.locator('.tk-card').count();
const nDelta = await page.locator('.tk-delta').count();
const nFill = await page.locator('.tk-fill__bar').count();
const nPill = await page.locator('.tk-pill').count();
const nTable = await page.locator('table.tk-table').count();
const nGear = await page.locator('.blockr-gear-btn').count();
console.log(`tiles=${nTiles} cards=${nCards} delta=${nDelta} fill=${nFill} pill=${nPill} matrix=${nTable} gears=${nGear}`);

await page.screenshot({ path: `${OUT}/01-light.png`, fullPage: true });

// Dark mode: set the theme attribute the CSS keys on.
await page.evaluate(() => document.documentElement.setAttribute('data-theme', 'dark'));
await page.waitForTimeout(400);
await page.screenshot({ path: `${OUT}/02-dark.png`, fullPage: true });
await page.evaluate(() => document.documentElement.setAttribute('data-theme', 'light'));

// Open a gear popover (first tile) to verify the config engine renders.
const gear = page.locator('.blockr-gear-btn').first();
if (await gear.count()) {
  await gear.click();
  await page.waitForTimeout(500);
  const nRoles = await page.locator('.dd-popover .dd-section').count();
  console.log(`popover sections=${nRoles}`);
  await page.screenshot({ path: `${OUT}/03-gear.png`, fullPage: true });
  await page.keyboard.press('Escape');
  await page.mouse.click(5, 5);
}

// Drill: click a region card in the drill tile, confirm downstream filters.
const drillCard = page.locator('.tk-block.tk-clickable .tk-card[data-group]').first();
let drilled = 'no-drill-card';
if (await drillCard.count()) {
  const grp = await drillCard.getAttribute('data-group');
  await drillCard.click();
  await page.waitForTimeout(1500);
  // downstream table rows (the new_table_block preview)
  const bodyRows = await page.locator('.blockr-data-row, .tk-data-row').count();
  drilled = `clicked group=${grp}; downstream-ish rows=${bodyRows}`;
  await page.screenshot({ path: `${OUT}/04-drill.png`, fullPage: true });
}
console.log(`drill: ${drilled}`);

console.log('--- console (last 20) ---');
console.log(logs.slice(-20).join('\n'));

await ctx.close();
