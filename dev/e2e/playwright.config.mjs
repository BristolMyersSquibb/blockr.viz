// Standalone Playwright config for the ECharts canvas-drill test. Uses the
// container's system chromium (no bundled browser download) with --no-sandbox,
// matching how the Playwright MCP is wired here. NOT part of R CMD check; run
// manually against a live app (see README.md).
import { defineConfig } from '@playwright/test';

export default defineConfig({
  testDir: '.',
  testMatch: /.*\.spec\.mjs/,
  timeout: 60_000,
  use: {
    baseURL: process.env.VIZ_E2E_URL || 'http://127.0.0.1:3838',
    launchOptions: {
      executablePath: process.env.PLAYWRIGHT_CHROMIUM || '/usr/bin/chromium',
      args: ['--no-sandbox', '--disable-gpu', '--disable-dev-shm-usage'],
    },
  },
  reporter: [['list']],
});
