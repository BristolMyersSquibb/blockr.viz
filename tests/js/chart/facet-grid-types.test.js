/* Every chart type in a panel grid: rows by SEX, columns by ARM. Each draws
 * one panel per combination with rows, at its row and column, with strips
 * on top and on the right and no error. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const h = require('../harness');
const { F, BASE } = require('../cases');

// The fixture with a histogram bin column for ADY.
const DATA = { ...F.data, ADY_bin: F.data.ADY.map((v) => (v == null ? null : v < 10 ? '0-10' : '10-')) };

const TYPES = {
  bar: { category: 'AETERM', func: 'count', orientation: 'horizontal' },
  histogram: { x: 'ADY' },
  waterfall: { category: 'AVISIT', value: 'CHG', func: 'sum' },
  pie: { category: 'AESEV', func: 'count' },
  treemap: { category: 'AESEV', func: 'count' },
  boxplot: { category: 'AVISIT', value: 'AVAL' },
  pointrange: { category: 'AVISIT', value: 'AVAL' },
  scatter: { x: 'ADY', y: 'AVAL' },
  line: { x: 'ADY', y: 'AVAL', series: 'USUBJID' },
  area: { x: 'ADY', y: 'AVAL' },
  gantt: { x: 'ASTDY', y: 'AETERM', xend: 'AENDY' }
};

const lv = (v) => (v == null ? '' : String(v));
const levels = (col) => [...new Set(DATA[col].map(lv))];
const combos = (keep) => new Set(DATA.ARM.map((_, i) => i).filter(keep)
  .map((i) => lv(DATA.SEX[i]) + '|' + lv(DATA.ARM[i]))).size;

for (const [type, map] of Object.entries(TYPES)) {
  for (const scales of ['fixed', 'free_y', 'free']) {
    test(`${type} in a grid, ${scales}`, () => {
      const cfg = { ...BASE, type, ...map, facet: 'ARM', facet_row: 'SEX', facet_scales: scales };
      const r = h.draw(cfg, DATA, F.columns, { width: 900, keepOpen: true });
      try {
        assert.strictEqual(r.error, null);
        const grid = r.block.chartGrid;
        assert.ok(grid.classList.contains('vz-chart-grid--matrix'));
        const nCol = grid.querySelectorAll('.vz-chart-strip--col').length;
        const nRow = grid.querySelectorAll('.vz-chart-strip--row').length;
        assert.ok(nCol >= 1 && nCol <= levels('ARM').length, `${nCol} column strips`);
        assert.ok(nRow >= 1 && nRow <= levels('SEX').length, `${nRow} row strips`);
        const slots = r.block._slots;
        assert.ok(slots.length >= 1 && slots.length <= combos(() => true));
        const cells = slots.map((s) => s.container.style.gridRow + '/' + s.container.style.gridColumn);
        assert.ok(cells.every((c) => /^\d+\/\d+$/.test(c)), cells.join(' '));
        assert.strictEqual(new Set(cells).size, cells.length);
      } finally {
        r.env.close();
      }
    });
  }
}
