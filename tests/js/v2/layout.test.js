/* B9: facet panels are laid out before any of them fits its labels. v1
 * reads the first panel's width before the others exist, so its first
 * panel fits its x labels to the whole row (flat) while the others turn
 * theirs; v2 fits every panel to its own track. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const h = require('../harness');
const { F, BASE } = require('../cases');

/** The x labels' turn and each canvas height, per panel. */
const layout = (engine, cfg) => {
  const r = h.draw({ ...BASE, ...cfg }, F.data, F.columns, { engine, width: 640 });
  return {
    rotate: r.options.map((s) => {
      const o = s.calls[0].option;
      return (Array.isArray(o.xAxis) ? o.xAxis[0] : o.xAxis).axisLabel.rotate;
    }),
    heights: r.dom.slots.map((s) => s.height)
  };
};

const CASES = {
  bar: { chart_type: 'bar', group: 'AVISIT', facet: 'SEX', orientation: 'vertical', drill: 'auto' },
  line: { chart_type: 'line', x: 'AVISIT', y: 'AVAL', series: 'USUBJID', facet: 'SEX',
          drill: 'auto' }
};

for (const [name, cfg] of Object.entries(CASES)) {
  test(`B9: every ${name} panel fits its labels to its own track`, () => {
    const v2 = layout('v2', cfg);
    assert.deepStrictEqual(v2.rotate, [90, 90]);
    assert.strictEqual(v2.heights[0], v2.heights[1]);
    const v1 = layout('v1', cfg);
    assert.deepStrictEqual(v1.rotate, [0, 90]);
  });
}
