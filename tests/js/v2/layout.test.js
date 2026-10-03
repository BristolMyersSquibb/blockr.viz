/* Facet panels are laid out before any of them fits its labels, so every
 * panel fits its x labels to its own track, the first one included. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const h = require('../harness');
const { F, BASE } = require('../cases');

/** The x labels' turn and each canvas height, per panel. */
const layout = (cfg) => {
  const r = h.draw({ ...BASE, ...cfg }, F.data, F.columns, { width: 640 });
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
  test(`every ${name} panel fits its labels to its own track`, () => {
    const l = layout(cfg);
    assert.deepStrictEqual(l.rotate, [90, 90]);
    assert.strictEqual(l.heights[0], l.heights[1]);
  });
}
