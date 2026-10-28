/* Hover shows the tooltip and nothing else, on every chart type the cases
 * draw (plain, coloured, faceted): no axis-pointer band or crosshair, no
 * emphasis or blur of the other marks, a hand cursor where a click selects.
 * A click outlines the selected marks and fades the rest. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const h = require('./harness');
const { F, aggregated, individual, timeline } = require('./cases');
const D = require('./chart/drive');

/** The visible axis pointers of an option: the tooltip's and the axes'. */
const visiblePointers = (o) => {
  const axes = [].concat(o.xAxis || [], o.yAxis || [], o.singleAxis || []);
  return [].concat(o.tooltip || [], axes)
    .map((x) => x && x.axisPointer)
    .filter((p) => p && p.show !== false && p.type !== 'none');
};

test('hover draws nothing: no pointer band or line, no emphasis, on every case', () => {
  let panels = 0;
  for (const cs of [...aggregated, ...individual, ...timeline]) {
    const r = h.draw(cs.config, cs.data || F.data, cs.columns || F.columns,
                     { width: cs.width, derived: cs.derived, keepOpen: true });
    try {
      for (const slot of r.block._slots) {
        if (!slot || !slot.chart) continue;
        const o = slot.chart.getOption();
        panels++;
        assert.strictEqual(visiblePointers(o).length, 0, cs.name + ': a visible axis pointer');
        for (const s of o.series || []) {
          assert.strictEqual(s.emphasis && s.emphasis.disabled, true, cs.name + ': ' + s.type + ' emphasis');
          assert.strictEqual(s.emphasis.scale, false, cs.name + ': ' + s.type + ' symbol scale');
          assert.strictEqual(s.blur, undefined, cs.name + ': blur');
          if (!s.silent) {
            assert.strictEqual(s.cursor, cs.config.drill ? 'pointer' : 'default', cs.name + ': cursor');
          }
        }
      }
    } finally {
      r.env.close();
    }
  }
  assert.ok(panels > 40, 'the cases drew their panels');
});

test('a click outlines what it selects and fades the rest to a quarter', () => {
  const style = (c, si, di) => {
    const d = c.chart().getOption().series[si].data[di];
    return d && d.itemStyle;
  };
  // A stacked bar: the clicked segment outlined, the other segments faded.
  const c = D.open({ type: 'bar', category: 'AETERM', orientation: 'horizontal', color: 'AESEV',
                     drill: 'auto' });
  c.click(0, 0, 0);
  const ink = c.block._ink.text;
  assert.deepStrictEqual(D.plain(style(c, 0, 0)), { ...D.plain(style(c, 0, 0)), opacity: 1,
                                                     borderColor: ink, borderWidth: 1.5 });
  assert.strictEqual(style(c, 0, 1).opacity, 0.25);
  assert.strictEqual(style(c, 1, 0).opacity, 0.25);
  // The clear brings every mark back, its border as drawn.
  c.click(0, 0, 0);
  assert.strictEqual(style(c, 0, 0).opacity, 1);
  assert.strictEqual(style(c, 0, 0).borderWidth, 0);
  c.close();
  // A box plot and a gantt lane.
  for (const cfg of [{ type: 'boxplot', category: 'ARM', value: 'AVAL', drill: 'auto' },
                     { type: 'gantt', x: 'ASTDY', y: 'AETERM', xend: 'AENDY', drill: 'auto' }]) {
    const b = D.open(cfg);
    b.click(0, 0, 0);
    const data = b.chart().getOption().series[0].data.map((d) => d && d.itemStyle);
    assert.strictEqual(data[0].borderWidth, 1.5, cfg.type + ' outlined');
    assert.ok(data.slice(1).some((s) => s && s.opacity === 0.25), cfg.type + ' others faded');
    b.close();
  }
});

test('the status line shows only while something is selected', () => {
  const c = D.open({ type: 'scatter', x: 'ADY', y: 'AVAL', drill: 'auto' });
  assert.strictEqual(c.footer(), null);
  // A rectangle that caught nothing: no line.
  c.block.selection.brushDrawn = true;
  c.block.status();
  assert.strictEqual(c.footer(), null);
  c.click(0, 0, 0);
  assert.match(c.footer(), /^Filtered: /);
  c.close();
});
