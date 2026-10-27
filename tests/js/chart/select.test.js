/* What a click, a brush, a legend chip and the clear button send to R as
 * `<id>_select` ({selection, nonce}, design.md section 2), per chart type,
 * and what the footer says (features 70-78). */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const D = require('./drive');

const BAR = { type: 'bar', y: 'AETERM', orientation: 'horizontal', drill: 'auto' };
const cat = (filters) => ({ type: 'categorical', filters });
const run = (config, steps, opts) => {
  const c = D.open(config, opts);
  try {
    steps(c);
    return { sent: D.plain(c.selections()), footer: c.footer(), c };
  } finally {
    c.close();
  }
};

test('a bar click selects its category; again, or a click beside the bars, clears', () => {
  const r = run(BAR, (c) => {
    c.clickWhere(0, D.at('Nausea'));
    assert.strictEqual(c.footer(), 'Filtered: Reported Term = Nausea');
    assert.strictEqual(c.block.clearBtn.hidden, false);
    c.clickWhere(0, D.at('Nausea'));
    c.clickWhere(0, D.at('Rash'));
    c.zr('click', {});
  });
  assert.deepStrictEqual(r.sent, [cat({ AETERM: ['Nausea'] }), null, cat({ AETERM: ['Rash'] }), null]);
  assert.strictEqual(r.footer, null);
});

test('the clear button on the footer clears the selection (UX 15)', () => {
  const r = run(BAR, (c) => {
    c.clickWhere(0, D.at('Rash'));
    c.block.clearBtn.click();
  });
  assert.deepStrictEqual(r.sent, [cat({ AETERM: ['Rash'] }), null]);
  assert.strictEqual(r.footer, null);
});

test('every message carries a new nonce', () => {
  const c = D.open(BAR);
  c.clickWhere(0, D.at('Nausea'));
  c.clickWhere(0, D.at('Nausea'));
  const nonces = c.inputs().map((x) => x.value.nonce);
  assert.deepStrictEqual(nonces, [1, 2]);
  assert.strictEqual(c.inputs()[0].opts.priority, 'event');
  c.close();
});

test('a colour segment adds its level; a missing level is null', () => {
  const r = run({ ...BAR, color: 'AESEV' }, (c) => {
    c.clickWhere(0, D.at('Rash', 'MODERATE'));
    c.clickWhere(0, D.at('Rash', ''));
  });
  assert.deepStrictEqual(r.sent, [cat({ AETERM: ['Rash'], AESEV: ['MODERATE'] }),
                                  cat({ AETERM: ['Rash'], AESEV: [null] })]);
});

test('a click in a facet panel carries the panel key', () => {
  const r = run({ ...BAR, facet: 'ARM' }, (c) => c.clickWhere(1, D.at('Nausea')));
  assert.deepStrictEqual(r.sent, [cat({ AETERM: ['Nausea'], ARM: ['High Dose'] })]);
});

test('a drill column sends its values in the rows under the mark; drill off sends nothing', () => {
  const r = run({ ...BAR, drill: 'USUBJID' }, (c) => c.clickWhere(0, D.at('Nausea')));
  assert.deepStrictEqual(Object.keys(r.sent[0].filters), ['USUBJID']);
  assert.strictEqual(r.sent[0].filters.USUBJID.length, 9);
  assert.strictEqual(r.footer, 'Filtered: USUBJID, 9 values');
  assert.deepStrictEqual(run({ ...BAR, drill: null }, (c) => c.clickWhere(0, D.at('Nausea'))).sent, []);
});

test('with a drill target a click is an event: sent every time, nothing latched', () => {
  const r = run({ ...BAR, drill_target: 'flt_ae' }, (c) => {
    c.clickWhere(0, D.at('Nausea'));
    c.clickWhere(0, D.at('Nausea'));
    assert.strictEqual(c.block._filter, null);
    assert.match(c.block.statusEl.textContent, /^Drilled down to Reported Term = Nausea/);
    c.advance(4000);
    assert.strictEqual(c.block.statusEl.textContent, '');
  });
  assert.deepStrictEqual(r.sent, [cat({ AETERM: ['Nausea'] }), cat({ AETERM: ['Nausea'] })]);
});

test('a legend chip selects its level', () => {
  const r = run({ ...BAR, color: 'AESEV' }, (c) => {
    c.block.legendEl.querySelectorAll('.dd-legend-chip')[1].click();
    assert.ok(c.block.legendEl.querySelector('.dd-legend-chip-on'));
  });
  assert.deepStrictEqual(r.sent, [cat({ AESEV: ['MILD'] })]);
});

test('a pie slice and a gantt lane select their level', () => {
  assert.deepStrictEqual(run({ type: 'pie', x: 'AESEV', drill: 'auto' }, (c) => c.click(0, 0, 0)).sent,
                         [cat({ AESEV: ['SEVERE'] })]);
  const g = { type: 'gantt', x: 'ASTDY', xend: 'AENDY', y: 'AETERM', drill: 'auto' };
  assert.deepStrictEqual(run(g, (c) => c.click(0, 0, 0)).sent, [cat({ AETERM: ['Rash'] })]);
});

test('a scatter point is a zero-width range; a brush its extent', () => {
  const r = run({ type: 'scatter', x: 'ADY', y: 'AVAL', drill: 'auto' }, (c) => {
    c.click(0, 0, 3);
    c.trigger('brushSelected', { batch: [{ selected: [{ seriesIndex: 0, dataIndex: [0, 1, 2] }] }] });
  });
  assert.deepStrictEqual(r.sent, [
    { type: 'range', x: 'ADY', y: 'AVAL', x_range: [57, 57], y_range: [34.48, 34.48] },
    { type: 'range', x: 'ADY', y: 'AVAL', x_range: [1, 29], y_range: [27.37, 31.11] }
  ]);
  assert.strictEqual(r.footer, 'Filtered: Analysis Relative Day 1 to 29, Analysis Value 27.4 to 31.1');
});

test('a line click waits for a second click; a double-click cancels it', () => {
  const L = { type: 'line', x: 'ADY', y: 'AVAL', series: 'USUBJID', drill: 'auto' };
  const r = run(L, (c) => {
    c.click(0, 1, 0);
    assert.strictEqual(c.selections().length, 0);
    c.advance(300);
    c.click(0, 1, 0);
    c.zr('dblclick');
    c.advance(300);
  });
  assert.strictEqual(r.sent.length, 1);
  assert.deepStrictEqual(Object.keys(r.sent[0].filters), ['USUBJID']);
});

test('a selection R sends back lights its marks and names itself in the footer', () => {
  const c = D.open({ ...BAR, color: 'AESEV', selection: cat({ AETERM: ['Nausea'] }) });
  assert.strictEqual(c.footer(), 'Filtered: Reported Term = Nausea');
  const patch = c.chart().log.filter((x) => x.op === 'setOption' && !x.notMerge).pop();
  assert.ok(patch && patch.option.series, 'a highlight patch');
  // The same selection again keeps what the reader sees; a new one replaces it.
  c.send({ ...c.config });
  assert.strictEqual(c.footer(), 'Filtered: Reported Term = Nausea');
  c.send({ ...c.config, selection: null });
  assert.strictEqual(c.footer(), null);
  assert.deepStrictEqual(c.selections(), [], 'restoring sends nothing');
  c.close();
});

test('a restored range survives the brush cursor being armed', () => {
  const sel = { type: 'range', x: 'ADY', y: 'AVAL', x_range: [1, 30], y_range: [25, 32] };
  const c = D.open({ type: 'scatter', x: 'ADY', y: 'AVAL', drill: 'auto', selection: sel });
  c.trigger('brushSelected', { batch: [{ areas: [], selected: [] }] });
  assert.match(c.footer(), /^Filtered: Analysis Relative Day 1 to 30/);
  assert.deepStrictEqual(c.selections(), []);
  // The reader's own clear: a brush event with no areas.
  c.trigger('brush', { areas: [] });
  assert.strictEqual(c.footer(), null);
  assert.deepStrictEqual(D.plain(c.selections()), [null]);
  c.close();
});

test('a drill column lights every bar holding a selected row, not only the clicked one (UX 14)', () => {
  // Subject a has Rash and Fever, b Rash only, c Cough only.
  const data = { T: ['Rash', 'Fever', 'Rash', 'Cough'], ID: ['a', 'a', 'b', 'c'] };
  const columns = [{ name: 'T', type: 'categorical', n_unique: 3, label: '' },
                   { name: 'ID', type: 'categorical', n_unique: 3, label: '' }];
  const c = D.open({ type: 'bar', y: 'T', orientation: 'horizontal', drill: 'ID' },
                   { data, columns });
  c.clickWhere(0, D.at('Fever'));
  assert.deepStrictEqual(D.plain(c.selections()[0].filters), { ID: ['a'] });
  const patch = c.chart().log.filter((x) => x.op === 'setOption' && !x.notMerge).pop();
  const cats = c.chart().getOption().yAxis[0].data;
  const lit = patch.option.series[0].data.map((d, i) => [cats[i], d.itemStyle.opacity])
    .filter(([, o]) => o === 1).map(([n]) => n).sort();
  assert.deepStrictEqual(lit, ['Fever', 'Rash']);
  c.close();
});
