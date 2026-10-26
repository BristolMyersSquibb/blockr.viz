/* `format` as a whole number of decimals, and `pct_of = "total"`: the two
 * values the chart shares with the table block. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure, measure } = require('./chart/load');

const C = loadPure({ roles: true });

test('a whole number of decimals prints that many, from a number or its text', () => {
  assert.strictEqual(C.valueFormat({ format: 2 })(4.3587), '4.36');
  assert.strictEqual(C.valueFormat({ format: '0' })(1234.5), (1235).toLocaleString());
  assert.strictEqual(C.valueFormat({ format: '1', unit: 'CHF' })(2), '2.0\u00a0CHF');
  assert.strictEqual(C.valueFormat({ format: 2 })(1.5), '1.50');
  // The named formats are unchanged, and junk is no number of decimals.
  assert.strictEqual(C.valueFormat({ format: 'number' }), null);
  assert.strictEqual(C.valueFormat({ format: 'compact' })(1500), '1.5K');
  assert.strictEqual(C.fixedDigits('7'), null);
  assert.strictEqual(C.fixedDigits('1.5'), null);
  assert.strictEqual(C.fixedDigits(''), null);
});

test('"number" prints up to two decimals, as the table block', () => {
  // The cases of dt_format_num() in the table's tests.
  assert.strictEqual(C.ddNum(4.3587), '4.36');
  assert.strictEqual(C.ddNum(1.5), '1.5');
  assert.strictEqual(C.ddNum(4.3), '4.3');
  assert.strictEqual(C.ddNum(2.004), '2');
  assert.strictEqual(C.ddNum(0.0034), '0.0034');
  assert.strictEqual(C.ddNum(-0.003456), '-0.0035');
  assert.strictEqual(C.ddNum(0.005), '0.01');
  assert.strictEqual(C.ddNum(1234.567), (1234.57).toLocaleString());
  assert.strictEqual(C.ddNum(12), '12');
  // Raw observations: three significant digits, then the same rule.
  assert.strictEqual(C.ddNum3(123.456), '123');
  assert.strictEqual(C.ddNum3(0.0123456), '0.01');
  assert.strictEqual(C.ddNum3(0.00123456), '0.0012');
  // With a unit: the same number, then the unit.
  assert.strictEqual(C.valueFormat({ unit: '%' })(4.3587), '4.36\u00a0%');
  assert.strictEqual(C.valueFormat({ format: 'compact' })(0.0034), '0.0034');
});

test('bar value labels and ticks follow the fixed decimals', () => {
  const rows = [{ G: 'a', V: 4.3587 }, { G: 'b', V: 1.2 }];
  const cols = [{ name: 'G', type: 'categorical' }, { name: 'V', type: 'numeric' }];
  const c = { chart_type: 'bar', group: 'G', value: 'V', func: 'identity', value_labels: 'on',
              format: '2', drill: 'auto', sort_by: 'alpha', sort_dir: 'asc' };
  const m = C.model.aggregated({ rows, ix: new C.RowIndex(), columns: cols, cfg: c });
  const opt = C.option.aggregated(m, { cfg: c, columns: cols, ink: C.INK_DEFAULT, theme: null,
                                       measure, widths: [600] }).panels[0].option;
  const s = opt.series[0];
  assert.strictEqual(s.label.formatter({ value: 4.3587 }), '4.36');
  assert.strictEqual(s.label.formatter({ value: 1.2 }), '1.20');
  const valAxis = opt.xAxis.type === 'value' ? opt.xAxis : opt.yAxis;
  assert.strictEqual(valAxis.axisLabel.formatter(3), '3.00');
});

test('pct_of "total": one denominator for every panel', () => {
  // 10 subjects, 4 in facet X, 6 in Y; 2 in X and 3 in Y had the event.
  const rows = [];
  for (let i = 0; i < 10; i++) {
    rows.push({ F: i < 4 ? 'X' : 'Y', ID: 's' + i, E: (i < 2 || (i >= 4 && i < 7)) ? 'yes' : '' });
  }
  const agg = (pct_of) => C.roles.DAgg.aggregate(rows, {
    group: 'E', facet: 'F', value: 'ID', func: 'pct_distinct', na_group: 'drop', pct_of });
  const byFacet = (cells) => Object.fromEntries(cells.map((a) => [a.facet, a.value]));
  assert.deepStrictEqual(byFacet(agg('facet')), { X: 0.5, Y: 0.5 });
  assert.deepStrictEqual(byFacet(agg('total')), { X: 0.2, Y: 0.3 });
});

test('the gear offers decimals and the whole chart', () => {
  const R = C.roles.ROLES;
  assert.strictEqual(R.format.options.map((o) => o.value).join(' '),
                     'number 0 1 2 compact percent');
  assert.ok(R.pct_of.options.some((o) => o.value === 'total'));
});
