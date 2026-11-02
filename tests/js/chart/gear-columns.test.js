/* gear.js: the columns the chart's gear offers per role. As in 0.2, a facet
 * offers columns with at most 10 distinct values and a category columns that
 * are not numbers or have at most 50, so a prepare script's helper columns
 * and the measures stay out of them. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure } = require('./load');

const C = loadPure();

const col = (name, cls, n) => ({ name, class: cls, label: '', n_unique: n });
const COLUMNS = [
  col('PARAM', 'string', 1), col('SEX', 'string', 2), col('TRT', 'string', 3),
  col('AVAL', 'number', 118), col('VISITNUM', 'number', 6), col('USUBJID', 'string', 120),
  col('.lab_waterfall_value', 'number', 118), col('.lab_waterfall_subject', 'string', 120)
];
const names = (cols) => cols.map((c) => c.name);
const host = C.gear.host({ columns: COLUMNS, derived: {} });

test('a facet offers columns with at most 10 distinct values', () => {
  const got = names(host.columns('facet', { type: 'bar' }, COLUMNS));
  assert.deepStrictEqual(got, ['PARAM', 'SEX', 'TRT', 'VISITNUM']);
  assert.ok(!got.includes('AVAL'));
  assert.ok(!got.some((n) => n.startsWith('.lab_waterfall')));
});

test('a category offers text columns and numbers with at most 50 values', () => {
  const got = names(host.columns('category', { type: 'bar' }, COLUMNS));
  assert.deepStrictEqual(got, ['PARAM', 'SEX', 'TRT', 'VISITNUM', 'USUBJID',
                               '.lab_waterfall_subject']);
});

test('colour and series still offer every column', () => {
  assert.strictEqual(host.columns('color', { type: 'bar' }, COLUMNS).length, COLUMNS.length);
  assert.strictEqual(host.columns('series', { type: 'line' }, COLUMNS).length, COLUMNS.length);
});
