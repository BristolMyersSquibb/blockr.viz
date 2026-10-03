/* v2 chart-keys.js: filters from mark keys, the messages, the footer words
 * (D4) and which marks a restored filter lights (D5). */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure, plain } = require('./load');

const C = loadPure();
const K = C.keys;

const columns = [
  { name: 'AETERM', type: 'categorical', label: 'Reported Term' },
  { name: 'AESEV', type: 'categorical', label: 'Severity' },
  { name: 'USUBJID', type: 'categorical', label: 'Patient' },
  { name: 'ARM', type: 'categorical' }
];

test('a filter from keys has one string value per column', () => {
  assert.deepStrictEqual(plain(K.fromKeys({ AETERM: 'Rash', AVISITN: 4 })),
                         { AETERM: ['Rash'], AVISITN: ['4'] });
});

test('a filter from rows takes the distinct non-missing values', () => {
  const rows = [{ ID: 'S01' }, { ID: 'S02' }, { ID: 'S01' }, { ID: null }, {}];
  assert.deepStrictEqual(plain(K.fromRows('ID', rows)), { ID: ['S01', 'S02'] });
  assert.strictEqual(K.fromRows('ID', [{ ID: null }]), null);
});

test('a restored filter needs a column and values', () => {
  assert.deepStrictEqual(plain(K.restored({ filter_column: 'AETERM', filter_values: ['Rash'] })),
                         { AETERM: ['Rash'] });
  assert.strictEqual(K.restored({ filter_values: ['Rash'] }), null);
  assert.strictEqual(K.restored({ filter_column: 'AETERM', filter_values: [] }), null);
  assert.strictEqual(K.restored({}), null);
});

test('the filter message is a named list of columns (D2, D3)', () => {
  assert.deepStrictEqual(plain(K.filterMessage({ AETERM: ['Rash'], AESEV: ['MODERATE'] }, 41)), {
    action: 'filter', filter_type: 'categorical',
    filters: { AETERM: ['Rash'], AESEV: ['MODERATE'] }, nonce: 41
  });
});

test('a clear names the type of what it clears (B1)', () => {
  const m = plain(K.clearMessage('categorical'));
  assert.strictEqual(m.filter_type, 'categorical');
  assert.strictEqual(m.column, null);
  assert.strictEqual(m.x_range, null);
  assert.strictEqual(plain(K.clearMessage('range')).filter_type, 'range');
});

test('the footer names the filter in column labels (D4)', () => {
  assert.strictEqual(K.describe({ AETERM: ['Nausea'] }, columns), 'Reported Term = Nausea');
  assert.strictEqual(K.describe({ AETERM: ['Nausea', 'Headache'] }, columns),
                     'Reported Term = Nausea, Headache');
  assert.strictEqual(K.describe({ AETERM: ['Rash'], AESEV: ['MODERATE'] }, columns),
                     'Reported Term = Rash, Severity = MODERATE');
  // No label: the column's name.
  assert.strictEqual(K.describe({ ARM: ['Placebo'] }, columns), 'ARM = Placebo');
  // Many values: the count.
  const ids = Array.from({ length: 214 }, (_, i) => 'S' + i);
  assert.strictEqual(K.describe({ USUBJID: ids }, columns), 'Patient, 214 values');
  assert.strictEqual(K.describe({ USUBJID: ids.slice(0, K.MAX_LISTED) }, columns),
                     'Patient = S0, S1, S2');
});

test('a restored filter lights the marks whose keys match (D5)', () => {
  const f = { AETERM: ['Nausea', 'Headache'] };
  assert.strictEqual(K.markLit({ AETERM: 'Nausea' }, [], f), true);
  assert.strictEqual(K.markLit({ AETERM: 'Headache' }, [], f), true);
  assert.strictEqual(K.markLit({ AETERM: 'Rash' }, [], f), false);
  // Two columns: both have to match.
  const two = { AETERM: ['Rash'], AESEV: ['MODERATE'] };
  assert.strictEqual(K.markLit({ AETERM: 'Rash', AESEV: 'MODERATE' }, [], two), true);
  assert.strictEqual(K.markLit({ AETERM: 'Rash', AESEV: 'MILD' }, [], two), false);
  // Numbers match their string form.
  assert.strictEqual(K.markLit({ AVISITN: 4 }, [], { AVISITN: ['4'] }), true);
});

test('a filter on a column the chart does not draw lights marks holding its rows (D5)', () => {
  const f = { USUBJID: ['S01'] };
  const rash = [{ AETERM: 'Rash', USUBJID: 'S01' }, { AETERM: 'Rash', USUBJID: 'S02' }];
  const nausea = [{ AETERM: 'Nausea', USUBJID: 'S02' }];
  assert.strictEqual(K.markLit({ AETERM: 'Rash' }, rash, f), true);
  assert.strictEqual(K.markLit({ AETERM: 'Nausea' }, nausea, f), false);
  // A key and a row column together: the rows decide, and must pass both.
  const g = { AETERM: ['Rash'], USUBJID: ['S02'] };
  assert.strictEqual(K.markLit({ AETERM: 'Rash' }, rash, g), true);
  assert.strictEqual(K.markLit({ AETERM: 'Nausea' }, nausea, g), false);
});

test('the v1 view of a selection: column and value(s)', () => {
  assert.deepStrictEqual(plain(K.selectionView(null)), { column: null, selected: null });
  assert.deepStrictEqual(plain(K.selectionView({ AETERM: ['Rash'] })),
                         { column: 'AETERM', selected: 'Rash' });
  assert.deepStrictEqual(plain(K.selectionView({ AETERM: ['Rash', 'Nausea'] })),
                         { column: 'AETERM', selected: ['Rash', 'Nausea'] });
  assert.deepStrictEqual(plain(K.selectionView({ USUBJID: ['S02'], ARM: ['Low Dose'] })), {
    column: ['USUBJID', 'ARM'], selected: { USUBJID: ['S02'], ARM: ['Low Dose'] }
  });
});
