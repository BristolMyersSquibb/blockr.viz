/* v2 chart-index.js: the row index, payload decoding and label counts. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure, plain } = require('./load');

const C = loadPure();

const rows = [
  { T: 'Rash', S: 'MILD', ID: 'S01' },
  { T: 'Nausea', S: null, ID: 'S01' },
  { T: 'Rash', S: 'SEVERE', ID: 'S02' },
  { T: null, S: 'MILD', ID: 'S03' }
];

test('buckets keep row order and first-seen key order', () => {
  const ix = new C.RowIndex();
  const b = ix.buckets(rows, 'T', 'nz');
  assert.deepStrictEqual(plain([...b.keys()]), ['Rash', 'Nausea', '']);
  assert.deepStrictEqual(plain(b.get('Rash').map((r) => r.ID)), ['S01', 'S02']);
  // The same answer object for the same rows and column.
  assert.strictEqual(ix.buckets(rows, 'T', 'nz'), b);
});

test("'nz' joins null to the empty level, 'raw' keeps it as \"null\"", () => {
  const ix = new C.RowIndex();
  assert.deepStrictEqual(plain(ix.levels(rows, 'S', 'nz')), ['MILD', '', 'SEVERE']);
  assert.deepStrictEqual(plain(ix.levels(rows, 'S', 'raw')), ['MILD', 'null', 'SEVERE']);
});

test('get and get2 answer from the buckets; a miss is the shared empty array', () => {
  const ix = new C.RowIndex();
  assert.strictEqual(ix.get(rows, 'T', 'raw', 'Rash').length, 2);
  assert.strictEqual(ix.get(rows, 'T', 'raw', 'nope'), C.NO_ROWS);
  assert.deepStrictEqual(plain(ix.get2(rows, 'T', 'nz', 'S', 'nz', 'Rash', 'SEVERE').map((r) => r.ID)),
                         ['S02']);
  assert.strictEqual(ix.get2(rows, 'T', 'nz', 'S', 'nz', 'Rash', 'nope'), C.NO_ROWS);
});

test('rowsFromPayload reads columns, dictionary columns, a JSON string and rows', () => {
  const cols = { A: [1, 2], B: ['x', null] };
  const want = [{ A: 1, B: 'x' }, { A: 2, B: null }];
  assert.deepStrictEqual(plain(C.rowsFromPayload(cols)), want);
  assert.deepStrictEqual(plain(C.rowsFromPayload(JSON.stringify(cols))), want);
  assert.deepStrictEqual(plain(C.rowsFromPayload({
    A: [1, 2], B: { __enc__: 'dict', levels: ['x', null], codes: [0, 1] }
  })), want);
  const asRows = [{ A: 1 }];
  assert.strictEqual(C.rowsFromPayload(asRows), asRows);
  assert.deepStrictEqual(plain(C.rowsFromPayload(null)), []);
  assert.deepStrictEqual(plain(C.rowsFromPayload({})), []);
});

test('labelCounts counts rows, or distinct count_col, within a facet', () => {
  const r = [
    { T: 'Rash', ID: 'S01', F: 'a' }, { T: 'Rash', ID: 'S01', F: 'a' },
    { T: 'Rash', ID: 'S02', F: 'b' }, { T: null, ID: null, F: 'b' }
  ];
  assert.deepStrictEqual(plain(C.labelCounts(r, {}, 'T')), { Rash: 3, '': 1 });
  assert.deepStrictEqual(plain(C.labelCounts(r, { count_col: 'ID' }, 'T')), { Rash: 2 });
  assert.deepStrictEqual(plain(C.labelCounts(r, {}, 'T', 'F', 'b')), { Rash: 1, '': 1 });
  // "None (as is)": the count column's first value per group, as it is.
  assert.deepStrictEqual(plain(C.labelCounts(r, { count_col: 'ID', func: 'identity' }, 'T')),
                         { Rash: 'S01' });
});

test('withCount appends "(n)" and rounds long floats', () => {
  const m = new Map([['a', 3], ['b', 1.23456]]);
  assert.strictEqual(C.withCount('a', m), 'a (3)');
  assert.strictEqual(C.withCount('b', m), 'b (1.23)');
  assert.strictEqual(C.withCount('c', m), 'c');
  assert.strictEqual(C.withCount(null, null), '');
});
