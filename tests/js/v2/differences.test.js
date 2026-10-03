/* The bookkeeping of v2 against v1: every listed difference names a
 * decision and has both snapshots, nothing in __snapshots__/v2/ is
 * unlisted, and every skipped test still exists. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');
const { DIR, V2_DIR, DIFF_FILE } = require('../snapshot');
const pending = require('../v2-pending');

// The behaviour changes and bug fixes v2 is written with (decisions.md in
// the chart-block-v2 spec).
const IDS = new Set(['D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7',
                     'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10']);

const diffs = JSON.parse(fs.readFileSync(DIFF_FILE, 'utf8'));

/** Every .json under `dir`, as snapshot names relative to it. */
const names = (dir, base = dir) => (fs.existsSync(dir) ? fs.readdirSync(dir) : [])
  .flatMap((f) => {
    const p = path.join(dir, f);
    if (fs.statSync(p).isDirectory()) return names(p, base);
    return f.endsWith('.json') ? [path.relative(base, p).replace(/\.json$/, '')] : [];
  });

test('every listed difference names decisions and says why', () => {
  for (const [name, d] of Object.entries(diffs)) {
    assert.ok(Array.isArray(d.ids) && d.ids.length, `${name}: no ids`);
    for (const id of d.ids) assert.ok(IDS.has(id), `${name}: ${id} is not a decision ID`);
    assert.ok(typeof d.why === 'string' && d.why.trim(), `${name}: no why`);
  }
});

test('every listed difference has a v1 snapshot and a v2 snapshot', () => {
  for (const name of Object.keys(diffs)) {
    assert.ok(fs.existsSync(path.join(DIR, name + '.json')), `${name}: no v1 snapshot`);
    assert.ok(fs.existsSync(path.join(V2_DIR, name + '.json')), `${name}: no v2 snapshot`);
  }
});

test('every v2 snapshot is a listed difference', () => {
  for (const name of names(V2_DIR)) {
    assert.ok(name in diffs, `__snapshots__/v2/${name}.json is not in v2-differences.json`);
  }
});

test('every skipped test names a test file that exists', () => {
  for (const keys of Object.values(pending)) {
    for (const k of keys) {
      const file = k.split(': ')[0];
      assert.ok(fs.existsSync(path.join(__dirname, '..', file)), `${k}: no ${file}`);
    }
  }
});
