/* File snapshots: one pretty JSON file per case in __snapshots__/.
 *
 * A missing snapshot fails. UPDATE_SNAPSHOTS=1 writes (or rewrites) them.
 *
 * Under CHART_ENGINE=v2 the snapshots are v1's, and v2 must reproduce them.
 * A snapshot v2 does not reproduce has to be listed in v2-differences.json
 * with an ID from the decisions (D1-D7, B1-B10); what v2 draws instead is
 * pinned in __snapshots__/v2/. An unlisted difference fails, and so does a
 * listed one that no longer differs. UPDATE_SNAPSHOTS=1 under v2 writes
 * only __snapshots__/v2/, never v1's files.
 */
'use strict';

const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');
const { isDeepStrictEqual } = require('node:util');

const DIR = path.join(__dirname, '__snapshots__');
const V2_DIR = path.join(DIR, 'v2');
const DIFF_FILE = path.join(__dirname, 'v2-differences.json');
const UPDATE = process.env.UPDATE_SNAPSHOTS === '1';
const ENGINE = process.env.CHART_ENGINE || 'v1';

const file = (name) => path.join(DIR, name + '.json');
const v2file = (name) => path.join(V2_DIR, name + '.json');
const read = (f) => JSON.parse(fs.readFileSync(f, 'utf8'));
const write = (f, value) => {
  fs.mkdirSync(path.dirname(f), { recursive: true });
  fs.writeFileSync(f, JSON.stringify(value, null, 1) + '\n');
};

/** The listed v2 differences: snapshot name -> { ids, why }. */
const differences = () => (fs.existsSync(DIFF_FILE) ? read(DIFF_FILE) : {});

/** Compare `value` (plain JSON) with the stored snapshot `name`. */
function matchSnapshot(name, value) {
  if (ENGINE === 'v2') return matchV2(name, value);
  const f = file(name);
  if (UPDATE) {
    write(f, value);
    return;
  }
  if (!fs.existsSync(f)) {
    assert.fail(`no snapshot for "${name}"; run UPDATE_SNAPSHOTS=1 npm test to create it`);
  }
  assert.deepStrictEqual(value, read(f));
}

/** v2 against v1's snapshot, or against its own when the difference is listed. */
function matchV2(name, value) {
  const f = file(name);
  if (!fs.existsSync(f)) assert.fail(`no v1 snapshot for "${name}"`);
  const v1 = read(f);
  const listed = differences()[name];
  if (isDeepStrictEqual(value, v1)) {
    if (listed) {
      assert.fail(`v2 now reproduces "${name}"; remove it from v2-differences.json ` +
                  'and delete its file in __snapshots__/v2/');
    }
    return;
  }
  if (!listed) {
    assert.deepStrictEqual(value, v1,
      `v2 differs from v1 on "${name}" and v2-differences.json does not list it`);
  }
  if (UPDATE) {
    write(v2file(name), value);
    return;
  }
  if (!fs.existsSync(v2file(name))) {
    assert.fail(`no v2 snapshot for listed difference "${name}"; run ` +
                'UPDATE_SNAPSHOTS=1 npm run test:v2 to create it');
  }
  assert.deepStrictEqual(value, read(v2file(name)));
}

module.exports = { matchSnapshot, DIR, V2_DIR, DIFF_FILE };
