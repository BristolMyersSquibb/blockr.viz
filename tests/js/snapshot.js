/* File snapshots: one pretty JSON file per case in __snapshots__/.
 *
 * A missing snapshot fails. UPDATE_SNAPSHOTS=1 writes (or rewrites) them.
 */
'use strict';

const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');

const DIR = path.join(__dirname, '__snapshots__');
const UPDATE = process.env.UPDATE_SNAPSHOTS === '1';

const file = (name) => path.join(DIR, name + '.json');
const read = (f) => JSON.parse(fs.readFileSync(f, 'utf8'));
const write = (f, value) => {
  fs.mkdirSync(path.dirname(f), { recursive: true });
  fs.writeFileSync(f, JSON.stringify(value, null, 1) + '\n');
};

/** Compare `value` (plain JSON) with the stored snapshot `name`. */
function matchSnapshot(name, value) {
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

module.exports = { matchSnapshot, DIR };
