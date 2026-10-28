'use strict';

// The busy cue ships with the core, so a board with tables and no chart has
// it: R's start dims the table, and the drawn table ends it.

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadTable, payload, flatModel, tick } = require('./load');

test('a table-only page has the busy cue, and the drawn table ends it', async () => {
  const env = loadTable();
  const busy = env.win.Blockr.busy;
  assert.ok(busy, 'Blockr.busy comes with the core');
  const el = env.add('tbl');
  const line = el.appendChild(env.win.document.createElement('div'));
  line.className = 'blockr-busy-line';

  busy.start('tbl', 'drawing');
  assert.equal(el.classList.contains('blockr-busy'), false, 'nothing in the first 300 ms');
  await tick(env.win, 350);
  assert.equal(el.classList.contains('blockr-busy'), true);
  assert.match(line.textContent, /^drawing… \d+\.\d s$/);

  env.b.renderValue(el, payload({ data: flatModel(3) }));
  await tick(env.win, 50);
  assert.equal(el.classList.contains('blockr-busy'), false, 'the drawn table ends the cue');
});
