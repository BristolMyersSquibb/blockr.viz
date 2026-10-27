'use strict';
/* Replays viz-api's recorded payloads (tests/js/fixtures/table/<scenario>.json,
 * written by make-fixtures.R) through the table view, step by step, and
 * checks what every drawn table must satisfy. TABLE_FIXTURES points at
 * another folder of them; without any, the test skips. The groups
 * scenarios (grp_*, and layout_* which switch to groups) run with
 * TABLE_FIXTURES_GROUPS=1 until R writes the groups payload as agreed. */

const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const { loadTable } = require('./load');

const DIR = process.env.TABLE_FIXTURES ||
  path.join(__dirname, '..', 'fixtures', 'table');
const FILES = fs.existsSync(DIR)
  ? fs.readdirSync(DIR).filter((f) => /^(rec|var)_.*\.json$/.test(f) ||
                                      (!!process.env.TABLE_FIXTURES_GROUPS && /^(grp|layout)_.*\.json$/.test(f)))
  : [];

/** What every drawn step must satisfy. */
function check(el, p, where) {
  const m = p.data;
  const table = el.querySelector('table.blockr-table');
  const empty = el.querySelector('.vd-empty-state');
  if (!m || !Array.isArray(m.cols)) {
    if (p.derived && p.derived.empty) assert.ok(empty, where + ': the empty message shows');
    return;
  }
  if (p.config.look === 'cards' && m.cards && p.config.layout !== 'variables') {
    assert.ok(el.querySelector('.blockr-cards-host:not([hidden])'), where + ': cards drawn');
    return;
  }
  assert.ok(table, where + ': a table is drawn');
  for (const c of m.cols) assert.equal(c.disp.length, m.n, where + ': ' + c.name + ' has one entry per row');
  const heads = m.head.reduce((k, r) => k + r.length, 0);
  assert.equal(table.querySelectorAll('thead th').length, heads, where + ': one th per head cell');
  const rows = table.querySelectorAll('tbody tr.blockr-data-row');
  assert.ok(rows.length <= m.n && (m.n === 0 || rows.length > 0 || m.collapsed), where + ': rows drawn');
  for (const r of rows) assert.equal(r.children.length, m.cols.length, where + ': one cell per column');
  assert.equal(el.innerHTML.includes('<script'), false, where + ': no markup from the data');
}

for (const f of FILES) {
  test('fixture ' + f.replace(/\.json$/, ''), () => {
    const { scenario, steps } = JSON.parse(fs.readFileSync(path.join(DIR, f), 'utf8'));
    const env = loadTable();
    const el = env.add();
    steps.forEach((p, k) => {
      if (!p) return;
      env.b.renderValue(el, p);
      check(el, p.data ? p : Object.assign({}, p, { data: el._viz && el._viz.view.view && el._viz.view.view.model }),
            scenario + ' step ' + (k + 1));
      const row = el.querySelector('tbody tr.blockr-data-row:not(.dt-row-nodrill)');
      if (row && el.querySelector('.vz-table.dt-clickable')) {
        row.dispatchEvent(new env.win.MouseEvent('click', { bubbles: true }));
        const sel = env.selects().at(-1);
        assert.ok(sel === null || (sel.type === 'categorical' && typeof sel.filters === 'object'),
                  scenario + ': a click sends a selection');
      }
    });
  });
}

test('fixtures are read when present', { skip: FILES.length === 0 && 'no fixtures in ' + DIR }, () => {
  assert.ok(FILES.length > 0);
});
