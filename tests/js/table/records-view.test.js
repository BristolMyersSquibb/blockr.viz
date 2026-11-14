'use strict';

// The table block's data view (R/table-records-view.R): a tool left of the
// download while R offers records, their table under the table.

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadTable, payload, flatModel } = require('./load');

function mount(p) {
  const env = loadTable();
  const el = env.add();
  env.b.renderValue(el, p);
  const $ = (sel) => el.querySelector(sel);
  const click = (node) => node.dispatchEvent(new env.win.MouseEvent('click', { bubbles: true }));
  const render = (q) => env.b.renderValue(el, payload(q));
  const dataSent = () => env.sent.filter((s) => s.name.endsWith('_data:blockr.viz.data_view'))
    .map((s) => JSON.parse(JSON.stringify(s.value)));
  return Object.assign(env, { el, $, click, render, dataSent });
}

/** The records' table as R sends it (interactive_table_payload()). */
function records(n = 3) {
  return payload({ data: flatModel(n, { drill: null }), config: { search: true, download: false } });
}

test('a table without records has no data view tool', () => {
  const t = mount(payload({ data: flatModel(3) }));
  assert.equal(t.$('.vz-data-tool'), null);
  assert.equal(t.$('.vz-data'), null);
});

test('records bring the tool, left of the download, after the search', () => {
  const t = mount(payload({ data: flatModel(3), derived: { data_view_tool: true } }));
  const tools = Array.from(t.$('.vz-head__tools').children).map((e) => e.className);
  const i = tools.findIndex((c) => c.includes('vz-data-tool'));
  assert.ok(i > tools.findIndex((c) => c.includes('vz-table-search')));
  assert.ok(i < tools.findIndex((c) => c.includes('vz-download')));
  assert.equal(t.$('.vz-data-tool').hidden, false);
  assert.equal(t.$('.vz-data-tool').getAttribute('aria-pressed'), 'false');
});

test('the tool asks R for the records and draws them, named and counted', () => {
  const t = mount(payload({ data: flatModel(3), derived: { data_view_tool: true } }));
  t.click(t.$('.vz-data-tool'));
  assert.equal(t.dataSent().at(-1).on, true);
  t.render({ data: flatModel(3), derived: {
    data_view_tool: true,
    data_view: { tab: 'records', label: 'Adverse events', rows: 2, total: 5, summary: null,
                 table: records(2) }
  } });
  assert.equal(t.$('.vz-data-tool').getAttribute('aria-pressed'), 'true');
  assert.equal(t.$('.vz-data__meta').textContent, 'Adverse events2 of 5');
  assert.equal(t.$('.vz-data__n').textContent, '2 of 5');
  const inner = t.$('.vz-data__table');
  assert.equal(inner.hidden, false);
  assert.equal(inner.querySelectorAll('tbody tr.vz-table-row').length, 2);
  // The records search: the table view's own box, inside the data view.
  assert.equal(inner.querySelector('input.vz-table-search').hidden, false);
});

test('records that go away hide the tool and close the table', () => {
  const t = mount(payload({ data: flatModel(3), derived: { data_view_tool: true } }));
  t.click(t.$('.vz-data-tool'));
  t.render({ data: flatModel(3), derived: {
    data_view_tool: true,
    data_view: { tab: 'records', label: 'Adverse events', rows: 3, total: 3, summary: null,
                 table: records(3) }
  } });
  t.render({ data: flatModel(3), derived: { data_view_tool: false } });
  assert.equal(t.$('.vz-data-tool').hidden, true);
  assert.equal(t.$('.vz-data-tool').getAttribute('aria-pressed'), 'false');
});
