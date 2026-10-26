'use strict';

// The records engine's gear (table.js) in the table block's records and
// variables layouts. Fixtures: tests/js/fixtures/make-table-fixtures.R, the
// block's own server under testServer().

const { test } = require('node:test');
const assert = require('node:assert/strict');
const { Window } = require('happy-dom');
const h = require('./harness.js');
const fixtures = require('./fixtures/table-records.json');

const FILES = [...h.UI_FILES, 'drilldown-agg.js', 'drilldown-config.js',
  'capture-pages.js', 'busy-cue.js', 'table.js'];

/** Mount a case's chrome, push its layout and payload `at` (default: the
 *  last one), and open the gear. */
function open(name, at) {
  const c = fixtures[name];
  const win = new Window({ url: 'http://localhost/' });
  win.eval(h.PRELUDE);
  win.document.body.innerHTML = c.chrome;
  win.eval(h.iconsScript());
  for (const f of FILES) win.eval(h.source(f));
  win.document.dispatchEvent(new win.Event('DOMContentLoaded'));
  const doc = win.document;
  win.__handlers['blockr-viz-table-layout'](c.layout);
  const p = c.payloads[at == null ? c.payloads.length - 1 : at];
  win.__handlers['blockr-viz-table-data']({ id: c.id, rev: p.rev, payload: p.payload });
  const env = {
    win, doc, id: c.id,
    band: () => doc.querySelector('.blockr-settings.dd-popover'),
    actions: () => h.normalize(Array.from(win.__inputs))
      .filter((x) => x.name === c.id + '_action').map((x) => x.value),
    close() { win.happyDOM.abort(); return win.happyDOM.close(); }
  };
  click(doc.querySelector('.blockr-gear-btn'));
  return env;
}

const click = (el) => el.dispatchEvent(new el.ownerDocument.defaultView.MouseEvent(
  'click', { bubbles: true, cancelable: true }));
const titlesOf = (band) => Array.from(band.querySelectorAll('.dd-section-title'))
  .map((e) => e.textContent.trim());
const has = (band, key) => band.querySelectorAll('.dd-role-' + key).length > 0;
const segment = (band, key, label) => Array.from(
  band.querySelectorAll('.dd-role-' + key + ' [role=radio]'))
  .find((b) => b.textContent.trim() === label);

test('the records gear reads Rows are, Presentation, Color, then the folds', async () => {
  const t = open('flat');
  const band = t.band();
  const titles = titlesOf(band);
  assert.deepEqual(titles.slice(0, 3), ['Rows are', 'Presentation', 'Color']);
  ['Titles', 'Drill-down', 'Prepare script'].forEach((w, k) => {
    assert.ok(titles[3 + k].startsWith(w), titles[3 + k]);
  });
  assert.equal(titles.includes('Mapping'), false, 'no Mapping section');
  assert.equal(titles.includes('Aggregation'), false, 'no Aggregation section');
  const layout = band.querySelector('.dd-section--layout');
  for (const key of ['group', 'rowname', 'columns']) {
    assert.ok(layout.querySelector('.dd-role-' + key), key + ' sits under the switch');
  }
  // A flat table: numbers and colour, no collapsing or missing groups.
  for (const key of ['format', 'unit', 'sortable', 'look', 'search', 'download']) {
    assert.ok(has(band, key), key);
  }
  assert.equal(has(band, 'collapsible'), false);
  assert.equal(has(band, 'missing'), false);
  assert.equal(band.querySelector('.dd-input-badge'), null, 'no badge');
  assert.match(band.textContent, /Header sorting/);
  await t.close();
});

test('the records gear shows the row names, the columns and the script', async () => {
  const t = open('flat');
  const band = t.band();
  assert.match(band.querySelector('.dd-role-rowname').textContent, /Species/);
  assert.match(band.querySelector('.dd-role-columns').textContent, /Petal/);
  const script = band.querySelector('textarea.dd-script-editor');
  assert.ok(script, 'the prepare script is open');
  assert.equal(script.value, 'dplyr::filter(data, Sepal > 5.5)');
  assert.equal(/Script failed/.test(band.textContent), false);
  await t.close();
});

test('a nested records table is no finished table: numbers, missing, collapse', async () => {
  const t = open('nested');
  const band = t.band();
  assert.equal(band.querySelector('.dd-input-badge'), null, 'no Finished table badge');
  for (const key of ['group', 'rowname', 'columns', 'format', 'unit',
    'collapsible', 'missing']) {
    assert.ok(has(band, key), key);
  }
  assert.equal(titlesOf(band).includes('Color'), false, 'no colour on nested rows');
  // The format reached the cells: 5.006 printed with no decimals.
  const cells = Array.from(t.doc.querySelectorAll('tbody td.blockr-data'))
    .map((c) => c.textContent.trim());
  assert.ok(cells.includes('5'), cells.join(','));

  click(segment(band, 'missing', 'Drop'));
  const last = t.actions().pop();
  assert.deepEqual([last.param, last.value], ['missing', 'drop']);
  await t.close();
});

test('the variables gear has missing and the script, no number format', async () => {
  const t = open('variables');
  const band = t.band();
  for (const key of ['collapsible', 'missing']) assert.ok(has(band, key), key);
  for (const key of ['format', 'unit', 'rowname', 'columns']) {
    assert.equal(has(band, key), false, key);
  }
  assert.equal(band.querySelector('.dd-input-badge'), null);
  assert.ok(titlesOf(band).some((x) => x.startsWith('Prepare script')));
  await t.close();
});

test('a finished input keeps its badge and offers no row controls', async () => {
  const t = open('finished');
  const band = t.band();
  assert.ok(band.querySelector('.dd-input-badge'), 'Finished table badge');
  for (const key of ['group', 'rowname', 'columns', 'format', 'missing']) {
    assert.equal(has(band, key), false, key);
  }
  assert.ok(has(band, 'collapsible'));
  await t.close();
});
