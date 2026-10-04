/* The summarize table block's browser side (inst/js/summarize-table.js), driven
 * the way the page drives it: the chrome the block's UI renders once, then
 * the payloads its server pushes. Both come from the block's own R code
 * (fixtures/make-summarize-fixtures.R), so a name the R side writes and the
 * JS side does not read fails here.
 */
'use strict';

const { test } = require('node:test');
const assert = require('node:assert/strict');
const { Window } = require('happy-dom');
const h = require('./harness.js');
const fixtures = require('./fixtures/summarize-table.json');

// summarize_table_dep(): blockr.ui's controls, the shared engine, table.js, then
// the block's own script.
const FILES = [...h.UI_FILES, 'drilldown-agg.js', 'drilldown-config.js',
  'capture-pages.js', 'busy-cue.js', 'table.js', 'summarize-table.js'];

/** Mount a case's chrome, load the scripts, and deliver its first payload
 *  unless `deliver` is false. */
function open(name, { deliver = true } = {}) {
  const c = fixtures[name];
  const win = new Window({ url: 'http://localhost/' });
  win.eval(h.PRELUDE);
  win.document.body.innerHTML = c.chrome;
  win.eval(h.iconsScript());
  for (const f of FILES) win.eval(h.source(f));
  const doc = win.document;
  const root = doc.querySelector('[data-summarize-elem-id]');
  const env = {
    win, doc, root, id: c.id,
    /** Deliver payload `i` (0-based) of the case, as the message handler gets it. */
    push(i) {
      const p = c.payloads[i];
      win.__handlers['blockr-viz-summarize-data']({ id: c.id, rev: p.rev, payload: p.payload });
    },
    rows: () => Array.from(doc.querySelectorAll('tr.blockr-summarize-row')),
    labels: () => env.rows().map((r) => r.getAttribute('data-summarize-label')),
    shown: () => env.rows().filter((r) =>
      !r.classList.contains('blockr-summarize-hidden-search') &&
      !r.classList.contains('collapsed-hidden')),
    inputs: () => h.normalize(Array.from(win.__inputs)),
    actions: () => env.inputs().filter((x) => x.name === c.id + '_action').map((x) => x.value),
    flush() {
      for (let round = 0; round < 10 && win.__timers.length; round++) {
        const due = win.__timers.splice(0).sort((a, b) => a.ms - b.ms);
        for (const t of due) t.fn();
      }
    },
    band: () => doc.querySelector('.blockr-settings.dd-popover'),
    isOpen: () => env.band().classList.contains('blockr-settings--open'),
    search: () => doc.querySelector('input.blockr-search'),
    footer: () => doc.querySelector('.dd-status-footer').textContent,
    close() { win.happyDOM.abort(); return win.happyDOM.close(); }
  };
  if (deliver) env.push(0);
  return env;
}

const click = (el) => el.dispatchEvent(new el.ownerDocument.defaultView.MouseEvent(
  'click', { bubbles: true, cancelable: true }));

test('the payload draws the rows, the title and the escaped labels', async () => {
  const t = open('flat');
  assert.deepEqual(t.labels(), ['T <1> & co', 'T2', 'T3', 'T4']);
  assert.equal(t.doc.querySelector('.dd-table-title').textContent, 'Terms');
  const label = t.rows()[0].querySelector('.blockr-summarize-label');
  assert.equal(label.textContent, 'T <1> & co');
  assert.equal(label.children.length, 0);
  await t.close();
});

test('a container with no payload yet announces itself, and draws when it comes', async () => {
  const t = open('flat', { deliver: false });
  assert.deepEqual(t.inputs().map((x) => x.name), [t.id + '_ready']);
  assert.equal(t.rows().length, 0);
  t.push(0);
  assert.equal(t.rows().length, 4);
  await t.close();
});

test('search hides the rows that do not match, and an empty query shows them again', async () => {
  const t = open('flat');
  const box = t.search();
  box.value = 'T3';
  box.dispatchEvent(new t.win.Event('input', { bubbles: true }));
  t.flush();
  assert.deepEqual(t.shown().map((r) => r.getAttribute('data-summarize-label')), ['T3']);
  box.value = '';
  box.dispatchEvent(new t.win.Event('input', { bubbles: true }));
  t.flush();
  assert.equal(t.shown().length, 4);
  await t.close();
});

test('a header click sorts, and the third click returns to the server order', async () => {
  const t = open('ascending');
  assert.deepEqual(t.labels(), ['T4', 'T3', 'T2', 'T <1> & co']);
  const th = t.doc.querySelector('th.blockr-sortable[data-col-index="1"]');
  click(th);
  assert.deepEqual(t.labels(), ['T <1> & co', 'T2', 'T3', 'T4']);
  click(th);
  assert.deepEqual(t.labels(), ['T4', 'T3', 'T2', 'T <1> & co']);
  click(th);
  assert.deepEqual(t.labels(), ['T4', 'T3', 'T2', 'T <1> & co']);
  await t.close();
});

test('a parent row folds and unfolds its children', async () => {
  const t = open('nested');
  assert.deepEqual(t.shown().map((r) => r.getAttribute('data-summarize-label')), ['SOC A', 'SOC B']);
  const btn = t.rows()[0].querySelector('.blockr-indent-btn');
  click(btn);
  assert.deepEqual(t.shown().map((r) => r.getAttribute('data-summarize-label')),
    ['SOC A', 'T <1> & co', 'T2', 'SOC B']);
  click(t.rows()[0].querySelector('.blockr-indent-btn'));
  assert.deepEqual(t.shown().map((r) => r.getAttribute('data-summarize-label')), ['SOC A', 'SOC B']);
  await t.close();
});

test('a colour column fills the legend band', async () => {
  const t = open('color');
  const lg = t.doc.querySelector('.blockr-summarize-legend');
  assert.equal(lg.style.display, '');
  assert.equal(lg.querySelector('.blockr-summarize-legend-title').textContent, 'SEV');
  assert.deepEqual(
    Array.from(lg.querySelectorAll('.blockr-summarize-legend-item')).map((x) => x.textContent),
    ['MILD', 'MODERATE']);
  await t.close();
});

test('a height makes the table scroll in its own box', async () => {
  const t = open('height');
  const w = t.doc.querySelector('.blockr-table-wrapper');
  assert.equal(w.style.maxHeight, '300px');
  assert.equal(w.classList.contains('dt-scroll-page'), false);
  await t.close();
});

test('a latched drill: a row click filters, a second click on it clears', async () => {
  const t = open('flat');
  const row = t.rows()[1];
  click(row);
  assert.deepEqual(t.actions().at(-1), {
    action: 'filter', type: 'categorical', column: 'TERM', values: ['T2'], nonce: 1
  });
  assert.ok(row.classList.contains('is-on'));
  assert.match(t.footer(), /TERM = T2/);
  click(row);
  assert.equal(t.actions().at(-1).action, 'clear_filter');
  assert.equal(row.classList.contains('is-on'), false);
  await t.close();
});

test('the gear stays open while its container settings change', async () => {
  const t = open('settings');
  click(t.doc.querySelector('.blockr-gear-btn'));
  assert.ok(t.isOpen());
  const band = t.band();
  const gear = t.doc.querySelector('.blockr-gear-btn');
  const clickRow = () => {
    const before = t.actions().length;
    click(t.rows()[0]);
    return t.actions().slice(before);
  };

  // Start: search on, no drill. A row click sends nothing.
  assert.equal(t.search().style.display, '');
  assert.deepEqual(clickRow(), []);

  // 1: search off. The box hides, the gear stays.
  t.push(1);
  assert.equal(t.search().style.display, 'none');

  // 2: drill on TERM. A click now filters, latched.
  t.push(2);
  assert.equal(t.root.getAttribute('data-summarize-drill'), 'TERM');
  assert.deepEqual(clickRow().map((a) => [a.action, a.values]), [['filter', ['T <1> & co']]]);
  assert.ok(t.rows()[0].classList.contains('is-on'));

  // 3: send to filter on. A click is an event: it sends, nothing latches.
  t.push(3);
  assert.equal(t.root.getAttribute('data-summarize-ctrl-target'), 'auto');
  const sent = clickRow();
  assert.equal(sent.length, 1);
  assert.equal(sent[0].action, 'filter');
  assert.equal(t.rows()[0].classList.contains('is-on'), false);

  // 4: send to filter off; 5: drill off. A click sends nothing again.
  t.push(4);
  assert.equal(t.root.getAttribute('data-summarize-ctrl-target'), '');
  t.push(5);
  assert.equal(t.root.hasAttribute('data-summarize-drill'), false);
  assert.deepEqual(clickRow(), []);

  // 6: search back on.
  t.push(6);
  assert.equal(t.search().style.display, '');

  assert.ok(t.isOpen(), 'the band is still open');
  assert.equal(t.band(), band, 'the band is the same node');
  assert.equal(t.doc.querySelector('.blockr-gear-btn'), gear, 'the gear is the same node');
  await t.close();
});

test('turning search off clears a typed query', async () => {
  const t = open('settings');
  const box = t.search();
  box.value = 'T3';
  box.dispatchEvent(new t.win.Event('input', { bubbles: true }));
  t.flush();
  assert.equal(t.shown().length, 1);
  t.push(1);
  assert.equal(box.value, '');
  assert.equal(t.shown().length, 4);
  await t.close();
});

test('a gear edit is sent as a config action', async () => {
  const t = open('flat');
  click(t.doc.querySelector('.blockr-gear-btn'));
  const asc = Array.from(t.band().querySelectorAll('button.blockr-segmented__seg'))
    .find((b) => b.textContent.trim() === 'Ascending');
  assert.ok(asc, 'the band has a sort direction control');
  click(asc);
  assert.deepEqual(t.actions().at(-1), { action: 'config', param: 'sort_dir', value: 'asc' });
  assert.ok(t.isOpen());
  await t.close();
});
