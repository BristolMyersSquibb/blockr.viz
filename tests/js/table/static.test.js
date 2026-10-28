'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadTable, payload, flatModel, tick } = require('./load');

/** A static table element as R writes it. */
function staticEl(doc, p, attrs = {}) {
  const el = doc.createElement('div');
  el.className = 'blockr-viz-static';
  el.setAttribute('data-viz-view', 'table');
  for (const [k, v] of Object.entries(attrs)) el.setAttribute(k, v);
  const s = doc.createElement('script');
  s.type = 'application/json';
  s.textContent = JSON.stringify(p);
  el.appendChild(s);
  doc.body.appendChild(el);
  return el;
}

const bars = () => ({
  n: 3, stub: 0,
  cols: [
    { name: 'ARM', disp: ['Placebo', 'Low', 'High'] },
    { name: 'Rows', disp: ['10', '20', '30'], key: [10, 20, 30],
      mark: { kind: 'bar', w: [33, 66, 100], fill: '#2563eb', dw: 2 } }
  ],
  head: [[{ text: 'Arm', stub: true, col: 0 }, { text: 'Rows', col: 1 }]]
});

test('a printed groups table is drawn on load: every row, no gear, no tools', () => {
  let el;
  const env = loadTable({ before: (win) => {
    el = staticEl(win.document, payload({ data: bars(), config: { layout: 'groups', by: ['ARM'] },
                                          derived: { titles: { title: 'Arms' } } }),
                  { 'data-viz-gear': 'off' });
  } });
  assert.ok(el.querySelector('.blockr-summarize-container'));
  assert.equal(el.querySelectorAll('tbody tr.blockr-data-row').length, 3);
  assert.equal(el.querySelectorAll('.blockr-summarize-fill').length, 3);
  assert.equal(el.querySelector('.blockr-gear-btn'), null);
  assert.ok(el.querySelector('input[type="search"], .vz-search, input').hidden);
  assert.match(el.textContent, /Arms/);
  assert.equal(env.sent.length, 0);
});

test('a table added later is drawn once; a printed one draws past the window', async () => {
  const env = loadTable();
  const el = staticEl(env.win.document, payload({ data: flatModel(600) }), { 'data-viz-gear': 'off' });
  await tick(env.win, 2);
  env.V.table.mount();
  assert.equal(el.querySelectorAll('tbody tr.blockr-data-row').length, 600);
  // A printed table has no search to count rows against.
  assert.equal(el.querySelector('.dt-row-count').textContent, '');
  assert.equal(el.querySelectorAll('.blockr-gear-btn').length, 0);
});

test('data-viz-still="off" keeps the row window, the tools and the gear', async () => {
  const env = loadTable();
  const el = staticEl(env.win.document, payload({ data: flatModel(600) }),
                      { 'data-viz-still': 'off', 'data-viz-gear': 'on' });
  await tick(env.win, 2);
  assert.ok(el.querySelectorAll('tbody tr.blockr-data-row').length < 600);
  assert.equal(el.querySelector('input').hidden, false);
  assert.ok(el.querySelector('.blockr-gear-btn'));
});

test('renderStatic again redraws through the same view and keeps the search', () => {
  const env = loadTable();
  const el = env.win.document.body.appendChild(env.win.document.createElement('div'));
  el.id = 'it';
  const T = env.V.table;
  const first = payload({ data: flatModel(5) });
  T.renderStatic(el, first, { gear: false });
  const input = el.querySelector('input');
  input.value = 'S2';
  T.renderStatic(el, payload({ data: null, dataRev: first.data_rev,
                               derived: { titles: { title: 'Kept rows' } } }));
  assert.equal(el.querySelectorAll('.vz-head').length, 1);
  assert.equal(el.querySelector('input'), input);
  assert.equal(input.value, 'S2');
  assert.match(el.textContent, /Kept rows/);
  assert.equal(el.querySelectorAll('tbody tr.blockr-data-row').length, 5);
  // Rows of another revision are not held: nothing is drawn from them.
  T.renderStatic(el, payload({ data: null, dataRev: first.data_rev + 99,
                               derived: { titles: { title: 'Other rows' } } }));
  assert.doesNotMatch(el.textContent, /Other rows/);
});

test('a click on a static table goes out as <id>_select', () => {
  const env = loadTable();
  const el = env.win.document.body.appendChild(env.win.document.createElement('div'));
  el.id = 'it';
  const m = flatModel(3);
  m.drill = 'ARM';
  env.V.table.renderStatic(el, payload({ data: m, config: { drill: 'ARM' } }), { gear: false });
  el.querySelector('tbody tr.blockr-data-row').dispatchEvent(
    new env.win.MouseEvent('click', { bubbles: true }));
  assert.deepEqual(env.selects(), [{ type: 'categorical', filters: { ARM: ['Placebo'] } }]);
});

test("R's blockr-viz-static message redraws the element with that id", () => {
  const handlers = {};
  const env = loadTable({ before: (win) => {
    win.Shiny.addCustomMessageHandler = (n, f) => { handlers[n] = f; };
  } });
  const el = staticEl(env.win.document, payload({ data: flatModel(2) }), { 'data-viz-gear': 'off' });
  el.id = 'it';
  env.V.table.mount();
  handlers['blockr-viz-static']({ id: 'it', payload: payload({ data: flatModel(4) }) });
  assert.equal(el.querySelectorAll('tbody tr.blockr-data-row').length, 4);
  handlers['blockr-viz-static']({ id: 'gone', payload: payload({ data: flatModel(4) }) });
});

test('the cards look draws its cards', () => {
  const env = loadTable();
  // A records exhibit sends the cards alone.
  const m = { n: 2, cols: [], head: [],
              cards: { n: 2, label: ['S1', 'S2'], cols: [{ label: 'Age', main: ['31', '45'] }] } };
  const el = staticEl(env.win.document, payload({ data: m, config: { look: 'cards' } }),
                      { 'data-viz-gear': 'off' });
  env.V.table.mount(env.win.document);
  assert.equal(el.querySelector('.blockr-cards-host').hidden, false);
  assert.match(el.querySelector('.blockr-cards-host').textContent, /45/);
});

test('an unknown layout says so; bad JSON or no payload draws nothing', () => {
  const env = loadTable();
  const el = staticEl(env.win.document, payload({ data: bars(), config: { layout: 'pivot' } }),
                      { 'data-viz-gear': 'off' });
  const bad = staticEl(env.win.document, {}, {});
  bad.querySelector('script').textContent = '{not json';
  const bare = env.win.document.createElement('div');
  bare.className = 'blockr-viz-static';
  bare.setAttribute('data-viz-view', 'table');
  env.win.document.body.appendChild(bare);
  env.V.table.mount();
  assert.match(el.textContent, /cannot be drawn/);
  assert.equal(bad.children.length, 1);
  assert.equal(bare.children.length, 0);
});
