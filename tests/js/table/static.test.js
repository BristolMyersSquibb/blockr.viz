'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadTable, payload, flatModel } = require('./load');

/** A static table element as R writes it (table_static()). */
function staticEl(doc, p) {
  const el = doc.createElement('div');
  el.className = 'blockr-viz-static';
  el.setAttribute('data-viz-view', 'table');
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

test('a static groups table is drawn on load: every row, no gear, no tools', () => {
  let el;
  const env = loadTable({ before: (win) => {
    el = staticEl(win.document, payload({ data: bars(), config: { layout: 'groups', by: ['ARM'] },
                                          derived: { titles: { title: 'Arms' } } }));
  } });
  assert.ok(el.querySelector('.blockr-summarize-container'));
  assert.equal(el.querySelectorAll('tbody tr.blockr-data-row').length, 3);
  assert.equal(el.querySelectorAll('.blockr-summarize-fill').length, 3);
  assert.equal(el.querySelector('.blockr-gear-btn'), null);
  const search = el.querySelector('.vz-head__tools input');
  assert.ok(!search || search.closest('[hidden]'));
  assert.match(el.textContent, /Arms/);
  assert.equal(env.sent.length, 0);
});

test('mount() draws a table added later once, and records draw past the window', () => {
  const env = loadTable();
  const el = staticEl(env.win.document, payload({ data: flatModel(600) }));
  env.V.table.mount();
  env.V.table.mount();
  assert.equal(el.querySelectorAll('tbody tr.blockr-data-row').length, 600);
  assert.equal(el.children.length, 2);
});

test('the cards look draws its cards', () => {
  const env = loadTable();
  // A records exhibit sends the cards alone (cards_exhibit()).
  const m = { n: 2, cols: [], head: [],
              cards: { n: 2, label: ['S1', 'S2'], cols: [{ label: 'Age', main: ['31', '45'] }] } };
  const el = staticEl(env.win.document, payload({ data: m, config: { look: 'cards' } }));
  env.V.table.mount(env.win.document);
  assert.equal(el.querySelector('.blockr-cards-host').hidden, false);
  assert.match(el.querySelector('.blockr-cards-host').textContent, /45/);
});

test('an unknown layout or a missing payload draws nothing', () => {
  const env = loadTable();
  const el = staticEl(env.win.document, payload({ data: bars(), config: { layout: 'pivot' } }));
  const bare = env.win.document.createElement('div');
  bare.className = 'blockr-viz-static';
  bare.setAttribute('data-viz-view', 'table');
  env.win.document.body.appendChild(bare);
  env.V.table.mount();
  assert.equal(el.children.length, 1);
  assert.equal(bare.children.length, 0);
});
