'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadCore } = require('./load');

const tick = (win, ms) => new Promise((r) => win.setTimeout(r, ms));

test('dispose removes listeners, observers, timers, nodes and children', async () => {
  const { win, V } = loadCore();
  const doc = win.document;
  const s = new V.Scope();
  let clicks = 0;
  let fired = 0;
  let disposed = 0;
  s.on(doc, 'click', () => { clicks += 1; });
  s.resize(doc.body, () => {});
  s.mutate(doc.body, () => {}, { childList: true });
  s.timeout(() => { fired += 1; }, 5);
  s.interval(() => { fired += 1; }, 5);
  const pop = s.node(doc.body.appendChild(doc.createElement('div')));
  s.own({ dispose: () => { disposed += 1; } });
  assert.equal(s.size(), 7);

  doc.dispatchEvent(new win.Event('click'));
  assert.equal(clicks, 1);

  s.dispose();
  assert.equal(s.size(), 0);
  assert.equal(s.disposed, true);
  doc.dispatchEvent(new win.Event('click'));
  assert.equal(clicks, 1);
  assert.equal(pop.parentNode, null);
  assert.equal(disposed, 1);
  await tick(win, 20);
  assert.equal(fired, 0);
  s.dispose();
  assert.equal(disposed, 1);
});

test('a fired timeout lets go of itself', async () => {
  const { win, V } = loadCore();
  const s = new V.Scope();
  let n = 0;
  s.timeout(() => { n += 1; }, 1);
  assert.equal(s.size(), 1);
  await tick(win, 10);
  assert.equal(n, 1);
  assert.equal(s.size(), 0);
});

test('an undo function removes one registration early', () => {
  const { win, V } = loadCore();
  const s = new V.Scope();
  let clicks = 0;
  const off = s.on(win.document, 'click', () => { clicks += 1; });
  off();
  off();
  win.document.dispatchEvent(new win.Event('click'));
  assert.equal(clicks, 0);
  assert.equal(s.size(), 0);
});

test('debounce calls once after the last of a run', async () => {
  const { win, V } = loadCore();
  const s = new V.Scope();
  const seen = [];
  const f = s.debounce((x) => seen.push(x), 5);
  f(1); f(2); f(3);
  await tick(win, 20);
  assert.deepEqual(seen, [3]);
  f(4);
  s.dispose();
  await tick(win, 20);
  assert.deepEqual(seen, [3]);
});

test('a registration after dispose is undone at once', () => {
  const { win, V } = loadCore();
  const s = new V.Scope();
  s.dispose();
  let clicks = 0;
  let disposed = 0;
  s.on(win.document, 'click', () => { clicks += 1; });
  s.own({ dispose: () => { disposed += 1; } });
  win.document.dispatchEvent(new win.Event('click'));
  assert.equal(clicks, 0);
  assert.equal(disposed, 1);
  assert.equal(s.size(), 0);
});

test('50 mounts and unmounts leave nothing open', () => {
  const { win, V } = loadCore();
  const start = V.Scope.open();
  for (let i = 0; i < 50; i++) {
    const s = new V.Scope();
    s.on(win, 'resize', () => {});
    s.interval(() => {}, 1000);
    s.node(win.document.body.appendChild(win.document.createElement('div')));
    s.dispose();
  }
  assert.equal(V.Scope.open(), start);
  assert.equal(win.document.body.children.length, 0);
});
