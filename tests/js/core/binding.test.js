'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadCore } = require('./load');

const tick = (win, ms) => new Promise((r) => win.setTimeout(r, ms));

// A view that records what it was asked to do, and in `rows` the
// rows-changed flag of every render.
const recorder = (log, rows) => class {
  constructor(el) { this.el = el; log.push(['new', el.id]); }
  render(p, rowsChanged) {
    log.push(['render', p.data_rev, p.data && p.data.g]);
    rows.push(rowsChanged);
  }
  error(msg) { log.push(['error', msg]); }
  dispose() { log.push(['dispose', this.el.id]); }
};

const setup = (opts) => {
  const env = loadCore();
  const log = [];
  const rows = [];
  env.V.binding('blockr.viz.test', 'test', recorder(log, rows), opts);
  const b = env.outputs['blockr.viz.test'];
  const add = (id) => {
    const el = env.win.document.createElement('div');
    el.id = id;
    el.className = 'blockr-viz-output';
    el.setAttribute('data-viz-view', 'test');
    env.win.document.body.appendChild(el);
    return el;
  };
  return Object.assign(env, { log, rows, b, add });
};

const payload = (rev, data) => ({ state_rev: 0, data_rev: rev, columns: [],
                                  config: {}, derived: {}, data });

test('the binding finds only its own view kind', () => {
  const { b, add, win } = setup();
  add('a');
  const other = win.document.createElement('div');
  other.className = 'blockr-viz-output';
  other.setAttribute('data-viz-view', 'chart');
  win.document.body.appendChild(other);
  assert.deepEqual(b.find(win.document).map((e) => e.id), ['a']);
});

test('the first value builds the view and draws it', () => {
  const { b, add, log } = setup();
  const el = add('a');
  b.renderValue(el, payload(1, { g: ['x'] }));
  assert.deepEqual(log, [['new', 'a'], ['render', 1, ['x']]]);
});

test('a replayed value draws nothing', () => {
  const { b, add, log } = setup();
  const el = add('a');
  const p = payload(1, { g: ['x'] });
  b.renderValue(el, p);
  b.renderValue(el, p);
  assert.equal(log.filter((l) => l[0] === 'render').length, 1);
});

test('a value without rows uses the rows held for its revision', () => {
  const { b, add, log, sent } = setup();
  const el = add('a');
  b.renderValue(el, payload(1, { g: ['x'] }));
  b.renderValue(el, payload(1, null));
  assert.deepEqual(log.at(-1), ['render', 1, ['x']]);
  assert.equal(sent.length, 0);
});

test('the view is told when the rows are new to it', async () => {
  const { b, add, rows, win } = setup();
  const el = add('a');
  b.renderValue(el, payload(1, { g: ['x'] }));
  b.renderValue(el, payload(1, null));
  b.renderValue(el, payload(1, { g: ['x'] }));
  b.renderValue(el, payload(2, { g: ['y'] }));
  win.document.documentElement.setAttribute('data-bs-theme', 'dark');
  await tick(win, 0);
  assert.deepEqual(rows, [true, false, false, true, false]);
  // A new element under the id draws its held rows as new: its view has none.
  el.remove();
  const again = add('a');
  b.renderValue(again, payload(2, null));
  assert.equal(rows.at(-1), true);
});

test('rows missing for a revision are asked for once', () => {
  const { b, add, log, sent } = setup();
  const el = add('a');
  b.renderValue(el, payload(2, null));
  b.renderValue(el, payload(2, null));
  assert.deepEqual(sent.map((s) => [s.name, s.value]), [['a_need', 2]]);
  assert.equal(log.filter((l) => l[0] === 'render').length, 0);
  b.renderValue(el, payload(2, { g: ['y'] }));
  assert.deepEqual(log.at(-1), ['render', 2, ['y']]);
});

test('a silent error keeps the picture; a real one reaches the view', () => {
  const { b, add, log } = setup();
  const el = add('a');
  b.renderValue(el, payload(1, { g: ['x'] }));
  b.renderError(el, { message: '' });
  assert.equal(log.filter((l) => l[0] === 'error').length, 0);
  b.renderError(el, { message: 'no data' });
  assert.deepEqual(log.at(-1), ['error', 'no data']);
  assert.equal(el.childNodes.length, 0);
});

test('unbinding an element that stays keeps its view', async () => {
  const { b, add, log, trigger, win } = setup();
  const el = add('a');
  b.renderValue(el, payload(1, { g: ['x'] }));
  trigger(el, 'shiny:unbound');
  await tick(win, 5);
  assert.equal(log.filter((l) => l[0] === 'dispose').length, 0);
});

test('an element that left the page loses its view, then its rows', async () => {
  const { V, b, add, log, trigger, sent, win } = setup({ grace: 20 });
  const open = V.Scope.open();
  const el = add('a');
  b.renderValue(el, payload(1, { g: ['x'] }));
  el.remove();
  trigger(el, 'shiny:unbound');
  await tick(win, 5);
  assert.deepEqual(log.at(-1), ['dispose', 'a']);
  assert.equal(V.Scope.open(), open);
  await tick(win, 40);
  const again = add('a');
  b.renderValue(again, payload(1, null));
  assert.deepEqual(sent.map((s) => s.name), ['a_need']);
});

test('a new element under the same id takes over the rows', async () => {
  const { b, add, log, trigger, sent, win } = setup({ grace: 1000 });
  const el = add('a');
  b.renderValue(el, payload(1, { g: ['x'] }));
  el.remove();
  trigger(el, 'shiny:unbound');
  await tick(win, 5);
  const again = add('a');
  b.renderValue(again, payload(1, null));
  assert.deepEqual(log.at(-1), ['render', 1, ['x']]);
  assert.equal(sent.length, 0);
});

test('a new element under a live id disposes the old view', () => {
  const { b, add, log } = setup();
  const first = add('a');
  b.renderValue(first, payload(1, { g: ['x'] }));
  first.remove();
  const second = add('a');
  b.renderValue(second, payload(1, null));
  assert.deepEqual(log.map((l) => l[0]),
                   ['new', 'render', 'dispose', 'new', 'render']);
});

test('with a busy label, a found element gets its view and a busy watch', () => {
  const env = setup({ busy: 'drawing' });
  const watched = [];
  env.win.Blockr.busy = { watch: (id, label) => watched.push([id, label]) };
  const el = env.add('a');
  env.b.find(env.win.document);
  env.b.find(env.win.document);
  assert.deepEqual(env.log, [['new', 'a']]);
  assert.deepEqual(watched, [['a', 'drawing']]);
  env.b.renderValue(el, payload(1, { g: ['x'] }));
  assert.deepEqual(env.log, [['new', 'a'], ['render', 1, ['x']]]);
});

test('without a busy label, finding an element builds nothing', () => {
  const { b, add, log, win } = setup();
  add('a');
  b.find(win.document);
  assert.deepEqual(log, []);
});

test('a light/dark switch draws the last value again; an unmounted view is left alone', async () => {
  const { b, add, log, win } = setup({ grace: 0 });
  const el = add('a');
  b.renderValue(el, payload(1, { g: ['x'] }));
  win.document.documentElement.setAttribute('data-bs-theme', 'dark');
  await Promise.resolve();
  assert.deepEqual(log.filter((l) => l[0] === 'render'), [['render', 1, ['x']], ['render', 1, ['x']]]);
  el._viz.scope.dispose();
  win.document.documentElement.setAttribute('data-bs-theme', 'light');
  await Promise.resolve();
  assert.equal(log.filter((l) => l[0] === 'render').length, 2);
});
