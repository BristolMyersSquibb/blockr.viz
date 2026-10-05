/* The binding: the messages R sends (with and without rows, before the
 * container exists, _ready, the busy cue), the theme, resize, dispose and
 * re-mounts, driven on a gantt. Snapshots in __snapshots__/interactions/,
 * named binding-*.
 */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const h = require('../harness');
const I = require('../interact');

const GANTT = { ...I.BASE, chart_type: 'gantt', x: 'ASTDY', xend: 'AENDY', y: 'AETERM',
                drill: 'auto' };
const cfg = (extra) => ({ ...GANTT, ...extra });

const state = (c) => {
  const el = c.env.win.document.getElementById(c.id);
  const blk = el && el._block;
  return {
    mounted: !!blk,
    rev: blk ? blk._lastDataRev : null,
    rows: blk ? blk.data.length : null,
    sortDir: blk ? blk.config.sort_dir ?? null : null,
    needRev: el ? el._needRev ?? null : null,
    busy: el ? el.classList.contains('blockr-busy') : null,
    busyLine: el && el.querySelector('.blockr-busy-line')
      ? el.querySelector('.blockr-busy-line').textContent : null
  };
};

const data = (rev, extra, label) => [label || `data rev ${rev} with rows`,
  (c) => c.send(cfg(extra), { dataRev: rev })];
const noRows = (rev, extra) => [`data rev ${rev} without rows`,
  (c) => c.send(cfg(extra), { data: null, dataRev: rev })];
const ready = (rev, id) => [`ready rev ${rev}`,
  (c) => c.env.handlers['drilldown-ready']({ id: id || c.id, data_rev: rev })];
const busy = (label) => [`busy ${label}`, (c) => c.env.handlers['blockr-busy']({ id: c.id, label })];
const busyDone = () => ['busy done', (c) => c.env.handlers['blockr-busy-done']({ id: c.id })];
const wait = (ms) => [`advance ${ms} ms`, (c) => c.advance(ms)];
const theme = (t) => [`theme ${t}`, (c) => c.env.handlers['drilldown-theme']({ id: c.id, theme: t })];

/** Run the steps and compare what they record with the snapshot `snap`. */
const same = (snap, name, steps, opts = {}) => test(name, () => {
  I.snap('binding-' + snap, I.runSteps(cfg(), steps, { noSend: true, extra: state, ...opts }));
});

same('data-revs', 'data with rows, again with the same rev, then a new rev', [
  data(1), data(1, { sort_dir: 'desc' }), data(2)
]);

same('data-without-rows', 'data without rows: ask once per rev, draw once they come', [
  noRows(5), noRows(5, { sort_dir: 'desc' }), noRows(6), data(6),
  noRows(6, { sort_dir: 'desc' }), noRows(7)
]);

same('ready', 'the reply to _ready', [
  ready(1), data(1), ready(1), ready(2), ready(2), ready(1, 'nope-drilldown_block')
]);

same('busy', 'the busy cue ends two frames after the picture', [
  busy('drawing'), wait(299), wait(1), wait(500), busy('again'), data(1), wait(16), wait(16),
  busy('drawing'), wait(300), busyDone(), wait(250)
]);

same('busy-without-rows', 'the busy cue waits for rows that are not there', [
  busy('drawing'), noRows(3), wait(400), data(3), wait(32)
]);

same('theme', 'a theme re-creates the instances; before data it only waits', [
  theme('dark'), data(1), theme('dark'), theme('vintage'), theme('default'), wait(400)
]);

same('facet-grid', 'a facet grid: panels, resize pass and the observer', [
  data(1, { y: 'USUBJID', facet: 'ARM', facet_cols: '2' }), wait(300),
  ['resize', (c) => c.block.resize()],
  data(1, { y: 'USUBJID', facet: 'ARM', facet_cols: '' }), wait(300)
], { width: 900 });

test('data before the container exists, and a theme', () => {
  const run = () => {
    const env = h.createEnv({ record: 'full' });
    I.useClock(env);
    const id = 'late-drilldown_block';
    const dataless = (rev, extra) => env.win.JSON.parse(JSON.stringify({
      id, columns: I.F.columns, config: cfg(extra), arguments: null, data_rev: rev }));
    h.send(env, id, { columns: I.F.columns, data: I.F.data, config: cfg(), dataRev: 1 });
    env.handlers['drilldown-data'](dataless(1, { sort_dir: 'desc' }));
    env.handlers['drilldown-theme']({ id, theme: 'dark' });
    const { el } = I.mountRoot(env, id);
    env.binding.initialize(el);
    const out = JSON.parse(JSON.stringify({
      inputs: env.inputs().map(I.cleanInput),
      rows: el._block.data.length, rev: el._block._lastDataRev,
      sortDir: el._block.config.sort_dir, theme: el._block.theme,
      instances: env.fake.instances.map((i) => ({ theme: i.theme, ops: i.log.map((x) => x.op) }))
    }));
    env.close();
    return out;
  };
  I.snap('binding-late-container', run());
});

test('bind with nothing waiting: the same card, and the _ready announce', () => {
  const run = () => {
    const env = h.createEnv({ record: 'full' });
    const { el } = I.mountRoot(env, I.ID, { download: true });
    env.binding.initialize(el);
    const skeleton = (node, depth) => Array.from(node.children)
      .filter((c) => c.tagName.toLowerCase() !== 'svg')
      .map((c) => [c.tagName.toLowerCase() + '.' + String(c.className).trim().split(/\s+/).join('.'),
                   c.style.display === 'none', depth > 1 ? skeleton(c, depth - 1) : null]);
    const out = JSON.parse(JSON.stringify({
      inputs: env.inputs().map(I.cleanInput), root: skeleton(el.parentElement, 4),
      footer: I.footer(el._block)
    }));
    env.close();
    return out;
  };
  I.snap('binding-bind-empty', run());
});

test('dispose: every instance goes, and a resize after it does nothing', () => {
  const c = I.open(cfg({ facet: 'ARM' }));
  c.block.dispose();
  const total = c.env.fake.instances.length;
  assert.ok(total > 1);
  assert.strictEqual(c.env.fake.instances.filter((i) => i.isDisposed()).length, total);
  c.block.resize();
  assert.strictEqual(c.env.fake.instances.length, total);
  c.close();
});

// -- Re-mounts ------------------------------------------------------------------

test('a second initialize disposes the first view\'s instances', () => {
  const c = I.open(cfg());
  const first = c.env.fake.instances.slice();
  assert.ok(first.length > 0);
  c.env.binding.initialize(c.el);
  assert.ok(first.every((i) => i.isDisposed()));
  c.close();
});

test('a dock re-mount (a new element, same id) disposes the old view', () => {
  const c = I.open(cfg());
  const first = c.env.fake.instances.slice();
  c.el.parentElement.remove();
  const { el } = I.mountRoot(c.env, I.ID);
  c.env.binding.initialize(el);
  assert.ok(first.every((i) => i.isDisposed()));
  c.close();
});
