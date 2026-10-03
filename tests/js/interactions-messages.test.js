/* The custom messages R sends that the chart handles, one scenario each:
 * drilldown-data (with rows, without rows held or not held, before the
 * container exists), drilldown-ready, drilldown-theme and the busy cue
 * (blockr-busy, blockr-busy-done). drilldown-capture is in
 * interactions-export.test.js. */
'use strict';

const test = require('node:test');
const h = require('./harness');
const I = require('./interact');

const cfg = (extra) => ({ ...I.BASE, chart_type: 'bar', group: 'AETERM', drill: 'auto', ...extra });

const state = (c) => {
  const el = c.env.win.document.getElementById(c.id);
  const blk = el && el._block;
  return {
    mounted: !!blk,
    rev: blk ? blk._lastDataRev : null,
    rows: blk ? blk.data.length : null,
    orientation: blk ? blk.config.orientation ?? null : null,
    needRev: el ? el._needRev ?? null : null,
    busy: el ? el.classList.contains('blockr-busy') : null,
    busyLine: el && el.querySelector('.blockr-busy-line')
      ? el.querySelector('.blockr-busy-line').textContent : null
  };
};

const data = (rev, extra, label) => [label || `drilldown-data rev ${rev} with rows`,
  (c) => c.send(cfg(extra), { dataRev: rev })];
const noRows = (rev, extra) => [`drilldown-data rev ${rev} without rows${extra ? ' ' + JSON.stringify(extra) : ''}`,
  (c) => c.send(cfg(extra), { data: null, dataRev: rev })];
const ready = (rev, id) => [`drilldown-ready rev ${rev}${id ? ' for ' + id : ''}`,
  (c) => c.env.handlers['drilldown-ready']({ id: id || c.id, data_rev: rev })];
const busy = (label) => [`blockr-busy "${label}"`,
  (c) => c.env.handlers['blockr-busy']({ id: c.id, label })];
const busyDone = () => ['blockr-busy-done', (c) => c.env.handlers['blockr-busy-done']({ id: c.id })];
const wait = (ms) => [`advance ${ms} ms`, (c) => c.advance(ms)];

const scenario = (name, steps, opts = {}) =>
  test(name, () => I.snap('msg-' + name, I.runSteps(cfg(), steps, { noSend: true, extra: state, ...opts })));

scenario('data-with-rows', [
  data(1),
  data(1, { orientation: 'vertical' }, 'drilldown-data rev 1 with rows again (kept, not re-parsed)'),
  data(2, null, 'drilldown-data rev 2 with rows')
]);

scenario('data-without-rows', [
  noRows(5),
  noRows(5, { orientation: 'vertical' }),
  noRows(6),
  data(6),
  noRows(6, { orientation: 'vertical' }),
  noRows(7)
]);

scenario('ready', [
  ready(1),
  data(1),
  ready(1),
  ready(2),
  ready(2),
  ready(1, 'nope-drilldown_block')
]);

scenario('busy', [
  busy('drawing'),
  wait(299),
  wait(1),
  wait(500),
  busy('again'),
  data(1),
  wait(16),
  wait(16),
  busy('drawing'),
  wait(300),
  busyDone()
]);

scenario('busy-without-rows', [
  busy('drawing'),
  noRows(3),
  wait(400),
  data(3),
  wait(32)
]);

test('data before the container exists', () => {
  const env = h.createEnv({ record: 'full' });
  I.useClock(env);
  const id = 'late-drilldown_block';
  const dataless = (rev, extra) => env.win.JSON.parse(JSON.stringify({
    id, columns: I.F.columns, config: cfg(extra), arguments: null, data_rev: rev }));
  const out = {};
  h.send(env, id, { columns: I.F.columns, data: I.F.data, config: cfg(), dataRev: 1 });
  // A message without rows for the same rev keeps the waiting rows.
  env.handlers['drilldown-data'](dataless(1, { orientation: 'vertical' }));
  // A theme waits too.
  env.handlers['drilldown-theme']({ id, theme: 'dark' });
  out.inputsBefore = env.inputs().map(I.cleanInput);
  const { el } = I.mountRoot(env, id);
  env.binding.initialize(el);
  out.afterMount = {
    inputs: env.inputs().map(I.cleanInput),
    rows: el._block.data.length, rev: el._block._lastDataRev,
    orientation: el._block.config.orientation, theme: el._block.theme,
    instances: env.fake.instances.map((i) => ({ theme: i.theme, ops: i.log.map((x) => x.op) }))
  };
  // A second container under another id gets nothing of this.
  const other = I.mountRoot(env, 'other-drilldown_block');
  env.binding.initialize(other.el);
  out.other = { rows: other.el._block.data.length,
                inputs: env.inputs().map(I.cleanInput).slice(out.afterMount.inputs.length) };
  // A message for a newer rev without rows replaces a waiting one.
  const id2 = 'later-drilldown_block';
  h.send(env, id2, { columns: I.F.columns, data: I.F.data, config: cfg(), dataRev: 1 });
  env.handlers['drilldown-data']({ ...dataless(2), id: id2 });
  const m2 = I.mountRoot(env, id2);
  env.binding.initialize(m2.el);
  out.newerRevWithoutRows = { rows: m2.el._block.data.length,
                              inputs: env.inputs().map(I.cleanInput).slice(-1) };
  I.snap('msg-before-mount', out);
  env.close();
});
