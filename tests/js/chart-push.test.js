/* Messages without rows: R leaves `data` out when the client already holds
 * the rows for that data_rev (send_chart_msg() in chart-block.R). */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const h = require('./harness');
const { F, aggregated } = require('./cases');

const config = aggregated.find((c) => c.name === 'bar-group').config;

// setOption calls so far, across every echarts instance: a redraw adds some.
const draws = (env) => env.fake.instances.reduce(
  (n, i) => n + i.log.filter((e) => e.op === 'setOption').length, 0);

const needs = (env, id) => env.inputs().filter((i) => i.name === id + '_need');

const dataless = (env, id, cfg, rev) => env.win.JSON.parse(JSON.stringify({
  id, columns: F.columns, config: cfg, arguments: null, data_rev: rev
}));

test('a message without rows reuses the rows and draws the new config', () => {
  const env = h.createEnv();
  const { block, id } = h.mount(env);
  h.send(env, id, { columns: F.columns, data: F.data, config, dataRev: 7 });
  const rows = block.data;
  const before = draws(env);

  env.handlers['drilldown-data'](
    dataless(env, id, { ...config, orientation: 'vertical' }, 7));

  assert.strictEqual(block.data, rows);
  assert.strictEqual(block.config.orientation, 'vertical');
  assert.ok(draws(env) > before);
  assert.deepStrictEqual(needs(env, id), []);
  env.close();
});

test('a message without rows draws what the same message with rows draws', () => {
  const vertical = { ...config, orientation: 'vertical' };
  const viaRows = h.createEnv();
  const a = h.mount(viaRows);
  h.send(viaRows, a.id, { columns: F.columns, data: F.data, config, dataRev: 7 });
  h.send(viaRows, a.id, { columns: F.columns, data: F.data, config: vertical, dataRev: 8 });

  const viaRev = h.createEnv();
  const b = h.mount(viaRev);
  h.send(viaRev, b.id, { columns: F.columns, data: F.data, config, dataRev: 7 });
  viaRev.handlers['drilldown-data'](dataless(viaRev, b.id, vertical, 7));

  const last = (env) => {
    const log = env.fake.instances.at(-1).log.filter((e) => e.op === 'setOption');
    return log.at(-1);
  };
  assert.deepStrictEqual(last(viaRev), last(viaRows));
  assert.deepStrictEqual(h.domSummary(b.block), h.domSummary(a.block));
  viaRows.close();
  viaRev.close();
});

test('a message without rows merges into a payload waiting for its container', () => {
  const env = h.createEnv();
  const id = 'block_pending-drilldown_block';
  h.send(env, id, { columns: F.columns, data: F.data, config, dataRev: 3 });
  env.handlers['drilldown-data'](
    dataless(env, id, { ...config, orientation: 'vertical' }, 3));

  const { block } = h.mount(env, id);
  assert.strictEqual(block.data.length, F.data[Object.keys(F.data)[0]].length);
  assert.strictEqual(block.config.orientation, 'vertical');
  assert.strictEqual(block._lastDataRev, 3);
  assert.deepStrictEqual(needs(env, id), []);
  assert.deepStrictEqual(env.inputs().filter((i) => i.name === id + '_ready'), []);
  env.close();
});

test('a client without the rows asks for them once, and draws nothing until they come', () => {
  const env = h.createEnv();
  const { block, id } = h.mount(env);
  const before = draws(env);

  env.handlers['drilldown-data'](dataless(env, id, config, 5));
  env.handlers['drilldown-data'](
    dataless(env, id, { ...config, orientation: 'vertical' }, 5));

  assert.strictEqual(draws(env), before);
  const asked = needs(env, id);
  assert.strictEqual(asked.length, 1);
  assert.strictEqual(asked[0].value, 5);
  assert.deepStrictEqual(asked[0].opts, { priority: 'event' });

  // R's answer: the whole message.
  h.send(env, id, { columns: F.columns, data: F.data, config, dataRev: 5 });
  assert.ok(draws(env) > before);
  assert.strictEqual(block._lastDataRev, 5);
  env.close();
});

test('a container that binds to a message without rows asks for them', () => {
  const env = h.createEnv();
  const id = 'block_remount-drilldown_block';
  env.handlers['drilldown-data'](dataless(env, id, config, 9));
  const { block } = h.mount(env, id);
  assert.strictEqual(needs(env, id).length, 1);
  assert.strictEqual(needs(env, id)[0].value, 9);
  assert.strictEqual((block.data || []).length, 0);
  env.close();
});

test('the reply to _ready does not redraw a client that got the push', () => {
  const env = h.createEnv();
  const { id } = h.mount(env);
  assert.strictEqual(env.inputs().filter((i) => i.name === id + '_ready').length, 1);
  // The push that was in flight when the client announced itself...
  h.send(env, id, { columns: F.columns, data: F.data, config, dataRev: 4 });
  const before = draws(env);
  // ...and R's reply to the announce.
  env.handlers['drilldown-ready'](env.win.JSON.parse(JSON.stringify({ id, data_rev: 4 })));
  assert.strictEqual(draws(env), before);
  assert.deepStrictEqual(needs(env, id), []);
  env.close();
});

test('the reply to _ready makes a client without the rows ask for them', () => {
  const env = h.createEnv();
  const { id } = h.mount(env);
  env.handlers['drilldown-ready'](env.win.JSON.parse(JSON.stringify({ id, data_rev: 4 })));
  assert.strictEqual(needs(env, id).length, 1);
  assert.strictEqual(needs(env, id)[0].value, 4);
  env.close();
});
