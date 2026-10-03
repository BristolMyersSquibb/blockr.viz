/* The busy cue (busy-cue.js): R's "blockr-busy" dims a chart after 300 ms;
 * the chart ends it once the new picture is drawn, and R's "blockr-busy-done"
 * ends it when no picture is coming. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const h = require('./harness');
const { F, aggregated } = require('./cases');

const config = aggregated.find((c) => c.name === 'bar-group').config;
const busy = (el) => el.classList.contains('blockr-busy');

test('a slow draw shows the cue, and the drawn picture ends it', () => {
  const env = h.createEnv();
  const { el, id } = h.mount(env);
  h.send(env, id, { columns: F.columns, data: F.data, config, dataRev: 1 });
  env.flushTimers(500);

  env.handlers['blockr-busy']({ id, label: 'drawing' });
  assert.ok(!busy(el), 'nothing in the first 300 ms');
  env.flushTimers(1);
  assert.ok(busy(el));
  assert.match(el.querySelector('.blockr-busy-line').textContent, /^drawing… \d+\.\d s$/);

  h.send(env, id, { columns: F.columns, data: F.data,
                    config: { ...config, orientation: 'vertical' }, dataRev: 2 });
  env.flushTimers(500);
  assert.ok(!busy(el));
  env.close();
});

test('a message without rows keeps the cue until the rows arrive', () => {
  const env = h.createEnv();
  const { el, id } = h.mount(env);
  env.handlers['blockr-busy']({ id, label: 'drawing' });
  env.handlers['drilldown-data'](env.win.JSON.parse(JSON.stringify({
    id, columns: F.columns, config, arguments: null, data_rev: 5
  })));
  env.flushTimers(500);
  assert.ok(busy(el), 'still waiting for the rows');
  assert.ok(env.inputs().some((i) => i.name === id + '_need'));

  h.send(env, id, { columns: F.columns, data: F.data, config, dataRev: 5 });
  env.flushTimers(500);
  assert.ok(!busy(el));
  env.close();
});

test('R ends the cue when no picture is coming', () => {
  const env = h.createEnv();
  const { el, id } = h.mount(env);
  env.handlers['blockr-busy']({ id, label: 'drawing' });
  env.flushTimers(1);
  assert.ok(busy(el));
  env.handlers['blockr-busy-done']({ id });
  assert.ok(!busy(el));
  env.close();
});
