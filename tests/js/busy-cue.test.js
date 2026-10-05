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
  env.flushTimers(1);
  assert.ok(!busy(el));
  env.close();
});

test('a done followed at once by a start is one wait', () => {
  const env = h.createEnv();
  const { el, id } = h.mount(env);
  env.handlers['blockr-busy']({ id, label: 'drawing' });
  env.flushTimers(1);
  env.handlers['blockr-busy-done']({ id });
  env.handlers['blockr-busy']({ id, label: 'drawing' });
  env.flushTimers(1);
  assert.ok(busy(el), 'the clock runs on');
  env.close();
});

// A lazily built block: the chart is bound while its panel is hidden, and R
// says nothing until the panel shows and the server has made the block.
const hide = (el) => { el.parentElement.style.visibility = 'hidden'; };
const front = (env, el) => {
  el.parentElement.style.visibility = '';
  env.win.document.dispatchEvent(new env.win.CustomEvent('dockview:active-panel', { bubbles: true }));
};

test('a chart with no picture counts from when its panel shows', () => {
  const env = h.createEnv();
  const { el, id } = h.mount(env);
  hide(el);
  env.flushTimers(500);
  assert.ok(!busy(el), 'hidden: no clock');

  front(env, el);
  env.flushTimers(1);
  env.flushTimers(1);
  assert.ok(busy(el), 'shown: the clock runs before R has said anything');
  assert.match(el.querySelector('.blockr-busy-line').textContent, /^drawing… \d+\.\d s$/);

  env.handlers['blockr-busy']({ id, label: 'drawing' });
  assert.ok(busy(el), "R's start keeps the clock");
  h.send(env, id, { columns: F.columns, data: F.data, config, dataRev: 1 });
  env.flushTimers(500);
  assert.ok(!busy(el), 'the picture ends it');
  env.close();
});

test('a chart R has already spoken about is not watched', () => {
  const env = h.createEnv();
  const id = 'block_spoken-drilldown_block';
  env.handlers['blockr-busy-done']({ id });
  const { el } = h.mount(env, id);
  front(env, el);
  env.flushTimers(500);
  assert.ok(!busy(el));
  env.close();
});
