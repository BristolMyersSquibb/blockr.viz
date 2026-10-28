'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const { JS_DIR } = require('./load');

const FILE = path.join(JS_DIR, 'drilldown-theme-register.js');
const run = (ctx) => vm.runInContext(fs.readFileSync(FILE, 'utf8'), vm.createContext(ctx),
                                     { filename: FILE });

test('the blockr theme is registered with echarts', () => {
  const themes = {};
  const ctx = { echarts: { registerTheme: (name, t) => { themes[name] = t; } } };
  ctx.window = ctx;
  run(ctx);
  assert.deepEqual(Object.keys(themes), ['blockr']);
  assert.equal(themes.blockr.color.length, 6);
  assert.equal(themes.blockr.color[0], '#0072B2');
});

test('without echarts, or an echarts without registerTheme, nothing happens', () => {
  const none = {};
  none.window = none;
  assert.doesNotThrow(() => run(none));
  const old = { echarts: {} };
  old.window = old;
  assert.doesNotThrow(() => run(old));
});
