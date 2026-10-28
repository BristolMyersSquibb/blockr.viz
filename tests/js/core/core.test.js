'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { Window } = require('happy-dom');
const { loadCore } = require('./load');

test('the core defines Blockr.viz and nothing else', () => {
  const blank = new Window();
  const before = new Set(Object.keys(blank));
  blank.happyDOM.close();
  const { win, V } = loadCore({ shiny: false });
  const added = Object.keys(win).filter((k) => !before.has(k));
  assert.deepEqual(added.sort(), ['Blockr']);
  assert.deepEqual(Object.keys(V).sort(),
                   ['Scope', 'binding', 'capture', 'captureReply', 'downloads', 'gear', 'scriptBand', 'spec', 'titles',
                    'util']);
});

test('loading the core twice keeps the namespace', () => {
  const { win, V } = loadCore();
  V.spec.chart = { type: {} };
  const fs = require('node:fs');
  const path = require('node:path');
  win.eval(fs.readFileSync(path.join(__dirname, '..', '..', '..', 'inst', 'js',
                                     'core', 'namespace.js'), 'utf8'));
  assert.equal(win.Blockr.viz, V);
  assert.ok(win.Blockr.viz.spec.chart);
});

test('esc makes text safe for content and attributes', () => {
  const { V } = loadCore();
  assert.equal(V.util.esc('<a href="x">\'&\'</a>'),
               '&lt;a href=&quot;x&quot;&gt;&#39;&amp;&#39;&lt;/a&gt;');
  assert.equal(V.util.esc(null), '');
  assert.equal(V.util.esc(3), '3');
});

test('send types each input for its R handler', () => {
  const { V, sent } = loadCore();
  assert.equal(V.util.send('b1-chart', 'set', { rev: 2, values: {} }, 'chart'), true);
  V.util.send('b1-chart', 'select', { selection: null, nonce: 1 });
  V.util.send('b1-chart', 'capture', { png: 'x' });
  assert.deepEqual(sent.map((s) => s.name), [
    'b1-chart_set:blockr.viz.chart',
    'b1-chart_select:blockr.viz.selection',
    'b1-chart_capture:blockr.viz.capture'
  ]);
  assert.ok(sent.every((s) => s.opts.priority === 'event'));
});

test('send does nothing without Shiny or an id', () => {
  const { V } = loadCore({ shiny: false });
  assert.equal(V.util.send('x', 'select', {}), false);
  const { V: V2, sent } = loadCore();
  assert.equal(V2.util.send('', 'select', {}), false);
  assert.equal(sent.length, 0);
});
