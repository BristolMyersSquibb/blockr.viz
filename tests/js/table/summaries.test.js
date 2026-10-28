'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadTable } = require('./load');

const COLS = [{ name: 'AGE', class: 'numeric', label: 'Age' }, { name: 'SEX', class: 'character', label: 'Sex' }];

/** The Columns editor with a recording stand-in for the gear's controls. */
function editor(summaries) {
  const built = [];
  const env = loadTable({ before: (win) => {
    win.Blockr.viz.gear = { controls: { build: (el, ctx) => built.push(ctx) } };
  } });
  const sets = [];
  const el = env.win.document.createElement('div');
  const draw = (list) => {
    built.length = 0;
    env.V.table.editors['groups.summaries'](el, {
      values: { layout: 'groups', summaries: list }, columns: COLS, scope: new env.V.Scope(),
      set: (v) => sets.push(JSON.parse(JSON.stringify(v)))
    });
  };
  draw(summaries);
  const click = (node) => node.dispatchEvent(new env.win.MouseEvent('click', { bubbles: true }));
  return { env, el, built, sets, draw, click };
}

test('one entry per summary: its name, what it computes, a chevron', () => {
  const e = editor([{ type: 'simple', func: 'mean', col: 'AGE' }, { type: 'dist', col: 'AGE', name: 'Age' },
                    { type: 'simple', color: 'SEX', facet: 'ARM' }]);
  const text = (cls) => Array.from(e.el.querySelectorAll(cls)).map((x) => x.textContent);
  assert.deepEqual(text('.lane-sum-name'), ['Summary', 'Age', 'Summary']);
  assert.deepEqual(text('.lane-sum-desc'), ['mean of AGE', 'distribution of AGE',
                                            'count of rows, split by SEX, by ARM']);
  assert.equal(e.el.querySelectorAll('.lane-sum-chev').length, 3);
  assert.equal(e.built.length > 0, true);
  assert.ok(Array.from(e.el.querySelectorAll('.lane-sum-body')).every((b) => b.hidden));
});

test('a lone column starts open; the words for other types', () => {
  const e = editor([{ type: 'field', col: 'SEX' }]);
  assert.equal(e.el.querySelector('.lane-sum-body').hidden, false);
  assert.equal(e.el.querySelectorAll('.lane-sum-up, .lane-sum-down').length, 0);
  const d = e.env.V.table.summaries.describe;
  assert.equal(d({ type: 'field', col: 'SEX' }), 'values of SEX');
  assert.equal(d({ type: 'expr', expr: 'n()' }), 'n()');
  assert.equal(d({ type: 'sparkline', col: 'AVAL' }), 'sparkline of AVAL');
  assert.equal(d({ type: 'simple', func: 'count_distinct', col: 'USUBJID' }), 'distinct count of USUBJID');
});

test('an entry\'s fields come from its type, with the spec\'s rules', () => {
  const e = editor([{ type: 'simple', func: 'count', show: 'bar' }]);
  assert.deepEqual(e.built.map((c) => c.key), ['facet', 'show', 'func', 'color', 'name']);
  e.draw([{ type: 'simple', func: 'mean', show: 'tile', color: 'SEX' }]);
  assert.deepEqual(e.built.map((c) => c.key), ['facet', 'show', 'func', 'col', 'fill', 'color', 'name']);
  e.draw([{ type: 'dist', col: 'AGE', show: 'text' }]);
  assert.deepEqual(e.built.map((c) => c.key), ['facet', 'show', 'col', 'summary', 'color', 'name']);
  assert.equal(e.built.find((c) => c.key === 'summary').value, 'median_q1_q3');
});

test('a field edit sends the whole list with that entry changed', () => {
  const e = editor([{ type: 'simple', func: 'count' }, { type: 'field', col: 'SEX' }]);
  e.built.find((c) => c.key === 'func').set('mean');
  assert.deepEqual(e.sets, [[{ type: 'simple', func: 'mean' }, { type: 'field', col: 'SEX' }]]);
});

test('move, remove, add and fold', () => {
  const list = [{ type: 'simple' }, { type: 'expr', expr: 'n()' }];
  const e = editor(list);
  e.click(e.el.querySelectorAll('.lane-sum-down')[0]);
  e.click(e.el.querySelectorAll('.lane-sum-up')[0]);
  e.click(e.el.querySelectorAll('.lane-sum-rm')[1]);
  const add = e.el.querySelector('.lane-sum-addcol select');
  assert.deepEqual(Array.from(add.options).map((o) => o.value).slice(0, 3), ['', 'simple', 'dist']);
  add.value = 'pair';
  add.dispatchEvent(new e.env.win.Event('change'));
  // The first entry moves only down, the last only up.
  assert.equal(e.el.querySelectorAll('.lane-sum-up').length, 1);
  assert.deepEqual(e.sets, [
    [{ type: 'expr', expr: 'n()' }, { type: 'simple' }],
    [{ type: 'expr', expr: 'n()' }, { type: 'simple' }],
    [{ type: 'simple' }],
    [{ type: 'simple' }, { type: 'expr', expr: 'n()' }, { type: 'pair' }]
  ]);
  const toggle = e.el.querySelector('.lane-sum-toggle');
  e.click(toggle);
  assert.equal(e.el.querySelector('.lane-sum-body').hidden, false);
  e.draw(list);
  assert.equal(e.el.querySelector('.lane-sum-body').hidden, false);
});
