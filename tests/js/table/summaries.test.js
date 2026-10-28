'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadTable } = require('./load');

const COLS = [{ name: 'AGE', class: 'number', label: 'Age' }, { name: 'SEX', class: 'character', label: 'Sex' }];

/** The Columns editor with a recording stand-in for the gear's controls. */
function editor(summaries) {
  const built = [];
  const env = loadTable({ before: (win) => {
    win.Blockr.viz.gear = { ...win.Blockr.viz.gear,
                            controls: { ...win.Blockr.viz.gear.controls, build: (el, ctx) => built.push(ctx) } };
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
  assert.deepEqual(text('.vz-table-sum-name'), ['Summary', 'Age', 'Summary']);
  assert.deepEqual(text('.vz-table-sum-desc'), ['mean of AGE', 'distribution of AGE',
                                            'count of rows, split by SEX, by ARM']);
  assert.equal(e.el.querySelectorAll('.vz-table-sum-chev').length, 3);
  assert.equal(e.built.length > 0, true);
  assert.ok(Array.from(e.el.querySelectorAll('.vz-table-sum-body')).every((b) => b.hidden));
});

test('a lone column starts open; the words for other types', () => {
  const e = editor([{ type: 'field', col: 'SEX' }]);
  assert.equal(e.el.querySelector('.vz-table-sum-body').hidden, false);
  assert.equal(e.el.querySelectorAll('.vz-table-sum-up, .vz-table-sum-down').length, 0);
  const d = e.env.V.table.summaries.describe;
  assert.equal(d({ type: 'field', col: 'SEX' }), 'values of SEX');
  assert.equal(d({ type: 'expr', expr: 'n()' }), 'n()');
  assert.equal(d({ type: 'sparkline', col: 'AVAL' }), 'sparkline of AVAL');
  assert.equal(d({ type: 'simple', func: 'count_distinct', col: 'USUBJID' }), 'distinct count of USUBJID');
});

test('an entry\'s fields come from its type, with the spec\'s rules', () => {
  const e = editor([{ type: 'simple', func: 'count', show: 'bar' }]);
  // The mark first: it decides the fields that follow. The name last.
  assert.deepEqual(e.built.map((c) => c.key), ['show', 'facet', 'func', 'color', 'name']);
  e.draw([{ type: 'simple', func: 'mean', show: 'tile', color: 'SEX' }]);
  assert.deepEqual(e.built.map((c) => c.key), ['show', 'facet', 'func', 'col', 'fill', 'color', 'name']);
  e.draw([{ type: 'dist', col: 'AGE', show: 'text' }]);
  assert.deepEqual(e.built.map((c) => c.key), ['show', 'facet', 'col', 'summary', 'color', 'name']);
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
  e.click(e.el.querySelectorAll('.vz-table-sum-down')[0]);
  e.click(e.el.querySelectorAll('.vz-table-sum-up')[0]);
  e.click(e.el.querySelectorAll('.vz-table-sum-rm')[1]);
  const add = e.el.querySelector('.vz-table-sum-addcol select');
  // blockr.ui's Select (here the test's stand-in) keys its options by label.
  assert.deepEqual(Array.from(add.options).map((o) => o.value).slice(0, 3), ['', 'Summary', 'Distribution']);
  add.value = 'Pair';
  add.dispatchEvent(new e.env.win.Event('change'));
  // The first entry moves only down, the last only up.
  assert.equal(e.el.querySelectorAll('.vz-table-sum-up').length, 1);
  assert.deepEqual(e.sets, [
    [{ type: 'expr', expr: 'n()' }, { type: 'simple' }],
    [{ type: 'expr', expr: 'n()' }, { type: 'simple' }],
    [{ type: 'simple' }],
    [{ type: 'simple' }, { type: 'expr', expr: 'n()' }, { type: 'pair' }]
  ]);
  const toggle = e.el.querySelector('.vz-table-sum-toggle');
  assert.equal(toggle.getAttribute('aria-expanded'), 'false');
  e.click(toggle);
  assert.equal(e.el.querySelector('.vz-table-sum-body').hidden, false);
  assert.equal(toggle.getAttribute('aria-expanded'), 'true');
  e.draw(list);
  assert.equal(e.el.querySelector('.vz-table-sum-body').hidden, false);
  assert.equal(e.el.querySelector('.vz-table-sum-toggle').getAttribute('aria-expanded'), 'true');
});

test('Show takes the editor\'s width; each entry is headed by its type\'s drawing', () => {
  const e = editor([{ type: 'simple', func: 'count', show: 'bar' }, { type: 'pair', x: 'AGE', xend: 'AGE' }]);
  const show = e.built.find((c) => c.key === 'show');
  assert.equal(show.arg.gear.control, 'segmented');
  assert.ok(show.arg.gear.icons);
  const row = e.el.querySelector('.vz-table-sum-field--wide');
  assert.equal(row.querySelector('.blockr-label').textContent, show.label);
  const heads = Array.from(e.el.querySelectorAll('.vz-table-sum-icon'));
  assert.equal(heads.length, 2);
  // The types' own drawings: three bars for Summary, a line and two diamonds for Pair.
  assert.equal(heads[0].querySelectorAll('svg[viewBox="0 0 24 24"] rect').length, 3);
  assert.equal(heads[1].querySelectorAll('svg[viewBox="0 0 24 24"] path[fill="none"]').length, 3);
});

test('"+ Add a column" opens blockr.ui\'s menu: a row per type, its drawing and its line', () => {
  const opened = [];
  const env = loadTable({ before: (win) => {
    win.Blockr.menu = (anchor, cfg) => {
      const el = win.document.createElement('div');
      for (const it of cfg.items) {
        const row = el.appendChild(win.document.createElement('button'));
        if (it.divider) { row.className = 'blockr-menu__divider'; continue; }
        row.className = 'blockr-menu__item';
        row.innerHTML = '<span class="blockr-menu__icon">' + (it.icon || '') + '</span>' +
          '<span class="blockr-menu__label">' + it.label + '</span>';
        row.onclick = it.onSelect;
      }
      const m = { el, closed: 0, close: () => { m.closed += 1; cfg.onClose(); } };
      opened.push(m);
      return m;
    };
  } });
  const sets = [];
  const el = env.win.document.createElement('div');
  env.V.table.editors['groups.summaries'](el, {
    values: { layout: 'groups', summaries: [{ type: 'simple' }] }, columns: COLS, scope: new env.V.Scope(),
    set: (v) => sets.push(JSON.parse(JSON.stringify(v)))
  });
  assert.equal(el.querySelector('.vz-table-sum-addcol select'), null);
  const btn = el.querySelector('.vz-table-sum-add');
  assert.equal(btn.textContent, '+ Add a column');
  btn.click();
  const m = opened[0];
  assert.ok(m.el.classList.contains('vz-table-sum-menu'));
  const rows = Array.from(m.el.querySelectorAll('.blockr-menu__item'));
  assert.equal(rows.length, 8);
  assert.equal(m.el.querySelectorAll('.blockr-menu__divider').length, 2);
  assert.match(rows[1].querySelector('.blockr-menu__icon').innerHTML, /^<svg/);
  assert.equal(rows[1].querySelector('.vz-table-sum-about').textContent, 'Spread of a number: box or dot range');
  // Again: the open menu closes.
  btn.click();
  assert.equal(m.closed, 1);
  assert.equal(opened.length, 1);
  btn.click();
  opened[1].el.querySelectorAll('.blockr-menu__item')[5].click();
  assert.deepEqual(sets, [[{ type: 'simple' }, { type: 'pair' }]]);
});
