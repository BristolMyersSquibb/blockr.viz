'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadTable, payload, table1 } = require('./load');

const VAR = { layout: 'variables', by: ['ARM'] };
const COLS = [{ name: 'AGE', class: 'number', label: 'Age' }, { name: 'SEX', class: 'character', label: 'Sex' },
              { name: 'ARM', class: 'character', label: 'Arm' }];

function mount(p, env = loadTable()) {
  const el = env.add();
  env.b.renderValue(el, p);
  return Object.assign(env, { el, $: (s) => el.querySelector(s), $$: (s) => Array.from(el.querySelectorAll(s)) });
}

test('the variables layout draws its Table 1 with sections and the drill', () => {
  const t = mount(payload({ data: table1(), config: VAR }));
  assert.deepEqual(t.$$('tr.vz-table-section-header').map((r) => r.textContent), ['Sex', 'Age', 'Age group']);
  assert.ok(t.$('.vz-table').classList.contains('vz-table-structured'));
  t.$$('tbody tr.vz-table-row')[0].children[1].dispatchEvent(new t.win.MouseEvent('click', { bubbles: true }));
  assert.deepEqual(t.selects(), [{ type: 'categorical', filters: { '.variable': ['SEX'], '.variable_level': ['F'] },
                                   columns: { ARM: ['Placebo'] } }]);
});

test('the cards look draws a Table 1 as a table', () => {
  const m = table1();
  m.cards = { n: 1, cols: [] };
  const t = mount(payload({ data: m, config: Object.assign({ look: 'cards' }, VAR) }));
  assert.equal(t.$$('tbody tr.vz-table-row').length, 5);
  assert.ok(t.$('.vz-table-cards').hidden);
});

test('a pick of variables becomes entries; kept entries keep their keys', () => {
  const { V } = loadTable();
  const s = [{ type: 'dist', col: 'AGE', summary: 'mean_sd' }, { type: 'simple', color: 'SEX' }];
  assert.equal(V.table.variables.varOf({ type: 'simple', col: 'X' }), 'X');
  assert.equal(V.table.variables.varOf({ type: 'expr', expr: '1' }), null);
  const out = JSON.parse(JSON.stringify(V.table.variables.withVars(s, ['RACE', 'AGE', 'WT'],
    COLS.concat([{ name: 'RACE', class: 'string' }, { name: 'WT', class: 'number' }]))));
  assert.deepEqual(out, [{ type: 'simple', color: 'RACE' }, { type: 'dist', col: 'AGE', summary: 'mean_sd' },
                         { type: 'dist', col: 'WT' }]);
});

test('the gear does not judge a picked layout by what this one draws', () => {
  let host;
  const env = loadTable({ before: (win) => {
    win.Blockr.viz.gear = { ...win.Blockr.viz.gear,
      mount: (tray, o) => { host = o.host; return { update() {}, dispose() {} }; } };
  } });
  mount(payload({ data: table1(), config: VAR }), env);
  // A Table 1 is structured and has no number columns; a list picked in the
  // gear, before R draws it, is neither.
  const v = { layout: 'records', by: [] };
  assert.equal(host.visible('format', v), true);
  assert.equal(host.visible('collapsible', v), false);
  assert.equal(host.visible('color', v), true);
});

test('the gear\'s Variables row picks among the columns not in by', () => {
  let rowCtx;
  let host;
  const env = loadTable({ before: (win) => {
    win.Blockr.viz.gear = { ...win.Blockr.viz.gear,
      mount: (tray, o) => { host = o.host; return { update() {}, dispose() {} }; },
      controls: { build: (el, ctx) => { rowCtx = ctx; } }
    };
  } });
  mount(payload({ data: table1(), config: VAR }), env);
  const sets = [];
  const el = env.win.document.createElement('div');
  host.custom('summaries', el, { values: { layout: 'variables', by: ['ARM'],
                                           summaries: [{ type: 'dist', col: 'AGE' }] },
                                 columns: COLS, scope: new env.V.Scope(), set: (v) => sets.push(v) });
  assert.deepEqual(rowCtx.value, ['AGE']);
  assert.deepEqual(rowCtx.columns.map((c) => c.name), ['AGE', 'SEX']);
  rowCtx.set(['AGE', 'SEX']);
  assert.deepEqual(JSON.parse(JSON.stringify(sets)), [[{ type: 'dist', col: 'AGE' }, { type: 'simple', color: 'SEX' }]]);
  // The layout needs a variable: required, and amber while a row count is all.
  assert.equal(host.required('summaries', { layout: 'variables' }), true);
  assert.equal(host.required('summaries', { layout: 'groups' }), false);
  const amber = (e) => e.classList.contains('blockr-field--required-empty');
  assert.equal(amber(el), false);
  const none = env.win.document.createElement('div');
  host.custom('summaries', none, { values: { layout: 'variables', by: [],
                                             summaries: [{ type: 'simple', func: 'count', name: 'Count' }] },
                                   columns: COLS, scope: new env.V.Scope(), set: () => {} });
  assert.equal(amber(none), true);
  const shade = env.win.document.createElement('div');
  host.custom('shadings', shade, { values: { layout: 'records', shadings: [] }, columns: COLS,
                                   scope: new env.V.Scope(), set: () => {} });
  assert.ok(shade.querySelector('.vz-table-shade-add'));
});
