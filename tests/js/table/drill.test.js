'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadTable, payload, flatModel, table1, nested } = require('./load');

const COLUMNS = [{ name: 'ARM', class: 'character', label: 'Arm', n_unique: 3 },
                 { name: 'SEX', class: 'character', label: 'Sex', n_unique: 2 }];

function mount(p) {
  const env = loadTable();
  const el = env.add();
  env.b.renderValue(el, p);
  const $ = (sel) => el.querySelector(sel);
  const $$ = (sel) => Array.from(el.querySelectorAll(sel));
  const click = (node, init = {}) => node.dispatchEvent(
    new env.win.MouseEvent('click', Object.assign({ bubbles: true }, init)));
  const row = (k) => $$('tbody tr.vz-table-row')[k];
  const lit = () => $$('tbody tr.vz-table-row-active').map((r) => Number(r.getAttribute('data-i')));
  const render = (q) => env.b.renderValue(el, payload(q));
  return Object.assign(env, { el, $, $$, click, row, lit, render });
}

const cat = (filters, columns) => Object.assign({ type: 'categorical', filters },
                                                columns ? { columns } : {});

test('a row click claims the drill value and lights every row with it', () => {
  const t = mount(payload({ data: flatModel(6), columns: COLUMNS }));
  t.click(t.row(3).children[2]);
  assert.deepEqual(t.selects(), [cat({ ARM: ['Placebo'] })]);
  assert.equal(t.sent[0].name, 'tbl_select:blockr.viz.selection');
  assert.equal(typeof t.sent[0].value.nonce, 'number');
  assert.deepEqual(t.lit(), [0, 3]);
  assert.equal(t.$('.vz-status').textContent, '', 'the line waits for R');
  assert.ok(t.$('.vz-table').classList.contains('vz-table-clickable'));
});

test("the selection's line is R's, and appears once when R holds it", () => {
  const t = mount(payload({ data: flatModel(6), columns: COLUMNS, dataRev: 7 }));
  const texts = [];
  const line = () => { const e = t.$('.vz-status-text'); return e ? e.textContent : ''; };
  new t.win.MutationObserver(() => texts.push(line()))
    .observe(t.$('.vz-status'), { childList: true, subtree: true, characterData: true });
  t.click(t.row(3).children[2]);
  // R echoes the selection with its own words (R/table-records.R), which
  // can name what the browser cannot: here a column label it does not hold.
  t.render({ dataRev: 7, columns: COLUMNS, config: { selection: cat({ ARM: ['Placebo'] }) },
             derived: { receipt: 'Filtered: Planned arm = Placebo' } });
  return new Promise((resolve) => setTimeout(resolve, 0)).then(() => {
    assert.equal(t.$('.vz-status-text').textContent, 'Filtered: Planned arm = Placebo');
    assert.deepEqual([...new Set(texts.filter(Boolean))], ['Filtered: Planned arm = Placebo'],
                     'one text, never another first');
    assert.equal(t.$('.vz-status-clear').hidden, false);
  });
});

test('a second click on a lit row clears the drill', () => {
  const t = mount(payload({ data: flatModel(6) }));
  t.click(t.row(0));
  t.click(t.row(3));
  assert.deepEqual(t.selects(), [cat({ ARM: ['Placebo'] }), null]);
  assert.deepEqual(t.lit(), []);
  assert.equal(t.$('.vz-status').textContent, '');
});

test('the clear button on the receipt clears the drill', () => {
  const t = mount(payload({ data: flatModel(6), dataRev: 7 }));
  t.click(t.row(1));
  t.render({ dataRev: 7, config: { selection: cat({ ARM: ['Low'] }) },
             derived: { receipt: 'Filtered: ARM = Low' } });
  t.click(t.$('.vz-status-clear'));
  assert.deepEqual(t.selects(), [cat({ ARM: ['Low'] }), null]);
  assert.deepEqual(t.lit(), []);
});

test('a row without a drill value does not click', () => {
  const t = mount(payload({ data: flatModel(4, { na: true }) }));
  assert.ok(t.row(1).classList.contains('vz-table-row-nodrill'));
  t.click(t.row(1));
  assert.deepEqual(t.selects(), []);
});

test('without a drill column nothing clicks or looks clickable', () => {
  const t = mount(payload({ data: flatModel(4, { drill: null }) }));
  t.click(t.row(0));
  assert.deepEqual(t.selects(), []);
  assert.equal(t.$$('tr.vz-table-row-nodrill').length, 0);
  assert.equal(t.$('.vz-table').classList.contains('vz-table-clickable'), false);
});

test("R's selection lights its rows and writes R's receipt", () => {
  const t = mount(payload({ data: flatModel(6), config: { selection: cat({ ARM: ['Low'] }) },
                            derived: { receipt: 'Filtered: Arm = Low dose' } }));
  assert.deepEqual(t.lit(), [1, 4]);
  assert.equal(t.$('.vz-status-text').textContent, 'Filtered: Arm = Low dose');
});

test('a payload with the same rows keeps the sort and shows the new selection', () => {
  const t = mount(payload({ data: flatModel(6), dataRev: 7 }));
  t.click(t.$('th[data-col-index="2"]'));
  const order = t.$$('tbody tr.vz-table-row').map((r) => r.getAttribute('data-i'));
  t.render({ dataRev: 7, config: { selection: cat({ ARM: ['High'] }) } });
  assert.deepEqual(t.$$('tbody tr.vz-table-row').map((r) => r.getAttribute('data-i')), order);
  assert.deepEqual(t.lit().sort(), [2, 5]);
});

test('Table 1: a row claims its level, a cell adds its column', () => {
  const t = mount(payload({ data: table1(), columns: COLUMNS }));
  t.click(t.row(0).children[0]);
  t.click(t.row(1).children[2]);
  assert.deepEqual(t.selects(), [
    cat({ '.variable': ['SEX'], '.variable_level': ['F'] }),
    cat({ '.variable': ['SEX'], '.variable_level': ['M'] }, { ARM: ['Active'] })
  ]);
  assert.deepEqual(t.lit(), [1]);
  assert.deepEqual(t.$$('th.vz-table-col-active').map((e) => e.getAttribute('data-h')), ['1,1']);
  assert.deepEqual(t.$$('td.vz-table-col-active').map((e) => e.textContent),
                   ['11', '9', '72 (9)', '8', '12']);
  assert.equal(t.$('.vz-status').textContent, '', 'the line waits for R');
});

test('Table 1: a stat row is inert', () => {
  const t = mount(payload({ data: table1() }));
  assert.ok(t.row(2).classList.contains('vz-table-row-nodrill'));
  t.click(t.row(2).children[1]);
  assert.deepEqual(t.selects(), []);
});

test('Table 1: the same cell again clears; the stub then claims the row alone', () => {
  const t = mount(payload({ data: table1() }));
  t.click(t.row(0).children[1]);
  t.click(t.row(0).children[0]);
  t.click(t.row(0).children[0]);
  const lvl = { '.variable': ['SEX'], '.variable_level': ['F'] };
  assert.deepEqual(t.selects(), [cat(lvl, { ARM: ['Placebo'] }), cat(lvl), null]);
});

test('a keyed header claims its column; its arrow still sorts', () => {
  const t = mount(payload({ data: table1() }));
  const th = t.$('th[data-col-index="1"]');
  t.click(th.querySelector('.blockr-col-name'));
  assert.deepEqual(t.selects(), [cat({}, { ARM: ['Placebo'] })]);
  assert.ok(t.$('th[data-col-index="1"]').classList.contains('vz-table-col-active'));
  assert.equal(t.$('th[data-col-index="1"]').getAttribute('aria-sort'), 'none');
  t.click(t.$('th[data-col-index="1"] .blockr-sort-icon'));
  assert.equal(t.selects().length, 1);
  assert.equal(t.$('th[data-col-index="1"]').getAttribute('aria-sort'), 'ascending');
});

test('a spanner claim lights the leaves under it', () => {
  const m = table1();
  m.head[0][1].keys = { ARM: ['Placebo'] };
  const t = mount(payload({ data: m, config: { selection: cat({}, { ARM: ['Placebo'] }) } }));
  const active = t.$$('th.vz-table-col-active').map((th) => th.getAttribute('data-h'));
  assert.deepEqual(active, ['0,1', '1,0']);
});

test('a keyed section value claims the section; its chevron still folds', () => {
  const t = mount(payload({ data: nested(), columns: COLUMNS }));
  const sec = t.$$('tr.vz-table-section-header')[1];
  t.click(sec.querySelector('.vz-table-section-value'));
  assert.deepEqual(t.selects(), [cat({ ARM: ['Placebo'], SEX: ['F'] })]);
  assert.ok(t.$$('tr.vz-table-section-header')[1].classList.contains('vz-table-row-active'));
  assert.equal(t.$('.vz-status').textContent, '', 'the line waits for R');
  // R echoes a click without a column half as `columns: {}`.
  t.render({ data: nested(), columns: COLUMNS,
             config: { selection: { type: 'categorical', filters: { ARM: ['Placebo'], SEX: ['F'] }, columns: {} } } });
  assert.ok(t.$$('tr.vz-table-section-header')[1].classList.contains('vz-table-row-active'));
  t.click(t.$$('tr.vz-table-section-header')[1].querySelector('.vz-table-chev'));
  assert.equal(t.selects().length, 1);
  assert.ok(t.$$('tr.vz-table-section-header')[1].classList.contains('collapsed'));
});

test('nested records: a row claims its groups and its drill value', () => {
  const m = nested();
  const g = (a, s, id) => ({ ARM: [a], SEX: [s], USUBJID: [id] });
  m.rows = { keys: [g('Placebo', 'F', 'a'), g('Placebo', 'F', 'b'), g('Placebo', 'M', 'c'),
                    g('Active', 'F', 'd')] };
  const t = mount(payload({ data: m }));
  t.click(t.row(2));
  assert.deepEqual(t.selects(), [cat(g('Placebo', 'M', 'c'))]);
  assert.deepEqual(t.lit(), [2]);
});

test('with a drill target every click is sent and nothing stays lit', () => {
  const t = mount(payload({ data: table1(), config: { drill_target: 'blk2' } }));
  t.click(t.row(0).children[1]);
  t.click(t.row(0).children[1]);
  assert.equal(t.selects().length, 2);
  assert.deepEqual(t.selects()[0], t.selects()[1]);
  assert.deepEqual(t.lit(), []);
  assert.ok(t.row(0).classList.contains('vz-table-flash-row'));
  assert.ok(t.row(0).children[1].classList.contains('vz-table-flash-cross'));
  assert.ok(t.row(1).children[1].classList.contains('vz-table-flash-col'));
  assert.match(t.$('.vz-status-receipt').textContent, /^Drilled down to SEX = F, ARM = Placebo/);
  assert.equal(t.$('.vz-status-clear'), null);
  t.row(0).dispatchEvent(new t.win.Event('animationend'));
  assert.equal(t.row(0).classList.contains('vz-table-flash-row'), false);
});

test('hover previews a cell claim: row and column faint, the cell strong', () => {
  const t = mount(payload({ data: table1() }));
  const cell = t.row(0).children[2];
  cell.dispatchEvent(new t.win.MouseEvent('mouseover', { bubbles: true }));
  assert.ok(t.row(0).classList.contains('vz-table-hot-row'));
  assert.ok(cell.classList.contains('vz-table-hot-cell'));
  assert.ok(t.row(1).children[2].classList.contains('vz-table-hot-col'));
  t.$('.blockr-table-wrapper').dispatchEvent(new t.win.MouseEvent('mouseleave'));
  assert.equal(t.$$('.vz-table-hot-row, .vz-table-hot-cell, .vz-table-hot-col').length, 0);
});

test('hover on a keyed header previews its column', () => {
  const t = mount(payload({ data: table1() }));
  t.$('th[data-col-index="2"] .blockr-col-name')
    .dispatchEvent(new t.win.MouseEvent('mouseover', { bubbles: true }));
  assert.equal(t.$$('td.vz-table-hot-col').length, 5);
});

test('the drill helpers compare claims', () => {
  const { V } = loadTable();
  const d = V.table.drill;
  assert.ok(d.equal({ a: ['1'], b: [null] }, { b: [null], a: ['1'] }));
  assert.equal(d.equal({ a: ['1'] }, { a: ['1'], b: ['2'] }), false);
  assert.ok(d.covers({ ARM: ['P'] }, { ARM: ['P'], SEX: ['F'] }));
  assert.equal(d.covers({ ARM: ['P'], SEX: ['F'] }, { ARM: ['P'] }), false);
  assert.ok(d.same(null, null));
  assert.equal(d.label(cat({ '.group1_level': ['Cardiac'], X: [null] }), { X: 'Ex' }),
               'Cardiac, Ex = (Missing)');
});

test('when R drops the selection the rows go dark and the receipt goes', () => {
  const t = mount(payload({ data: flatModel(6), dataRev: 3, config: { selection: cat({ ARM: ['Low'] }) },
                            derived: { receipt: 'Filtered: Arm = Low dose' } }));
  assert.deepEqual(t.lit(), [1, 4]);
  // Same rows (R sends them once per data revision), no selection.
  t.render({ dataRev: 3, config: { selection: null }, derived: { receipt: null } });
  assert.deepEqual(t.lit(), []);
  const status = t.$('.vz-status-text');
  assert.equal(status ? status.textContent : '', '');
  assert.deepEqual(t.selects(), [], 'nothing goes back to R');
});
