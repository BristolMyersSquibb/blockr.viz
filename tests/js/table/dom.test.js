'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadTable, payload, flatModel, table1, nested, tick } = require('./load');

/** Mount the table view with one payload. */
function mount(p, env = loadTable()) {
  const el = env.add();
  env.b.renderValue(el, p);
  const $ = (sel) => el.querySelector(sel);
  const $$ = (sel) => Array.from(el.querySelectorAll(sel));
  const click = (node, init = {}) => node.dispatchEvent(
    new env.win.MouseEvent('click', Object.assign({ bubbles: true }, init)));
  const stubs = () => $$('tbody tr.blockr-data-row').map((r) => r.children[0].textContent);
  const search = async (q) => {
    const inp = $('input.blockr-search');
    inp.value = q;
    inp.dispatchEvent(new env.win.Event('input', { bubbles: true }));
    await tick(env.win, 200);
  };
  return Object.assign(env, { el, $, $$, click, stubs, search });
}

test('a flat table draws its header, rows and row count', () => {
  const t = mount(payload({ data: flatModel(5) }));
  assert.deepEqual(t.$$('thead th').map((th) => th.querySelector('.blockr-col-name').textContent),
                   ['USUBJID', 'Arm', 'Age']);
  assert.equal(t.$('thead th[data-col-index="2"] .blockr-col-label').textContent, 'years');
  assert.deepEqual(t.stubs(), ['S1', 'S2', 'S3', 'S4', 'S5']);
  assert.ok(t.$('thead th[data-col-index="2"]').classList.contains('dt-col-num'));
  assert.equal(t.$('.dt-row-count').textContent, '5 rows');
});

test('missing values draw a dash, and text is escaped', () => {
  const m = flatModel(3, { na: true });
  m.cols[0].disp[0] = '<b>x</b>';
  const t = mount(payload({ data: m }));
  const first = t.$('tbody tr.blockr-data-row');
  assert.equal(first.children[0].innerHTML, '&lt;b&gt;x&lt;/b&gt;');
  const second = t.$$('tbody tr.blockr-data-row')[1];
  assert.ok(second.children[1].classList.contains('blockr-dash'));
});

test('cell fills, ink and data bars become styles', () => {
  const m = flatModel(2);
  m.cols[2].bg = ['#ff0000', null];
  m.cols[2].fg = ['#ffffff', null];
  m.cols[2].bar = [null, 0.5];
  m.cols[2].bar_fill = '#00f';
  const t = mount(payload({ data: m }));
  const [a, b] = t.$$('tbody tr.blockr-data-row').map((r) => r.children[2].getAttribute('style'));
  assert.match(a, /background:#ff0000;color:#ffffff/);
  assert.match(b, /linear-gradient\(90deg,#00f 0 50%,transparent 50%\)/);
});

test('a header click sorts up, down, then back to the data order', () => {
  const t = mount(payload({ data: flatModel(5) }));
  const th = () => t.$('th[data-col-index="2"]');
  const ages = () => t.$$('tbody tr.blockr-data-row').map((r) => Number(r.children[2].textContent));
  const data = ages();
  t.click(th());
  assert.deepEqual(ages(), data.slice().sort((x, y) => x - y));
  assert.equal(th().getAttribute('aria-sort'), 'ascending');
  t.click(th());
  assert.deepEqual(ages(), data.slice().sort((x, y) => y - x));
  assert.ok(th().querySelector('.blockr-sort-icon-desc'));
  t.click(th());
  assert.deepEqual(ages(), data);
  assert.equal(th().getAttribute('aria-sort'), 'none');
});

test('Enter on a header sorts; text sorts by locale', () => {
  const t = mount(payload({ data: flatModel(4) }));
  const th = t.$('th[data-col-index="1"]');
  th.dispatchEvent(new t.win.KeyboardEvent('keydown', { key: 'Enter', bubbles: true }));
  assert.deepEqual(t.$$('tbody tr.blockr-data-row').map((r) => r.children[1].textContent),
                   ['High', 'Low', 'Placebo', 'Placebo']);
});

test('without header sorting the headers do not sort', () => {
  const t = mount(payload({ data: flatModel(4), config: { sortable: false } }));
  assert.equal(t.$$('th.blockr-sortable').length, 0);
  t.click(t.$$('thead th')[2]);
  assert.deepEqual(t.stubs(), ['S1', 'S2', 'S3', 'S4']);
});

test('search narrows the rows and the count, and says when nothing matches', async () => {
  const t = mount(payload({ data: flatModel(6), config: { drill: 'ARM' } }));
  await t.search('placebo');
  assert.deepEqual(t.stubs(), ['S1', 'S4']);
  assert.equal(t.$('.dt-row-count').textContent, '2 of 6 rows');
  await t.search('nothing');
  assert.equal(t.$('.blockr-search-empty').textContent, 'No matching rows');
  await t.search('');
  assert.equal(t.$('.blockr-search-empty'), null);
  assert.equal(t.$('.dt-row-count').textContent, '6 rows');
});

test('turning search off hides the box and drops the query', async () => {
  const env = loadTable();
  const t = mount(payload({ data: flatModel(6), dataRev: 1 }), env);
  await t.search('placebo');
  t.b.renderValue(t.el, payload({ config: { search: false }, dataRev: 1 }));
  assert.ok(t.$('input.blockr-search').hidden);
  assert.equal(t.stubs().length, 6);
});

test('a long table draws only a window of rows and follows the scroll', async () => {
  const t = mount(payload({ data: flatModel(2000) }));
  const box = t.$('.blockr-table-wrapper');
  const drawn = () => t.$$('tbody tr.blockr-data-row');
  assert.ok(drawn().length < 200, 'windowed: ' + drawn().length);
  assert.equal(t.$$('tbody tr.dt-vspacer').length, 1);
  assert.equal(t.$('.dt-row-count').textContent, '2000 rows');
  Object.defineProperty(box, 'clientHeight', { value: 600, configurable: true });
  box.scrollTop = 30 * 1000;
  box.dispatchEvent(new t.win.Event('scroll'));
  await tick(t.win, 40);
  const first = Number(drawn()[0].getAttribute('data-i'));
  assert.ok(first > 900 && first < 1000, 'window starts at ' + first);
  assert.equal(t.$$('tbody tr.dt-vspacer').length, 2);
});

test('sorting a long table sorts all of it, not the window', () => {
  const t = mount(payload({ data: flatModel(1000) }));
  t.click(t.$('th[data-col-index="2"]'));
  t.click(t.$('th[data-col-index="2"]'));
  assert.equal(t.$('tbody tr.blockr-data-row').children[2].textContent, '69');
});

test('sections head their rows; a click on the header folds it', () => {
  const t = mount(payload({ data: table1() }));
  const heads = () => t.$$('tr.blockr-section-header').map((r) => r.textContent);
  assert.deepEqual(heads(), ['Sex', 'Age', 'Age group']);
  assert.equal(t.$('.dt-row-count').textContent, '');
  assert.ok(t.$('.vz-table').classList.contains('drilldown-table-structured'));
  t.click(t.$('tr.blockr-section-header .blockr-chev'));
  assert.deepEqual(t.stubs(), ['Mean (SD)', '<65', '>=65']);
  assert.ok(t.$('tr.blockr-section-header').classList.contains('collapsed'));
  t.click(t.$('tr.blockr-section-header .blockr-section-value'));
  assert.equal(t.stubs().length, 5);
});

test('alt-click and the corner button fold every section', () => {
  const t = mount(payload({ data: table1() }));
  t.click(t.$('tr.blockr-section-header'), { altKey: true });
  assert.equal(t.stubs().length, 0);
  assert.equal(t.$('.dt-foldall').getAttribute('aria-label'), 'Expand all groups');
  t.click(t.$('.dt-foldall'));
  assert.equal(t.stubs().length, 5);
  t.click(t.$('.dt-foldall'));
  assert.equal(t.stubs().length, 0);
});

test('without collapsible the sections are plain labels', () => {
  const t = mount(payload({ data: table1(), config: { collapsible: false } }));
  assert.equal(t.$('.blockr-section-btn-static') != null, true);
  assert.equal(t.$('.dt-foldall'), null);
  t.click(t.$('tr.blockr-section-header .blockr-chev'));
  assert.equal(t.stubs().length, 5);
});

test('a table that starts collapsed starts folded', () => {
  const m = table1();
  m.collapsed = true;
  const t = mount(payload({ data: m }));
  assert.equal(t.stubs().length, 0);
});

test('spanners span their leaves, and the stub header spans the header rows', () => {
  const t = mount(payload({ data: table1() }));
  const top = t.$$('thead tr')[0].children;
  assert.equal(top[0].getAttribute('rowspan'), '2');
  assert.equal(top[1].getAttribute('colspan'), '2');
  assert.ok(top[1].classList.contains('group'));
  assert.equal(t.$$('thead tr')[1].children[0].querySelector('.arm__n').textContent, 'N = 22');
});

test('nested sections: both levels, search keeps the headers of what matches', async () => {
  const t = mount(payload({ data: nested() }));
  const heads = () => t.$$('tr.blockr-section-header').map((r) => r.querySelector('.blockr-section-value').textContent);
  assert.deepEqual(heads(), ['Placebo', 'F', 'M', 'Active', 'F']);
  assert.equal(t.$('tr.blockr-section-header .blockr-section-label').textContent, 'Arm: ');
  await t.search('d');
  assert.deepEqual(heads(), ['Active', 'F']);
  assert.deepEqual(t.stubs(), ['d']);
});

test('sorting keeps rows inside their sections', () => {
  const t = mount(payload({ data: nested() }));
  t.click(t.$('th[data-col-index="1"]'));
  t.click(t.$('th[data-col-index="1"]'));
  const order = t.$$('tbody tr').map((r) => r.classList.contains('blockr-section-header')
    ? '#' + r.querySelector('.blockr-section-value').textContent : r.children[0].textContent);
  assert.deepEqual(order, ['#Placebo', '#F', 'b', 'a', '#M', 'c', '#Active', '#F', 'd']);
});

test('a folded outer section hides its inner headers', () => {
  const t = mount(payload({ data: nested() }));
  t.click(t.$('tr.blockr-section-header[data-level="1"] .blockr-chev'));
  const heads = t.$$('tr.blockr-section-header').map((r) => r.querySelector('.blockr-section-value').textContent);
  assert.deepEqual(heads, ['Placebo', 'Active', 'F']);
});

test('an indented row folds the rows under it', () => {
  const m = {
    n: 4, stub: 0,
    cols: [{ name: '.label', disp: ['Cardiac', 'Angina', 'Arrhythmia', 'Skin'] },
           { name: 'n', num: true, disp: ['5', '3', '2', '1'] }],
    head: [[{ text: '', stub: true }, { text: 'n', col: 1 }]],
    rows: { indent: [0, 1, 1, 0], strong: [true, false, false, true] }
  };
  const t = mount(payload({ data: m }));
  const parent = t.$('tr.blockr-indent-toggle');
  assert.ok(parent.classList.contains('blockr-bold'));
  assert.match(t.$$('tbody tr')[1].children[0].getAttribute('style'), /padding-left:calc\(40px/);
  t.click(parent.querySelector('.blockr-indent-btn'));
  assert.deepEqual(t.stubs(), ['Cardiac', 'Skin']);
  t.click(t.$('tr.blockr-indent-toggle .blockr-indent-btn'));
  assert.equal(t.stubs().length, 4);
});

test('a bold stub header keeps its class; leaves without sorting are not sortable', () => {
  const m = table1();
  m.head[0][0].bold = true;
  const t = mount(payload({ data: m, config: { sortable: false } }));
  assert.ok(t.$('th.blockr-stub-header').classList.contains('blockr-stub-header-bold'));
  assert.equal(t.$$('th.blockr-sortable').length, 0);
});

test('column widths make a fixed layout', () => {
  const m = flatModel(2);
  m.widths = [80, 120, 60];
  const t = mount(payload({ data: m }));
  assert.deepEqual(t.$$('colgroup col').map((c) => c.getAttribute('style')),
                   ['width:80px', 'width:120px', 'width:60px']);
});

test('parseNum reads the first number of a display text', () => {
  const { V } = loadTable();
  assert.equal(V.table.parseNum('1,234.5 kg'), 1234.5);
  assert.equal(V.table.parseNum('-3'), -3);
  assert.equal(V.table.parseNum('n/a'), null);
  assert.equal(V.table.parseNum(null), null);
});
