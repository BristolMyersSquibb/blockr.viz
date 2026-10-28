'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadTable, payload, tick } = require('./load');

const G = { layout: 'groups', by: ['ARM'] };

/** A groups table of three arms, one column per mark kind. */
function groups() {
  const arms = ['Placebo', 'Low', 'High'];
  const claim = (a) => ({ ARM: [a] });
  const col = (name, mark, extra = {}) => Object.assign({
    name, disp: ['10', '20', '30'], key: [10, 20, 30], mark
  }, extra);
  return {
    n: 3, stub: 0,
    cols: [
      { name: 'ARM', disp: arms },
      col('Rows', { kind: 'bar', w: [33, 66, null], fill: '#2563eb', dw: 4 },
          { pct: ['(17%)', '(33%)', '(50%)'], axis: { at: [0, 50, 100], labels: ['0', '15', '30'] } }),
      col('Split', { kind: 'barsplit', mode: 'stacked', names: ['F', 'M'], fills: ['#f00', '#00f'],
                     seg: [[20, 30, 0], [10, 40, 50]], segv: [[2, 3, 0], [1, 4, 5]],
                     slab: [['2', '3', null], ['1', '4', '5']], sink: ['#fff', '#fff'] }),
      col('Diff', { kind: 'bardiv', w: [20, 0, 40], pos: [true, false, false], dw: 3 }),
      col('Age', { kind: 'box', wl: [10, null, 5], w1: [10, null, 5], bl: [20, null, 15],
                   bw: [30, null, 20], bc: [35, null, 25], b2: [50, null, 35], w2: [20, null, 10],
                   wh: [70, null, 45], tip: ['n = 4', null, 'n = 2'] }),
      col('Weight', { kind: 'pointrange', c: [50, 40, null], l: [30, 20, null],
                      rw: [40, 40, null], ol: [10, null, null], ow: [80, null, null],
                      tip: ['mean 50', 'mean 40', null] }),
      col('Change', { kind: 'pair', a: [10, 20, null], b: [60, 70, null], l: [10, 20, null],
                      w: [50, 50, null], bl: [null, null, null], bw: [null, null, null],
                      fill: ['#0a0', null, null], dash: [false, true, false],
                      open: [false, true, false], tip: ['10 to 60', '20 to 70', null], rf: 40 }),
      col('Course', { kind: 'interval', segs: [[[0, 20, 1, 'Mild'], [30, 10, 2]], [], [[50, 50, 1]]],
                      tips: [['day 1 to 5', 'day 8 to 10'], [], ['day 15 to 30']],
                      fills: ['#aaa', '#555'], d0: 0, d1: 30, lg: true },
          { axis: { at: [0, 50, 100], labels: ['1', '15', '30'], foot: true } }),
      col('Trend', { kind: 'sparkline', pl: ['0,30 50,10 100,20', '', '0,10 100,30'],
                     bd: ['0,5 100,5 100,25 0,25', null, null], dx: [100, null, 100],
                     dy: [55, null, 83], xs: ['1,2,3', '', '1,2'], ys: ['3,1,2', '', '1,3'], rc: 18 }),
      col('Grade', { kind: 'tile' }, { bg: ['#fee', null, '#c00'], fg: ['#000', null, '#fff'] }),
      col('Mean', null, { num: true, disp: ['1.5', '2.5', null] }),
      col('Terms', null, { disp: ['Headache', '<b>x</b>', 'Nausea'] })
    ],
    head: [[{ text: 'Arm', stub: true, col: 0 }].concat(
      ['Rows', 'Split', 'Diff', 'Age', 'Weight', 'Change', 'Course', 'Trend', 'Grade', 'Mean', 'Terms']
        .map((t, k) => ({ text: t, col: k + 1 })))],
    rows: { keys: arms.map(claim) },
    note: '2 more in Other'
  };
}

/** SOC over terms: two parents with their children. */
function nested() {
  const k = (s, t) => Object.assign({ AESOC: [s] }, t ? { AEDECOD: [t] } : {});
  return {
    n: 5, stub: 0,
    cols: [{ name: 'AEDECOD', disp: ['Cardiac', 'Angina', 'Arrhythmia', 'Skin', 'Rash'] },
           { name: 'Rows', disp: ['5', '2', '3', '4', '4'], key: [5, 2, 3, 4, 4],
             mark: { kind: 'bar', w: [100, 40, 60, 80, 80], dw: 2 } }],
    head: [[{ text: 'Term', stub: true, col: 0 }, { text: 'Rows', col: 1 }]],
    rows: { indent: [0, 1, 1, 0, 1],
            keys: [k('Cardiac'), k('Cardiac', 'Angina'), k('Cardiac', 'Arrhythmia'), k('Skin'),
                   k('Skin', 'Rash')] },
    collapsed: false
  };
}

function mount(p, env = loadTable()) {
  const el = env.add();
  env.b.renderValue(el, p);
  const $ = (sel) => el.querySelector(sel);
  const $$ = (sel) => Array.from(el.querySelectorAll(sel));
  const click = (node, init = {}) => node.dispatchEvent(
    new env.win.MouseEvent('click', Object.assign({ bubbles: true }, init)));
  const cell = (r, c) => $$('tbody tr.blockr-data-row')[r].children[c];
  const render = (q) => env.b.renderValue(el, payload(q));
  return Object.assign(env, { el, $, $$, click, cell, render });
}

test('the groups table carries its classes and settings', () => {
  const t = mount(payload({ data: groups(), config: Object.assign({ bar_width: 'narrow', look: 'compact' }, G) }));
  assert.ok(t.$('.vz-table').classList.contains('blockr-summarize-container'));
  const tbl = t.$('table');
  assert.ok(tbl.classList.contains('blockr-summarize-table'));
  assert.equal(tbl.getAttribute('data-summarize-width'), 'narrow');
  assert.equal(tbl.getAttribute('data-summarize-compact'), '1');
  assert.equal(tbl.getAttribute('data-summarize-nested'), null);
  assert.ok(t.cell(0, 0).classList.contains('blockr-summarize-label-col'));
  assert.equal(t.$('.dt-row-count').textContent, '2 more in Other');
});

test('a bar: the track, its fill and the value with its percentage', () => {
  const t = mount(payload({ data: groups(), config: G }));
  const c = t.cell(0, 1);
  assert.equal(c.className, 'blockr-summarize-bar-col');
  assert.match(c.querySelector('.blockr-summarize-fill').getAttribute('style'), /width:33%;background:#2563eb/);
  assert.equal(c.querySelector('.blockr-summarize-barval').textContent, '10 (17%)');
  assert.equal(c.querySelector('.blockr-summarize-barval').getAttribute('style'), 'width:4ch');
  assert.equal(t.cell(2, 1).querySelector('.blockr-summarize-fill'), null);
});

test('values off: the marks draw without their numbers', () => {
  const t = mount(payload({ data: groups(), config: Object.assign({ value_labels: false }, G) }));
  assert.equal(t.$$('.blockr-summarize-barval').length, 0);
  assert.equal(t.$('table').getAttribute('data-summarize-values'), 'off');
});

test('a colour split: one segment per level with a value, its label inside', () => {
  const t = mount(payload({ data: groups(), config: G }));
  const segs = t.cell(1, 2).querySelectorAll('.blockr-summarize-fill');
  assert.equal(segs.length, 2);
  assert.match(segs[0].getAttribute('style'), /width:30%;background:#f00/);
  assert.equal(segs[1].querySelector('.blockr-summarize-seglab').textContent, '4');
  assert.equal(segs[0].getAttribute('data-summarize-tip'), 'F: 3');
  assert.equal(t.cell(2, 2).querySelectorAll('.blockr-summarize-fill').length, 1);
});

test('a grouped split with labels draws one labelled track per level', () => {
  const m = groups();
  Object.assign(m.cols[2].mark, { mode: 'grouped', ldisp: [['2', '3', null], ['1', '4', '5']], dw: 2 });
  const t = mount(payload({ data: m, config: G }));
  const lv = t.cell(0, 2).querySelectorAll('.blockr-summarize-lv');
  assert.equal(lv.length, 2);
  assert.equal(lv[1].querySelector('.blockr-summarize-barval').textContent, '1');
});

test('a diverging bar: positive and negative, zero empty', () => {
  const t = mount(payload({ data: groups(), config: G }));
  assert.ok(t.cell(0, 3).querySelector('.blockr-summarize-fill.is-pos'));
  assert.ok(t.cell(2, 3).querySelector('.blockr-summarize-fill.is-neg'));
  assert.equal(t.cell(1, 3).querySelector('.blockr-summarize-fill'), null);
});

test('a box: whiskers, caps, box, median; nothing without a median', () => {
  const t = mount(payload({ data: groups(), config: G }));
  const box = t.cell(0, 4).querySelector('.blockr-summarize-boxcell');
  assert.deepEqual(Array.from(box.children).map((i) => i.className),
                   ['lane-wh', 'lane-wh', 'lane-cap', 'lane-cap', 'lane-box', 'lane-med']);
  assert.equal(box.getAttribute('data-summarize-tip'), 'n = 4');
  assert.equal(t.cell(1, 4).querySelector('.blockr-summarize-boxcell').children.length, 0);
});

test('a point range: fence, range and centre', () => {
  const t = mount(payload({ data: groups(), config: G }));
  const pr = t.cell(0, 5).querySelector('.blockr-summarize-prcell');
  assert.deepEqual(Array.from(pr.children).map((i) => i.className), ['lane-fence', 'lane-rng', 'lane-ctr']);
  assert.deepEqual(Array.from(t.cell(1, 5).querySelector('.blockr-summarize-prcell').children)
    .map((i) => i.className), ['lane-rng', 'lane-ctr']);
});

test('a pair: reference, link, from and an open to; dashed by its level', () => {
  const t = mount(payload({ data: groups(), config: G }));
  const pa = t.cell(1, 6).querySelector('.blockr-summarize-pacell');
  assert.ok(pa.classList.contains('is-dash'));
  assert.deepEqual(Array.from(pa.children).map((i) => i.className),
                   ['lane-ref', 'lane-link', 'lane-from', 'lane-to is-open']);
  assert.match(t.cell(0, 6).querySelector('.blockr-summarize-pacell').getAttribute('style'),
               /--blockr-summarize-fill:#0a0/);
});

test('a colour split of lanes draws one lane per level that has rows', () => {
  const m = groups();
  const g = (c) => ({ c, l: [10, 10, 10], rw: [20, 20, 20], tip: ['a', 'b', 'c'], disp: ['1', '2', '3'] });
  m.cols[5].mark = { kind: 'pointrange', multi: true, levels: ['F', 'M'], fills: ['#f00', '#00f'],
                     lv: [g([40, null, 30]), g([60, 50, null])], dw: 2 };
  const t = mount(payload({ data: m, config: G }));
  const lv = t.cell(0, 5).querySelectorAll('.blockr-summarize-lv');
  assert.equal(lv.length, 2);
  assert.match(lv[1].getAttribute('style'), /#00f/);
  assert.equal(lv[1].querySelector('.blockr-summarize-barval').textContent, '1');
  assert.equal(t.cell(1, 5).querySelectorAll('.blockr-summarize-lv').length, 1);
});

test('a swimlane: its segments in their level colours, the axis in the foot', () => {
  const t = mount(payload({ data: groups(), config: G }));
  const c = t.cell(0, 7);
  assert.ok(c.classList.contains('blockr-summarize-wide'));
  const segs = c.querySelectorAll('.lane-seg');
  assert.equal(segs.length, 2);
  assert.match(segs[1].getAttribute('style'), /background:#555/);
  assert.equal(segs[0].getAttribute('data-l'), 'Mild');
  assert.equal(segs[0].getAttribute('data-tip'), 'day 1 to 5');
  assert.equal(t.$('tfoot .blockr-summarize-axis-cell').children.length, 1);
  assert.equal(t.$$('thead .blockr-summarize-axis').length, 1);
});

test('a sparkline: band, reference line, line and the last point', () => {
  const t = mount(payload({ data: groups(), config: G }));
  assert.ok(t.cell(0, 8).classList.contains('is-spark'));
  const sp = t.cell(0, 8).querySelector('.blockr-summarize-spcell');
  assert.deepEqual(Array.from(sp.querySelector('svg').children).map((e) => e.tagName.toLowerCase()),
                   ['polygon', 'line', 'polyline']);
  assert.match(sp.querySelector('.lane-dot').getAttribute('style'), /left:100%;top:55%/);
  assert.equal(t.cell(1, 8).querySelector('polyline'), null);
});

test('a tile is painted with its value; numbers and text draw as they are', () => {
  const t = mount(payload({ data: groups(), config: G }));
  const tile = t.cell(2, 9).querySelector('.blockr-summarize-tile');
  assert.equal(tile.getAttribute('style'), 'background:#c00;color:#fff');
  assert.equal(t.cell(1, 9).innerHTML, '');
  assert.ok(t.cell(0, 10).classList.contains('blockr-summarize-num'));
  assert.equal(t.cell(1, 11).innerHTML, '&lt;b&gt;x&lt;/b&gt;');
});

test('the axis strip sits under its header, with the value slot', () => {
  const t = mount(payload({ data: groups(), config: G }));
  const ax = t.$('th[data-col-index="1"] .blockr-summarize-axis');
  assert.ok(ax.classList.contains('has-val'));
  assert.deepEqual(Array.from(ax.querySelectorAll('.blockr-summarize-axis-in span')).map((s) => s.className),
                   ['is-first', '', 'is-last']);
  t.render({ data: groups(), config: Object.assign({ column_axis: false }, G) });
  assert.equal(t.$$('.blockr-summarize-axis').length, 0);
});

test('a row click claims the group and lights its row; again clears', () => {
  const t = mount(payload({ data: groups(), config: G, derived: { legend: null } }));
  t.click(t.cell(1, 1));
  t.click(t.cell(1, 4));
  assert.deepEqual(t.selects(), [{ type: 'categorical', filters: { ARM: ['Low'] } }, null]);
});

test('a click on a colour split\'s segment claims that level only', () => {
  const m = groups();
  m.cols[2].keys = { SEX: ['F', 'M'] };
  const t = mount(payload({ data: m, config: G }));
  t.click(t.cell(1, 2).querySelectorAll('.blockr-summarize-fill')[1]);
  t.click(t.cell(0, 2));
  assert.deepEqual(t.selects(), [
    { type: 'categorical', filters: { ARM: ['Low'] }, columns: { SEX: ['M'] } },
    { type: 'categorical', filters: { ARM: ['Placebo'] }, columns: { SEX: ['F', 'M'] } }
  ]);
});

test('without by there is no search or header sorting', () => {
  const m = groups();
  const t = mount(payload({ data: m, config: { layout: 'groups', by: [] } }));
  assert.ok(t.$('input.blockr-search').hidden);
  assert.equal(t.$$('th.blockr-sortable').length, 0);
});

test('nested groups: parents fold their terms; search keeps the parent', async () => {
  const t = mount(payload({ data: nested(), config: Object.assign({}, G, { by: ['AESOC', 'AEDECOD'] }) }));
  assert.equal(t.$('table').getAttribute('data-summarize-nested'), '1');
  const rows = () => t.$$('tbody tr.blockr-data-row');
  assert.deepEqual(rows().map((r) => r.className.split(' ').filter((c) => c.startsWith('is-')).join(' ')),
                   ['is-parent', 'is-child', 'is-child is-group-end', 'is-parent', 'is-child is-group-end']);
  t.click(rows()[0].querySelector('.blockr-indent-btn'));
  assert.equal(rows().length, 3);
  t.click(rows()[0].querySelector('.blockr-indent-btn'));
  const inp = t.$('input.blockr-search');
  inp.value = 'rash';
  inp.dispatchEvent(new t.win.Event('input'));
  await tick(t.win, 200);
  assert.deepEqual(rows().map((r) => r.children[0].textContent), ['Skin', 'Rash']);
  assert.equal(t.$('.dt-row-count').textContent, '');
});

test('nested groups sort as a tree: parents, then each parent\'s terms', () => {
  const t = mount(payload({ data: nested(), config: Object.assign({}, G, { by: ['AESOC', 'AEDECOD'] }) }));
  t.click(t.$('th[data-col-index="1"]'));
  assert.deepEqual(t.$$('tbody tr.blockr-data-row').map((r) => r.children[0].textContent),
                   ['Skin', 'Rash', 'Cardiac', 'Angina', 'Arrhythmia']);
});

test('the legend: one group per colour column, hidden with cards', () => {
  const legend = { groups: [{ title: 'Sex', items: [{ color: '#f00', label: 'F' }, { color: '#00f', label: 'M' }] }] };
  const t = mount(payload({ data: groups(), config: G, derived: { legend }, dataRev: 9 }));
  assert.equal(t.$('.blockr-summarize-legend').hidden, false);
  assert.equal(t.$('.blockr-summarize-legend-title').textContent, 'Sex');
  assert.equal(t.$$('.blockr-summarize-legend-item').length, 2);
  assert.ok(t.$('.blockr-summarize-legend').nextElementSibling.classList.contains('blockr-table-wrapper'));
  t.render({ dataRev: 9, config: G, derived: {} });
  assert.ok(t.$('.blockr-summarize-legend').hidden);
});

test('a card draws its column\'s mark without the value', () => {
  const m = groups();
  m.cards = { n: 3, multi: true, label: m.cols[0].disp,
              cols: [{ label: 'Rows', main: ['10', '20', '30'], glyph: 1 }] };
  const t = mount(payload({ data: m, config: Object.assign({ look: 'cards' }, G) }));
  const glyph = t.$('.tc-glyph');
  assert.equal(glyph.getAttribute('data-k'), '1');
  assert.ok(glyph.querySelector('.blockr-summarize-track'));
  assert.equal(glyph.querySelector('.blockr-summarize-barval'), null);
  t.click(t.$$('.tc-row')[2]);
  assert.deepEqual(t.selects(), [{ type: 'categorical', filters: { ARM: ['High'] } }]);
});

test('the picture draws every row off screen without tools, and goes', async () => {
  const seen = [];
  const env = loadTable({ before: (win) => {
    win.snapdom = { toCanvas: (el) => {
      seen.push({ rows: el.querySelectorAll('tbody tr.blockr-data-row').length,
                  search: el.querySelector('input.blockr-search').hidden,
                  host: !!el.closest('.blockr-capture-host') });
      return Promise.resolve({ toDataURL: () => 'data:image/png;base64,AAA' });
    } };
  } });
  const m = groups();
  m.n = 600;
  m.cols = m.cols.slice(0, 2);
  m.cols[0].disp = Array.from({ length: 600 }, (_, i) => 'g' + i);
  m.cols[1] = { name: 'Rows', disp: m.cols[0].disp.map(() => '1'), mark: { kind: 'bar', w: m.cols[0].disp.map(() => 50) } };
  m.head = [m.head[0].slice(0, 2)];
  delete m.rows;
  const pic = await env.V.table.picture(payload({ data: m, config: G }), 800, 2);
  assert.deepEqual(seen, [{ rows: 600, search: true, host: true }]);
  assert.equal(pic.png, 'data:image/png;base64,AAA');
  assert.equal(env.win.document.querySelector('.blockr-capture-host'), null);
});

test('opening the download menu sends R the picture, once per payload', async () => {
  const env = loadTable({ before: (win) => {
    win.snapdom = { toCanvas: () => Promise.resolve({ toDataURL: () => 'data:image/png;base64,BBB' }) };
  } });
  const dl = [{ id: 'tbl_png', label: 'Image', ext: 'png' }, { id: 'tbl_xlsx', label: 'Excel', ext: 'xlsx' }];
  const t = mount(payload({ data: groups(), config: G, derived: { downloads: dl } }), env);
  const trigger = t.$('.blockr-action-menu__trigger');
  t.click(trigger);
  trigger.dispatchEvent(new t.win.Event('pointerenter'));
  await tick(t.win, 200);
  t.click(trigger);
  await tick(t.win, 200);
  const caps = t.sent.filter((s) => s.name === 'tbl_capture:blockr.viz.capture');
  assert.equal(caps.length, 1);
  assert.equal(caps[0].value.png, 'data:image/png;base64,BBB');
});

test('without snapdom the picture fails and nothing is sent', async () => {
  const env = loadTable();
  await assert.rejects(env.V.table.picture(payload({ data: groups(), config: G }), 500), /snapdom/);
});

test('50 groups mounts and unmounts leave no scope open', async () => {
  const env = loadTable();
  const open = env.V.Scope.open();
  for (let k = 0; k < 50; k++) {
    const el = env.add('tbl');
    env.b.renderValue(el, payload({ data: k % 2 ? nested() : groups(), config: G }));
    el.querySelector('tbody tr.blockr-data-row').dispatchEvent(new env.win.MouseEvent('click', { bubbles: true }));
    el.remove();
    env.trigger(el, 'shiny:unbound');
    await tick(env.win, 5);
  }
  assert.equal(env.V.Scope.open(), open);
  assert.equal(env.win.document.querySelectorAll('.blockr-summarize-legend').length, 0);
});

/** Move the pointer onto `node` and return the card's text and node. */
function hover(t, node) {
  node.dispatchEvent(new t.win.MouseEvent('mousemove', { bubbles: true, clientX: 10, clientY: 10 }));
  const tip = t.win.document.querySelector('.blockr-lane-tip');
  return { tip, text: tip.hidden ? '' : tip.textContent, html: tip.innerHTML };
}

test('hover: a bar names its value under the row and the column context', () => {
  const m = groups();
  m.cols[1].mark.tt = { head: 'Count', meas: 'Rows' };
  const t = mount(payload({ data: m, config: G }));
  const c = hover(t, t.cell(1, 1).querySelector('.blockr-summarize-fill'));
  assert.equal(c.tip.querySelector('.dd-tt-head').textContent, 'Low');
  assert.match(c.text, /Count/);
  assert.match(c.text, /Rows20 \(33%\)/);
  assert.equal(hover(t, t.cell(1, 1)).text, '');
});

test('hover: a colour split lists each level, the one under the pointer lit', () => {
  const m = groups();
  m.cols[2].mark.tt = { cvar: 'Sex', den: 10, meas: 'Rows' };
  const t = mount(payload({ data: m, config: G }));
  const segs = t.cell(1, 2).querySelectorAll('.blockr-summarize-fill');
  const c = hover(t, segs[1]);
  const rows = c.tip.querySelectorAll('.dd-tt-row');
  assert.equal(rows[0].textContent, 'Sex F3 (30%)');
  assert.ok(rows[0].classList.contains('is-dim'));
  assert.ok(rows[1].classList.contains('is-hit'));
  assert.match(c.text, /Rows20 of 10/);
});

test('hover: a box names its statistics; a pair its clauses', () => {
  const m = groups();
  Object.assign(m.cols[4].mark, { nn: [4, null, 2], r: { bc: [35, null, 25], bl: [30, null, 20], bh: [40, null, 30],
                                                          wl: [10, null, 5], wh: [70, null, 45] },
                                  tt: { words: { center: 'Median', range: 'Q1–Q3', whisk: 'Tukey' } } });
  const t = mount(payload({ data: m, config: G }));
  const box = hover(t, t.cell(0, 4).querySelector('.lane-box'));
  assert.match(box.text, /n4/);
  assert.match(box.text, /Median35/);
  assert.match(box.text, /Box \(Q1–Q3\)30 – 40/);
  assert.match(box.text, /Whiskers \(Tukey\)10 – 70/);
  assert.equal(hover(t, t.cell(0, 4).querySelector('.blockr-summarize-boxcell')).text, '');
  const pr = hover(t, t.cell(0, 6).querySelector('.lane-to'));
  assert.match(pr.text, /10 to 60/);
});

test('hover: a swimlane segment says its text and lights its label everywhere', () => {
  const t = mount(payload({ data: groups(), config: G }));
  const seg = t.cell(0, 7).querySelector('.lane-seg');
  const c = hover(t, seg);
  assert.equal(c.tip.querySelector('.dd-tt-head').textContent, 'day 1 to 5');
  assert.ok(seg.classList.contains('is-same'));
  assert.ok(t.$('.blockr-table-wrapper').classList.contains('seg-hover'));
  t.$('.blockr-table-wrapper').dispatchEvent(new t.win.MouseEvent('mouseleave'));
  assert.ok(c.tip.hidden);
  assert.equal(seg.classList.contains('is-same'), false);
});

test('hover: a sparkline names its point; the card goes with the view', () => {
  const t = mount(payload({ data: groups(), config: G }));
  const c = hover(t, t.cell(0, 8).querySelector('.blockr-summarize-spcell'));
  assert.equal(c.tip.querySelector('.dd-tt-head').textContent, '1');
  assert.equal(c.tip.querySelector('.dd-tt-row').textContent, '3');
  t.el.remove();
  t.trigger(t.el, 'shiny:unbound');
  return tick(t.win, 5).then(() => {
    assert.equal(t.win.document.querySelector('.blockr-lane-tip'), null);
  });
});

test('hover: a card glyph gives its column\'s card', () => {
  const m = groups();
  m.cards = { n: 3, multi: true, label: m.cols[0].disp, cols: [{ label: 'Rows', main: ['10', '20', '30'], glyph: 1 }] };
  const t = mount(payload({ data: m, config: Object.assign({ look: 'cards' }, G) }));
  const c = hover(t, t.$$('.tc-row')[0].querySelector('.blockr-summarize-fill'));
  assert.match(c.text, /^Placebo/);
  assert.match(c.text, /10 \(17%\)/);
});

test('bar_width: narrow, medium and wide set the table width; fit leaves it', () => {
  for (const w of ['narrow', 'medium', 'wide', 'fit']) {
    const t = mount(payload({ data: groups(), config: Object.assign({ bar_width: w }, G) }));
    assert.equal(t.$('table').getAttribute('data-summarize-width'), w === 'fit' ? null : w, w);
  }
});

test("the picture takes R's pixel ratio, else 2", async () => {
  const scales = [];
  const env = loadTable({ before: (win) => {
    win.snapdom = { toCanvas: (el, o) => {
      scales.push(o.scale);
      return Promise.resolve({ toDataURL: () => 'data:image/png;base64,AAA' });
    } };
  } });
  await env.V.table.picture(payload({ data: groups(), config: G, derived: { capture_ratio: 3 } }), 500);
  await env.V.table.picture(payload({ data: groups(), config: G }), 500);
  await env.V.table.picture(payload({ data: groups(), config: G, derived: { capture_ratio: 3 } }), 500, 1);
  assert.deepEqual(scales, [3, 2, 1]);
});
