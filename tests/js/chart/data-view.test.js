/* The chart's data view (core/data-view.js): the header tool that opens the
 * table under the chart and tells R, the tabs and their counts from R's
 * `derived.data_view`, a tab pick that waits for R's answer, the view
 * opened again from the payload after a re-mount, a row click on Summary
 * that goes to R, and teardown: 50 mounts with the table open leave
 * nothing behind. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const D = require('./drive');
const { payload: tablePayload } = require('../table/load');

const BAR = { type: 'bar', category: 'AETERM', orientation: 'horizontal', drill: 'auto' };
const INPUT = D.ID + '_data:blockr.viz.data_view';

const lvl = (l) => ({ '.variable': ['AETERM'], '.variable_level': [l] });

/** Summary as R's chart_summary() and interactive_table_payload() send it. */
function summaryTable(selection = null) {
  const terms = ['Rash', 'Nausea', 'Headache'];
  return tablePayload({
    config: { search: false, download: false, drill: 'auto', selection },
    dataRev: 1,
    data: {
      n: 3, stub: 0,
      cols: [
        { name: '.label', disp: terms },
        { name: 'Count of rows', num: true, disp: ['12', '8', '5'], key: [12, 8, 5] }
      ],
      head: [[{ text: 'AETERM', stub: true, col: 0 }, { text: 'Count of rows', col: 1 }]],
      rows: { keys: terms.map(lvl) }
    }
  });
}

/** Rows: the input rows behind the selection. */
function rowsTable(n) {
  const id = Array.from({ length: n }, (_, i) => 'S' + (i + 1));
  return tablePayload({
    config: { search: false, download: false },
    dataRev: 2,
    data: { n, stub: 0, cols: [{ name: 'USUBJID', disp: id, raw: id }],
            head: [[{ text: 'USUBJID', stub: true, col: 0 }]] }
  });
}

const dv = (o = {}) => Object.assign({ tab: 'summary', summary: 3, rows: 25, total: 25,
                                       table: summaryTable() }, o);

const tool = (c) => c.el.querySelector('.vz-data-tool');
const isOpen = (c) => c.el.querySelector('.vz-frame').classList.contains('vz-data-open');
const segs = (c) => Array.from(c.el.querySelectorAll('.vz-data__bar .blockr-segmented__seg'));
const tabs = (c) => segs(c).map((b) => b.textContent);
const bodyRows = (c) => c.el.querySelectorAll('.vz-data__table tbody tr.vz-table-row').length;
const sent = (c, name) => c.inputs().filter((x) => x.name === name).map((x) => D.plain(x.value));
/** The data view's requests without their count. */
const asks = (c) => sent(c, INPUT).map(({ on, tab }) => ({ on, tab }));

test('the tool opens the table under the chart and tells R', () => {
  const c = D.open(BAR);
  const t = tool(c);
  assert.ok(t, 'a tool in the header');
  assert.strictEqual(t.nextElementSibling, c.block.frame.downloadSlot, 'left of the download');
  assert.strictEqual(t.getAttribute('aria-label'), 'Show as table');
  assert.strictEqual(isOpen(c), false);
  t.click();
  assert.strictEqual(isOpen(c), true);
  assert.strictEqual(t.getAttribute('aria-pressed'), 'true');
  assert.strictEqual(t.getAttribute('aria-label'), 'Hide table');
  assert.deepStrictEqual(asks(c), [{ on: true, tab: 'summary' }]);
  t.click();
  assert.strictEqual(isOpen(c), false);
  assert.deepStrictEqual(asks(c)[1], { on: false, tab: 'summary' });
  const n = sent(c, INPUT).map((x) => x.n);
  assert.ok(n[1] > n[0], 'each request has a new count');
  c.close();
});

test('the tabs carry R\'s counts, and a selection changes the count of Rows', () => {
  const c = D.open(BAR);
  tool(c).click();
  c.send(c.config, { derived: { data_view: dv() } });
  assert.deepStrictEqual(tabs(c), ['Summary3', 'Rows25']);
  assert.strictEqual(bodyRows(c), 3, 'Summary drawn by the table view');
  // A click on a bar: R answers with the rows behind it and lights the row.
  c.send(c.config, { derived: { data_view: dv({
    rows: 12, table: summaryTable({ type: 'categorical', filters: lvl('Rash') }) }) } });
  assert.deepStrictEqual(tabs(c), ['Summary3', 'Rows12 of 25']);
  const lit = c.el.querySelectorAll('.vz-data__table tbody tr.vz-table-row-active');
  assert.strictEqual(lit.length, 1);
  assert.strictEqual(lit[0].getAttribute('data-i'), '0');
  c.close();
});

test('a tab pick goes to R, and a payload that does not answer it is skipped', () => {
  const c = D.open(BAR);
  tool(c).click();
  c.send(c.config, { derived: { data_view: dv() } });
  segs(c)[1].click();
  assert.deepStrictEqual(asks(c).pop(), { on: true, tab: 'rows' });
  // R's payload for a setting, built before it read the pick.
  c.send(c.config, { derived: { data_view: dv({ rows: 24 }) } });
  assert.strictEqual(bodyRows(c), 3, 'still Summary');
  assert.ok(segs(c)[1].classList.contains('is-selected'), 'the pick shows');
  c.send(c.config, { derived: { data_view: dv({ tab: 'rows', rows: 7, table: rowsTable(7) }) } });
  assert.strictEqual(bodyRows(c), 7);
  assert.deepStrictEqual(tabs(c), ['Summary3', 'Rows7 of 25']);
  c.close();
});

test('the table\'s rows come once per data revision; without them the view asks again', () => {
  const c = D.open(BAR);
  tool(c).click();
  c.send(c.config, { derived: { data_view: dv() } });
  const noRows = (o = {}) => {
    const t = summaryTable(o.selection);
    t.data = null;
    return dv({ table: t });
  };
  // A click: the same rows, so R leaves them out.
  c.send(c.config, { derived: { data_view: noRows({ selection: { type: 'categorical',
                                                                 filters: lvl('Nausea') } }) } });
  assert.strictEqual(bodyRows(c), 3);
  assert.strictEqual(c.el.querySelectorAll('.vz-data__table tr.vz-table-row-active').length, 1);
  assert.strictEqual(sent(c, INPUT).length, 1, 'nothing asked');
  c.close();
  // A re-mounted block: Shiny replays a payload without rows.
  const d = D.open(BAR, { derived: { data_view: noRows() } });
  assert.strictEqual(isOpen(d), true);
  assert.strictEqual(bodyRows(d), 0);
  assert.deepStrictEqual(asks(d), [{ on: true, tab: 'summary' }]);
  d.send(d.config, { derived: { data_view: noRows() } });
  assert.strictEqual(sent(d, INPUT).length, 1, 'asked once');
  d.send(d.config, { derived: { data_view: dv() } });
  assert.strictEqual(bodyRows(d), 3);
  d.close();
});

test('a chart without a Summary shows its rows and their count alone', () => {
  const c = D.open({ type: 'scatter', x: 'ADY', y: 'AVAL' });
  tool(c).click();
  c.send(c.config, { derived: { data_view: dv({ tab: 'rows', summary: null, rows: 9,
                                                total: 9, table: rowsTable(9) }) } });
  assert.deepStrictEqual(tabs(c), []);
  assert.strictEqual(c.el.querySelector('.vz-data__meta').textContent, '9 rows');
  assert.strictEqual(bodyRows(c), 9);
  c.close();
});

test('a re-mounted block opens the table from the payload; R closing it closes it', () => {
  const c = D.open(BAR, { derived: { data_view: dv() } });
  assert.strictEqual(isOpen(c), true, 'R still holds the input');
  assert.strictEqual(bodyRows(c), 3);
  c.send(c.config, { derived: {} });
  assert.strictEqual(isOpen(c), false);
  c.close();
});

test('a closed table stays closed on a payload R built before it read the close', () => {
  const c = D.open(BAR);
  tool(c).click();
  c.send(c.config, { derived: { data_view: dv() } });
  tool(c).click();
  c.send(c.config, { derived: { data_view: dv() } });
  assert.strictEqual(isOpen(c), false);
  c.close();
});

test('a row click on Summary goes to R as the table\'s selection', () => {
  const c = D.open(BAR);
  tool(c).click();
  c.send(c.config, { derived: { data_view: dv() } });
  const row = c.el.querySelectorAll('.vz-data__table tbody tr.vz-table-row')[1];
  row.children[1].dispatchEvent(new c.env.win.MouseEvent('click', { bubbles: true }));
  const msg = sent(c, D.ID + '-data_select:blockr.viz.selection');
  assert.strictEqual(msg.length, 1);
  assert.deepStrictEqual(msg[0].selection.filters, lvl('Nausea'));
  assert.deepStrictEqual(c.selections(), [], 'the chart sends nothing itself');
  c.close();
});

test('50 mounts with the table open leave no listener, timer, scope, node or chart behind', () => {
  const c = D.open(BAR);
  const win = c.env.win;
  const live = new Set();
  for (const target of [win, win.document]) {
    const add = target.addEventListener.bind(target);
    const rm = target.removeEventListener.bind(target);
    target.addEventListener = (t, f, o) => { live.add(f); return add(t, f, o); };
    target.removeEventListener = (t, f, o) => { live.delete(f); return rm(t, f, o); };
  }
  const observers = new Set();
  const MO = win.MutationObserver;
  win.MutationObserver = class extends MO {
    observe(...a) { observers.add(this); return super.observe(...a); }
    disconnect() { observers.delete(this); return super.disconnect(); }
  };
  const V = win.Blockr.viz;
  c.el._viz.scope.dispose();
  c.advance(10000);
  const base = { listeners: live.size, timers: c.clock.timers.length, scopes: V.Scope.open(),
                 nodes: c.doc.body.querySelectorAll('*').length, observers: observers.size };
  const h = require('../harness');
  const { F } = require('../cases');
  for (let i = 0; i < 50; i++) {
    const m = h.mount(c.env, 'cycle-' + i);
    const p = { columns: F.columns, data: F.data, config: { ...BAR, color: 'AESEV' } };
    h.send(c.env, m.id, p);
    m.el.querySelector('.vz-data-tool').click();
    h.send(c.env, m.id, { ...p, derived: { data_view: dv() } });
    m.el.querySelectorAll('.vz-data__bar .blockr-segmented__seg')[1].click();
    h.send(c.env, m.id, { ...p, derived: { data_view: dv({ tab: 'rows', table: rowsTable(25) }) } });
    m.el.parentElement.remove();
    m.el.__jq['shiny:unbound']();
    c.advance(10000);
  }
  assert.deepStrictEqual({ listeners: live.size, timers: c.clock.timers.length,
                           scopes: V.Scope.open(), nodes: c.doc.body.querySelectorAll('*').length,
                           observers: observers.size }, base);
  c.close();
});
