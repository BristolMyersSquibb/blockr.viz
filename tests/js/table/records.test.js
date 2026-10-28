'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadTable, payload, flatModel, table1, tick } = require('./load');

function mount(p, env = loadTable()) {
  const el = env.add();
  if (p) env.b.renderValue(el, p);
  const $ = (sel) => el.querySelector(sel);
  const $$ = (sel) => Array.from(el.querySelectorAll(sel));
  const click = (node) => node.dispatchEvent(new env.win.MouseEvent('click', { bubbles: true }));
  const render = (q) => env.b.renderValue(el, payload(q));
  return Object.assign(env, { el, $, $$, click, render });
}

/** A flat model with its cards: one card per column, one row per row. */
function withCards(n = 4) {
  const m = flatModel(n);
  m.cards = {
    n, multi: true, label: m.cols[0].disp, hex: m.cols[0].disp.map((_, i) => i ? null : '#123456'),
    cols: [{ label: 'Arm', main: m.cols[1].disp, text: true },
           { label: 'Age', main: m.cols[2].disp, sub: m.cols[2].disp.map(() => 'years'), word: 'Age' }]
  };
  return m;
}

test('the frame: titles on the left, tools and footer', () => {
  const t = mount(payload({ data: flatModel(3),
                            derived: { titles: { title: 'Subjects', caption: 'Source: ADSL' } } }));
  assert.equal(t.$('.vz-head .dd-table-title').textContent, 'Subjects');
  assert.ok(t.$('.dd-table-subtitle').hidden);
  assert.equal(t.$('.dd-table-caption').textContent, 'Source: ADSL');
  assert.equal(t.$('input.blockr-search').hidden, false);
  assert.equal(t.$('.dt-download-slot').hidden, false);
});

test('with no table the tools stay hidden and the message shows', () => {
  const t = mount(payload({ data: {}, derived: { empty: { kind: 'needs', message: 'Connect a data block' } } }));
  assert.equal(t.$('.vd-empty-state').textContent, 'Connect a data block');
  assert.equal(t.$('.vd-empty-state').getAttribute('data-kind'), 'needs');
  assert.ok(t.$('input.blockr-search').hidden);
  assert.ok(t.$('.dt-download-slot').hidden);
  assert.equal(t.$('table'), null);
  t.render({ data: flatModel(2) });
  assert.equal(t.$('.vd-empty-state'), null);
  assert.equal(t.$('input.blockr-search').hidden, false);
  assert.equal(t.$$('tbody tr.blockr-data-row').length, 2);
});

test('download off hides the download slot only', () => {
  const t = mount(payload({ data: flatModel(2), config: { download: false } }));
  assert.ok(t.$('.dt-download-slot').hidden);
  assert.equal(t.$('input.blockr-search').hidden, false);
});

test('an error from R shows in the frame', () => {
  const t = mount(payload({ data: flatModel(2) }));
  t.b.renderError(t.el, { message: 'Column AGE is gone' });
  assert.equal(t.$('.vd-empty-state').getAttribute('data-kind'), 'error');
  assert.equal(t.$('.vd-empty-text').textContent, 'Column AGE is gone');
});

test('the cards look draws a card per column and drills on a card row', () => {
  const t = mount(payload({ data: withCards(4), config: { look: 'cards' } }));
  assert.equal(t.$('table'), null);
  assert.equal(t.$$('.tc-row').length, 4);
  assert.deepEqual(t.$$('.tc-colhead b').map((b) => b.textContent), ['Arm', 'Age']);
  assert.equal(t.$('.tc-row .tc-strip').getAttribute('style'), 'background:#123456');
  assert.equal(t.$('.dt-row-count').textContent, '4 rows');
  t.click(t.$$('.tc-row')[3].querySelector('.tc-val'));
  assert.deepEqual(t.selects(), [{ type: 'categorical', filters: { ARM: ['Placebo'] } }]);
  assert.deepEqual(t.$$('.tc-row.dt-row-active').map((r) => r.getAttribute('data-i')), ['0', '3']);
});

test('search narrows the card rows', async () => {
  const t = mount(payload({ data: withCards(6), config: { look: 'cards' } }));
  const inp = t.$('input.blockr-search');
  inp.value = 'low';
  inp.dispatchEvent(new t.win.Event('input'));
  await tick(t.win, 200);
  assert.deepEqual(t.$$('.tc-row').map((r) => r.getAttribute('data-i')), ['1', '4']);
  assert.equal(t.$('.dt-row-count').textContent, '2 of 6 rows');
});

test('a single row of cards names the statistic in the card', () => {
  const m = withCards(1);
  m.cards.multi = false;
  m.cards.cols[1].legend = [{ color: '#f00', label: 'F' }];
  const t = mount(payload({ data: m, config: { look: 'cards' } }));
  assert.equal(t.$('.tc-colhead'), null);
  assert.equal(t.$$('.tc .tc-word')[0].textContent, 'Age: ');
  assert.equal(t.$('.tc-legend span').textContent, 'F');
});

test('cards without columns say so; switching back to the table draws it', () => {
  const m = withCards(2);
  m.cards.cols = [];
  const t = mount(payload({ data: m, config: { look: 'cards' }, dataRev: 3 }));
  assert.equal(t.$('.tc-empty').textContent, 'No summary to show as a card.');
  t.render({ config: { look: 'table' }, dataRev: 3 });
  assert.equal(t.$$('tbody tr.blockr-data-row').length, 2);
  assert.ok(t.$('.blockr-cards-host').hidden);
});

test('the cards follow the width of their host', () => {
  const env = loadTable({ before: (win) => {
    // happy-dom lays nothing out: report a width and call back at once.
    win.ResizeObserver = class {
      constructor(fn) { this.fn = fn; }
      observe(el) { this.fn([{ target: el }]); }
      disconnect() {}
    };
  } });
  const t = mount(null, env);
  Object.defineProperty(t.el, 'clientWidth', { value: 0, configurable: true });
  t.render({ data: withCards(2), config: { look: 'cards' } });
  const host = t.$('.blockr-cards-host');
  Object.defineProperty(host, 'clientWidth', { value: 250, configurable: true });
  t.render({ data: withCards(2), config: { look: 'cards' } });
  assert.equal(t.$('.tc-grid').getAttribute('data-tc-mode'), 'narrow');
  Object.defineProperty(host, 'clientWidth', { value: 360 });
  t.render({ data: withCards(2), config: { look: 'cards' } });
  assert.equal(t.$('.tc-grid').getAttribute('data-tc-mode'), 'medium');
});

test('another layout replaces the records view and its table', () => {
  const t = mount(payload({ data: flatModel(3) }));
  t.render({ data: flatModel(3), config: { layout: 'pivot' } });
  assert.equal(t.$('table'), null);
  assert.match(t.$('.vd-empty-text').textContent, /pivot layout/);
  t.render({ data: flatModel(3), config: { layout: 'records' } });
  assert.equal(t.$('.vd-empty-state'), null);
  assert.equal(t.$$('tbody tr.blockr-data-row').length, 3);
  assert.equal(t.$$('.dt-table-slot').length, 1);
});

test('the gear engine, when loaded, is mounted in a tray and updated after the layout', () => {
  const log = [];
  let host;
  const env = loadTable({ before: (win) => {
    win.Blockr.viz.gear = { ...win.Blockr.viz.gear,
      mount: (tray, o) => {
        host = o.host;
        log.push(['mount', tray.className, o.block, o.id, o.typeKey]);
        return { update: (p) => log.push(['update', p.config.layout]), dispose: () => log.push(['dispose']) };
      }
    };
  } });
  const t = mount(payload({ data: flatModel(2) }), env);
  assert.deepEqual(log, [['mount', 'blockr-settings blockr-settings--beak dd-popover', 'table', 'tbl', 'layout'],
                         ['update', 'records']]);
  const btn = t.$('.vz-head > .blockr-gear-btn');
  t.click(btn);
  assert.ok(t.$('.blockr-settings').classList.contains('blockr-settings--open'));
  assert.equal(host.label('by', { layout: 'records' }), 'Nest by');
  assert.equal(host.label('by', { layout: 'variables' }), 'One column per');
  assert.equal(host.label('title', { layout: 'records' }), null);
  const v = { layout: 'records', by: [], drill: null };
  assert.equal(host.visible('shadings', v), true);
  assert.equal(host.visible('collapsible', v), false);
  assert.equal(host.visible('missing', v), false);
  assert.equal(host.visible('drill_target', v), false);
  assert.equal(host.visible('drill_target', Object.assign({}, v, { drill: 'ARM' })), true);
  assert.equal(host.visible('missing', { layout: 'groups', by: [] }), true);
});

test('the gear hides what a finished input cannot change', () => {
  let host;
  const env = loadTable({ before: (win) => {
    win.Blockr.viz.gear = { ...win.Blockr.viz.gear, mount: (tray, o) => { host = o.host; return { update() {}, dispose() {} }; } };
  } });
  mount(payload({ data: table1() }), env);
  const v = { layout: 'records', by: [] };
  for (const k of ['by', 'rowname', 'columns', 'color', 'shadings', 'missing']) {
    assert.equal(host.visible(k, v), false, k);
  }
  assert.equal(host.visible('collapsible', v), true);
  assert.equal(host.visible('format', v), true);
});

test('the shading editor round-trips every key of a rule', () => {
  const { V, win } = loadTable();
  const el = win.document.createElement('div');
  const sets = [];
  const ctx = {
    values: { shadings: [{ mode: 'bar', columns: ['AGE'], domain: [{ value: -1, label: null }, { value: 1, label: null }] }] },
    columns: [{ name: 'AGE', class: 'number', label: 'Age' }, { name: 'WT', class: 'number', label: 'Weight' },
              { name: 'ARM', class: 'character', label: 'Arm' }],
    set: (x) => sets.push(JSON.parse(JSON.stringify(x)))
  };
  ctx.scope = new V.Scope();
  V.table.gear.shadings(el, ctx);
  const fire = (x, type) => x.dispatchEvent(new win.Event(type, { bubbles: true }));
  assert.equal(el.querySelectorAll('.vz-shade-cols label').length, 2);
  const boxes = el.querySelectorAll('.vz-shade-cols input');
  boxes[1].checked = true;
  fire(boxes[1], 'change');
  const mode = el.querySelector('select');
  mode.value = 'Sequential';
  fire(mode, 'change');
  fire(el.querySelector('.vz-shade-add'), 'click');
  fire(el.querySelector('.vz-shade-remove'), 'click');
  assert.ok(el.querySelector('.vz-shade-add').classList.contains('blockr-btn'));
  const dom = [{ value: -1, label: null }, { value: 1, label: null }];
  assert.deepEqual(sets, [
    [{ mode: 'bar', columns: ['AGE', 'WT'], domain: dom }],
    [{ mode: 'sequential', columns: ['AGE'], domain: dom }],
    [{ mode: 'bar', columns: ['AGE'], domain: dom }, { mode: 'diverging', columns: null, domain: null }],
    []
  ]);
});

test('titles paint through the title slots when they are loaded', () => {
  const painted = [];
  const env = loadTable({ before: (win) => {
    win.Blockr.viz.gear = { ...win.Blockr.viz.gear, mount: () => ({ update() {}, dispose() {} }) };
    win.Blockr.viz.titles = { slots: (o) => ({
      paint: (el, band) => { painted.push([el.className, band && band.text]); el.textContent = band ? band.text : ''; },
      dispose() {}
    }) };
  } });
  const t = mount(payload({ data: flatModel(2), derived: { titles: {
    title: { text: 'AEs by Arm', parts: [{ text: 'AEs by ' }, { text: 'Arm', arg: 'by' }] },
    subtitle: null, caption: { text: 'Source' } } } }), env);
  assert.deepEqual(painted, [['dd-table-title', 'AEs by Arm'], ['dd-table-subtitle', null]]);
  assert.equal(t.$('.dd-table-caption').textContent, 'Source');
  assert.ok(t.$('.dd-table-subtitle').hidden);
});

test('the download control lists the formats R offers', () => {
  const dl = [{ id: 'tbl_xlsx', label: 'Excel', ext: 'xlsx' }, { id: 'tbl_html', label: 'Web page', ext: 'html' }];
  const t = mount(payload({ data: flatModel(2), derived: { downloads: dl }, dataRev: 4 }));
  assert.deepEqual(t.$$('.dt-download-slot a.shiny-download-link').map((a) => a.id), ['tbl_xlsx', 'tbl_html']);
  t.render({ dataRev: 4, config: { download: false }, derived: { downloads: dl } });
  assert.ok(t.$('.dt-download-slot').hidden);
  assert.equal(t.$$('.dt-download-slot a').length, 0);
});

/** Count what a page holds: listeners, observers, timers and frames. */
function counting(win) {
  const c = { listeners: [], observers: 0, timers: new Set(), frames: new Set() };
  const cap = (o) => !!(o === true || (o && o.capture));
  // happy-dom's nodes and its window inherit addEventListener from two
  // EventTarget prototypes, neither of them win.EventTarget's.
  const owner = (x) => {
    let p = Object.getPrototypeOf(x);
    while (p && !Object.prototype.hasOwnProperty.call(p, 'addEventListener')) p = Object.getPrototypeOf(p);
    return p;
  };
  for (const ET of new Set([owner(win.document), owner(win)])) {
    const add = ET.addEventListener;
    const rem = ET.removeEventListener;
    ET.addEventListener = function (type, fn, o) {
      c.listeners.push([this, type, fn, cap(o)]);
      return add.call(this, type, fn, o);
    };
    ET.removeEventListener = function (type, fn, o) {
      const k = c.listeners.findIndex((l) => l[0] === this && l[1] === type && l[2] === fn &&
                                             l[3] === cap(o));
      if (k >= 0) c.listeners.splice(k, 1);
      return rem.call(this, type, fn, o);
    };
  }
  for (const name of ['ResizeObserver', 'MutationObserver']) {
    const Base = win[name];
    win[name] = class extends Base {
      observe(...a) { c.observers += 1; this._on = true; return super.observe(...a); }
      disconnect() { if (this._on) c.observers -= 1; this._on = false; return super.disconnect(); }
    };
  }
  const st = win.setTimeout.bind(win);
  const ct = win.clearTimeout.bind(win);
  win.setTimeout = (fn, ms) => {
    const id = st(() => { c.timers.delete(id); fn(); }, ms);
    c.timers.add(id);
    return id;
  };
  win.clearTimeout = (id) => { c.timers.delete(id); ct(id); };
  const raf = win.requestAnimationFrame.bind(win);
  const caf = win.cancelAnimationFrame.bind(win);
  win.requestAnimationFrame = (fn) => {
    const id = raf((t) => { c.frames.delete(id); fn(t); });
    c.frames.add(id);
    return id;
  };
  win.cancelAnimationFrame = (id) => { c.frames.delete(id); caf(id); };
  return c;
}

test('50 mounts and unmounts leave nothing behind', async () => {
  let c;
  const env = loadTable({ before: (win) => { c = counting(win); } });
  const { win, V, b, add, trigger } = env;
  const snap = () => ({ listeners: c.listeners.length,
                        page: c.listeners.filter((l) => l[0] === win.document || l[0] === win).length,
                        observers: c.observers,
                        timers: c.timers.size, frames: c.frames.size, scopes: V.Scope.open() });
  const cycle = async (k) => {
    const el = add('tbl');
    const look = k % 2 ? 'cards' : 'table';
    b.renderValue(el, payload({ data: withCards(500), config: { look, drill: 'ARM' } }));
    const row = el.querySelector('tbody tr.blockr-data-row, .tc-row');
    row.dispatchEvent(new win.MouseEvent('click', { bubbles: true }));
    row.dispatchEvent(new win.MouseEvent('mouseover', { bubbles: true }));
    const box = el.querySelector('.blockr-table-wrapper');
    box.dispatchEvent(new win.Event('scroll'));
    const inp = el.querySelector('input.blockr-search');
    inp.value = 'p';
    inp.dispatchEvent(new win.Event('input'));
    el.remove();
    trigger(el, 'shiny:unbound');
    await tick(win, 5);
  };
  const base = snap();
  await cycle(0);
  const one = snap();
  for (let k = 1; k < 50; k++) await cycle(k);
  const fifty = snap();
  assert.deepEqual(fifty, one);
  // One timer stays from the binding: it drops the last block's rows.
  assert.deepEqual(one, Object.assign({}, base, { timers: base.timers + 1 }));
});

test('the compact look turns the column titles on their side', () => {
  const p = payload({ data: flatModel(3), config: { look: 'compact' } });
  const t = mount(p);
  assert.equal(t.$('table.blockr-table').getAttribute('data-dt-rotate-titles'), 'on');
  t.render({ data: null, dataRev: p.data_rev, config: { look: 'table' } });
  assert.equal(t.$('table.blockr-table').getAttribute('data-dt-rotate-titles'), null);
  const el = t.win.document.body.appendChild(t.win.document.createElement('div'));
  t.V.table.renderStatic(el, payload({ data: flatModel(3), config: { look: 'compact' } }),
                         { gear: false, still: true });
  assert.equal(el.querySelector('table.blockr-table').getAttribute('data-dt-rotate-titles'), 'on');
});

test('table.css rotates the titles the compact look marks', () => {
  const fs = require('node:fs');
  const path = require('node:path');
  const css = fs.readFileSync(path.join(__dirname, '..', '..', '..', 'inst', 'css', 'table.css'), 'utf8');
  assert.match(css, /\.blockr-table\[data-dt-rotate-titles="on"\] thead th\.blockr-col-header \{/);
  assert.match(css, /\.blockr-table\[data-dt-rotate-titles="on"\] thead th\.blockr-col-header \.blockr-col-name \{[^}]*rotate\(180deg\)/);
  const dep = fs.readFileSync(path.join(__dirname, '..', '..', '..', 'R', 'table-dep.R'), 'utf8');
  assert.match(dep, /"table\.css"/);
});

test('the gear offers the blocks a drill can be sent to', () => {
  let host;
  const env = loadTable({ before: (win) => {
    win.Blockr.viz.gear = { ...win.Blockr.viz.gear,
      mount: (tray, o) => { host = o.host; return { update() {}, dispose() {} }; } };
  } });
  const targets = [{ value: 'b2', label: 'Listing' }, { value: 'b3', label: 'Chart' }];
  mount(payload({ data: flatModel(2), derived: { ctrl_choices: targets } }), env);
  assert.deepEqual(JSON.parse(JSON.stringify(host.choices('drill_target', {}))),
                   [{ value: '', label: 'None' }].concat(targets));
  assert.equal(host.choices('format', {}), null);
  // An empty picker says what it means.
  assert.equal(host.placeholder('columns', { layout: 'records' }), 'All columns');
  assert.equal(host.placeholder('by', { layout: 'records' }), 'All');
  assert.equal(host.placeholder('by', { layout: 'groups' }), null);
});

test('a prepare script draws its controls and its error under the header', () => {
  const env = loadTable({ before: (win) => {
    win.Blockr.viz.gear = { ...win.Blockr.viz.gear,
      mount: () => ({ update() {}, dispose() {}, values: () => ({}), set() {} }) };
  } });
  const script = { inputs: [{ key: 'n', name: 'n', label: 'Rows', kind: 'number' }],
                   values: { n: 10 }, error: 'boom' };
  const t = mount(payload({ data: flatModel(2), derived: { script } }), env);
  const band = t.$('.vz-table > .dd-mapping-band');
  assert.ok(band);
  assert.equal(band.hidden, false);
  assert.equal(band.querySelector('[data-role="n"] .blockr-label').textContent, 'Rows');
  assert.equal(band.querySelector('.dd-script-failed').textContent, 'Script failed: boom');
  t.render({ data: flatModel(2), derived: {} });
  assert.equal(band.hidden, true);
});
