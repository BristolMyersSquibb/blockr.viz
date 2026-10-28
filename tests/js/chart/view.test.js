/* The chart view around the drawing: its gear (the core engine with the
 * chart's hooks), the titles and their words, the download control, the
 * theme, rows kept per data revision, errors, the capture service, and
 * teardown: 50 mounts and unmounts leave nothing behind. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const D = require('./drive');

const BAR = { type: 'bar', category: 'AETERM', orientation: 'horizontal', drill: 'auto' };
const rows = (b) => Array.from(b.frame.tray.band.querySelectorAll('.vz-row[data-role]'),
                               (r) => r.getAttribute('data-role'));
const row = (b, key) => b.frame.tray.band.querySelector('.vz-row[data-role="' + key + '"]');

test('the gear shows the rows the spec offers for the chart type', () => {
  const c = D.open(BAR);
  const bar = rows(c.block);
  for (const k of ['type', 'func', 'sort_by', 'orientation', 'value_labels', 'drill']) {
    assert.ok(bar.includes(k), k);
  }
  for (const k of ['smoother', 'connect', 'bins', 'percent_of']) assert.ok(!bar.includes(k), k);
  assert.strictEqual(row(c.block, 'category').querySelector('.blockr-label').textContent, 'Category *');
  assert.strictEqual(row(c.block, 'value'), null, 'a count reads no value');
  c.send({ ...BAR, type: 'scatter', x: 'ADY', y: 'AVAL' });
  const sc = rows(c.block);
  assert.ok(sc.includes('smoother') && !sc.includes('func'));
  c.send({ ...BAR, func: 'pct_distinct', value: 'USUBJID' });
  assert.ok(rows(c.block).includes('percent_of'), 'a share asks what it is a share of');
  c.close();
});

test('the type tiles name their types with a capital, as 0.2 did', () => {
  const c = D.open(BAR);
  const tile = (v) => row(c.block, 'type').querySelector('[data-value="' + v + '"]').textContent.trim();
  assert.strictEqual(tile('bar'), 'Bar');
  assert.strictEqual(tile('boxplot'), 'Boxplot');
  assert.strictEqual(tile('scatter'), 'Scatter');
  c.close();
});

test('a gear edit goes to R as the changed key, with the payload revision', () => {
  const c = D.open(BAR);
  c.send(c.config, { stateRev: 7 });
  row(c.block, 'type').querySelector('[data-value="line"]').click();
  assert.deepStrictEqual(D.plain(c.sets()), [{ type: 'line' }]);
  const msg = c.inputs().find((x) => x.name === D.ID + '_set:blockr.viz.chart');
  assert.strictEqual(msg.value.rev, 7);
  c.close();
});

test('the gear hooks: type groups, drill words, sort orders, drill target', () => {
  const c = D.open(BAR, { derived: { ctrl_choices: [{ value: 'flt_ae', label: 'Filter AE' }] } });
  const heads = Array.from(row(c.block, 'type').querySelectorAll('.vz-tile-group-head'),
                           (e) => e.textContent);
  assert.deepStrictEqual(heads, ['Aggregated', 'Individual']);
  const texts = (key) => c.block.gear.slot(key).options.map((o) => o.label);
  assert.ok(texts('drill').includes('Auto, the clicked group'));
  assert.ok(texts('sort_by').includes('Data order'));
  assert.ok(texts('drill_target').includes('Filter AE'));
  c.send({ ...BAR, drill: null });
  assert.ok(!rows(c.block).includes('drill_target'), 'no target without a drill');
  c.close();
});

test('titles: R\'s words are live where the gear has a control, offers after them', () => {
  const titles = {
    title: { text: 'AEs by AETERM', parts: [{ text: 'AEs by ' }, { text: 'AETERM', arg: 'y' }],
             offers: ['color'] },
    subtitle: null,
    caption: { text: 'N = 60' },
    args: { y: 'AETERM' }
  };
  const c = D.open(BAR, { derived: { titles } });
  const b = c.block;
  assert.strictEqual(b.frame.titleEl.querySelector('.blockr-slot').textContent, 'AETERM');
  assert.strictEqual(b.frame.titleEl.querySelector('.blockr-slot-offer').textContent, '+ Color');
  assert.strictEqual(b.frame.subEl.hidden, true);
  assert.strictEqual(b.frame.capEl.textContent, 'N = 60');
  assert.strictEqual(c.env.win.Blockr.viz.chart.titleText(b.derived.titles).title, 'AEs by AETERM');
  c.close();
});

test('downloads: the formats R offers, while download is on', () => {
  const downloads = [{ id: 'blk-dl_xlsx', ext: 'xlsx', label: 'Excel' },
                     { id: 'blk-dl_png', ext: 'png', label: 'Image' }];
  const c = D.open({ ...BAR, download: true }, { derived: { downloads } });
  assert.ok(c.block.frame.downloadSlot.querySelector('#blk-dl_xlsx'));
  assert.strictEqual(c.block.frame.downloadSlot.hidden, false);
  c.send({ ...c.config, download: false });
  assert.strictEqual(c.block.frame.downloadSlot.hidden, true);
  assert.strictEqual(c.block.frame.downloadSlot.querySelector('#blk-dl_xlsx'), null);
  c.close();
});

test('an empty state offers no Image: there is no picture to take', () => {
  const downloads = [{ id: 'blk-dl_xlsx', ext: 'xlsx', label: 'Excel' },
                     { id: 'blk-dl_png', ext: 'png', label: 'Image' }];
  const c = D.open({ ...BAR, category: null, download: true }, { derived: { downloads } });
  assert.match(c.block.chartGrid.textContent, /^Pick/);
  assert.ok(c.block.frame.downloadSlot.querySelector('#blk-dl_xlsx'));
  assert.strictEqual(c.block.frame.downloadSlot.querySelector('#blk-dl_png'), null);
  c.send({ ...c.config, category: 'AETERM' }, { derived: { downloads } });
  assert.ok(c.block.frame.downloadSlot.querySelector('#blk-dl_png'), 'drawn again: Image is back');
  c.close();
});

test('the theme R names is the ECharts theme; a new one draws new instances', () => {
  const c = D.open(BAR, { derived: { theme: 'dark' } });
  assert.strictEqual(c.chart().theme, 'dark');
  const first = c.chart();
  c.send(c.config, { derived: { theme: 'vintage' } });
  assert.strictEqual(c.chart().theme, 'vintage');
  assert.notStrictEqual(c.chart(), first);
  c.send(c.config, { derived: { theme: 'default' } });
  assert.strictEqual(c.chart().theme, null, "'default' is ECharts' own theme");
  c.close();
});

test('a payload without rows draws from the rows of its data revision', () => {
  const c = D.open(BAR);
  const first = c.block.data;
  const ix = c.block._ix;
  c.send({ ...BAR, color: 'AESEV' }, { data: null, dataRev: 1 });
  assert.strictEqual(c.block.data, first);
  assert.strictEqual(c.block._ix, ix, 'held rows are not parsed or indexed again');
  assert.ok(c.block.legend.el.querySelector('.vz-chart-legend-chip'));
  c.send(BAR, { data: null, dataRev: 2 });
  assert.ok(c.inputs().some((x) => x.name === D.ID + '_need' && x.value === 2),
            'rows it does not hold are asked for');
  c.close();
});

test('an error R sends is said where the chart would be', () => {
  const c = D.open(BAR);
  c.env.binding.renderError(c.el, { message: 'object "AETERM" not found' });
  assert.match(c.block.chartGrid.textContent, /object "AETERM" not found/);
  c.close();
});

test('the capture service draws a chart off screen and replies once', () => {
  const c = D.open(BAR);
  const handler = c.env.handlers['blockr-viz:capture'];
  const msg = { state_rev: 1, data_rev: 9, columns: require('../cases').F.columns, derived: {},
                config: BAR, data: require('../cases').F.data, req: 'r1', width: 640, height: 360 };
  handler(c.env.win.JSON.parse(JSON.stringify(msg)));
  assert.ok(c.doc.querySelector('.vz-capture-host'));
  return new Promise((resolve) => setImmediate(resolve)).then(async () => {
    for (let i = 0; i < 40; i++) {
      c.advance(100);
      await new Promise((r) => setImmediate(r));
    }
    const reply = c.inputs().filter((x) => x.name === 'blockr_viz_capture_result');
    assert.strictEqual(reply.length, 1);
    assert.strictEqual(reply[0].value.req, 'r1');
    // happy-dom lays nothing out, so the grid has no size and the reply says so.
    assert.ok(reply[0].value.png || /no size|nothing drawn/.test(reply[0].value.error));
    assert.strictEqual(c.doc.querySelector('.vz-capture-host'), null, 'the host is gone');
    c.close();
  });
});

test('50 mounts and unmounts leave no listener, timer, scope, node or chart behind', () => {
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
  for (let i = 0; i < 50; i++) {
    const m = h.mount(c.env, 'cycle-' + i);
    h.send(c.env, m.id, { columns: require('../cases').F.columns, data: require('../cases').F.data,
                          config: { ...BAR, color: 'AESEV', facet: 'SEX' } });
    const b = m.block;
    b._slots[0].chart.trigger('click', D.paramsAt(b._slots[0].chart, 0, 0));
    b.frame.tray.set(true);
    b.gear.set({ func: 'mean' });
    m.el.parentElement.remove();
    m.el.__jq['shiny:unbound']();
    c.advance(10000);
  }
  assert.deepStrictEqual({ listeners: live.size, timers: c.clock.timers.length,
                           scopes: V.Scope.open(), nodes: c.doc.body.querySelectorAll('*').length,
                           observers: observers.size }, base);
  assert.ok(c.env.fake.instances.every((i) => i.isDisposed()), 'every ECharts instance disposed');
  c.close();
});

test('a light/dark switch redraws with the new ink, without a payload', async () => {
  const c = D.open(BAR);
  const before = c.chart().log.filter((x) => x.op === 'setOption').length;
  c.el.style.setProperty('--blockr-color-text-default', '#eeeeee');
  c.doc.documentElement.setAttribute('data-bs-theme', 'dark');
  await Promise.resolve();  // the observer reports in a microtask
  assert.strictEqual(c.block._ink.text, '#eeeeee');
  assert.ok(c.chart().log.filter((x) => x.op === 'setOption').length > before, 'drawn again');
  c.doc.documentElement.removeAttribute('data-bs-theme');
  c.close();
});

test('every chart type: the gear rows are the spec rows that apply to it', () => {
  const c = D.open(BAR);
  const spec = c.env.win.Blockr.viz.spec.chart;
  // Rows the chart's hooks hold back: a share's base, the drill target.
  const held = ['percent_of', 'drill_target', 'drill_table'];
  const set = ['category', 'value', 'x', 'y', 'color', 'facet', 'drill'];
  for (const type of spec.type.values) {
    // A mean, so an aggregated chart's value row shows (a count hides it).
    c.send({ ...BAR, type, category: 'AETERM', value: 'AVAL', func: 'mean', x: 'ADY', y: 'AVAL',
             color: 'ARM', facet: 'SEX' });
    const want = Object.keys(spec).filter((k) => {
      const a = spec[k];
      if (!a.gear || a.gear.control === 'none' || held.includes(k)) return false;
      if (a.gear.group === 'Add mapping' && !set.includes(k)) return false;
      const ap = a.applies;
      if (ap && ap.only && !ap.only.includes(type)) return false;
      if (ap && ap.except && ap.except.includes(type)) return false;
      return !a.gear.when || set.includes(a.gear.when);
    });
    const got = rows(c.block).filter((k) => !held.includes(k));
    assert.deepStrictEqual(got.slice().sort(), want.sort(), type);
  }
  c.close();
});

test('the spec hides what does nothing: stacking without a colour, value lines on a pie', () => {
  const c = D.open(BAR);
  assert.ok(!rows(c.block).includes('position'), 'no colour: nothing to stack');
  c.send({ ...BAR, color: 'AESEV' });
  assert.ok(rows(c.block).includes('position'));
  assert.ok(rows(c.block).includes('value_lines'));
  for (const type of ['pie', 'treemap']) {
    c.send({ type, category: 'AETERM' });
    assert.ok(!rows(c.block).includes('value_lines'), type);
  }
  c.close();
});

test('with no category, a bar lies horizontal and asks for one', () => {
  const c = D.open({ type: 'bar', category: null, value: null });
  assert.match(c.block.chartGrid.textContent, /^Pick Category to plot/);
  assert.strictEqual(c.block.settings.orientation, 'horizontal');
  c.close();
});

test('the gear offers every column of the input, not only those whose rows were sent', () => {
  const { F } = require('../cases');
  const c = D.open(BAR);
  require('../harness').send(c.env, D.ID, { columns: F.columns, data: { AETERM: F.data.AETERM },
                                            config: BAR });
  const names = c.block.gear.slot('color').options.map((o) => o.value);
  assert.ok(names.includes('USUBJID') && names.includes('AVAL'), names.join(' '));
  c.close();
});

test('a histogram stands its bins on x', () => {
  const { F } = require('../cases');
  const bins = F.data.ADY.map((v) => (v == null ? null : (v < 30 ? '0-30' : '30-90')));
  const cols = F.columns.concat([{ name: 'ADY_bin', class: 'string', n_unique: 2,
                                   levels: ['0-30', '30-90'] }]);
  const c = D.open({ type: 'histogram', x: 'ADY' }, { columns: cols,
                                                       data: { ...F.data, ADY_bin: bins } });
  const o = c.chart().getOption();
  assert.strictEqual(o.xAxis[0].type, 'category');
  assert.deepStrictEqual(D.plain(o.xAxis[0].data), ['0-30', '30-90']);
  c.close();
});

test('facet_cols sets the panels per row; unset, the grid fills by width', () => {
  const grid = (c) => c.block.chartGrid.style.gridTemplateColumns;
  const c = D.open({ ...BAR, facet: 'ARM', facet_cols: 2 });
  assert.match(grid(c), /^repeat\(2, /);
  c.send({ ...BAR, facet: 'ARM', facet_cols: null });
  assert.strictEqual(grid(c), '');
  c.send({ ...BAR, facet: 'ARM', facet_cols: 9 });
  const panels = c.block._slots.length;
  assert.ok(panels > 1);
  assert.match(grid(c), new RegExp('^repeat\\(' + Math.min(9, panels) + ', '));
  c.close();
});

test('a type switch leaves no listener on the reset button of a chart it took down', () => {
  const c = D.open(BAR);
  const win = c.env.win;
  // Click listeners on zoom reset buttons, by button.
  const live = new Map();
  // The prototype that holds addEventListener, as the window's elements see it.
  let proto = win.document.createElement('button');
  while (!Object.prototype.hasOwnProperty.call(proto, 'addEventListener')) proto = Object.getPrototypeOf(proto);
  const add = proto.addEventListener;
  const rm = proto.removeEventListener;
  proto.addEventListener = function (t, f, o) {
    if (this.classList && this.classList.contains('vz-chart-zoom-reset')) {
      live.set(this, (live.get(this) || 0) + 1);
    }
    return add.call(this, t, f, o);
  };
  proto.removeEventListener = function (t, f, o) {
    if (live.has(this)) live.set(this, live.get(this) - 1);
    return rm.call(this, t, f, o);
  };
  const SCATTER = { type: 'scatter', x: 'ADY', y: 'AVAL', drill: 'auto' };
  for (let i = 0; i < 3; i++) {
    c.send({ ...SCATTER });
    c.send({ ...SCATTER, type: 'line' });
    c.send(BAR);
  }
  const held = [...live].filter(([btn, n]) => n > 0 && !btn.isConnected);
  proto.addEventListener = add;
  proto.removeEventListener = rm;
  assert.ok(live.size > 0, 'the switches built reset buttons');
  assert.strictEqual(held.length, 0);
  c.close();
});
