/* Lifecycle: binding a container (with and without a payload waiting),
 * re-binding the same id the way a dock re-mount does, dispose, theme
 * changes (the drilldown-theme message and data-bs-theme on <html>),
 * resizing, and the facet grid at a few widths. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const h = require('./harness');
const I = require('./interact');

const AE = { group: 'AETERM', drill: 'auto' };
const cfg = (extra) => ({ ...I.BASE, chart_type: 'bar', ...AE, ...extra });

/** A fresh window on the virtual clock, nothing mounted. */
function bare(opts = {}) {
  const env = h.createEnv({ record: 'full', ...opts });
  const clock = I.useClock(env);
  return { env, clock };
}

/** The element tree under `el`, as class names, `depth` levels down. */
function skeleton(el, depth = 3) {
  return Array.from(el.children)
    .filter((c) => c.tagName !== 'svg' && c.tagName !== 'SVG')
    .map((c) => {
      const name = c.tagName.toLowerCase() + (c.className ? '.' + String(c.className).trim().split(/\s+/).join('.') : '');
      const kids = depth > 1 ? skeleton(c, depth - 1) : [];
      const o = { [name]: kids.length ? kids : null };
      if (c.style && c.style.display === 'none') o.hidden = true;
      return o;
    });
}

const counts = (env) => ({
  instances: env.fake.instances.length,
  disposed: env.fake.instances.filter((i) => i.isDisposed()).length,
  themes: env.fake.instances.map((i) => i.theme)
});

test('bind with nothing waiting: the card, and the _ready announce', () => {
  const { env } = bare();
  const { el } = I.mountRoot(env, I.ID, { download: true });
  env.binding.initialize(el);
  const ctx = I.makeCtx(env, null, el);
  const out = {
    inputs: env.inputs().map(I.cleanInput),
    // The mapping band and the download host are moved into the card.
    root: skeleton(el.parentElement, 4),
    footer: I.footer(el._block),
    gear: { expanded: el._block.gearBtn.getAttribute('aria-expanded'),
            label: el._block.gearBtn.getAttribute('aria-label') },
    familyClass: el.className,
    instances: counts(env)
  };
  void ctx;
  I.snap('life-bind-empty', out);
  env.close();
});

test('bind with a payload and a theme waiting: drawn, no announce', () => {
  const { env } = bare();
  const msg = h.message(env, I.ID, { columns: I.F.columns, data: I.F.data, config: cfg(),
                                     dataRev: 3 });
  env.handlers['drilldown-theme']({ id: I.ID, theme: 'dark' });
  env.handlers['drilldown-data'](msg);
  const before = counts(env);
  const { el } = I.mountRoot(env, I.ID);
  env.binding.initialize(el);
  I.snap('life-bind-pending', {
    before,
    after: counts(env),
    inputs: env.inputs().map(I.cleanInput),
    theme: el._block.theme,
    rev: el._block._lastDataRev,
    rows: el._block.data.length,
    familyClass: el.className
  });
  env.close();
});

test('dock re-mount: a new element under the same id', () => {
  const c = I.open(cfg());
  const first = c.block;
  const m = c.mark();
  c.el.parentElement.remove();
  const { el } = I.mountRoot(c.env, I.ID);
  c.env.binding.initialize(el);
  c.el = el;
  const afterMount = c.since(m);
  // R answers the announce with the rev the client already lost.
  c.env.handlers['drilldown-ready']({ id: I.ID, data_rev: 1 });
  const afterReady = c.since(m);
  c.send(c.cfg, { dataRev: 1 });
  const afterData = c.since(m);
  I.snap('life-remount', {
    sameBlock: first === c.block,
    oldSlotsKept: first._slots.length,
    afterMount: { inputs: afterMount.inputs, newInstances: afterMount.newInstances,
                  disposed: afterMount.disposed },
    afterReady: { inputs: afterReady.inputs.slice(afterMount.inputs.length) },
    afterData: { newInstances: afterData.newInstances, disposed: afterData.disposed,
                 slots: c.block._slots.length }
  });
  c.close();
});

test('initialize twice on one element', () => {
  const c = I.open(cfg());
  const first = c.block;
  const m = c.mark();
  c.env.binding.initialize(c.el);
  const s = c.since(m);
  I.snap('life-initialize-twice', {
    replaced: first !== c.block,
    inputs: s.inputs,
    disposed: s.disposed,
    cards: c.el.querySelectorAll(':scope > .dd-card').length,
    bandInCard: !!c.el.querySelector(':scope > .dd-card > .dd-mapping-band'),
    oldInstanceAlive: first._slots.map((x) => !x.chart.isDisposed()),
    newRows: c.block.data.length
  });
  c.close();
});

test('dispose', () => {
  const c = I.open(cfg({ facet: 'SEX' }));
  let disconnected = 0;
  const ro = c.block._resizeObserver;
  ro.disconnect = () => { disconnected++; };
  const m = c.mark();
  c.block.dispose();
  const s = c.since(m);
  I.snap('life-dispose', {
    disposed: s.disposed, instances: c.env.fake.instances.length,
    observerDisconnected: disconnected,
    slots: c.block._slots.length, charts: c.block.charts.length,
    grid: c.block.chartGrid.innerHTML,
    pending: c.clock.pending(),
    // The 300 ms resize queued by the last render still runs, on nothing.
    afterTimers: (() => { const m2 = c.mark(); c.advance(400); return c.since(m2).calls; })()
  });
  c.close();
});

test('drilldown-theme: re-creates the instances with the theme', () => {
  const r = I.runSteps(cfg({ facet: 'SEX' }), [
    ['theme dark', (c) => c.env.handlers['drilldown-theme']({ id: c.id, theme: 'dark' })],
    ['theme dark again', (c) => c.env.handlers['drilldown-theme']({ id: c.id, theme: 'dark' })],
    ['theme default', (c) => c.env.handlers['drilldown-theme']({ id: c.id, theme: 'default' })],
    ['theme null', (c) => c.env.handlers['drilldown-theme']({ id: c.id, theme: null })],
    ['theme for an id not on the page', (c) =>
      c.env.handlers['drilldown-theme']({ id: 'nope-drilldown_block', theme: 'dark' })]
  ], { extra: (c) => ({ theme: c.block.theme, instances: counts(c.env) }) });
  I.snap('life-theme-message', r);
});

test('drilldown-theme before any data: no draw', () => {
  const c = I.open(cfg(), { noSend: true });
  const m = c.mark();
  c.env.handlers['drilldown-theme']({ id: c.id, theme: 'dark' });
  const s = c.since(m);
  c.send(c.cfg);
  I.snap('life-theme-before-data', { before: s, theme: c.block.theme, after: counts(c.env) });
  c.close();
});

test('data-bs-theme on <html> redraws every chart holding rows', async () => {
  const { env, clock } = bare();
  const a = I.mountRoot(env, 'a-drilldown_block');
  env.binding.initialize(a.el);
  const b = I.mountRoot(env, 'b-drilldown_block');
  env.binding.initialize(b.el);
  h.send(env, 'a-drilldown_block', { columns: I.F.columns, data: I.F.data, config: cfg(), dataRev: 1 });
  const logs = () => env.fake.instances.map((i) => i.log.map((x) => (x.op === 'setOption' && x.notMerge ? 'redraw' : x.op)));
  const before = logs();
  const doc = env.win.document;
  doc.documentElement.setAttribute('data-bs-theme', 'dark');
  const sync = logs();
  await new Promise((r) => setImmediate(r));
  const afterHtml = logs();
  doc.body.setAttribute('data-bs-theme', 'light');
  await new Promise((r) => setImmediate(r));
  const afterBody = logs();
  doc.documentElement.setAttribute('data-other', 'x');
  await new Promise((r) => setImmediate(r));
  const afterOther = logs();
  void clock;
  I.snap('life-bs-theme', { before, sync, afterHtml, afterBody, afterOther,
                            bHasChart: !!b.el._block._slots.length });
  env.close();
});

// -- resize ------------------------------------------------------------------

test('resize: the 300 ms pass, hidden and empty containers, the observer', () => {
  const c = I.open(cfg({ facet: 'SEX' }));
  const out = {};
  const step = (label, fn) => { const m = c.mark(); fn(); out[label] = c.since(m).calls; };
  step('render queues a resize at 300 ms', () => c.advance(300));
  step('resize()', () => c.block.resize());
  step('hidden: offsetParent null', () => {
    Object.defineProperty(c.block.chartGrid, 'offsetParent', { configurable: true, get: () => null });
    c.block._resizeCharts();
    delete c.block.chartGrid.offsetParent;
  });
  step('zero width', () => {
    c.env.layout.width = 0;
    c.block._resizeCharts();
    c.env.layout.width = 800;
  });
  step('observer fires twice: one resize a frame later', () => {
    const ro = c.block._resizeObserver;
    ro.cb([]); ro.cb([]);
    out.queuedAfterObserver = c.clock.pending();
    c.advance(16);
  });
  step('open the gear', () => c.block.gearBtn.click());
  step('close the gear', () => c.block.gearBtn.click());
  I.snap('life-resize', out);
  c.close();
});

test('resize: a vertical bar re-fits its x labels to the new width', () => {
  const c = I.open(cfg({ orientation: 'vertical' }), { width: 320 });
  const out = { first: { height: c.slot().chartDiv.style.height, xFit: !!c.chart().__xFit } };
  for (const w of [1200, 320, 320, 600]) {
    c.env.layout.width = w;
    const m = c.mark();
    c.block._resizeCharts();
    out['width ' + w + (out['width ' + w] ? ' again' : '')] = {
      calls: c.since(m).calls, height: c.slot().chartDiv.style.height
    };
  }
  I.snap('life-refit', out);
  c.close();
});

test('facet grid at several widths and panel-column settings', () => {
  const out = {};
  for (const facetCols of [null, '1', '2', '5']) {
    for (const width of [320, 640, 1200]) {
      const c = I.open(cfg({ facet: 'ARM', facet_cols: facetCols }), { width });
      const d = h.domSummary(c.block);
      out[`cols=${facetCols} width=${width}`] = {
        grid: d.grid,
        panels: c.block._slots.map((s) => ({ facet: s.facetVal, width: s.chart.getWidth(),
                                             height: s.chartDiv.style.height }))
      };
      c.close();
    }
  }
  // One panel: always the single class, whatever facet_cols says.
  const c = I.open(cfg({ facet: 'ARM', facet_cols: '3' }), {
    data: { AETERM: I.F.data.AETERM.slice(0, 5), ARM: I.F.data.ARM.slice(0, 5) } });
  out['one panel, cols=3'] = { grid: h.domSummary(c.block).grid };
  c.close();
  I.snap('life-facet-grid', out);
});

test('an empty container never throws on resize', () => {
  const c = I.open(cfg(), { noSend: true });
  assert.doesNotThrow(() => c.block._resizeCharts());
  c.close();
});
