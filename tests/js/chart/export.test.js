/* Export and capture: what picture() composes (title band, panels, legend
 * band, caption) and where the picture goes (its promise, the `<id>_capture`
 * input, a failure), and the capture service's request and reply.
 *
 * happy-dom has no layout and no pixels. This file gives the window a small
 * layout of its own (getBoundingClientRect / offsetParent) and a canvas
 * context that records its calls, so the composition is checked as the calls
 * it makes. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const D = require('./drive');
const { F } = require('../cases');

const BAR = { type: 'bar', category: 'AETERM', orientation: 'horizontal', drill: 'auto' };

const LABEL_H = 20;   // facet label height in the stub layout
const LEGEND_H = 24;  // legend band height in the stub layout

const hiddenUp = (el) => {
  for (let e = el; e && e.nodeType === 1; e = e.parentElement) {
    if (e.style && e.style.display === 'none') return true;
  }
  return false;
};

/** A layout for the chart card, enough for picture() to measure. */
function stubLayout(env) {
  const win = env.win;
  const proto = win.HTMLElement.prototype;
  const rect = (left, top, width, height) => ({
    left, top, width, height, x: left, y: top, right: left + width, bottom: top + height
  });
  // The width the grid gets: the nearest ancestor with an inline px width
  // (the capture host sets one), else the window's layout width.
  const widthOf = (el) => {
    for (let e = el; e && e.nodeType === 1; e = e.parentElement) {
      const w = parseFloat(e.style && e.style.width);
      if (Number.isFinite(w) && /px$/.test(e.style.width)) return w;
    }
    return env.layout.width;
  };
  const gridOf = (grid) => {
    const facets = Array.from(grid.querySelectorAll(':scope > .vz-chart-facet'));
    const W = widthOf(grid);
    if (!facets.length) {
      const ch = grid.querySelector(':scope > .vz-chart-panel');
      return { W, cols: 1, cellW: W, cellH: ch ? ch.clientHeight : 0, n: 1, facets };
    }
    const m = /repeat\((\d+)/.exec(grid.style.gridTemplateColumns || '');
    const cols = m ? Number(m[1]) : Math.max(1, Math.min(facets.length, Math.floor(W / 300)));
    const ch = facets[0].querySelector('.vz-chart-panel');
    return { W, cols, cellW: Math.floor(W / cols), cellH: LABEL_H + (ch ? ch.clientHeight : 0),
             n: facets.length, facets };
  };
  Object.defineProperty(proto, 'offsetParent', {
    configurable: true,
    get() { return !this.isConnected || hiddenUp(this) ? null : win.document.body; }
  });
  proto.getBoundingClientRect = function () {
    if (!this.isConnected || hiddenUp(this)) return rect(0, 0, 0, 0);
    const grid = this.closest('.vz-chart-grid');
    if (this.classList.contains('vz-chart-grid')) {
      const g = gridOf(this);
      return rect(0, 100, g.W, Math.ceil(g.n / g.cols) * g.cellH);
    }
    if (grid) {
      const g = gridOf(grid);
      const gr = grid.getBoundingClientRect();
      const facet = this.closest('.vz-chart-facet');
      if (!facet) return rect(gr.left, gr.top, g.W, g.cellH);
      const i = g.facets.indexOf(facet);
      const left = gr.left + (i % g.cols) * g.cellW;
      const top = gr.top + Math.floor(i / g.cols) * g.cellH;
      if (this === facet) return rect(left, top, g.cellW, g.cellH);
      if (this.classList.contains('vz-chart-facet-label')) return rect(left, top, g.cellW, LABEL_H);
      return rect(left, top + LABEL_H, g.cellW, g.cellH - LABEL_H);
    }
    const band = this.closest('.vz-chart-legend');
    if (band) {
      const card = band.closest('.vz-frame');
      const gr = card.querySelector('.vz-chart-grid').getBoundingClientRect();
      const top = gr.top + gr.height;
      if (this === band) return rect(0, top, gr.width, LEGEND_H);
      const kids = Array.from(band.children);
      const chip = this.closest('.vz-chart-legend-chip, .vz-chart-legend-title');
      const i = kids.indexOf(chip);
      const left = 10 + 80 * i;
      if (this === chip) return rect(left, top + 4, 70, 16);
      if (this.classList.contains('vz-chart-legend-swatch')) return rect(left, top + 7, 10, 10);
      return rect(left + 14, top + 4, 56, 16);
    }
    return rect(0, 0, widthOf(this), this.clientHeight);
  };
}

/** Canvases whose 2d context records every call; toDataURL names its size. */
function recordingCanvas(env) {
  const win = env.win;
  const canvases = [];
  win.HTMLCanvasElement.prototype.getContext = function () {
    if (this.__ctx) return this.__ctx;
    const ops = [];
    const st = { font: '10px sans-serif', fillStyle: '#000', textAlign: 'left',
                 textBaseline: 'top', globalAlpha: 1 };
    const ctx = {
      ops,
      measureText: (t) => ({ width: 7 * String(t == null ? '' : t).length }),
      scale: (x, y) => ops.push(['scale', x, y]),
      fillRect: (x, y, w, h2) => ops.push(['fillRect', st.fillStyle, x, y, w, h2]),
      fillText: (t, x, y) => ops.push(['fillText', t, x, y, st.font, st.fillStyle,
                                       st.textAlign, st.textBaseline, st.globalAlpha]),
      drawImage: (im, x, y, w, h2) => ops.push(['drawImage', im && im.src, x, y, w, h2]),
      save() {}, restore() {}
    };
    for (const k of Object.keys(st)) {
      Object.defineProperty(ctx, k, { get: () => st[k], set: (v) => { st[k] = v; } });
    }
    this.__ctx = ctx;
    canvases.push(this);
    return ctx;
  };
  win.HTMLCanvasElement.prototype.toDataURL = function () {
    return `data:image/png;base64,COMPOSED-${this.width}x${this.height}`;
  };
  return canvases;
}

/** Instances hand out a data URL naming the panel and the ratio asked for. */
function panelUrls(env) {
  env.fake.instances.forEach((inst, i) => {
    inst.getDataURL = (o) => `data:image/png;base64,PANEL${i}-r${o && o.pixelRatio}` +
      `-${o && o.backgroundColor}-${JSON.stringify(o && o.excludeComponents)}`;
  });
}

/** The composed canvas: the one whose ops include a scale. */
const composed = (canvases) => {
  const c = canvases.find((x) => x.__ctx.ops.some((o) => o[0] === 'scale'));
  return c ? { width: c.width, height: c.height, ops: c.__ctx.ops } : null;
};

/** A chart with the stub layout and recording canvases. */
function prepare(config, opts = {}) {
  let canvases = null;
  const c = D.open(config, { ...opts, before: (env) => {
    stubLayout(env);
    canvases = recordingCanvas(env);
  } });
  panelUrls(c.env);
  return { c, canvases: /** @type {any[]} */ (canvases) };
}

const texts = (cv) => cv.ops.filter((o) => o[0] === 'fillText').map((o) => o[1]);
const images = (cv) => cv.ops.filter((o) => o[0] === 'drawImage').map((o) => o[1]);

test('compose: titles, one panel, caption, at the ratio asked for', async () => {
  const { c, canvases } = prepare(BAR, { width: 400, derived: { titles: {
    title: { text: 'Adverse events' }, subtitle: { text: 'Count of rows by Reported Term' },
    caption: { text: 'Safety population\nN = 60 records' } } } });
  let done = null;
  const pic = (view, opts) => c.env.win.Blockr.viz.chart.picture(view, opts);
  pic(c.block, { pixelRatio: 3 }).then((p) => { done = { url: p.png, w: p.width, h: p.height }; });
  await c.clock.advanceAsync(0);
  const cv = composed(canvases);
  assert.ok(done && /^data:image\/png;base64,COMPOSED-/.test(done.url));
  assert.strictEqual(cv.width, done.w * 3);
  const t = texts(cv);
  assert.ok(t.indexOf('Adverse events') < t.indexOf('Count of rows by Reported Term'));
  assert.ok(t.includes('Safety population') && t.includes('N = 60 records'));
  assert.deepStrictEqual(images(cv).map((u) => u.replace(/-r.*/, '')), ['data:image/png;base64,PANEL0']);
  assert.ok(images(cv)[0].includes('-r3-'), 'panels at the ratio asked for');
  c.close();
});

test('compose: every facet panel and the legend chips', async () => {
  const { c, canvases } = prepare({ ...BAR, color: 'AESEV', facet: 'ARM', facet_cols: 2 },
                                  { width: 800 });
  let done = false;
  c.env.win.Blockr.viz.chart.picture(c.block).then(() => { done = true; });
  await c.clock.advanceAsync(0);
  const cv = composed(canvases);
  assert.ok(done);
  assert.strictEqual(images(cv).length, c.block._slots.length);
  assert.ok(c.block._slots.length > 1);
  assert.ok(texts(cv).includes('MILD'), 'the legend band is drawn');
  c.close();
});

test('compose: reaching for the download sends the picture as <id>_capture', async () => {
  const downloads = [{ id: 'blk-dl_png', ext: 'png', label: 'Image' }];
  const { c } = prepare({ ...BAR, download: true }, { width: 300, derived: { downloads,
                                                                             capture_ratio: 4 } });
  const tool = c.block.frame.downloadSlot.querySelector('.blockr-tool, .blockr-action-menu__trigger');
  tool.dispatchEvent(new c.env.win.Event('pointerdown'));
  await c.clock.advanceAsync(0);
  const sent = c.inputs().filter((x) => x.name === D.ID + '_capture:blockr.viz.capture');
  assert.strictEqual(sent.length, 1);
  assert.match(sent[0].value.png, /^data:image\/png;base64,COMPOSED-/);
  assert.ok(sent[0].value.width > 0 && sent[0].value.height > 0);
  c.close();
});

test('compose: a chart with nothing drawn says so', async () => {
  const { c } = prepare({ ...BAR, category: 'NOPE' }, { width: 300 });
  let err = null;
  c.env.win.Blockr.viz.chart.picture(c.block).then(() => { err = 'drawn'; }, (e) => { err = e.message; });
  await c.clock.advanceAsync(0);
  assert.strictEqual(err, 'the chart has nothing drawn');
  c.close();
});

/** A capture request for `config`, as R's chart_capture_service() sends it. */
const captureMsg = (env, config, extra = {}) => env.win.JSON.parse(JSON.stringify({
  state_rev: 1, data_rev: 9, columns: F.columns, derived: {},
  config: { ...require('../cases').BASE, ...config }, data: F.data,
  req: 'tok-1', width: 640, height: 360, ratio: 2.5, ...extra
}));
const replies = (c) => c.inputs().filter((x) => x.name === 'blockr_viz_capture_result');
const hosts = (c) => c.doc.querySelectorAll('.vz-capture-host').length;

test('capture: a picture once the panels finished rendering, one reply, the host removed', async () => {
  const { c } = prepare(BAR, { width: 800 });
  const init = c.env.fake.api.init;
  c.env.win.echarts.init = (...a) => {
    const inst = init(...a);
    inst.getDataURL = () => 'data:image/png;base64,CAPTURE';
    return inst;
  };
  c.env.handlers['blockr-viz:capture'](captureMsg(c.env, BAR));
  assert.strictEqual(hosts(c), 1);
  assert.strictEqual(replies(c).length, 0, 'not before the panels finished rendering');
  await c.clock.advanceAsync(20);
  assert.strictEqual(replies(c).length, 1);
  assert.strictEqual(c.env.fake.instances.at(-1).getOption().animation, false, 'drawn still');
  assert.strictEqual(replies(c)[0].value.req, 'tok-1');
  assert.match(replies(c)[0].value.png, /^data:image\/png;base64,COMPOSED-/);
  assert.strictEqual(hosts(c), 0);
  await c.clock.advanceAsync(20000);
  assert.strictEqual(replies(c).length, 1, 'the timeout adds no second reply');
  c.close();
});

test('capture: a chart that cannot draw answers at once with why', async () => {
  const { c } = prepare(BAR, { width: 800 });
  c.env.handlers['blockr-viz:capture'](captureMsg(c.env, { ...BAR, category: 'NOPE' }, { req: 'tok-2' }));
  await c.clock.advanceAsync(6000);
  const r = replies(c);
  assert.strictEqual(r.length, 1);
  assert.strictEqual(r[0].value.req, 'tok-2');
  assert.match(r[0].value.error, /nothing drawn/);
  assert.strictEqual(hosts(c), 0);
  c.close();
});

test('capture: two requests in flight get their own replies', async () => {
  const { c } = prepare(BAR, { width: 800 });
  c.env.handlers['blockr-viz:capture'](captureMsg(c.env, BAR, { req: 'a', width: 300, height: 200 }));
  c.env.handlers['blockr-viz:capture'](captureMsg(c.env, { type: 'pie', category: 'AESEV' },
                                                  { req: 'b', width: 500, height: 300 }));
  assert.strictEqual(hosts(c), 2);
  await c.clock.advanceAsync(1500);
  assert.deepStrictEqual(replies(c).map((x) => x.value.req).sort(), ['a', 'b']);
  assert.strictEqual(hosts(c), 0);
  c.close();
});
