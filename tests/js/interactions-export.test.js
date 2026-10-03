/* Export and capture: what _downloadImage composes (title band, panels,
 * legend band, caption) and where the picture goes (the done callback, the
 * `<id>_capture` input, a browser download), and the drilldown-capture
 * handler's request/reply cycle.
 *
 * happy-dom has no layout and no pixels. This file gives the window a small
 * layout of its own (getBoundingClientRect / offsetParent, below) and a
 * canvas context that records its calls, so the composition is recorded as
 * the calls it makes, not as an image. The geometry is the stub's; what the
 * snapshot pins is which pieces are drawn, in which order, from which text,
 * and at which offsets relative to that geometry. */
'use strict';

const test = require('node:test');
const h = require('./harness');
const I = require('./interact');

const AE = { group: 'AETERM', drill: 'auto' };
const cfg = (extra) => ({ ...I.BASE, chart_type: 'bar', ...AE, ...extra });

const LABEL_H = 20;   // facet label height in the stub layout
const LEGEND_H = 24;  // legend band height in the stub layout

const hiddenUp = (el) => {
  for (let e = el; e && e.nodeType === 1; e = e.parentElement) {
    if (e.style && e.style.display === 'none') return true;
  }
  return false;
};

/** A layout for the chart card, enough for _downloadImage to measure. */
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
    const facets = Array.from(grid.querySelectorAll(':scope > .dd-facet'));
    const W = widthOf(grid);
    if (!facets.length) {
      const ch = grid.querySelector(':scope > .dd-chart');
      return { W, cols: 1, cellW: W, cellH: ch ? ch.clientHeight : 0, n: 1, facets };
    }
    const m = /repeat\((\d+)/.exec(grid.style.gridTemplateColumns || '');
    const cols = m ? Number(m[1]) : Math.max(1, Math.min(facets.length, Math.floor(W / 300)));
    const ch = facets[0].querySelector('.dd-chart');
    return { W, cols, cellW: Math.floor(W / cols), cellH: LABEL_H + (ch ? ch.clientHeight : 0),
             n: facets.length, facets };
  };
  Object.defineProperty(proto, 'offsetParent', {
    configurable: true,
    get() { return !this.isConnected || hiddenUp(this) ? null : win.document.body; }
  });
  proto.getBoundingClientRect = function () {
    if (!this.isConnected || hiddenUp(this)) return rect(0, 0, 0, 0);
    const grid = this.closest('.dd-chart-grid');
    if (this.classList.contains('dd-chart-grid')) {
      const g = gridOf(this);
      return rect(0, 100, g.W, Math.ceil(g.n / g.cols) * g.cellH);
    }
    if (grid) {
      const g = gridOf(grid);
      const gr = grid.getBoundingClientRect();
      const facet = this.closest('.dd-facet');
      if (!facet) return rect(gr.left, gr.top, g.W, g.cellH);
      const i = g.facets.indexOf(facet);
      const left = gr.left + (i % g.cols) * g.cellW;
      const top = gr.top + Math.floor(i / g.cols) * g.cellH;
      if (this === facet) return rect(left, top, g.cellW, g.cellH);
      if (this.classList.contains('dd-facet-label')) return rect(left, top, g.cellW, LABEL_H);
      return rect(left, top + LABEL_H, g.cellW, g.cellH - LABEL_H);
    }
    const band = this.closest('.dd-legend-band');
    if (band) {
      const card = band.closest('.dd-card');
      const gr = card.querySelector('.dd-chart-grid').getBoundingClientRect();
      const top = gr.top + gr.height;
      if (this === band) return rect(0, top, gr.width, LEGEND_H);
      const kids = Array.from(band.children);
      const chip = this.closest('.dd-legend-chip, .dd-legend-title');
      const i = kids.indexOf(chip);
      const left = 10 + 80 * i;
      if (this === chip) return rect(left, top + 4, 70, 16);
      if (this.classList.contains('dd-legend-swatch')) return rect(left, top + 7, 10, 10);
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

function prepare(config, opts = {}) {
  const c = I.open(config, { noSend: true, ...opts });
  stubLayout(c.env);
  const canvases = recordingCanvas(c.env);
  c.send(c.cfg);
  panelUrls(c.env);
  return { c, canvases };
}

test('compose: titles, one panel, caption, the done callback', async () => {
  const { c, canvases } = prepare(cfg({
    title_resolved: 'Adverse events', subtitle_resolved: 'Count of rows by Reported Term',
    caption_resolved: 'Safety population\nN = 60 records and a long second line that has to wrap at the grid width'
  }), { width: 400 });
  let done = null;
  c.block._downloadImage(false, { pixelRatio: 3, done: (url, w, h2) => { done = { url, w, h: h2 }; } });
  await c.clock.advanceAsync(0);
  I.snap('export-compose-titles', { done, lastCapture: c.block._lastCapture,
                                    canvas: composed(canvases), inputs: c.since({ inputs: 0, logs: [], instances: 0 }).inputs.filter((i) => !/_ready$/.test(i.name)) });
  c.close();
});

test('compose: facet panels and the legend band, default ratio', async () => {
  const { c, canvases } = prepare(cfg({ color: 'AESEV', facet: 'ARM', facet_cols: '2' }),
                                  { width: 800 });
  // One level toggled off: the chip's opacity rides into the export.
  c.block.legendEl.querySelector('.dd-legend-chip').click();
  let done = null;
  c.block._downloadImage(false, { done: (url, w, h2) => { done = { url, w, h: h2 }; } });
  await c.clock.advanceAsync(0);
  I.snap('export-compose-facets', { done, canvas: composed(canvases) });
  c.close();
});

test('compose: capture_ratio from the config, sent to R as <id>_capture', async () => {
  const { c } = prepare(cfg({ capture_export: true, capture_ratio: 4 }),
                        { width: 300, download: true });
  const m = c.mark();
  // The download tool R rendered, hoisted into the gear header: opening it
  // composes and sends.
  const tool = c.block.gearHeader.querySelector('.dd-chart-dl .blockr-tool');
  tool.click();
  await c.clock.advanceAsync(0);
  const sent = c.since(m).inputs;
  // Without capture_export the same click composes nothing.
  c.send({ ...c.cfg, capture_export: false });
  const m2 = c.mark();
  tool.click();
  await c.clock.advanceAsync(0);
  I.snap('export-capture-input', { sent, withoutCaptureExport: c.since(m2).inputs,
                                   hoisted: !!c.block.gearHeader.querySelector('.dd-chart-dl') });
  c.close();
});

test('compose: no callback and no send saves chart.png in the browser', async () => {
  const { c } = prepare(cfg(), { width: 300 });
  const clicks = [];
  c.env.win.HTMLAnchorElement.prototype.click = function () {
    clicks.push({ href: this.href, download: this.download });
  };
  c.block._downloadImage();
  await c.clock.advanceAsync(0);
  I.snap('export-browser-download', { clicks });
  c.close();
});

test('compose: nothing to compose', async () => {
  const { c } = prepare(cfg({ group: 'NOPE' }), { width: 300 });
  let called = false;
  c.block._downloadImage(false, { done: () => { called = true; } });
  await c.clock.advanceAsync(0);
  I.snap('export-nothing', { called, slots: c.block._slots.length });
  c.close();
});

// -- drilldown-capture -------------------------------------------------------

function captureEnv() {
  const env = h.createEnv({ record: 'full', width: 800 });
  const clock = I.useClock(env);
  stubLayout(env);
  const canvases = recordingCanvas(env);
  // Panels of capture charts are created inside the handler; give each a
  // URL as it is made.
  const init = env.fake.api.init;
  env.win.echarts.init = (...a) => {
    const inst = init(...a);
    const i = env.fake.instances.length - 1;
    inst.getDataURL = (o) => `data:image/png;base64,PANEL${i}-r${o && o.pixelRatio}`;
    return inst;
  };
  return { env, clock, canvases };
}

const captureMsg = (env, extra) => env.win.JSON.parse(JSON.stringify({
  req: 'tok-1', width: 640, height: 360, ratio: 2.5,
  columns: I.F.columns, data: I.F.data, data_rev: 9, config: cfg(), ...extra
}));

// Note: the panels inside a capture host are still drawn at the window's
// layout width (the harness's clientWidth ignores inline widths); only the
// composition reads the host's width, through the stub layout above.
const results = (env) => env.inputs().filter((i) => i.name === 'blockr_viz_capture_result');
const hosts = (env) => Array.from(env.win.document.querySelectorAll('.blockr-capture-host'),
  (w) => ({ style: w.getAttribute('style'),
            chart: { id: w.firstElementChild.id, style: w.firstElementChild.getAttribute('style'),
                     cls: w.firstElementChild.className } }));

test('drilldown-capture: a picture, one reply, the host removed', async () => {
  const { env, clock, canvases } = captureEnv();
  env.handlers['drilldown-capture'](captureMsg(env, {
    config: cfg({ title_resolved: 'Adverse events' }) }));
  const out = { hostsWhileDrawing: hosts(env), pending: clock.pending() };
  await clock.advanceAsync(1399);
  out.repliesBefore1400 = results(env);
  await clock.advanceAsync(1);
  out.replies = results(env);
  out.hostsAfter = hosts(env);
  out.canvas = composed(canvases);
  // The 20 s backstop still fires, and is swallowed.
  await clock.advanceAsync(20000);
  out.repliesAfterBackstop = results(env).length;
  out.otherInputs = env.inputs().filter((i) => i.name !== 'blockr_viz_capture_result');
  I.snap('export-capture-ok', out);
  env.close();
});

test('drilldown-capture: a chart that cannot draw answers after 20 s', async () => {
  const { env, clock } = captureEnv();
  env.handlers['drilldown-capture'](captureMsg(env, { req: 'tok-2', config: cfg({ group: 'NOPE' }) }));
  await clock.advanceAsync(19999);
  const before = results(env);
  await clock.advanceAsync(1);
  I.snap('export-capture-timeout', { before, after: results(env), hosts: hosts(env) });
  env.close();
});

test('drilldown-capture: a payload that throws answers at once', async () => {
  const { env, clock } = captureEnv();
  env.handlers['drilldown-capture'](captureMsg(env, { req: 'tok-3', data: '{not json' }));
  const now = results(env);
  await clock.advanceAsync(21000);
  I.snap('export-capture-throws', {
    now: now.map((r) => ({ ...r, value: { ...r.value, error: String(r.value.error).replace(/:.*$/, ':…') } })),
    later: results(env).length, hosts: hosts(env)
  });
  env.close();
});

test('drilldown-capture: two requests in flight', async () => {
  const { env, clock } = captureEnv();
  env.handlers['drilldown-capture'](captureMsg(env, { req: 'a', width: 300, height: 200 }));
  env.handlers['drilldown-capture'](captureMsg(env, { req: 'b', width: 500, height: 300,
                                                      config: cfg({ chart_type: 'pie', group: 'AESEV' }) }));
  const during = hosts(env);
  await clock.advanceAsync(1500);
  I.snap('export-capture-two', { during, replies: results(env), after: hosts(env) });
  env.close();
});
