/* Mount the chart block's JavaScript in a headless DOM and drive it the way
 * R does.
 *
 * The files run unmodified, in the order the page loads them: blockr.ui's
 * controls (controls_dep()), then drilldown-agg.js, drilldown-config.js and
 * capture-pages.js (drilldown_shared_dep()), then drilldown-theme-register.js
 * and chart.js (drilldown_chart_dep()). A chart is created through the input
 * binding's `initialize`, and its data arrives through the `drilldown-data`
 * message handler, exactly as from build_chart_msg().
 *
 * What is stubbed:
 *   - Shiny: the binding, the custom message handlers and setInputValue are
 *     captured. `$` is a do-nothing jQuery (the binding's `find` is the only
 *     user, and the tests call `initialize` themselves).
 *   - echarts: a fake that records every setOption / dispatchAction / resize
 *     per instance and keeps a merged option for getOption(). See fakeEcharts.
 *   - Layout: happy-dom has none, so clientWidth / clientHeight come from the
 *     `width` / `height` a test asks for (a facet panel gets its share of the
 *     width, as the stylesheet's auto-fit grid would give it).
 *   - Canvas text: measureText is 7 px per character, so label fitting is
 *     deterministic.
 *   - Timers, requestAnimationFrame, ResizeObserver, IntersectionObserver:
 *     queued and never run unless a test calls `flushTimers()`. A snapshot is
 *     the synchronous result of one setData.
 *   - Locale: toLocaleString and localeCompare are pinned to en-US.
 */
'use strict';

const fs = require('node:fs');
const path = require('node:path');
const { Window } = require('happy-dom');

const JS_DIR = path.join(__dirname, '..', '..', 'inst', 'js');

// blockr.ui's scripts in controls_dep() order. Read from the installed
// package, the copy R serves; BLOCKR_UI_JS points at a source tree instead
// (CI checks blockr.ui out next to this repo).
const UI_FILES = ['blockr-ui.js', 'blockr-select.js', 'blockr-input.js'];
const VIZ_FILES = ['drilldown-agg.js', 'drilldown-config.js', 'capture-pages.js',
                   'drilldown-theme-register.js', 'chart.js'];

let uiDir = process.env.BLOCKR_UI_JS;
const UI_DIR = () => uiDir || (uiDir = require('node:child_process').execFileSync(
  'Rscript', ['-e', 'cat(system.file("assets", "js", package = "blockr.ui", mustWork = TRUE))'],
  { encoding: 'utf8' }
).trim());

/** @type {Record<string, string>} */
const sourceCache = {};
const source = (file) => {
  if (!(file in sourceCache)) {
    const dir = UI_FILES.includes(file) ? UI_DIR() : JS_DIR;
    // CHART_JS points at another chart.js (a scratch copy for an
    // experiment); everything else always loads from inst/js.
    const f = file === 'chart.js' && process.env.CHART_JS
      ? process.env.CHART_JS : path.join(dir, file);
    sourceCache[file] = fs.readFileSync(f, 'utf8');
  }
  return sourceCache[file];
};

/** The head script controls_dep() writes: `Blockr.icons = {name: svg}`. */
let iconsCache = null;
const iconsScript = () => {
  if (iconsCache == null) {
    const dir = path.join(UI_DIR(), '..', 'icons');
    const icons = {};
    if (fs.existsSync(dir)) {
      for (const f of fs.readdirSync(dir).filter((x) => x.endsWith('.svg')).sort()) {
        icons[f.replace(/\.svg$/, '')] = fs.readFileSync(path.join(dir, f), 'utf8').trim();
      }
    }
    iconsCache = '(window.Blockr = window.Blockr || {}).icons = ' + JSON.stringify(icons) + ';';
  }
  return iconsCache;
};

const isPlain = (x) => Object.prototype.toString.call(x) === '[object Object]';

// ---------------------------------------------------------------------------
// Normalization for snapshots.

const FN = '[fn]';

/** Round to 6 significant digits; non-finite numbers become strings. */
const num = (v) => {
  if (Number.isNaN(v)) return 'NaN';
  if (v === Infinity) return 'Infinity';
  if (v === -Infinity) return '-Infinity';
  if (Object.is(v, -0)) return 0;
  if (Number.isInteger(v)) return v;
  return Number(v.toPrecision(6));
};

/**
 * Deep copy into plain JSON: functions -> "[fn]", numbers rounded, undefined
 * properties dropped (as JSON.stringify would), DOM nodes -> "[node]".
 * Works across the window realm boundary.
 */
function normalize(x, seen = new Set()) {
  if (x === null) return null;
  const t = typeof x;
  if (t === 'function') return FN;
  if (t === 'number') return num(x);
  if (t === 'string' || t === 'boolean') return x;
  if (t === 'undefined') return null;
  if (t === 'bigint' || t === 'symbol') return String(x);
  if (seen.has(x)) return '[circular]';
  if (x && typeof x.nodeType === 'number') return '[node]';
  seen.add(x);
  let out;
  if (Array.isArray(x)) {
    // Built here, not with x.map(): an array from the window realm would map
    // into another window-realm array.
    out = [];
    for (let i = 0; i < x.length; i++) out.push(normalize(x[i], seen));
  } else if (x instanceof Map || Object.prototype.toString.call(x) === '[object Map]') {
    out = { '[Map]': Array.from(x.entries()).map(([k, v]) => [normalize(k, seen), normalize(v, seen)]) };
  } else {
    out = {};
    for (const k of Object.keys(x)) {
      if (x[k] === undefined) continue;
      out[k] = normalize(x[k], seen);
    }
  }
  seen.delete(x);
  return out;
}

// ---------------------------------------------------------------------------
// Probes: call the formatters an option carries with plausible params, so a
// snapshot pins what they print and not just that they exist.

const PROBE_CATS = 12;
const PROBE_SERIES = 8;
const PROBE_DATA = 4;

const tryCall = (fn, ...args) => {
  try {
    const r = fn(...args);
    return typeof r === 'string' ? r : normalize(r);
  } catch (e) {
    return '[threw: ' + (e && e.message) + ']';
  }
};

const asArray = (v) => (v == null ? [] : Array.isArray(v) ? v : [v]);
const datumValue = (d) => (d != null && typeof d === 'object' && !Array.isArray(d) ? d.value : d);
const seriesColor = (s) => (s && s.itemStyle && typeof s.itemStyle.color === 'string'
  ? s.itemStyle.color : null);

function probeOption(option) {
  const out = {};
  const axes = { xAxis: asArray(option.xAxis), yAxis: asArray(option.yAxis) };
  const series = asArray(option.series);

  // Axis label formatters, over the categories (or a few ticks).
  const axisFmt = {};
  for (const key of ['xAxis', 'yAxis']) {
    axes[key].forEach((a, i) => {
      const f = a && a.axisLabel && a.axisLabel.formatter;
      if (typeof f !== 'function') return;
      const inputs = Array.isArray(a.data) ? a.data.slice(0, PROBE_CATS) : [0, 0.25, 0.5, 1, 12.5];
      axisFmt[key + i] = inputs.map((v) => tryCall(f, v));
    });
  }
  if (Object.keys(axisFmt).length) out.axisLabels = axisFmt;

  // Tooltip.
  const tip = asArray(option.tooltip)[0];
  if (tip && typeof tip.formatter === 'function') {
    const catAxis = [...axes.xAxis, ...axes.yAxis].find(
      (a) => a && a.type === 'category' && Array.isArray(a.data));
    const cats = catAxis ? catAxis.data : null;
    const calls = [];
    if (tip.trigger === 'axis') {
      const n = cats ? cats.length
        : Math.max(0, ...series.map((s) => (Array.isArray(s.data) ? s.data.length : 0)));
      for (let idx = 0; idx < Math.min(n, PROBE_CATS); idx++) {
        const ps = [];
        series.forEach((s, si) => {
          const d = Array.isArray(s.data) ? s.data[idx] : undefined;
          if (d === undefined) return;
          const label = cats ? cats[idx] : (Array.isArray(datumValue(d)) ? datumValue(d)[0] : idx);
          ps.push({
            componentType: 'series', seriesType: s.type, seriesIndex: si,
            seriesName: s.name != null ? s.name : 'series' + si,
            name: label, axisValueLabel: label, axisValue: label,
            value: datumValue(d), data: d, dataIndex: idx, color: seriesColor(s)
          });
        });
        calls.push(tryCall(tip.formatter, ps));
      }
    } else {
      series.slice(0, PROBE_SERIES).forEach((s, si) => {
        const data = Array.isArray(s.data) ? s.data.slice(0, PROBE_DATA) : [];
        data.forEach((d, di) => {
          const name = d != null && typeof d === 'object' && !Array.isArray(d) && d.name != null
            ? d.name : (cats ? cats[di] : '');
          calls.push(tryCall(tip.formatter, {
            componentType: 'series', seriesType: s.type, seriesIndex: si,
            seriesName: s.name != null ? s.name : 'series' + si,
            name, value: datumValue(d), data: d, dataIndex: di,
            color: seriesColor(s), percent: 25
          }));
        });
      });
    }
    out.tooltip = calls;
  }

  // Series label formatters (value labels).
  const labels = {};
  series.forEach((s, si) => {
    const f = s && s.label && s.label.formatter;
    if (typeof f !== 'function') return;
    const data = Array.isArray(s.data) ? s.data.slice(0, PROBE_CATS) : [];
    labels['series' + si] = data.map((d, di) => tryCall(f, {
      value: datumValue(d), data: d, dataIndex: di, seriesIndex: si,
      name: cats0(axes, di)
    }));
  });
  if (Object.keys(labels).length) out.seriesLabels = labels;
  return out;
}

const cats0 = (axes, i) => {
  const a = [...axes.xAxis, ...axes.yAxis].find((x) => x && Array.isArray(x.data));
  return a ? a.data[i] : undefined;
};

// ---------------------------------------------------------------------------
// Fake echarts.

const COMPONENTS = new Set(['series', 'xAxis', 'yAxis', 'grid', 'legend', 'tooltip',
  'toolbox', 'brush', 'radar', 'title', 'dataZoom', 'visualMap', 'graphic',
  'polar', 'angleAxis', 'radiusAxis', 'markLine']);

const mergeDeep = (a, b) => {
  if (b === undefined) return a;
  if (isPlain(a) && isPlain(b)) {
    const out = { ...a };
    for (const k of Object.keys(b)) out[k] = mergeDeep(a[k], b[k]);
    return out;
  }
  return b;
};

const mergeOption = (state, opt, notMerge) => {
  const out = notMerge || !state ? {} : { ...state };
  for (const k of Object.keys(opt || {})) {
    const v = opt[k];
    if (v === undefined) continue;
    if (!COMPONENTS.has(k)) { out[k] = notMerge ? v : mergeDeep(out[k], v); continue; }
    const arr = Array.isArray(v) ? v : [v];
    if (notMerge || !out[k]) { out[k] = arr.slice(); continue; }
    const cur = out[k].slice();
    arr.forEach((item, i) => { cur[i] = mergeDeep(cur[i], item); });
    out[k] = cur;
  }
  return out;
};

// Nice-rounded [lo, hi], the way an echarts value axis widens its extent.
const niceExtent = (lo, hi) => {
  if (!(hi > lo)) return [lo, hi];
  const span = hi - lo;
  const mag = Math.pow(10, Math.floor(Math.log10(span)));
  const rel = span / mag;
  const step = rel >= 5 ? mag : (rel >= 2 ? mag / 2 : mag / 5);
  return [Math.floor(lo / step) * step, Math.ceil(hi / step) * step];
};

// The data extent of value axis `key`[idx] in a merged option. An
// approximation of echarts' own (no stacking): enough for _harmoniseAxes to
// read a deterministic, data-dependent domain.
const axisExtent = (merged, key, idx) => {
  const axis = (merged[key] || [])[idx];
  if (!axis) return [NaN, NaN];
  if (typeof axis.min === 'number' && typeof axis.max === 'number') return [axis.min, axis.max];
  const dimKey = key === 'xAxis' ? 'x' : 'y';
  const idxKey = key === 'xAxis' ? 'xAxisIndex' : 'yAxisIndex';
  let lo = Infinity, hi = -Infinity;
  const take = (v) => {
    const n = typeof v === 'number' ? v : Number(v);
    if (typeof v === 'string' && v.trim() === '') return;
    if (!Number.isFinite(n)) return;
    if (n < lo) lo = n;
    if (n > hi) hi = n;
  };
  for (const s of merged.series || []) {
    if (!s || (s[idxKey] || 0) !== idx || !Array.isArray(s.data)) continue;
    for (const d of s.data) {
      const v = datumValue(d);
      if (v == null) continue;
      if (Array.isArray(v)) {
        let dims;
        if (s.encode && s.encode[dimKey] != null) dims = [].concat(s.encode[dimKey]);
        else if (s.type === 'boxplot') dims = v.map((_, i) => i);
        else dims = [dimKey === 'x' ? 0 : 1];
        for (const di of dims) take(v[di]);
      } else {
        take(v);
      }
    }
  }
  if (!Number.isFinite(lo)) return [NaN, NaN];
  if (axis.scale !== true) { lo = Math.min(0, lo); hi = Math.max(0, hi); }
  if (lo === hi) hi = lo + 1;
  return niceExtent(lo, hi);
};

/**
 * @param {{ record: 'full' | 'none' }} mode  'full' keeps a normalized copy of
 *   every call plus formatter probes (snapshots); 'none' keeps only the merged
 *   option (timing).
 */
function fakeEcharts(mode) {
  const instances = [];
  const themes = {};
  const byDom = new Map();

  const init = (dom, theme, opts) => {
    let merged = null;
    const handlers = [];
    const zrHandlers = [];
    const log = [];
    let disposed = false;
    const inst = {
      __fake: true,
      theme: theme == null ? null : theme,
      log,
      handlers,
      zrHandlers,
      getDom: () => dom,
      setOption(option, notMerge) {
        const nm = typeof notMerge === 'object' && notMerge !== null
          ? !!notMerge.notMerge : !!notMerge;
        merged = mergeOption(merged, option, nm);
        if (mode === 'full') {
          const entry = { op: 'setOption', notMerge: nm, option: normalize(option) };
          if (nm) entry.probes = probeOption(option);
          log.push(entry);
        }
      },
      getOption: () => merged,
      getModel: () => ({
        getComponent: (key, idx = 0) => {
          const opt = merged && merged[key] && merged[key][idx];
          if (!opt) return null;
          return {
            get: (p) => (p === 'type' && opt.type == null
              ? (key === 'xAxis' ? 'category' : 'value') : opt[p]),
            axis: { scale: { getExtent: () => axisExtent(merged, key, idx) } }
          };
        }
      }),
      getWidth: () => dom.clientWidth,
      getHeight: () => dom.clientHeight,
      resize(o) { if (mode === 'full') log.push({ op: 'resize', opts: normalize(o) }); },
      dispatchAction(a) { if (mode === 'full') log.push({ op: 'dispatchAction', action: normalize(a) }); },
      on(evt, query, fn) { handlers.push({ evt, fn: typeof query === 'function' ? query : fn }); },
      off(evt) { for (let i = handlers.length - 1; i >= 0; i--) if (handlers[i].evt === evt) handlers.splice(i, 1); },
      trigger(evt, params) { for (const h of handlers) if (h.evt === evt) h.fn(params); },
      getZr: () => ({
        on: (evt, fn) => zrHandlers.push({ evt, fn }),
        off: () => {},
        handler: { proxy: { setCursor: () => {} } },
        setCursorStyle: () => {}
      }),
      convertToPixel: (finder, v) => (Array.isArray(v) ? v.map(Number) : Number(v)),
      convertFromPixel: (finder, v) => (Array.isArray(v) ? v.map(Number) : Number(v)),
      containPixel: () => true,
      getDataURL: () => 'data:image/png;base64,',
      clear() { merged = null; if (mode === 'full') log.push({ op: 'clear' }); },
      dispose() { disposed = true; byDom.delete(dom); },
      isDisposed: () => disposed
    };
    instances.push(inst);
    byDom.set(dom, inst);
    return inst;
  };

  return {
    instances,
    themes,
    api: {
      version: 'fake',
      init,
      registerTheme: (name, t) => { themes[name] = t; },
      getInstanceByDom: (dom) => byDom.get(dom),
      dispose: (dom) => { const i = byDom.get(dom); if (i) i.dispose(); },
      graphic: {
        clipRectByRect(a, b) {
          const x = Math.max(a.x, b.x), y = Math.max(a.y, b.y);
          const x2 = Math.min(a.x + a.width, b.x + b.width);
          const y2 = Math.min(a.y + a.height, b.y + b.height);
          if (x2 <= x || y2 <= y) return undefined;
          return { x, y, width: x2 - x, height: y2 - y };
        }
      }
    }
  };
}

// ---------------------------------------------------------------------------
// The window.

const PRELUDE = `
  window.__handlers = {};
  window.__inputs = [];
  window.__binding = null;
  window.Shiny = {
    InputBinding: function () {},
    inputBindings: { register: function (b) { window.__binding = b; } },
    addCustomMessageHandler: function (n, f) { window.__handlers[n] = f; },
    setInputValue: function (n, v, o) { window.__inputs.push({ name: n, value: v, opts: o }); }
  };
  window.$ = window.jQuery = function () { return { find: function () { return []; } }; };

  // Timers queue and never fire on their own (see flushTimers()).
  window.__timers = [];
  window.__timerSeq = 0;
  window.setTimeout = function (fn, ms) {
    var id = ++window.__timerSeq;
    window.__timers.push({ id: id, fn: fn, ms: ms || 0 });
    return id;
  };
  window.clearTimeout = function (id) {
    for (var i = 0; i < window.__timers.length; i++) {
      if (window.__timers[i].id === id) { window.__timers.splice(i, 1); return; }
    }
  };
  window.setInterval = function () { return 0; };
  window.clearInterval = function () {};
  window.requestAnimationFrame = function (fn) { return window.setTimeout(fn, 16); };
  window.cancelAnimationFrame = window.clearTimeout;
  window.ResizeObserver = function (cb) { this.cb = cb; };
  window.ResizeObserver.prototype.observe = function () {};
  window.ResizeObserver.prototype.unobserve = function () {};
  window.ResizeObserver.prototype.disconnect = function () {};
  window.IntersectionObserver = window.ResizeObserver;

  // One locale whatever the machine's.
  (function () {
    var tls = Number.prototype.toLocaleString;
    Number.prototype.toLocaleString = function (loc, opts) { return tls.call(this, 'en-US', opts); };
    var lc = String.prototype.localeCompare;
    String.prototype.localeCompare = function (that, loc, opts) { return lc.call(this, that, 'en-US', opts); };
  })();
`;

const CHAR_W = 7;

/**
 * A window with the chart's scripts loaded.
 * @param {{ width?: number, height?: number, record?: 'full' | 'none' }} [opts]
 */
function createEnv(opts = {}) {
  const layout = { width: opts.width || 800, height: opts.height || 400 };
  const win = new Window({ url: 'http://localhost/' });
  const fake = fakeEcharts(opts.record || 'full');
  win.echarts = fake.api;

  win.eval(PRELUDE);
  // The page font (blockr.ui's tokens set it on body); the canvas reads it.
  win.document.body.style.fontFamily = "'Open Sans', system-ui, sans-serif";

  // Layout: a container is as wide as the test says; a facet panel gets its
  // track of the grid (facet_cols, else auto-fit at 300px per track).
  const proto = win.HTMLElement.prototype;
  Object.defineProperty(proto, 'clientWidth', {
    configurable: true,
    get() {
      if (!this.isConnected) return 0;
      if (this.classList && this.classList.contains('dd-chart')) {
        const facet = this.closest('.dd-facet');
        if (facet) {
          const grid = facet.parentElement;
          const n = grid ? grid.querySelectorAll(':scope > .dd-facet').length : 1;
          const m = /repeat\((\d+)/.exec((grid && grid.style.gridTemplateColumns) || '');
          const cols = m ? Number(m[1]) : Math.max(1, Math.min(n, Math.floor(layout.width / 300)));
          return Math.floor(layout.width / cols);
        }
      }
      return layout.width;
    }
  });
  Object.defineProperty(proto, 'clientHeight', {
    configurable: true,
    get() {
      if (!this.isConnected) return 0;
      const h = parseFloat(this.style && this.style.height);
      return Number.isFinite(h) ? h : layout.height;
    }
  });

  const ctx = {
    font: '10px sans-serif',
    measureText: (t) => ({ width: CHAR_W * String(t == null ? '' : t).length }),
    fillText() {}, fillRect() {}, drawImage() {}, scale() {}, save() {}, restore() {},
    textAlign: 'left', textBaseline: 'top', fillStyle: '#000', globalAlpha: 1
  };
  win.HTMLCanvasElement.prototype.getContext = function () { return ctx; };

  // controls_dep() writes Blockr.icons into the page head, one SVG per file
  // in blockr.ui's assets/icons, ahead of blockr-ui.js.
  win.eval(iconsScript());
  for (const f of UI_FILES) win.eval(source(f));
  for (const f of VIZ_FILES) win.eval(source(f));

  const binding = win.__binding;
  if (!binding) throw new Error('chart.js registered no input binding');

  return {
    win,
    layout,
    fake,
    binding,
    handlers: win.__handlers,
    inputs: () => normalize(Array.from(win.__inputs)),
    timers: () => Array.from(win.__timers),
    /** Run queued timers (ms order), up to `limit` rounds. */
    flushTimers(limit = 10) {
      for (let round = 0; round < limit && win.__timers.length; round++) {
        const due = win.__timers.splice(0).sort((a, b) => a.ms - b.ms);
        for (const t of due) t.fn();
      }
    },
    /** Close the window. The memory is only released once the returned
     *  promise settles; a test that opens many windows awaits it. */
    close() { win.happyDOM.abort(); return win.happyDOM.close(); }
  };
}

let mountSeq = 0;

/**
 * Mount a chart container the way the block's UI renders it (the mapping
 * band beside it, both inside one block root) and bind it.
 * @returns {{ el: any, block: any, id: string }}
 */
function mount(env, id) {
  const doc = env.win.document;
  const cid = id || `block_${++mountSeq}-drilldown_block`;
  const root = doc.createElement('div');
  root.className = 'block-root';
  root.innerHTML =
    `<div id="${cid.replace(/drilldown_block$/, 'mapping_band')}" class="dd-mapping-band" style="display:none"></div>` +
    `<div id="${cid}" class="drilldown-chart-container"></div>`;
  doc.body.appendChild(root);
  const el = doc.getElementById(cid);
  env.binding.initialize(el);
  return { el, block: el._block, id: cid };
}

/**
 * The `drilldown-data` message R's build_chart_msg() sends, parsed into the
 * page's realm the way Shiny delivers it (a data string stays a string).
 */
function message(env, id, { columns, data, config, dataRev }) {
  const msg = env.win.JSON.parse(JSON.stringify({
    id, columns, config, arguments: null, data_rev: dataRev == null ? null : dataRev,
    data: typeof data === 'string' ? null : data
  }));
  if (typeof data === 'string') msg.data = data;
  return msg;
}

/** Deliver a `drilldown-data` message to the registered handler. */
function send(env, id, payload) {
  const handler = env.handlers['drilldown-data'];
  if (!handler) throw new Error('no drilldown-data handler');
  handler(message(env, id, payload));
}

/**
 * Milliseconds one setData takes on a fresh chart: mount, build the message,
 * then time only the handler call (which is setData). The chart is disposed
 * and removed afterwards.
 */
function timeDraw(env, payload) {
  const { el, block, id } = mount(env);
  const msg = message(env, id, payload);
  const handler = env.handlers['drilldown-data'];
  const t0 = performance.now();
  handler(msg);
  const ms = performance.now() - t0;
  block.dispose();
  el.parentElement.remove();
  return ms;
}

const text = (el) => (el && el.style.display !== 'none' ? el.textContent : null);

/** What the reader sees outside the canvases. */
function domSummary(block) {
  const legend = block.legendEl && block.legendEl.style.display !== 'none'
    ? {
        title: (block.legendEl.querySelector('.dd-legend-title') || {}).textContent || null,
        chips: Array.from(block.legendEl.querySelectorAll('.dd-legend-chip')).map((c) => ({
          name: c.textContent,
          color: c.querySelector('.dd-legend-swatch').style.background,
          off: c.classList.contains('dd-legend-chip-off')
        }))
      }
    : null;
  const empty = Array.from(block.chartGrid.querySelectorAll('.vd-empty-text'))
    .map((e) => e.textContent);
  return {
    title: text(block.titleEl),
    subtitle: text(block.subtitleEl),
    caption: text(block.captionEl),
    legend,
    status: block.statusEl ? block.statusEl.textContent : null,
    empty,
    grid: {
      single: block.chartGrid.classList.contains('dd-chart-grid-single'),
      columns: block.chartGrid.style.gridTemplateColumns || null
    },
    slots: Array.from(block._slots || [], (s) => ({
      label: s.labelEl && s.labelEl.style.display !== 'none' ? s.labelEl.textContent : null,
      height: s.chartDiv.style.height || null,
      chart: !!s.chart
    }))
  };
}

/** Per slot, the recorded calls on its echarts instance. */
function slotLogs(block) {
  return Array.from(block._slots || [], (s) => (s.chart ? {
    theme: s.chart.theme,
    calls: s.chart.log
  } : null));
}

/**
 * Draw one chart in a fresh window and return what it did.
 * @param {object} config chart config (build_chart_msg()$config)
 * @param {object|string} data column object (or its JSON string)
 * @param {object[]} columns column metadata (dd_col_meta())
 * @param {{ width?: number, height?: number, keepOpen?: boolean,
 *           theme?: string, dataRev?: any }} [opts]
 */
function draw(config, data, columns, opts = {}) {
  const env = createEnv({ width: opts.width, height: opts.height, record: 'full' });
  try {
    const { el, block, id } = mount(env, opts.id || 'block-drilldown_block');
    if (opts.theme) env.handlers['drilldown-theme']({ id, theme: opts.theme });
    let error = null;
    try {
      send(env, id, { columns, data, config, dataRev: opts.dataRev });
    } catch (e) {
      error = String(e && e.stack ? e.stack.split('\n')[0] : e);
    }
    const result = {
      error,
      options: slotLogs(block),
      dom: domSummary(block),
      inputs: env.inputs().map((i) => (/_ready$/.test(i.name) ? { ...i, value: '[time]' } : i)),
      disposed: env.fake.instances.filter((i) => i.isDisposed()).length,
      env, el, block, id
    };
    if (!opts.keepOpen) env.close();
    return result;
  } catch (e) {
    env.close();
    throw e;
  }
}

module.exports = {
  createEnv, mount, message, send, timeDraw, draw, normalize, domSummary, slotLogs,
  source, JS_DIR
};
