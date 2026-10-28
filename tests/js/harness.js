/* Mount the chart block's JavaScript in a headless DOM and drive it the way
 * R does.
 *
 * The files run unmodified, in the order the page loads them: blockr.ui's
 * controls (controls_dep()), the core (inst/js/core/scripts.txt), the
 * generated chart spec, then the chart scripts in inst/js/chart/, in the
 * order scripts.txt lists them. A chart is built by the shared output
 * binding on its element's first payload, exactly as Shiny hands R's
 * viz_payload() to `renderValue`.
 *
 * What is stubbed:
 *   - Shiny: the output binding, the custom message handlers and
 *     setInputValue are captured. `$` is a small jQuery stand-in (the
 *     binding's `find`, and `on` / `off` for "shiny:unbound").
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
const vm = require('node:vm');
const path = require('node:path');
const { Window } = require('happy-dom');

const JS_DIR = path.join(__dirname, '..', '..', 'inst', 'js');

// blockr.ui's scripts in controls_dep() order. Read from the installed
// package, the copy R serves; BLOCKR_UI_JS points at a source tree instead
// (CI checks blockr.ui out next to this repo).
const UI_FILES = ['blockr-ui.js', 'blockr-select.js', 'blockr-input.js'];
/** The core scripts, then the generated spec. */
const CORE_FILES = fs.readFileSync(path.join(JS_DIR, 'core', 'scripts.txt'), 'utf8')
  .split('\n').map((l) => l.trim()).filter((l) => l && !l.startsWith('#'))
  .map((f) => 'core/' + f);
// busy-cue.js is blockr.viz's busy cue until the core has one.
const SHARED_FILES = ['busy-cue.js', ...CORE_FILES, 'spec/chart.js'];

/** The chart scripts in load order, as the html dependency lists them. */
const CHART_FILES = fs.readFileSync(path.join(JS_DIR, 'chart', 'scripts.txt'), 'utf8')
  .split('\n').map((l) => l.trim()).filter((l) => l && !l.startsWith('#'))
  .map((f) => 'chart/' + f);

const VIZ_FILES = [...SHARED_FILES, ...CHART_FILES];

let uiDir = process.env.BLOCKR_UI_JS;
const UI_DIR = () => uiDir || (uiDir = require('node:child_process').execFileSync(
  'Rscript', ['-e', 'cat(system.file("assets", "js", package = "blockr.ui", mustWork = TRUE))'],
  { encoding: 'utf8' }
).trim());

const fileOf = (file) => path.join(UI_FILES.includes(file) ? UI_DIR() : JS_DIR, file);

/** @type {Record<string, string>} */
const sourceCache = {};
const source = (file) => {
  if (!(file in sourceCache)) sourceCache[file] = fs.readFileSync(fileOf(file), 'utf8');
  return sourceCache[file];
};

/** @type {Record<string, vm.Script>} */
const scriptCache = {};
/**
 * Run one of the page's scripts in a window. Compiled with its file name, so
 * V8 (and with it c8) attributes the coverage to the file.
 */
const run = (win, file) => {
  if (!(file in scriptCache)) {
    scriptCache[file] = new vm.Script(source(file), { filename: fileOf(file) });
  }
  scriptCache[file].runInContext(win);
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
        // Real echarts paints into a <canvas> under its element; the chart's
        // busy cue waits for one before it ends.
        if (dom && !dom.querySelector('canvas')) {
          dom.appendChild(dom.ownerDocument.createElement('canvas'));
        }
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
      dispose() {
        disposed = true; byDom.delete(dom);
        const c = dom && dom.querySelector('canvas');
        if (c) c.remove();
      },
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
    OutputBinding: function () {},
    outputBindings: { register: function (b) { window.__binding = b; } },
    // The old table scripts still register input bindings.
    InputBinding: function () {},
    inputBindings: { register: function (b) { window.__binding = b; } },
    addCustomMessageHandler: function (n, f) { window.__handlers[n] = f; },
    setInputValue: function (n, v, o) { window.__inputs.push({ name: n, value: v, opts: o }); }
  };
  window.$ = window.jQuery = function (x) {
    var q = { find: function (sel) { return x && x.querySelectorAll ? x.querySelectorAll(sel) : []; },
              on: function (t, f) { (x.__jq = x.__jq || {})[t] = f; return q; },
              off: function (t) { if (x.__jq) delete x.__jq[t]; return q; } };
    return q;
  };

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
  for (const f of UI_FILES) run(win, f);
  for (const f of VIZ_FILES) run(win, f);

  const binding = win.__binding;
  if (!binding) throw new Error('the chart scripts registered no output binding');

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
 * Mount a chart output the way viz_output() renders it. The view is built
 * on the first payload; `block` reads it.
 * @returns {{ el: any, id: string, readonly block: any }}
 */
function mount(env, id) {
  const doc = env.win.document;
  const cid = id || `block_${++mountSeq}-chart`;
  const root = doc.createElement('div');
  root.className = 'block-root';
  root.innerHTML = `<div id="${cid}" class="blockr-viz-output" data-viz-view="chart"></div>`;
  doc.body.appendChild(root);
  const el = doc.getElementById(cid);
  return { el, id: cid, get block() { return el._viz ? el._viz.view : null; } };
}

let revSeq = 0;

/**
 * The payload R's viz_payload() sends, parsed into the page's realm the way
 * Shiny delivers it.
 * @param {{ columns?: any[], data?: any, config: object, derived?: object,
 *           dataRev?: any, stateRev?: number }} p
 */
function message(env, id, { columns, data, config, derived, dataRev, stateRev }) {
  const msg = env.win.JSON.parse(JSON.stringify({
    state_rev: stateRev == null ? ++revSeq : stateRev,
    data_rev: dataRev == null ? 1 : dataRev,
    columns: columns || [], config,
    derived: Object.assign({ column_meta: columns || [] }, derived || {}),
    data: typeof data === 'string' ? null : (data == null ? null : data)
  }));
  if (typeof data === 'string') msg.data = data;
  return msg;
}

/** Hand a payload to the output binding, as Shiny does. */
function send(env, id, payload) {
  const el = env.win.document.getElementById(id);
  env.binding.renderValue(el, message(env, id, payload));
}

/**
 * Milliseconds one render takes on a fresh chart: mount, build the payload,
 * then time only the binding call. The chart is disposed and removed
 * afterwards.
 */
function timeDraw(env, payload) {
  const { el, id } = mount(env);
  const msg = message(env, id, payload);
  const t0 = performance.now();
  env.binding.renderValue(el, msg);
  const ms = performance.now() - t0;
  el._viz.scope.dispose();
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
    // The footer's words; its clear button is not text.
    status: block.statusEl ? Array.from(block.statusEl.childNodes)
      .filter((n) => !(n.classList && n.classList.contains('dd-status-clear')))
      .map((n) => n.textContent).join('') : null,
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
    const m = mount(env, opts.id || 'block-chart');
    let error = null;
    try {
      send(env, m.id, { columns, data, config, derived: opts.derived, dataRev: opts.dataRev });
    } catch (e) {
      error = String(e && e.stack ? e.stack.split('\n')[0] : e);
    }
    const block = m.block;
    const result = {
      error,
      options: block ? slotLogs(block) : [],
      dom: block ? domSummary(block) : null,
      inputs: env.inputs(),
      disposed: env.fake.instances.filter((i) => i.isDisposed()).length,
      env, el: m.el, block, id: m.id
    };
    if (!opts.keepOpen) env.close();
    return result;
  } catch (e) {
    env.close();
    throw e;
  }
}

/** draw() for settings in the builders' words (chart/internal-config.js). */
function drawInternal(settings, data, columns, opts = {}) {
  const { toPayload } = require('./chart/internal-config');
  const p = toPayload(settings);
  return draw(p.config, data, columns,
              { ...opts, derived: { ...p.derived, ...(opts.derived || {}) } });
}

/** send() for settings in the builders' words. */
function sendInternal(env, id, { settings, ...rest }) {
  const { toPayload } = require('./chart/internal-config');
  const p = toPayload(settings);
  send(env, id, { ...rest, config: p.config, derived: { ...p.derived, ...(rest.derived || {}) } });
}

module.exports = {
  createEnv, mount, message, send, timeDraw, draw, drawInternal, sendInternal, normalize, domSummary, slotLogs,
  source, run, JS_DIR, CHART_FILES, PRELUDE, UI_FILES, SHARED_FILES, iconsScript
};
