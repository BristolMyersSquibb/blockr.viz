/* Drive a drawn chart the way a reader or R does, on the harness
 * (../harness.js) and a virtual clock: open a chart from a config under the
 * spec's names, click and brush its marks through the fake ECharts, and read
 * what went to R (`<id>_select`, `<id>_set`) and what the footer says. */
'use strict';

const h = require('../harness');
const { F, BASE } = require('../cases');

const ID = 'blk-chart';

/** Replace the window's timers with a clock that moves only on advance(). */
function useClock(win) {
  const clock = { now: 0, seq: 0, timers: [] };
  win.setTimeout = (fn, ms) => {
    const id = ++clock.seq;
    clock.timers.push({ id, fn, due: clock.now + (Number(ms) || 0) });
    return id;
  };
  win.clearTimeout = (id) => {
    const i = clock.timers.findIndex((t) => t.id === id);
    if (i >= 0) clock.timers.splice(i, 1);
  };
  win.requestAnimationFrame = (fn) => win.setTimeout(() => fn(clock.now), 16);
  win.cancelAnimationFrame = win.clearTimeout;
  Object.defineProperty(win, 'performance', { configurable: true, value: { now: () => clock.now } });
  clock.advance = (ms) => {
    const until = clock.now + ms;
    for (;;) {
      const due = clock.timers.filter((t) => t.due <= until).sort((a, b) => a.due - b.due || a.id - b.id);
      if (!due.length) break;
      const t = due[0];
      clock.timers.splice(clock.timers.indexOf(t), 1);
      clock.now = t.due;
      t.fn();
    }
    clock.now = until;
  };
  /** As advance(), letting promises settle between timers. */
  clock.advanceAsync = async (ms) => {
    const settle = () => new Promise((r) => setImmediate(r));
    const until = clock.now + ms;
    await settle();
    for (;;) {
      const due = clock.timers.filter((t) => t.due <= until).sort((a, b) => a.due - b.due || a.id - b.id);
      if (!due.length) break;
      const t = due[0];
      clock.timers.splice(clock.timers.indexOf(t), 1);
      clock.now = t.due;
      t.fn();
      await settle();
    }
    clock.now = until;
    await settle();
  };
  return clock;
}

const datumValue = (d) => (d != null && typeof d === 'object' && !Array.isArray(d) ? d.value : d);

/** The click params ECharts builds for datum `di` of series `si`. */
function paramsAt(chart, si, di, extra = {}) {
  const o = chart.getOption();
  const s = o.series[si];
  const d = s.data[di];
  const axes = [...(o.xAxis || []), ...(o.yAxis || [])];
  const cat = axes.find((a) => a && a.type === 'category' && Array.isArray(a.data));
  const named = d != null && typeof d === 'object' && !Array.isArray(d) && d.name != null;
  const name = named ? d.name : (cat ? cat.data[di] : '');
  return { componentType: 'series', componentSubType: s.type, seriesType: s.type,
           seriesIndex: si, seriesName: s.name, dataIndex: di,
           name: name == null ? '' : name, data: d, value: datumValue(d), ...extra };
}

/**
 * Draw `config` (over BASE) and return a context to drive it.
 * @param {object} config
 * @param {{ width?: number, derived?: object, data?: any, columns?: any[] }} [opts]
 */
function open(config, opts = {}) {
  const env = h.createEnv({ width: opts.width, record: 'full' });
  if (opts.before) opts.before(env);
  const clock = useClock(env.win);
  // No tray animation: happy-dom rejects a cancelled one.
  env.win.matchMedia = () => ({ matches: true });
  const m = h.mount(env, ID);
  const ctx = {
    env, clock, id: ID, el: m.el,
    get block() { return m.block; },
    get doc() { return env.win.document; },
    config: { ...BASE, ...config },
    /** Send a payload; rows only when `data` is given or on the first. */
    send(cfg, o = {}) {
      ctx.config = cfg;
      h.send(env, ID, { columns: opts.columns || F.columns, data: o.data === undefined ? (opts.data || F.data) : o.data,
                        config: cfg, derived: o.derived || opts.derived, dataRev: o.dataRev || 1,
                        stateRev: o.stateRev });
    },
    chart(i = 0) { return m.block._slots[i].chart; },
    inputs() { return env.inputs(); },
    /** The `_select` messages sent so far: their selections. */
    selections() {
      return ctx.inputs().filter((x) => x.name === ID + '_select:blockr.viz.selection')
        .map((x) => x.value.selection);
    },
    /** The `_set` messages sent so far: their values. */
    sets() {
      return ctx.inputs().filter((x) => x.name === ID + '_set:blockr.viz.chart')
        .map((x) => x.value.values);
    },
    footer() {
      const t = m.block.frame.statusEl.querySelector('.vz-status-text');
      return t ? t.textContent : null;
    },
    trigger(evt, params, i = 0) { ctx.chart(i).trigger(evt, params); },
    zr(evt, e = {}, i = 0) { for (const z of ctx.chart(i).zrHandlers) if (z.evt === evt) z.fn(e); },
    click(i, si, di, extra) { ctx.trigger('click', paramsAt(ctx.chart(i), si, di, extra), i); },
    /** Click the first datum on slot `i` whose params pass `pred`. */
    clickWhere(i, pred, extra) {
      const o = ctx.chart(i).getOption();
      for (let si = 0; si < o.series.length; si++) {
        const data = o.series[si].data || [];
        for (let di = 0; di < data.length; di++) {
          const v = datumValue(data[di]);
          if (v == null || (Array.isArray(v) && v.every((x) => x == null))) continue;
          if (pred(paramsAt(ctx.chart(i), si, di))) { ctx.click(i, si, di, extra); return; }
        }
      }
      throw new Error('no datum matches');
    },
    advance(ms) { clock.advance(ms); },
    close() { return env.close(); }
  };
  ctx.send(ctx.config);
  return ctx;
}

/** A datum by its category name and, optionally, series. */
const at = (name, seriesName) => (p) => String(p.name) === name &&
  (seriesName === undefined || p.seriesName === seriesName);

const plain = (x) => JSON.parse(JSON.stringify(x));

module.exports = { open, at, plain, paramsAt, useClock, ID };
