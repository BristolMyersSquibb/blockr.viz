/* Helpers for the interaction tests (interactions-*.test.js): open a drawn
 * chart, drive it the way a reader or R would, and reduce what it did to
 * plain JSON for a snapshot in __snapshots__/interactions/.
 *
 * Every chart opened here runs on a virtual clock: setTimeout,
 * requestAnimationFrame and performance.now are replaced in the window, so
 * deferred clicks, debounces and the footer receipt are driven by
 * `advance(ms)` and their output does not depend on the machine.
 */
'use strict';

const h = require('./harness');
const { F, BASE } = require('./cases');
const { matchSnapshot } = require('./snapshot');

const ID = 'blk-drilldown_block';

// ---------------------------------------------------------------------------
// Virtual clock.

/** Replace the window's timers with a clock that only moves on advance(). */
function useClock(env) {
  const win = env.win;
  const clock = { now: 0, seq: 0, timers: [] };
  win.setTimeout = (fn, ms) => {
    const id = ++clock.seq;
    clock.timers.push({ id, fn, due: clock.now + (Number(ms) || 0), ms: Number(ms) || 0 });
    return id;
  };
  win.clearTimeout = (id) => {
    const i = clock.timers.findIndex((t) => t.id === id);
    if (i >= 0) clock.timers.splice(i, 1);
  };
  win.requestAnimationFrame = (fn) => win.setTimeout(() => fn(clock.now), 16);
  win.cancelAnimationFrame = win.clearTimeout;
  // The window shares node's `performance` object, so the clock gets a
  // window-only one rather than patching the shared one.
  Object.defineProperty(win, 'performance', { configurable: true,
    value: { now: () => clock.now } });

  /** The next due timer, earliest first, ties in scheduling order. */
  const next = (until) => {
    let best = null;
    for (const t of clock.timers) {
      if (t.due > until) continue;
      if (!best || t.due < best.due || (t.due === best.due && t.id < best.id)) best = t;
    }
    return best;
  };
  /** Run every timer due within `ms`, in due order, then set the clock. */
  clock.advance = (ms) => {
    const until = clock.now + ms;
    for (let t = next(until); t; t = next(until)) {
      clock.timers.splice(clock.timers.indexOf(t), 1);
      clock.now = t.due;
      t.fn();
    }
    clock.now = until;
  };
  /** As advance(), letting promises settle between timers. */
  clock.advanceAsync = async (ms) => {
    const until = clock.now + ms;
    const settle = () => new Promise((r) => setImmediate(r));
    await settle();
    for (let t = next(until); t; t = next(until)) {
      clock.timers.splice(clock.timers.indexOf(t), 1);
      clock.now = t.due;
      t.fn();
      await settle();
    }
    clock.now = until;
    await settle();
  };
  /** What is queued: delay as scheduled, and time left. */
  clock.pending = () => clock.timers.map((t) => ({ ms: t.ms, left: t.due - clock.now }));
  return clock;
}

// ---------------------------------------------------------------------------
// Opening a chart.

/**
 * A block root as the chart block's UI renders it, with the optional
 * download host (dl_control_ui()) beside the container.
 */
function mountRoot(env, id, { download = false } = {}) {
  const doc = env.win.document;
  const root = doc.createElement('div');
  root.className = 'block-root';
  root.innerHTML =
    `<div id="${id.replace(/drilldown_block$/, 'mapping_band')}" class="dd-mapping-band" style="display:none"></div>` +
    (download ? '<div class="dd-chart-dl-host" style="display:none"><button class="blockr-tool">dl</button></div>' : '') +
    `<div id="${id}" class="drilldown-chart-container"></div>`;
  doc.body.appendChild(root);
  const el = doc.getElementById(id);
  return { root, el };
}

/**
 * Draw `config` (merged over the defaults in cases.js) and return a context
 * for driving it.
 * @param {object} config
 * @param {{ width?: number, height?: number, theme?: string, data?: any,
 *           columns?: any[], dataRev?: any, download?: boolean,
 *           noSend?: boolean }} [opts]
 */
function open(config, opts = {}) {
  const env = h.createEnv({ width: opts.width, height: opts.height, record: 'full' });
  const clock = useClock(env);
  const { el } = mountRoot(env, ID, opts);
  env.binding.initialize(el);
  const ctx = makeCtx(env, clock, el);
  if (opts.theme) env.handlers['drilldown-theme']({ id: ID, theme: opts.theme });
  ctx.cfg = { ...BASE, ...config };
  if (!opts.noSend) {
    ctx.send(ctx.cfg, { data: opts.data, columns: opts.columns, dataRev: opts.dataRev ?? 1 });
  }
  return ctx;
}

function makeCtx(env, clock, el) {
  const ctx = {
    env, clock, el, id: el.id,
    get block() { return ctx.el._block; },
    get doc() { return env.win.document; },
    /** Deliver a drilldown-data message (rows included unless data: null). */
    send(config, { data, columns, dataRev = 1 } = {}) {
      const msg = env.win.JSON.parse(JSON.stringify({
        id: ctx.id, columns: columns || F.columns, config, arguments: null,
        data_rev: dataRev, data: data === null ? null : (data || F.data)
      }));
      if (data === null) delete msg.data;
      env.handlers['drilldown-data'](msg);
    },
    /** Re-send the current config (R's echo after a gear edit). */
    echo(extra = {}) {
      ctx.send({ ...ctx.block.config, ...extra }, { data: null, dataRev: ctx.block._lastDataRev });
    },
    slot(i = 0) { return ctx.block._slots[i]; },
    chart(i = 0) { return ctx.block._slots[i].chart; },
    /** A position in the record, for since(). */
    mark() {
      return {
        inputs: env.win.__inputs.length,
        logs: env.fake.instances.map((i) => i.log.length),
        instances: env.fake.instances.length
      };
    },
    /** What happened after `m`: new inputs and echarts calls. */
    since(m) {
      const inputs = env.inputs().slice(m.inputs).map(cleanInput);
      const calls = [];
      env.fake.instances.forEach((inst, i) => {
        const from = i < m.logs.length ? m.logs[i] : 0;
        const log = inst.log.slice(from);
        if (log.length) calls.push({ instance: i, calls: log.map(summarizeCall) });
      });
      return {
        inputs, calls,
        newInstances: env.fake.instances.length - m.instances,
        disposed: env.fake.instances.filter((i) => i.isDisposed()).length
      };
    },
    footer() { return footer(ctx.block); },
    /** Fire an echarts event on slot `i`'s instance. */
    trigger(evt, params, i = 0) { ctx.chart(i).trigger(evt, params); },
    /** Fire a zrender event on slot `i`'s instance. */
    zr(evt, e = {}, i = 0) {
      for (const hd of ctx.chart(i).zrHandlers) if (hd.evt === evt) hd.fn(e);
    },
    /** Click params the way echarts builds them, for slot/series/datum. */
    params(i, si, di, extra = {}) { return paramsAt(ctx.chart(i), si, di, extra); },
    click(i, si, di, extra = {}) {
      ctx.trigger('click', ctx.params(i, si, di, extra), i);
    },
    /**
     * [seriesIndex, dataIndex] of the first datum on slot `i` whose click
     * params pass `pred` (a datum with no value is never a click target).
     */
    locate(i, pred) {
      const o = ctx.chart(i).getOption();
      for (let si = 0; si < o.series.length; si++) {
        const data = o.series[si].data || [];
        for (let di = 0; di < data.length; di++) {
          const v = datumValue(data[di]);
          if (v == null || (Array.isArray(v) && v.every((x) => x == null))) continue;
          if (pred(paramsAt(ctx.chart(i), si, di, {}))) return [si, di];
        }
      }
      throw new Error('no datum matches');
    },
    /** Click the first datum on slot `i` matching `pred`. */
    clickWhere(i, pred, extra = {}) {
      const [si, di] = ctx.locate(i, pred);
      ctx.click(i, si, di, extra);
    },
    /** Click the footer's Reset button. */
    reset() {
      const b = ctx.block.statusEl.querySelector('.dd-status-reset');
      if (!b) throw new Error('no Reset button');
      b.click();
    },
    advance(ms) { clock.advance(ms); },
    close() { return env.close(); }
  };
  return ctx;
}

/** `_ready` carries Date.now(); everything else as sent. */
function cleanInput(inp) {
  return /_ready$/.test(inp.name) ? { ...inp, value: '[time]' } : inp;
}

/**
 * One fake echarts call for a snapshot. A full redraw (notMerge) is drawing,
 * which the option snapshots cover, so it is reduced to the series types it
 * draws; patches and actions are kept whole.
 */
function summarizeCall(c) {
  if (c.op === 'setOption' && c.notMerge) {
    const series = Array.isArray(c.option.series) ? c.option.series : [];
    return { op: 'redraw', series: series.map((s) => (s && s.type) || null) };
  }
  if (c.op === 'setOption') return { op: 'setOption', option: c.option };
  return c;
}

// ---------------------------------------------------------------------------
// echarts click params.

const datumValue = (d) => (d != null && typeof d === 'object' && !Array.isArray(d) ? d.value : d);

/**
 * What echarts passes a click handler for datum `di` of series `si`: the
 * datum's own name, else the category at that index on a category axis
 * (echarts names a datum after its ordinal dimension).
 */
function paramsAt(chart, si, di, extra) {
  const o = chart.getOption();
  const s = o.series[si];
  if (!s) throw new Error(`no series ${si}`);
  const d = s.data[di];
  if (d === undefined) throw new Error(`no datum ${di} in series ${si}`);
  const axes = [...(o.xAxis || []), ...(o.yAxis || [])];
  const cat = axes.find((a) => a && a.type === 'category' && Array.isArray(a.data));
  const named = d != null && typeof d === 'object' && !Array.isArray(d) && d.name != null;
  const name = named ? d.name : (cat ? cat.data[di] : '');
  return {
    componentType: 'series', componentSubType: s.type, seriesType: s.type,
    seriesIndex: si, seriesName: s.name, dataIndex: di,
    name: name == null ? '' : name, data: d, value: datumValue(d),
    ...extra
  };
}

// ---------------------------------------------------------------------------
// DOM summaries.

const shown = (el) => !!el && el.style.display !== 'none';
const txt = (el) => (el ? el.textContent.replace(/\s+/g, ' ').trim() : null);

/** The status footer: each part with its kind, and the Reset button. */
function footer(block) {
  const el = block.statusEl;
  return Array.from(el.children, (c) => {
    if (c.classList.contains('dd-status-reset')) return { reset: c.textContent };
    const o = { cls: c.className, text: c.textContent };
    if (c.style.animation) o.animation = c.style.animation;
    return o;
  });
}

/** The drill signal (flash box or pulse ring) on a slot, if any. */
function signal(slot) {
  const s = slot.chartDiv.querySelector('.dd-drill-flash, .dd-drill-pulse');
  if (!s) return null;
  const st = s.style;
  return { cls: s.className, left: st.left, top: st.top, width: st.width || null,
           height: st.height || null, borderRadius: st.borderRadius || null };
}

/** The legend band's chips that are toggled off, or null with no band. */
function legendOff(block) {
  const el = block.legendEl;
  if (!el || el.style.display === 'none') return null;
  return Array.from(el.querySelectorAll('.dd-legend-chip-off'), (c) => c.textContent);
}

/** One control in a gear row. */
function control(c) {
  if (c.classList.contains('blockr-select')) {
    const multi = c.classList.contains('blockr-select--multi');
    const value = c.querySelector('.blockr-select__value');
    const tags = Array.from(c.querySelectorAll('.blockr-select__tag'),
      (t) => t.getAttribute('data-value') || txt(t));
    const search = c.querySelector('.blockr-select__search');
    return {
      kind: multi ? 'multi' : 'select',
      value: multi ? tags : (value ? txt(value) : null),
      placeholder: (search && search.getAttribute('placeholder')) || null
    };
  }
  if (c.classList.contains('blockr-segmented')) {
    const segs = Array.from(c.querySelectorAll('.blockr-segmented__seg'));
    return { kind: 'segmented', options: segs.map(txt),
             selected: txt(segs.find((s) => s.classList.contains('is-selected'))) };
  }
  if (c.classList.contains('blockr-checkbox')) {
    return { kind: 'checkbox', label: txt(c.querySelector('.blockr-checkbox__label')),
             checked: !!c.querySelector('input').checked };
  }
  if (c.tagName === 'INPUT' && c.type === 'range') {
    const v = c.parentElement.querySelector('.dd-slider-value');
    return { kind: 'slider', value: c.value, min: c.min, max: c.max, step: c.step,
             shown: txt(v) };
  }
  if (c.tagName === 'INPUT') {
    return { kind: c.type === 'text' ? 'text' : c.type, value: c.value,
             placeholder: c.getAttribute('placeholder') || null };
  }
  return { kind: c.tagName.toLowerCase(), text: txt(c) };
}

const CONTROLS = '.blockr-select, .blockr-segmented, .blockr-checkbox, ' +
  'input.blockr-text-input, input.dd-slider, input[type=color]';

/** A row: its role, label, controls, and what it says. */
function row(r) {
  if (r.classList.contains('dd-add-wrap')) {
    return { add: Array.from(r.querySelectorAll('.dd-add-item'), txt) };
  }
  if (!r.classList.contains('dd-form-row')) {
    return { other: r.className, text: txt(r) };
  }
  const role = Array.from(r.classList).find((c) => /^dd-(role|title)-/.test(c) &&
    c !== 'dd-title-row');
  const head = r.querySelector(':scope > .dd-row-head .blockr-label');
  const out = {};
  if (role) out.role = role.replace(/^dd-(role|title)-/, '');
  if (head) out.label = txt(head);
  if (!shown(r)) out.hidden = true;
  if (r.querySelector('.dd-role-remove')) out.removable = true;
  // Controls nested inside another control (a select's search box) are
  // part of it, not separate controls.
  out.controls = Array.from(r.querySelectorAll(CONTROLS))
    .filter((c) => !c.parentElement.closest('.blockr-select, .blockr-checkbox'))
    .map(control);
  const help = Array.from(r.querySelectorAll('.dd-form-help')).filter(shown).map(txt)
    .filter(Boolean);
  if (help.length) out.help = help;
  return out;
}

/**
 * The gear as a reader sees it: the type tiles, then each section with its
 * title, fold state and rows.
 */
function gearOutline(block) {
  const pop = block.popoverEl;
  const tiles = Array.from(pop.querySelectorAll('.dd-type-tile'), (b) =>
    (b.classList.contains('dd-type-active') ? '*' : '') + txt(b));
  const sections = Array.from(pop.querySelectorAll(':scope > .dd-section'), (sec) => {
    const t = sec.querySelector(':scope > .dd-section-title');
    const s = {};
    if (t) {
      const sum = t.querySelector('.dd-fold-sum');
      const label = Array.from(t.querySelectorAll(':scope > span'))
        .filter((x) => !x.classList.contains('dd-fold-sum') &&
                       !x.classList.contains('dd-fold-chev'))
        .map(txt).join(' ') || txt(t);
      s.title = label;
      if (t.classList.contains('dd-section-title--fold')) {
        s.fold = t.getAttribute('aria-expanded') === 'true' ? 'open' : 'closed';
        if (sum) s.summary = txt(sum);
      }
    }
    s.rows = Array.from(sec.children).filter((c) => c !== t).map(row);
    return s;
  });
  return { open: pop.classList.contains('blockr-settings--open'), tiles, sections };
}

/** The script's control strip (the mapping band moved into the card). */
function bandOutline(block) {
  const band = block.el.querySelector('.dd-mapping-band');
  if (!band) return null;
  return { shown: shown(band), rows: Array.from(band.children).map(row) };
}

// ---------------------------------------------------------------------------
// Driving the gear.

/** The gear row for a role (`dd-role-<key>`). */
function roleRow(block, key) {
  const r = block.popoverEl.querySelector('.dd-role-' + key);
  if (!r) throw new Error(`no gear row for ${key}`);
  return r;
}

/** Pick `value` in the select inside `scope` (opens it, clicks the option). */
function pickSelect(env, scope, value) {
  const sel = scope.classList.contains('blockr-select') ? scope : scope.querySelector('.blockr-select');
  if (!sel) throw new Error('no select');
  sel.querySelector('.blockr-select__control').click();
  const opts = Array.from(env.win.document.querySelectorAll('.blockr-select__option'));
  const o = opts.find((x) => x.getAttribute('data-value') === value);
  if (!o) {
    throw new Error(`no option ${value} in [${opts.map((x) => x.getAttribute('data-value'))}]`);
  }
  o.click();
}

/** The options a select offers (opens it, reads, closes it again). */
function selectOptions(env, scope) {
  const sel = scope.classList.contains('blockr-select') ? scope : scope.querySelector('.blockr-select');
  const ctl = sel.querySelector('.blockr-select__control');
  ctl.click();
  const out = Array.from(env.win.document.querySelectorAll('.blockr-select__option'),
    (o) => o.getAttribute('data-value'));
  ctl.click();
  return out;
}

/** Click the segment labelled `label` in `scope`. */
function pickSegment(scope, label) {
  const seg = Array.from(scope.querySelectorAll('.blockr-segmented__seg'))
    .find((s) => txt(s) === label);
  if (!seg) throw new Error(`no segment ${label}`);
  seg.click();
}

/** Toggle the checkbox in `scope` the way a click on it does. */
function toggleCheckbox(env, scope) {
  const inp = scope.querySelector('.blockr-checkbox input');
  if (!inp) throw new Error('no checkbox');
  inp.checked = !inp.checked;
  inp.dispatchEvent(new env.win.Event('change', { bubbles: true }));
}

/** Type into a text field and press Enter. */
function typeText(env, input, value) {
  input.value = value;
  input.dispatchEvent(new env.win.Event('input', { bubbles: true }));
  input.dispatchEvent(new env.win.KeyboardEvent('keydown', { key: 'Enter', bubbles: true }));
}

/** The section whose title reads `title`. */
function section(block, title) {
  const secs = Array.from(block.popoverEl.querySelectorAll(':scope > .dd-section'));
  const s = secs.find((x) => {
    const t = x.querySelector(':scope > .dd-section-title');
    return t && t.textContent.includes(title);
  });
  if (!s) throw new Error(`no section ${title}`);
  return s;
}

// ---------------------------------------------------------------------------
// Step scenarios.

/** The state a step leaves behind, beside what it sent and called. */
function stepState(c) {
  return {
    footer: c.footer(),
    state: {
      selected: c.block._selected ?? null,
      selectedColumn: c.block._selectedColumn ?? null,
      brush: !!c.block._hasBrushFilter
    },
    signal: c.block._slots.map(signal).find(Boolean) || null,
    legendOff: legendOff(c.block),
    pending: c.clock.pending()
  };
}

/**
 * Open `config`, run `steps` ([label, (ctx) => void] each) and return, per
 * step, the new inputs, the new echarts calls and the state it leaves.
 * `opts.extra(ctx)` adds fields of its own to each step.
 */
function runSteps(config, steps, opts = {}) {
  const c = open(config, opts);
  const out = { config, steps: [] };
  try {
    for (const [label, fn] of steps) {
      const m = c.mark();
      fn(c);
      const s = c.since(m);
      out.steps.push({ step: label, inputs: s.inputs, calls: s.calls, ...stepState(c),
                       ...(opts.extra ? opts.extra(c) : {}) });
    }
  } finally {
    c.close();
  }
  return out;
}

// ---------------------------------------------------------------------------

const snap = (name, value) => matchSnapshot('interactions/' + name,
  JSON.parse(JSON.stringify(value)));

/** A flash target shaped like a zrender rect, for transient clicks. */
const rectTarget = (x, y, width, height, r) => ({
  shape: { x, y, width, height, r },
  transform: null,
  getBoundingRect() {
    return { x, y, width, height, clone() { return { ...this, applyTransform() {} }; } };
  }
});

module.exports = {
  ID, F, BASE, open, mountRoot, useClock, makeCtx, paramsAt, footer, signal, legendOff,
  gearOutline, bandOutline, roleRow, pickSelect, selectOptions, pickSegment,
  toggleCheckbox, typeText, section, snap, runSteps, stepState, cleanInput, summarizeCall, rectTarget, txt
};
