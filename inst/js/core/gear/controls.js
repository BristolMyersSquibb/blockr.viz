// @ts-check
/**
 * The gear's controls, one builder per `gear.control` of the spec. A builder
 * gets the row's context (VizRowCtx: the argument, its value, the columns,
 * the host's hooks, a Scope and `set()`), draws into the row and calls
 * `set(value)` with a value in its wire form. It never sends anything itself.
 *
 * blockr.ui's widgets are used where the page has them (Blockr.Select,
 * Blockr.segmented, Blockr.checkbox); otherwise plain form elements.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  V.gear = V.gear || /** @type {VizGearNamespace} */ ({});
  const ui = () => /** @type {VizUi} */ (root.Blockr);

  /** @param {unknown} v */
  const isSet = (v) => {
    if (v == null || v === '') return false;
    if (Array.isArray(v)) return v.length > 0;
    if (typeof v === 'object') return Object.keys(/** @type {object} */ (v)).length > 0;
    return true;
  };
  /** @param {unknown} a @param {unknown} b */
  const same = (a, b) => JSON.stringify(a == null ? null : a) === JSON.stringify(b == null ? null : b);

  /** The number formats both blocks offer (type `format`). */
  /** @type {VizChoice[]} */
  const FORMATS = [
    { value: 'number', label: 'Auto' }, { value: 0, label: '1,235' },
    { value: 1, label: '1,234.6' }, { value: 2, label: '1,234.57' },
    { value: 3, label: '1,234.568' }, { value: 4, label: '1,234.5679' },
    { value: 'compact', label: '1.2K' }, { value: 'percent', label: '12.3%' }
  ];

  /** Free text that takes more than one line. */
  const MULTILINE = ['script', 'expr', 'fn'];

  /** @type {Record<string, string>} */
  const PLACEHOLDERS = {
    lines: 'e.g. 2 or Target: 4',
    ranges: 'e.g. Recession: 2008-01-01 to 2009-06-30'
  };

  // -- values as text ---------------------------------------------------------

  /** @param {string} t */
  const scalar = (t) => (t !== '' && Number.isFinite(Number(t)) ? Number(t) : t);

  /** "Target: 4; 6" as `[{value: 4, label: "Target"}, {value: 6, label: null}]`.
   * A colon needs a space after it, so a time of day is no label; numbers
   * apart by spaces are several lines.
   * @param {string} text @param {boolean} numbers */
  function parseLines(text, numbers) {
    /** @type {{ value: number | string, label: string | null }[]} */
    const out = [];
    for (const raw of String(text).split(/[;,]/)) {
      const part = raw.trim();
      if (!part) continue;
      const m = /^([^:]+):\s+(.*)$/.exec(part);
      const label = m ? m[1].trim() : null;
      const v = (m ? m[2] : part).trim();
      if (!v) continue;
      const tok = v.split(/\s+/);
      if (tok.length > 1 && tok.every((t) => Number.isFinite(Number(t)))) {
        tok.forEach((t, i) => out.push({ value: Number(t), label: i ? null : label }));
      } else {
        const val = scalar(v);
        if (numbers && typeof val !== 'number') continue;
        out.push({ value: val, label });
      }
    }
    return out;
  }

  /** "Recession: 2008-01-01 to 2009-06-30; ..." as a named list of two values.
   * @param {string} text */
  function parseRanges(text) {
    /** @type {Record<string, { value: number | string, label: null }[]>} */
    const out = {};
    let n = 0;
    for (const raw of String(text).split(';')) {
      const part = raw.trim();
      if (!part) continue;
      const m = /^(.*?):\s+(.*)$/.exec(part);
      const span = (m ? m[2] : part).split(/\s+to\s+/i).map((b) => scalar(b.trim()));
      if (span.length !== 2 || span.some((b) => b === '')) continue;
      n += 1;
      const name = (m && m[1].trim()) || 'Range ' + n;
      out[name] = span.map((value) => ({ value, label: null }));
    }
    return out;
  }

  /** @param {unknown} x */
  const labelled = (x) => /** @type {{ value: unknown, label: string | null }[]} */ (
    Array.isArray(x) ? x : []);

  /** The value as one line of text.
   * @param {VizArg} arg @param {unknown} value @returns {string} */
  function toText(arg, value) {
    if (value == null) return '';
    const parse = arg.gear && arg.gear.parse;
    if (parse === 'ranges') {
      return Object.entries(/** @type {Record<string, unknown>} */ (value)).map(([name, span]) =>
        name + ': ' + labelled(span).map((b) => String(b.value)).join(' to ')).join('; ');
    }
    if (parse === 'lines') {
      return labelled(value).map((l) => (l.label ? l.label + ': ' : '') + String(l.value)).join('; ');
    }
    return Array.isArray(value) ? value.join(', ') : String(value);
  }

  /** Text back to the wire form; `null` for an emptied nullable field.
   * @param {VizArg} arg @param {string} text @returns {unknown} */
  function fromText(arg, text) {
    const t = String(text).trim();
    const parse = arg.gear && arg.gear.parse;
    const name = arg.type.name;
    let v;
    if (parse === 'ranges') v = parseRanges(t);
    else if (parse === 'lines') v = parseLines(t, name === 'numbers');
    else if (name === 'number' || name === 'integer') {
      const n = Number(t);
      v = t === '' || !Number.isFinite(n) ? null : (name === 'integer' ? Math.round(n) : n);
    } else v = t;
    if (isSet(v)) return v;
    return arg.nullable ? null : (name === 'string' ? '' : arg.default ?? null);
  }

  // -- choices ----------------------------------------------------------------

  /** @param {VizRowCtx} ctx @param {VizColumn[]} cols @returns {VizChoice[]} */
  function columnChoices(ctx, cols) {
    const named = ctx.host.choices ? ctx.host.choices(ctx.key, ctx.values, ctx.columns) : null;
    /** @type {VizChoice[]} */
    const out = (ctx.arg.also || []).map((w) => {
      const c = named && named.find((x) => x.value === w);
      return { value: w, label: c ? c.label : w.charAt(0).toUpperCase() + w.slice(1) };
    });
    for (const c of cols) out.push({ value: c.name, label: c.name, note: c.label || '' });
    return out;
  }

  /** What a select-like control offers, in order, the current value always
   * among them.
   * @param {VizRowCtx} ctx @returns {VizChoice[]} */
  function choicesFor(ctx) {
    const { arg, host } = ctx;
    const name = arg.type.name;
    /** @type {VizChoice[]} */
    let out;
    if (name === 'column' || (name === 'columns' && ctx.arg.gear?.control === 'column')) {
      const cols = host.columns ? host.columns(ctx.key, ctx.values, ctx.columns) : ctx.columns;
      out = columnChoices(ctx, cols);
    } else if (name === 'columns') {
      const cols = host.columns ? host.columns(ctx.key, ctx.values, ctx.columns) : ctx.columns;
      out = cols.map((c) => ({ value: c.name, label: c.name, note: c.label || '' }));
    } else if (name === 'format') {
      out = FORMATS.slice();
    } else if (arg.values) {
      const labels = (arg.gear && arg.gear.labels) || [];
      out = arg.values.map((v, i) => ({ value: v, label: labels[i] || String(v) }));
    } else {
      out = (host.choices && host.choices(ctx.key, ctx.values, ctx.columns)) || [];
    }
    const cur = Array.isArray(ctx.value) && name !== 'columns' && name !== 'enums'
      ? null : ctx.value;
    const curs = Array.isArray(cur) ? cur : (isSet(cur) ? [cur] : []);
    for (const c of curs) {
      if (!out.some((o) => same(o.value, c))) {
        const missing = name === 'column' || name === 'columns';
        out.push({ value: c, label: String(c) + (missing ? ' (missing)' : '') });
      }
    }
    return out;
  }

  /** Whether a single pick may be left empty, and what empty is called.
   * @param {VizRowCtx} ctx */
  function emptyChoice(ctx) {
    if (ctx.required || !ctx.arg.nullable) return null;
    const name = ctx.arg.type.name;
    return { value: null, label: name === 'column' || name === 'columns' ? '(none)' : 'Auto' };
  }

  // -- widgets ----------------------------------------------------------------

  /** @param {VizChoice} c */
  const display = (c) => (c.note ? c.label + ' (' + c.note + ')' : c.label);

  /** Choices as Blockr.Select options. Select prints an object as its value
   * with its label muted, which suits a column (name, variable label); a
   * fixed option goes in as its label alone. `keys[i]` is the string that
   * stands for choice i.
   * @param {VizChoice[]} choices */
  function uiOptions(choices) {
    /** @type {string[]} */
    const keys = [];
    const options = choices.map((c) => {
      let k = c.note != null ? String(c.value ?? c.label) : c.label;
      while (keys.includes(k)) k += ' ';
      keys.push(k);
      return c.note ? { value: k, label: c.note } : k;
    });
    return { options, keys };
  }

  /** A single pick among `choices`; `onPick` gets the chosen value.
   * @param {HTMLElement} el @param {VizChoice[]} choices @param {unknown} value
   * @param {(v: unknown) => void} onPick @param {VizRowCtx} ctx */
  function pickOne(el, choices, value, onPick, ctx) {
    const at = choices.findIndex((c) => same(c.value, value));
    const Sel = ui().Select;
    if (Sel) {
      const { options, keys } = uiOptions(choices);
      const inst = Sel.single(el, {
        options, selected: at < 0 ? '' : keys[at], allowEmpty: true, bordered: true,
        label: ctx.label,
        onChange: (/** @type {string} */ v) => {
          const i = keys.indexOf(v);
          if (i >= 0) onPick(choices[i].value);
        }
      });
      ctx.scope.own({ dispose: () => inst.destroy() });
      return;
    }
    const sel = document.createElement('select');
    sel.className = 'dd-select';
    sel.setAttribute('aria-label', ctx.label);
    sel.setAttribute('data-gear-input', ctx.key);
    choices.forEach((c, i) => {
      const o = document.createElement('option');
      o.value = String(i);
      o.textContent = display(c);
      sel.appendChild(o);
    });
    sel.value = at < 0 ? '' : String(at);
    ctx.scope.on(sel, 'change', () => onPick(choices[Number(sel.value)].value));
    el.appendChild(sel);
  }

  /** Several picks among `choices`, in pick order.
   * @param {HTMLElement} el @param {VizChoice[]} choices @param {unknown[]} values
   * @param {(v: unknown[]) => void} onPick @param {VizRowCtx} ctx */
  function pickMany(el, choices, values, onPick, ctx) {
    const idx = values.map((v) => choices.findIndex((c) => same(c.value, v))).filter((i) => i >= 0);
    const Sel = ui().Select;
    if (Sel) {
      const { options, keys } = uiOptions(choices);
      const inst = Sel.multi(el, {
        options, selected: idx.map((i) => keys[i]), bordered: true, label: ctx.label,
        onChange: (/** @type {string[]} */ v) =>
          onPick(v.map((k) => keys.indexOf(k)).filter((i) => i >= 0).map((i) => choices[i].value))
      });
      ctx.scope.own({ dispose: () => inst.destroy() });
      return;
    }
    const sel = document.createElement('select');
    sel.multiple = true;
    sel.className = 'dd-select dd-select--multi';
    sel.setAttribute('aria-label', ctx.label);
    sel.setAttribute('data-gear-input', ctx.key);
    choices.forEach((c, i) => {
      const o = document.createElement('option');
      o.value = String(i);
      o.textContent = display(c);
      o.selected = idx.includes(i);
      sel.appendChild(o);
    });
    ctx.scope.on(sel, 'change', () => onPick(Array.from(sel.options).filter((o) => o.selected)
      .map((o) => choices[Number(o.value)].value)));
    el.appendChild(sel);
  }

  /** @param {HTMLElement} el @param {string} label @param {boolean} on
   * @param {(on: boolean) => void} onToggle @param {VizRowCtx} ctx */
  function checkbox(el, label, on, onToggle, ctx) {
    const box = ui().checkbox;
    if (box) {
      const h = box(label, on, onToggle);
      h.input.setAttribute('data-gear-input', ctx.key);
      el.appendChild(h.el);
      return;
    }
    const wrap = document.createElement('label');
    wrap.className = 'blockr-checkbox';
    const input = document.createElement('input');
    input.type = 'checkbox';
    input.checked = on;
    input.setAttribute('data-gear-input', ctx.key);
    const txt = document.createElement('span');
    txt.className = 'blockr-checkbox__label';
    txt.textContent = label;
    wrap.append(input, txt);
    ctx.scope.on(input, 'change', () => onToggle(input.checked));
    el.appendChild(wrap);
  }

  /** @param {HTMLElement} el @param {VizChoice[]} choices @param {unknown} value
   * @param {(v: unknown) => void} onPick @param {VizRowCtx} ctx */
  function segments(el, choices, value, onPick, ctx) {
    const at = String(choices.findIndex((c) => same(c.value, value)));
    const opts = choices.map((c, i) => ({ value: String(i), label: c.label }));
    const pick = (/** @type {string} */ v) => onPick(choices[Number(v)].value);
    const seg = ui().segmented;
    if (seg) {
      el.appendChild(seg(opts, at, pick, { label: ctx.label }).el);
      return;
    }
    const wrap = document.createElement('div');
    wrap.className = 'blockr-segmented';
    wrap.setAttribute('role', 'radiogroup');
    wrap.setAttribute('aria-label', ctx.label);
    for (const o of opts) {
      const b = document.createElement('button');
      b.type = 'button';
      b.className = 'blockr-segmented__seg' + (o.value === at ? ' is-selected' : '');
      b.setAttribute('role', 'radio');
      b.setAttribute('aria-checked', o.value === at ? 'true' : 'false');
      b.textContent = o.label;
      ctx.scope.on(b, 'click', () => { if (o.value !== at) pick(o.value); });
      wrap.appendChild(b);
    }
    el.appendChild(wrap);
  }

  /** A text field that commits on Enter or when it is left; Escape puts the
   * value back.
   * @param {HTMLElement} el @param {VizRowCtx} ctx
   * @param {{ multiline?: boolean, type?: string, onInput?: (inp: HTMLInputElement, e: Event) => void,
   *           onKeydown?: (e: KeyboardEvent, inp: HTMLInputElement) => boolean,
   *           commit?: (text: string) => void }} [opts] */
  function field(el, ctx, opts = {}) {
    const inp = /** @type {HTMLInputElement} */ (document.createElement(
      opts.multiline ? 'textarea' : 'input'));
    if (!opts.multiline) inp.type = opts.type || 'text';
    inp.className = 'blockr-text-input dd-text' + (opts.multiline ? ' dd-script-editor' : '');
    const before = ctx.arg.type.name === 'template' ? String(ctx.value ?? '')
      : toText(ctx.arg, ctx.value);
    inp.value = before;
    inp.setAttribute('aria-label', ctx.label);
    inp.setAttribute('data-gear-input', ctx.key);
    const ph = (ctx.host.placeholder && ctx.host.placeholder(ctx.key, ctx.values)) ||
      PLACEHOLDERS[(ctx.arg.gear && ctx.arg.gear.parse) || ''] || '';
    if (ph) inp.placeholder = ph;
    const commit = opts.commit || ((/** @type {string} */ t) => {
      const v = fromText(ctx.arg, t);
      if (!same(v, ctx.value)) ctx.set(v);
    });
    ctx.scope.on(inp, 'keydown', (ev) => {
      const e = /** @type {KeyboardEvent} */ (ev);
      if (opts.onKeydown && opts.onKeydown(e, inp)) return;
      if (e.key === 'Enter' && !(opts.multiline && !e.metaKey && !e.ctrlKey)) {
        e.preventDefault();
        commit(inp.value);
      } else if (e.key === 'Escape') {
        inp.value = before;
        inp.blur();
      }
    });
    ctx.scope.on(inp, 'blur', () => { if (inp.value !== before) commit(inp.value); });
    if (opts.onInput) {
      const f = opts.onInput;
      ctx.scope.on(inp, 'input', (e) => f(inp, e));
    }
    el.appendChild(inp);
    return inp;
  }

  // -- the controls -----------------------------------------------------------

  /** One column; also the column of a highlight, and one column of a
   * `columns` argument (an interval's low and high).
   * @param {HTMLElement} el @param {VizRowCtx} ctx */
  function column(el, ctx) {
    const name = ctx.arg.type.name;
    if (name === 'named') return namedColumn(el, ctx);
    const many = name === 'columns';
    const cur = many ? (Array.isArray(ctx.value) ? ctx.value[0] : null) : ctx.value;
    const empty = emptyChoice(ctx);
    const choices = /** @type {VizChoice[]} */ (empty ? [empty] : []).concat(choicesFor({ ...ctx, value: cur }));
    pickOne(el, choices, cur ?? null, (v) => ctx.set(many ? (v == null ? null : [v]) : v), ctx);
  }

  /** A column and some of its values, as `{column: [{value, label}]}`.
   * @param {HTMLElement} el @param {VizRowCtx} ctx */
  function namedColumn(el, ctx) {
    const cur = /** @type {Record<string, unknown>} */ (ctx.value || {});
    const col = Object.keys(cur)[0] || null;
    const cols = ctx.host.columns ? ctx.host.columns(ctx.key, ctx.values, ctx.columns) : ctx.columns;
    const choices = /** @type {VizChoice[]} */ ([{ value: null, label: '(none)' }]).concat(columnChoices(ctx, cols));
    pickOne(el, choices, col, (v) => ctx.set(v == null ? null : { [String(v)]: [] }), ctx);
    if (!col) return;
    const vals = labelled(cur[col]).map((x) => x.value);
    const offered = (ctx.host.choices && ctx.host.choices(ctx.key, ctx.values, ctx.columns)) || [];
    const all = offered.concat(vals.filter((v) => !offered.some((o) => same(o.value, v)))
      .map((v) => ({ value: v, label: String(v) })));
    const sub = document.createElement('div');
    sub.className = 'dd-row-sub';
    el.appendChild(sub);
    pickMany(sub, all, vals, (vs) =>
      ctx.set({ [col]: vs.map((value) => ({ value, label: null })) }), ctx);
  }

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function columns(el, ctx) {
    const cur = Array.isArray(ctx.value) ? ctx.value : [];
    pickMany(el, choicesFor(ctx), cur, (vs) =>
      ctx.set(vs.length || !ctx.arg.nullable ? vs : null), ctx);
  }

  /** Some values of the column another argument names, offered by the host.
   * @param {HTMLElement} el @param {VizRowCtx} ctx */
  function valuesCtl(el, ctx) {
    const cur = labelled(ctx.value).map((x) => x.value);
    const choices = choicesFor({ ...ctx, value: cur });
    pickMany(el, choices, cur, (vs) => ctx.set(vs.length ? vs.map((value) => ({ value, label: null }))
      : (ctx.arg.nullable ? null : [])), ctx);
  }

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function select(el, ctx) {
    if (ctx.arg.type.name === 'columns' || ctx.arg.type.name === 'enums') return columns(el, ctx);
    const empty = emptyChoice(ctx);
    const choices = /** @type {VizChoice[]} */ (empty ? [empty] : []).concat(choicesFor(ctx));
    pickOne(el, choices, ctx.value ?? null, (v) => ctx.set(v), ctx);
  }

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function segmented(el, ctx) {
    if (ctx.arg.type.name === 'bool') {
      const labels = (ctx.arg.gear && ctx.arg.gear.labels) || [];
      checkbox(el, labels[0] || ctx.label, ctx.value === true, (on) => ctx.set(on), ctx);
      return;
    }
    segments(el, choicesFor(ctx), ctx.value ?? null, (v) => ctx.set(v), ctx);
  }

  /** A checkbox; on a column it switches the setting on with its first
   * keyword (drill "auto") and shows the column picker beside it.
   * @param {HTMLElement} el @param {VizRowCtx} ctx */
  function toggle(el, ctx) {
    if (ctx.arg.type.name === 'bool') {
      checkbox(el, ctx.label, ctx.value === true, (on) => ctx.set(on), ctx);
      return;
    }
    const on = isSet(ctx.value);
    const choices = choicesFor(ctx);
    checkbox(el, ctx.label, on, (now) =>
      ctx.set(now ? (choices[0] ? choices[0].value : null) : null), ctx);
    if (!on) return;
    const sub = document.createElement('div');
    sub.className = 'dd-row-sub';
    el.appendChild(sub);
    pickOne(sub, choices, ctx.value, (v) => ctx.set(v), ctx);
  }

  /** @param {VizArg} arg */
  const bounds = (arg) => {
    const b = arg.bounds || { min: null, max: null, minOpen: false };
    const step = arg.type.name === 'integer' ? 1 : 0.1;
    const min = b.min == null ? -Infinity : (b.minOpen ? b.min + step : b.min);
    return { min, max: b.max == null ? Infinity : b.max, step };
  };

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function number(el, ctx) {
    const { min, max } = bounds(ctx.arg);
    const inp = field(el, ctx, {
      type: 'number',
      commit: (t) => {
        let v = /** @type {number | null} */ (fromText(ctx.arg, t));
        if (v != null) v = Math.min(max, Math.max(min, v));
        if (!same(v, ctx.value)) ctx.set(v);
      }
    });
    if (Number.isFinite(min)) inp.min = String(min);
    if (Number.isFinite(max)) inp.max = String(max);
    inp.step = ctx.arg.type.name === 'integer' ? '1' : 'any';
  }

  /** The prepare script sits behind a checkbox until there is one;
   * unticking it drops the script.
   * @param {HTMLElement} el @param {VizRowCtx} ctx */
  function scriptCtl(el, ctx) {
    const box = document.createElement('div');
    box.className = 'dd-script-wrap';
    const show = (/** @type {boolean} */ on) => { box.style.display = on ? '' : 'none'; };
    show(isSet(ctx.value));
    checkbox(el, 'Use a prepare script', isSet(ctx.value), (on) => {
      show(on);
      if (!on && isSet(ctx.value)) ctx.set(null);
    }, { ...ctx, key: ctx.key + '_on' });
    el.appendChild(box);
    field(box, ctx, { multiline: true });
  }

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function textCtl(el, ctx) {
    if (ctx.key === 'script') scriptCtl(el, ctx);
    else field(el, ctx, { multiline: MULTILINE.includes(ctx.key) });
  }

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function slider(el, ctx) {
    const { min, max, step } = bounds(ctx.arg);
    const inp = document.createElement('input');
    inp.type = 'range';
    inp.className = 'dd-slider';
    inp.min = String(Number.isFinite(min) ? min : 0);
    inp.max = String(Number.isFinite(max) ? max : 10);
    inp.step = String(step);
    inp.value = String(ctx.value ?? ctx.arg.default ?? inp.min);
    inp.setAttribute('aria-label', ctx.label);
    inp.setAttribute('data-gear-input', ctx.key);
    const out = document.createElement('span');
    out.className = 'dd-slider-value';
    out.textContent = inp.value + '×';
    ctx.scope.on(inp, 'input', () => { out.textContent = inp.value + '×'; });
    ctx.scope.on(inp, 'change', () => ctx.set(Number(inp.value)));
    el.append(inp, out);
  }

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function tiles(el, ctx) {
    const choices = choicesFor(ctx);
    const icon = (/** @type {unknown} */ v) => (ctx.host.icon ? ctx.host.icon(ctx.key, v) : '') ||
      V.gear.icons[ctx.key + ':' + String(v)] || '';
    const groups = (ctx.host.tiles && ctx.host.tiles(ctx.key, ctx.values)) ||
      [{ values: choices.map((c) => c.value) }];
    for (const g of groups) {
      // One block per group: its heading over its tiles.
      const box = document.createElement('div');
      box.className = 'dd-tile-group';
      el.appendChild(box);
      if (g.label) {
        const head = document.createElement('div');
        head.className = 'dd-type-group-head';
        head.textContent = g.label;
        box.appendChild(head);
      }
      const opts = choices.filter((c) => g.values.some((v) => same(v, c.value)))
        .map((c) => ({ value: String(c.value), label: c.label, icon: icon(c.value) }));
      V.gear.tiles.build(box, opts, String(ctx.value), (v) => {
        const c = choices.find((x) => String(x.value) === v);
        if (c) ctx.set(c.value);
      }, ctx.scope);
    }
  }

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function template(el, ctx) {
    V.titles.row(el, ctx);
  }

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function none(el, ctx) {
    if (!ctx.host.custom) return;
    ctx.host.custom(ctx.key, el, { values: ctx.values, columns: ctx.columns, scope: ctx.scope,
                                   set: ctx.set });
  }

  /** @type {Record<string, (el: HTMLElement, ctx: VizRowCtx) => void>} */
  const BUILD = { column, columns, values: valuesCtl, select, segmented, toggle, number,
                  text: textCtl, slider,
                  tiles, template, none };

  /** Draw a row's controls. A template row draws its own field, preview and
   * help.
   * @param {HTMLElement} el @param {VizRowCtx} ctx */
  function build(el, ctx) {
    const control = (ctx.arg.gear && ctx.arg.gear.control) || 'text';
    (BUILD[control] || textCtl)(el, ctx);
  }

  V.gear.controls = { build, options: choicesFor, toText, fromText, isSet, same, emptyChoice,
                       uiOptions };
})();
