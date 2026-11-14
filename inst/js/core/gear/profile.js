// @ts-check
/**
 * A block's own gear (R's table_gear(), sent as `derived.gear`): its tabs and
 * sections in place of the table's, and the rows they name. A row is a
 * setting of the spec, moved into the gear's section; a value the prepare
 * script declares; the column checklist; the "In the subtitle" ticks; or an
 * editor a package registers (`V.gear.editors[key]`). Settings the gear does
 * not name keep their spec entry, so a word in the titles still opens them,
 * but have no row.
 *
 * The rows the spec does not have write settings it does have: a script
 * row `script_values`, the checklist `summaries`, the ticks `subtitle`
 * (translate()). An editor writes what it likes through `ctx.setValues`.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  V.gear = V.gear || /** @type {VizGearNamespace} */ ({});
  V.gear.editors = V.gear.editors || {};

  /** A script row's key: its variable's name behind a prefix no setting has. */
  const SV = 'sv.';

  /** A declared control as a spec argument the gear can draw (R's
   * script_control_records(): kind select, multi, segmented, number, date,
   * text). @param {VizScriptControl} sp @returns {VizArg} */
  const scriptArg = (sp) => {
    const gear = { label: sp.label, group: '', placeholder: sp.placeholder || undefined };
    const bounds = { min: sp.min ?? null, max: sp.max ?? null, minOpen: false };
    switch (sp.kind) {
      case 'select': return /** @type {VizArg} */ ({ type: { name: 'enum' }, values: sp.options || [],
                                                     gear: { ...gear, control: 'select', labels: sp.labels } });
      case 'multi': return /** @type {VizArg} */ ({ type: { name: 'enums' }, values: sp.options || [],
                                                    gear: { ...gear, control: 'select', labels: sp.labels } });
      case 'segmented': return /** @type {VizArg} */ ({ type: { name: 'bool' },
                                                        gear: { ...gear, control: 'toggle' } });
      case 'number': return /** @type {VizArg} */ ({ type: { name: 'number' }, bounds,
                                                     gear: { ...gear, control: 'number' } });
      default: return /** @type {VizArg} */ ({ type: { name: 'string' }, nullable: true,
                                               gear: { ...gear, control: 'text' } });
    }
  };

  /** The control's value as the row holds it: R sends a flag as "on"/"off"
   * and a single choice as a one-element array (a map's values).
   * @param {VizScriptControl} sp @param {unknown} v */
  const scriptValue = (sp, v) => {
    if (sp.kind === 'segmented') return v === 'on' || v === true;
    if (sp.kind !== 'multi' && Array.isArray(v)) return v[0] ?? null;
    return v;
  };

  /** Is a clause in the template? @param {string} tpl @param {string} c */
  const has = (tpl, c) => tpl.includes(c);

  /**
   * The subtitle with exactly the ticked clauses: an unticked one taken out
   * with its comma, a ticked one put in after the nearest earlier clause the
   * subtitle has (in the gear's sentence order), else before the nearest
   * later one, else at the end. Text the builder typed stays.
   * @param {string} tpl @param {[string, string][]} clauses In sentence order.
   * @param {Set<string>} on
   */
  function setWords(tpl, clauses, on) {
    let t = tpl;
    for (const [k, c] of clauses) {
      if (on.has(k) || !has(t, c)) continue;
      if (has(t, ', ' + c)) t = t.replace(', ' + c, '');
      else if (has(t, c + ', ')) t = t.replace(c + ', ', '');
      else t = t.replace(c, '');
    }
    clauses.forEach(([k, c], i) => {
      if (!on.has(k) || has(t, c)) return;
      const before = clauses.slice(0, i).reverse().find(([, p]) => has(t, p));
      const after = clauses.slice(i + 1).find(([, p]) => has(t, p));
      if (before) {
        const at = t.indexOf(before[1]) + before[1].length;
        t = t.slice(0, at) + ', ' + c + t.slice(at);
      } else if (after) {
        const at = t.indexOf(after[1]);
        t = t.slice(0, at) + c + ', ' + t.slice(at);
      } else {
        t = t ? t + ', ' + c : c;
      }
    });
    return t.trim();
  }

  /**
   * The gear's spec, sections and tabs. Rows come first, in the gear's order,
   * then the rest of the spec, which has no row.
   * @param {VizSpec} base @param {VizGearWire} wire @param {VizScript | null} script
   * @returns {VizProfile}
   */
  function build(base, wire, script) {
    /** @type {VizSpec} */
    const spec = {};
    /** @type {Record<string, [string, number]>} */
    const sections = {};
    /** @type {[string, string][]} */
    const groups = [];
    /** @type {Set<string>} */
    const keys = new Set();
    /** @type {Record<string, VizProfileRow>} */
    const rows = {};
    const inputs = /** @type {VizScriptControl[]} */ (
      /** @type {VizScriptInput[]} */ ((script && script.inputs) || [])
        .filter((sp) => sp && sp.kind !== 'error'));
    const columns = wire.columns || [];
    const clauses = Object.entries(wire.clauses || {});
    let ord = 0;
    /** @param {string} key @param {VizArg} arg @param {string} group */
    const put = (key, arg, group) => {
      spec[key] = Object.assign({}, arg, { gear: Object.assign({}, arg.gear, { group }) });
      keys.add(key);
    };
    (wire.tabs || []).forEach((tab, ti) => {
      const gk = 'tab' + ti;
      groups.push([gk, tab.title]);
      for (const sec of tab.sections || []) {
        // A section title is its key in the plan: one used by an earlier tab
        // is told apart by its tab.
        const title = sections[sec.title] ? sec.title + ' (' + tab.title + ')' : sec.title;
        sections[title] = [gk, ord++];
        for (const k of /** @type {string[]} */ ([]).concat(sec.keys || [])) {
          const sp = inputs.find((x) => x.name === k);
          const ed = V.gear.editors[k];
          if (sp) {
            rows[SV + k] = { kind: 'script', name: k, sp };
            put(SV + k, scriptArg(sp), title);
          } else if (k === 'columns' && wire.pick) {
            rows[k] = { kind: 'picks' };
            put(k, /** @type {VizArg} */ ({ type: { name: 'values' },
              gear: { label: 'Listing columns', control: 'profile' } }), title);
          } else if (k === 'columns' && columns.length) {
            rows[k] = { kind: 'columns' };
            put(k, /** @type {VizArg} */ ({ type: { name: 'values' },
              gear: { label: 'Listing columns', control: 'profile' } }), title);
          } else if (k === 'group' && (wire.groups?.values || []).length) {
            const g = /** @type {{ values: string[], labels: string[] }} */ (wire.groups);
            rows[k] = { kind: 'group' };
            put(k, /** @type {VizArg} */ ({ type: { name: 'enum' }, values: [''].concat(g.values),
              gear: { label: 'Group by', control: 'select', labels: ['None'].concat(g.labels) } }),
              title);
          } else if (k === 'words' && clauses.length) {
            rows[k] = { kind: 'words' };
            put(k, /** @type {VizArg} */ ({ type: { name: 'values' },
              gear: { label: 'In the subtitle', control: 'profile' } }), title);
          } else if (base[k] && base[k].gear) {
            put(k, base[k], title);
          } else if (ed) {
            rows[k] = { kind: 'editor' };
            put(k, /** @type {VizArg} */ ({ type: { name: 'any' },
              gear: { label: ed.label || k, control: 'profile' } }), title);
          }
        }
      }
    });
    for (const [k, arg] of Object.entries(base)) if (!spec[k]) spec[k] = arg;
    return { spec, sections, groups, keys, rows, wire, clauses,
             sig: JSON.stringify([wire, inputs.map((sp) => [sp.name, sp.kind, sp.options])]) };
  }

  /** The values of the rows the spec does not have, read off the settings
   * and the script's values. @param {VizProfile} prof @param {VizValues} cfg
   * @param {VizScript | null} script @returns {VizValues} */
  function values(prof, cfg, script) {
    /** @type {VizValues} */
    const out = {};
    const sent = /** @type {Record<string, unknown>} */ (
      script && !Array.isArray(script.values) ? script.values || {} : {});
    const names = new Set((prof.wire.columns || []).map((c) => c.name));
    for (const [key, r] of Object.entries(prof.rows)) {
      if (r.kind === 'script' && r.sp) out[key] = scriptValue(r.sp, sent[r.sp.key]);
      else if (r.kind === 'columns') {
        out[key] = (Array.isArray(cfg.summaries) ? cfg.summaries : [])
          .map((s) => s && s.name).filter((n) => names.has(n));
      } else if (r.kind === 'picks') {
        out[key] = (Array.isArray(cfg.summaries) ? cfg.summaries : [])
          .map((s) => pickId(s, names)).filter((x) => x != null);
      } else if (r.kind === 'group') {
        const by = Array.isArray(cfg.by) ? cfg.by : [];
        out[key] = by.length > 1 ? by[0] : '';
      } else if (r.kind === 'words') {
        const tpl = String(cfg.subtitle ?? '');
        out[key] = prof.clauses.filter(([, c]) => has(tpl, c)).map(([k]) => k);
      }
    }
    return out;
  }

  /** The settings a change of the gear's rows writes: script rows into
   * `script_values`, the checklist into `summaries`, the ticks into
   * `subtitle`; an editor's own key into nothing (it writes through
   * `setValues`). @param {VizProfile} prof @param {VizValues} changed
   * @param {VizValues} now @returns {VizValues} */
  function translate(prof, changed, now) {
    /** @type {VizValues} */
    const out = {};
    for (const [key, v] of Object.entries(changed)) {
      const r = prof.rows[key];
      if (!r) {
        out[key] = v;
      } else if (r.kind === 'script') {
        const held = /** @type {Record<string, unknown>} */ (
          out.script_values || now.script_values || {});
        out.script_values = Object.assign({}, held, { [/** @type {string} */ (r.name)]: v });
      } else if (r.kind === 'columns') {
        const pick = new Set(/** @type {string[]} */ (v || []));
        const cols = prof.wire.columns || [];
        const names = new Set(cols.map((c) => c.name));
        const other = (Array.isArray(now.summaries) ? now.summaries : [])
          .filter((s) => !s || !names.has(s.name));
        out.summaries = cols.filter((c) => pick.has(c.name)).map((c) => c.summary).concat(other);
      } else if (r.kind === 'picks') {
        const ids = /** @type {string[]} */ (v || []);
        const cols = prof.wire.columns || [];
        const names = new Set(cols.map((c) => c.name));
        const more = (prof.wire.pick && prof.wire.pick.more) || [];
        const held = Array.isArray(now.summaries) ? now.summaries : [];
        const other = held.filter((s) => pickId(s, names) == null);
        out.summaries = ids.map((id) => {
          if (id.startsWith('c:')) {
            const c = cols.find((x) => x.name === id.slice(2));
            return c ? c.summary : null;
          }
          const col = id.slice(2);
          const was = held.find((s) => pickId(s, names) === id);
          if (was) return was;
          const m = more.find((x) => x.name === col);
          return { type: 'field', col, name: m ? m.label : col, optional: true };
        }).filter(Boolean).concat(other);
        const sv = prof.wire.pick && prof.wire.pick.sort;
        if (sv) {
          const vals = /** @type {Record<string, unknown>} */ (
            out.script_values || now.script_values || {});
          out.script_values = Object.assign({}, vals, { [sv]: ids.length ? ids[0].slice(2) : '' });
        }
      } else if (r.kind === 'group') {
        // The outer column of a nested `by`: picked, replaced or dropped.
        const by = /** @type {string[]} */ (Array.isArray(now.by) ? now.by : []);
        const inner = by.length > 1 ? by.slice(1) : by;
        out.by = v ? [String(v)].concat(inner) : inner;
      } else if (r.kind === 'words') {
        out.subtitle = setWords(String(now.subtitle ?? ''), prof.clauses,
                                new Set(/** @type {string[]} */ (v || [])));
      }
    }
    return out;
  }

  /** A summary as a pick of the several-picks columns row: `c:<name>` for a
   * gear column, `d:<column>` for a field over a data column, null for the
   * rest (the swimlane). @param {any} s @param {Set<string>} names
   * @returns {string | null} */
  const pickId = (s, names) => {
    if (!s) return null;
    if (names.has(s.name)) return 'c:' + s.name;
    if (s.type === 'field' && typeof s.col === 'string' && s.col) return 'd:' + s.col;
    return null;
  };

  /** The several-picks columns row: one select, its tags in pick order (a
   * drag reorders them), the gear's columns first, then the data's; a pick
   * the data no longer has says so. @param {HTMLElement} el @param {VizRowCtx} ctx
   * @param {VizProfile} prof */
  function picks(el, ctx, prof) {
    const cols = prof.wire.columns || [];
    const pk = /** @type {NonNullable<VizGearWire['pick']>} */ (prof.wire.pick);
    const more = pk.more || [];
    /** @type {[string, string, string][]} id, label, note */
    const items = cols.map((c) => /** @type {[string, string, string]} */ (['c:' + c.name, c.label || c.name, '']))
      .concat(more.map((m) => /** @type {[string, string, string]} */ (['d:' + m.name, m.label, m.label === m.name ? '' : m.name])));
    const cur = /** @type {string[]} */ (Array.isArray(ctx.value) ? ctx.value : []);
    const held = Array.isArray(ctx.values.summaries) ? ctx.values.summaries : [];
    for (const id of cur) {
      if (items.some(([x]) => x === id)) continue;
      const s = held.find((x) => pickId(x, new Set(cols.map((c) => c.name))) === id);
      items.push([id, ((s && s.name) || id.slice(2)) + ' (missing)', '']);
    }
    // The select keys an option by its text: the label, made unique.
    /** @type {string[]} */
    const keys = [];
    const options = items.map(([, label, note]) => {
      let k = label;
      while (keys.includes(k)) k += ' ';
      keys.push(k);
      return note ? { value: k, label: note } : k;
    });
    const at = (/** @type {string} */ id) => items.findIndex(([x]) => x === id);
    const inst = root.Blockr.Select.multi(el, {
      options, selected: cur.map(at).filter((i) => i >= 0).map((i) => keys[i]),
      bordered: true, label: ctx.label, placeholder: 'Patient and timeline only',
      onChange: (/** @type {string[]} */ v) =>
        ctx.set(v.map((k) => keys.indexOf(k)).filter((i) => i >= 0).map((i) => items[i][0]))
    });
    ctx.scope.own({ dispose: () => inst.destroy() });
    if (pk.note) {
      const n = document.createElement('div');
      n.className = 'vz-row-note';
      n.textContent = pk.note;
      el.appendChild(n);
    }
  }

  /** @param {HTMLElement} el @param {string} label @param {boolean} on
   * @param {(on: boolean) => void} onToggle @param {string} key */
  const box = (el, label, on, onToggle, key) => {
    const h = root.Blockr.checkbox(label, on, onToggle);
    h.input.setAttribute('data-gear-input', key);
    el.appendChild(h.el);
  };

  /** A list of checkboxes, one per [key, label], writing the ticked keys.
   * @param {HTMLElement} el @param {VizRowCtx} ctx @param {[string, string][]} items */
  function checklist(el, ctx, items) {
    const now = new Set(/** @type {string[]} */ (Array.isArray(ctx.value) ? ctx.value : []));
    const list = document.createElement('div');
    list.className = 'vz-checklist';
    el.appendChild(list);
    for (const [k, label] of items) {
      box(list, label, now.has(k), (on) => {
        const next = new Set(now);
        if (on) next.add(k); else next.delete(k);
        ctx.set(items.map(([x]) => x).filter((x) => next.has(x)));
      }, ctx.key + ':' + k);
    }
  }

  /** The control of a row the spec does not have (`gear.control: "profile"`).
   * @param {HTMLElement} el @param {VizRowCtx} ctx */
  function draw(el, ctx) {
    const prof = ctx.profile;
    const r = prof && prof.rows[ctx.key];
    if (!prof || !r) return;
    if (r.kind === 'picks') {
      picks(el, ctx, prof);
    } else if (r.kind === 'columns') {
      checklist(el, ctx, (prof.wire.columns || []).map((c) => [c.name, c.label || c.name]));
    } else if (r.kind === 'words') {
      // A setting by its label; its clause in the template says where it goes.
      checklist(el, ctx, prof.clauses.map(([k]) => [k, ctx.labelOf(k)]));
    } else if (r.kind === 'editor') {
      const ed = V.gear.editors[ctx.key];
      if (ed) ed.draw(el, ctx);
    }
  }

  /** The row a title word opens in a block's own gear: `{@parent}`, the rows'
   * grouping, is the group row when there is one. @param {VizProfile | null} prof
   * @param {string} key */
  const alias = (prof, key) => (prof && key === 'parent' && prof.rows.group ? 'group' : key);

  V.gear.more = Object.assign(V.gear.more || {}, { profile: draw });
  V.gear.profile = { build, values, translate, setWords, scriptArg, scriptValue, alias, SV };
})();
