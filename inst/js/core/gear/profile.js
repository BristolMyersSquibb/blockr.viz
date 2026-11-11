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
                                                     gear: { ...gear, control: 'select' } });
      case 'multi': return /** @type {VizArg} */ ({ type: { name: 'enums' }, values: sp.options || [],
                                                    gear: { ...gear, control: 'select' } });
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

  /** The checklist column a summary is: by its name, or an unnamed one by its
   * data column. @param {VizProfile} prof @param {any} s @returns {string | null} */
  const columnOf = (prof, s) => {
    if (!s) return null;
    const hit = (prof.wire.columns || []).find((c) => (s.name != null
      ? c.summary.name === s.name : c.summary.name == null && c.summary.col === s.col));
    return hit ? hit.name : null;
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
    for (const [key, r] of Object.entries(prof.rows)) {
      if (r.kind === 'script' && r.sp) out[key] = scriptValue(r.sp, sent[r.sp.key]);
      else if (r.kind === 'columns') {
        out[key] = (Array.isArray(cfg.summaries) ? cfg.summaries : [])
          .map((s) => columnOf(prof, s)).filter((n) => n != null);
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
        const other = (Array.isArray(now.summaries) ? now.summaries : [])
          .filter((s) => columnOf(prof, s) == null);
        out.summaries = cols.filter((c) => pick.has(c.name)).map((c) => c.summary).concat(other);
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
    if (r.kind === 'columns') {
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
