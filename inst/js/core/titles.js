// @ts-check
/**
 * Titles: the gear's template rows (title, subtitle, caption) with their
 * `@` menu. The sentence on the block's face, whose words open their
 * setting's control, is in title-words.js.
 *
 * Both read `derived.titles` from the payload (VizTitles in gear.d.ts):
 * per band the parts R resolved (`{text, arg?, by?}`) and the settings it
 * would name if they were set (`offers`); `args`, what each `{@name}`
 * prints now; and `choices`, the values a sentence lists for a word. The JS never resolves
 * a template, so the face, the preview and the exports say the same.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const ui = () => /** @type {VizUi} */ (root.Blockr);

  /** @typedef {{ label: string, tok: string, val: string, indent?: boolean }} AtItem */
  /** @typedef {{ head: string, note?: string, items: AtItem[] }} AtGroup */
  /** @typedef {{ el: HTMLElement, pos: number, query: string, idx: number,
   *              items: { tok: string, el: HTMLElement }[], scope: VizScope }} AtState */

  /** What the `@` menu offers, in groups, under public names. The prepare script's
   * values get their own group: a script value takes the word of a block setting with the
   * same name, and the group's note says which.
   * @param {VizTitles} titles @param {VizColumn[]} columns
   * @param {(key: string) => string} label @param {(key: string) => VizWordOf} [wordOf]
   * @returns {AtGroup[]} */
  function menuGroups(titles, columns, label, wordOf) {
    const args = titles.args || {};
    /** @type {AtItem[]} */
    const settings = [];
    /** @type {AtItem[]} */
    const script = [];
    /** @type {string[]} */
    const shadowed = [];
    for (const name of Object.keys(args)) {
      const w = wordOf ? wordOf(name) : null;
      const into = w && w.script ? script : settings;
      if (w && w.script && w.shadows) shadowed.push('{@' + name + '}');
      const printed = args[name] == null ? '' : String(args[name]);
      into.push({ label: w ? w.label : label(name), tok: '{@' + name + '}', val: printed });
      const col = columns.find((c) => c.name === printed);
      if (col) {
        into.push({ label: 'as its variable label', indent: true,
                    tok: '{label(@' + name + ')}', val: col.label || col.name });
      }
    }
    /** @type {AtItem[]} */
    const cols = [];
    for (const c of columns) {
      cols.push({ label: c.name, tok: '{' + c.name + '}', val: c.label || '' },
                { label: 'as its variable label', indent: true,
                  tok: '{label(' + c.name + ')}', val: c.label || c.name },
                { label: 'as a count of distinct values', indent: true,
                  tok: '{n_distinct(' + c.name + ')}', val: '' });
    }
    /** @type {AtGroup[]} */
    const groups = [];
    if (settings.length) {
      groups.push({ head: "This block's settings", note: 'Printed as a word the reader can click.',
                    items: settings });
    }
    if (script.length) {
      const note = shadowed.length
        ? shadowed.join(', ') + " is the script's, not the block setting of that name, " +
          'which stays in the gear.'
        : 'Printed as a word the reader can click.';
      groups.push({ head: "The prepare script's values", note, items: script });
    }
    groups.push({ head: 'Counts and filters', items: [
      { label: 'Row count', tok: '{n}', val: '' },
      { label: 'Filter trail', tok: '{filters}', val: 'what the filters upstream applied' }
    ] }, { head: 'Structure', items: [
      { label: 'Optional clause', tok: '[ ]', val: 'drops when its setting is empty' },
      { label: 'Line break', tok: '\\n', val: 'breaks the sentence' }
    ] });
    if (cols.length) {
      groups.push({ head: 'Data columns', note: "Prints the column's values, not its name.",
                    items: cols });
    }
    return groups;
  }

  /** Tokens that name nothing R knows; R prints them as nothing.
   * @param {string} tpl @param {VizTitles} titles @param {VizColumn[]} columns */
  function unknownTokens(tpl, titles, columns) {
    const known = new Set(['n', 'filters']);
    for (const name of Object.keys(titles.args || {})) known.add('@' + name);
    for (const c of columns) known.add(c.name);
    /** @type {string[]} */
    const bad = [];
    for (const m of String(tpl).matchAll(/\{([^{}]+)\}/g)) {
      const tok = m[1].trim();
      const inner = /^[A-Za-z_.][A-Za-z0-9_.]*\(\s*(.+?)\s*\)$/.exec(tok);
      const name = (inner ? inner[1] : tok).trim().replace(/\s+in\s+.*$/, '');
      if (!known.has(name) && !bad.includes(m[0])) bad.push(m[0]);
    }
    return bad;
  }

  /** Every argument the title, subtitle and caption name: their words, and the settings
   * offered beside them because a clause dropped.
   * @param {VizTitles | null | undefined} titles @returns {string[]} */
  function namedArgs(titles) {
    /** @type {Set<string>} */
    const out = new Set();
    for (const b of [titles && titles.title, titles && titles.subtitle, titles && titles.caption]) {
      if (!b || typeof b !== 'object') continue;
      for (const p of b.parts || []) if (p && p.arg) out.add(p.arg);
      for (const o of b.offers || []) if (typeof o === 'string') out.add(o);
    }
    return Array.from(out);
  }

  /** The resolved band as text, its slot words marked, and up to three offers. A word's
   * tooltip names what it opens, and says so when that is the prepare script's control.
   * @param {HTMLElement} el @param {VizTitleBand | null | undefined} band
   * @param {(k: string) => string} label @param {(k: string) => VizWordOf} [wordOf] */
  function paintPreview(el, band, label, wordOf) {
    el.textContent = '';
    const parts = (band && band.parts) || [];
    if (!parts.length && band && band.text) el.textContent = band.text;
    for (const p of parts) {
      if (!p || !p.text) continue;
      if (!p.arg) { el.appendChild(document.createTextNode(p.text)); continue; }
      const w = document.createElement('span');
      w.className = 'blockr-slot blockr-slot--preview';
      w.textContent = p.text;
      const wo = wordOf ? wordOf(p.arg) : null;
      w.title = wo && wo.script ? wo.label + ', from the prepare script' : label(p.arg);
      el.appendChild(w);
    }
    for (const arg of ((band && band.offers) || []).slice(0, 3)) {
      const chip = document.createElement('span');
      chip.className = 'vz-template-offer';
      chip.textContent = '+ ' + label(arg);
      el.appendChild(chip);
    }
    el.classList.toggle('vz-template-preview--empty', !el.childNodes.length);
  }

  /** Under the field, flipped above it when there is no room below.
   * @param {HTMLElement} inp @param {HTMLElement} el */
  function place(inp, el) {
    const r = inp.getBoundingClientRect();
    const box = el.getBoundingClientRect();
    const M = 8;
    el.style.left = Math.max(M, Math.min(r.left, window.innerWidth - box.width - M)) + 'px';
    let top = r.bottom + 4;
    if (top + box.height > window.innerHeight - M) {
      const above = r.top - box.height - 4;
      top = above >= M ? above : Math.max(M, window.innerHeight - box.height - M);
    }
    el.style.top = top + 'px';
  }

  /** @param {AtItem} it @param {AtState} s @param {(i: number) => void} pick
   * @param {() => void} hl */
  function menuItem(it, s, pick, hl) {
    const b = document.createElement('button');
    b.type = 'button';
    b.className = 'vz-at-item' + (it.indent ? ' vz-at-item--sub' : '');
    const lab = document.createElement('b');
    lab.textContent = it.label;
    const val = document.createElement('span');
    val.className = 'vz-at-val' + (it.val ? '' : ' vz-at-val--unset');
    val.textContent = it.val || (it.indent ? '' : 'unset');
    const tok = document.createElement('span');
    tok.className = 'vz-at-tok';
    tok.textContent = it.tok;
    b.append(lab, val, tok);
    const idx = s.items.length;
    // mousedown, prevented: a click would blur the field first, and blur
    // commits it.
    s.scope.on(b, 'mousedown', (e) => { e.preventDefault(); pick(idx); });
    s.scope.on(b, 'mouseenter', () => { s.idx = idx; hl(); });
    s.items.push({ tok: it.tok, el: b });
    return b;
  }

  /** Fill an open menu with the items that match its query.
   * @param {AtState} s @param {AtGroup[]} groups @param {(i: number) => void} pick
   * @param {() => void} hl */
  function drawMenu(s, groups, pick, hl) {
    const q = s.query.toLowerCase();
    s.el.textContent = '';
    s.items = [];
    const hit = (/** @type {AtItem} */ it) =>
      (it.label + ' ' + it.tok + ' ' + it.val).toLowerCase().includes(q);
    groups.forEach((g, gi) => {
      // A sub-row rides on the row above it.
      const hits = g.items.filter((it, i) => {
        const parent = it.indent ? g.items.slice(0, i).reverse().find((x) => !x.indent) : null;
        return hit(it) || (!!parent && hit(parent));
      });
      if (!hits.length) return;
      const h = document.createElement('div');
      h.className = 'vz-at-head';
      h.textContent = g.head;
      s.el.appendChild(h);
      if (g.note && !q) {
        const n = document.createElement('div');
        n.className = 'vz-at-note';
        n.textContent = g.note;
        s.el.appendChild(n);
      }
      for (const it of hits) s.el.appendChild(menuItem(it, s, pick, hl));
      if (gi < groups.length - 1) {
        const sep = document.createElement('div');
        sep.className = 'vz-at-sep';
        s.el.appendChild(sep);
      }
    });
    if (!s.items.length) {
      const n = document.createElement('div');
      n.className = 'vz-at-note';
      n.textContent = 'Nothing matches "' + s.query + '".';
      s.el.appendChild(n);
    }
    if (s.idx >= s.items.length) s.idx = 0;
    hl();
  }

  /** The `@` menu of one template field. Portalled to <body>; Escape, a
   * click elsewhere, a space or brace typed, or the field leaving the screen
   * close it. A pick writes the token at the `@`.
   * @param {HTMLInputElement} inp @param {VizRowCtx} ctx */
  function atMenu(inp, ctx) {
    /** @type {AtState | null} */
    let st = null;
    const close = () => {
      const s = st;
      st = null;
      if (s) s.scope.dispose();
    };
    const hl = () => {
      if (!st) return;
      const s = st;
      s.items.forEach((it, i) => it.el.classList.toggle('vz-at-item--hl', i === s.idx));
      const cur = s.items[s.idx];
      if (cur && typeof cur.el.scrollIntoView === 'function') cur.el.scrollIntoView({ block: 'nearest' });
    };
    const pick = (/** @type {number} */ i) => {
      if (!st || !st.items[i]) return;
      const raw = st.items[i].tok;
      const tok = raw === '[ ]' ? '[, ]' : raw;
      const caret = inp.selectionStart || 0;
      const from = st.pos;
      inp.value = inp.value.slice(0, from) + tok + inp.value.slice(caret);
      close();
      const pos = from + (raw === '[ ]' ? tok.length - 1 : tok.length);
      inp.focus();
      inp.setSelectionRange(pos, pos);
      inp.dispatchEvent(new Event('input'));
    };
    const draw = () => {
      if (!st) return;
      drawMenu(st, menuGroups(ctx.titles, ctx.columns, ctx.labelOf, ctx.wordOf), pick, hl);
      place(inp, st.el);
    };
    const open = () => {
      close();
      const scope = ctx.scope.own(new V.Scope());
      const el = scope.node(document.body.appendChild(document.createElement('div')));
      el.className = 'vz-at-menu';
      st = { el, pos: Math.max(0, (inp.selectionStart || 1) - 1), query: '', idx: 0, items: [],
             scope };
      const layer = ui().layer;
      if (layer) {
        const h = layer(el, { escape: close, outside: close, from: inp });
        scope.own({ dispose: () => h.remove() });
      }
      scope.on(document, 'scroll', (e) => {
        if (!st || st.el.contains(/** @type {Node} */ (e.target))) return;
        const r = inp.getBoundingClientRect();
        if (r.bottom < 0 || r.top > window.innerHeight) close();
        else place(inp, st.el);
      }, true);
      draw();
    };
    const refilter = () => {
      if (!st) return;
      const caret = inp.selectionStart || 0;
      const q = inp.value.slice(st.pos + 1, caret);
      if (caret <= st.pos || inp.value[st.pos] !== '@' || /[\s{}[\]]/.test(q)) { close(); return; }
      st.query = q;
      draw();
    };
    const keydown = (/** @type {KeyboardEvent} */ e) => {
      if (!st) return false;
      const n = st.items.length;
      if (e.key === 'Escape') { e.preventDefault(); close(); return true; }
      if (!n) return false;
      if (e.key === 'Enter' || e.key === 'Tab') { e.preventDefault(); pick(st.idx); return true; }
      const step = e.key === 'ArrowDown' ? 1 : e.key === 'ArrowUp' ? -1 : 0;
      if (!step) return false;
      e.preventDefault();
      st.idx = (st.idx + step + n) % n;
      hl();
      return true;
    };
    return { open, refilter, keydown, close };
  }

  /** A template row: the field, what the card shows now, a warning for a
   * token nothing answers to, and the help line. An emptied field goes back
   * to the automatic text (`null`).
   * @param {HTMLElement} el @param {VizRowCtx} ctx */
  function row(el, ctx) {
    const band = /** @type {VizTitleBand | null | undefined} */ (
      /** @type {any} */ (ctx.titles)[ctx.key]);
    // A line break in the saved template (one written in R code) shows as the `\n` R reads
    // as a break: a one-line field would drop it, and the row would load as edited.
    const before = ctx.value == null ? '' : String(ctx.value).replace(/\r\n|\r|\n/g, '\\n');
    const inp = document.createElement('input');
    inp.type = 'text';
    inp.className = 'blockr-text-input vz-text vz-template';
    inp.value = before;
    inp.spellcheck = false;
    inp.setAttribute('aria-label', ctx.label);
    inp.setAttribute('data-gear-input', ctx.key);
    const ph = ctx.host.placeholder && ctx.host.placeholder(ctx.key, ctx.values);
    if (ph) inp.placeholder = ph;
    const shows = document.createElement('div');
    shows.className = 'vz-template-shows';
    const turn = document.createElement('span');
    turn.className = 'vz-template-turn';
    turn.setAttribute('aria-hidden', 'true');
    turn.textContent = '↳';
    const body = document.createElement('span');
    body.className = 'vz-template-preview';
    paintPreview(body, band, ctx.labelOf, ctx.wordOf);
    // Plain text with no slots, clause or break: the line would repeat it.
    shows.classList.toggle('is-same', before !== '' && !/[{[]|\\n/.test(before));
    const stale = document.createElement('span');
    stale.className = 'vz-template-stale';
    stale.textContent = 'not applied';
    shows.append(turn, body, stale);
    const warn = document.createElement('div');
    warn.className = 'vz-template-warn';
    const note = document.createElement('div');
    note.className = 'vz-template-script';
    const help = document.createElement('div');
    help.className = 'vz-help';
    const at = document.createElement('code');
    at.className = 'vz-at-key';
    at.textContent = '@';
    help.append('Type ', at, ' for a setting or a value');
    el.append(inp, shows, warn, note, help);

    const menu = atMenu(inp, ctx);
    const sync = () => {
      (el.closest('.vz-row') || el).classList.toggle('vz-row--dirty', inp.value !== before);
      const bad = unknownTokens(inp.value, ctx.titles, ctx.columns);
      warn.hidden = !bad.length;
      warn.textContent = bad.length
        ? 'Nothing is called ' + bad.join(', ') + '. It prints nothing, and takes its [ ] clause with it.'
        : '';
      note.textContent = ctx.shadowNote ? ctx.shadowNote(inp.value) : '';
      note.hidden = !note.textContent;
    };
    const commit = () => {
      const v = inp.value.trim() === '' ? null : inp.value;
      // Unchanged against the field as it loaded, so a template with a line break is not
      // sent back as an edit when the field is left.
      if ((v ?? '') !== before) ctx.set(v);
    };
    ctx.scope.on(inp, 'input', (e) => {
      if (/** @type {InputEvent} */ (e).data === '@') menu.open();
      else menu.refilter();
      sync();
    });
    const revert = () => { inp.value = before; sync(); inp.blur(); };
    ctx.scope.on(inp, 'keydown', (ev) => {
      const e = /** @type {KeyboardEvent} */ (ev);
      if (menu.keydown(e)) return;
      if (e.key === 'Enter') { e.preventDefault(); commit(); }
      if (e.key === 'Escape') revert();
    });
    // Blockr.layer takes Escape on window, before the field's own keydown,
    // and gives it to the top layer: the tray, which closes, and the blur
    // then commits the draft. While it has the focus the field is a layer
    // of its own, so Escape reverts it. The `@` menu opens above it and
    // takes the first Escape.
    /** @type {{ remove(): void } | null} */
    let layer = null;
    const unlayer = () => { if (layer) { layer.remove(); layer = null; } };
    ctx.scope.own({ dispose: unlayer });
    ctx.scope.on(inp, 'focus', () => {
      const ly = ui().layer;
      if (!layer && ly) layer = ly(inp, { escape: () => { layer = null; revert(); } });
    });
    ctx.scope.on(inp, 'blur', () => {
      unlayer();
      menu.close();
      if (inp.value !== before) commit();
    });
    sync();
  }

  V.titles = { row, menuGroups, unknownTokens, paintPreview, namedArgs };
})();
