// @ts-check
/**
 * Chart block: set axis limits as a phrase at the end of the subtitle,
 * "y from 20 to auto". Each end is a word the reader clicks to type over;
 * a blank end is auto. The × after a phrase clears that axis. The phrase
 * shows only while a limit is set, and only with a gear (the setting goes
 * through it). It is painted after the subtitle on every payload from R, so
 * a kept zoom (zoom.js) or an edit shows once R echoes it.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;
  const ZO = NS.zoomOption;

  /** @typedef {{ role: 'x' | 'y', name: string, ends: (number | string | null)[] }} LimitsPart */

  /** The phrases a config shows, x first. On the aggregated types y is the
   * value axis, whichever way it lies, so it reads "values".
   * @param {Record<string, any>} cfg @returns {LimitsPart[]} */
  const parts = (cfg) => /** @type {('x' | 'y')[]} */ (['x', 'y'])
    .filter((role) => ZO.hasLimits(cfg, role))
    .map((role) => ({
      role,
      name: role === 'y' && NS.familyOf(cfg.type) === 'aggregated' ? 'values' : role,
      ends: cfg[role + '_limits'].map((/** @type {unknown} */ v) => (v == null || v === '' ? null : v))
    }));

  /** A typed end as the limit takes it: blank is auto (null), a date stays
   * text, else a number; undefined when it is neither.
   * @param {string} text @param {boolean} dates
   * @returns {number | string | null | undefined} */
  const parseEnd = (text, dates) => {
    const t = text.trim();
    if (t === '' || t === 'auto') return null;
    if (dates) return Number.isFinite(Date.parse(t)) ? t : undefined;
    const n = Number(t);
    return Number.isFinite(n) ? n : undefined;
  };

  /** Whether a pair runs from below to, where both ends are set.
   * @param {(number | string | null)[]} pair */
  const ordered = (pair) => {
    if (pair[0] == null || pair[1] == null) return true;
    const num = (/** @type {number | string} */ v) => (typeof v === 'number' ? v : Date.parse(v));
    return num(pair[0]) < num(pair[1]);
  };

  /** The pair after one end is typed over; undefined when R would refuse it.
   * Both ends auto is null.
   * @param {LimitsPart} part @param {number} i @param {string} text */
  const next = (part, i, text) => {
    const dates = part.ends.some((v) => typeof v === 'string');
    const v = parseEnd(text, dates);
    if (v === undefined) return undefined;
    const pair = part.ends.slice();
    pair[i] = v;
    if (!ordered(pair)) return undefined;
    return pair.every((x) => x == null) ? null : pair;
  };

  /** Type over an end: Enter or leaving commits, Escape puts the word back.
   * @param {VizScope} scope @param {HTMLElement} word @param {(text: string) => void} commit */
  const edit = (scope, word, commit) => {
    const es = scope.own(new V.Scope());
    const before = word.dataset.auto ? '' : String(word.textContent);
    const input = document.createElement('input');
    input.type = 'text';
    input.className = 'blockr-slot-input';
    input.value = before;
    input.placeholder = 'auto';
    input.size = Math.max(4, before.length + 1);
    input.setAttribute('aria-label', String(word.getAttribute('aria-label')));
    let done = false;
    const finish = (/** @type {boolean} */ ok) => {
      if (done) return;
      done = true;
      es.dispose();
      input.replaceWith(word);
      if (ok && input.value.trim() !== before) commit(input.value);
      else word.focus();
    };
    es.on(input, 'click', (e) => e.stopPropagation());
    es.on(input, 'keydown', (ev) => {
      const e = /** @type {KeyboardEvent} */ (ev);
      e.stopPropagation();
      if (e.key === 'Enter') { e.preventDefault(); finish(true); }
      if (e.key === 'Escape') { e.preventDefault(); finish(false); }
    });
    es.on(input, 'blur', () => finish(true));
    word.replaceWith(input);
    input.focus();
    input.select();
  };

  /** @param {VizScope} scope @param {HTMLElement} el @param {() => void} fn */
  const press = (scope, el, fn) => {
    el.setAttribute('role', 'button');
    el.setAttribute('tabindex', '0');
    scope.on(el, 'click', (e) => { e.stopPropagation(); fn(); });
    scope.on(el, 'keydown', (ev) => {
      const e = /** @type {KeyboardEvent} */ (ev);
      if (e.key !== 'Enter' && e.key !== ' ') return;
      e.preventDefault();
      fn();
    });
  };

  /** The phrase at the end of the view's subtitle, painted again from the
   * current settings. @param {ChartView} view */
  const paint = (view) => {
    const sub = view.frame.subEl;
    if (view._limitsScope) view._limitsScope.dispose();
    const old = sub.querySelector('.vz-limits');
    if (old) old.remove();
    const list = view.gear ? parts(view.settings) : [];
    if (!list.length || !view.gear) return;
    const gear = view.gear;
    const ps = view._limitsScope = view.scope.own(new V.Scope());
    const box = document.createElement('span');
    box.className = 'vz-limits';
    if (sub.textContent) box.classList.add('vz-limits--after');
    list.forEach((part, pi) => {
      if (pi) box.appendChild(document.createTextNode(', '));
      const key = part.role + '_limits';
      box.appendChild(document.createTextNode(part.name + ' from '));
      part.ends.forEach((v, i) => {
        if (i) box.appendChild(document.createTextNode(' to '));
        const w = document.createElement('span');
        w.className = 'blockr-slot' + (v == null ? ' vz-limits__auto' : '');
        w.textContent = v == null ? 'auto' : String(v);
        if (v == null) w.dataset.auto = '1';
        w.setAttribute('aria-label', part.name + (i ? ' to' : ' from'));
        press(ps, w, () => edit(ps, w, (text) => {
          const pair = next(part, i, text);
          if (pair !== undefined) gear.set({ [key]: pair });
        }));
        box.appendChild(w);
      });
      const x = document.createElement('span');
      x.className = 'vz-limits__clear';
      x.textContent = '×';
      x.title = 'Clear ' + part.name + ' limits';
      x.setAttribute('aria-label', x.title);
      press(ps, x, () => gear.set({ [key]: null }));
      box.appendChild(x);
    });
    sub.appendChild(box);
    sub.hidden = false;
    view.frame.titleBox.hidden = false;
  };

  NS.limitsPhrase = { parts, next, paint };
})();
