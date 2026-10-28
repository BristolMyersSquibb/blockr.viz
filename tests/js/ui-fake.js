/* A stand-in for the blockr.ui calls the core gear makes (Blockr.Select,
 * checkbox, segmented, gearTray, trayNav), for tests that run without
 * blockr.ui's scripts. It draws plain form elements a test can drive: a
 * <select> for a pick, a checkbox input, a row of buttons, the tray's open
 * class. The gear's code takes the same path as in the browser; only the
 * widgets are simpler. Handlers are `on...` properties, which go with
 * their element and stay out of the tests' listener counts. */
'use strict';

/**
 * Put the fake on `win.Blockr`, keeping whatever is there already.
 * @param {Window & Record<string, any>} win
 */
function installUiFake(win) {
  const doc = win.document;
  const B = win.Blockr = win.Blockr || {};

  /** An option's value and its text: a string is both; an object prints its
   * value with its label after it, as Blockr.Select shows a column. */
  const opt = (o) => (typeof o === 'string' ? { value: o, text: o }
    : { value: String(o.value), text: String(o.value) + (o.label ? ' (' + o.label + ')' : '') });

  /** @param {HTMLElement} el @param {Record<string, any>} cfg @param {boolean} multi */
  const select = (el, cfg, multi) => {
    const sel = doc.createElement('select');
    sel.className = multi ? 'ui-fake-select ui-fake-select--multi' : 'ui-fake-select';
    sel.multiple = multi;
    if (cfg.label) sel.setAttribute('aria-label', cfg.label);
    if (cfg.placeholder) {
      const first = doc.createElement('option');
      first.value = '';
      first.textContent = cfg.placeholder;
      sel.appendChild(first);
    }
    const chosen = new Set([].concat(cfg.selected == null ? [] : cfg.selected).map(String));
    for (const o of cfg.options || []) {
      const { value, text } = opt(o);
      const e = doc.createElement('option');
      e.value = value;
      e.textContent = text;
      e.selected = chosen.has(value);
      sel.appendChild(e);
    }
    if (!multi && !chosen.size) sel.value = '';
    const get = () => (multi ? Array.from(sel.options).filter((o) => o.selected).map((o) => o.value)
      : sel.value);
    sel.onchange = () => { if (cfg.onChange) cfg.onChange(get()); };
    el.appendChild(sel);
    return { getValue: get, destroy: () => sel.remove() };
  };

  B.Select = B.Select || {
    single: (el, cfg) => select(el, cfg, false),
    multi: (el, cfg) => select(el, cfg, true)
  };

  B.checkbox = B.checkbox || ((label, checked, onChange) => {
    const el = doc.createElement('label');
    el.className = 'blockr-checkbox';
    const input = doc.createElement('input');
    input.type = 'checkbox';
    input.checked = checked;
    const txt = doc.createElement('span');
    txt.className = 'blockr-checkbox__label';
    txt.textContent = label;
    el.append(input, txt);
    input.onchange = () => onChange(input.checked);
    return { el, input, set: (v) => { input.checked = v; }, get: () => input.checked };
  });

  B.segmented = B.segmented || ((options, selected, onChange, opts = {}) => {
    const el = doc.createElement('div');
    el.className = 'blockr-segmented';
    el.setAttribute('role', 'radiogroup');
    if (opts.label) el.setAttribute('aria-label', opts.label);
    let current = selected;
    for (const o of options) {
      const b = doc.createElement('button');
      b.type = 'button';
      b.className = 'blockr-segmented__seg' + (o.value === current ? ' is-selected' : '');
      b.setAttribute('role', 'radio');
      b.setAttribute('aria-checked', o.value === current ? 'true' : 'false');
      b.textContent = o.label;
      b.onclick = () => { if (o.value !== current) onChange(o.value); };
      el.appendChild(b);
    }
    return { el, set: (v) => { current = v; }, get: () => current };
  });

  B.gearTray = B.gearTray || ((band, btn, opts = {}) => {
    let open = false;
    const set = (on) => {
      open = on;
      band.classList.toggle('blockr-settings--open', on);
      btn.classList.toggle('blockr-gear-active', on);
      btn.setAttribute('aria-expanded', String(on));
    };
    if (opts.label) btn.setAttribute('aria-label', opts.label);
    btn.setAttribute('aria-expanded', 'false');
    btn.onclick = () => set(!open);
    return { set, toggle: () => set(!open), isOpen: () => open };
  });

  // Every group's sections, one after the other.
  B.trayNav = B.trayNav || ((groups, opts = {}) => {
    const el = doc.createElement('div');
    el.className = 'blockr-tray-nav';
    for (const g of groups) for (const s of g.sections) el.appendChild(s.el);
    let current = opts.current || (groups[0] && groups[0].key) || '';
    return { el, show: (k) => { current = k; }, current: () => current };
  });

  return B;
}

module.exports = { installUiFake };
