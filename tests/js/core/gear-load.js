/* Load the core scripts (gear and titles included) and the generated specs
 * into a fresh happy-dom window, optionally with blockr.ui first. Also
 * counts the listeners on document and window, for the leak tests. */
'use strict';

const fs = require('node:fs');
const path = require('node:path');
const { after } = require('node:test');
const { Window } = require('happy-dom');
const { CORE_FILES } = require('./load');

const JS = path.join(__dirname, '..', '..', '..', 'inst', 'js');
const UI_FILES = ['blockr-ui.js', 'blockr-select.js'];

let uiDir = process.env.BLOCKR_UI_JS;
/** blockr.ui's scripts, or null when neither BLOCKR_UI_JS nor R finds them. */
function findUi() {
  if (uiDir !== undefined) return uiDir;
  try {
    uiDir = require('node:child_process').execFileSync('Rscript', ['-e',
      'cat(system.file("assets", "js", package = "blockr.ui"))'], { encoding: 'utf8' }).trim() || null;
  } catch (e) {
    uiDir = null;
  }
  return uiDir;
}
const hasUi = () => {
  const d = findUi();
  return !!d && fs.existsSync(path.join(d, 'blockr-ui.js'));
};
/** blockr.ui main, the one with the grouped tray. */
const hasTrayNav = () => hasUi() &&
  fs.readFileSync(path.join(findUi(), 'blockr-ui.js'), 'utf8').includes('function trayNav');

const windows = [];
after(async () => {
  for (const w of windows) {
    w.happyDOM.abort();
    await w.happyDOM.close();
  }
});

/** Count listeners added and not removed on document and window. */
function countListeners(win) {
  const live = new Set();
  for (const target of [win, win.document]) {
    const add = target.addEventListener.bind(target);
    const rm = target.removeEventListener.bind(target);
    const id = (type, fn, opts) => [target === win ? 'w' : 'd', type,
      typeof opts === 'boolean' ? opts : !!(opts && opts.capture)].join(':');
    const keyOf = new Map();
    target.addEventListener = (type, fn, opts) => {
      const k = id(type, fn, opts);
      if (!keyOf.has(fn)) keyOf.set(fn, new Set());
      keyOf.get(fn).add(k);
      live.add(fn);
      return add(type, fn, opts);
    };
    target.removeEventListener = (type, fn, opts) => {
      const ks = keyOf.get(fn);
      if (ks) {
        ks.delete(id(type, fn, opts));
        if (!ks.size) { keyOf.delete(fn); live.delete(fn); }
      }
      return rm(type, fn, opts);
    };
  }
  return () => live.size;
}

/** @param {{ ui?: boolean }} [opts] */
function loadGear(opts = {}) {
  const win = new Window({ width: 1200, height: 900 });
  windows.push(win);
  const sent = [];
  // Values go through JSON, as they do on the wire.
  win.Shiny = { setInputValue: (name, value, o) =>
    sent.push({ name, value: JSON.parse(JSON.stringify(value)), opts: Object.assign({}, o) }) };
  if (opts.ui) {
    for (const f of UI_FILES) win.eval(fs.readFileSync(path.join(findUi(), f), 'utf8'));
    win.Blockr.icons = win.Blockr.icons || {};
  }
  const listeners = countListeners(win);
  for (const f of CORE_FILES) win.eval(fs.readFileSync(path.join(JS, 'core', f), 'utf8'));
  for (const f of ['chart.js', 'table.js']) {
    win.eval(fs.readFileSync(path.join(JS, 'spec', f), 'utf8'));
  }
  return { win, doc: win.document, V: win.Blockr.viz, sent, listeners };
}

const COLUMNS = [
  { name: 'USUBJID', class: 'character', label: 'Subject', n_unique: 40 },
  { name: 'ARM', class: 'character', label: 'Arm', n_unique: 3 },
  { name: 'SEX', class: 'factor', label: '', n_unique: 2 },
  { name: 'AGE', class: 'integer', label: 'Age', n_unique: 30 },
  { name: 'AVAL', class: 'double', label: 'Analysis Value', n_unique: 200 },
  { name: 'LO', class: 'double', label: '', n_unique: 100 },
  { name: 'ADT', class: 'date', label: 'Date', n_unique: 90 }
];

/** A payload for `config`, the way R sends one. */
function payload(config, extra = {}) {
  return Object.assign({ state_rev: 3, data_rev: 1, columns: COLUMNS, config,
                         derived: {} }, extra);
}

/** Mount a gear on a fresh tray element. */
function mountGear(env, opts = {}) {
  const el = env.doc.body.appendChild(env.doc.createElement('div'));
  el.id = 'tray';
  const scope = new env.V.Scope();
  const gear = scope.own(env.V.gear.mount(el, Object.assign(
    { block: 'chart', id: 'blk-chart', scope }, opts)));
  return { el, scope, gear };
}

/** The `values` of the last settings message. */
const lastSet = (env) => {
  const m = env.sent.filter((s) => s.name.endsWith('_set:blockr.viz.' + 'chart') ||
                                    s.name.endsWith('_set:blockr.viz.table')).pop();
  return m ? m.value : null;
};

/** Rows shown, in order. */
const rows = (el) => Array.from(el.querySelectorAll('.dd-form-row[data-role]'))
  .map((r) => r.getAttribute('data-role'));

const row = (el, key) => el.querySelector('.dd-form-row[data-role="' + key + '"]');

/** Pick the option whose text is `text` in a native select. */
function choose(win, sel, text) {
  const o = Array.from(sel.options).find((x) => x.textContent === text);
  if (!o) throw new Error('no option "' + text + '": ' + Array.from(sel.options).map((x) => x.textContent).join(', '));
  sel.value = o.value;
  sel.dispatchEvent(new win.Event('change'));
}

function type(win, inp, text) {
  inp.focus();
  inp.value = text;
  inp.dispatchEvent(new win.Event('input'));
  inp.dispatchEvent(new win.KeyboardEvent('keydown', { key: 'Enter' }));
}

module.exports = { loadGear, mountGear, payload, lastSet, rows, row, choose, type, hasUi,
                   hasTrayNav, COLUMNS };
