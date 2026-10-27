/* Load blockr.viz's core scripts, the table spec and the table scripts into
 * a fresh happy-dom window (tests/js/core/load.js), and build payloads and
 * table models for the tests. Each script is evaluated with a sourceURL, so
 * c8 maps its coverage to the file. */
'use strict';

const fs = require('node:fs');
const path = require('node:path');
const { loadCore } = require('../core/load');

const JS = path.join(__dirname, '..', '..', '..', 'inst', 'js');
const TABLE_DIR = path.join(JS, 'table');
const TABLE_FILES = fs.readFileSync(path.join(TABLE_DIR, 'scripts.txt'), 'utf8')
  .split('\n').map((l) => l.trim()).filter((l) => l && !l.startsWith('#'));

/** @param {string} file */
const source = (file) => fs.readFileSync(file, 'utf8') +
  '\n//# sourceURL=file://' + file;

/**
 * @param {{ before?: (win: any) => void }} [opts] `before` runs ahead of
 *   the table scripts (to count listeners, say).
 */
function loadTable(opts = {}) {
  const env = loadCore();
  if (opts.before) opts.before(env.win);
  env.win.eval(source(path.join(JS, 'spec', 'table.js')));
  for (const f of TABLE_FILES) env.win.eval(source(path.join(TABLE_DIR, f)));
  const b = env.outputs['blockr.viz.table'];
  const add = (id = 'tbl') => {
    const el = env.win.document.createElement('div');
    el.id = id;
    el.className = 'blockr-viz-output';
    el.setAttribute('data-viz-view', 'table');
    env.win.document.body.appendChild(el);
    return el;
  };
  const selects = () => env.sent.filter((s) => s.name.endsWith('_select:blockr.viz.selection'))
    // Through JSON: objects made in the window are not node's.
    .map((s) => JSON.parse(JSON.stringify(s.value.selection)));
  return Object.assign(env, { b, add, selects });
}

const CONFIG = {
  layout: 'records', search: true, sortable: true, collapsible: true,
  download: true, look: 'table', drill: null, drill_target: '', selection: null
};

let rev = 0;

/**
 * A payload as R sends it. `data` is the table model; leave it out to keep
 * the rows of the last data revision.
 */
function payload({ config = {}, derived = {}, data, dataRev, columns = [] } = {}) {
  rev += 1;
  return {
    state_rev: rev, data_rev: dataRev == null ? rev : dataRev, columns,
    config: Object.assign({}, CONFIG, config), derived, data: data === undefined ? null : data
  };
}

/** A flat table: a stub, a text and a number column. */
function flatModel(n = 5, opts = {}) {
  const arms = ['Placebo', 'Low', 'High'];
  const name = Array.from({ length: n }, (_, i) => 'S' + (i + 1));
  const arm = name.map((_, i) => (opts.na && i === 1 ? null : arms[i % 3]));
  const age = name.map((_, i) => 20 + ((i * 37) % 50));
  return {
    n, stub: 0,
    cols: [
      { name: 'USUBJID', disp: name, raw: name },
      { name: 'ARM', disp: arm, raw: arm },
      { name: 'AGE', num: true, disp: age.map(String), key: age }
    ],
    head: [[
      { text: 'USUBJID', stub: true, col: 0 },
      { text: 'Arm', col: 1 },
      { text: 'Age', sub: 'years', col: 2 }
    ]],
    drill: opts.drill === undefined ? 'ARM' : opts.drill
  };
}

const lvl = (v, l) => ({ '.variable': [v], '.variable_level': [l] });

/** A Table 1: two arms under a spanner, a section per variable. */
function table1() {
  const ck = (arm) => ({ ARM: [arm] });
  return {
    n: 5, stub: 0,
    cols: [
      { name: '.label', disp: ['F', 'M', 'Mean (SD)', '<65', '>=65'] },
      { name: 'Arms||Placebo', num: true, disp: ['10', '12', '70 (8)', '5', '17'], keys: ck('Placebo') },
      { name: 'Arms||Active', num: true, disp: ['11', '9', '72 (9)', '8', '12'], keys: ck('Active') }
    ],
    head: [
      [{ text: '', stub: true, rows: 2 }, { text: 'Arms', span: 2 }],
      [{ text: 'Placebo', sub: 'N = 22', col: 1, keys: ck('Placebo') },
       { text: 'Active', sub: 'N = 20', col: 2, keys: ck('Active') }]
    ],
    sections: [
      { at: 0, level: 1, value: 'Sex' },
      { at: 2, level: 1, value: 'Age' },
      { at: 3, level: 1, value: 'Age group' }
    ],
    rows: { keys: [lvl('SEX', 'F'), lvl('SEX', 'M'), null, lvl('AGEGR', '<65'), lvl('AGEGR', '>=65')] }
  };
}

/** Records nested under two levels of `by`, with keyed group headers. */
function nested() {
  const g = (a, s) => Object.assign({ ARM: [a] }, s ? { SEX: [s] } : {});
  return {
    n: 4, stub: 0,
    cols: [{ name: 'USUBJID', disp: ['a', 'b', 'c', 'd'] },
           { name: 'AGE', num: true, disp: ['30', '50', '40', '20'] }],
    head: [[{ text: 'USUBJID', stub: true, col: 0 }, { text: 'AGE', col: 1 }]],
    sections: [
      { at: 0, level: 1, value: 'Placebo', label: 'Arm', keys: g('Placebo') },
      { at: 0, level: 2, value: 'F', label: 'Sex', keys: g('Placebo', 'F') },
      { at: 2, level: 2, value: 'M', label: 'Sex', keys: g('Placebo', 'M') },
      { at: 3, level: 1, value: 'Active', label: 'Arm', keys: g('Active') },
      { at: 3, level: 2, value: 'F', label: 'Sex', keys: g('Active', 'F') }
    ]
  };
}

const tick = (win, ms = 0) => new Promise((r) => win.setTimeout(r, ms));

module.exports = { loadTable, payload, flatModel, table1, nested, tick, TABLE_FILES };
