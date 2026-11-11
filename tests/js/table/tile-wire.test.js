'use strict';
/* A tile column as R sends it (R/table-groups-view.R, table_groups_tile_wire()):
 * the filled rows only, the text colour once per fill. The groups view puts
 * both back before it draws. */

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadTable, payload } = require('./load');

const G = { layout: 'groups', by: ['ID'], look: 'compact' };

/** Ten subjects, one tile column. `tile` is the column as it travels. */
function matrix(tile) {
  const ids = Array.from({ length: 10 }, (_, i) => 'S' + i);
  return {
    n: 10, stub: 0,
    cols: [{ name: 'ID', disp: ids },
           Object.assign({ name: 'RASH', mark: { kind: 'tile' } }, tile)],
    head: [[{ text: 'ID', stub: true, col: 0 }, { text: 'RASH', col: 1 }]],
    rows: { keys: ids.map((s) => ({ ID: [s] })) }
  };
}

const DENSE = {
  disp: [null, null, '3', null, null, null, null, '1', null, null],
  key: [null, null, 3, null, null, null, null, 1, null, null],
  bg: [null, null, '#c00', null, null, null, null, '#fee', null, null],
  fg: [null, null, '#fff', null, null, null, null, '#000', null, null]
};
const WIRE = {
  at: [2, 7], disp: ['3', '1'], key: [3, 1], bg: ['#c00', '#fee'],
  ink: { '#c00': '#fff', '#fee': '#000' }
};

function draw(tile) {
  const env = loadTable();
  const el = env.add();
  const p = payload({ data: matrix(tile), config: G });
  env.b.renderValue(el, p);
  return { env, el, p, html: el.querySelector('tbody').innerHTML };
}

test('a sparse tile column draws as the dense one', () => {
  const a = draw(Object.assign({}, DENSE));
  const b = draw(JSON.parse(JSON.stringify(WIRE)));
  assert.equal(b.html, a.html);
  assert.match(b.html, /background:#c00;color:#fff/);
  // The model the view holds has one entry per row again.
  const c = b.p.data.cols[1];
  assert.deepEqual(Array.from(c.disp), DENSE.disp);
  assert.deepEqual(Array.from(c.key), DENSE.key);
  assert.deepEqual(Array.from(c.bg), DENSE.bg);
  assert.deepEqual(Array.from(c.fg), DENSE.fg);
});

test('the same model drawn again stays as it is', () => {
  const b = draw(JSON.parse(JSON.stringify(WIRE)));
  b.env.b.renderValue(b.el, payload({ config: Object.assign({}, G, { sortable: false }),
                                      dataRev: b.p.data_rev }));
  assert.deepEqual(Array.from(b.p.data.cols[1].disp), DENSE.disp);
  assert.equal(b.el.querySelector('tbody').innerHTML, b.html);
});

test('a dense column with a colour map gets its text colours', () => {
  const b = draw({ disp: DENSE.disp.slice(), key: DENSE.key.slice(), bg: DENSE.bg.slice(),
                   ink: WIRE.ink });
  assert.deepEqual(Array.from(b.p.data.cols[1].fg), DENSE.fg);
});
