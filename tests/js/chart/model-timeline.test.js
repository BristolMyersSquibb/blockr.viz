/* model-timeline.js: lanes, marks, facets and the click filter, on
 * small frames. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure, plain } = require('./load');

const C = loadPure();

const cat = (name, extra) => ({ name, type: 'categorical', ...extra });
const num = (name) => ({ name, type: 'numeric' });
const COLUMNS = [cat('T', { label: 'Term' }), cat('ID'), cat('ARM'), cat('SEV'),
                 num('S'), num('E'), num('D'), cat('V')];

const ROWS = [
  { T: 'Rash', ID: 'S01', ARM: 'B', SEV: 'MILD', S: 4, E: 9, D: 3, V: 'Week 2' },
  { T: 'Nausea', ID: 'S01', ARM: 'B', SEV: 'SEVERE', S: 2, E: null, D: 1, V: 'Week 1' },
  { T: 'Rash', ID: 'S02', ARM: 'A', SEV: null, S: 1, E: 3, D: 9, V: 'Week 1' },
  { T: null, ID: 'S02', ARM: 'A', SEV: 'MILD', S: 7, E: 8, D: 2, V: 'Week 3' },
  { T: 'Fever', ID: 'S03', ARM: 'A', SEV: 'MILD', S: null, E: 5, D: 4, V: 'Week 2' }
];

const BASE = { chart_type: 'gantt', x: 'S', xend: 'E', y: 'T', drill: 'auto' };

const build = (cfg, rows = ROWS, columns = COLUMNS) => C.model.timeline(
  { rows, ix: new C.RowIndex(), columns, cfg: { ...BASE, ...cfg } });

test('one mark per row with an x; a missing end makes a dot', () => {
  const m = build({});
  assert.strictEqual(m.empty, null);
  assert.strictEqual(m.panels.length, 1);
  const p = m.panels[0];
  // By onset. Fever's x is missing, which sorts as 0 (Number(null)): its
  // lane is first and empty.
  assert.deepStrictEqual(plain(p.lanes), ['Fever', 'Rash', 'Nausea', '']);
  assert.deepStrictEqual(plain(p.marks), [
    [4, 9, 1, 'Rash', '', '', '', 'Rash', 0],
    [2, 2, 2, 'Nausea', '', '', '', 'Nausea', 0],
    [1, 3, 1, 'Rash', '', '', '', 'Rash', 0],
    [7, 8, 3, '', '', '', '', '', 0]
  ]);
  assert.strictEqual(p.label, null);
  assert.strictEqual(m.single, true);
});

test('no x or y: an empty state', () => {
  const m = build({ x: '' });
  assert.match(m.empty, /Select X \(start\) and Y \(term\) columns/);
  assert.strictEqual(m.panels.length, 0);
});

test('sort: onset descending, A-Z both ways, and by another column', () => {
  assert.deepStrictEqual(plain(build({ sort_dir: 'desc' }).panels[0].lanes),
                         ['', 'Nausea', 'Rash', 'Fever']);
  assert.deepStrictEqual(plain(build({ sort_by: 'alpha' }).panels[0].lanes),
                         ['', 'Fever', 'Nausea', 'Rash']);
  assert.deepStrictEqual(plain(build({ sort_by: 'alpha', sort_dir: 'desc' }).panels[0].lanes),
                         ['Rash', 'Nausea', 'Fever', '']);
  // By D: Nausea 1, '' 2, Rash 3, Fever 4 (Fever's lane exists, its x does not).
  const byD = build({ sort_by: 'D' }).panels[0];
  assert.deepStrictEqual(plain(byD.lanes), ['Nausea', '', 'Rash', 'Fever']);
  assert.strictEqual(byD.marks.length, 4);
});

test('a categorical x draws at category positions, in factor order', () => {
  const columns = COLUMNS.map((c) => (c.name === 'V'
    ? cat('V', { levels: ['Week 3', 'Week 2', 'Week 1'] }) : c));
  const m = build({ x: 'V', xend: null }, ROWS, columns);
  assert.strictEqual(m.x.type, 'category');
  assert.deepStrictEqual(plain(m.x.cats), ['Week 3', 'Week 2', 'Week 1']);
  // Onset sorts on the position: '' (0), Rash/Fever (1), Nausea (2).
  assert.deepStrictEqual(plain(m.panels[0].lanes), ['', 'Rash', 'Fever', 'Nausea']);
  assert.deepStrictEqual(plain(m.panels[0].marks.map((t) => [t[0], t[1], t[3]])),
                         [[1, 1, 'Rash'], [2, 2, 'Nausea'], [2, 2, 'Rash'], [0, 0, ''],
                          [1, 1, 'Fever']]);
});

test('colour levels follow the factor levels; the board scale order wins', () => {
  const columns = COLUMNS.map((c) => (c.name === 'SEV'
    ? cat('SEV', { levels: ['SEVERE', 'MILD'] }) : c));
  const m = build({ color: 'SEV' }, ROWS, columns);
  // Unknown levels ('', the missing severity) after the known ones.
  assert.deepStrictEqual(plain(m.color.levels), ['SEVERE', 'MILD', '']);
  const s = build({ color: 'SEV', scales: { var: 'SEV', order: ['MILD', 'SEVERE'] } },
                  ROWS, columns);
  assert.deepStrictEqual(plain(s.color.levels), ['MILD', 'SEVERE', '']);
  // Without levels: alphabetical.
  assert.deepStrictEqual(plain(build({ color: 'SEV' }).color.levels), ['', 'MILD', 'SEVERE']);
  assert.strictEqual(build({}).color, null);
});

test('facets: sorted panels, shared lanes when fixed, own lanes when free', () => {
  const fixed = build({ facet: 'ARM' });
  assert.strictEqual(fixed.nFacets, 2);
  assert.strictEqual(fixed.single, false);
  assert.deepStrictEqual(plain(fixed.panels.map((p) => p.facet)), ['A', 'B']);
  assert.deepStrictEqual(plain(fixed.panels.map((p) => p.lanes)),
                         [['Fever', 'Rash', 'Nausea', ''], ['Fever', 'Rash', 'Nausea', '']]);
  assert.deepStrictEqual(plain(fixed.panels.map((p) => p.label)), ['A', 'B']);
  const free = build({ facet: 'ARM', facet_scales: 'free' });
  assert.deepStrictEqual(plain(free.panels.map((p) => p.lanes)),
                         [['Fever', 'Rash', ''], ['Nausea', 'Rash']]);
});

test('counts: events or distinct ids per lane, rows per facet', () => {
  const m = build({ facet: 'ARM', count_on: 'both' });
  assert.deepStrictEqual(plain(m.panels.map((p) => p.label)), ['A (3)', 'B (2)']);
  assert.deepStrictEqual(plain(m.panels[0].laneCounts), { Rash: 1, '': 1, Fever: 1 });
  const d = build({ count_on: 'axis', count_col: 'ID' });
  assert.deepStrictEqual(plain(d.panels[0].laneCounts), { Rash: 2, Nausea: 1, '': 1, Fever: 1 });
  assert.strictEqual(build({}).panels[0].laneCounts, null);
});

test('label, series, an override drill column and tooltip fields ride on the mark', () => {
  const m = build({ label: 'SEV', series: 'ID', drill: 'ID', tt_fields: ['ARM', 'D'] });
  assert.deepStrictEqual(plain(m.panels[0].marks[0]),
                         [4, 9, 1, 'Rash', '', 'MILD', 'S01', 'S01', ['B', 3]]);
  // Drill off: nothing to filter on.
  assert.strictEqual(build({ drill: '' }).panels[0].marks[0][7], '');
});

test('a click filters the lane; in a facet panel the panel too (D3)', () => {
  const cfg = { ...BASE, facet: 'ARM' };
  const mark = [1, 3, 0, 'Rash', '', '', '', 'Rash', 0];
  assert.deepStrictEqual(plain(C.model.timelineClick({ ...BASE }, '__all__', mark)),
                         { T: ['Rash'] });
  assert.deepStrictEqual(plain(C.model.timelineClick(cfg, 'A', mark)),
                         { T: ['Rash'], ARM: ['A'] });
  // A panel of missing facet values filters the missing facet, as null.
  assert.deepStrictEqual(plain(C.model.timelineClick(cfg, '', mark)), { T: ['Rash'], ARM: [null] });
  // An override column: the clicked event's own value, already in the panel.
  const byId = { ...cfg, drill: 'ID' };
  assert.deepStrictEqual(plain(C.model.timelineClick(byId, 'A', [1, 3, 0, 'Rash', '', '', '', 'S02', 0])),
                         { ID: ['S02'] });
  // A missing lane is a key too.
  assert.deepStrictEqual(plain(C.model.timelineClick({ ...BASE }, '__all__',
    [7, 8, 2, '', '', '', '', '', 0])), { T: [null] });
  // Nothing to send: drill off, or an override column the event has no value of.
  assert.strictEqual(C.model.timelineClick({ ...BASE, drill: '' }, '__all__', mark), null);
  assert.strictEqual(C.model.timelineClick(byId, 'A', [7, 8, 2, 'Rash', '', '', '', '', 0]), null);
});

test('the keys of a mark are its lane and facet, whatever the drill column', () => {
  const mark = [1, 3, 0, 'Rash', 'MILD', '', '', 'S02', 0];
  assert.deepStrictEqual(plain(C.model.timelineKeys({ ...BASE, drill: 'ID', color: 'SEV',
                                                      facet: 'ARM' }, 'A', mark)),
                         { T: 'Rash', ARM: 'A' });
});
