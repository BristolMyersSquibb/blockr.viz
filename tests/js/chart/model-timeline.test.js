/* model-timeline.js: lanes, marks, facets and the click filter, on
 * small frames. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure, plain } = require('./load');

const C = loadPure();

const cat = (name, extra) => ({ name, class: 'string', ...extra });
const num = (name) => ({ name, class: 'number' });
const COLUMNS = [cat('T', { label: 'Term' }), cat('ID'), cat('ARM'), cat('SEV'),
                 num('S'), num('E'), num('D'), cat('V')];

const ROWS = [
  { T: 'Rash', ID: 'S01', ARM: 'B', SEV: 'MILD', S: 4, E: 9, D: 3, V: 'Week 2' },
  { T: 'Nausea', ID: 'S01', ARM: 'B', SEV: 'SEVERE', S: 2, E: null, D: 1, V: 'Week 1' },
  { T: 'Rash', ID: 'S02', ARM: 'A', SEV: null, S: 1, E: 3, D: 9, V: 'Week 1' },
  { T: null, ID: 'S02', ARM: 'A', SEV: 'MILD', S: 7, E: 8, D: 2, V: 'Week 3' },
  { T: 'Fever', ID: 'S03', ARM: 'A', SEV: 'MILD', S: null, E: 5, D: 4, V: 'Week 2' }
];

const BASE = { type: 'gantt', x: 'S', xend: 'E', y: 'T', drill: 'auto' };

// `derived` is R's derived object (the colour scales).
const settingsOf = (cfg, columns = COLUMNS) => C.model.settings({ ...BASE, ...cfg }, columns);

const build = (cfg, rows = ROWS, columns = COLUMNS, derived = {}) => C.model.timeline(
  { rows, ix: new C.RowIndex(), columns, cfg: settingsOf(cfg, columns), derived });

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
  assert.match(m.empty.message, /Select X \(start\) and Y \(term\) columns/);
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
  const s = build({ color: 'SEV' }, ROWS, columns,
                  { scales: { var: 'SEV', order: ['MILD', 'SEVERE'] } });
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
  const m = build({ facet: 'ARM', show_n: 'both' });
  assert.deepStrictEqual(plain(m.panels.map((p) => p.label)), ['A (3)', 'B (2)']);
  assert.deepStrictEqual(plain(m.panels[0].laneCounts), { Rash: 1, '': 1, Fever: 1 });
  const d = build({ show_n: 'axis', id: 'ID' });
  assert.deepStrictEqual(plain(d.panels[0].laneCounts), { Rash: 2, Nausea: 1, '': 1, Fever: 1 });
  assert.strictEqual(build({}).panels[0].laneCounts, null);
});

test('label, series, an override drill column and tooltip fields ride on the mark', () => {
  const m = build({ label: 'SEV', series: 'ID', drill: 'ID', tooltip: ['ARM', 'D'] });
  assert.deepStrictEqual(plain(m.panels[0].marks[0]),
                         [4, 9, 1, 'Rash', '', 'MILD', 'S01', 'S01', ['B', 3]]);
  // Drill off: nothing to filter on.
  assert.strictEqual(build({ drill: '' }).panels[0].marks[0][7], '');
});

test('a click filters the lane; in a facet panel the panel too (D3)', () => {
  const cfg = settingsOf({ facet: 'ARM' });
  const mark = [1, 3, 0, 'Rash', '', '', '', 'Rash', 0];
  assert.deepStrictEqual(plain(C.model.timelineClick(settingsOf({}), '__all__', mark)),
                         { T: ['Rash'] });
  assert.deepStrictEqual(plain(C.model.timelineClick(cfg, 'A', mark)),
                         { T: ['Rash'], ARM: ['A'] });
  // A panel of missing facet values filters the missing facet, as null.
  assert.deepStrictEqual(plain(C.model.timelineClick(cfg, '', mark)), { T: ['Rash'], ARM: [null] });
  // An override column: the clicked event's own value, already in the panel.
  const byId = settingsOf({ facet: 'ARM', drill: 'ID' });
  assert.deepStrictEqual(plain(C.model.timelineClick(byId, 'A', [1, 3, 0, 'Rash', '', '', '', 'S02', 0])),
                         { ID: ['S02'] });
  // A missing lane is a key too.
  assert.deepStrictEqual(plain(C.model.timelineClick(settingsOf({}), '__all__',
    [7, 8, 2, '', '', '', '', '', 0])), { T: [null] });
  // Nothing to send: drill off, or an override column the event has no value of.
  assert.strictEqual(C.model.timelineClick(settingsOf({ drill: '' }), '__all__', mark), null);
  assert.strictEqual(C.model.timelineClick(byId, 'A', [7, 8, 2, 'Rash', '', '', '', '', 0]), null);
});

test('the keys of a mark are its lane and facet, whatever the drill column', () => {
  const mark = [1, 3, 0, 'Rash', 'MILD', '', '', 'S02', 0];
  assert.deepStrictEqual(plain(C.model.timelineKeys(settingsOf({ drill: 'ID', color: 'SEV',
                                                                    facet: 'ARM' }), 'A', mark)),
                         { T: 'Rash', ARM: 'A' });
});

test('a Date x is a time axis: starts and ends in ms, sorted by date', () => {
  const columns = [cat('T'), { name: 'S', class: 'date' }, { name: 'E', class: 'date' },
                   { name: 'L', class: 'date' }];
  const rows = [
    { T: 'Rash', S: '2024-01-05', E: '2024-01-10', L: '2024-02-01' },
    { T: 'Nausea', S: '2024-01-02', E: null, L: '2024-01-01' },
    { T: 'Fever', S: 'not a date', E: '2024-01-03', L: null }
  ];
  const day = (d) => Date.UTC(2024, 0, d);
  const m = build({}, rows, columns);
  assert.strictEqual(m.x.type, 'time');
  assert.strictEqual(m.x.cats, null);
  assert.deepStrictEqual(plain(m.panels[0].lanes), ['Nausea', 'Rash']);
  // Fever's start does not parse: no lane, no mark.
  assert.deepStrictEqual(plain(m.panels[0].marks.map((t) => [t[0], t[1], t[3]])),
                         [[day(5), day(10), 'Rash'], [day(2), day(2), 'Nausea']]);
  // Sort by another date column: Nausea's L comes first.
  assert.deepStrictEqual(plain(build({ sort_by: 'L', sort_dir: 'desc' }, rows, columns)
    .panels[0].lanes), ['Rash', 'Nausea']);
});

test('a date-time x arrives in ms and is a time axis sampled at times', () => {
  const columns = [cat('T'), { name: 'S', class: 'datetime' }, { name: 'E', class: 'datetime' }];
  const t0 = Date.UTC(2024, 0, 1, 8);
  const rows = [{ T: 'A', S: t0, E: t0 + 3600e3 }, { T: 'B', S: t0 + 1800e3, E: t0 + 7200e3 }];
  const m = build({}, rows, columns);
  assert.strictEqual(m.x.type, 'time');
  assert.strictEqual(m.x.period, 'time');
  assert.deepStrictEqual(plain(m.panels[0].marks.map((t) => [t[0], t[1]])),
                         [[t0, t0 + 3600e3], [t0 + 1800e3, t0 + 7200e3]]);
});

test('open: a TRUE row gets the flag, and without an end runs to the last x', () => {
  const rows = [
    { T: 'Rash', S: 4, E: 9, O: false },
    { T: 'Nausea', S: 2, E: null, O: true },
    { T: 'Cough', S: 6, E: 7, O: 'TRUE' },
    { T: 'Fever', S: 3, E: null, O: null }
  ];
  const cols = [cat('T'), num('S'), num('E'), { name: 'O', class: 'logical' }];
  const marks = plain(build({ open: 'O' }, rows, cols).panels[0].marks);
  const byTerm = Object.fromEntries(marks.map((mk) => [mk[3], mk]));
  assert.deepStrictEqual([byTerm.Rash[1], byTerm.Rash[9]], [9, 0]);
  assert.deepStrictEqual([byTerm.Nausea[1], byTerm.Nausea[9]], [9, 1]);
  assert.deepStrictEqual([byTerm.Cough[1], byTerm.Cough[9]], [7, 1]);
  assert.deepStrictEqual([byTerm.Fever[1], byTerm.Fever[9]], [3, 0]);
  // Without the column a mark keeps its nine fields.
  assert.strictEqual(plain(build({}, rows, cols).panels[0].marks)[0].length, 9);
});
