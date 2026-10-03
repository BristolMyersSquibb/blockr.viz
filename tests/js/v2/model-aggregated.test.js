/* v2 model-aggregated.js: panels, group order, colour levels, counts,
 * distribution slots and the click filter, on small frames. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure, plain } = require('./load');

const C = loadPure({ roles: true });

const cat = (name, extra) => ({ name, type: 'categorical', ...extra });
const num = (name) => ({ name, type: 'numeric' });
const COLUMNS = [cat('T', { label: 'Term' }), cat('ID'), cat('ARM'), cat('SEV'), num('V'),
                 num('D'), cat('SEX', { levels: ['M', 'F', 'U'] })];

const ROWS = [
  { T: 'Rash', ID: 'S01', ARM: 'B', SEV: 'MILD', V: 4, D: 3, SEX: 'F' },
  { T: 'Nausea', ID: 'S01', ARM: 'B', SEV: 'SEVERE', V: 2, D: 1, SEX: 'F' },
  { T: 'Rash', ID: 'S02', ARM: 'A', SEV: null, V: 1, D: 9, SEX: 'M' },
  { T: null, ID: 'S02', ARM: 'A', SEV: 'MILD', V: 7, D: 2, SEX: 'M' },
  { T: 'Fever', ID: 'S03', ARM: 'A', SEV: 'MILD', V: null, D: 4, SEX: 'M' },
  { T: 'Rash', ID: 'S03', ARM: 'A', SEV: 'SEVERE', V: 6, D: 5, SEX: 'M' }
];

const BASE = { chart_type: 'bar', group: 'T', value: '.count', func: 'count', drill: 'auto',
               sort_by: 'value', sort_dir: 'desc' };

const build = (cfg, rows = ROWS, columns = COLUMNS) => C.model.aggregated(
  { rows, ix: new C.RowIndex(), columns, cfg: { ...BASE, ...cfg } });

const groupsOf = (m) => plain(m.panels.map((p) => p.groups));

test('a missing group is its own level; na_group drop removes it', () => {
  const m = build({});
  assert.strictEqual(m.empty, null);
  assert.strictEqual(m.single, true);
  // Rash 3, then the ties in first-seen order of the cells.
  assert.deepStrictEqual(groupsOf(m), [['Rash', 'Nausea', '', 'Fever']]);
  assert.deepStrictEqual(groupsOf(build({ na_group: 'drop' })), [['Rash', 'Nausea', 'Fever']]);
  // Drop also removes a row whose colour is missing.
  const c = build({ color: 'SEV', na_group: 'drop' });
  assert.deepStrictEqual(plain(c.colors), ['MILD', 'SEVERE']);
  assert.strictEqual(C.model.cellOf(c, c.panels[0], 'Rash', 'MILD').value, 1);
});

test('no cells: the empty state', () => {
  const m = build({ na_group: 'drop' }, [{ T: null, ID: 'S01' }]);
  assert.match(m.empty, /No data to chart/);
});

test('group order: value, alpha, factor levels, data, a column', () => {
  assert.deepStrictEqual(groupsOf(build({ sort_dir: 'asc' })), [['Nausea', '', 'Fever', 'Rash']]);
  assert.deepStrictEqual(groupsOf(build({ sort_by: 'alpha', sort_dir: 'asc' })),
                         [['', 'Fever', 'Nausea', 'Rash']]);
  // Factor levels win over the alphabet, and unused levels drop out.
  assert.deepStrictEqual(groupsOf(build({ group: 'SEX', sort_by: 'alpha', sort_dir: 'asc' })),
                         [['M', 'F']]);
  assert.deepStrictEqual(groupsOf(build({ sort_by: 'data', sort_dir: 'asc' })),
                         [['Rash', 'Nausea', '', 'Fever']]);
  assert.deepStrictEqual(groupsOf(build({ sort_by: 'data', sort_dir: 'desc' })),
                         [['Fever', '', 'Nausea', 'Rash']]);
  // The smallest D per group, ascending.
  assert.deepStrictEqual(groupsOf(build({ sort_by: 'D', sort_dir: 'asc' })),
                         [['Nausea', '', 'Rash', 'Fever']]);
  // A bridge keeps the data order whatever the sort says.
  assert.deepStrictEqual(groupsOf(build({ chart_type: 'waterfall', sort_by: 'value' })),
                         [['Rash', 'Nausea', '', 'Fever']]);
});

test('colour levels follow the scale order, then factor levels, then the alphabet', () => {
  assert.deepStrictEqual(plain(build({ color: 'SEV' }).colors), ['', 'MILD', 'SEVERE']);
  const scaled = build({ color: 'SEV', scales: { var: 'SEV', order: ['SEVERE', 'MILD'] } });
  assert.deepStrictEqual(plain(scaled.colors), ['SEVERE', 'MILD', '']);
  assert.deepStrictEqual(plain(build({ group: 'T', color: 'SEX' }).colors), ['M', 'F']);
});

test('facets: fixed scales share one group order, free scales keep their own', () => {
  const fixed = build({ facet: 'ARM' });
  assert.strictEqual(fixed.single, false);
  assert.deepStrictEqual(plain(fixed.panels.map((p) => [p.facet, p.label])),
                         [['A', 'A'], ['B', 'B']]);
  assert.deepStrictEqual(groupsOf(fixed), [['Rash', 'Nausea', '', 'Fever'],
                                           ['Rash', 'Nausea', '', 'Fever']]);
  const free = build({ facet: 'ARM', facet_scales: 'free' });
  assert.deepStrictEqual(groupsOf(free), [['Rash', '', 'Fever'], ['Rash', 'Nausea']]);
  // Facet counts on the strip.
  const counted = build({ facet: 'ARM', count_on: 'facet', count_col: 'ID' });
  assert.deepStrictEqual(plain(counted.panels.map((p) => p.label)), ['A (2)', 'B (1)']);
});

test('count_on axis: rows, or distinct count_col, per panel', () => {
  const rows = build({ count_on: 'axis' });
  assert.deepStrictEqual(plain(rows.panels[0].axisCounts), { Rash: 3, Nausea: 1, '': 1, Fever: 1 });
  const ids = build({ facet: 'ARM', count_on: 'axis', count_col: 'ID' });
  assert.deepStrictEqual(plain(ids.panels.map((p) => p.axisCounts)),
                         [{ Rash: 2, '': 1, Fever: 1 }, { Rash: 1, Nausea: 1 }]);
  assert.strictEqual(build({}).panels[0].axisCounts, null);
});

test('tooltip fields: the first row of each group in the panel', () => {
  const m = build({ facet: 'ARM', tt_fields: ['ID', 'SEV'] });
  assert.deepStrictEqual(plain(m.ttFields), ['ID', 'SEV']);
  assert.deepStrictEqual(plain(m.panels[0].ttRep.Rash), ['S02', '']);
  assert.deepStrictEqual(plain(m.panels[1].ttRep.Rash), ['S01', 'MILD']);
});

test('a radar shares its spoke max across a fixed grid, not a free one', () => {
  const cfg = { chart_type: 'radar', color: 'SEV', facet: 'ARM', value: 'V', func: 'sum' };
  // The largest cell of any panel: '' / MILD in A.
  assert.strictEqual(build(cfg).sharedMax, 7);
  assert.strictEqual(build({ ...cfg, facet_scales: 'free' }).sharedMax, null);
});

test('summarizeStat: quartiles, Tukey fences clipped to the data, sd and se', () => {
  const v = [1, 2, 3, 4, 100];
  assert.deepStrictEqual(plain(C.model.summarizeStat(v, 'median_q1_q3')), { center: 3, lo: 2, hi: 4 });
  assert.deepStrictEqual(plain(C.model.summarizeStat(v, 'tukey')), { center: 3, lo: 1, hi: 7 });
  assert.deepStrictEqual(plain(C.model.summarizeStat(v, 'min_max')), { center: 3, lo: 1, hi: 100 });
  const sd = C.model.summarizeStat([2, 4], 'mean_sd');
  assert.strictEqual(sd.center, 3);
  assert.ok(Math.abs(sd.hi - (3 + Math.SQRT2)) < 1e-12);
  assert.deepStrictEqual(plain(C.model.summarizeStat([5], 'mean_se')), { center: 5, lo: 5, hi: 5 });
  assert.strictEqual(C.model.summarizeStat([], 'tukey'), null);
});

test('boxplot slots: one per group, the five numbers, the outliers past the whiskers', () => {
  const rows = [1, 2, 3, 4, 100].map((V) => ({ G: 'a', V, C: 'x' }))
    .concat([{ G: 'b', V: null, C: 'y' }, { G: 'b', V: 5, C: 'y' }]);
  const cols = [cat('G'), num('V'), cat('C')];
  const cfg = { chart_type: 'boxplot', group: 'G', value: 'V', func: 'mean',
                sort_by: 'data', sort_dir: 'asc', box_points: 'outliers' };
  const m = build(cfg, rows, cols);
  const s = m.panels[0].slots;
  assert.deepStrictEqual(plain(s.map((x) => [x.cat, x.n, x.body, x.whisker, x.outliers])), [
    ['a', 5, { center: 3, lo: 2, hi: 4 }, { center: 3, lo: 1, hi: 7 }, [100]],
    ['b', 1, { center: 5, lo: 5, hi: 5 }, { center: 5, lo: 5, hi: 5 }, []]
  ]);
  // A colour split: a slot per (group, level) with rows, group-major.
  const split = build({ ...cfg, color: 'C' }, rows, cols);
  assert.strictEqual(split.split, true);
  assert.deepStrictEqual(plain(split.panels[0].slots.map((x) => [x.group, x.level])),
                         [['a', 'x'], ['b', 'y']]);
  assert.strictEqual(split.panels[0].slots[0].cat, 'a\u0000x');
  // No numeric value: nothing to summarise.
  const none = build({ chart_type: 'boxplot', group: 'G', value: '' }, rows, cols);
  assert.strictEqual(none.noValue, true);
  assert.strictEqual(none.panels[0].slots, null);
});

test('a fixed facet grid keeps every distribution slot in every panel', () => {
  const rows = [{ G: 'a', C: 'x', F: 'p', V: 1 }, { G: 'b', C: 'y', F: 'q', V: 2 }];
  const cols = [cat('G'), cat('C'), cat('F'), num('V')];
  const cfg = { chart_type: 'pointrange', group: 'G', color: 'C', facet: 'F', value: 'V',
                func: 'mean', sort_by: 'data', sort_dir: 'asc' };
  const slots = (m) => plain(m.panels.map((p) => p.slots.map((s) => [s.cat, s.n])));
  assert.deepStrictEqual(slots(build(cfg, rows, cols)), [
    [['a\u0000x', 1], ['a\u0000y', 0], ['b\u0000x', 0], ['b\u0000y', 0]],
    [['a\u0000x', 0], ['a\u0000y', 0], ['b\u0000x', 0], ['b\u0000y', 1]]]);
  assert.deepStrictEqual(slots(build({ ...cfg, facet_scales: 'free' }, rows, cols)),
                         [[['a\u0000x', 1]], [['b\u0000y', 1]]]);
});

test('the mark a click lands on: a bar segment, a split box, a radar shape', () => {
  const bar = build({ color: 'SEV' });
  const at = (m, cfg, p) => plain(C.model.aggregatedMarkAt(m, { ...BASE, ...cfg }, p));
  assert.deepStrictEqual(at(bar, { color: 'SEV' },
                            { componentType: 'series', name: 'Rash', seriesName: 'MILD' }),
                         { group: 'Rash', level: 'MILD' });
  assert.strictEqual(at(bar, { color: 'SEV' }, { componentType: 'series', name: '' }), null);
  assert.strictEqual(at(bar, { color: 'SEV' }, { componentType: 'markLine', name: 'Rash' }), null);
  const wf = build({ chart_type: 'waterfall' });
  assert.deepStrictEqual(at(wf, { chart_type: 'waterfall' }, { name: 'Rash', seriesName: 'delta' }),
                         { group: 'Rash', level: null });
  const box = build({ chart_type: 'boxplot', value: 'V', func: 'mean', color: 'SEV' });
  assert.deepStrictEqual(at(box, { chart_type: 'boxplot', color: 'SEV' }, { name: 'Rash\u0000MILD' }),
                         { group: 'Rash', level: 'MILD' });
  const radar = build({ chart_type: 'radar', color: 'SEV' });
  assert.deepStrictEqual(at(radar, { chart_type: 'radar', color: 'SEV' }, { name: 'MILD' }),
                         { group: null, level: 'MILD' });
  assert.strictEqual(at(build({ chart_type: 'radar' }), { chart_type: 'radar' }, { name: 'Count' }),
                     null);
});

test('keys: group, colour where it splits the mark, facet in a panel (D2, D3)', () => {
  const keys = (cfg, fv, g, lv) => plain(C.model.aggregatedKeys({ ...BASE, ...cfg }, fv, g, lv));
  assert.deepStrictEqual(keys({}, '__all__', 'Rash', null), { T: 'Rash' });
  assert.deepStrictEqual(keys({ color: 'SEV', facet: 'ARM' }, 'A', 'Rash', 'MILD'),
                         { T: 'Rash', SEV: 'MILD', ARM: 'A' });
  assert.deepStrictEqual(keys({ chart_type: 'radar', color: 'SEV' }, '__all__', null, 'MILD'),
                         { SEV: 'MILD' });
});

test('a click sends the keys, without a missing value; a drill column its values', () => {
  const ix = new C.RowIndex();
  const send = (cfg, keys) => plain(C.model.aggregatedClick({ ...BASE, ...cfg }, keys,
    () => C.model.rowsUnder(ROWS, ix, keys)));
  assert.deepStrictEqual(send({ color: 'SEV' }, { T: 'Rash', SEV: 'MILD' }),
                         { T: ['Rash'], SEV: ['MILD'] });
  // A missing colour is left out; a missing group sends nothing.
  assert.deepStrictEqual(send({ color: 'SEV' }, { T: 'Rash', SEV: '' }), { T: ['Rash'] });
  assert.strictEqual(send({}, { T: '' }), null);
  // An explicit drill column: its values in the rows under the mark only.
  assert.deepStrictEqual(send({ color: 'SEV', drill: 'ID' }, { T: 'Rash', SEV: 'SEVERE' }),
                         { ID: ['S03'] });
  assert.deepStrictEqual(send({ drill: 'ID', facet: 'ARM' }, { T: 'Rash', ARM: 'A' }),
                         { ID: ['S02', 'S03'] });
  assert.strictEqual(send({ drill: '' }, { T: 'Rash' }), null);
});

test('the model reads only the config fields it lists', () => {
  const configs = [
    {}, { color: 'SEV', facet: 'ARM', count_on: 'both', count_col: 'ID', tt_fields: ['ID'] },
    { chart_type: 'waterfall', value: 'V', func: 'sum' }, { chart_type: 'pie', facet: 'ARM' },
    { chart_type: 'radar', color: 'SEV', facet: 'ARM' },
    { chart_type: 'boxplot', value: 'V', func: 'mean', color: 'SEV', box_points: 'outliers' },
    { chart_type: 'pointrange', value: 'V', func: 'mean', summary: 'mean_sd', facet: 'ARM' },
    { sort_by: 'D' }, { sort_by: 'data' }, { func: 'pct_distinct', value: 'ID', pct_of: 'color' }
  ];
  for (const cfg of configs) {
    const read = new Set();
    const spy = new Proxy({ ...BASE, ...cfg }, { get(t, k) { read.add(k); return t[k]; } });
    C.model.aggregated({ rows: ROWS, ix: new C.RowIndex(), columns: COLUMNS, cfg: spy });
    const extra = [...read].filter((k) => typeof k === 'string' &&
                                          !C.model.aggregated.FIELDS.includes(k));
    assert.deepStrictEqual(extra, [], `${JSON.stringify(cfg)} reads ${extra}`);
  }
});
