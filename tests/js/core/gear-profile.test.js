/* A block's own gear (R's table_gear(), `derived.gear`, core/gear/profile.js): its
 * tabs and sections, a prepare script's values as rows, the column checklist, the
 * "In the subtitle" ticks and a package's own row. Settings it does not name have
 * no row and still work. */
'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadGear, mountGear, payload, lastSet, rows, row, hasUi } = require('./gear-load');

const noUi = { skip: !hasUi() && 'blockr.ui not found (set BLOCKR_UI_JS)' };
const plain = (x) => JSON.parse(JSON.stringify(x));

const SCRIPT = {
  inputs: [
    { name: 'doses', key: 'sv_doses', label: 'Doses', kind: 'select',
      options: ['Exceptions', 'Dots', 'Plain'] },
    { name: 'con_meds', key: 'sv_con_meds', label: 'Con meds', kind: 'segmented' },
    { name: 'unit', key: 'sv_unit', label: 'Time unit', kind: 'select', options: ['days', 'weeks'] },
    { name: 'drugs', key: 'sv_drugs', label: 'Drugs', kind: 'multi', options: ['A', 'B'] }
  ],
  values: { sv_doses: ['Exceptions'], sv_con_meds: 'off', sv_unit: ['weeks'], sv_drugs: ['A', 'B'] },
  error: null
};

const WEEKS = { type: 'simple', name: 'Weeks', func: 'max', col: 'weeks' };
const SEX = { type: 'field', name: 'Sex', col: 'SEX' };
const SPANS = { type: 'spans', name: 'Study week', x: 'start', xend: 'end' };

const GEAR = {
  tabs: [
    { title: 'Data', sections: [{ title: 'Time', keys: ['unit'] }] },
    { title: 'Patients', sections: [{ title: 'Rows', keys: ['group', 'sort_by', 'sort_dir'] },
                                    { title: 'Columns', keys: ['columns'] }] },
    { title: 'Lanes', sections: [{ title: 'Drug lines', keys: ['drugs', 'doses', 'con_meds'] }] },
    { title: 'Titles', sections: [{ title: 'Titles', keys: ['title', 'subtitle', 'words'] }] },
    { title: 'Interaction', sections: [{ title: 'Reader tools', keys: ['search', 'nope'] }] }
  ],
  groups: { values: ['ARM', 'BOR'], labels: ['Arm', 'Best response'] },
  columns: [{ name: 'Weeks', label: 'Weeks', summary: WEEKS },
            { name: 'Sex', label: 'Sex', summary: SEX }],
  clauses: { doses: 'doses shown as {@doses}', con_meds: 'con meds {@con_meds}',
             unit: 'by study {@unit}' }
};

const CONFIG = { layout: 'groups', by: ['ARM', 'USUBJID'], summaries: [WEEKS, SPANS],
                 subtitle: 'Response by study {@unit}, doses shown as {@doses}',
                 script_values: { doses: 'Exceptions' }, search: true, look: 'table' };

const setup = (opts = {}) => {
  const env = loadGear(opts);
  const m = mountGear(env, { block: 'table', id: 'blk-table', typeKey: 'layout' });
  m.gear.update(payload(CONFIG, { derived: { script: SCRIPT, gear: GEAR } }));
  return Object.assign(env, m);
};

test('the gear shows its own rows, in its order, and no other setting', () => {
  const t = setup();
  assert.deepEqual(rows(t.el), ['sv.unit', 'group', 'sort_by', 'sort_dir', 'columns',
    'sv.drugs', 'sv.doses', 'sv.con_meds', 'title', 'subtitle', 'words', 'search']);
  // A key nothing answers to ('nope') has no row; layout, summaries, script are hidden.
  for (const k of ['layout', 'by', 'summaries', 'script', 'look', 'nope']) {
    assert.equal(row(t.el, k), null);
  }
});

test('the group row picks, replaces or drops the outer column of by', () => {
  const t = setup();
  assert.equal(t.gear.values().group, 'ARM');
  t.gear.set({ group: 'BOR' });
  assert.deepEqual(plain(lastSet(t).values), { by: ['BOR', 'USUBJID'] });
  t.gear.set({ group: '' });
  assert.deepEqual(plain(lastSet(t).values), { by: ['USUBJID'] });
  t.gear.set({ group: 'ARM' });
  assert.deepEqual(plain(lastSet(t).values), { by: ['ARM', 'USUBJID'] });
  // The word for the rows' grouping opens the same row.
  assert.equal(t.gear.label('parent'), 'Group by');
  assert.equal(t.gear.slot('parent').kind, 'menu');
  const labels = Array.from(row(t.el, 'group').querySelectorAll('option')).map((o) => o.textContent);
  assert.deepEqual(labels, ['None', 'Arm', 'Best response']);
});

test('the gear has its own tabs, in its order', noUi, () => {
  const t = setup({ ui: true });
  const tabs = Array.from(t.el.querySelectorAll('.blockr-tray-nav__tab')).map((x) => x.textContent);
  assert.deepEqual(tabs, ['Data', 'Patients', 'Lanes', 'Titles', 'Interaction']);
});

test('a script row shows its value and writes script_values', () => {
  const t = setup();
  const g = t.gear;
  assert.equal(g.values()['sv.doses'], 'Exceptions');
  assert.equal(g.values()['sv.con_meds'], false);
  assert.deepEqual(plain(g.values()['sv.drugs']), ['A', 'B']);
  g.set({ 'sv.doses': 'Dots' });
  assert.deepEqual(plain(lastSet(t).values), { script_values: { doses: 'Dots' } });
  // Held until R echoes it: the row and the word show the pick.
  assert.equal(g.values()['sv.doses'], 'Dots');
  assert.equal(g.slot('doses').selected, 'Dots');
  g.set({ 'sv.con_meds': true });
  assert.deepEqual(plain(lastSet(t).values), { script_values: { doses: 'Dots', con_meds: true } });
});

test('the checklist writes summaries: ticked columns in its order, then the others', () => {
  const t = setup();
  assert.deepEqual(plain(t.gear.values().columns), ['Weeks']);
  t.gear.set({ columns: ['Weeks', 'Sex'] });
  assert.deepEqual(plain(lastSet(t).values.summaries), [WEEKS, SEX, SPANS]);
  t.gear.set({ columns: ['Sex'] });
  assert.deepEqual(plain(lastSet(t).values.summaries), [SEX, SPANS]);
});

test('a tick puts a clause into the subtitle and takes it out again', () => {
  const t = setup();
  assert.deepEqual(plain(t.gear.values().words), ['doses', 'unit']);
  t.gear.set({ words: ['doses', 'unit', 'con_meds'] });
  assert.equal(lastSet(t).values.subtitle,
    'Response by study {@unit}, doses shown as {@doses}, con meds {@con_meds}');
  t.gear.set({ words: ['unit'] });
  assert.equal(lastSet(t).values.subtitle, 'Response by study {@unit}');
});

test('setWords keeps the sentence order and the builder\'s own text', () => {
  const env = loadGear();
  const P = env.V.gear.profile;
  const cl = [['a', 'one {@a}'], ['b', 'two {@b}'], ['c', 'three {@c}']];
  assert.equal(P.setWords('Lead, three {@c}', cl, new Set(['a', 'c'])), 'Lead, one {@a}, three {@c}');
  assert.equal(P.setWords('one {@a}, three {@c}', cl, new Set(['a', 'b', 'c'])),
               'one {@a}, two {@b}, three {@c}');
  assert.equal(P.setWords('three {@c}', cl, new Set(['a', 'c'])), 'one {@a}, three {@c}');
  assert.equal(P.setWords('', cl, new Set(['b'])), 'two {@b}');
  assert.equal(P.setWords('Lead, two {@b}', cl, new Set()), 'Lead');
});

test('a package registers a row of its own, which writes through setValues', () => {
  const env = loadGear();
  let drawn = null;
  env.V.gear.editors.mapping = { label: 'Tables', draw: (el, ctx) => { drawn = ctx; } };
  const m = mountGear(env, { block: 'table', id: 'blk-table', typeKey: 'layout' });
  const gear = Object.assign({}, GEAR, { tabs: [{ title: 'Data', sections: [{ title: 'Tables',
                                                                             keys: ['mapping'] }] }] });
  m.gear.update(payload(CONFIG, { derived: { script: SCRIPT, gear } }));
  assert.deepEqual(rows(m.el), ['mapping']);
  assert.ok(drawn && drawn.profile);
  drawn.setValues({ script_values: { doses: 'Plain' } });
  assert.deepEqual(plain(lastSet(env).values), { script_values: { doses: 'Plain' } });
  // Its own key is no setting: nothing is sent.
  const before = env.sent.length;
  drawn.set('x');
  assert.equal(env.sent.length, before);
});

test('a setting without a row still takes its word', () => {
  const t = setup();
  assert.equal(row(t.el, 'look'), null);
  const s = t.gear.slot('look');
  assert.ok(s && s.kind === 'menu');
  s.set('compact');
  assert.deepEqual(plain(lastSet(t).values), { look: 'compact' });
});

test('with its own gear the script band shows none of the script values', () => {
  const t = setup();
  assert.deepEqual(Array.from(t.gear.scriptWords()).sort(), ['con_meds', 'doses', 'drugs', 'unit']);
});

test('without a gear the table shows its own tabs and rows', () => {
  const env = loadGear();
  const m = mountGear(env, { block: 'table', id: 'blk-table', typeKey: 'layout' });
  m.gear.update(payload(CONFIG, { derived: { script: SCRIPT } }));
  assert.ok(rows(m.el).includes('layout'));
  assert.ok(!rows(m.el).includes('sv.doses'));
});

test('showTab opens the gear at a tab of its own', noUi, () => {
  const t = setup({ ui: true });
  t.gear.showTab('Lanes');
  const cur = t.el.querySelector('.blockr-tray-nav__tab.is-current');
  assert.equal(cur && cur.textContent, 'Lanes');
  // A title the gear does not have changes nothing.
  t.gear.showTab('Nope');
  assert.equal(t.el.querySelector('.blockr-tray-nav__tab.is-current').textContent, 'Lanes');
});

test('a labelled script select shows its labels and writes its values', noUi, () => {
  const env = loadGear();
  const m = mountGear(env, { block: 'table', id: 'blk-lab', typeKey: 'layout' });
  const script = {
    inputs: [{ name: 'ae_colour', key: 'sv_ae_colour', label: 'Colour by', kind: 'select',
               options: ['AETOXGR', 'AESEV'], labels: ['Toxicity grade', 'Severity'] }],
    values: { sv_ae_colour: ['AESEV'] }, error: null
  };
  const gear = { tabs: [{ title: 'Lanes', sections: [{ title: 'AEs', keys: ['ae_colour'] }] }] };
  m.gear.update(payload(Object.assign({}, CONFIG, { script_values: { ae_colour: 'AESEV' } }),
                        { derived: { script, gear } }));
  const t = Object.assign(env, m);
  const shown = Array.from(row(t.el, 'sv.ae_colour').querySelectorAll('option')).map((o) => o.textContent);
  assert.deepEqual(shown.filter(Boolean), ['Toxicity grade', 'Severity']);
  // The subtitle word's menu: the labels, standing for the column names.
  const s = t.gear.slot('ae_colour');
  assert.deepEqual(plain(s.options), [{ value: 'AETOXGR', label: 'Toxicity grade' },
                                      { value: 'AESEV', label: 'Severity' }]);
  t.gear.set({ 'sv.ae_colour': 'AETOXGR' });
  assert.deepEqual(plain(lastSet(t).values), { script_values: { ae_colour: 'AETOXGR' } });
});

test('with pick_by the columns row is one select of picks, in order, the data\'s too', () => {
  const env = loadGear();
  const m = mountGear(env, { block: 'table', id: 'blk-table', typeKey: 'layout' });
  const pick = { by: 'USUBJID', sort: 'sort_col', note: 'Per patient only.',
                 more: [{ name: 'AGE', label: 'Age' }, { name: 'bmi', label: 'bmi' }] };
  const gear = Object.assign({}, GEAR, { pick, columns: [GEAR.columns[0]] });
  const AGE = { type: 'field', name: 'Age', col: 'AGE', optional: true };
  const GONE = { type: 'field', name: 'Gone', col: 'GONE', optional: true };
  m.gear.update(payload(Object.assign({}, CONFIG, { summaries: [AGE, WEEKS, GONE, SPANS] }),
                        { derived: { script: SCRIPT, gear } }));
  // Held summaries as picks: a gear column by name, a field by its column.
  assert.deepEqual(plain(m.gear.values().columns), ['d:AGE', 'c:Weeks', 'd:GONE']);
  // A new pick is an optional field named by the label; the first sets the sort.
  m.gear.set({ columns: ['d:bmi', 'c:Weeks', 'd:AGE'] });
  const v = lastSet(env).values;
  assert.deepEqual(plain(v.summaries), [
    { type: 'field', col: 'bmi', name: 'bmi', optional: true }, WEEKS, AGE, SPANS]);
  assert.equal(v.script_values.sort_col, 'bmi');
  m.gear.set({ columns: ['c:Weeks'] });
  assert.equal(lastSet(env).values.script_values.sort_col, 'Weeks');
  m.gear.set({ columns: [] });
  assert.deepEqual(plain(lastSet(env).values.summaries), [SPANS]);
  assert.equal(lastSet(env).values.script_values.sort_col, '');
});

test('the picks row draws a select with the missing pick named, and the note', noUi, () => {
  const env = loadGear({ ui: true });
  const m = mountGear(env, { block: 'table', id: 'blk-table', typeKey: 'layout' });
  const pick = { by: 'USUBJID', sort: 'sort_col', note: 'Per patient only.',
                 more: [{ name: 'AGE', label: 'Age' }] };
  const gear = Object.assign({}, GEAR, { pick, columns: [GEAR.columns[0]] });
  const GONE = { type: 'field', name: 'Gone', col: 'GONE', optional: true };
  m.gear.update(payload(Object.assign({}, CONFIG, { summaries: [WEEKS, GONE, SPANS] }),
                        { derived: { script: SCRIPT, gear } }));
  const r = row(m.el, 'columns');
  assert.ok(r.querySelector('.blockr-select--multi'));
  assert.equal(r.querySelector('.vz-row-note').textContent, 'Per patient only.');
  assert.match(r.textContent, /Gone \(missing\)/);
});
