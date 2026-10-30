/* The groups layout's title words (R's groups_title_roles()): `{@by}` and `{@parent}`
 * print the row columns, `{@value}`, `{@func}`, `{@facet}`, `{@fill}` and a set
 * `{@color}` print the first summary's parts. On 0.2.x's summarize table each was a link
 * (`By {@group}, top {@top_n}`); on the rewrite `{@by}` was plain text on the first
 * payload (the layouts paint the titles before the gear's update) and the summary words
 * were plain text always (no setting of that name). table/gear.js groupsSlot(). */
'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadTable, payload, flatModel } = require('./load');

const COLUMNS = [{ name: 'ARM', class: 'character', label: 'Arm' },
                 { name: 'SEX', class: 'character', label: 'Sex' },
                 { name: 'AESOC', class: 'character', label: 'System organ class' },
                 { name: 'AGE', class: 'number', label: 'Age' }];
const S1 = { type: 'simple', name: 'n', func: 'count', col: null, show: 'number' };
const S2 = { type: 'simple', name: 'Mean age', func: 'mean', col: 'AGE', show: 'number' };

const part = (text, arg) => (arg ? { text, arg } : { text });

function mount(config, parts) {
  const env = loadTable();
  const el = env.add();
  env.b.renderValue(el, payload({
    data: flatModel(3), columns: COLUMNS,
    config: Object.assign({ layout: 'groups', by: ['ARM'], top_n: 3, summaries: [S1, S2] }, config),
    derived: { titles: { title: null, subtitle: { text: '', parts }, caption: null } }
  }));
  const gear = el._viz.view.gear.engine;
  const words = () => Array.from(el.querySelectorAll('.vz-subtitle .blockr-slot')).map((w) => w.textContent);
  const sets = () => env.sent.filter((s) => s.name.endsWith('_set:blockr.viz.table'))
    .map((s) => JSON.parse(JSON.stringify(s.value.values)));
  return Object.assign(env, { el, gear, words, sets });
}

test('every groups word is live on the first payload', () => {
  const t = mount({ by: ['AESOC', 'ARM'], column_axis: true, summaries: [Object.assign({}, S1, { color: 'SEX' }), S2] },
    [part('By '), part('Arm', 'by'), part(' in '), part('System organ class', 'parent'), part(', '),
     part('count', 'func'), part(' coloured by '), part('Sex', 'color'), part(', top '), part('3', 'top_n'),
     part(' axis', 'column_axis')]);
  assert.deepEqual(t.words(), ['Arm', 'System organ class', 'count', 'Sex', '3', ' axis']);
});

test('a flag word reads the current value on the first payload', () => {
  const t = mount({ column_axis: true }, [part('axis', 'column_axis')]);
  t.el.querySelector('.vz-subtitle .blockr-slot').click();
  assert.deepEqual(t.sets(), [{ column_axis: false }]);
});

test('the by word picks one column and replaces the innermost, as {@group} did', () => {
  const t = mount({ by: ['AESOC', 'ARM'] }, [part('Arm', 'by')]);
  const by = t.gear.slot('by');
  assert.equal(by.kind, 'menu');
  assert.equal(by.multi, false);
  assert.equal(by.selected, 'ARM');
  assert.ok(by.options.some((o) => o.value === 'SEX'));
  assert.ok(!by.options.some((o) => o.value == null), 'no (none): the table needs a row column');
  by.set('SEX');
  assert.deepEqual(t.sets(), [{ by: ['AESOC', 'SEX'] }]);
});

test('the parent word replaces or drops the outer column', () => {
  const t = mount({ by: ['AESOC', 'ARM'] }, [part('System organ class', 'parent')]);
  const parent = t.gear.slot('parent');
  assert.equal(parent.selected, 'AESOC');
  parent.set('SEX');
  t.gear.slot('parent').set(null);
  assert.deepEqual(t.sets(), [{ by: ['SEX', 'ARM'] }, { by: ['ARM'] }]);
  const single = mount({ by: ['ARM'] }, []);
  assert.equal(single.gear.slot('parent'), null, 'one level: no outer column');
});

test('the summary words write the first summary, keeping the others', () => {
  const t = mount({ summaries: [Object.assign({}, S1, { col: 'AGE', func: 'mean' }), S2] }, []);
  const func = t.gear.slot('func');
  assert.equal(func.kind, 'menu');
  assert.ok(func.options.some((o) => o.value === 'median'));
  func.set('median');
  t.gear.slot('value').set('ARM');
  const sets = t.sets();
  assert.deepEqual(sets[0], { summaries: [Object.assign({}, S1, { col: 'AGE', func: 'median' }), S2] });
  assert.equal(sets[1].summaries[0].col, 'ARM');
  assert.deepEqual(sets[1].summaries[1], S2);
  assert.equal(t.gear.slot('facet').kind, 'menu');
  assert.equal(t.gear.slot('fill').kind, 'menu');
});

test('{@color} is the summary\'s colour when it has one, else the table\'s own', () => {
  const plain = mount({}, []);
  plain.gear.slot('color').set('SEX');
  assert.deepEqual(plain.sets(), [{ color: 'SEX' }]);
  const painted = mount({ summaries: [Object.assign({}, S1, { color: 'ARM' }), S2] }, []);
  assert.equal(painted.gear.slot('color').selected, 'ARM');
  painted.gear.slot('color').set('SEX');
  assert.equal(painted.sets()[0].summaries[0].color, 'SEX');
});

test('outside the groups layout the words are the settings of those names', () => {
  const t = mount({ layout: 'records', by: ['ARM'] }, []);
  assert.equal(t.gear.slot('value'), null);
  assert.equal(t.gear.slot('func'), null);
  assert.equal(t.gear.slot('by').multi, true, 'the records layout nests by a list of columns');
});

test('a groups word wins over a prepare-script value of the same name, as in R', () => {
  const t = mount({}, []);
  t.gear.prime(payload({
    columns: COLUMNS, config: { layout: 'groups', by: ['ARM'], summaries: [S1] },
    derived: { script: { inputs: [{ name: 'func', key: 'sv_func', label: 'Func', kind: 'text' }],
                         values: { sv_func: 'x' }, error: null } }
  }));
  assert.equal(t.gear.scriptSlot('func'), null);
  assert.equal(t.gear.slot('func').label, 'Aggregate');
});
