/* The groups table's sentence on the face: the words R marks as settings
 * are live, as they were on 0.2.x (a summarize table's subtitle
 * "By {@group}, top {@top_n}" drew both words as links). The gear and the
 * title painter are the real ones. */
'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadTable, payload, flatModel } = require('./load');

const SUBTITLE = {
  text: 'By ARM, top 3',
  parts: [{ text: 'By ' }, { text: 'ARM', arg: 'by' }, { text: ', top ' }, { text: '3', arg: 'top_n' }]
};

function mount() {
  const env = loadTable();
  const el = env.add();
  env.b.renderValue(el, payload({
    data: flatModel(3), config: { layout: 'groups', by: ['ARM'], top_n: 3 },
    derived: { titles: { title: null, subtitle: SUBTITLE, caption: null } }
  }));
  const words = () => Array.from(el.querySelectorAll('.vz-subtitle .blockr-slot')).map((w) => w.textContent);
  const sets = () => env.sent.filter((s) => s.name.startsWith('tbl_set'))
    .map((s) => JSON.parse(JSON.stringify(s.value.values)));
  return Object.assign(env, { el, words, sets });
}

test('the top_n word opens a number field, and Enter writes top_n', () => {
  const t = mount();
  assert.ok(t.words().includes('3'));
  Array.from(t.el.querySelectorAll('.vz-subtitle .blockr-slot')).find((w) => w.textContent === '3').click();
  const input = t.el.querySelector('.vz-subtitle input');
  assert.equal(input.type, 'number');
  input.value = '10';
  input.dispatchEvent(new t.win.KeyboardEvent('keydown', { key: 'Enter', bubbles: true }));
  assert.deepEqual(t.sets(), [{ top_n: 10 }]);
});

test('the by word opens its menu, as {@group} did on 0.2', () => {
  const t = mount();
  assert.deepEqual(t.words(), ['ARM', '3']);
  // One column, replacing the word's own (groups-words.test.js has the nested case).
  const by = t.el._viz.view.gear.engine.slot('by');
  assert.equal(by.kind, 'menu');
  assert.equal(by.multi, false);
  by.set('SEX');
  assert.deepEqual(t.sets(), [{ by: ['SEX'] }]);
});
