/* A prepare script's values named in a title (`{@min_share}`): the word is the value's
 * control on the face, and it writes `script_values` by the variable's name, as the
 * script band's own control does (core/gear/engine.js scriptSlot(), core/titles.js).
 * Production 0.2.x drew them so; the rewrite lost it while R still resolved the text,
 * which is why these tests drive the browser side. Tables and charts share the engine;
 * the views are tested in table/script-slots.test.js and chart/script-band.test.js. */
'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadGear, mountGear, payload, lastSet, hasUi } = require('./gear-load');

const plain = (x) => JSON.parse(JSON.stringify(x));
const tick = (win, ms = 5) => new Promise((r) => win.setTimeout(r, ms));
const key = (t, el, k) => el.dispatchEvent(new t.win.KeyboardEvent('keydown', { key: k }));

const SCRIPT = {
  inputs: [
    { name: 'min_share', key: 'sv_min_share', label: 'Minimum threshold', kind: 'number', min: 0, max: 100 },
    { name: 'share_unit', key: 'sv_share_unit', label: 'Threshold unit', kind: 'select', options: ['Patients', 'Percent'] },
    { name: 'serious', key: 'sv_serious', label: 'Serious only', kind: 'segmented',
      options: [{ value: 'on', label: 'Serious only' }, { value: 'off', label: 'Serious only' }] },
    { name: 'note', key: 'sv_note', label: 'Note', kind: 'text' },
    { name: 'arms', key: 'sv_arms', label: 'Arms', kind: 'multi', options: ['A', 'B'] },
    { name: 'bins', key: 'sv_bins', label: 'Bins (script)', kind: 'number' },
    { name: 'bad', key: 'sv_bad', label: 'Bad', kind: 'error', error: 'no column "X"' }
  ],
  values: { sv_min_share: 1, sv_share_unit: 'Patients', sv_serious: 'off', sv_note: 'all',
            sv_arms: ['A'], sv_bins: 4 },
  error: null
};

const SUBTITLE = {
  parts: [{ text: 'At least ' }, { text: '1', arg: 'min_share' }, { text: ' ' },
          { text: 'Patients', arg: 'share_unit' }, { text: ', ' }, { text: 'by ARM' }],
  offers: ['serious', 'color']
};

const CONFIG = { type: 'bar', category: 'ARM', orientation: 'vertical', func: 'count',
                 script_values: { keep: 3 } };

const setup = (opts = {}) => {
  const env = loadGear(opts);
  const m = mountGear(env);
  m.gear.update(payload(CONFIG, { derived: { titles: { subtitle: SUBTITLE }, script: SCRIPT } }));
  return Object.assign(env, m);
};

/** The subtitle painted with the face's slots, in a fresh element. */
const face = (t, band = SUBTITLE, openGear) => {
  const slots = t.V.titles.slots({ gear: t.gear, scope: t.scope, openGear });
  const head = t.doc.body.appendChild(t.doc.createElement('div'));
  slots.paint(head, band);
  return { head, slots, words: () => Array.from(head.querySelectorAll('.blockr-slot')) };
};

test('a word that names a script value opens that control, with its label and bounds', () => {
  const t = setup();
  assert.deepEqual(plain(t.gear.slot('min_share')),
    { kind: 'number', min: 0, max: 100, step: 1, value: 1, label: 'Minimum threshold' });
  assert.deepEqual(plain(t.gear.slot('share_unit')),
    { kind: 'menu', options: [{ value: 'Patients', label: 'Patients' }, { value: 'Percent', label: 'Percent' }],
      selected: 'Patients', multi: false, label: 'Threshold unit' });
  assert.deepEqual(plain(t.gear.slot('serious')), { kind: 'flag', value: false, label: 'Serious only' });
  assert.equal(t.gear.slot('note').kind, 'entry');
  assert.equal(t.gear.slot('arms').multi, true);
});

test('a block setting of the same name wins; a failed or unknown declaration opens nothing', () => {
  const t = setup();
  assert.equal(t.gear.slot('bins').label, 'Bins', "the chart's own bins, not the script's");
  assert.equal(t.gear.scriptSlot('bins'), null);
  assert.equal(t.gear.slot('bad'), null);
  assert.equal(t.gear.slot('nothing'), null);
  t.gear.update(payload(CONFIG, { derived: { titles: { subtitle: SUBTITLE } } }));
  assert.equal(t.gear.slot('min_share'), null, 'no script, no control');
});

test("scriptWords: the script values the titles' words and offers open", () => {
  const t = setup();
  assert.deepEqual(Array.from(t.gear.scriptWords()).sort(), ['min_share', 'serious', 'share_unit']);
  t.gear.update(payload(CONFIG, { derived: { titles: { subtitle: { text: 'Plain' } }, script: SCRIPT } }));
  assert.deepEqual(Array.from(t.gear.scriptWords()), []);
});

test('on the face the words are slots; the offer reads the control\'s label', () => {
  const t = setup();
  const f = face(t);
  assert.deepEqual(f.words().map((w) => w.textContent), ['1', 'Patients']);
  assert.match(f.head.textContent, /by ARM/);
  const offers = Array.from(f.head.querySelectorAll('.blockr-slot-offer')).map((o) => o.textContent);
  assert.deepEqual(offers, ['+ Serious only', '+ Color']);
});

test('typing over a number word writes script_values by name, keeping the others', () => {
  const t = setup();
  const f = face(t);
  const word = f.words()[0];
  word.click();
  const input = f.head.querySelector('input.blockr-slot-input');
  assert.equal(input.type, 'number');
  assert.equal(input.value, '1');
  assert.equal(input.getAttribute('aria-label'), 'Minimum threshold');
  input.value = '5';
  key(t, input, 'Enter');
  assert.deepEqual(lastSet(t).values, { script_values: { keep: 3, min_share: 5 } });
  assert.ok(f.head.contains(word), 'the word is back');
  // The edit is held until R echoes it: the word reopens on the new value.
  word.click();
  assert.equal(f.head.querySelector('input.blockr-slot-input').value, '5');
});

test('a number word is held in the control\'s range; Escape keeps the value', () => {
  const t = setup();
  const f = face(t);
  f.words()[0].click();
  let input = f.head.querySelector('input.blockr-slot-input');
  input.value = '500';
  key(t, input, 'Enter');
  assert.deepEqual(lastSet(t).values, { script_values: { keep: 3, min_share: 100 } });
  const before = t.sent.length;
  f.words()[0].click();
  input = f.head.querySelector('input.blockr-slot-input');
  input.value = '7';
  key(t, input, 'Escape');
  assert.equal(t.sent.length, before);
});

test('a flag offer toggles the value; a text word is typed over', () => {
  const t = setup();
  const f = face(t, { parts: [{ text: 'Note: ' }, { text: 'all', arg: 'note' }], offers: ['serious'] });
  f.head.querySelector('.blockr-slot-offer').click();
  assert.deepEqual(lastSet(t).values, { script_values: { keep: 3, serious: true } });
  f.words()[0].click();
  const input = f.head.querySelector('input.blockr-slot-input');
  assert.equal(input.type, 'text');
  assert.equal(input.value, 'all');
  input.value = 'serious only';
  key(t, input, 'Enter');
  assert.deepEqual(lastSet(t).values, { script_values: { keep: 3, serious: true, note: 'serious only' } });
});

test('a choice word opens its list and a pick writes script_values', { skip: !hasUi() }, async () => {
  const t = setup({ ui: true });
  const f = face(t);
  const word = f.words()[1];
  word.click();
  assert.ok(word.classList.contains('blockr-slot--open'));
  const opt = Array.from(t.doc.querySelectorAll('.blockr-select__option'))
    .find((o) => o.textContent.includes('Percent'));
  assert.ok(opt, 'the menu lists the declared choices');
  opt.click();
  assert.deepEqual(lastSet(t).values, { script_values: { keep: 3, share_unit: 'Percent' } });
  await tick(t.win);
  f.slots.dispose();
});
