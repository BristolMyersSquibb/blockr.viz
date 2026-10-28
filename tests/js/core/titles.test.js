'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadGear, mountGear, payload, lastSet, row, hasUi, COLUMNS } = require('./gear-load');

const plain = (x) => JSON.parse(JSON.stringify(x));
const tick = (win, ms = 5) => new Promise((r) => win.setTimeout(r, ms));

const TITLES = {
  title: { parts: [{ text: 'Count of ' }, { text: 'ARM', arg: 'x', by: 'name' }],
           offers: ['color'] },
  subtitle: null,
  caption: { text: 'N = 40' },
  args: { x: 'ARM', func: 'count', color: null, value_labels: false, bins: 30 },
  choices: {}
};

const setup = (opts = {}) => {
  const env = loadGear(opts);
  const m = mountGear(env);
  m.gear.update(payload({ type: 'bar', category: 'ARM', orientation: 'vertical', func: 'count', title: 'Count of {@x}',
                          value_labels: false }, { derived: { titles: TITLES } }));
  return Object.assign(env, m);
};
const field = (t, key = 'title') => row(t.el, key).querySelector('input');

/** Type `text` at the caret, one character at a time, as InputEvents. */
function typeAt(t, inp, text) {
  inp.focus();
  for (const ch of text) {
    const at = inp.selectionStart ?? inp.value.length;
    inp.value = inp.value.slice(0, at) + ch + inp.value.slice(at);
    inp.setSelectionRange(at + 1, at + 1);
    inp.dispatchEvent(new t.win.InputEvent('input', { data: ch }));
  }
}
const key = (t, inp, k) => inp.dispatchEvent(new t.win.KeyboardEvent('keydown', { key: k }));
const menuEl = (t) => t.doc.body.querySelector('.vz-at-menu');
const items = (t) => Array.from(menuEl(t).querySelectorAll('.vz-at-tok')).map((e) => e.textContent);

test('menuGroups: settings under public names, columns, counts and structure', () => {
  const t = setup();
  const groups = plain(t.V.titles.menuGroups(TITLES, COLUMNS,
    (k) => t.gear.label(k)));
  assert.deepEqual(groups.map((g) => g.head),
    ["This block's settings", 'Counts and filters', 'Structure', 'Data columns']);
  const toks = groups[0].items.map((i) => i.tok);
  assert.deepEqual(toks.slice(0, 3), ['{@x}', '{label(@x)}', '{@func}']);
  assert.equal(groups[0].items[0].label, 'X');
  assert.equal(groups[0].items[1].val, 'Arm');
  assert.ok(groups[3].items.some((i) => i.tok === '{n_distinct(AGE)}'));
  assert.ok(!toks.some((x) => /group|chart_type|agg_fn/.test(x)), 'no old names');
});

test('the template row shows what the card shows, with its slots and offers', () => {
  const t = setup();
  const r = row(t.el, 'title');
  assert.equal(field(t).value, 'Count of {@x}');
  assert.equal(field(t).spellcheck, false);
  const body = r.querySelector('.vz-template-preview');
  assert.equal(body.querySelector('.blockr-slot--preview').textContent, 'ARM');
  assert.equal(body.querySelector('.vz-template-offer').textContent, '+ Color');
  assert.equal(row(t.el, 'caption').querySelector('.vz-template-preview').textContent, 'N = 40');
});

test('typing @ opens the menu; a pick writes the public name at the @', () => {
  const t = setup();
  const inp = field(t);
  inp.setSelectionRange(inp.value.length, inp.value.length);
  typeAt(t, inp, ' by @');
  assert.ok(menuEl(t), 'the menu is on <body>');
  assert.ok(items(t).includes('{@func}'));
  typeAt(t, inp, 'fu');
  assert.deepEqual(items(t).slice(0, 1), ['{@func}']);
  key(t, inp, 'Enter');
  assert.equal(menuEl(t), null);
  assert.equal(inp.value, 'Count of {@x} by {@func}');
  assert.equal(inp.selectionStart, inp.value.length);
  assert.equal(t.sent.length, 0, 'the pick does not commit');
  key(t, inp, 'Enter');
  assert.deepEqual(lastSet(t).values, { title: 'Count of {@x} by {@func}' });
});

test('the menu: arrows move, [ ] wraps the caret, Escape and a space close it', () => {
  const t = setup();
  const inp = field(t);
  inp.value = '';
  inp.setSelectionRange(0, 0);
  typeAt(t, inp, '@');
  const hl = () => menuEl(t).querySelector('.vz-at-item--hl .vz-at-tok').textContent;
  assert.equal(hl(), '{@x}');
  key(t, inp, 'ArrowDown');
  assert.equal(hl(), '{label(@x)}');
  key(t, inp, 'ArrowUp');
  key(t, inp, 'ArrowUp');
  assert.equal(hl(), items(t)[items(t).length - 1], 'wraps around');
  key(t, inp, 'Escape');
  assert.equal(menuEl(t), null);
  typeAt(t, inp, '@opt');
  key(t, inp, 'Enter');
  assert.equal(inp.value, '@[, ]');
  assert.equal(inp.selectionStart, 4, 'the caret sits inside the brackets, after ", "');
  typeAt(t, inp, '@');
  assert.ok(menuEl(t));
  typeAt(t, inp, ' ');
  assert.equal(menuEl(t), null, 'a space ends the query');
  typeAt(t, inp, '@zzz');
  assert.match(menuEl(t).textContent, /Nothing matches "zzz"/);
});

test('a click on an item picks it; leaving the field closes the menu', () => {
  const t = setup();
  const inp = field(t);
  inp.value = '';
  typeAt(t, inp, '@');
  const target = Array.from(menuEl(t).querySelectorAll('.vz-at-item'))
    .find((b) => b.querySelector('.vz-at-tok').textContent === '{n}');
  target.dispatchEvent(new t.win.MouseEvent('mousedown', { bubbles: true, cancelable: true }));
  assert.equal(inp.value, '{n}');
  typeAt(t, inp, '@');
  inp.dispatchEvent(new t.win.Event('blur'));
  assert.equal(menuEl(t), null);
});

test('a token nothing answers to is named; an emptied field goes back to auto', () => {
  const t = setup();
  const inp = field(t);
  inp.value = 'By {@colr} and {AGE} and {label(@x)}';
  inp.dispatchEvent(new t.win.Event('input'));
  const warn = row(t.el, 'title').querySelector('.vz-template-warn');
  assert.equal(warn.hidden, false);
  assert.match(warn.textContent, /Nothing is called \{@colr\}\./);
  assert.ok(row(t.el, 'title').classList.contains('vz-row--dirty'));
  inp.value = '   ';
  key(t, inp, 'Enter');
  assert.deepEqual(lastSet(t).values, { title: null });
});

test('the line under a field is marked when it would only repeat the field', () => {
  const t = setup();
  const same = (key) => row(t.el, key).querySelector('.vz-template-shows')
    .classList.contains('is-same');
  assert.equal(same('title'), false, 'a slot: the line says what it prints');
  assert.equal(same('caption'), false, 'an empty field: the line shows the auto text');
  t.gear.update(payload({ type: 'bar', x: 'ARM', func: 'count', title: 'Adverse events',
                          value_labels: false }, { derived: { titles: TITLES } }));
  assert.equal(same('title'), true, 'plain text: nothing to add');
});

test('slots: words open their setting; plain words stay text', () => {
  const t = setup();
  const opened = [];
  const slots = t.V.titles.slots({ gear: t.gear, scope: t.scope, openGear: () => opened.push(1) });
  const head = t.doc.body.appendChild(t.doc.createElement('div'));
  slots.paint(head, { parts: [{ text: 'Count' }, { text: ' of ' }, { text: 'ARM', arg: 'x' },
                              { text: ' in CHF', arg: 'unit' }, { text: ' values', arg: 'value_labels' },
                              { text: ' 30 bins', arg: 'bins' }],
                      offers: ['color', 'facet', 'label', 'tooltip'] });
  const words = Array.from(head.querySelectorAll('.blockr-slot')).map((w) => w.textContent);
  assert.deepEqual(words, ['ARM', ' values', ' 30 bins'], 'unit has no control on the face');
  assert.match(head.textContent, /in CHF/);
  const offers = Array.from(head.querySelectorAll('.blockr-slot-offer')).map((w) => w.textContent);
  assert.deepEqual(offers, ['+ Color', '+ Facet', 'More settings']);
  head.querySelector('.blockr-slot-offer--more').click();
  assert.equal(opened.length, 1);

  head.querySelectorAll('.blockr-slot')[1].click();
  assert.deepEqual(lastSet(t).values, { value_labels: true }, 'a flag toggles in place');

  const num = head.querySelectorAll('.blockr-slot')[2];
  num.click();
  const input = head.querySelector('input.blockr-slot-input');
  assert.equal(input.type, 'number');
  input.value = '30';
  key(t, input, 'ArrowUp');
  assert.equal(input.value, '31');
  input.value = '999';
  key(t, input, 'Enter');
  assert.deepEqual(lastSet(t).values, { bins: 200 }, 'held in its range');
  assert.ok(head.contains(num), 'the word is back');
});

test('slots without blockr.ui open the gear for a list', () => {
  const t = setup();
  const opened = [];
  const slots = t.V.titles.slots({ gear: t.gear, scope: t.scope, openGear: () => opened.push(1) });
  const head = t.doc.body.appendChild(t.doc.createElement('div'));
  slots.paint(head, { parts: [{ text: 'ARM', arg: 'x' }] });
  head.querySelector('.blockr-slot').click();
  assert.equal(opened.length, 1);
});

test('slots with blockr.ui: a word opens Blockr.Select.menu and a pick sends', { skip: !hasUi() }, async () => {
  const t = setup({ ui: true });
  const slots = t.V.titles.slots({ gear: t.gear, scope: t.scope });
  const head = t.doc.body.appendChild(t.doc.createElement('div'));
  slots.paint(head, { parts: [{ text: 'ARM', arg: 'x', by: 'name' }] });
  const word = head.querySelector('.blockr-slot');
  word.click();
  assert.ok(word.classList.contains('blockr-slot--open'));
  const opt = Array.from(t.doc.querySelectorAll('.blockr-select__option'))
    .find((o) => o.getAttribute('data-value') === 'SEX');
  assert.ok(opt, 'the menu lists the columns');
  opt.click();
  assert.deepEqual(lastSet(t).values, { x: 'SEX' });
  await tick(t.win);
  assert.equal(t.doc.querySelector('.blockr-select-menu-host'), null, 'the menu is gone');
  slots.dispose();
});
