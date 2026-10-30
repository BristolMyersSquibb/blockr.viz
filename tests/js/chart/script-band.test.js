/* The prepare script's band (core/script-band.js, features 96 to 98): a
 * control per declared value, written to `script_values` by name, and the
 * script's error said above the chart. Also the histogram's bins from R's
 * edges (derived.hist). */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const D = require('./drive');

const BAR = { type: 'bar', category: 'AETERM', orientation: 'horizontal', drill: 'auto' };
const SCRIPT = {
  inputs: [
    { name: 'top_n', key: 'sv_top_n', label: 'Top n', kind: 'number', min: 1, max: 50 },
    { name: 'serious', key: 'sv_serious', label: 'Serious only', kind: 'segmented',
      options: [{ value: 'on', label: 'Serious only' }, { value: 'off', label: 'Serious only' }] },
    { name: 'arm', key: 'sv_arm', label: 'Arm', kind: 'select', options: ['A', 'B'] },
    { name: 'lvl', key: 'sv_lvl', label: 'Lvl', kind: 'error', error: 'no column "X"' }
  ],
  values: { sv_top_n: 10, sv_serious: 'off', sv_arm: 'A' },
  error: null
};
const input = (c, key) => c.block.band.el.querySelector(`[data-gear-input="${key}"]`);

test('the script band draws a control per declared value, and names one it could not make', () => {
  const c = D.open(BAR, { derived: { script: SCRIPT } });
  const band = c.block.band.el;
  assert.strictEqual(band.style.display, '');
  assert.deepStrictEqual([...band.querySelectorAll('[data-role]')].map((r) => r.dataset.role),
                         ['sv_top_n', 'sv_serious', 'sv_arm']);
  assert.strictEqual(input(c, 'sv_top_n').value, '10');
  assert.strictEqual(input(c, 'sv_serious').checked, false);
  assert.match(band.querySelector('.vz-script-input-error').textContent, /Lvl.*no column "X"/);
  c.close();
});

test('a script control writes script_values by name, a flag as true or false', () => {
  const c = D.open({ ...BAR, script_values: { top_n: 10 } }, { derived: { script: SCRIPT } });
  input(c, 'sv_serious').click();
  const n = input(c, 'sv_top_n');
  n.focus();
  n.value = '5';
  n.dispatchEvent(new c.env.win.Event('blur'));
  assert.deepStrictEqual(D.plain(c.sets()), [
    { script_values: { top_n: 10, serious: true } },
    { script_values: { top_n: 5, serious: true } }
  ]);
  c.close();
});

test('a failing script is said above the chart; no script, no band', () => {
  const c = D.open(BAR, { derived: { script: { inputs: [], values: {}, error: 'object x not found' } } });
  assert.strictEqual(c.block.band.el.querySelector('.vz-script-failed').textContent,
                     'Script failed: object x not found');
  assert.strictEqual(c.block.band.el.hidden, false);
  c.send(BAR, { derived: { script: { inputs: [], values: {}, error: null } } });
  assert.strictEqual(c.block.band.el.hidden, true);
  assert.strictEqual(c.block.band.el.children.length, 0);
  c.close();
});

test("a histogram draws every bin R's edges list, the empty ones too", () => {
  const data = { AVAL: [1, 2, 9], AVAL_bin: ['0–5', '0–5', '5–10'] };
  const columns = [{ name: 'AVAL', class: 'number', n_unique: 3, label: '' },
                   { name: 'AVAL_bin', class: 'string', n_unique: 2, label: '' }];
  const edges = { label: ['0–5', '5–10', '10–15'], lo: [0, 5, 10], hi: [5, 10, 15] };
  const c = D.open({ type: 'histogram', x: 'AVAL' }, { data, columns, derived: { hist: edges } });
  assert.deepStrictEqual(D.plain(c.chart().getOption().xAxis[0].data), ['0–5', '5–10', '10–15']);
  c.close();
});

// A value the subtitle names (`Top {@top_n} terms`) is a word on the face and not a row in
// the band: production 0.2.x drew script values so, and the rewrite had lost it.
const NAMED = (n) => ({ subtitle: { parts: [{ text: 'Top ' }, { text: String(n), arg: 'top_n' },
                                            { text: ' terms' }] } });

test('a script value the subtitle names is a word on the face, not a band row', () => {
  const c = D.open(BAR, { derived: { titles: NAMED(10), script: SCRIPT } });
  const sub = c.block.frame.subEl;
  const word = sub.querySelector('.blockr-slot');
  assert.ok(word, 'the value is a clickable word');
  assert.strictEqual(word.textContent, '10');
  assert.deepStrictEqual([...c.block.band.el.querySelectorAll('[data-role]')].map((r) => r.dataset.role),
                         ['sv_serious', 'sv_arm'], 'the band keeps only the controls no word opens');
  c.close();
});

test('the word sets the value; R echoes the new subtitle; a dropped word brings the row back', () => {
  const c = D.open(BAR, { derived: { titles: NAMED(10), script: SCRIPT } });
  c.block.frame.subEl.querySelector('.blockr-slot').click();
  const input = c.block.frame.subEl.querySelector('input.blockr-slot-input');
  assert.strictEqual(input.value, '10');
  input.value = '5';
  input.dispatchEvent(new c.env.win.KeyboardEvent('keydown', { key: 'Enter' }));
  assert.deepStrictEqual(D.plain(c.sets()), [{ script_values: { top_n: 5 } }]);
  const echoed = { ...SCRIPT, values: { ...SCRIPT.values, sv_top_n: 5 } };
  c.send({ ...BAR, script_values: { top_n: 5 } }, { derived: { titles: NAMED(5), script: echoed } });
  assert.strictEqual(c.block.frame.subEl.textContent, 'Top 5 terms');
  assert.strictEqual(c.block.band.el.querySelector('[data-role="sv_top_n"]'), null);
  c.send({ ...BAR, script_values: { top_n: 5 } }, { derived: { titles: { subtitle: { text: 'All terms' } },
                                                             script: echoed } });
  assert.ok(c.block.band.el.querySelector('[data-role="sv_top_n"]'), 'no word, so the band has the control');
  assert.strictEqual(input.isConnected, false);
  c.close();
});
