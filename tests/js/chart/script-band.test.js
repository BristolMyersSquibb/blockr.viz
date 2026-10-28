/* The prepare script's band (chart/script-band.js, features 96 to 98): a
 * control per declared value, written to `script_values` by name, and the
 * script's error said above the chart. Also the histogram's bins from R's
 * edges (derived.hist). */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const D = require('./drive');

const BAR = { type: 'bar', y: 'AETERM', orientation: 'horizontal', drill: 'auto' };
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
const input = (c, key) => c.block.bandEl.querySelector(`[data-gear-input="${key}"]`);

test('the script band draws a control per declared value, and names one it could not make', () => {
  const c = D.open(BAR, { derived: { script: SCRIPT } });
  const band = c.block.bandEl;
  assert.strictEqual(band.style.display, '');
  assert.deepStrictEqual([...band.querySelectorAll('[data-role]')].map((r) => r.dataset.role),
                         ['sv_top_n', 'sv_serious', 'sv_arm']);
  assert.strictEqual(input(c, 'sv_top_n').value, '10');
  assert.strictEqual(input(c, 'sv_serious').checked, false);
  assert.match(band.querySelector('.dd-script-input-error').textContent, /Lvl.*no column "X"/);
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
  assert.strictEqual(c.block.bandEl.querySelector('.dd-script-failed').textContent,
                     'Script failed: object x not found');
  assert.strictEqual(c.block.bandEl.style.display, '');
  c.send(BAR, { derived: { script: { inputs: [], values: {}, error: null } } });
  assert.strictEqual(c.block.bandEl.style.display, 'none');
  assert.strictEqual(c.block.bandEl.children.length, 0);
  c.close();
});

test("a histogram draws every bin R's edges list, the empty ones too", () => {
  const data = { AVAL: [1, 2, 9], AVAL_bin: ['0–5', '0–5', '5–10'] };
  const columns = [{ name: 'AVAL', type: 'numeric', n_unique: 3, label: '' },
                   { name: 'AVAL_bin', type: 'categorical', n_unique: 2, label: '' }];
  const edges = { label: ['0–5', '5–10', '10–15'], lo: [0, 5, 10], hi: [5, 10, 15] };
  const c = D.open({ type: 'histogram', x: 'AVAL' }, { data, columns, derived: { hist: edges } });
  assert.deepStrictEqual(D.plain(c.chart().getOption().xAxis[0].data), ['0–5', '5–10', '10–15']);
  c.close();
});
