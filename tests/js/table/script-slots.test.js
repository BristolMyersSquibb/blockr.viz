/* The table's face for a prepare script whose values the subtitle names: the words are
 * the controls, the band leaves those controls out, and a word writes `script_values`.
 * The layouts paint the titles before the gear's update, so the view hands the gear the
 * script first (table/view.js); the first payload already has its words. */
'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadTable, payload, flatModel } = require('./load');

const SCRIPT = (values = {}) => ({
  inputs: [
    { name: 'min_share', key: 'sv_min_share', label: 'Minimum threshold', kind: 'number', min: 0 },
    { name: 'min_share_unit', key: 'sv_min_share_unit', label: 'Threshold unit', kind: 'select',
      options: ['Patients', 'Percent'] },
    { name: 'rows', key: 'sv_rows', label: 'Rows', kind: 'select',
      options: ['Preferred Term', 'System Organ Class & Preferred Term'] }
  ],
  values: Object.assign({ sv_min_share: 1, sv_min_share_unit: 'Patients', sv_rows: 'Preferred Term' }, values),
  error: null
});

// `By: {@rows}[ Display AEs with at least {@min_share} {@min_share_unit}]`, as R resolves it.
const TITLES = (n = 1) => ({
  subtitle: { parts: [{ text: 'By: ' }, { text: 'Preferred Term', arg: 'rows' },
                      { text: ' Display AEs with at least ' }, { text: String(n), arg: 'min_share' },
                      { text: ' ' }, { text: 'Patients', arg: 'min_share_unit' }] }
});

function mount(p, env = loadTable()) {
  const el = env.add();
  env.b.renderValue(el, p);
  const sets = () => env.sent.filter((s) => s.name.endsWith('_set:blockr.viz.table'))
    .map((s) => JSON.parse(JSON.stringify(s.value.values)));
  return Object.assign(env, { el, sets, $: (s) => el.querySelector(s),
                              $$: (s) => Array.from(el.querySelectorAll(s)),
                              render: (q) => env.b.renderValue(el, q) });
}

test('the subtitle\'s script values are words from the first payload; the band has none', () => {
  const t = mount(payload({ data: flatModel(3), derived: { titles: TITLES(), script: SCRIPT() } }));
  assert.deepEqual(t.$$('.vz-subtitle .blockr-slot').map((w) => w.textContent),
                   ['Preferred Term', '1', 'Patients']);
  assert.equal(t.$('.vz-script-band').hidden, true, 'every control has its word');
  assert.equal(t.$$('.vz-script-band-row').length, 0);
});

test('a control the subtitle does not name stays in the band', () => {
  const titles = { subtitle: { parts: [{ text: 'At least ' }, { text: '1', arg: 'min_share' }] } };
  const t = mount(payload({ data: flatModel(3), derived: { titles, script: SCRIPT() } }));
  assert.deepEqual(t.$$('.vz-script-band-row').map((r) => r.getAttribute('data-role')),
                   ['sv_min_share_unit', 'sv_rows']);
  assert.equal(t.$('.vz-script-band').hidden, false);
});

test('a word sets its value; the echoed subtitle shows it', () => {
  const first = payload({ data: flatModel(3), derived: { titles: TITLES(), script: SCRIPT() } });
  const t = mount(first);
  t.$$('.vz-subtitle .blockr-slot')[1].click();
  const input = t.$('.vz-subtitle input.blockr-slot-input');
  assert.equal(input.value, '1');
  input.value = '3';
  input.dispatchEvent(new t.win.KeyboardEvent('keydown', { key: 'Enter' }));
  assert.deepEqual(t.sets(), [{ script_values: { min_share: 3 } }]);
  t.render(payload({ config: { script_values: { min_share: 3 } }, dataRev: first.data_rev,
                     derived: { titles: TITLES(3), script: SCRIPT({ sv_min_share: 3 }) } }));
  assert.match(t.$('.vz-subtitle').textContent, /at least 3 Patients/);
  assert.equal(t.$$('.vz-script-band-row').length, 0);
});

// The table's own `top_n` ("Rows shown") is unset; the script declares `top_n` too. R
// prints the script's 25, so the word opens the script's control and the band skips it.
test("a script's top_n is the word, not the table's unset top_n", () => {
  const script = {
    inputs: [{ name: 'top_n', key: 'sv_top_n', label: 'Top adverse events', kind: 'number', min: 1 },
             { name: 'ae_variable', key: 'sv_ae_variable', label: 'Adverse event', kind: 'select',
               options: ['AEDECOD', 'AETERM'] }],
    values: { sv_top_n: 25, sv_ae_variable: 'AEDECOD' }, error: null
  };
  const titles = {
    subtitle: { parts: [{ text: 'Top ' }, { text: '25', arg: 'top_n' }, { text: ' Adverse Event ' },
                        { text: 'AEDECOD', arg: 'ae_variable' }] }
  };
  const t = mount(payload({ data: flatModel(3), config: { layout: 'groups', by: ['ARM'] },
                            derived: { titles, script } }));
  assert.deepEqual(t.$$('.vz-subtitle .blockr-slot').map((w) => w.textContent), ['25', 'AEDECOD']);
  assert.equal(t.$$('.vz-script-band-row').length, 0, 'no "Top adverse events" row');
  t.$$('.vz-subtitle .blockr-slot')[0].click();
  const input = t.$('.vz-subtitle input.blockr-slot-input');
  assert.equal(input.getAttribute('aria-label'), 'Top adverse events');
  assert.equal(input.value, '25');
  input.value = '10';
  input.dispatchEvent(new t.win.KeyboardEvent('keydown', { key: 'Enter' }));
  assert.deepEqual(t.sets(), [{ script_values: { top_n: 10 } }], 'the script value, not the setting');
});
