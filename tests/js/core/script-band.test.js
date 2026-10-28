'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadCore } = require('./load');

/** The band on a fresh element, with a gear that records what is set. */
function band() {
  const env = loadCore();
  const doc = env.win.document;
  const parent = doc.body.appendChild(doc.createElement('div'));
  const sets = [];
  const gear = {
    values: () => ({ script_values: { keep: 3 } }),
    set: (v) => sets.push(JSON.parse(JSON.stringify(v)))
  };
  const scope = new env.win.Blockr.viz.Scope();
  const b = env.win.Blockr.viz.scriptBand(parent, scope, gear);
  return { env, doc, host: b.el, sets, b, scope };
}

const SCRIPT = {
  inputs: [
    { key: 'n', name: 'n', label: 'N', kind: 'number' },
    { key: 'bad', name: 'bad', label: 'Bad', kind: 'error', error: 'not a plain value' }
  ],
  values: { n: 5 }
};

test('the band is hidden until a script declares a value or fails', () => {
  const t = band();
  assert.equal(t.host.hidden, true);
  t.b.update(null);
  assert.equal(t.host.hidden, true);
  t.b.update(SCRIPT);
  assert.equal(t.host.hidden, false);
});

test('one row per declared value, the declaration that failed named', () => {
  const t = band();
  t.b.update(SCRIPT);
  const rows = t.host.querySelectorAll('.dd-band-row');
  assert.equal(rows.length, 2);
  assert.equal(rows[0].getAttribute('data-role'), 'n');
  assert.equal(rows[0].querySelector('.blockr-label').textContent, 'N');
  assert.equal(rows[0].querySelector('input').value, '5');
  assert.ok(rows[1].classList.contains('dd-script-input-error'));
  assert.match(rows[1].textContent, /not a plain value/);
});

test('an edit writes script_values by the variable name, keeping the others', () => {
  const t = band();
  t.b.update(SCRIPT);
  const inp = t.host.querySelector('[data-role="n"] input');
  inp.value = '8';
  inp.dispatchEvent(new t.env.win.KeyboardEvent('keydown', { key: 'Enter', bubbles: true }));
  assert.deepEqual(t.sets, [{ script_values: { keep: 3, n: 8 } }]);
});

test('a failed script says so; the same script again draws nothing new', () => {
  const t = band();
  t.b.update({ inputs: [], error: 'object x not found' });
  assert.equal(t.host.querySelector('.dd-script-failed').textContent,
               'Script failed: object x not found');
  const first = t.host.firstChild;
  t.b.update({ inputs: [], error: 'object x not found' });
  assert.equal(t.host.firstChild, first);
  t.b.update({ inputs: [] });
  assert.equal(t.host.children.length, 0);
  assert.equal(t.host.hidden, true);
});
