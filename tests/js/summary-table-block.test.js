/* The Table 1 widget's Statistics control (inst/js/summary-table-block.js):
 * a multi-select whose tag order is the row order, mapped to stat keys.
 */
'use strict';

const { test } = require('node:test');
const assert = require('node:assert/strict');
const { Window } = require('happy-dom');
const h = require('./harness.js');

function mount() {
  const win = new Window({ url: 'http://localhost/' });
  win.eval(h.PRELUDE);
  win.document.body.innerHTML =
    '<div id="stb" class="summary-table-block-container"></div>';
  win.eval(h.iconsScript());
  for (const f of [...h.UI_FILES, 'summary-table-block.js']) h.run(win, f);
  const doc = win.document;
  const el = doc.getElementById('stb');
  win.__binding.initialize(el);
  let sent = 0;
  win.__binding.subscribe(el, () => { sent++; });
  const wrap = () => Array.from(doc.querySelectorAll('.blockr-settings__field'))
    .find((w) => w.querySelector('.blockr-label')?.textContent === 'Statistics');
  const env = {
    win, doc, el, wrap,
    sent: () => sent,
    flush() {
      for (let round = 0; round < 10 && win.__timers.length; round++) {
        const due = win.__timers.splice(0).sort((a, b) => a.ms - b.ms);
        for (const t of due) t.fn();
      }
    },
    tags: () => Array.from(wrap().querySelectorAll('.blockr-select__tag'),
      (t) => t.getAttribute('data-value')),
    options() {
      const sel = wrap().querySelector('.blockr-select');
      sel.querySelector('.blockr-select__control').click();
      const search = sel.querySelector('.blockr-select__search');
      const lb = doc.getElementById(search.getAttribute('aria-controls'));
      return Array.from(lb.querySelectorAll('.blockr-select__option'));
    },
    pick(name) {
      const o = env.options().find((x) => x.getAttribute('data-value') === name);
      assert.ok(o, 'no option ' + name);
      o.click();
    },
    remove(name) {
      const tag = Array.from(wrap().querySelectorAll('.blockr-select__tag'))
        .find((t) => t.getAttribute('data-value') === name);
      tag.querySelector('.blockr-select__tag-remove').click();
    },
    stats: () => Array.from(el._block.getValue().stats),
    close() { win.happyDOM.abort(); return win.happyDOM.close(); }
  };
  return env;
}

test('statistics: the options are the display names, without keys', async () => {
  const env = mount();
  assert.deepEqual(env.tags(), ['Mean (SD)']);
  assert.deepEqual(env.options().map((o) => o.textContent.trim()),
    ['N', 'n (%)', 'Mean', 'SD', 'Mean (SD)', 'Median', 'Median (Q1, Q3)',
      'Q1, Q3', 'Min, Max']);
  assert.equal(env.wrap().querySelector('.blockr-select__search--visible'), null);
  await env.close();
});

test('statistics: the stored order is the pick order', async () => {
  const env = mount();
  env.pick('Min, Max');
  env.pick('N');
  env.flush();
  assert.deepEqual(env.stats(), ['mean_sd', 'min_max', 'n']);
  env.remove('Mean (SD)');
  env.flush();
  assert.deepEqual(env.stats(), ['min_max', 'n']);
  await env.close();
});

test('statistics: an empty pick shows the cue and sends nothing', async () => {
  const env = mount();
  env.pick('N');
  env.flush();
  const before = env.sent();
  env.remove('Mean (SD)');
  env.remove('N');
  env.flush();
  assert.equal(env.sent(), before + 1, 'only the first removal is sent');
  assert.deepEqual(env.stats(), ['n']);
  assert.ok(env.wrap().classList.contains('blockr-field--required-empty'));
  env.pick('SD');
  env.flush();
  assert.ok(!env.wrap().classList.contains('blockr-field--required-empty'));
  assert.deepEqual(env.stats(), ['sd']);
  await env.close();
});

test('statistics: a restored state keeps its order', async () => {
  const env = mount();
  env.el._block.setState({ stats: ['q1_q3', 'n', 'q1_q3', 'nonsense'] });
  assert.deepEqual(env.tags(), ['Q1, Q3', 'N']);
  env.el._block.setState({ stats: 'compact' });
  assert.deepEqual(env.tags(), ['Mean (SD)']);
  await env.close();
});
