'use strict';

const { test } = require('node:test');
const assert = require('node:assert/strict');
const { Window } = require('happy-dom');
const h = require('./harness.js');

const FILES = [...h.UI_FILES, 'drilldown-agg.js', 'drilldown-config.js',
  'busy-cue.js', 'table.js'];

const ONE_ROW_HEAD = `
          <tr>
            <th class="blockr-stub-header">Row</th>
            <th class="blockr-col-header leaf blockr-sortable" data-col-index="1">n</th>
          </tr>`;

function open(head = ONE_ROW_HEAD) {
  const win = new Window({ url: 'http://localhost/' });
  win.eval(h.PRELUDE);
  win.document.body.innerHTML = `
    <div class="drilldown-table-container" data-dt-gear="0" data-dt-structured="1">
      <table class="blockr-table">
        <thead>${head}
        </thead>
        <tbody>
          <tr class="blockr-data-row blockr-indent-toggle" data-indent="0">
            <td class="blockr-stub blockr-has-toggle">
              <button class="blockr-indent-btn" type="button" aria-expanded="true">
                <svg class="blockr-chev" viewBox="0 0 24 24"></svg>
              </button>
              SOC A
            </td>
            <td class="blockr-data">10</td>
          </tr>
          <tr class="blockr-data-row" data-indent="1">
            <td class="blockr-stub">PT A1</td>
            <td class="blockr-data">6</td>
          </tr>
          <tr class="blockr-data-row blockr-indent-toggle" data-indent="0">
            <td class="blockr-stub blockr-has-toggle">
              <button class="blockr-indent-btn" type="button" aria-expanded="true">
                <svg class="blockr-chev" viewBox="0 0 24 24"></svg>
              </button>
              SOC B
            </td>
            <td class="blockr-data">8</td>
          </tr>
          <tr class="blockr-data-row" data-indent="1">
            <td class="blockr-stub">PT B1</td>
            <td class="blockr-data">5</td>
          </tr>
        </tbody>
      </table>
    </div>`;
  win.eval(h.iconsScript());
  for (const f of FILES) h.run(win, f);
  win.document.dispatchEvent(new win.Event('DOMContentLoaded'));
  return {
    win,
    doc: win.document,
    parents: () => Array.from(
      win.document.querySelectorAll('tr.blockr-indent-toggle')),
    children: () => Array.from(
      win.document.querySelectorAll('tr.blockr-data-row:not(.blockr-indent-toggle)')),
    close() { win.happyDOM.abort(); return win.happyDOM.close(); }
  };
}

const click = (el) => el.dispatchEvent(new el.ownerDocument.defaultView.MouseEvent(
  'click', { bubbles: true, cancelable: true }));

test('the fold-all control works for indent-only grouped tables', async () => {
  const t = open();
  const all = t.doc.querySelector('.dt-foldall');

  assert.ok(all, 'fold-all button was created');
  assert.equal(all.getAttribute('aria-expanded'), 'true');
  assert.deepEqual(t.parents().map((r) => r.classList.contains('collapsed')),
    [false, false]);

  click(all);
  assert.equal(all.getAttribute('aria-expanded'), 'false');
  assert.deepEqual(t.parents().map((r) => r.classList.contains('collapsed')),
    [true, true]);
  assert.deepEqual(t.children().map((r) =>
    r.classList.contains('blockr-hidden-collapse')), [true, true]);

  click(all);
  assert.equal(all.getAttribute('aria-expanded'), 'true');
  assert.deepEqual(t.parents().map((r) => r.classList.contains('collapsed')),
    [false, false]);
  assert.deepEqual(t.children().map((r) =>
    r.classList.contains('blockr-hidden-collapse')), [false, false]);

  await t.close();
});

test('a titled multi-row stub header gets fold-all beside its top title', async () => {
  const t = open(`
          <tr>
            <th class="blockr-stub-header">Primary System Organ Class</th>
            <th class="blockr-col-header">Group A</th>
          </tr>
          <tr>
            <th class="blockr-stub-header">Dictionary-Derived Term</th>
            <th class="blockr-col-header leaf blockr-sortable" data-col-index="1">n</th>
          </tr>`);
  const ths = t.doc.querySelectorAll('thead th.blockr-stub-header');
  const all = t.doc.querySelectorAll('.dt-foldall');

  assert.equal(all.length, 1);
  assert.ok(ths[0].contains(all[0]), 'button sits in the top stub header');
  assert.equal(ths[0].querySelector('.dt-stub-title').textContent,
    'Primary System Organ Class');
  assert.equal(ths[0].querySelector('.dt-foldall-all'), null);
  assert.equal(ths[1].textContent, 'Dictionary-Derived Term');

  click(all[0]);
  assert.deepEqual(t.parents().map((r) => r.classList.contains('collapsed')),
    [true, true]);

  await t.close();
});
