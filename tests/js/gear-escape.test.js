/* Escape closes the gear tray of the chart, table and summarize table,
 * as it does the heatmap's (Blockr.gearTray): pressed in the band or on the
 * gear, it closes the band and focuses the gear. */
'use strict';

const { test } = require('node:test');
const assert = require('node:assert/strict');
const { Window } = require('happy-dom');
const h = require('./harness.js');
const D = require('./chart/drive');
const fixtures = require('./fixtures/summarize-table.json');

const SHARED = [...h.UI_FILES, 'drilldown-agg.js', 'drilldown-config.js',
  'capture-pages.js', 'busy-cue.js'];

function mount(html, files) {
  const win = new Window({ url: 'http://localhost/' });
  win.eval(h.PRELUDE);
  win.document.body.innerHTML = html;
  win.eval(h.iconsScript());
  for (const f of [...SHARED, ...files]) h.run(win, f);
  win.document.dispatchEvent(new win.Event('DOMContentLoaded'));
  return win;
}

const click = (el) => el.dispatchEvent(new el.ownerDocument.defaultView.MouseEvent(
  'click', { bubbles: true, cancelable: true }));
const escape = (el) => el.dispatchEvent(new el.ownerDocument.defaultView.KeyboardEvent(
  'keydown', { key: 'Escape', bubbles: true, cancelable: true }));

/** Open with the gear, close with Escape on `where(band, gear)`, then check
 *  that the gear still toggles and its close drops the layer. Focus on the
 *  gear can open its tooltip, a layer too, so counts are taken around the
 *  gear's own close only. */
function check(win, band, gear, where) {
  const isOpen = () => band.classList.contains('blockr-settings--open');
  const before = win.Blockr.layer.count();
  click(gear);
  assert.ok(isOpen(), 'the gear opens the band');
  assert.equal(win.Blockr.layer.count(), before + 1, 'the open band is a layer');
  escape(where(band, gear));
  assert.equal(isOpen(), false, 'Escape closes the band');
  if (gear.hasAttribute('aria-expanded')) {
    assert.equal(gear.getAttribute('aria-expanded'), 'false');
  }
  assert.equal(win.document.activeElement, gear, 'focus is on the gear');
  click(gear);
  assert.ok(isOpen(), 'the gear opens it again');
  const open = win.Blockr.layer.count();
  click(gear);
  assert.equal(isOpen(), false);
  assert.equal(win.Blockr.layer.count(), open - 1, 'a gear close drops the layer');
}

const onGear = (band, gear) => gear;
const inBand = (band) => band;

test('Escape closes the chart gear tray', async () => {
  const c = D.open({ type: 'bar', y: 'AETERM', orientation: 'horizontal' });
  try {
    check(c.env.win, c.block.popoverEl, c.block.gearBtn, onGear);
    check(c.env.win, c.block.popoverEl, c.block.gearBtn, inBand);
    assert.equal(c.block._tray.isOpen(), false);
  } finally {
    await c.env.close();
  }
});

const TABLE = `
  <div class="drilldown-table-container" data-dt-elem-id="tbl">
    <table class="blockr-table">
      <thead><tr><th class="blockr-col-header leaf" data-col-index="0">n</th></tr></thead>
      <tbody><tr class="blockr-data-row"><td class="blockr-data">1</td></tr></tbody>
    </table>
  </div>`;

test('Escape closes the table gear tray', async () => {
  const win = mount(TABLE, ['table.js']);
  const band = win.document.querySelector('.blockr-settings.dd-popover');
  const gear = win.document.querySelector('.blockr-gear-btn');
  assert.ok(band && gear, 'the table has a gear');
  check(win, band, gear, onGear);
  check(win, band, gear, inBand);
  win.happyDOM.abort();
  await win.happyDOM.close();
});

test('Escape closes the summarize table gear tray and resets its band', async () => {
  const c = fixtures.flat;
  const win = mount(c.chrome, ['table.js', 'summarize-table.js']);
  const p = c.payloads[0];
  win.__handlers['blockr-viz-summarize-data']({ id: c.id, rev: p.rev, payload: p.payload });
  const root = win.document.querySelector('[data-summarize-elem-id]');
  const band = win.document.querySelector('.blockr-settings.dd-popover');
  const gear = win.document.querySelector('.blockr-gear-btn');
  let resets = 0;
  const reset = root._summarizeBand;
  root._summarizeBand = function () { resets++; return reset.apply(this, arguments); };
  check(win, band, gear, onGear);
  assert.equal(resets, 2, 'Escape and the gear both reset the band');
  check(win, band, gear, inBand);
  assert.equal(resets, 4);
  win.happyDOM.abort();
  await win.happyDOM.close();
});

test('Escape closes the summary table gear tray', async () => {
  const win = mount('<div class="summary-table-block-container" id="stb"></div>',
    ['summary-table-block.js']);
  const host = win.document.getElementById('stb');
  win.__binding.initialize(host);
  const band = host.querySelector('.blockr-settings');
  const gear = host.querySelector('.blockr-gear-btn');
  assert.ok(band && gear, 'the summary table has a gear');
  check(win, band, gear, onGear);
  check(win, band, gear, inBand);
  win.happyDOM.abort();
  await win.happyDOM.close();
});
