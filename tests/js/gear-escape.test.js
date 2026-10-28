/* Escape closes the gear tray of the chart and the table (Blockr.gearTray):
 * pressed in the band or on the gear, it closes the band and focuses the
 * gear. */
'use strict';

const { test } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const { Window } = require('happy-dom');
const h = require('./harness.js');
const D = require('./chart/drive');

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
  const c = D.open({ type: 'bar', category: 'AETERM', orientation: 'horizontal' });
  try {
    check(c.env.win, c.block.frame.tray.band, c.block.frame.tray.button, onGear);
    check(c.env.win, c.block.frame.tray.band, c.block.frame.tray.button, inBand);
    assert.equal(c.block.frame.tray.isOpen(), false);
  } finally {
    await c.env.close();
  }
});

const listed = (dir) => fs.readFileSync(path.join(h.JS_DIR, dir, 'scripts.txt'), 'utf8')
  .split('\n').map((l) => l.trim()).filter((l) => l && !l.startsWith('#'))
  .map((f) => dir + '/' + f);

test('Escape closes the table gear tray', async () => {
  const win = new Window({ url: 'http://localhost/' });
  win.eval(h.PRELUDE);
  // Reduced motion: the tray opens and closes without its animation.
  win.matchMedia = () => ({ matches: true, addEventListener() {}, removeEventListener() {} });
  win.eval(h.iconsScript());
  for (const f of [...h.UI_FILES, ...listed('core'), 'spec/table.js', ...listed('table')]) {
    h.run(win, f);
  }
  win.document.dispatchEvent(new win.Event('DOMContentLoaded'));
  const el = win.document.body.appendChild(win.document.createElement('div'));
  el.id = 'tbl';
  const V = win.Blockr.viz;
  const view = new V.table.View(el, new V.Scope());
  view.render({ state_rev: 1, data_rev: 1, columns: [], derived: {},
                config: { layout: 'records', search: true, download: false },
                data: { n: 1, stub: 0, cols: [{ name: 'n', disp: ['1'] }],
                        head: [[{ text: 'n', stub: true, col: 0 }]] } });
  const band = el.querySelector('.blockr-settings.vz-tray');
  const gear = el.querySelector('.blockr-gear-btn');
  assert.ok(band && gear, 'the table has a gear');
  check(win, band, gear, onGear);
  check(win, band, gear, inBand);
  win.happyDOM.abort();
  await win.happyDOM.close();
});

/* Escape in a title field: Blockr.layer takes Escape before the field's own
 * keydown, so the field is a layer while it has the focus. Escape reverts
 * the draft and leaves the tray open; with the `@` menu open, the first
 * Escape closes the menu and the second reverts. Nothing is sent. */
test('Escape in a title field reverts it and keeps the tray open', async () => {
  const c = D.open({ type: 'bar', y: 'AETERM', orientation: 'horizontal', title: 'Adverse events' });
  try {
    const win = c.env.win;
    const { band, button } = c.block.frame.tray;
    click(button);
    const isOpen = () => band.classList.contains('blockr-settings--open');
    assert.ok(isOpen());
    const inp = band.querySelector('input[data-gear-input="title"]');
    assert.ok(inp, 'the tray has the title field');
    const type = (text) => {
      for (const ch of text) {
        inp.value += ch;
        inp.setSelectionRange(inp.value.length, inp.value.length);
        inp.dispatchEvent(new win.InputEvent('input', { data: ch }));
      }
    };
    const layers = win.Blockr.layer.count();

    inp.focus();
    assert.equal(win.Blockr.layer.count(), layers + 1, 'the focused field is a layer');
    type(' draft');
    escape(inp);
    assert.ok(isOpen(), 'the tray stays open');
    assert.equal(inp.value, 'Adverse events', 'the draft is reverted');
    assert.notEqual(win.document.activeElement, inp, 'the field lets go of the focus');
    assert.equal(win.Blockr.layer.count(), layers, 'the field is no layer once blurred');

    inp.focus();
    type(' by @');
    const menu = () => win.document.body.querySelector('.vz-at-menu');
    assert.ok(menu(), 'the @ menu is open');
    escape(inp);
    assert.equal(menu(), null, 'the first Escape closes the menu');
    assert.ok(isOpen());
    assert.equal(inp.value, 'Adverse events by @', 'the draft stays');
    escape(inp);
    assert.equal(inp.value, 'Adverse events', 'the second reverts it');
    assert.ok(isOpen(), 'and the tray is still open');

    escape(band);
    assert.equal(isOpen(), false, 'Escape outside a field still closes the tray');
    assert.deepEqual(c.sets(), [], 'nothing was sent');
  } finally {
    await c.env.close();
  }
});
