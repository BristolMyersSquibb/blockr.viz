/* The chart's header (layout E): titles, download, gear and tray in one
 * element, placed by chart.css on the vz-head--open state. Closed, one row:
 * titles, tools, gear. Open, the gear on top, the tray, then titles and
 * tools. The DOM order stays the same in both states. */
'use strict';

const { test } = require('node:test');
const assert = require('node:assert/strict');
const D = require('./chart/drive');

const cls = (el) => el.className.split(/\s+/)[0];
const BAR = { type: 'bar', y: 'AETERM', orientation: 'horizontal' };
const TITLES = { titles: { title: { text: 'A title' }, subtitle: { text: 'A subtitle' } } };
const tick = () => new Promise((r) => setImmediate(r));

test('chart header: titles, tools, gear and tray, in that order', async () => {
  const c = D.open(BAR, { derived: TITLES });
  try {
    const head = c.block.frame.head;
    assert.ok(head.classList.contains('vz-head'));
    assert.deepEqual(Array.from(head.children).map(cls),
      ['vz-head__titles', 'vz-head__tools', 'blockr-gear-btn', 'blockr-settings']);
    assert.equal(head.querySelector('.vz-title').textContent, 'A title');
    assert.equal(head.querySelector('.vz-subtitle').textContent, 'A subtitle');
    assert.ok(head.querySelector('.vz-head__tools > .vz-download'), 'the download is a tool');
  } finally {
    await c.env.close();
  }
});

test('chart header: the gear sets vz-head--open, and a new payload keeps it', async () => {
  const c = D.open(BAR, { derived: TITLES });
  try {
    const open = () => c.block.frame.head.classList.contains('vz-head--open');
    assert.equal(open(), false);
    c.block.frame.tray.button.click();
    await tick();
    assert.ok(open(), 'open');
    assert.ok(c.block.frame.tray.band.classList.contains('blockr-settings--open'));
    assert.equal(c.block.frame.tray.band.parentNode, c.block.frame.head, 'the tray is in the header');
    c.send({ ...c.config, color: 'AESEV' });
    await tick();
    assert.ok(open(), 'a payload while open keeps the open layout');
    c.block.frame.tray.button.click();
    await tick();
    assert.equal(open(), false, 'closed');
    assert.equal(c.block.frame.tray.band.classList.contains('blockr-settings--open'), false);
  } finally {
    await c.env.close();
  }
});
