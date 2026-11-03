/* Blockr.trayNav as blockr.viz carries it (inst/js/core/gear/tray-nav.js),
 * until blockr.ui ships it. The test is blockr.ui's, from feat/tray-nav. */
'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const { Window } = require('happy-dom');

const FILE = path.join(__dirname, '..', '..', '..', 'inst', 'js', 'core', 'gear', 'tray-nav.js');
const SRC = fs.readFileSync(FILE, 'utf8') + '\n//# sourceURL=file://' + FILE;

const windows = [];
test.after(async () => {
  for (const w of windows) {
    w.happyDOM.abort();
    await w.happyDOM.close();
  }
});

/** A window with the script loaded, after `before` has run on it. */
function load(before) {
  const win = new Window();
  windows.push(win);
  if (before) before(win);
  win.eval(SRC);
  return win;
}

const navSection = (win, title) => {
  const el = win.document.createElement('div');
  el.className = 'vz-section';
  const t = win.document.createElement('div');
  t.className = 'vz-section-title';
  t.textContent = title;
  el.appendChild(t);
  return { el, title, titleEl: t };
};

test('trayNav: one group shows, the index lists its sections, tabs follow', () => {
  const win = load();
  const s = ['Rows', 'Columns', 'Order', 'Titles'].map((t) => navSection(win, t));
  const picks = [];
  const nav = win.Blockr.trayNav([
    { key: 'data', title: 'Data', sections: [s[0], s[1]] },
    { key: 'sort', title: 'Sort', sections: [s[2]] },
    { key: 'empty', title: 'Empty', sections: [] },
    { key: 'titles', title: 'Titles', sections: [s[3]] }
  ], { current: 'sort', onPick: (k) => picks.push(k) });
  win.document.body.appendChild(nav.el);
  const shown = () => s.filter((x) => !x.el.classList.contains('blockr-tray-nav__off')).map((x) => x.title);
  const tabs = Array.from(nav.el.querySelectorAll('.blockr-tray-nav__tab'));

  assert.deepEqual(tabs.map((t) => t.textContent), ['Data', 'Sort', 'Titles'], 'an empty group leaves');
  assert.equal(nav.current(), 'sort');
  assert.deepEqual(shown(), ['Order']);
  assert.ok(s[2].titleEl.classList.contains('blockr-tray-nav__hidden-title'),
    'a group of one section shows no section title');
  assert.ok(!s[0].titleEl.classList.contains('blockr-tray-nav__hidden-title'));

  // The index: groups, and the open group's sections under it.
  const index = () => Array.from(nav.el.querySelectorAll('.blockr-tray-nav__index button'))
    .map((b) => (b.classList.contains('blockr-tray-nav__section') ? '  ' : '') +
      (b.classList.contains('is-current') ? '*' : '') + b.textContent);
  assert.deepEqual(index(), ['Data', '*Sort', 'Titles']);
  nav.el.querySelector('.blockr-tray-nav__group').click();
  assert.deepEqual(index(), ['*Data', '  Rows', '  Columns', 'Sort', 'Titles']);
  assert.deepEqual(shown(), ['Rows', 'Columns']);
  assert.deepEqual(picks, ['data']);
  assert.equal(tabs[0].getAttribute('aria-selected'), 'true');
  assert.equal(nav.el.querySelector('[role=tabpanel]').getAttribute('aria-labelledby'), tabs[0].id);

  // A section in the index lights up; the light goes once played.
  Array.from(nav.el.querySelectorAll('.blockr-tray-nav__section'))[1].click();
  assert.ok(s[1].el.classList.contains('blockr-tray-nav__flash'));
  s[1].el.dispatchEvent(new win.Event('animationend'));
  assert.ok(!s[1].el.classList.contains('blockr-tray-nav__flash'));

  // Arrow keys move along the tabs, wrapping; Home and End go to the ends.
  const key = (k) => tabs[0].dispatchEvent(new win.KeyboardEvent('keydown', { key: k, bubbles: true }));
  key('ArrowLeft');
  assert.equal(nav.current(), 'titles');
  assert.deepEqual(shown(), ['Titles']);
  key('ArrowRight');
  assert.equal(nav.current(), 'data');
  key('End');
  assert.equal(nav.current(), 'titles');
  key('Home');
  assert.equal(nav.current(), 'data');
  key('x');
  assert.equal(nav.current(), 'data', 'other keys do nothing');

  // show() picks a group; a group that is open already stays quiet.
  nav.show('sort');
  nav.show('sort');
  assert.deepEqual(shown(), ['Order']);
  assert.deepEqual(picks, ['data', 'titles', 'data', 'titles', 'data', 'sort']);
});

test('trayNav: an unknown current opens the first group, and nothing is fine too', () => {
  const win = load();
  const s = navSection(win, 'Rows');
  const nav = win.Blockr.trayNav([{ key: 'data', title: 'Data', sections: [s] }], { current: 'nope' });
  assert.equal(nav.current(), 'data');
  assert.equal(nav.el.querySelector('nav').getAttribute('aria-label'), 'Settings groups');
  assert.equal(win.Blockr.trayNav([]).current(), '');
});

test('trayNav: a blockr.ui that has one keeps its own', () => {
  const own = () => 'ui';
  const win = load((w) => { w.Blockr = { trayNav: own }; });
  assert.equal(win.Blockr.trayNav, own);
});
