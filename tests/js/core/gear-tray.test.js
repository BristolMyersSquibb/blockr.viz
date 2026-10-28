'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadGear, mountGear, payload, lastSet, row, hasUi, hasTrayNav } = require('./gear-load');

const noUi = { skip: !hasUi() && 'blockr.ui not found (set BLOCKR_UI_JS)' };
const noNav = { skip: !hasTrayNav() && 'blockr.ui without trayNav (set BLOCKR_UI_JS to main)' };
const tick = (win, ms = 5) => new Promise((r) => win.setTimeout(r, ms));
const TITLES = { title: { parts: [{ text: 'ARM', arg: 'x' }] }, args: { x: 'ARM' } };

test('with Blockr.trayNav the groups are tabs, and the open one is kept', noNav, () => {
  const env = loadGear({ ui: true });
  const m = mountGear(env, { title: 'Chart settings' });
  m.gear.update(payload({ type: 'bar', x: 'ARM' }));
  const nav = m.el.querySelector('.blockr-tray-nav');
  assert.ok(nav);
  const tabs = Array.from(nav.querySelectorAll('.blockr-tray-nav__tab'));
  assert.deepEqual(tabs.map((t) => t.textContent), ['Data', 'Sort', 'Format', 'Titles', 'Interaction']);
  tabs[2].click();
  m.gear.update(payload({ type: 'bar', x: 'SEX' }));
  const cur = m.el.querySelector('.blockr-tray-nav__tab.is-current');
  assert.equal(cur.textContent, 'Format', 'a re-render keeps the group');
  // A scatter has no Sort group.
  m.gear.update(payload({ type: 'scatter', x: 'AGE', y: 'AVAL' }));
  const names = Array.from(m.el.querySelectorAll('.blockr-tray-nav__tab')).map((t) => t.textContent);
  assert.ok(!names.includes('Sort'));
});

test('with blockr.ui the controls are its widgets and still send', noUi, () => {
  const env = loadGear({ ui: true });
  const m = mountGear(env);
  m.gear.update(payload({ type: 'bar', x: 'ARM', color: 'SEX', position: 'stack' }));
  const x = row(m.el, 'x');
  assert.ok(x.querySelector('.blockr-select'), 'Blockr.Select');
  x.querySelector('.blockr-select__control').click();
  const opt = Array.from(env.doc.querySelectorAll('.blockr-select__option'))
    .find((o) => o.getAttribute('data-value') === 'AGE');
  assert.ok(opt);
  opt.click();
  assert.deepEqual(lastSet(env).values, { x: 'AGE' });
  const seg = row(m.el, 'position').querySelector('.blockr-segmented');
  seg.querySelectorAll('button')[2].click();
  assert.deepEqual(lastSet(env).values, { position: 'fill' });
  // A fixed option shows its label alone.
  const func = row(m.el, 'func');
  func.querySelector('.blockr-select__control').click();
  const texts = Array.from(env.doc.querySelectorAll('.blockr-select__option')).map((o) => o.textContent);
  assert.ok(texts.includes('Count distinct'));
  assert.ok(!texts.some((t) => t.includes('count_distinct')));
  m.scope.dispose();
});

/** Mount, use and drop a gear and its sentence `n` times. */
async function cycles(env, n) {
  for (let i = 0; i < n; i++) {
    const m = mountGear(env);
    m.gear.update(payload({ type: 'bar', x: 'ARM', title: 'T', color: 'SEX' },
      { derived: { titles: TITLES } }));
    m.gear.set({ func: 'mean' });
    const inp = row(m.el, 'title').querySelector('input');
    inp.focus();
    inp.value = 'T @';
    inp.setSelectionRange(3, 3);
    inp.dispatchEvent(new env.win.InputEvent('input', { data: '@' }));
    assert.ok(env.doc.querySelector('.vz-at-menu'));
    const head = env.doc.body.appendChild(env.doc.createElement('div'));
    const slots = env.V.titles.slots({ gear: m.gear, scope: m.scope });
    slots.paint(head, TITLES.title);
    head.querySelector('.blockr-slot').click();
    const sel = row(m.el, 'color').querySelector('.blockr-select__control');
    if (sel) sel.click();
    m.scope.dispose();
    head.remove();
    m.el.remove();
    await tick(env.win, 2);
  }
}

const baseline = (env) => ({
  listeners: env.listeners(), scopes: env.V.Scope.open(),
  nodes: env.doc.body.querySelectorAll('*').length,
  layers: env.win.Blockr.layer ? env.win.Blockr.layer.count() : 0
});

test('50 mount and unmount cycles leave nothing behind', async () => {
  const env = loadGear();
  const before = baseline(env);
  await cycles(env, 50);
  assert.deepEqual(baseline(env), before);
});

test('50 cycles with blockr.ui leave nothing behind', noUi, async () => {
  const env = loadGear({ ui: true });
  await cycles(env, 1);
  await tick(env.win, 10);
  const before = baseline(env);
  await cycles(env, 50);
  await tick(env.win, 10);
  assert.deepEqual(baseline(env), before);
});

test('Escape closes the gear tray of a chart or table view, and focus goes to the gear', noUi, () => {
  const env = loadGear({ ui: true });
  // happy-dom's animations reject on cancel; the tray opens without one.
  env.win.HTMLElement.prototype.animate = undefined;
  const head = env.doc.body.appendChild(env.doc.createElement('div'));
  const scope = new env.V.Scope();
  const t = env.V.gear.tray(head, scope, 'Table settings');
  const esc = (el) => el.dispatchEvent(new env.win.KeyboardEvent('keydown',
    { key: 'Escape', bubbles: true, cancelable: true }));
  for (const where of [t.band, t.button]) {
    t.button.click();
    assert.equal(t.isOpen(), true, 'the gear opens the tray');
    assert.ok(head.classList.contains('vz-head--open') || t.button.getAttribute('aria-expanded') === 'true');
    esc(where);
    assert.equal(t.isOpen(), false, 'Escape closes it');
    assert.equal(env.doc.activeElement, t.button);
  }
  scope.dispose();
});
