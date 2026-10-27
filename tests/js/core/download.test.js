'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadCore } = require('./load');

const FORMATS = [
  { id: 'b-dl_xlsx', label: 'Excel', ext: 'xlsx' },
  { id: 'b-dl_png', label: 'Image', ext: 'png' }
];

const setup = () => {
  const env = loadCore();
  const host = env.win.document.createElement('div');
  env.win.document.body.appendChild(host);
  return Object.assign(env, { host, scope: new env.V.Scope() });
};

test('several formats are a menu of download links', () => {
  const { V, host, scope, bound } = setup();
  V.downloads(host, FORMATS, scope);
  const trigger = host.querySelector('.blockr-action-menu__trigger');
  assert.ok(trigger);
  const links = Array.from(host.querySelectorAll('a.shiny-download-link'));
  assert.deepEqual(links.map((a) => a.id), ['b-dl_xlsx', 'b-dl_png']);
  assert.deepEqual(links.map((a) => a.textContent), ['Excel.xlsx', 'Image.png']);
  assert.ok(host.querySelector('.blockr-menu').hasAttribute('hidden'));
  assert.deepEqual(bound.map((b) => b[0]), ['unbind', 'bind']);
});

test('one format is a tool that downloads', () => {
  const { V, host, scope } = setup();
  V.downloads(host, [FORMATS[0]], scope);
  const a = host.querySelector('a.shiny-download-link.blockr-tool');
  assert.equal(a.id, 'b-dl_xlsx');
  assert.equal(a.getAttribute('aria-label'), 'Download as Excel (.xlsx)');
  assert.equal(host.querySelector('.blockr-action-menu'), null);
});

test('labels are escaped', () => {
  const { V, host, scope } = setup();
  V.downloads(host, [{ id: 'x', label: '<b>Excel</b>', ext: 'xlsx' },
                     FORMATS[1]], scope);
  assert.equal(host.querySelector('b'), null);
  assert.match(host.innerHTML, /&lt;b&gt;Excel/);
});

test('the same list draws nothing; another redraws; none clears', () => {
  const { V, host, scope, bound } = setup();
  V.downloads(host, FORMATS, scope);
  const first = host.firstElementChild;
  V.downloads(host, FORMATS.map((f) => Object.assign({}, f)), scope);
  assert.equal(host.firstElementChild, first);
  V.downloads(host, [FORMATS[0]], scope);
  assert.notEqual(host.firstElementChild, first);
  V.downloads(host, [], scope);
  assert.equal(host.childNodes.length, 0);
  assert.equal(bound.filter((b) => b[0] === 'bind').length, 2);
});

test('reaching for the menu runs onOpen', () => {
  const { V, host, scope, win } = setup();
  let opened = 0;
  V.downloads(host, FORMATS, scope, { onOpen: () => { opened += 1; } });
  const trigger = host.querySelector('.blockr-action-menu__trigger');
  trigger.dispatchEvent(new win.Event('pointerenter'));
  trigger.dispatchEvent(new win.Event('click'));
  assert.equal(opened, 2);
  V.downloads(host, [FORMATS[0]], scope, { onOpen: () => { opened += 10; } });
  trigger.dispatchEvent(new win.Event('click'));
  assert.equal(opened, 2);
});

test('the control goes with its scope, unbound', () => {
  const { V, host, scope, bound, win } = setup();
  let opened = 0;
  V.downloads(host, FORMATS, scope, { onOpen: () => { opened += 1; } });
  const trigger = host.querySelector('.blockr-action-menu__trigger');
  V.downloads(host, [FORMATS[0]], scope);
  V.downloads(host, FORMATS, scope);
  const size = scope.size();
  scope.dispose();
  assert.equal(host.childNodes.length, 0);
  assert.equal(bound.at(-1)[0], 'unbind');
  trigger.dispatchEvent(new win.Event('click'));
  assert.equal(opened, 0);
  assert.ok(size <= 1 + 0, 'one cleanup per host, listeners only with onOpen');
});
