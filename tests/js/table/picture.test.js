/**
 * The groups table's picture fetches SnapDOM the first time it is taken
 * (table/groups.js snapdom(), core/lazy.js). happy-dom fires neither load
 * nor error for a script tag, so the tags are driven by hand, and the
 * package folder the core would have been served from is set by hand.
 */
const test = require('node:test');
const assert = require('node:assert/strict');
const { loadTable, payload, tick } = require('./load');

const G = { layout: 'groups', by: ['ARM'] };
const BASE = 'http://app/blockr-viz-core-1/';

/** A three-group model: one label column and a count. */
function groups() {
  const arms = ['Placebo', 'Low', 'High'];
  return {
    n: 3, cols: [
      { name: 'ARM', label: true, disp: arms, claim: arms.map((a) => ({ ARM: [a] })) },
      { name: 'Rows', disp: ['10', '20', '30'] }
    ],
    head: [[{ label: 'ARM', span: 1 }, { label: 'Rows', span: 1 }]]
  };
}

// The script tags the picture adds, kept off the page: happy-dom would
// fetch the URL and fail it.
const served = () => {
  const env = loadTable();
  env.V.lazy.base = BASE;
  const head = env.win.document.head;
  const add = head.appendChild.bind(head);
  env.tags = [];
  head.appendChild = (el) => (el.tagName === 'SCRIPT' ? (env.tags.push(el), el) : add(el));
  // A failed load takes its tag off the page.
  const remove = env.win.HTMLElement.prototype.remove;
  env.win.HTMLScriptElement.prototype.remove = function () {
    env.tags = env.tags.filter((t) => t !== this);
    return remove.call(this);
  };
  return env;
};

const snapTags = (env) => env.tags.filter((el) => el.src === BASE + 'js/vendor/snapdom.js');

test('the first picture fetches snapdom once and waits for it', async () => {
  const env = served();
  const p = payload({ data: groups(), config: G });
  const first = env.V.table.picture(p, 500);
  const second = env.V.table.picture(p, 500);
  await tick(env.win, 0);
  const tags = snapTags(env);
  assert.equal(tags.length, 1, 'both pictures wait for the same script');
  env.win.snapdom = { toCanvas: () => Promise.resolve({ toDataURL: () => 'data:image/png;base64,CCC' }) };
  tags[0].onload();
  assert.equal((await first).png, 'data:image/png;base64,CCC');
  assert.equal((await second).png, 'data:image/png;base64,CCC');
  await env.V.table.picture(p, 500);
  assert.equal(snapTags(env).length, 1, 'a loaded snapdom is not fetched again');
});

test('a snapdom that fails to load fails the picture; the next one tries again', async () => {
  const env = served();
  const p = payload({ data: groups(), config: G });
  const first = env.V.table.picture(p, 500);
  await tick(env.win, 0);
  snapTags(env)[0].onerror();
  await assert.rejects(first, /snapdom/);
  assert.equal(snapTags(env).length, 0);
  const again = env.V.table.picture(p, 500);
  await tick(env.win, 0);
  const tags = snapTags(env);
  assert.equal(tags.length, 1);
  // A script that runs but defines no snapdom fails the picture too.
  tags[0].onload();
  await assert.rejects(again, /snapdom is not loaded/);
});

test('with no package folder the picture fails at once and fetches nothing', async () => {
  const env = loadTable();
  assert.equal(env.V.lazy.base, '');
  await assert.rejects(env.V.table.picture(payload({ data: groups(), config: G }), 500),
                       /snapdom is not loaded/);
  assert.equal(env.win.document.head.querySelectorAll('script').length, 0);
});
