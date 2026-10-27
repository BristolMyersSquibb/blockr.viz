'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadCore } = require('./load');

const PIC = { png: 'data:image/png;base64,AA', width: 600, height: 400 };
const hosts = (win) => win.document.querySelectorAll('.blockr-capture-host').length;

test('a picture is drawn in an off-screen host that then goes', async () => {
  const { V, win } = loadCore();
  let seen = null;
  const pic = await V.capture({ width: 600, height: 400, draw: (host) => {
    seen = { width: host.style.width, left: host.style.left, n: hosts(win) };
    return PIC;
  } });
  assert.deepEqual(pic, PIC);
  assert.deepEqual(seen, { width: '600px', left: '-20000px', n: 1 });
  assert.equal(hosts(win), 0);
});

test('the host goes when drawing fails', async () => {
  const { V, win } = loadCore();
  await assert.rejects(
    V.capture({ width: 10, height: 10, draw: () => { throw new Error('no data'); } }),
    /no data/
  );
  assert.equal(hosts(win), 0);
});

test('a picture that never comes times out, and the host goes', async () => {
  const { V, win } = loadCore();
  const open = V.Scope.open();
  await assert.rejects(
    V.capture({ width: 10, height: 10, timeout: 10, draw: () => new Promise(() => {}) }),
    /did not draw in time/
  );
  assert.equal(hosts(win), 0);
  assert.equal(V.Scope.open(), open);
});

test('every request gets exactly one reply', async () => {
  const { V, sent } = loadCore();
  await V.captureReply('t1', Promise.resolve(PIC));
  await V.captureReply('t2', Promise.reject(new Error('blank')));
  // The values were built in the window's realm; compare them as JSON.
  assert.deepEqual(JSON.parse(JSON.stringify(sent.map((s) => [s.name, s.value]))), [
    ['blockr_viz_capture_result', { req: 't1', ...PIC }],
    ['blockr_viz_capture_result', { req: 't2', error: 'blank' }]
  ]);
  assert.ok(sent.every((s) => s.opts.priority === 'event'));
});
