'use strict';

// Tags on one swimlane that would overlap split around the centre line
// (tags.js, tagStack() and stackTags()).

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadTable } = require('./load');

const tags = () => loadTable().win.Blockr.viz.table.tags;

test('tagStack: tags apart stay on the centre line', () => {
  assert.deepEqual(tags().tagStack([0, 40, 80], [14, 14, 14]), [0, 0, 0]);
});

test('tagStack: a later tag that overlaps goes above, the earlier one below', () => {
  // One day apart on a 900px lane: the second box starts inside the first.
  assert.deepEqual(tags().tagStack([100, 106], [14, 14]), [1, -1]);
  // The same day, the second in drawing order is the later one.
  assert.deepEqual(tags().tagStack([100, 100], [14, 14]), [1, -1]);
});

test('tagStack: order is by x, not by drawing order', () => {
  assert.deepEqual(tags().tagStack([106, 100], [14, 14]), [-1, 1]);
});

test('tagStack: the gap counts as overlap, a third tag goes down again', () => {
  assert.deepEqual(tags().tagStack([100, 115], [14, 14]), [1, -1]);
  assert.deepEqual(tags().tagStack([100, 117], [14, 14]), [0, 0]);
  assert.deepEqual(tags().tagStack([100, 105, 110], [14, 14, 14]), [1, -1, 1]);
  // Only the overlapping pair moves.
  assert.deepEqual(tags().tagStack([10, 100, 105], [14, 14, 14]), [0, 1, -1]);
});

test('stackTags: marks the overlapping pair on a drawn lane', () => {
  const env = loadTable();
  const doc = env.win.document;
  const host = doc.createElement('div');
  host.innerHTML =
    '<div class="vz-table-groups-ivcell">' +
    '<span class="vz-table-lane-ev is-tag" style="left:50%">AE</span>' +
    '<span class="vz-table-lane-ev is-tag is-inv" style="left:50.5%">WD</span>' +
    '<span class="vz-table-lane-ev is-tag" style="left:90%">AE</span></div>' +
    '<div class="vz-table-groups-ivcell">' +
    '<span class="vz-table-lane-ev is-tag" style="left:50%">AE</span>' +
    '<span class="vz-table-lane-ev is-tag is-inv" style="left:50.5%">WD</span></div>';
  doc.body.appendChild(host);
  const [a, b] = Array.from(host.querySelectorAll('.vz-table-groups-ivcell'));
  // happy-dom has no layout: the first lane is 900px wide, the second not
  // drawn yet (a closed panel).
  let wa = 900;
  Object.defineProperty(a, 'clientWidth', { get: () => wa });
  Object.defineProperty(b, 'clientWidth', { value: 0 });
  host.querySelectorAll('.is-tag').forEach((t) => Object.defineProperty(t, 'offsetWidth', { value: 14 }));
  env.win.Blockr.viz.table.tags.stackTags(host);
  const cls = (el) => Array.from(el.querySelectorAll('.is-tag'))
    .map((t) => (t.classList.contains('is-lo') ? 'lo' : t.classList.contains('is-hi') ? 'hi' : '-'));
  assert.deepEqual(cls(a), ['lo', 'hi', '-']);
  assert.deepEqual(cls(b), ['-', '-']);

  // Wider, the pair no longer overlaps and goes back on the centre line.
  wa = 9000;
  env.win.Blockr.viz.table.tags.stackTags(host);
  assert.deepEqual(cls(a), ['-', '-', '-']);
});
