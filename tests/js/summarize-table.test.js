/* The summarize table block's browser side (inst/js/summarize-table.js), driven
 * the way the page drives it: the chrome the block's UI renders once, then
 * the payloads its server pushes. Both come from the block's own R code
 * (fixtures/make-summarize-fixtures.R), so a name the R side writes and the
 * JS side does not read fails here.
 */
'use strict';

const { test } = require('node:test');
const assert = require('node:assert/strict');
const { Window } = require('happy-dom');
const h = require('./harness.js');
const fixtures = require('./fixtures/summarize-table.json');

// summarize_table_dep(): blockr.ui's controls, the shared engine, table.js, then
// the block's own script.
const FILES = [...h.UI_FILES, 'drilldown-agg.js', 'drilldown-config.js',
  'capture-pages.js', 'busy-cue.js', 'table.js', 'summarize-table.js'];

/** Mount a case's chrome, load the scripts, and deliver its first payload
 *  unless `deliver` is false. */
function open(name, { deliver = true } = {}) {
  const c = fixtures[name];
  const win = new Window({ url: 'http://localhost/' });
  win.eval(h.PRELUDE);
  win.document.body.innerHTML = c.chrome;
  win.eval(h.iconsScript());
  for (const f of FILES) h.run(win, f);
  const doc = win.document;
  const root = doc.querySelector('[data-summarize-elem-id]');
  const env = {
    win, doc, root, id: c.id,
    /** Deliver payload `i` (0-based) of the case, as the message handler gets it. */
    push(i) {
      const p = c.payloads[i];
      win.__handlers['blockr-viz-summarize-data']({ id: c.id, rev: p.rev, payload: p.payload });
    },
    rows: () => Array.from(doc.querySelectorAll('tr.blockr-summarize-row')),
    labels: () => env.rows().map((r) => r.getAttribute('data-summarize-label')),
    shown: () => env.rows().filter((r) =>
      !r.classList.contains('blockr-summarize-hidden-search') &&
      !r.classList.contains('collapsed-hidden')),
    inputs: () => h.normalize(Array.from(win.__inputs)),
    actions: () => env.inputs().filter((x) => x.name === c.id + '_action').map((x) => x.value),
    flush() {
      for (let round = 0; round < 10 && win.__timers.length; round++) {
        const due = win.__timers.splice(0).sort((a, b) => a.ms - b.ms);
        for (const t of due) t.fn();
      }
    },
    band: () => doc.querySelector('.blockr-settings.dd-popover'),
    isOpen: () => env.band().classList.contains('blockr-settings--open'),
    search: () => doc.querySelector('input.blockr-search'),
    footer: () => doc.querySelector('.dd-status-footer').textContent,
    close() { win.happyDOM.abort(); return win.happyDOM.close(); }
  };
  if (deliver) env.push(0);
  return env;
}

const click = (el) => el.dispatchEvent(new el.ownerDocument.defaultView.MouseEvent(
  'click', { bubbles: true, cancelable: true }));

test('the payload draws the rows, the title and the escaped labels', async () => {
  const t = open('flat');
  assert.deepEqual(t.labels(), ['T <1> & co', 'T2', 'T3', 'T4']);
  assert.equal(t.doc.querySelector('.dd-table-title').textContent, 'Terms');
  const label = t.rows()[0].querySelector('.blockr-summarize-label');
  assert.equal(label.textContent, 'T <1> & co');
  assert.equal(label.children.length, 0);
  await t.close();
});

test('a container with no payload yet announces itself, and draws when it comes', async () => {
  const t = open('flat', { deliver: false });
  assert.deepEqual(t.inputs().map((x) => x.name), [t.id + '_ready']);
  assert.equal(t.rows().length, 0);
  t.push(0);
  assert.equal(t.rows().length, 4);
  await t.close();
});

test('search hides the rows that do not match, and an empty query shows them again', async () => {
  const t = open('flat');
  const box = t.search();
  box.value = 'T3';
  box.dispatchEvent(new t.win.Event('input', { bubbles: true }));
  t.flush();
  assert.deepEqual(t.shown().map((r) => r.getAttribute('data-summarize-label')), ['T3']);
  box.value = '';
  box.dispatchEvent(new t.win.Event('input', { bubbles: true }));
  t.flush();
  assert.equal(t.shown().length, 4);
  await t.close();
});

test('a header click sorts, and the third click returns to the server order', async () => {
  const t = open('ascending');
  assert.deepEqual(t.labels(), ['T4', 'T3', 'T2', 'T <1> & co']);
  const th = t.doc.querySelector('th.blockr-sortable[data-col-index="1"]');
  click(th);
  assert.deepEqual(t.labels(), ['T <1> & co', 'T2', 'T3', 'T4']);
  click(th);
  assert.deepEqual(t.labels(), ['T4', 'T3', 'T2', 'T <1> & co']);
  click(th);
  assert.deepEqual(t.labels(), ['T4', 'T3', 'T2', 'T <1> & co']);
  await t.close();
});

test('a parent row folds and unfolds its children', async () => {
  const t = open('nested');
  assert.deepEqual(t.shown().map((r) => r.getAttribute('data-summarize-label')), ['SOC A', 'SOC B']);
  const btn = t.rows()[0].querySelector('.blockr-indent-btn');
  click(btn);
  assert.deepEqual(t.shown().map((r) => r.getAttribute('data-summarize-label')),
    ['SOC A', 'T <1> & co', 'T2', 'SOC B']);
  click(t.rows()[0].querySelector('.blockr-indent-btn'));
  assert.deepEqual(t.shown().map((r) => r.getAttribute('data-summarize-label')), ['SOC A', 'SOC B']);
  await t.close();
});

test('a colour column fills the legend band', async () => {
  const t = open('color');
  const lg = t.doc.querySelector('.blockr-summarize-legend');
  assert.equal(lg.style.display, '');
  assert.equal(lg.querySelector('.blockr-summarize-legend-title').textContent, 'SEV');
  assert.deepEqual(
    Array.from(lg.querySelectorAll('.blockr-summarize-legend-item')).map((x) => x.textContent),
    ['MILD', 'MODERATE']);
  await t.close();
});

test('a pair tooltip opens only over the drawn mark', async () => {
  const t = open('flat', { deliver: false });
  const payload = {
    kind: 'flat',
    n: 1,
    label: ['Gastrointestinal disorders'],
    head: '<table class="blockr-table blockr-summarize-table"><tbody></tbody></table>',
    chrome: {},
    cols: [{
      kind: 'pair',
      multi: true,
      levels: ['Grade 1'],
      fills: ['#47978f'],
      rf: null,
      v: [40],
      tt: {
        cvar: 'Toxicity grade',
        words: { from: 'Mean start', to: 'Mean end' }
      },
      lv: [{
        kind: 'pair',
        fill: [null],
        dash: [false],
        tip: ['Grade 1 · n=4 · Mean start 20 · Mean end 60'],
        bw: [null],
        bl: [null],
        rf: null,
        w: [40],
        l: [20],
        a: [20],
        b: [60],
        open: [false],
        ab: ['+40']
      }]
    }]
  };
  t.win.__handlers['blockr-viz-summarize-data']({
    id: t.id,
    rev: 'pair-hover',
    payload: JSON.stringify(payload)
  });

  const move = (el) => el.dispatchEvent(new t.win.MouseEvent(
    'mousemove', { bubbles: true, clientX: 40, clientY: 40 }));
  const lane = t.doc.querySelector('.blockr-summarize-pacell');

  move(lane);
  assert.equal(t.doc.querySelector('.blockr-lane-tip').hidden, true);

  move(lane.querySelector('.lane-to'));
  const tip = t.doc.querySelector('.blockr-lane-tip');
  assert.equal(tip.hidden, false);
  assert.match(tip.textContent, /Grade 1/);
  await t.close();
});

test('a pair tooltip escapes its text once', async () => {
  const t = open('flat', { deliver: false });
  const payload = {
    kind: 'flat',
    n: 1,
    label: ['A'],
    head: '<table class="blockr-table blockr-summarize-table"><tbody></tbody></table>',
    chrome: {},
    cols: [{
      kind: 'pair', rf: null, v: [60], tt: {},
      fill: [null], dash: [false], bw: [null], bl: [null], w: [40], l: [20],
      a: [20], b: [60], open: [false],
      tip: ['Start <day> & "1" 20 → End 60']
    }]
  };
  t.win.__handlers['blockr-viz-summarize-data']({
    id: t.id, rev: 'pair-esc', payload: JSON.stringify(payload)
  });
  const lane = t.doc.querySelector('.blockr-summarize-pacell');
  assert.equal(lane.getAttribute('data-summarize-tip'),
               'Start <day> & "1" 20 → End 60');
  lane.querySelector('.lane-to').dispatchEvent(new t.win.MouseEvent(
    'mousemove', { bubbles: true, clientX: 40, clientY: 40 }));
  const tip = t.doc.querySelector('.blockr-lane-tip');
  assert.match(tip.textContent, /Start <day> & "1" 20/);
  await t.close();
});

test('a height makes the table scroll in its own box', async () => {
  const t = open('height');
  const w = t.doc.querySelector('.blockr-table-wrapper');
  assert.equal(w.style.maxHeight, '300px');
  assert.equal(w.classList.contains('dt-scroll-page'), false);
  await t.close();
});

test('a latched drill: a row click filters, a second click on it clears', async () => {
  const t = open('flat');
  const row = t.rows()[1];
  click(row);
  assert.deepEqual(t.actions().at(-1), {
    action: 'filter', type: 'categorical', column: ['TERM'], values: ['T2'], nonce: 1
  });
  assert.ok(row.classList.contains('is-on'));
  assert.match(t.footer(), /TERM = T2/);
  click(row);
  assert.equal(t.actions().at(-1).action, 'clear_filter');
  assert.equal(row.classList.contains('is-on'), false);
  await t.close();
});

test('a gear change that drops the filter sends clear_filter', async () => {
  const t = open('flat');
  const row = t.rows()[1];
  click(row);
  click(t.doc.querySelector('.blockr-gear-btn'));
  const box = Array.from(t.band().querySelectorAll('.blockr-checkbox'))
    .find((b) => /Filter downstream on a click/.test(b.textContent));
  assert.ok(box, 'the band has the drill checkbox');
  click(box.querySelector('input'));
  const acts = t.actions();
  assert.deepEqual(acts.at(-2), { action: 'config', param: 'drill', value: '' });
  assert.equal(acts.at(-1).action, 'clear_filter');
  assert.equal(row.classList.contains('is-on'), false);
  assert.equal(t.footer(), '');
  await t.close();
});

test('a nested drill claims the row path: parent the outer column, child both', async () => {
  const t = open('nested_drill');
  const row = (label) => t.rows().find((r) => r.getAttribute('data-summarize-label') === label);
  click(row('SOC B'));
  let a = t.actions().at(-1);
  assert.deepEqual([a.column, a.values], [['SOC'], ['SOC B']]);
  assert.match(t.footer(), /Filtered: SOC = SOC B/);
  click(row('T2'));
  a = t.actions().at(-1);
  assert.deepEqual([a.column, a.values], [['SOC', 'TERM'], ['SOC A', 'T2']]);
  assert.match(t.footer(), /Filtered: SOC = SOC A, TERM = T2/);
  await t.close();
});

test('the gear stays open while its container settings change', async () => {
  const t = open('settings');
  click(t.doc.querySelector('.blockr-gear-btn'));
  assert.ok(t.isOpen());
  const band = t.band();
  const gear = t.doc.querySelector('.blockr-gear-btn');
  const clickRow = () => {
    const before = t.actions().length;
    click(t.rows()[0]);
    return t.actions().slice(before);
  };

  // Start: search on, no drill. A row click sends nothing.
  assert.equal(t.search().style.display, '');
  assert.deepEqual(clickRow(), []);

  // 1: search off. The box hides, the gear stays.
  t.push(1);
  assert.equal(t.search().style.display, 'none');

  // 2: drill on TERM. A click now filters, latched.
  t.push(2);
  assert.equal(t.root.getAttribute('data-summarize-drill'), 'TERM');
  assert.deepEqual(clickRow().map((a) => [a.action, a.values]), [['filter', ['T <1> & co']]]);
  assert.ok(t.rows()[0].classList.contains('is-on'));

  // 3: send to filter on. A click is an event: it sends, nothing latches.
  t.push(3);
  assert.equal(t.root.getAttribute('data-summarize-ctrl-target'), 'auto');
  const sent = clickRow();
  assert.equal(sent.length, 1);
  assert.equal(sent[0].action, 'filter');
  assert.equal(t.rows()[0].classList.contains('is-on'), false);

  // 4: send to filter off; 5: drill off. A click sends nothing again.
  t.push(4);
  assert.equal(t.root.getAttribute('data-summarize-ctrl-target'), '');
  t.push(5);
  assert.equal(t.root.hasAttribute('data-summarize-drill'), false);
  assert.deepEqual(clickRow(), []);

  // 6: search back on.
  t.push(6);
  assert.equal(t.search().style.display, '');

  assert.ok(t.isOpen(), 'the band is still open');
  assert.equal(t.band(), band, 'the band is the same node');
  assert.equal(t.doc.querySelector('.blockr-gear-btn'), gear, 'the gear is the same node');
  await t.close();
});

test('turning search off clears a typed query', async () => {
  const t = open('settings');
  const box = t.search();
  box.value = 'T3';
  box.dispatchEvent(new t.win.Event('input', { bubbles: true }));
  t.flush();
  assert.equal(t.shown().length, 1);
  t.push(1);
  assert.equal(box.value, '');
  assert.equal(t.shown().length, 4);
  await t.close();
});

test('a gear edit is sent as a config action', async () => {
  const t = open('flat');
  click(t.doc.querySelector('.blockr-gear-btn'));
  const asc = Array.from(t.band().querySelectorAll('button.blockr-segmented__seg'))
    .find((b) => b.textContent.trim() === 'Ascending');
  assert.ok(asc, 'the band has a sort direction control');
  click(asc);
  assert.deepEqual(t.actions().at(-1), { action: 'config', param: 'sort_dir', value: 'asc' });
  assert.ok(t.isOpen());
  await t.close();
});

test('a segment pick switches the selection once, and the echo keeps it', async () => {
  const t = open('segments');
  click(t.doc.querySelector('.blockr-gear-btn'));
  // The group a pick is made in, found by its options' labels: a segmented
  // control, or a row of tiles (Look).
  const group = (labels) => Array.from(t.band().querySelectorAll('.blockr-segmented, .dd-layout-tiles'))
    .find((g) => g.textContent === labels);
  const picked = (g) => Array.from(g.children)
    .filter((b) => (b.classList.contains('is-selected') || b.classList.contains('dd-layout-tile--on')) &&
      b.getAttribute('aria-checked') === 'true')
    .map((b) => b.textContent);
  // In the order the fixture's edits arrive: payload i + 1 is the server's
  // answer to pick i.
  const picks = [
    ['DescendingAscending', 'Ascending'],
    ['ColumnTable', 'Table'],
    ['DropShow', 'Show'],
    ['NarrowMediumWideFit', 'Wide'],
    ['TableCompactCards', 'Compact']
  ];
  picks.forEach(([labels, want], i) => {
    const g = group(labels);
    assert.ok(g, 'the gear has the ' + labels + ' control');
    const seg = Array.from(g.children).find((b) => b.textContent === want);
    // The selection at each point the browser could paint between: before
    // the click, after it, after its timers, after the echo, after its
    // timers. Looked up afresh each time, so a rebuilt gear counts too.
    const now = () => { const x = group(labels); return x ? picked(x) : ['(gone)']; };
    const seen = [now()];
    click(seg);
    seen.push(now());
    t.flush();
    seen.push(now());
    t.push(i + 1);
    seen.push(now());
    t.flush();
    seen.push(now());
    const changes = seen.filter((s, k) => k > 0 &&
      s.join() !== seen[k - 1].join()).length;
    assert.equal(changes, 1, labels + ': ' + JSON.stringify(seen));
    assert.deepEqual(seen.at(-1), [want]);
    // assert.ok: on a failure, assert.equal would print two happy-dom nodes,
    // which walks the whole document and never finishes.
    assert.ok(group(labels) === g, labels + ': the control is the same node');
  });
  assert.ok(t.isOpen());
  await t.close();
});

test('without a grouping the table is one Total row with nothing to sort, search or drill', async () => {
  const t = open('no_by');
  assert.deepEqual(t.labels(), ['Total']);
  assert.equal(t.search().style.display, 'none');
  assert.equal(t.root.hasAttribute('data-summarize-drill'), false);
  assert.equal(t.doc.querySelectorAll('th.blockr-sortable').length, 0);
  assert.deepEqual(clickRowActions(t), []);

  click(t.doc.querySelector('.blockr-gear-btn'));
  const band = t.band();
  const row = Array.from(band.querySelectorAll('.dd-form-row'))
    .find((r) => /One row per/.test(r.textContent));
  assert.ok(row, 'the gear has a One row per row');
  assert.equal(band.querySelectorAll('.dd-role-required-empty').length, 0,
    'One row per is optional: not marked as missing');
  assert.equal(/\*/.test(row.textContent), false, 'no required asterisk');
  assert.equal(/Drill-down/.test(band.textContent), false, 'no drill section');
  assert.equal(/Header sorting/.test(band.textContent), false);
  assert.equal(/Search/.test(band.textContent), false);

  // A grouping picked: one row per term, the search box back.
  t.push(1);
  assert.equal(t.labels().length, 4);
  assert.equal(t.search().style.display, '');
  await t.close();
});

function clickRowActions(t) {
  const before = t.actions().length;
  click(t.rows()[0]);
  return t.actions().slice(before);
}

// ---- the groups gear: one grouping, one column list ------------------------

/** Open the gear of a case and return its band. */
function openGear(t) {
  click(t.doc.querySelector('.blockr-gear-btn'));
  return t.band();
}
const titlesOf = (band) => Array.from(band.querySelectorAll('.dd-section-title'))
  .map((e) => e.textContent.trim());
const sumRows = (band) => Array.from(band.querySelectorAll('.lane-sum-row'));
const lineOf = (row) => row.querySelector('.lane-sum-line').textContent.trim();
const labelsIn = (el) => Array.from(el.querySelectorAll('.blockr-label'))
  .map((e) => e.textContent.trim());
const lastSummaries = (t) => {
  const a = t.actions().filter((x) => x.param === 'summaries');
  return a.length ? a[a.length - 1].value : null;
};

// The table block's server pushes its layout to both engines (table.js,
// "blockr-viz-table-layout"); the switch draws from the push, so a click on
// any segment but the layout on screen sends it, however often it is clicked.
test('the layout tiles follow the pushed layout, and every click on another layout sends', async () => {
  const t = open('three');
  const push = (layout) => t.win.__handlers['blockr-viz-table-layout'](
    { ids: ['x-records-drilldown_table_block', t.id], layout });
  const segs = () => Array.from(t.band().querySelectorAll('.dd-section--layout [role=radio]'));
  const selected = () => segs().filter((s) => s.getAttribute('aria-checked') === 'true')
    .map((s) => s.textContent.trim());
  const sent = () => t.actions().filter((x) => x.param === 'layout').map((x) => x.value);
  const pick = (name) => click(segs().find((s) => s.textContent.trim() === name));

  push('groups');
  openGear(t);
  assert.deepEqual(selected(), ['Per group']);
  // Groups -> Variables: the records engine takes over, this gear closes.
  pick('Per variable');
  assert.deepEqual(sent(), ['variables']);
  push('variables');
  assert.equal(t.isOpen(), false, 'the groups gear closes');
  // Back to groups with a gear open in the block: this gear opens on Groups.
  t.band().classList.add('blockr-settings--open');
  push('groups');
  assert.equal(t.isOpen(), true);
  assert.deepEqual(selected(), ['Per group']);
  // The click before the switch went to Variables; one click on it sends.
  pick('Per variable');
  assert.deepEqual(sent(), ['variables', 'variables']);
  push('variables');
  // A layout pushed while the gear is closed is what it opens on.
  push('records');
  openGear(t);
  assert.deepEqual(selected(), ['List']);
  pick('Per group');
  assert.deepEqual(sent(), ['variables', 'variables', 'groups']);
  await t.close();
});

test('the groups gear is Table with One row per, then Columns, then the rest', async () => {
  const t = open('three');
  const band = openGear(t);
  const titles = titlesOf(band);
  // Five groups; Data opens first, with the layout switch, the column list
  // and the prepare script. Presentation is split into Sort, Format and
  // Interaction's subsections.
  const tabs = Array.from(band.querySelectorAll('.blockr-tray-nav__tab')).map((e) => e.textContent);
  assert.deepEqual(tabs, ['Data', 'Sort', 'Format', 'Titles', 'Interaction']);
  const inGroup = (g) => Array.from(band.querySelectorAll('.dd-section'))
    .filter((e) => e.dataset.trayGroup === g).map((e) => e.dataset.ddTitle);
  assert.deepEqual(inGroup('data'), ['Table', 'Columns', 'Prepare script']);
  assert.deepEqual(inGroup('sort'), ['Order', 'Rows shown']);
  assert.deepEqual(inGroup('format'), ['Look', 'Numbers', 'Bars']);
  assert.deepEqual(inGroup('titles'), ['Titles']);
  assert.deepEqual(inGroup('interact'), ['Reader tools', 'Drill-down']);
  assert.equal(titles.includes('Presentation'), false, 'no Presentation section');
  assert.equal(titles.includes('Mapping'), false, 'no Mapping section');
  const layout = band.querySelector('.dd-section--layout');
  const tiles = Array.from(layout.querySelectorAll('.dd-layout-tile'));
  assert.deepEqual(tiles.map((b) => b.textContent.trim()), ['List', 'Per group', 'Per variable']);
  assert.ok(tiles.every((b) => b.querySelector('.dd-layout-tile-face svg')), 'each tile has its drawing');
  assert.match(layout.querySelector('.dd-layout-hint').textContent,
    /^One row (per .+|for all rows), each column a summary\.$/);
  assert.ok(layout.querySelector('.dd-role-by'), 'One row per sits under the tiles');
  assert.equal(band.querySelectorAll('.dd-role-by').length, 1, 'one grouping');
  assert.equal(/Adding columns/.test(band.textContent), false, 'no mode hint');
  for (const gone of ['group', 'parent', 'func', 'value', 'id_var', 'fields', 'bar_mode', 'color', 'facet']) {
    assert.equal(band.querySelectorAll('.dd-role-' + gone).length, 0, gone + ' has no gear row');
  }
  await t.close();
});

test('Top caps the rows after Sort and Order, and leaves without a grouping', async () => {
  const t = open('three');
  const band = openGear(t);
  const keys = Array.from(band.querySelectorAll('.dd-form-row'))
    .map((r) => (r.className.match(/dd-role-(\w+)/) || [])[1]).filter(Boolean);
  const at = keys.indexOf('top_n');
  assert.ok(at > 0, 'Top is in the gear');
  assert.deepEqual(keys.slice(at - 2, at + 1), ['sort_by', 'sort_dir', 'top_n']);
  const inp = band.querySelector('.dd-role-top_n input[type=number]');
  assert.equal(inp.value, '', 'empty: every row');
  inp.value = '2';
  inp.dispatchEvent(new t.win.Event('change', { bubbles: true }));
  const sent = t.actions().filter((a) => a.param === 'top_n').pop();
  assert.equal(sent.value, 2);
  await t.close();

  const t0 = open('no_by');
  assert.equal(openGear(t0).querySelectorAll('.dd-role-top_n').length, 0);
  await t0.close();
});

test('a new groups table opens on its one count column', async () => {
  const t = open('fresh');
  const band = openGear(t);
  const rows = sumRows(band);
  assert.equal(rows.length, 1);
  assert.ok(rows[0].classList.contains('is-open'), 'the column is open');
  assert.equal(lineOf(rows[0]), 'Count of rows');
  const body = rows[0].querySelector('.lane-sum-body');
  assert.deepEqual(labelsIn(body), ['Name', 'Aggregate', 'Show']);
  assert.equal(body.querySelector('input.blockr-text-input').value, 'Count');
  // Show is a tile per mark, each a drawing of the column.
  const tiles = Array.from(body.querySelectorAll('.dd-layout-tile'));
  assert.deepEqual(tiles.map((b) => b.textContent.trim()), ['Bar', 'Number', 'Dot'],
    'the tile is the compact look\'s mark');
  assert.ok(tiles.every((b) => b.querySelector('.dd-layout-tile-face svg')), 'each tile has its drawing');
  assert.equal(body.querySelector('.dd-layout-tile--on').textContent.trim(), 'Bar');
  assert.deepEqual(Array.from(body.querySelectorAll('.lane-sum-offer')).map((b) => b.textContent),
    ['+ Color', '+ Facet']);
  const rm = rows[0].querySelector('.lane-sum-rm');
  assert.ok(rm.disabled, 'the last column cannot be removed');
  assert.equal(rm.getAttribute('aria-label'), 'A table needs one column');

  // An offer sets the mapping and turns into its field.
  click(Array.from(body.querySelectorAll('.lane-sum-offer'))[0]);
  assert.ok(lastSummaries(t)[0].color, 'a colour split is set');
  assert.ok(labelsIn(sumRows(band)[0]).includes('Color'));

  // Show: Number sends the list with the column drawn as a number, and the
  // column stays open. A number takes no colour split, so its offer leaves.
  const num = Array.from(sumRows(band)[0].querySelectorAll('.dd-layout-tile'))
    .find((b) => b.textContent.trim() === 'Number');
  click(num);
  assert.equal(lastSummaries(t)[0].show, 'number');
  assert.ok(sumRows(band)[0].classList.contains('is-open'));
  await t.close();
});

test('with three columns every row is one closed line', async () => {
  const t = open('three');
  const band = openGear(t);
  let rows = sumRows(band);
  assert.equal(rows.length, 3);
  assert.equal(band.querySelectorAll('.lane-sum-row.is-open').length, 0);
  assert.deepEqual(rows.map(lineOf),
    ['Count of rows, split by SEV', 'Value distribution of AVAL', 'Mean of AVAL']);
  assert.ok(rows.every((r) => !r.querySelector('.lane-sum-rm').disabled));

  // A click opens one row and sends nothing.
  const before = t.actions().length;
  click(rows[1].querySelector('.lane-sum-toggle'));
  rows = sumRows(band);
  assert.ok(rows[1].classList.contains('is-open'));
  assert.equal(rows[1].querySelector('.lane-sum-toggle').getAttribute('aria-expanded'), 'true');
  assert.equal(t.actions().length, before);
  // A distribution's marks are three tiles.
  assert.deepEqual(Array.from(rows[1].querySelectorAll('.dd-layout-tile'))
    .map((b) => b.textContent.trim()), ['Dot range', 'Box', 'Text']);

  // Alt+Down moves the column, and the keyboard stays on it.
  const toggle = rows[0].querySelector('.lane-sum-toggle');
  toggle.dispatchEvent(new t.win.KeyboardEvent('keydown',
    { key: 'ArrowDown', altKey: true, bubbles: true }));
  assert.deepEqual(lastSummaries(t).map((s) => s.type), ['dist', 'simple', 'simple']);
  assert.equal(t.doc.activeElement, sumRows(band)[1].querySelector('.lane-sum-toggle'));

  // Remove: the list goes with one column less.
  click(sumRows(band)[2].querySelector('.lane-sum-rm'));
  assert.equal(lastSummaries(t).length, 2);
  assert.equal(sumRows(band).length, 2);
  await t.close();
});

test('an old ranked-bar summarize table opens as one column of the list', async () => {
  const t = open('legacy');
  // parent then group: SOC rows with TERM rows under them.
  assert.ok(t.labels().includes('SOC A'));
  const band = openGear(t);
  const tags = Array.from(band.querySelectorAll('.dd-role-by .blockr-select__tag'))
    .map((e) => e.textContent.trim());
  assert.deepEqual(tags, ['SOC', 'TERM']);
  const rows = sumRows(band);
  assert.equal(rows.length, 1);
  assert.ok(rows[0].classList.contains('is-open'));
  assert.equal(lineOf(rows[0]), 'Count distinct USUBJID, split by SEV');
  assert.equal(rows[0].querySelector('.dd-layout-tile--on').textContent.trim(), 'Bar');
  const split = Array.from(rows[0].querySelectorAll('.blockr-segmented__seg.is-selected'))
    .map((b) => b.textContent);
  assert.deepEqual(split, ['Grouped'], 'bar_mode = "grouped" is the split\'s position');
  await t.close();
});
