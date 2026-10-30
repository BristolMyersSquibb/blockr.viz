'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadGear, mountGear, payload, lastSet, rows, row, choose, type } = require('./gear-load');

const setup = (config, opts) => {
  const env = loadGear();
  const m = mountGear(env, opts);
  m.gear.update(payload(config));
  return Object.assign(env, m);
};
const sel = (el, key) => row(el, key).querySelector('select');
const inp = (el, key) => row(el, key).querySelector('input, textarea');
const byKey = (el, key) => el.querySelector(`[data-gear-input="${key}"]`);
const picked = (s) => Array.from(s.options).filter((o) => o.selected);
/** Out of the window's realm, for deepEqual. */
const plain = (x) => JSON.parse(JSON.stringify(x));

test('rows follow the spec: applies, when and the host hide rows', () => {
  const t = setup({ type: 'histogram', x: 'AGE' });
  const shown = rows(t.el);
  assert.ok(shown.includes('x') && shown.includes('bins'));
  assert.ok(!shown.includes('y'), 'y does not apply to a histogram');
  assert.ok(!shown.includes('legend'), 'legend waits for a color');
  t.gear.update(payload({ type: 'bar', category: 'ARM', orientation: 'vertical', color: 'SEX' }));
  assert.ok(rows(t.el).includes('legend'));
  assert.ok(rows(t.el).includes('value') && !rows(t.el).includes('y'));

  const h = setup({ type: 'bar' }, { host: { visible: (k) => k !== 'unit' } });
  assert.ok(!rows(h.el).includes('unit'));
});

test('sections come in tray order with spec labels', () => {
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical' });
  const titles = Array.from(t.el.querySelectorAll('.vz-section-title')).map((e) => e.textContent);
  assert.deepEqual(titles.slice(0, 3), ['Chart type', 'Mapping', 'Prepare script']);
  assert.ok(titles.indexOf('Order') < titles.indexOf('Look'));
  assert.ok(titles.indexOf('Look') < titles.indexOf('Titles'));
  assert.ok(titles.indexOf('Titles') < titles.indexOf('Drill-down'));
  assert.equal(row(t.el, 'func').querySelector('.blockr-label').textContent, 'Aggregate');
  // The chart type is alone in its section, and its header names it.
  assert.equal(row(t.el, 'type').querySelector('.blockr-label'), null);
  assert.ok(row(t.el, 'type').classList.contains('vz-row--nolabel'));
});

test('every row has a label, checkboxes and dropped-label rows included', () => {
  const label = (el, key) => {
    const l = row(el, key).querySelector('.vz-row-head .blockr-label');
    return l ? l.textContent : null;
  };
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical', color: 'SEX',
                    sort_by: 'value', drill: 'auto' });
  // These used to have none: a label equal to the section, and checkboxes.
  assert.equal(label(t.el, 'format'), 'Number format');
  assert.equal(label(t.el, 'sort_dir'), 'Direction');
  assert.equal(label(t.el, 'value_labels'), 'Value labels');
  assert.equal(label(t.el, 'download'), 'Download');
  assert.equal(label(t.el, 'drill'), 'On click');
  // A checkbox's own words are a short verb.
  assert.equal(row(t.el, 'drill').querySelector('.blockr-checkbox__label').textContent, 'Filter on');
  const unlabelled = rows(t.el).filter((k) => label(t.el, k) == null);
  assert.deepEqual(unlabelled, ['type', 'script']);
});

test('short sections are merged: counts, facets and shading join their neighbours', () => {
  const titles = (el) => Array.from(el.querySelectorAll('.vz-section-title')).map((e) => e.textContent);
  const section = (el, key) => row(el, key).closest('.vz-section').getAttribute('data-section');
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical', facet: 'SEX' });
  for (const gone of ['Counts', 'Facets', 'Bars', 'Color']) assert.ok(!titles(t.el).includes(gone), gone);
  assert.equal(section(t.el, 'show_n'), 'Numbers');
  assert.equal(section(t.el, 'value_labels'), 'Numbers');
  assert.equal(section(t.el, 'facet_cols'), 'Look');
  assert.equal(section(t.el, 'x_ranges'), 'Reference lines');
  // The two-cell field goes last in its section.
  const refs = Array.from(row(t.el, 'x_ranges').parentElement.querySelectorAll('.vz-row[data-role]'))
    .map((r) => r.getAttribute('data-role'));
  assert.equal(refs[refs.length - 1], 'x_ranges');
});

test('a change sends only the changed key, with the payload revision', () => {
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical', func: 'count' });
  choose(t.win, sel(t.el, 'func'), 'Mean');
  assert.equal(t.sent.length, 1);
  assert.equal(t.sent[0].name, 'blk-chart_set:blockr.viz.chart');
  assert.deepEqual(t.sent[0].value, { rev: 3, values: { func: 'mean' } });
  assert.equal(t.sent[0].opts.priority, 'event');
  t.gear.set({ func: 'mean', category: 'ARM' });
  assert.equal(t.sent.length, 1, 'nothing changed, nothing sent');
  t.gear.set({ func: 'sum', category: 'SEX', nope: 1 });
  assert.deepEqual(lastSet(t).values, { func: 'sum', category: 'SEX' });
});

test('column: (none) unless required, a missing column kept, labels muted', () => {
  const t = setup({ type: 'bar', category: 'GONE', orientation: 'vertical', color: 'ARM' });
  const texts = Array.from(sel(t.el, 'color').options).map((o) => o.textContent);
  assert.equal(texts[0], '(none)');
  assert.ok(texts.includes('ARM (Arm)'));
  assert.ok(texts.includes('SEX'));
  choose(t.win, sel(t.el, 'color'), '(none)');
  assert.deepEqual(lastSet(t).values, { color: null });
  assert.ok(Array.from(sel(t.el, 'category').options).some((o) => o.textContent === 'GONE (missing)'));

  const r = setup({ type: 'bar' }, { host: { required: (k) => k === 'category',
    columns: (k, v, cols) => (k === 'category' ? cols.filter((c) => c.class !== 'double') : cols) } });
  assert.equal(row(r.el, 'category').querySelector('.blockr-label').textContent, 'Category *');
  const xs = Array.from(sel(r.el, 'category').options).map((o) => o.textContent);
  assert.ok(!xs.includes('(none)'));
  assert.ok(!xs.some((x) => x.startsWith('AVAL')));
});

test('an empty required row is amber until it is picked', () => {
  const amber = (t, key) => row(t.el, key).classList.contains('blockr-field--required-empty');
  const host = { required: (k) => k === 'category' || k === 'value' };
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical' }, { host });
  assert.equal(amber(t, 'category'), false);
  assert.equal(amber(t, 'value'), true);
  assert.equal(amber(t, 'func'), false, 'a row that is not required never is');
  t.gear.update(payload({ type: 'bar', category: 'ARM', value: 'AVAL', orientation: 'vertical' }));
  assert.equal(amber(t, 'value'), false);
});

test('column on a columns argument sends an array', () => {
  const t = setup({ type: 'line', x: 'ADT', y: 'AVAL' });
  choose(t.win, sel(t.el, 'ymin'), 'LO');
  assert.deepEqual(lastSet(t).values, { ymin: ['LO'] });
});

test('optional mappings wait in the Add menu until set or added', () => {
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical' });
  assert.ok(!rows(t.el).includes('color'));
  const add = t.el.querySelector('.vz-add select');
  const offered = Array.from(add.options).map((o) => o.textContent);
  assert.ok(offered.includes('Color') && offered.includes('Facet'));
  assert.ok(!offered.includes('Series'), 'series does not apply to a bar');
  choose(t.win, add, 'Color');
  assert.ok(rows(t.el).includes('color'));
  assert.equal(t.sent.length, 0, 'adding a row sends nothing');
  t.gear.update(payload({ type: 'bar', category: 'ARM', orientation: 'vertical', facet: 'SEX' }));
  assert.ok(rows(t.el).includes('facet') && rows(t.el).includes('color'));
});

test('columns: several picks, empty to null where the spec allows it', () => {
  const env = loadGear();
  const m = mountGear(env, { block: 'table', id: 'tb' });
  m.gear.update(payload({ layout: 'groups', by: [] }));
  const s = sel(m.el, 'by');
  assert.equal(s.multiple, true);
  s.options[1].selected = true;
  s.options[2].selected = true;
  s.dispatchEvent(new env.win.Event('change'));
  assert.equal(env.sent[0].name, 'tb_set:blockr.viz.table');
  assert.deepEqual(env.sent[0].value.values, { by: ['ARM', 'SEX'] });
  const s2 = sel(m.el, 'by');
  for (const o of s2.options) o.selected = false;
  s2.dispatchEvent(new env.win.Event('change'));
  assert.deepEqual(env.sent[1].value.values, { by: [] }, 'by is not nullable');
});

test('enums in a columns control (table stats)', () => {
  const env = loadGear();
  const m = mountGear(env, { block: 'table', id: 'tb' });
  m.gear.update(payload({ layout: 'variables', stats: ['mean_sd'] }));
  const s = sel(m.el, 'stats');
  assert.deepEqual(picked(s).map((o) => o.textContent), ['Mean (SD)']);
  s.options[0].selected = true;
  s.dispatchEvent(new env.win.Event('change'));
  assert.deepEqual(env.sent[0].value.values, { stats: ['n', 'mean_sd'] });
});

test('values: picks among what the host offers, as labelled values', () => {
  const t = setup({ type: 'waterfall', category: 'ARM' }, { host: {
    choices: (k) => (k === 'waterfall_totals' ? [{ value: 'Total', label: 'Total' },
                                                 { value: 'Q2', label: 'Q2' }] : null) } });
  const s = sel(t.el, 'waterfall_totals');
  s.options[0].selected = true;
  s.dispatchEvent(new t.win.Event('change'));
  assert.deepEqual(lastSet(t).values, { waterfall_totals: [{ value: 'Total', label: null }] });
});

test('highlight: a column, then its values', () => {
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical', highlight: { ARM: [{ value: 'A', label: null }] } },
    { host: { choices: (k, v) => (k === 'highlight' ? [{ value: 'A', label: 'A' },
                                                        { value: 'B', label: 'B' }] : null) } });
  const valSel = row(t.el, 'highlight').querySelectorAll('select')[1];
  assert.deepEqual(picked(valSel).map((o) => o.value), ['A']);
  valSel.options[1].selected = true;
  valSel.dispatchEvent(new t.win.Event('change'));
  assert.deepEqual(lastSet(t).values, { highlight: { ARM: [{ value: 'A', label: null },
                                                           { value: 'B', label: null }] } });
  choose(t.win, row(t.el, 'highlight').querySelector('select'), 'SEX');
  assert.deepEqual(lastSet(t).values, { highlight: { SEX: [] } });
});

test('select: spec labels, format choices, host choices, Auto for null', () => {
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical', format: 'number', sort_by: null }, { host: {
    choices: (k) => (k === 'sort_by' ? [{ value: 'value', label: 'By value' },
                                         { value: 'alpha', label: 'A–Z' }] : null) } });
  const fmts = Array.from(sel(t.el, 'format').options).map((o) => o.textContent);
  assert.deepEqual(fmts.slice(0, 3), ['Auto', '1,235', '1,234.6']);
  choose(t.win, sel(t.el, 'format'), '1,234.57');
  assert.deepEqual(lastSet(t).values, { format: 2 });
  const sorts = Array.from(sel(t.el, 'sort_by').options).map((o) => o.textContent);
  assert.deepEqual(sorts, ['Auto', 'By value', 'A–Z']);
  choose(t.win, sel(t.el, 'sort_by'), 'A–Z');
  assert.deepEqual(lastSet(t).values, { sort_by: 'alpha' });
});

test('tiles: the chart type, grouped by the host, keys move the pick', () => {
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical' }, { host: { tiles: () => [
    { label: 'Aggregated', values: ['bar', 'histogram', 'pie'] },
    { label: 'Individual', values: ['scatter', 'line'] }] } });
  const r = row(t.el, 'type');
  assert.deepEqual(Array.from(r.querySelectorAll('.vz-tile-group-head')).map((e) => e.textContent),
    ['Aggregated', 'Individual']);
  const tiles = Array.from(r.querySelectorAll('.vz-tile'));
  assert.equal(tiles.length, 5);
  assert.ok(tiles[0].querySelector('svg'), 'the bar has its drawing');
  assert.equal(tiles[0].getAttribute('aria-checked'), 'true');
  tiles[0].click();
  assert.equal(t.sent.length, 0, 'the chosen tile does nothing');
  r.querySelector('[data-value="line"]').click();
  assert.deepEqual(lastSet(t).values, { type: 'line' });
});

test('segmented: an enum picks, a bool is a checkbox with its first label', () => {
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical', color: 'SEX', position: 'stack' });
  const segs = Array.from(row(t.el, 'position').querySelectorAll('button'));
  assert.deepEqual(segs.map((b) => b.textContent), ['Stacked', 'Side by side', '100%']);
  segs[1].click();
  assert.deepEqual(lastSet(t).values, { position: 'dodge' });
  const vl = row(t.el, 'value_labels');
  assert.equal(vl.querySelector('.blockr-checkbox__label').textContent, 'Show');
  const box = vl.querySelector('input');
  box.checked = true;
  box.dispatchEvent(new t.win.Event('change'));
  assert.deepEqual(lastSet(t).values, { value_labels: true });
});

test('segmented with icons: icon and word; where the words do not fit, icons named by tooltips', () => {
  const env = loadGear();
  // Layout: the row 100px wide, the control 300px with its words, 90 without.
  const P = env.win.HTMLElement.prototype;
  const rect = P.getBoundingClientRect;
  let room = 400;
  P.getBoundingClientRect = function () {
    const w = this.classList.contains('vz-row-controls') ? room
      : this.classList.contains('blockr-segmented') ? (this.classList.contains('vz-seg--bare') ? 90 : 300) : 0;
    return { width: w, height: 0, top: 0, left: 0, right: w, bottom: 0, x: 0, y: 0 };
  };
  const observed = [];
  env.win.ResizeObserver = function (cb) {
    this.observe = (el) => observed.push({ el, cb, ro: this });
    this.disconnect = () => { this.gone = true; };
  };
  try {
    const m = mountGear(env);
    m.gear.update(payload({ type: 'bar', category: 'ARM', color: 'SEX', position: 'stack' }));
    const seg = row(m.el, 'position').querySelector('.blockr-segmented');
    assert.ok(seg.classList.contains('vz-seg--icons'));
    assert.equal(seg.querySelectorAll('.vz-seg-icon svg').length, 3);
    assert.deepEqual(Array.from(seg.querySelectorAll('.vz-seg-word')).map((w) => w.textContent),
                     ['Stacked', 'Side by side', '100%']);
    assert.ok(!seg.classList.contains('vz-seg--bare'));
    assert.equal(seg.querySelector('button').getAttribute('data-blockr-tooltip'), null);
    // Four or more icons take the whole row, so their words have room.
    assert.ok(row(m.el, 'legend').classList.contains('vz-row--wide'));
    assert.ok(!row(m.el, 'position').classList.contains('vz-row--wide'));
    const watch = observed.find((o) => o.el.contains(seg));
    room = 100;
    watch.cb();
    assert.ok(seg.classList.contains('vz-seg--bare'));
    const b = seg.querySelectorAll('button')[1];
    assert.equal(b.getAttribute('data-blockr-tooltip'), 'Side by side');
    assert.equal(b.getAttribute('aria-label'), 'Side by side');
    room = 400;
    watch.cb();
    assert.ok(!seg.classList.contains('vz-seg--bare'));
    assert.equal(b.getAttribute('aria-label'), null);
    // A segmented control without icons keeps its words only.
    m.gear.update(payload({ type: 'bar', category: 'ARM', missing: 'level' }));
    assert.equal(row(m.el, 'missing').querySelector('.vz-seg-icon'), null);
    m.scope.dispose();
    assert.ok(watch.ro.gone, 'the observer goes with the gear');
  } finally {
    P.getBoundingClientRect = rect;
  }
});

test('every option of an icons entry has a drawing, in both blocks and their records', () => {
  const env = loadGear();
  const glyphs = env.V.gear.glyphs;
  let n = 0;
  const walk = (args) => {
    for (const [key, arg] of Object.entries(args || {})) {
      if (arg.gear && arg.gear.icons) {
        assert.equal(arg.gear.control, 'segmented', key);
        for (const v of arg.values) {
          assert.match(glyphs[key + ':' + v] || '', /^<svg class="vz-glyph"/, key + ':' + v);
          n += 1;
        }
      }
      walk(arg.type.fields);
      for (const fields of Object.values(arg.type.variants || {})) walk(fields);
    }
  };
  walk(env.V.spec.chart);
  walk(env.V.spec.table);
  assert.ok(n >= 40, n + ' options with icons');
  for (const t of Object.keys(env.V.spec.table.summaries.type.variants)) {
    assert.match(glyphs['summary:' + t], /viewBox="0 0 24 24"/, t);
  }
});

test('toggle: drill switches on with auto and shows the column picker', () => {
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical', drill: null }, { host: {
    choices: (k) => (k === 'drill' ? [{ value: 'auto', label: 'Auto, the clicked group' }] : null) } });
  const r = row(t.el, 'drill');
  assert.equal(r.querySelector('select'), null);
  const box = r.querySelector('input[type=checkbox]');
  box.checked = true;
  box.dispatchEvent(new t.win.Event('change'));
  assert.deepEqual(lastSet(t).values, { drill: 'auto' });
  const s = sel(t.el, 'drill');
  assert.equal(s.options[0].textContent, 'Auto, the clicked group');
  choose(t.win, s, 'USUBJID (Subject)');
  assert.deepEqual(lastSet(t).values, { drill: 'USUBJID' });
  const off = row(t.el, 'drill').querySelector('input[type=checkbox]');
  off.checked = false;
  off.dispatchEvent(new t.win.Event('change'));
  assert.deepEqual(lastSet(t).values, { drill: null });
});

test('toggle on a bool (table total)', () => {
  const env = loadGear();
  const m = mountGear(env, { block: 'table', id: 'tb' });
  m.gear.update(payload({ layout: 'variables', total: false }));
  const box = row(m.el, 'total').querySelector('input');
  box.checked = true;
  box.dispatchEvent(new env.win.Event('change'));
  assert.deepEqual(env.sent[0].value.values, { total: true });
});

test('number: held in its bounds, committed on Enter', () => {
  const t = setup({ type: 'histogram', x: 'AGE', bins: 30 });
  const i = inp(t.el, 'bins');
  assert.equal(i.type, 'number');
  type(t.win, i, '500');
  assert.deepEqual(lastSet(t).values, { bins: 200 });
});

test('text: lines and ranges parse into their wire forms and back', () => {
  const t = setup({ type: 'line', x: 'ADT', y: 'AVAL', value_lines: null });
  const i = inp(t.el, 'value_lines');
  assert.equal(i.placeholder, 'e.g. 2 or Target: 4');
  type(t.win, i, 'Target: 4; 6');
  assert.deepEqual(lastSet(t).values, { value_lines: [{ value: 4, label: 'Target' },
                                                  { value: 6, label: null }] });
  const C = t.V.gear.controls;
  const spec = t.V.spec.chart;
  assert.equal(C.toText(spec.value_lines, lastSet(t).values.value_lines), 'Target: 4; 6');
  assert.deepEqual(plain(C.fromText(spec.x_lines, 'Listed: 2026-10-21; 3')),
    [{ value: '2026-10-21', label: 'Listed' }, { value: 3, label: null }]);
  assert.deepEqual(C.fromText(spec.value_lines, '10:30'), null, 'a time is no label, and no number');
  const r = plain(C.fromText(spec.x_ranges, 'Recession: 2008-01-01 to 2009-06-30; 2 to 4'));
  assert.deepEqual(r, { Recession: [{ value: '2008-01-01', label: null }, { value: '2009-06-30', label: null }],
                        'Range 2': [{ value: 2, label: null }, { value: 4, label: null }] });
  assert.equal(C.toText(spec.x_ranges, r), 'Recession: 2008-01-01 to 2009-06-30; Range 2: 2 to 4');
});

test('text: a string commits on blur, Escape restores, empty is ""', () => {
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical', unit: '' });
  const i = inp(t.el, 'unit');
  i.focus();
  i.value = 'CHF';
  i.dispatchEvent(new t.win.Event('blur'));
  assert.deepEqual(lastSet(t).values, { unit: 'CHF' });
  const j = inp(t.el, 'unit');
  j.value = 'EUR';
  j.dispatchEvent(new t.win.KeyboardEvent('keydown', { key: 'Escape' }));
  assert.equal(j.value, 'CHF');
  type(t.win, inp(t.el, 'unit'), '  ');
  assert.deepEqual(lastSet(t).values, { unit: '' });
  assert.equal(byKey(t.el, 'script').tagName, 'TEXTAREA');
});

test('text fields take blockr.ui text input class', () => {
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical' });
  assert.ok(inp(t.el, 'unit').classList.contains('blockr-text-input'));
});

test('the prepare script sits behind a checkbox; unticking drops the script', () => {
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical', script: null });
  const box = byKey(t.el, 'script_on');
  assert.equal(box.checked, false);
  assert.equal(byKey(t.el, 'script').closest('.vz-script-wrap').style.display, 'none');
  box.click();
  assert.equal(byKey(t.el, 'script').closest('.vz-script-wrap').style.display, '');
  const u = setup({ type: 'bar', category: 'ARM', orientation: 'vertical', script: 'data <- data' });
  assert.equal(byKey(u.el, 'script_on').checked, true);
  byKey(u.el, 'script_on').click();
  assert.deepEqual(lastSet(u).values, { script: null });
});

test('slider: sends a number on change', () => {
  const t = setup({ type: 'line', x: 'ADT', y: 'AVAL', line_width: 1 });
  const i = inp(t.el, 'line_width');
  assert.equal(i.type, 'range');
  assert.equal(i.min, '0.5');
  i.value = '2';
  i.dispatchEvent(new t.win.Event('input'));
  assert.equal(row(t.el, 'line_width').querySelector('.vz-slider-value').textContent, '2×');
  i.dispatchEvent(new t.win.Event('change'));
  assert.deepEqual(lastSet(t).values, { line_width: 2 });
});

test('none: drawn by the host, whose set sends', () => {
  const env = loadGear();
  let seen = null;
  const m = mountGear(env, { block: 'table', id: 'tb', host: { custom: (key, el, ctx) => {
    seen = key;
    el.textContent = 'editor';
    if (key === 'summaries') ctx.set([{ type: 'simple', func: 'count' }]);
  } } });
  m.gear.update(payload({ layout: 'groups', summaries: [] }));
  assert.equal(seen, 'summaries');
  assert.deepEqual(env.sent[0].value.values, { summaries: [{ type: 'simple', func: 'count' }] });
  const n = loadGear();
  const plain = mountGear(n, { block: 'table', id: 'tb' });
  plain.gear.update(payload({ layout: 'groups', summaries: [] }));
  assert.ok(!rows(plain.el).includes('summaries'), 'no editor, no row');
});

test('a local edit stays until R echoes it, and R wins after that', () => {
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical', func: 'count' });
  choose(t.win, sel(t.el, 'func'), 'Mean');
  t.gear.update(payload({ type: 'bar', category: 'ARM', orientation: 'vertical', func: 'count' }));
  assert.equal(t.gear.values().func, 'mean', 'a payload from before the edit');
  t.gear.update(payload({ type: 'bar', value: 'ARM', func: 'mean' }));
  assert.equal(t.gear.values().func, 'mean');
  choose(t.win, sel(t.el, 'func'), 'Sum');
  t.gear.update(payload({ type: 'bar', value: 'ARM', func: 'median' }));
  assert.equal(t.gear.values().func, 'median', 'R changed it: R wins');
});

test('a payload that changes nothing shown keeps the tray', () => {
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical' });
  const first = t.el.firstElementChild;
  t.gear.update(payload({ type: 'bar', category: 'ARM', orientation: 'vertical' }, { state_rev: 3 }));
  assert.equal(t.el.firstElementChild, first);
  t.gear.update(payload({ type: 'bar', category: 'SEX', orientation: 'vertical' }));
  assert.notEqual(t.el.firstElementChild, first);
});

test('a tile click selects once; R\'s echo leaves the tile alone', () => {
  const env = loadGear();
  const m = mountGear(env, { block: 'table', id: 'tb' });
  const titles = (layout) => ({ titles: {
    title: { text: 'Subjects', parts: [{ text: 'Subjects' }], offers: [] },
    args: Object.assign({ layout, look: 'table' }, layout === 'variables' ? { summaries: '' } : {}),
    choices: [] } });
  m.gear.update(payload({ layout: 'groups', look: 'table' }, { derived: titles('groups') }));
  const tile = (v) => m.el.querySelector('.vz-tile[data-value="' + v + '"]');
  /** Every class change on a layout tile, as `value:on|off`. */
  const log = [];
  const mo = new env.win.MutationObserver(() => {});
  mo.observe(m.el, { subtree: true, attributes: true, attributeFilter: ['class'] });
  const flush = () => {
    for (const r of mo.takeRecords()) {
      if (!r.target.classList.contains('vz-tile')) continue;
      log.push(r.target.getAttribute('data-value') + ':' +
               (r.target.classList.contains('vz-tile--on') ? 'on' : 'off'));
    }
  };

  try {
    tile('variables').click();
    flush();
    assert.deepEqual(log.filter((x) => x.startsWith('variables')), ['variables:on'],
                     'the clicked tile turns on once');
    const drawn = m.el.firstElementChild;
    const on = tile('variables');
    assert.ok(on.classList.contains('vz-tile--on'));
    log.length = 0;

    // A payload sent before R read the click, then R's echo: what R prints
    // for each argument changed, nothing the tray shows did.
    m.gear.update(payload({ layout: 'groups', look: 'table' }, { derived: titles('groups') }));
    m.gear.update(payload({ layout: 'variables', look: 'table' },
                          { state_rev: 4, data_rev: 2, derived: titles('variables') }));
    flush();
    assert.ok(m.el.firstElementChild === drawn, 'the echo keeps the tray');
    assert.ok(tile('variables') === on, 'and the tile');
    assert.ok(on.classList.contains('vz-tile--on'));
    assert.deepEqual(log, [], 'no tile changed class');
  } finally {
    mo.disconnect();
  }

  // The `@` menu reads the titles of the latest payload, not those drawn.
  assert.equal(m.gear._ctx('title', m.gear.values(), m.scope).titles.args.layout, 'variables');

  // A selected tile keeps its look under the pointer (gear.css).
  const css = require('node:fs').readFileSync(require('node:path').join(
    __dirname, '..', '..', '..', 'inst', 'css', 'gear.css'), 'utf8');
  assert.match(css, /\.vz-tile--on:hover \.vz-tile-face\s*[,{]/);
});

test('new rows redraw the tray only when they change what it offers', () => {
  let levels = ['A', 'B'];
  const host = { choices: (k) => (k === 'drill_target' ? levels.map((v) => ({ value: v, label: v })) : null) };
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical', drill: 'auto' }, { host });
  const first = t.el.firstElementChild;
  const cfg = { type: 'bar', category: 'ARM', orientation: 'vertical', drill: 'auto' };
  t.gear.update(payload(cfg, { data_rev: 2 }));
  assert.ok(t.el.firstElementChild === first, 'same offers, same tray');
  levels = ['A', 'B', 'C'];
  t.gear.update(payload(cfg, { data_rev: 3 }));
  assert.ok(t.el.firstElementChild !== first, 'a new offer redraws');
});

test('a template row redraws when the arguments it names come or go', () => {
  const cfg = { type: 'bar', category: 'ARM', orientation: 'vertical', title: 'By {@color}' };
  const t = setup(cfg);
  const warn = () => row(t.el, 'title').querySelector('.vz-template-warn');
  assert.equal(warn().hidden, false, 'nothing is called @color yet');
  const first = t.el.firstElementChild;
  t.gear.update(payload(cfg, { derived: { titles: { args: { category: 'ARM' } } } }));
  assert.ok(t.el.firstElementChild === first, 'other arguments: same warning, same tray');
  t.gear.update(payload(cfg, { derived: { titles: { args: { category: 'ARM', color: '' } } } }));
  assert.equal(warn().hidden, true);
});

test('a field being typed in survives a payload', () => {
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical', unit: '' });
  const i = inp(t.el, 'unit');
  i.focus();
  i.value = 'CH';
  i.setSelectionRange(2, 2);
  t.gear.update(payload({ type: 'bar', category: 'SEX', orientation: 'vertical', unit: '' }));
  const j = inp(t.el, 'unit');
  assert.notEqual(j, i);
  assert.equal(j.value, 'CH');
  assert.equal(t.doc.activeElement, j);
  assert.equal(t.sent.length, 0);
});

test('the table follows its own spec: layout tiles and per-layout rows', () => {
  const env = loadGear();
  const m = mountGear(env, { block: 'table', id: 'tb' });
  m.gear.update(payload({ layout: 'records', look: 'table' }));
  const tiles = Array.from(row(m.el, 'layout').querySelectorAll('.vz-tile-name'))
    .map((e) => e.textContent);
  assert.deepEqual(tiles, ['List', 'Per group', 'Per variable']);
  assert.ok(row(m.el, 'look').querySelector('svg'));
  assert.ok(rows(m.el).includes('rowname'));
  assert.ok(!rows(m.el).includes('stats'));
  row(m.el, 'layout').querySelector('[data-value="variables"]').click();
  assert.deepEqual(env.sent[0].value.values, { layout: 'variables' });
  assert.ok(rows(m.el).includes('stats') && !rows(m.el).includes('rowname'));
});

test('slot(): what a sentence word opens', () => {
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical', func: 'count', value_labels: false, bins: 30 },
    {});
  t.gear.update(payload({ type: 'bar', category: 'ARM', orientation: 'vertical', func: 'count', value_labels: false },
    { derived: { titles: { choices: { func: ['mean', 'median'] } } } }));
  assert.deepEqual(plain(t.gear.slot('value_labels')), { kind: 'flag', value: false, label: 'Value labels' });
  assert.equal(t.gear.slot('bins').kind, 'number');
  assert.equal(t.gear.slot('value_lines').kind, 'entry');
  assert.equal(t.gear.slot('unit'), null);
  const x = t.gear.slot('x');
  assert.equal(x.kind, 'menu');
  assert.equal(x.options[0].label, '(none)');
  const f = t.gear.slot('func');
  assert.deepEqual(plain(f.options.map((o) => o.value)), ['count', 'mean', 'median']);
  assert.equal(t.gear.slot('type').options.length, 12);
  assert.equal(t.gear.slot('selection'), null);
});

test('dispose empties the tray and stops sending', () => {
  const t = setup({ type: 'bar', category: 'ARM', orientation: 'vertical' });
  t.scope.dispose();
  assert.equal(t.el.childNodes.length, 0);
  t.gear.set({ x: 'SEX' });
  assert.equal(t.sent.length, 0);
});

test('the variables layout: Group by, Section by, Individual id, Label and Nest send their keys', () => {
  const env = loadGear();
  const m = mountGear(env, { block: 'table', id: 'tb' });
  m.gear.update(payload({ layout: 'variables', by: [], sections: [], id: null,
                          total: true, total_label: 'Total', nest: false }));
  for (const k of ['by', 'sections', 'id', 'stats', 'total', 'total_label', 'nest']) {
    assert.ok(rows(m.el).includes(k), k + ' is a row');
  }
  const pick = (key, i) => {
    const s = sel(m.el, key);
    s.options[i].selected = true;
    s.dispatchEvent(new env.win.Event('change'));
    return env.sent.at(-1).value.values;
  };
  assert.deepEqual(pick('by', 1), { by: ['ARM'] });
  assert.deepEqual(pick('sections', 2), { sections: ['SEX'] });
  choose(env.win, sel(m.el, 'id'), 'USUBJID (Subject)');
  assert.deepEqual(env.sent.at(-1).value.values, { id: 'USUBJID' });
  const label = inp(m.el, 'total_label');
  label.value = 'All';
  label.dispatchEvent(new env.win.Event('blur'));
  assert.deepEqual(env.sent.at(-1).value.values, { total_label: 'All' });
  const box = row(m.el, 'nest').querySelector('input');
  box.checked = true;
  box.dispatchEvent(new env.win.Event('change'));
  assert.deepEqual(env.sent.at(-1).value.values, { nest: true });
});
