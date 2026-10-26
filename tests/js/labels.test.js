/* A column's label is how the chart names it: every axis name, tooltip
 * row, note, legend title, footer clause and menu that names a column shows
 * its label when it has one (R's `label` attribute, sent as `label` in the
 * column metadata, R/col-meta.R), and its name only when it has none.
 *
 * Every column below carries a label except PLAIN, so a bare name in any
 * output is a column printed without it. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { loadPure, measure } = require('./chart/load');
const I = require('./interact');

const C = loadPure({ roles: true });

const cat = (name, label, extra) => ({ name, type: 'categorical', ...(label ? { label } : {}), ...extra });
const num = (name, label) => ({ name, type: 'numeric', ...(label ? { label } : {}) });
const COLUMNS = [
  cat('AETERM', 'Reported Term'), cat('AESEV', 'Severity'), cat('ARM', 'Planned Arm'),
  cat('USUBJID', 'Subject'), num('ADY', 'Study Day'), num('AVAL', 'Analysis Value'),
  num('ASTDY', 'Start Day'), num('AENDY', 'End Day'), num('ANRHI', 'Upper Limit'),
  // A histogram's bin column, as R adds it: it carries x's label.
  cat('ADY_bin', 'Study Day'),
  cat('PLAIN')
];
const NAMES = ['AETERM', 'AESEV', 'ARM', 'USUBJID', 'ADY', 'AVAL', 'ASTDY', 'AENDY', 'ANRHI',
               'ADY_bin'];
const ROWS = [
  { AETERM: 'Rash', AESEV: 'MILD', ARM: 'A', USUBJID: 'S01', ADY: 1, AVAL: 4, ASTDY: 1, AENDY: 5,
    ANRHI: 8, ADY_bin: '0-5', PLAIN: 'p' },
  { AETERM: 'Rash', AESEV: 'SEVERE', ARM: 'B', USUBJID: 'S02', ADY: 3, AVAL: 6, ASTDY: 2, AENDY: 9,
    ANRHI: 8, ADY_bin: '0-5', PLAIN: 'q' },
  { AETERM: 'Fever', AESEV: 'MILD', ARM: 'A', USUBJID: 'S03', ADY: 7, AVAL: 2, ASTDY: 4, AENDY: 6,
    ANRHI: 8, ADY_bin: '5-10', PLAIN: 'p' },
  { AETERM: 'Nausea', AESEV: 'SEVERE', ARM: 'B', USUBJID: 'S04', ADY: 9, AVAL: 9, ASTDY: 3, AENDY: 8,
    ANRHI: 8, ADY_bin: '5-10', PLAIN: 'q' }
];

/** Fails when `text` names a labelled column by its bare name. */
const noBareName = (text, what) => {
  for (const n of NAMES) {
    assert.ok(!new RegExp('\\b' + n + '\\b').test(String(text)),
              `${what} names ${n} without its label: ${text}`);
  }
};

const look = (cfg, extra = {}) => ({ cfg, columns: COLUMNS, ink: C.INK_DEFAULT, theme: null,
                                     measure, widths: [600, 600], ...extra });
const agg = (cfg, rows = ROWS) => {
  const c = { drill: 'auto', sort_by: 'value', sort_dir: 'desc', ...cfg };
  const m = C.model.aggregated({ rows, ix: new C.RowIndex(), columns: COLUMNS, cfg: c });
  const o = C.option.aggregated(m, look(c));
  return { m, o, opt: o.panels[0].option, c };
};
const ind = (cfg, rows = ROWS) => {
  const c = { drill: 'auto', ...cfg };
  const m = C.model.individual({ rows, ix: new C.RowIndex(), columns: COLUMNS, cfg: c });
  const o = C.option.individual(m, look(c, { widths: [600] }));
  return { m, o, opt: o.panels[0].option, c };
};
const gantt = (cfg) => {
  const c = { chart_type: 'gantt', x: 'ASTDY', xend: 'AENDY', y: 'AETERM', drill: 'auto', ...cfg };
  const m = C.model.timeline({ rows: ROWS, ix: new C.RowIndex(), columns: COLUMNS, cfg: c });
  const o = C.option.timeline(m, look(c));
  return { m, o, opt: o.panels[0].option };
};
const valueAxis = (opt) => (opt.xAxis.type === 'value' ? opt.xAxis : opt.yAxis);

// -- axis names ----------------------------------------------------------------

test('axisTitle: the label, else the name', () => {
  assert.strictEqual(C.axes.axisTitle(COLUMNS, 'AESEV'), 'Severity');
  assert.strictEqual(C.axes.axisTitle(COLUMNS, 'PLAIN'), 'PLAIN');
  assert.strictEqual(C.axes.axisTitle([cat('X', 'X')], 'X'), 'X');
});

test('axisName: the label with the unit; unlabelled, the unit alone, else the name', () => {
  assert.strictEqual(C.axisName('Analysis Value', 'AVAL', { unit: 'mg' }), 'Analysis Value (mg)');
  assert.strictEqual(C.axisName('Analysis Value', 'AVAL', {}), 'Analysis Value');
  assert.strictEqual(C.axisName('PLAIN', 'PLAIN', { unit: 'mg' }), 'mg');
  assert.strictEqual(C.axisName('PLAIN', 'PLAIN', {}), 'PLAIN');
  assert.strictEqual(C.axisName('Analysis Value', 'AVAL',
    { title_resolved: 'Mean analysis value by visit' }), '');
});

test('bar: the value axis by the value column\'s label, a count as "Count"', () => {
  assert.strictEqual(valueAxis(agg({ chart_type: 'bar', group: 'AETERM', value: 'AVAL',
                                     func: 'sum' }).opt).name, 'Analysis Value');
  assert.strictEqual(valueAxis(agg({ chart_type: 'bar', group: 'AETERM', value: '.count',
                                     func: 'count' }).opt).name, 'Count');
  assert.strictEqual(valueAxis(agg({ chart_type: 'bar', group: 'AETERM', value: 'AVAL',
                                     func: 'mean', unit: 'mg' }).opt).name,
                     'Analysis Value (mg)');
});

test('bar: "% of panel" names its denominator by label', () => {
  const { opt } = agg({ chart_type: 'bar', group: 'AETERM', color: 'AESEV', value: 'USUBJID',
                        func: 'pct_distinct', pct_of: ['color'] });
  assert.strictEqual(valueAxis(opt).name, '% of Severity');
  const both = agg({ chart_type: 'bar', group: 'AETERM', color: 'AESEV', facet: 'ARM',
                     value: 'USUBJID', func: 'pct_distinct', pct_of: ['facet', 'color'] });
  assert.strictEqual(valueAxis(both.opt).name, '% of Planned Arm x Severity');
});

test('waterfall: the value axis by label', () => {
  const { opt } = agg({ chart_type: 'bar', baseline: 'cumulative', group: 'AETERM',
                        value: 'AVAL', func: 'sum' });
  assert.strictEqual(opt.yAxis.name, 'Analysis Value');
});

test('boxplot and point range: the value axis by label', () => {
  for (const ct of ['boxplot', 'pointrange']) {
    const { opt } = agg({ chart_type: ct, group: 'AETERM', value: 'AVAL', func: 'mean' });
    assert.strictEqual(valueAxis(opt).name, 'Analysis Value', ct);
  }
});

test('histogram: bins are counted, and the bin column names itself with x\'s label', () => {
  const { opt } = agg({ chart_type: 'histogram', group: 'ADY_bin', value: '.count',
                        func: 'count', sort_by: '' });
  assert.strictEqual(valueAxis(opt).name, 'Count');
  assert.strictEqual(C.axes.axisTitle(COLUMNS, 'ADY_bin'), 'Study Day');
});

test('scatter, line, band: x and y by label', () => {
  for (const ct of ['scatter', 'line']) {
    const { opt } = ind({ chart_type: ct, x: 'ADY', y: 'AVAL' });
    // A line chart carries a second, hidden pair of axes for its veil.
    const first = (a) => (Array.isArray(a) ? a[0] : a);
    assert.strictEqual(first(opt.xAxis).name, 'Study Day', ct);
    assert.strictEqual(first(opt.yAxis).name, 'Analysis Value', ct);
  }
});

test('gantt: the time axis by label', () => {
  assert.strictEqual(gantt({}).opt.xAxis.name, 'Start Day');
});

// -- tooltips ------------------------------------------------------------------

test('bar tooltip: the aggregation by label, the colour rows and tooltip fields too', () => {
  const plainBar = agg({ chart_type: 'bar', group: 'AETERM', value: 'AVAL', func: 'mean',
                         tt_fields: ['USUBJID'] });
  const html1 = plainBar.opt.tooltip.formatter([{ name: 'Fever', value: 2, dataIndex: 1 }]);
  assert.match(html1, /Mean of Analysis Value/);
  noBareName(html1, 'bar tooltip');

  const split = agg({ chart_type: 'bar', group: 'AETERM', color: 'AESEV', value: '.count',
                      func: 'count' });
  const html2 = split.opt.tooltip.formatter({ name: 'Rash', dataIndex: 0, seriesName: 'MILD' });
  assert.match(html2, /Severity MILD/);
  noBareName(html2, 'split bar tooltip');

  const pct = agg({ chart_type: 'bar', group: 'AETERM', color: 'AESEV', bar_mode: 'percent',
                    value: '.count', func: 'count' });
  const html3 = pct.opt.tooltip.formatter({ name: 'Rash', dataIndex: 0, seriesName: 'MILD' });
  assert.match(html3, /Severity MILD/);
});

test('bar tooltip: tooltip fields by label', () => {
  const { opt } = agg({ chart_type: 'bar', group: 'AETERM', value: '.count', func: 'count',
                        tt_fields: ['USUBJID'] });
  const html = opt.tooltip.formatter([{ name: 'Fever', value: 1, dataIndex: 1 }]);
  assert.match(html, /Subject/);
  noBareName(html, 'bar tooltip fields');
});

test('pie and treemap tooltips: the aggregation by label', () => {
  for (const ct of ['pie', 'treemap']) {
    const { opt } = agg({ chart_type: ct, group: 'AETERM', value: 'AVAL', func: 'sum' });
    const html = opt.tooltip.formatter({ name: 'Rash', value: 10, data: { n: 2 }, percent: 50 });
    assert.match(html, /Sum of Analysis Value/, ct);
    noBareName(html, ct + ' tooltip');
  }
});

test('waterfall tooltip: the aggregation by label', () => {
  const { opt } = agg({ chart_type: 'bar', baseline: 'cumulative', group: 'AETERM',
                        value: 'AVAL', func: 'sum' });
  const html = opt.tooltip.formatter([{ seriesName: 'delta', name: 'Rash', value: 10 }]);
  assert.match(html, /Sum of Analysis Value/);
  noBareName(html, 'waterfall tooltip');
});

test('boxplot and point range tooltips: the colour column by label', () => {
  const SEP = C.model.BOX_CAT_SEP;
  for (const ct of ['boxplot', 'pointrange']) {
    const { opt } = agg({ chart_type: ct, group: 'AETERM', color: 'AESEV', value: 'AVAL',
                          func: 'mean' });
    const html = opt.tooltip.formatter({ name: 'Rash' + SEP + 'MILD' });
    assert.match(html, /Severity MILD/, ct);
    noBareName(html, ct + ' tooltip');
  }
});

test('point tooltip: x, y, the split column and the tooltip fields by label', () => {
  const { opt } = ind({ chart_type: 'scatter', x: 'ADY', y: 'AVAL', color: 'AESEV',
                        tt_fields: ['USUBJID'] });
  const html = opt.tooltip.formatter({ seriesName: 'MILD', value: [1, 4, 'S01'], color: '#123' });
  for (const l of ['Study Day', 'Analysis Value', 'Severity', 'Subject']) assert.match(html, new RegExp(l));
  noBareName(html, 'point tooltip');
});

test('band tooltip: the point tooltip, by label', () => {
  const { opt } = ind({ chart_type: 'band', x: 'ADY', y: 'AVAL' });
  const html = opt.tooltip.formatter({ value: [1, 4] });
  assert.match(html, /Study Day/);
  assert.match(html, /Analysis Value/);
  noBareName(html, 'band tooltip');
});

test('line tooltip: x in the headline, the line and its tooltip fields by label', () => {
  const { opt } = ind({ chart_type: 'line', x: 'ADY', y: 'AVAL', tt_fields: ['USUBJID'] });
  const html = opt.tooltip.formatter([{ seriesType: 'line', seriesIndex: 0, seriesName: '',
                                         value: [3, 6, 'S02'] }]);
  assert.match(html, /Study Day: 3/);
  assert.match(html, /Analysis Value/);
  assert.match(html, /Subject: S02/);
  noBareName(html, 'line tooltip');
});

test('gantt tooltip: lane, interval, colour, series and fields by label', () => {
  const { opt } = gantt({ color: 'AESEV', series: 'USUBJID', tt_fields: ['ARM'] });
  const html = opt.tooltip.formatter({ value: [1, 5, 0, 'Rash', 'MILD', '', 'S01', '', ['A']] });
  for (const l of ['Reported Term', 'Start Day', 'End Day', 'Severity', 'Planned Arm']) {
    assert.match(html, new RegExp(l));
  }
  noBareName(html, 'gantt tooltip');
});

// -- notes on the chart ----------------------------------------------------------

test('a thinned category axis names its column by label', () => {
  const rows = Array.from({ length: 300 }, (_, i) => ({ ...ROWS[0], AETERM: 'T' + i }));
  const { o } = agg({ chart_type: 'bar', group: 'AETERM', value: '.count', func: 'count' }, rows);
  assert.match(o.labelNote, /Reported Term labelled/);
  noBareName(o.labelNote, 'thinned-axis note');
  const box = agg({ chart_type: 'boxplot', group: 'AETERM', value: 'AVAL', func: 'mean',
                    orientation: 'horizontal' }, rows);
  assert.match(box.o.labelNote, /Reported Term labelled/);
});

test('a line chart past its series cap names the series column by label', () => {
  const rows = Array.from({ length: 1001 }, (_, i) => ({ ...ROWS[0], USUBJID: 'S' + i }));
  const { m } = ind({ chart_type: 'line', x: 'ADY', y: 'AVAL', series: 'USUBJID' }, rows);
  assert.match(m.capMessage, /\(Subject\)/);
  noBareName(m.capMessage, 'series cap note');
});

test('a band\'s reference line names its column by label', () => {
  const cfg = { band_refs: { hi: { col: 'ANRHI', value: 8, lo: 8, hi: 8, n_distinct: 1 } } };
  const lines = C.option.indParts.bandRefLines(cfg, null, C.INK_DEFAULT, COLUMNS);
  assert.strictEqual(lines[0].label.formatter, 'Upper Limit 8');
});

// -- the footer ------------------------------------------------------------------

test('the filter line names columns by label, the histogram bin by x\'s label', () => {
  assert.strictEqual(C.keys.describe({ AETERM: ['Rash'], AESEV: ['MILD'] }, COLUMNS),
                     'Reported Term = Rash, Severity = MILD');
  assert.strictEqual(C.keys.describe({ ADY_bin: ['0-5'] }, COLUMNS), 'Study Day = 0-5');
  assert.strictEqual(C.keys.describe({ PLAIN: ['p'] }, COLUMNS), 'PLAIN = p');
  assert.strictEqual(
    C.keys.describeRange({ x_col: 'ADY', y_col: 'AVAL', x_range: [1, 7], y_range: [2, 9] },
                         COLUMNS, 'value', { ARM: ['A'] }),
    'Study Day 1 to 7, Analysis Value 2 to 9, Planned Arm = A');
});

// -- legend, facets, gear and sentence, drawn ------------------------------------

const DRAWN = { chart_type: 'bar', group: 'AETERM', value: '.count', func: 'count', drill: 'auto' };
const draw = (config) => I.open({ ...DRAWN, ...config }, { data: ROWS, columns: COLUMNS });

test('drawn: the legend title is the colour column\'s label, none without one', () => {
  const c = draw({ color: 'AESEV' });
  const t = c.block.legendEl.querySelector('.dd-legend-title');
  assert.strictEqual(t && t.textContent, 'Severity');
  c.close();
  const p = draw({ color: 'PLAIN' });
  assert.strictEqual(p.block.legendEl.querySelector('.dd-legend-title'), null);
  p.close();
});

test('drawn: facet strips show the facet values, never the column', () => {
  const c = draw({ facet: 'ARM' });
  const strips = Array.from(c.block.el.querySelectorAll('.dd-facet-label'), (e) => e.textContent);
  assert.deepStrictEqual(strips, ['A', 'B']);
  c.close();
});

test('drawn: a click names its filter by label in the footer', () => {
  const c = draw({});
  c.clickWhere(0, (p) => p.name === 'Rash');
  const text = c.footer().map((f) => f.text).join(' ');
  assert.match(text, /Reported Term = Rash/);
  noBareName(text, 'footer');
  c.close();
});

test('drawn: the gear\'s column menus show each label', () => {
  const c = draw({ color: 'AESEV' });
  c.block._openPopover();
  const sel = I.roleRow(c.block, 'color').querySelector('.blockr-select');
  sel.querySelector('.blockr-select__control').click();
  const lb = c.doc.getElementById(sel.querySelector('.blockr-select__search')
    .getAttribute('aria-controls'));
  const opts = Array.from(lb.querySelectorAll('.blockr-select__option'));
  const sev = opts.find((o) => o.getAttribute('data-value') === 'AESEV');
  assert.ok(sev, 'AESEV is offered');
  assert.match(sev.textContent, /Severity/);
  c.close();
});

test('drawn: a word printed from a label opens a list led by labels', () => {
  const c = draw({
    color: 'AESEV',
    subtitle_resolved: 'by Severity',
    subtitle_parts: [{ text: 'by ' }, { text: 'Severity', arg: 'color', by: 'label' }]
  });
  const w = Array.from(c.block.subtitleEl.querySelectorAll('.blockr-slot'))
    .find((x) => x.textContent === 'Severity');
  w.click();
  const menus = c.doc.querySelectorAll('.blockr-select__dropdown--menu');
  const menu = menus[menus.length - 1];
  const opt = Array.from(menu.querySelectorAll('.blockr-select__option'))
    .find((o) => o.getAttribute('data-value') === 'AESEV');
  assert.ok(opt, 'AESEV is in the menu');
  const text = opt.textContent;
  assert.ok(text.indexOf('Severity') >= 0 && text.indexOf('Severity') < Math.max(0, text.indexOf('AESEV')),
            'the label leads: ' + text);
  c.close();
});

test('the subtitle placeholder writes its columns as labels', () => {
  const ph = C.roles.ROLES.subtitle.ph;
  assert.match(ph, /\{label\(@color\)\}/);
  assert.ok(!/\{@color\}/.test(ph));
});
