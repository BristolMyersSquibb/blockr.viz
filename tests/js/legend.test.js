/* The legend setting: where the band sits, its title and swatch, and what
 * "marks" does per chart type (common.js legendPlan, chrome.js
 * _updateLegendBand, the option builders). */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const h = require('./harness');
const { F } = require('./cases');
const { loadPure, plain, measure } = require('./chart/load');

const BASE = {
  value: '.count', func: 'count', bar_mode: 'stacked', value_labels: 'off',
  line_width_mult: 1, dot_size_mult: 1, connect: 'monotone', value_lines: [], x_lines: [],
  smoother: 'none', identity_line: 'off', box_points: 'none', connect_centers: 'off',
  count_on: 'off', na_group: 'level', facet_scales: 'fixed', download: 'off',
  waterfall_totals: [], filter_values: [], drill: 'auto'
};
const AE = { group: 'AETERM' };
const LAB = { x: 'ADY', y: 'AVAL' };

/** Draw a config and keep the window open for the test. */
const draw = (config) => h.draw({ ...BASE, ...config }, F.data, F.columns, { keepOpen: true });

/** The last full option each panel was given. */
const lastOption = (r, slot = 0) => {
  // The full option, not a highlight patch after it.
  const calls = r.options[slot].calls.filter((c) => c.op === 'setOption' &&
    c.option.series && c.option.series.some((s) => s.type));
  return calls[calls.length - 1].option;
};
const band = (r) => (r.block.legendEl.style.display === 'none' ? null : r.block.legendEl);
const markNames = (opt) => opt.series.filter((s) => String(s.id || '').startsWith('__legend_'))
  .map((s) => s.label.formatter);

// -- the band: position, title, swatch ----------------------------------------

test('the band sits below the plot by default and above it with legend = "top"', () => {
  const below = draw({ chart_type: 'bar', ...AE, color: 'AESEV' });
  const el = band(below);
  assert.ok(el);
  assert.strictEqual(el.previousElementSibling, below.block.chartGrid);
  assert.strictEqual(el.nextElementSibling, below.block.captionEl);
  below.env.close();

  const above = draw({ chart_type: 'bar', ...AE, color: 'AESEV', legend: 'top' });
  const top = band(above);
  assert.strictEqual(top.nextElementSibling, above.block.chartGrid);
  assert.ok(top.classList.contains('dd-legend-band--top'));
  // The title band is the header's, which comes before.
  assert.ok(top.compareDocumentPosition(above.block.chartGrid) & 4);
  assert.ok(above.block.card.firstElementChild.contains(above.block.titleEl));
  above.env.close();
});

test('the band moves back below when the setting returns to bottom', () => {
  const config = { ...BASE, chart_type: 'bar', ...AE, color: 'AESEV', legend: 'top' };
  const r = h.draw(config, F.data, F.columns, { keepOpen: true });
  assert.strictEqual(band(r).nextElementSibling, r.block.chartGrid);
  h.send(r.env, r.id, { columns: F.columns, data: F.data, config: { ...config, legend: 'bottom' } });
  assert.strictEqual(band(r).previousElementSibling, r.block.chartGrid);
  assert.ok(!band(r).classList.contains('dd-legend-band--top'));
  r.env.close();
});

test('legend = "none" draws no band', () => {
  const r = draw({ chart_type: 'bar', ...AE, color: 'AESEV', legend: 'none' });
  assert.strictEqual(band(r), null);
  r.env.close();
});

test('the title shows for a labelled colour column only', () => {
  // ARM has a label in the fixture, AESEV has none.
  const labelled = draw({ chart_type: 'bar', ...AE, color: 'ARM' });
  assert.strictEqual(band(labelled).querySelector('.dd-legend-title').textContent, 'Planned Arm');
  labelled.env.close();
  const plainCol = draw({ chart_type: 'bar', ...AE, color: 'AESEV' });
  assert.strictEqual(band(plainCol).querySelector('.dd-legend-title'), null);
  plainCol.env.close();
});

test('the swatch follows the mark', () => {
  const swatchOf = (config) => {
    const r = draw(config);
    const el = band(r);
    const cls = el.querySelector('.dd-legend-swatch').className;
    r.env.close();
    return cls.replace('dd-legend-swatch', '').trim();
  };
  assert.strictEqual(swatchOf({ chart_type: 'bar', ...AE, color: 'AESEV' }), 'dd-legend-swatch--square');
  assert.strictEqual(swatchOf({ chart_type: 'line', ...LAB, color: 'SEX' }), 'dd-legend-swatch--line');
  assert.strictEqual(swatchOf({ chart_type: 'area', ...LAB, color: 'SEX' }), 'dd-legend-swatch--square');
  assert.strictEqual(swatchOf({ chart_type: 'scatter', ...LAB, color: 'ARM' }), 'dd-legend-swatch--dot');
  assert.strictEqual(swatchOf({ chart_type: 'pointrange', group: 'AVISIT', color: 'ARM', value: 'AVAL' }),
                     'dd-legend-swatch--dot');
  assert.strictEqual(swatchOf({ chart_type: 'band', ...LAB, color: 'ARM', band_series: F.band_series.color,
                                whiskers: 'p10_p90' }), 'dd-legend-swatch--line');
  assert.strictEqual(swatchOf({ chart_type: 'boxplot', group: 'AVISIT', color: 'ARM', value: 'AVAL' }),
                     'dd-legend-swatch--square');
});

test('a chip still filters its level, and a second click sends again', () => {
  const r = draw({ chart_type: 'bar', ...AE, color: 'AESEV', legend: 'top' });
  const chip = band(r).querySelector('.dd-legend-chip');
  const sent = () => r.env.win.__inputs.filter((i) => /_action$|_filter$/.test(i.name)).length;
  const before = sent();
  chip.click();
  chip.click();
  assert.ok(sent() >= before + 2);
  r.env.close();
});

// -- marks: the line family ---------------------------------------------------

test('marks on a line: end labels, room on the right, no band', () => {
  const r = draw({ chart_type: 'line', ...LAB, color: 'SEX', legend: 'marks' });
  const opt = lastOption(r);
  const ended = opt.series.filter((s) => s.endLabel && s.endLabel.show);
  assert.ok(ended.length >= 2);
  assert.deepStrictEqual(ended[0].labelLayout, { moveOverlap: 'shiftY' });
  assert.ok(opt.grid.right > 5);
  assert.strictEqual(band(r), null);
  r.env.close();
});

test('the old direct_labels = "on" means marks', () => {
  const r = draw({ chart_type: 'line', ...LAB, color: 'SEX', direct_labels: 'on' });
  assert.ok(lastOption(r).series.some((s) => s.endLabel && s.endLabel.show));
  assert.strictEqual(band(r), null);
  r.env.close();
});

test('marks on a crowd of lines keep the band', () => {
  const r = draw({ chart_type: 'line', ...LAB, series: 'USUBJID', color: 'ARM', legend: 'marks' });
  assert.ok(!lastOption(r).series.some((s) => s.endLabel));
  assert.ok(band(r));
  r.env.close();
});

test('marks on an area name the bands at their end', () => {
  const r = draw({ chart_type: 'area', ...LAB, color: 'SEX', legend: 'marks' });
  assert.ok(lastOption(r).series.some((s) => s.areaStyle && s.endLabel && s.endLabel.show));
  assert.strictEqual(band(r), null);
  r.env.close();
});

test('marks on a band chart name the centre lines', () => {
  const r = draw({ chart_type: 'band', ...LAB, color: 'ARM', band_series: F.band_series.color,
                   whiskers: 'p10_p90', legend: 'marks' });
  assert.ok(lastOption(r).series.some((s) => s.endLabel && s.endLabel.show));
  assert.strictEqual(band(r), null);
  r.env.close();
});

test('marks on a scatter write each colour group at its centre of mass', () => {
  const r = draw({ chart_type: 'scatter', ...LAB, color: 'ARM', legend: 'marks' });
  const opt = lastOption(r);
  const levels = opt.series.filter((s) => s.type === 'scatter' && s.id == null && s.name)
    .map((s) => s.name);
  assert.ok(levels.length >= 2);
  assert.deepStrictEqual(markNames(opt).sort(), levels.slice().sort());
  const first = opt.series.find((s) => s.name === markNames(opt)[0]);
  const mark = opt.series.find((s) => s.label && s.label.formatter === first.name);
  const mx = first.data.reduce((a, p) => a + Number(p[0]), 0) / first.data.length;
  assert.ok(Math.abs(mark.data[0][0] - mx) < 1e-6);
  assert.strictEqual(mark.silent, true);
  assert.ok(mark.label.textBorderWidth > 0);
  assert.strictEqual(band(r), null);
  r.env.close();
});

// -- marks: bars ---------------------------------------------------------------

const C = loadPure({ roles: true });
const COLUMNS = [{ name: 'T', type: 'categorical' }, { name: 'SEV', type: 'categorical' },
                 { name: 'V', type: 'numeric' }];
const ROWS = [
  { T: 'a', SEV: 'LOW', V: 2 }, { T: 'a', SEV: 'HIGH', V: 4 },
  { T: 'b', SEV: 'LOW', V: 3 }, { T: 'b', SEV: 'HIGH', V: 1 },
  { T: 'c', SEV: 'LOW', V: 5 }
];
const build = (cfg) => {
  const c = { ...BASE, chart_type: 'bar', group: 'T', value: 'V', func: 'sum', color: 'SEV',
              sort_by: 'alpha', sort_dir: 'asc', ...cfg };
  const m = C.model.aggregated({ rows: ROWS, ix: new C.RowIndex(), columns: COLUMNS, cfg: c });
  const o = C.option.aggregated(m, { cfg: c, columns: COLUMNS, ink: C.INK_DEFAULT, theme: null,
                                     measure, widths: [600] });
  return { m, c, plan: C.legendPlan(m, c), opt: o.panels[0].option };
};

test('marks on a vertical stack whose last bar lacks a level keep the legend', () => {
  // The last bar, c, holds LOW only: HIGH would have no name.
  const { plan, opt } = build({ orientation: 'vertical', legend: 'marks' });
  assert.strictEqual(plan.marks, null);
  assert.strictEqual(plan.band, true);
  assert.strictEqual(plan.position, 'bottom');
  assert.deepStrictEqual(plain(opt.series.filter((s) => String(s.id || '').startsWith('__legend_'))), []);
});

test('marks on a vertical stack: a name at each segment of the last bar', () => {
  const { plan, opt } = build({ orientation: 'vertical', legend: 'marks',
                                sort_by: 'alpha', sort_dir: 'desc' });
  assert.strictEqual(plan.marks, 'stack');
  assert.strictEqual(plan.band, false);
  // Now the last bar is a: HIGH 4 then LOW 2 (series order HIGH, LOW).
  const names = opt.series.filter((s) => String(s.id || '').startsWith('__legend_'));
  assert.deepStrictEqual(plain(names.map((s) => [s.label.formatter, s.data[0]])),
                         [['HIGH', ['a', 2]], ['LOW', ['a', 5]]]);
  assert.strictEqual(names[0].label.position, 'right');
  assert.ok(opt.grid.right >= 10);
  // The bars themselves are untouched.
  assert.deepStrictEqual(plain(opt.series.slice(0, 2).map((s) => s.name)), ['HIGH', 'LOW']);
});

test('marks on a stack spread the names of thin segments a line apart', () => {
  const rows = [];
  const add = (t, sev, n) => { for (let i = 0; i < n; i++) rows.push({ T: t, SEV: sev, V: 1, ID: t + sev + i, ARM: 'A' }); };
  add('a', 'HIGH', 40); add('a', 'LOW', 40); add('a', 'MID', 1);
  add('b', 'HIGH', 1); add('b', 'LOW', 1); add('b', 'MID', 1);
  const c = { ...BASE, chart_type: 'bar', group: 'T', value: '.count', func: 'count', color: 'SEV',
              sort_by: 'alpha', sort_dir: 'asc', orientation: 'vertical', legend: 'marks' };
  const m = C.model.aggregated({ rows, ix: new C.RowIndex(), columns: COLUMNS, cfg: c });
  const opt = C.option.aggregated(m, { cfg: c, columns: COLUMNS, ink: C.INK_DEFAULT, theme: null,
                                       measure, widths: [600] }).panels[0].option;
  const at = plain(opt.series.filter((s) => String(s.id || '').startsWith('__legend_'))
    .map((s) => [s.label.formatter, s.data[0][1]]));
  // The last bar, b, has three segments of 1: their middles 0.5, 1.5, 2.5
  // sit closer than a line on an axis up to 81, so they spread, top down.
  assert.strictEqual(at.length, 3);
  const ys = at.map((x) => x[1]).sort((p, q) => q - p);
  const line = (12 + 3) * 81 / (350 - 30 - 66);
  for (let k = 1; k < ys.length; k++) assert.ok(ys[k - 1] - ys[k] >= line - 1e-9);
  assert.ok(Math.min(...ys) >= line / 2 - 1e-9);
});

test('marks on a horizontal stack: names above the first bar, at each centre', () => {
  const { plan, opt } = build({ orientation: 'horizontal', legend: 'marks', bar_mode: 'percent' });
  assert.strictEqual(plan.marks, 'stack-h');
  const names = opt.series.filter((s) => String(s.id || '').startsWith('__legend_'));
  assert.deepStrictEqual(plain(names.map((s) => [s.label.formatter, s.label.position])),
                         [['HIGH', 'top'], ['LOW', 'top']]);
  // The first bar is a: shares 2/3 and 1/3.
  assert.ok(Math.abs(names[0].data[0][0] - 1 / 3) < 1e-9);
  assert.strictEqual(names[0].data[0][1], 'a');
});

test('marks on a dodged bar, a histogram and a boxplot fall back to the band', () => {
  const dodged = build({ orientation: 'vertical', bar_mode: 'grouped', legend: 'marks' });
  assert.strictEqual(dodged.plan.marks, null);
  assert.strictEqual(dodged.plan.band, true);
  assert.strictEqual(dodged.plan.position, 'bottom');
  assert.ok(!dodged.opt.series.some((s) => String(s.id || '').startsWith('__legend_')));

  assert.strictEqual(build({ chart_type: 'boxplot', legend: 'marks' }).plan.band, true);

  // Through the view: the band stays at the bottom.
  const r = draw({ chart_type: 'bar', ...AE, color: 'AESEV', bar_mode: 'grouped',
                   orientation: 'vertical', legend: 'marks' });
  assert.ok(band(r));
  assert.strictEqual(band(r).previousElementSibling, r.block.chartGrid);
  r.env.close();
});

test('marks fall back to the band, at the bottom, even when the setting was top', () => {
  const r = draw({ chart_type: 'bar', ...AE, color: 'AESEV', bar_mode: 'grouped', legend: 'marks' });
  assert.strictEqual(band(r).previousElementSibling, r.block.chartGrid);
  r.env.close();
});

// Every term with both grades: the last bar carries each level.
const CROSS = {
  data: { T: ['a', 'a', 'b', 'b', 'b'], G: ['HIGH', 'LOW', 'HIGH', 'LOW', 'LOW'] },
  columns: [{ name: 'T', type: 'categorical', n_unique: 2 }, { name: 'G', type: 'categorical', n_unique: 2 }]
};

test('marks through the view on a stacked bar hide the band', () => {
  const r = h.draw({ ...BASE, chart_type: 'bar', group: 'T', color: 'G', orientation: 'vertical',
                     legend: 'marks' }, CROSS.data, CROSS.columns, { keepOpen: true });
  try {
    assert.strictEqual(band(r), null);
    assert.deepStrictEqual(plain(markNames(lastOption(r))), ['HIGH', 'LOW']);
  } finally { r.env.close(); }
});

test('marks through the view keep the band where the last bar lacks a level', () => {
  // No adverse event term in the fixture has every grade.
  const r = draw({ chart_type: 'bar', ...AE, color: 'AESEV', orientation: 'vertical', legend: 'marks' });
  try {
    assert.ok(band(r));
    assert.deepStrictEqual(plain(markNames(lastOption(r))), []);
  } finally { r.env.close(); }
});

test('marks on a point range write the levels over the points', () => {
  const r = draw({ chart_type: 'pointrange', group: 'AVISIT', color: 'ARM', value: 'AVAL',
                   orientation: 'vertical', legend: 'marks' });
  assert.ok(markNames(lastOption(r)).length >= 2);
  assert.strictEqual(band(r), null);
  r.env.close();
});

test('marks on a pie need no band: the slices name themselves', () => {
  const r = draw({ chart_type: 'pie', ...AE, legend: 'marks' });
  assert.strictEqual(band(r), null);
  r.env.close();
});

// -- the setting --------------------------------------------------------------

test('legendSetting: the value, bottom when missing, marks for an old direct_labels', () => {
  assert.strictEqual(C.legendSetting({}), 'bottom');
  assert.strictEqual(C.legendSetting({ legend: 'top' }), 'top');
  assert.strictEqual(C.legendSetting({ legend: 'bogus' }), 'bottom');
  assert.strictEqual(C.legendSetting({ direct_labels: 'on' }), 'marks');
  assert.strictEqual(C.legendSetting({ direct_labels: true }), 'marks');
  assert.strictEqual(C.legendSetting({ direct_labels: 'off' }), 'bottom');
  assert.strictEqual(C.legendSetting({ legend: 'none', direct_labels: 'on' }), 'none');
});

test('the gear offers Legend for the types with a colour legend', () => {
  const spec = C.roles.FAMILY_ROLES;
  const has = (fam, type) => (spec[fam].presentation || []).some((e) =>
    (typeof e === 'string' ? e === 'legend' : e.role === 'legend' && (!e.types || e.types.includes(type))));
  for (const t of ['bar', 'boxplot', 'pointrange', 'radar']) assert.ok(has('aggregated', t), t);
  for (const t of ['scatter', 'line', 'area', 'band']) assert.ok(has('individual', t), t);
  assert.ok(has('histogram', 'histogram'));
  assert.ok(has('timeline', 'gantt'));
  assert.ok(!has('aggregated', 'pie'));
  const role = C.roles.ROLES.legend;
  assert.deepStrictEqual(plain(role.options.map((o) => o.value)), ['bottom', 'top', 'marks', 'none']);
  assert.strictEqual(role.when({ color: 'ARM' }), true);
  assert.strictEqual(role.when({}), false);
});
