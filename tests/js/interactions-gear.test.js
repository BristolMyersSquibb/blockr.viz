/* The gear: what it shows for each chart type (sections, rows, control
 * kinds, current values, what each select offers), and what changing a
 * control through the DOM sends to R (`<id>_action`, `action: "config"`
 * and `"set_mults"`) and redraws. */
'use strict';

const test = require('node:test');
const I = require('./interact');

const AE = { group: 'AETERM', drill: 'auto' };
const LAB = { x: 'ADY', y: 'AVAL', drill: 'auto' };
const GANTT = { chart_type: 'gantt', x: 'ASTDY', xend: 'AENDY', y: 'AETERM', drill: 'auto' };

// What R always sends beside the chart's own settings (build_chart_msg()).
const R = {
  script: '', ctrl_target: '', ctrl_table: '',
  ctrl_choices: [{ value: 'auto', label: 'Automatic (Filter AE)' },
                 { value: 'flt_ae', label: 'Filter AE' }]
};

const OUTLINES = {
  bar: { chart_type: 'bar', ...AE, color: 'AESEV' },
  'bar-facet-counts': { chart_type: 'bar', ...AE, facet: 'SEX', value: 'USUBJID',
                        func: 'pct_distinct', pct_of: 'facet', count_on: 'axis',
                        count_col: 'USUBJID', facet_cols: '2' },
  'bar-drill-off': { chart_type: 'bar', group: 'AETERM', drill: '' },
  'bar-drill-column-ctrl': { chart_type: 'bar', group: 'AETERM', drill: 'USUBJID',
                             ctrl_target: 'flt_ae', ctrl_table: 'adae' },
  'bar-ctrl-missing-target': { chart_type: 'bar', ...AE, ctrl_target: 'gone', ctrl_choices: [] },
  waterfall: { chart_type: 'waterfall', group: 'AVISIT', value: 'CHG', func: 'sum', drill: 'auto' },
  pie: { chart_type: 'pie', group: 'AESEV', drill: 'auto' },
  treemap: { chart_type: 'treemap', group: 'AETERM', value: 'AVAL', func: 'sum', drill: 'auto' },
  radar: { chart_type: 'radar', group: 'AVISIT', color: 'ARM', value: 'AVAL', func: 'mean',
           drill: 'auto' },
  boxplot: { chart_type: 'boxplot', group: 'AVISIT', color: 'ARM', value: 'AVAL',
             box_points: 'outliers', drill: 'auto' },
  pointrange: { chart_type: 'pointrange', group: 'AVISIT', value: 'AVAL', connect_centers: 'on',
                drill: 'auto' },
  scatter: { chart_type: 'scatter', ...LAB, color: 'ARM', smoother: 'lm',
             smoother_series: I.F.smoother_series.color },
  'scatter-facet': { chart_type: 'scatter', ...LAB, facet: 'SEX', facet_scales: 'free_y',
                     count_on: 'facet', count_col: 'USUBJID' },
  line: { chart_type: 'line', ...LAB, series: 'USUBJID', color: 'ARM', lo: 'LO', hi: 'HI',
          line_width_mult: 1.5 },
  band: { chart_type: 'band', ...LAB, color: 'ARM', band_series: I.F.band_series.color,
          whiskers: 'p10_p90' },
  gantt: { ...GANTT, color: 'AESEV', label: 'AETERM', tt_fields: ['USUBJID'] },
  'gantt-defaults': { chart_type: 'gantt' },
  'bar-defaults': { chart_type: 'bar' }
};

for (const [name, cfg] of Object.entries(OUTLINES)) {
  test(`outline: ${name}`, () => {
    const c = I.open({ ...R, ...cfg });
    try {
      c.block.gearBtn.click();
      const m = c.mark();
      const outline = I.gearOutline(c.block, c.env);
      I.snap('gear-' + name, {
        config: cfg,
        outline,
        // Reading the gear (opening and closing its selects) sends nothing.
        sent: c.since(m).inputs
      });
    } finally { c.close(); }
  });
}

test('outline: Titles fold opened, Prepare script fold closed', () => {
  const c = I.open({ ...R, chart_type: 'bar', ...AE, title: 'AEs by {ARM}', subtitle: null,
                     caption: '', title_resolved: 'AEs by Placebo',
                     subtitle_resolved: 'Count of rows by Reported Term',
                     script: 'data <- data\n# two lines' });
  try {
    c.block.gearBtn.click();
    const folds = () => Array.from(c.block.popoverEl.querySelectorAll('.dd-section-title--fold'));
    const before = I.gearOutline(c.block);
    folds().find((f) => f.textContent.includes('Titles')).click();
    folds().find((f) => f.textContent.includes('Prepare script')).click();
    const m = c.mark();
    const after = I.gearOutline(c.block);
    I.snap('gear-folds', { before, after, sent: c.since(m).inputs });
  } finally { c.close(); }
});

test('outline: script controls on the band', () => {
  const c = I.open({
    ...R, chart_type: 'bar', ...AE, script: 'top_n <- 5\narm <- factor("Placebo", lv)',
    script_inputs: [
      { name: 'top_n', key: 'sv_top_n', label: 'Top n', kind: 'number', min: 1, max: 20 },
      { name: 'arm', key: 'sv_arm', label: 'Arm', kind: 'select',
        options: ['Placebo', 'Low Dose', 'High Dose'] },
      { name: 'arms', key: 'sv_arms', label: 'Arms', kind: 'multi',
        options: ['Placebo', 'Low Dose', 'High Dose'] },
      { name: 'flag', key: 'sv_flag', label: 'Serious only', kind: 'segmented',
        options: [{ value: 'on', label: 'Serious only' }, { value: 'off', label: 'Serious only' }] },
      { name: 'bad', key: 'sv_bad', label: 'Bad', kind: 'error', error: 'no such level' }
    ],
    sv_top_n: 5, sv_arm: 'Placebo', sv_arms: ['Placebo'], sv_flag: 'off'
  });
  try {
    I.snap('gear-script-band', { band: I.bandOutline(c.block, c.env) });
  } finally { c.close(); }
});

// -- driving controls --------------------------------------------------------

const gearOpen = () => ['open the gear', (c) => c.block.gearBtn.click()];
const gearClose = () => ['close the gear', (c) => c.block.gearBtn.click()];
const sel = (role, value) => [`select ${role} = ${value}`,
  (c) => I.pickSelect(c.env, I.roleRow(c.block, role), value)];
const seg = (role, label) => [`segment ${role} = ${label}`,
  (c) => I.pickSegment(I.roleRow(c.block, role), label)];
const check = (role) => [`toggle checkbox ${role}`,
  (c) => I.toggleCheckbox(c.env, I.roleRow(c.block, role))];
const text = (role, value) => [`type ${role} = ${JSON.stringify(value)} + Enter`,
  (c) => I.typeText(c.env, I.roleRow(c.block, role).querySelector('input.blockr-text-input'), value)];
const remove = (role) => [`remove ${role}`,
  (c) => I.roleRow(c.block, role).querySelector('.dd-role-remove').click()];
const add = (label) => [`add mapping ${label}`, (c) => {
  const trig = c.block.popoverEl.querySelector('.dd-add-trigger');
  trig.click();
  const item = Array.from(c.block.popoverEl.querySelectorAll('.dd-add-item'))
    .find((x) => x.textContent === label);
  if (!item) throw new Error(`no add item ${label}`);
  item.click();
}];
const tile = (t) => [`type tile ${t}`, (c) => {
  const b = Array.from(c.block.popoverEl.querySelectorAll('.dd-type-tile'))
    .find((x) => I.txt(x) === t);
  b.click();
}];
const wait = (ms) => [`advance ${ms} ms`, (c) => c.advance(ms)];
const echo = () => ['R echoes the config', (c) => c.echo()];
const drillCheck = () => ['toggle Filter downstream on a click', (c) =>
  I.toggleCheckbox(c.env, I.section(c.block, 'Drill-down').querySelector('.dd-section-enable'))];
const drillOn = (value) => [`Filter on = ${value}`, (c) => {
  const rows = Array.from(I.section(c.block, 'Drill-down').querySelectorAll('.dd-form-row'));
  const r = rows.find((x) => I.txt(x.querySelector('.blockr-label')) === 'Filter on');
  I.pickSelect(c.env, r, value);
}];
const ctrlCheck = () => ['toggle Send to filter', (c) =>
  I.toggleCheckbox(c.env, I.section(c.block, 'Drill-down').querySelector('.dd-ctrl-toggle'))];
const ctrlRow = (label) => (c) => Array.from(I.section(c.block, 'Drill-down')
  .querySelectorAll('.dd-form-row')).find((x) => I.txt(x.querySelector('.blockr-label')) === label);
const ctrlTarget = (v) => [`Target filter = ${v}`, (c) => I.pickSelect(c.env, ctrlRow('Target filter')(c), v)];
const ctrlTable = (v) => [`Table = ${v} (change)`, (c) => {
  const inp = ctrlRow('Table')(c).querySelector('input');
  inp.value = v;
  inp.dispatchEvent(new c.env.win.Event('change'));
}];
const slider = (role, v) => [`slider ${role} = ${v}`, (c) => {
  const inp = I.roleRow(c.block, role).querySelector('input.dd-slider');
  inp.value = String(v);
  inp.dispatchEvent(new c.env.win.Event('input'));
}];

const scenario = (name, config, steps, opts = {}) =>
  test(`drive: ${name}`, () => I.snap('gear-drive-' + name, I.runSteps({ ...R, ...config }, steps, {
    ...opts,
    extra: (c) => ({ gear: I.gearLines(c.block), awaitData: !!c.block._awaitData })
  })));

scenario('bar', { chart_type: 'bar', ...AE, color: 'AESEV' }, [
  gearOpen(),
  sel('sort_by', 'A–Z'),
  seg('sort_dir', 'Ascending'),
  seg('orientation', 'Vertical'),
  seg('bar_mode', '100%'),
  check('value_labels'),
  seg('baseline', 'Waterfall'),
  text('value_lines', '5, 12'),
  sel('count_on', 'On category axis'),
  wait(0),
  sel('count_col', 'USUBJID'),
  check('download'),
  sel('na_group', 'Not a category'),
  wait(300),
  gearClose()
]);

scenario('bar-mapping', { chart_type: 'bar', ...AE, color: 'AESEV' }, [
  gearOpen(),
  sel('group', 'AVISIT'),
  remove('color'),
  wait(0),
  add('Facet'),
  wait(0),
  sel('facet', 'SEX'),
  echo(),
  sel('facet_cols', '1 per row'),
  sel('facet_scales', 'Free per panel'),
  sel('func', 'Mean'),
  echo(),
  sel('value', 'AVAL'),
  sel('func', '% of panel'),
  echo(),
  sel('pct_of', 'Group'),
  add('Tooltip fields'),
  wait(0)
]);

scenario('bar-drill', { chart_type: 'bar', ...AE }, [
  gearOpen(),
  drillOn('USUBJID'),
  drillCheck(),
  drillCheck(),
  ctrlCheck(),
  ctrlTarget('flt_ae'),
  ctrlTable('adae'),
  ctrlCheck()
]);

scenario('bar-selection-then-mapping', { chart_type: 'bar', ...AE }, [
  ['click Nausea', (c) => c.clickWhere(0, (p) => p.name === 'Nausea')],
  gearOpen(),
  // A column pick clears the selection and sends a clear filter after the
  // config.
  sel('group', 'AESEV'),
  echo()
]);

scenario('type-switch', { chart_type: 'bar', ...AE, color: 'AESEV' }, [
  gearOpen(),
  tile('pie'),
  wait(0),
  tile('boxplot'),
  wait(0),
  tile('scatter'),
  wait(0),
  tile('gantt'),
  wait(0),
  tile('bar'),
  wait(0)
]);

scenario('boxplot', { chart_type: 'boxplot', group: 'AVISIT', value: 'AVAL', drill: 'auto' }, [
  gearOpen(),
  sel('summary', 'Mean ± SD'),
  sel('whiskers', 'Min–Max'),
  seg('box_points', 'Outliers'),
  tile('pointrange'),
  wait(0),
  check('connect_centers')
]);

scenario('scatter', { chart_type: 'scatter', ...LAB }, [
  gearOpen(),
  sel('smoother', 'loess'),
  check('identity_line'),
  text('x_lines', '28'),
  text('value_lines', ''),
  sel('x', 'ASTDY'),
  add('Color'),
  wait(0),
  sel('color', 'ARM'),
  slider('dot_size_mult', 2),
  slider('dot_size_mult', 2.5),
  wait(149),
  wait(1)
]);

scenario('line', { chart_type: 'line', ...LAB, series: 'USUBJID' }, [
  gearOpen(),
  sel('connect', 'Step at end'),
  sel('lo', 'LO'),
  slider('line_width_mult', 2),
  wait(150)
]);

scenario('band', { chart_type: 'band', ...LAB, color: 'ARM', band_series: I.F.band_series.color,
                   whiskers: 'p10_p90' }, [
  gearOpen(),
  sel('band_window', 'Fixed (x units)'),
  wait(0),
  text('band_size', '30'),
  text('band_min_n', '5'),
  sel('ref_hi', 'HI')
]);

scenario('gantt', { ...GANTT }, [
  gearOpen(),
  sel('sort_by', 'A–Z'),
  seg('sort_dir', 'Descending'),
  sel('xend', 'ADY'),
  add('Label'),
  wait(0),
  sel('label', 'AESEV')
]);

scenario('titles', { chart_type: 'bar', ...AE, title_resolved: 'Adverse events' }, [
  gearOpen(),
  ['open the Titles fold', (c) => Array.from(c.block.popoverEl
    .querySelectorAll('.dd-section-title--fold')).find((f) => f.textContent.includes('Titles')).click()],
  ['type title = "My title" + Enter', (c) => I.typeText(c.env,
    c.block.popoverEl.querySelector('.dd-title-title input.blockr-text-input'), 'My title')],
  ['type caption = "N = {n}" + Enter', (c) => I.typeText(c.env,
    c.block.popoverEl.querySelector('.dd-title-caption input.blockr-text-input'), 'N = {n}')]
]);

scenario('script-band', {
  chart_type: 'bar', ...AE, script: 'top_n <- 5',
  script_inputs: [
    { name: 'top_n', key: 'sv_top_n', label: 'Top n', kind: 'number', min: 1, max: 20 },
    { name: 'arm', key: 'sv_arm', label: 'Arm', kind: 'select',
      options: ['Placebo', 'Low Dose', 'High Dose'] },
    { name: 'arms', key: 'sv_arms', label: 'Arms', kind: 'multi',
      options: ['Placebo', 'Low Dose', 'High Dose'] }
  ],
  sv_top_n: 5, sv_arm: 'Placebo', sv_arms: ['Placebo']
}, [
  ['band: Top n = 7 (change)', (c) => {
    const inp = c.block.el.querySelector('.dd-mapping-band .dd-role-sv_top_n input');
    inp.value = '7';
    inp.dispatchEvent(new c.env.win.Event('change'));
  }],
  ['band: Arm = Low Dose', (c) => I.pickSelect(c.env,
    c.block.el.querySelector('.dd-mapping-band .dd-role-sv_arm'), 'Low Dose')],
  ['band: Arms untick Placebo', (c) => I.pickSelect(c.env,
    c.block.el.querySelector('.dd-mapping-band .dd-role-sv_arms'), 'Placebo')]
]);

// A mapping the rows do not carry yet: the chart holds its picture and waits
// for R to send the rows.
scenario('await-data', { chart_type: 'bar', ...AE }, [
  gearOpen(),
  sel('group', 'ARM'),
  ['R sends the rows with ARM', (c) => c.send({ ...c.block.config },
    { dataRev: 2 })]
], { data: { AETERM: I.F.data.AETERM, AESEV: I.F.data.AESEV } });
