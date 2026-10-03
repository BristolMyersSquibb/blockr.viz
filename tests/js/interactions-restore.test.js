/* State restore: what a drilldown-data message carrying a saved selection
 * (`filter_column` + `filter_values`) shows, how it interacts with later
 * clicks and echoes, and the view state a re-render keeps (zoom window,
 * brush, legend toggles are in interactions-click). */
'use strict';

const test = require('node:test');
const I = require('./interact');

const AE = { group: 'AETERM', drill: 'auto' };
const LAB = { x: 'ADY', y: 'AVAL', drill: 'auto' };

const sendCfg = (label, extra) => [label, (c) => c.send({ ...c.cfg, ...extra }, { dataRev: 1 })];
const echo = (extra) => [`R echoes the config ${JSON.stringify(extra || {})}`, (c) => c.echo(extra)];
const clickName = (name) => [`click ${name}`, (c) => c.clickWhere(0, (p) => String(p.name) === name)];
const reset = () => ['Reset', (c) => c.reset()];
const wait = (ms) => [`advance ${ms} ms`, (c) => c.advance(ms)];

const scenario = (name, config, steps, opts = {}) =>
  test(name, () => I.snap('restore-' + name, I.runSteps(config, steps, { noSend: true, ...opts })));

scenario('bar-single', { chart_type: 'bar', ...AE, filter_column: 'AETERM',
                         filter_values: ['Nausea'] }, [
  sendCfg('R sends the saved selection'),
  clickName('Nausea'),
  clickName('Headache')
]);

scenario('bar-multi', { chart_type: 'bar', ...AE, color: 'AESEV', filter_column: 'AETERM',
                        filter_values: ['Nausea', 'Headache'] }, [
  sendCfg('R sends a saved selection of two values'),
  clickName('Nausea'),
  reset()
]);

scenario('bar-column-differs', { chart_type: 'bar', ...AE, filter_column: 'USUBJID',
                                 filter_values: ['S01'] }, [
  sendCfg('R sends a selection on another column than the group')
]);

scenario('bar-no-column', { chart_type: 'bar', ...AE, filter_values: ['Nausea'] }, [
  sendCfg('R sends values without a filter_column')
]);

scenario('bar-transient', { chart_type: 'bar', ...AE, ctrl_target: 'auto',
                            filter_column: 'AETERM', filter_values: ['Nausea'] }, [
  sendCfg('R sends a selection to a transient chart')
]);

scenario('bar-echo-keeps', { chart_type: 'bar', ...AE }, [
  sendCfg('R sends no selection'),
  clickName('Rash'),
  // An echo without values does not clear the selection...
  echo({ filter_values: [] }),
  // ...and one with other values replaces it.
  echo({ filter_column: 'AETERM', filter_values: ['Fatigue'] }),
  reset(),
  // After Reset an echo still carrying the old values latches them again.
  echo({ filter_column: 'AETERM', filter_values: ['Fatigue'] })
]);

scenario('pie', { chart_type: 'pie', group: 'AESEV', drill: 'auto', filter_column: 'AESEV',
                  filter_values: ['MILD'] }, [sendCfg('R sends the saved selection')]);

scenario('treemap', { chart_type: 'treemap', group: 'AETERM', value: 'AVAL', func: 'sum',
                      drill: 'auto', filter_column: 'AETERM', filter_values: ['Rash'] },
[sendCfg('R sends the saved selection')]);

scenario('radar', { chart_type: 'radar', group: 'AVISIT', color: 'ARM', value: 'AVAL',
                    func: 'mean', drill: 'auto', filter_column: 'ARM',
                    filter_values: ['Placebo'] }, [sendCfg('R sends the saved selection')]);

scenario('boxplot-color', { chart_type: 'boxplot', group: 'AVISIT', color: 'ARM', value: 'AVAL',
                            drill: 'auto', filter_column: 'AVISIT',
                            filter_values: ['Week 4'] }, [sendCfg('R sends the saved selection')]);

scenario('waterfall', { chart_type: 'waterfall', group: 'AVISIT', value: 'CHG', func: 'sum',
                        drill: 'auto', filter_column: 'AVISIT', filter_values: ['Week 2'] },
[sendCfg('R sends the saved selection')]);

scenario('gantt', { chart_type: 'gantt', x: 'ASTDY', xend: 'AENDY', y: 'AETERM', drill: 'auto',
                    filter_column: 'AETERM', filter_values: ['Rash'] },
[sendCfg('R sends the saved selection')]);

scenario('scatter', { chart_type: 'scatter', ...LAB, color: 'ARM', filter_column: 'ARM',
                      filter_values: ['Placebo'] }, [sendCfg('R sends the saved selection')]);

scenario('line', { chart_type: 'line', ...LAB, series: 'USUBJID', filter_column: 'USUBJID',
                   filter_values: ['S03'] }, [sendCfg('R sends the saved selection')]);

// -- view state across re-renders --------------------------------------------

const zoom = (e) => [`datazoom ${JSON.stringify(e)}`, (c) => c.trigger('datazoom', e)];

test('line zoom window survives a re-render, a type switch forgets it', () => {
  const zoomState = (c) => ({ zoom: c.slot().zoom, zoomArmed: c.slot().zoomArmed,
                              brushable: c.slot().brushable });
  const r = I.runSteps({ chart_type: 'line', ...LAB, series: 'USUBJID' }, [
    zoom({ batch: [{ startValue: 10, endValue: 60, start: 10, end: 70 }] }),
    echo(),
    zoom({ start: 0, end: 100 }),
    echo(),
    zoom({ batch: [{ start: 20, end: 50 }] }),
    echo(),
    echo({ chart_type: 'scatter' }),
    echo({ chart_type: 'line' })
  ], { extra: zoomState });
  I.snap('restore-line-zoom', r);
});

test('a brush filter survives a re-render', () => {
  const r = I.runSteps({ chart_type: 'scatter', ...LAB }, [
    ['brushSelected', (c) => c.trigger('brushSelected',
      { batch: [{ selected: [{ seriesIndex: 0, dataIndex: [0, 1] }] }] })],
    echo(),
    echo({ chart_type: 'line' })
  ]);
  I.snap('restore-brush', r);
});

test('a transient receipt survives a re-render at its own age', () => {
  const box = { event: { offsetX: 1, offsetY: 1, target: I.rectTarget(0, 0, 10, 10, 2) } };
  const r = I.runSteps({ chart_type: 'bar', ...AE, ctrl_target: 'auto' }, [
    ['click Nausea', (c) => c.clickWhere(0, (p) => p.name === 'Nausea', box)],
    wait(1000),
    echo(),
    wait(1000),
    echo(),
    wait(1100)
  ]);
  I.snap('restore-receipt', r);
});
