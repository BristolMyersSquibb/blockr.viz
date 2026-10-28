// @ts-check
/**
 * Small icons for the gear's choices, keyed `<argument>:<value>` as the
 * tiles are (`position:dodge`, `show:box`). A segmented control whose spec
 * entry says `icons` draws them beside its words; the table's column types
 * (`summary:<type>`) head the Add menu and each column.
 *
 * One language with the tiles (tiles.js): one colour (currentColor), solid
 * marks, a second series at 45%, areas at 20%, 1.5px round lines that keep
 * their width at any size, guides at 1px and 35%. Choices are drawn on a
 * 16 grid and shown at 16px; column types on a 24 grid, shown at 20px.
 * Marks keep 1.5 from the edge, and table marks lie as they do in a row.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  V.gear = V.gear || /** @type {VizGearNamespace} */ ({});

  /** @typedef {[string, ...(string | number)[]]} Prim */

  // Inks: m a mark, s a second series or word, t an area, o a hollow
  // outline, k a data line, ks a second line, k1 a thin tick, g a guide; a
  // number is a swatch at that opacity.
  const LINE = 'fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" ' +
    'vector-effect="non-scaling-stroke"';
  /** @type {Record<string, string>} */
  const INK = {
    m: 'fill="currentColor"',
    s: 'fill="currentColor" fill-opacity=".45"',
    t: 'fill="currentColor" fill-opacity=".2"',
    o: LINE + ' stroke-width="1.5"',
    k: LINE + ' stroke-width="1.5"',
    ks: LINE + ' stroke-width="1.5" stroke-opacity=".5"',
    k1: LINE + ' stroke-width="1"',
    g: LINE + ' stroke-width="1" stroke-opacity=".35"'
  };
  /** @param {string | number} k */
  const ink = (k) => (typeof k === 'number' ? 'fill="currentColor" fill-opacity="' + k + '"' : INK[k]);

  /** @param {Prim} p @param {number} n The grid. */
  function prim(p, n) {
    if (p[0] === 'p') return '<path d="' + p[1] + '" ' + ink(p[2]) + '/>';
    if (p[0] === 'c') return '<circle cx="' + p[1] + '" cy="' + p[2] + '" r="' + p[3] + '" ' + ink(p[4]) + '/>';
    let [, x, y, w, h, k, rx] = /** @type {[string, number, number, number, number, string | number, number?]} */ (p);
    // A box's outline sits inside the box, as the tint does.
    if (k === 'ob') { x += 0.75; y += 0.75; w -= 1.5; h -= 1.5; k = 'o'; }
    if (rx == null) rx = n === 24 ? 1.5 : 1;
    return '<rect x="' + x + '" y="' + y + '" width="' + w + '" height="' + h + '" rx="' + rx + '" ' +
      ink(k) + '/>';
  }

  /** Facet layout: one summary as solid squares, the other as half-ink dots,
   * under two headers. By summary AA | BB, by level AB | AB.
   * @param {string} order @returns {Prim[]} */
  function facet(order) {
    const xs = [2.9, 6.1, 9.9, 13.1];
    /** @type {Prim[]} */
    const out = [['p', 'M1.5 4.5H6.8M9.2 4.5H14.5', 'k']];
    order.split('').forEach((t, i) => out.push(t === 'A'
      ? ['r', xs[i] - 1.5, 8.5, 3, 3, 'm', 0.6]
      : ['c', xs[i], 10, 1.6, 's']));
    return out;
  }
  /** Data points on a line shape: where the riser sits against them tells
   * the steps apart. @param {string} d @returns {Prim[]} */
  const line = (d) => [['p', d, 'k'],
    ...[[2, 12], [6, 6], [10, 9], [14, 4]].map(([x, y]) => /** @type {Prim} */ (['c', x, y, 1.5, 'm']))];
  /** Scatter points under a smoother. @param {Prim[]} fit @returns {Prim[]} */
  const scatter = (fit) => [[3, 11.5], [5.5, 8.5], [7.5, 12], [9.5, 7], [12, 4.5], [13, 9.5]]
    .map(([x, y]) => /** @type {Prim} */ (['c', x, y, 1.4, fit.length ? 's' : 'm'])).concat(fit);
  /** @param {number[]} ops @returns {Prim[]} */
  const ramp = (ops) => ops.map((o, i) => /** @type {Prim} */ (['r', 1.5 + i * 2.6, 4.5, 2.6, 7, o, 0]));
  /** @type {Record<string, Prim[]>} */
  const SCALE = {
    identity: [['p', 'M3.5 1.5V14.5H14.5', 'g'], ['p', 'M1.5 3H3.5M1.5 6.5H3.5M1.5 10H3.5M1.5 13.5H3.5', 'k1'],
      ['p', 'M5.5 12.5L14 3', 'k']],
    log10: [['p', 'M3.5 1.5V14.5H14.5', 'g'], ['p', 'M1.5 2.5H3.5M1.5 4.5H3.5M1.5 7.5H3.5M1.5 12H3.5', 'k1'],
      ['p', 'M5.5 12.5C7 6.5 10 4 14 3', 'k']]
  };
  /** @type {Record<string, Prim[]>} */
  const SORT = {
    asc: [['r', 2, 2.5, 2.5, 2.6, 'm'], ['r', 2, 6.7, 4.5, 2.6, 'm'], ['r', 2, 10.9, 7, 2.6, 'm'],
      ['p', 'M12.5 3V13M10.5 11L12.5 13L14.5 11', 'k']],
    desc: [['r', 2, 2.5, 7, 2.6, 'm'], ['r', 2, 6.7, 4.5, 2.6, 'm'], ['r', 2, 10.9, 2.5, 2.6, 'm'],
      ['p', 'M12.5 13V3M10.5 5L12.5 3L14.5 5', 'k']]
  };

  /** The 16 grid. @type {Record<string, Prim[]>} */
  const SMALL = {
    // A summary column's mark, as it sits in a table cell.
    'show:bar': [['p', 'M2 1.5V14.5', 'g'], ['r', 2, 3, 11, 3.5, 'm'], ['r', 2, 9.5, 7, 3.5, 'm']],
    'show:number': [['p', 'M3 5.6L5.6 4V12.5', 'k'],
      ['p', 'M8.4 5.8C8.7 4.6 9.6 4 10.6 4c1.2 0 2.1.8 2.1 2 0 2-4.3 3.6-4.3 6.5h4.5', 'k']],
    'show:dot': [['p', 'M2 5H14M2 11H14', 'k1'], ['c', 10.5, 5, 2.2, 'm'], ['c', 6, 11, 2.2, 'm']],
    'show:tile': [['r', 2, 2, 5.5, 5.5, 1], ['r', 8.5, 2, 5.5, 5.5, 0.45], ['r', 2, 8.5, 5.5, 5.5, 0.2],
      ['r', 8.5, 8.5, 5.5, 5.5, 0.75]],
    'show:pointrange': [['p', 'M2 8H14', 'g'], ['p', 'M3.5 8H12.5', 'k'], ['c', 8.5, 8, 2.4, 'm']],
    'show:box': [['p', 'M2 8H4.5M11.5 8H14', 'k'], ['p', 'M2 6V10M14 6V10', 'k1'], ['r', 4.5, 4.5, 7, 7, 's', 1],
      ['p', 'M8 4.5V11.5', 'k']],
    'show:text': [['p', 'M2 12.5L5 3.5L8 12.5M3 9.5H7', 'k'], ['c', 11.6, 10.3, 2.1, 'o'], ['p', 'M13.8 7.8V12.5', 'k']],
    'show:dumbbell': [['p', 'M4.5 8H11.5', 'k'], ['c', 4, 8, 2.2, 'o'], ['c', 12, 8, 2.4, 'm']],
    // A colour split: lying, as bar charts and table bars do by default.
    'position:stack': [['r', 2, 3, 6, 4, 'm'], ['r', 8, 3, 4, 4, 's'], ['r', 2, 9, 3, 4, 'm'], ['r', 5, 9, 4, 4, 's']],
    'position:dodge': [['r', 2, 2, 10, 2.5, 'm'], ['r', 2, 4.5, 7, 2.5, 's'], ['r', 2, 9, 6, 2.5, 'm'],
      ['r', 2, 11.5, 11, 2.5, 's']],
    'position:fill': [['p', 'M14.5 2V14', 'k1'], ['r', 2, 3, 7, 4, 'm'], ['r', 9, 3, 4.5, 4, 's'], ['r', 2, 9, 4, 4, 'm'],
      ['r', 6, 9, 7.5, 4, 's']],
    'orientation:horizontal': [['p', 'M1.5 1.5V14.5', 'g'], ['r', 2, 2.5, 8, 3, 'm'], ['r', 2, 6.5, 12, 3, 'm'],
      ['r', 2, 10.5, 5, 3, 'm']],
    'orientation:vertical': [['p', 'M1.5 14.5H14.5', 'g'], ['r', 2.5, 6, 3, 8, 'm'], ['r', 6.5, 2, 3, 12, 'm'],
      ['r', 10.5, 9, 3, 5, 'm']],
    'sort_dir:asc': SORT.asc,
    'sort_dir:desc': SORT.desc,
    'legend:bottom': [['p', 'M2 9.5L6 7L9.5 8L14 5.5', 'k'], ['p', 'M2 5.5L6 3L9.5 4L14 1.5', 'ks'],
      ['c', 3.2, 13, 1.6, 'm'], ['r', 5.4, 12.1, 3, 1.8, 's', 0.9], ['c', 10.6, 13, 1.6, 's'],
      ['r', 12.8, 12.1, 2, 1.8, 's', 0.9]],
    'legend:top': [['c', 3.2, 3, 1.6, 'm'], ['r', 5.4, 2.1, 3, 1.8, 's', 0.9], ['c', 10.6, 3, 1.6, 's'],
      ['r', 12.8, 2.1, 2, 1.8, 's', 0.9], ['p', 'M2 14L6 11.5L9.5 12.5L14 10', 'k'],
      ['p', 'M2 10L6 7.5L9.5 8.5L14 6', 'ks']],
    'legend:marks': [['p', 'M2 13L5 10.5L8 11.5L10.5 9.5', 'k'], ['p', 'M2 7L5 4.5L8 6L10.5 3.5', 'ks'],
      ['r', 11.5, 8.5, 3.5, 2, 'm', 1], ['r', 11.5, 2.5, 3.5, 2, 's', 1]],
    'legend:none': [['p', 'M2 12.5L6 9.5L9.5 11L14 6.5', 'k'], ['p', 'M2 8L6 5L9.5 6.5L14 2.5', 'ks']],
    'y_transform:identity': SCALE.identity,
    'y_transform:log10': SCALE.log10,
    'x_transform:identity': SCALE.identity,
    'x_transform:log10': SCALE.log10,
    'connect:straight': line('M2 12L6 6L10 9L14 4'),
    'connect:monotone': line('M2 12C4 8 4.6 6 6 6S8.6 9 10 9 12.4 5.2 14 4'),
    'connect:step_start': line('M2 12V6H6V9H10V4H14'),
    'connect:step_middle': line('M2 12H4V6H8V9H12V4H14'),
    'connect:step_end': line('M2 12H6V6H10V9H14V4'),
    'smoother:none': scatter([]),
    'smoother:lm': scatter([['p', 'M2 12L14 4', 'k']]),
    'smoother:loess': scatter([['p', 'M2 12C6.5 12 7.5 4 14 4', 'k']]),
    'box_points:none': [['p', 'M8 2.5V5M8 11V13.5', 'k'], ['p', 'M6.5 2.5H9.5M6.5 13.5H9.5', 'k1'],
      ['r', 4.5, 5, 7, 6, 's', 1], ['p', 'M4.5 8H11.5', 'k']],
    'box_points:outliers': [['p', 'M8 4.5V6M8 10V11.5', 'k'], ['r', 4.5, 6, 7, 4, 's', 1], ['p', 'M4.5 8H11.5', 'k'],
      ['c', 8, 2.3, 1.2, 'm'], ['c', 8, 13.7, 1.2, 'm']],
    'facet_layout:by_summary': facet('AABB'),
    'facet_layout:by_level': facet('ABAB'),
    'mode:diverging': ramp([1, 0.5, 0.1, 0.5, 1]).concat([['p', 'M8 3V13', 'k1']]),
    'mode:sequential': ramp([0.1, 0.3, 0.5, 0.75, 1]),
    'mode:bar': [['p', 'M1.5 2.5H14.5V7.5H1.5ZM1.5 8.5H14.5V13.5H1.5Z', 'g'], ['r', 2.5, 3.5, 8, 3, 'm'],
      ['r', 2.5, 9.5, 4.5, 3, 'm']]
  };

  /** The 24 grid: the table's column types. @type {Record<string, Prim[]>} */
  const LARGE = {
    'summary:simple': [['p', 'M3 2.5V21.5', 'g'], ['r', 3, 4.5, 13, 4, 'm'], ['r', 3, 10, 8, 4, 'm'],
      ['r', 3, 15.5, 17, 4, 'm']],
    'summary:dist': [['p', 'M2.5 12H8M16 12H21.5', 'k'], ['p', 'M2.5 9V15M21.5 9V15', 'k1'],
      ['r', 8, 7, 8, 10, 't', 1.5], ['r', 8, 7, 8, 10, 'ob', 1.5], ['p', 'M12 7V17', 'k']],
    'summary:field': [['r', 3, 4.5, 7, 3.5, 'm', 1.75], ['r', 11.5, 4.5, 4.5, 3.5, 's', 1.75],
      ['r', 17, 4.5, 4, 3.5, 's', 1.75], ['r', 3, 10.25, 5, 3.5, 's', 1.75], ['r', 9.5, 10.25, 8, 3.5, 'm', 1.75],
      ['r', 3, 16, 8, 3.5, 's', 1.75], ['r', 12.5, 16, 5, 3.5, 'm', 1.75]],
    'summary:sparkline': [['p', 'M2.5 20H21.5', 'g'], ['p', 'M2.5 16L6.5 11L10 13.5L14 7L17.5 9.5L21 5', 'k'],
      ['c', 21, 5, 1.8, 'm']],
    'summary:spans': [['p', 'M2 7H22M2 12H22M2 17H22', 'g'], ['r', 3, 5, 9, 4, 'm'], ['r', 8, 10, 12, 4, 'm'],
      ['r', 4, 15, 6, 4, 'm'], ['r', 12, 15, 7, 4, 's']],
    'summary:pair': [['p', 'M7 8H16.5M10 16H15.5', 'k'], ['p', 'M5.5 5.5L8 8L5.5 10.5L3 8Z', 'o'],
      ['c', 18, 8, 2.5, 'm'], ['p', 'M17.5 13.5L20 16L17.5 18.5L15 16Z', 'o'], ['c', 8.5, 16, 2.5, 'm']],
    'summary:expr': [['p', 'M9.5 20V8C9.5 5.8 10.8 4.5 13 4.5M6.5 10H12.5', 'k'],
      ['p', 'M13.5 13.5L19.5 20M19.5 13.5L13.5 20', 'k']],
    'summary:custom': [['p', 'M8.5 6.5L3.5 12L8.5 17.5M15.5 6.5L20.5 12L15.5 17.5M13.5 5L10.5 19', 'k']]
  };

  /** @type {Record<string, string>} */
  const glyphs = {};
  for (const [set, n] of /** @type {[Record<string, Prim[]>, number][]} */ ([[SMALL, 16], [LARGE, 24]])) {
    for (const k of Object.keys(set)) {
      const size = n === 24 ? 20 : 16;
      glyphs[k] = '<svg class="vz-glyph" viewBox="0 0 ' + n + ' ' + n + '" width="' + size + '" height="' + size +
        '" aria-hidden="true">' + set[k].map((p) => prim(p, n)).join('') + '</svg>';
    }
  }

  V.gear.glyphs = glyphs;
})();
