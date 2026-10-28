// @ts-check
/**
 * Picker tiles: a drawing over its name, in a grid, one of them chosen. The
 * chart type, the table layout and look, a summary column's mark and a
 * shading's mode are picked this way. A click on the chosen tile does
 * nothing; the arrow keys move and pick, skipping a disabled tile.
 *
 * The drawings are keyed `<argument>:<value>` (`type:bar`, `look:cards`) for
 * the spec's tiles controls; a summary column's marks bring their own.
 * One language for every tile: a 48 x 32 box, marks inside a 4px inset, in
 * currentColor so the tile's state colours it.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  V.gear = V.gear || /** @type {VizGearNamespace} */ ({});

  /**
   * @param {HTMLElement} parent
   * @param {VizTileOption[]} options
   * @param {string} current
   * @param {(v: string) => void} onPick
   * @param {VizScope} scope
   */
  function build(parent, options, current, onPick, scope) {
    const row = document.createElement('div');
    row.className = 'vz-tiles';
    row.setAttribute('role', 'radiogroup');
    /** @type {HTMLButtonElement[]} */
    const btns = [];
    const select = (/** @type {number} */ i) => btns.forEach((b, j) => {
      b.classList.toggle('vz-tile--on', j === i);
      b.setAttribute('aria-checked', j === i ? 'true' : 'false');
      b.tabIndex = j === i ? 0 : -1;
    });
    options.forEach((o, i) => {
      const b = document.createElement('button');
      b.type = 'button';
      b.className = 'vz-tile';
      b.setAttribute('role', 'radio');
      b.setAttribute('data-value', o.value);
      const face = document.createElement('span');
      face.className = 'vz-tile-face';
      face.innerHTML = o.icon;
      const name = document.createElement('span');
      name.className = 'vz-tile-name';
      name.textContent = o.label;
      b.append(face, name);
      if (o.disabled) {
        b.setAttribute('aria-disabled', 'true');
        b.classList.add('is-disabled');
        if (o.title) b.setAttribute('data-blockr-tooltip', o.title);
      }
      scope.on(b, 'click', () => {
        if (o.disabled || b.classList.contains('vz-tile--on')) return;
        select(i);
        onPick(o.value);
      });
      scope.on(b, 'keydown', (e) => {
        const k = /** @type {KeyboardEvent} */ (e).key;
        const step = k === 'ArrowRight' || k === 'ArrowDown' ? 1
          : k === 'ArrowLeft' || k === 'ArrowUp' ? -1 : 0;
        if (!step) return;
        e.preventDefault();
        let j = i;
        do { j = (j + step + btns.length) % btns.length; } while (j !== i && options[j].disabled);
        btns[j].focus();
        btns[j].click();
      });
      btns.push(b);
      row.appendChild(b);
    });
    // A group without the current value has no pick; its first tile still
    // takes the keyboard.
    const cur = options.findIndex((o) => o.value === current);
    select(cur);
    if (cur < 0 && btns[0]) btns[0].tabIndex = 0;
    parent.appendChild(row);
    return row;
  }

  // Inks: solid marks; words and sub-marks at half; areas, boxes and the
  // table header band as a 20% tint; row rules and grids as a faint line.
  /** @type {Record<string, string>} */
  const INK = {
    m: 'fill="currentColor"',
    w: 'fill="currentColor" fill-opacity=".5"',
    t: 'fill="currentColor" fill-opacity=".2"',
    ln: 'fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"',
    sub: 'fill="none" stroke="currentColor" stroke-opacity=".5"',
    rule: 'fill="none" stroke="currentColor" stroke-opacity=".2"'
  };
  /** @param {number} x @param {number} y @param {number} w @param {number} h @param {string} k @param {number} [rx] */
  const R = (x, y, w, h, k, rx) => '<rect x="' + x + '" y="' + y + '" width="' + w +
    '" height="' + h + '" rx="' + (rx == null ? 1 : rx) + '" ' + INK[k] + '/>';
  /** @param {number} x @param {number} y @param {number} r @param {string} [k] */
  const C = (x, y, r, k) => '<circle cx="' + x + '" cy="' + y + '" r="' + r + '" ' + INK[k || 'm'] + '/>';
  /** @param {string} d @param {string} k */
  const P = (d, k) => '<path d="' + d + '" ' + INK[k] + '/>';
  /** A bar on the baseline: rounded top, square foot.
   * @param {number} x @param {number} y @param {number} w @param {number} h */
  const B = (x, y, w, h) => P('M' + x + ' ' + (y + h) + 'V' + (y + 1) + 'a1 1 0 0 1 1-1h' +
    (w - 2) + 'a1 1 0 0 1 1 1V' + (y + h) + 'Z', 'm');
  const BASE = P('M4 28.5H44', 'sub');
  /** @param {number} [h] */
  const HEAD = (h) => R(4, 4, 40, h || 5, 't', 1.5);
  /** @param {number[]} xs @param {number[]} ws @param {number} y @param {string} k */
  const words = (xs, ws, y, k) => ws.map((w, i) => R(xs[i], y, w, 2, k)).join('');
  const C4 = [7, 18, 28, 37];
  /** @param {[number, number[]][]} rows */
  const lines = (rows) => rows.map(([y, ws]) => words(C4, ws, y, 'w')).join('');
  /** A boxplot: whiskers, box, median.
   * @param {number[][]} list */
  const boxes = (list) => list.map(([x, w0, q1, q3, w1, md]) =>
    P('M' + x + ' ' + w0 + 'V' + q1 + 'M' + x + ' ' + q3 + 'V' + w1, 'sub') +
    R(x - 5, q1, 10, q3 - q1, 't', 0) + R(x - 5, q1, 10, q3 - q1, 'ln', 0) +
    P('M' + (x - 5) + ' ' + md + 'H' + (x + 5), 'ln')).join('');
  /** @type {Record<string, string>} */
  const D = {
    'type:bar': B(7, 14, 8, 14) + B(20, 6, 8, 22) + B(33, 18, 8, 10) + BASE,
    'type:histogram': [[8, 7], [14, 14], [20, 22], [26, 17], [32, 10], [38, 4]]
      .map(([x, h]) => R(x, 28 - h, 5, h, 'm', 0)).join('') + BASE,
    'type:waterfall': P('M13 14.5H16M23 8.5H26M33 16.5H36', 'sub') + B(6, 14, 7, 14) +
      R(16, 8, 7, 6, 'm') + R(26, 8, 7, 9, 'w') + B(36, 17, 7, 11) + BASE,
    'type:pie': C(24, 16, 12, 't') + C(24, 16, 12, 'ln') + P('M24 16V4A12 12 0 0 1 34.39 22Z', 'm'),
    'type:treemap': R(6, 4, 18, 24, 'm') + R(26, 4, 16, 13, 'm') + R(26, 19, 7, 9, 'm') +
      R(35, 19, 7, 9, 'm'),
    'type:boxplot': boxes([[15, 6, 11, 21, 26, 16], [33, 10, 14, 23, 27, 18]]),
    'type:pointrange': P('M12 10V26M24 6V20M36 9V22', 'ln') + C(12, 18, 2.5) + C(24, 12, 2.5) +
      C(36, 15, 2.5),
    'type:scatter': [[8, 24], [14, 20], [22, 23], [30, 18], [38, 20], [11, 15], [19, 12],
      [27, 9], [34, 11], [41, 5]].map(([x, y]) => C(x, y, 2)).join(''),
    'type:line': P('M4 24L12 18L20 21L28 11L36 14L44 6', 'ln'),
    'type:area': P('M4 28V20L12 15L20 18L28 9L36 12L44 6V28Z', 't') +
      P('M4 20L12 15L20 18L28 9L36 12L44 6', 'ln') + BASE,
    'type:band': P('M4 14L12 10L20 13L28 5L36 8L44 4V16L36 20L28 17L20 25L12 22L4 26Z', 't') +
      P('M4 20L12 16L20 19L28 11L36 14L44 10', 'ln'),
    'type:gantt': R(6, 5, 18, 5, 'm') + R(14, 13.5, 20, 5, 'm') + R(24, 22, 18, 5, 'm'),

    'layout:records': HEAD() + words(C4, [7, 6, 6, 5], 5.5, 'm') +
      lines([[12, [8, 5, 6, 4]], [16.5, [7, 6, 5, 5]], [21, [9, 4, 6, 4]], [25.5, [6, 5, 6, 5]]]),
    'layout:groups': HEAD() + words([7, 20, 34], [8, 6, 6], 5.5, 'm') +
      [[13.5, 8, 12, 0], [19.5, 9, 7, 1], [25.5, 7, 9, -1]].map(([c, lw, bw, o]) =>
        R(7, c - 1, lw, 2, 'w') + R(20, c - 1.5, bw, 3, 'm') +
        P('M' + (34 + o) + ' ' + c + 'H' + (43 + o), 'sub') +
        R(36.5 + o, c - 2, 4, 4, 't', 0) + R(36.5 + o, c - 2, 4, 4, 'sub', 0)).join(''),
    'layout:variables': HEAD() + words([24, 32, 40], [4, 4, 4], 5.5, 'm') +
      [[11, 12, [14, 17]], [20.5, 9, [23.5, 26.5]]].map(([y, nw, ss]) =>
        R(7, /** @type {number} */ (y), /** @type {number} */ (nw), 2, 'm') +
        /** @type {number[]} */ (ss).map((sy, i) => R(10, sy, i ? 5 : 7, 2, 'w') +
          words([24, 32, 40], [4, 4, 4], sy, 'w')).join('')).join(''),

    'look:table': HEAD() + words(C4, [7, 6, 6, 5], 5.5, 'm') + P('M4 16H44M4 22H44', 'rule') +
      lines([[12.5, [8, 5, 6, 4]], [18.5, [7, 6, 5, 5]], [24.5, [9, 4, 6, 4]]]),
    'look:compact': HEAD(4) + words([6, 15, 24, 33], [7, 6, 6, 5], 5, 'm') +
      [10, 13, 16, 19, 22, 25].map((y, j) =>
        words([6, 15, 24, 33], [8, 5, 6, 4].map((w) => w - (j % 2)), y, 'w')).join(''),
    'look:cards': [[4.5, 4.5], [25.5, 4.5], [4.5, 17], [25.5, 17]].map(([x, y], i) =>
      R(x, y, 18, 10.5, 'sub', 2) + R(x + 3, y + 2.5, [8, 6, 9, 7][i], 2, 'w') +
      R(x + 3, y + 6, [10, 8, 11, 9][i], 2.5, 'm')).join('')
  };

  /** @type {Record<string, string>} */
  const icons = {};
  for (const k of Object.keys(D)) {
    icons[k] = '<svg viewBox="0 0 48 32" width="48" height="32" aria-hidden="true">' + D[k] + '</svg>';
  }

  V.gear.tiles = { build };
  V.gear.icons = icons;
})();
