// @ts-check
/**
 * The cards look: a TableCards model (table.d.ts) drawn as one row of cards
 * per table row, one card per column (records) or per summary (groups).
 * The layout calls `render()` with the rows to draw, in order, and the
 * classes of each row, so search, the drill and its highlight work on card
 * rows as on table rows. Groups passes `glyph` to draw its mark in a card.
 *
 * The grid follows the host's width:
 *   wide    a row head column, then one column per card, with column heads
 *   medium  each row on its own line, the row head above its cards
 *   narrow  one card per line, no legend
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const esc = V.util.esc;

  const NARROW = 320;
  const CARD_MIN = 150;
  const HEAD_W = 120;

  /** @param {TableCardCol['legend']} items */
  function legendHtml(items) {
    if (!items || !items.length) return '';
    return '<div class="tc-legend">' + items.map((it) =>
      '<span><i style="background:' + esc(it.color) + '"></i>' + esc(it.label) + '</span>')
      .join('') + '</div>';
  }

  /**
   * @param {TableCards} cards
   * @param {number} k Column.
   * @param {number} i Row.
   * @param {TableCardOpts['glyph']} glyph
   */
  function cardHtml(cards, k, i, glyph) {
    const c = cards.cols[k];
    const main = c.main ? c.main[i] : null;
    const sub = c.sub ? c.sub[i] : null;
    const g = c.glyph != null && glyph ? glyph(c.glyph, i) : '';
    let out = '<div class="tc' + (c.text ? ' is-text' : '') + '">' +
      '<div class="tc-lab" title="' + esc(c.label) + '">' + esc(c.label) + '</div>' +
      '<div class="tc-val">' + (main == null || main === ''
        ? '<span class="tc-na">—</span>' : esc(main)) + '</div>';
    if (c.sub) {
      // A single row's card names its statistic; a column of cards names it
      // once in its head.
      out += '<div class="tc-sub">' + (sub && c.word
        ? '<span class="tc-word">' + esc(c.word) + ': </span>' : '') + esc(sub || '') + '</div>';
    }
    if (g) out += '<div class="tc-glyph" data-k="' + c.glyph + '">' + g + '</div>';
    if (!cards.multi) out += legendHtml(c.legend);
    return out + '</div>';
  }

  /** @param {TableCards} cards */
  function headHtml(cards) {
    return '<div class="tc-corner"></div>' + cards.cols.map((c) => {
      const note = [c.word, c.range].filter(Boolean).join(' · ');
      return '<div class="tc-colhead"><b title="' + esc(c.label) + '">' + esc(c.label) +
        '</b>' + (note ? '<span>' + esc(note) + '</span>' : '') + legendHtml(c.legend) + '</div>';
    }).join('');
  }

  /**
   * @param {TableCards} cards
   * @param {number} i
   * @param {TableCardOpts} opts
   */
  function rowHtml(cards, i, opts) {
    const cls = opts.rowClass ? opts.rowClass(i) : '';
    let out = '<div class="tc-row' + (cls ? ' ' + cls : '') + '" data-i="' + i + '">';
    if (cards.multi) {
      const hex = cards.hex ? cards.hex[i] : null;
      const strip = !cards.hex ? ''
        : '<i class="tc-strip" style="' + (hex ? 'background:' + esc(hex) : 'visibility:hidden') +
          '"></i>';
      out += '<div class="tc-rowhead">' + strip + '<span class="tc-rowlab">' +
        esc(cards.label ? cards.label[i] : '') + '</span></div>';
    }
    for (let k = 0; k < cards.cols.length; k++) out += cardHtml(cards, k, i, opts.glyph);
    return out + '</div>';
  }

  /**
   * The layout mode for the host's width.
   * @param {HTMLElement} host
   * @param {TableCards} cards
   */
  function fit(host, cards) {
    const grid = host.querySelector('.tc-grid');
    if (!grid) return;
    const w = host.clientWidth || 0;
    const mode = w > 0 && w < NARROW ? 'narrow'
      : cards.multi && w > 0 && w < HEAD_W + cards.cols.length * CARD_MIN ? 'medium' : 'wide';
    if (grid.getAttribute('data-tc-mode') !== mode) grid.setAttribute('data-tc-mode', mode);
  }

  /** Hosts watched for width, with the model each shows. */
  const watched = new WeakMap();

  /**
   * Draw `cards` into `host`.
   * @param {HTMLElement} host
   * @param {TableCards} cards
   * @param {VizScope} scope Owns the width watch.
   * @param {TableCardOpts} [opts]
   */
  function render(host, cards, scope, opts) {
    const o = opts || {};
    const order = o.order || Array.from({ length: cards.n || 0 }, (_, i) => i);
    let out;
    if (!cards.cols.length) {
      out = '<div class="tc-empty">No summary to show as a card.</div>';
    } else {
      out = '<div class="tc-grid' + (cards.multi ? ' is-multi' : '') + '" style="--tc-k:' +
        cards.cols.length + '">' + (cards.multi ? headHtml(cards) : '') +
        order.map((i) => rowHtml(cards, i, o)).join('') + '</div>' +
        (cards.fold ? '<div class="tc-fold">' + esc(cards.fold) + '</div>' : '');
    }
    host.innerHTML = out;
    const w = watched.get(host);
    if (w && !w.scope.disposed) {
      w.cards = cards;
    } else {
      const entry = { scope, cards };
      watched.set(host, entry);
      scope.resize(host, () => fit(host, entry.cards));
    }
    fit(host, cards);
  }

  V.table.cards = { render };
})();
