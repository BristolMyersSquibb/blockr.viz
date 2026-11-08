// @ts-check
/**
 * A pair setting on the block's face (axis limits): R prints it as one
 * phrase, "20 to auto"; the face draws each end as its own word, typed over
 * like a value (titles.js). The other end stays as it is. A setting of
 * several parts (axis ticks, "every 7 days") draws a word per part, each
 * opening its own control (slotWords).
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /** @param {HTMLElement} el @param {VizScope} ps @param {VizEndOpener[]} ends */
  V.slotPair = (el, ps, ends) => ends.forEach((end, i) => {
    if (i) el.appendChild(document.createTextNode(' to '));
    const w = el.appendChild(document.createElement('span'));
    w.className = 'blockr-slot';
    w.textContent = end.value == null ? 'auto' : String(end.value);
    V.titles.wire(ps, w, () => V.titles.edit(ps, w, end));
  });

  /** @param {HTMLElement} el @param {VizScope} ps @param {VizWordPart[]} words
   * @param {(i: number, w: HTMLElement) => void} open */
  V.slotWords = (el, ps, words, open) => words.forEach((wd, i) => {
    if (i) el.appendChild(document.createTextNode(' '));
    const w = el.appendChild(document.createElement('span'));
    w.className = 'blockr-slot';
    w.textContent = wd.text;
    V.titles.wire(ps, w, () => open(i, w));
  });
})();
