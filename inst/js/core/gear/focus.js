// @ts-check
/**
 * The field being typed in, kept across a re-render of the gear's tray: the
 * engine saves it before it draws the rows again and restores it after.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  V.gear.focus = {
    /** The focused field under `el`, with its text and caret, or null.
     * @param {HTMLElement} el @returns {VizGearFocus | null} */
    save(el) {
      const a = /** @type {HTMLInputElement | null} */ (document.activeElement);
      if (!a || !el.contains(a)) return null;
      const key = a.getAttribute('data-gear-input');
      if (!key) return null;
      const typed = a.tagName === 'INPUT' || a.tagName === 'TEXTAREA';
      return { key, text: typed ? a.value : null,
               start: typed ? a.selectionStart : null, end: typed ? a.selectionEnd : null };
    },
    /** @param {HTMLElement} el @param {VizGearFocus | null} f */
    restore(el, f) {
      if (!f) return;
      const field = /** @type {HTMLInputElement | null} */ (
        el.querySelector('[data-gear-input="' + f.key + '"]'));
      if (!field) return;
      if (f.text != null && field.value !== f.text) {
        field.value = f.text;
        field.dispatchEvent(new Event('input'));
      }
      field.focus();
      if (f.start != null && typeof field.setSelectionRange === 'function') {
        try { field.setSelectionRange(f.start, f.end); } catch (e) { /* not a text field */ }
      }
    }
  };
})();
