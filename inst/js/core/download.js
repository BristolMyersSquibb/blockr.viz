// @ts-check
/**
 * A view's download control, built from the payload's `derived.downloads`
 * (R/export-registry.R, viz_downloads()): one format is a tool that
 * downloads, several are blockr.ui's action menu with one row per format.
 * The markup is blockr.ui's own (action_menu(), menu_item()), so its page
 * listeners open and close the menu. The links are Shiny download links,
 * bound here so their href arrives, and unbound when the view goes.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const esc = V.util.esc;

  const ICON = '<svg width="14" height="14" viewBox="0 0 16 16" aria-hidden="true" ' +
    'fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" ' +
    'stroke-linejoin="round"><path d="M8 2.5 V10 M4.8 7 L8 10.2 L11.2 7"/>' +
    '<path d="M2.5 11.5 V12.8 A1.2 1.2 0 0 0 3.7 14 H12.3 A1.2 1.2 0 0 0 13.5 12.8 V11.5"/></svg>';

  /** @param {VizDownload} d */
  const tool = (d) => {
    const tip = esc('Download as ' + d.label + ' (.' + d.ext + ')');
    return '<a id="' + esc(d.id) + '" class="shiny-download-link blockr-tool" href="" ' +
      'download target="_blank" aria-label="' + tip + '" data-blockr-tooltip="' + tip +
      '">' + ICON + '</a>';
  };

  /** @param {VizDownload[]} list */
  const menu = (list) => '<span class="blockr-action-menu" data-align="end">' +
    '<button type="button" class="blockr-tool blockr-action-menu__trigger" ' +
    'aria-haspopup="menu" aria-expanded="false" aria-label="Download" ' +
    'data-blockr-tooltip="Download">' + ICON + '</button>' +
    '<div class="blockr-menu" role="menu" tabindex="-1" hidden>' +
    list.map((d) => '<a id="' + esc(d.id) + '" class="shiny-download-link ' +
      'blockr-menu__item" role="menuitem" tabindex="-1" href="" download ' +
      'target="_blank"><span class="blockr-menu__label">' + esc(d.label) +
      '</span><span class="blockr-menu__meta">.' + esc(d.ext) + '</span></a>').join('') +
    '</div></span>';

  /** @type {WeakMap<HTMLElement, { key: string, offs: (() => void)[] }>} */
  const drawn = new WeakMap();

  /** @param {HTMLElement} host */
  const clear = (host) => {
    const shiny = root.Shiny;
    if (shiny && typeof shiny.unbindAll === 'function') shiny.unbindAll(host);
    host.textContent = '';
  };

  /**
   * Draw the control into `host`. The same list again draws nothing.
   * @param {HTMLElement} host
   * @param {VizDownload[] | null | undefined} list
   * @param {VizScope} scope The view's scope; the control goes with it.
   * @param {{ onOpen?: () => void }} [opts] `onOpen` runs when the reader
   *   reaches for the control (opens the menu, or points at the one tool),
   *   which is when a chart takes its picture.
   */
  function downloads(host, list, scope, opts = {}) {
    const items = Array.isArray(list) ? list : [];
    const key = JSON.stringify(items);
    const prev = drawn.get(host);
    if (prev && prev.key === key) return;
    if (prev) {
      prev.offs.forEach((f) => f());
    } else {
      scope.own({ dispose: () => { clear(host); drawn.delete(host); } });
    }
    clear(host);
    /** @type {(() => void)[]} */
    const offs = [];
    drawn.set(host, { key, offs });
    if (!items.length) return;
    host.innerHTML = items.length === 1 ? tool(items[0]) : menu(items);
    const first = /** @type {HTMLElement} */ (host.firstElementChild);
    const reach = /** @type {HTMLElement} */ (
      first.querySelector('.blockr-action-menu__trigger') || first);
    const onOpen = opts.onOpen;
    if (onOpen) {
      for (const type of ['click', 'pointerenter', 'focus']) {
        offs.push(scope.on(reach, type, () => onOpen()));
      }
    }
    const shiny = root.Shiny;
    if (shiny && typeof shiny.bindAll === 'function') shiny.bindAll(host);
  }

  V.downloads = downloads;
})();
