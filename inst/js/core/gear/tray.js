// @ts-check
/**
 * The gear's button and the tray it opens, in a view's header.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  V.gear = V.gear || /** @type {VizGearNamespace} */ ({});

  /**
   * A gear button and its tray, appended to a view's header `head`, opened
   * and closed by blockr.ui's Blockr.gearTray. The header gets
   * `vz-head--open` while the tray is open; a tray left open is closed when
   * `scope` goes, which takes it off blockr.ui's dismiss stack.
   * @param {HTMLElement} head @param {VizScope} scope @param {string} label
   * @returns {{ band: HTMLElement, button: HTMLButtonElement, set(on: boolean): void,
   *             isOpen(): boolean }}
   */
  V.gear.tray = (head, scope, label) => {
    const btn = document.createElement('button');
    btn.type = 'button';
    btn.className = 'blockr-gear-btn';
    btn.innerHTML = root.Blockr.icons && root.Blockr.icons.gear ? root.Blockr.icons.gear : '⚙';
    const band = document.createElement('div');
    band.className = 'blockr-settings blockr-settings--beak vz-tray';
    head.append(btn, band);
    const tray = /** @type {VizUi} */ (root.Blockr).gearTray(band, btn, { label });
    const set = (/** @type {boolean} */ on) => tray.set(on);
    const sync = () => head.classList.toggle('vz-head--open',
                                             btn.getAttribute('aria-expanded') === 'true');
    scope.mutate(btn, sync, { attributes: true, attributeFilter: ['aria-expanded'] });
    scope.own({ dispose: () => set(false) });
    return { band, button: btn, set, isOpen: () => tray.isOpen() };
  };
})();
