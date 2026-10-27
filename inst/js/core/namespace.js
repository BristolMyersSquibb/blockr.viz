// @ts-check
/**
 * blockr.viz's one global: window.Blockr.viz. Every other viz script hangs
 * what it shares off it (spec, util, Scope, the block views) and defines no
 * other global. Loaded first.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {{ viz?: VizNamespace }} */ (root.Blockr = root.Blockr || {});
  B.viz = B.viz || /** @type {VizNamespace} */ ({ spec: {} });
})();
