// @ts-check
/**
 * Scripts the page fetches when it first needs them, not with every page:
 * SnapDOM for a table's picture (table/groups.js) and an ECharts theme the
 * board names (chart/view.js). Each URL is fetched once; every caller gets
 * the same promise.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  // The folder the core's dependency serves, the package's installed
  // directory: this script's own URL up to its js/ folder (js/dist/core.js
  // or, with the dev_js option, js/core/lazy.js). Empty off a page.
  const self = root.document && root.document.currentScript;
  const src = self && typeof self.src === 'string' ? self.src : '';
  const cut = src.lastIndexOf('/js/');
  const base = cut >= 0 ? src.slice(0, cut + 1) : '';

  /** @type {Map<string, Promise<void>>} */
  const loading = new Map();

  /**
   * A script added to the page once; resolves when it has run. A failed
   * load rejects and is forgotten, so a later call tries again.
   * @param {string} url
   * @returns {Promise<void>}
   */
  const script = (url) => {
    const known = loading.get(url);
    if (known) return known;
    const doc = root.document;
    const p = new Promise((resolve, reject) => {
      if (!doc || !doc.head) {
        reject(new Error('no page to load ' + url + ' into'));
        return;
      }
      const el = doc.createElement('script');
      el.src = url;
      el.async = true;
      el.onload = () => resolve(undefined);
      el.onerror = () => {
        loading.delete(url);
        el.remove();
        reject(new Error('could not load ' + url));
      };
      doc.head.appendChild(el);
    });
    loading.set(url, p);
    return p;
  };

  /**
   * A file of the installed package (`path` under inst/), as a URL; null
   * when the page did not load the core from the package's folder (a node
   * test, a Web page download with the scripts inlined).
   * @param {string} path
   */
  const asset = (path) => (lazy.base ? lazy.base + path : null);

  const lazy = { base, script, asset };
  V.lazy = lazy;
})();
