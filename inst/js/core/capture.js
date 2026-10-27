// @ts-check
/**
 * Pictures of a view drawn off screen, for an export that asks for a block
 * whose panel may be closed (R/chart-capture.R, the capture service). A view
 * draws itself into a host placed outside the page at the size the export
 * wants; the host goes in every case, after a picture, an error or the
 * timeout.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /**
   * Draw a picture off screen.
   * @param {{ width: number, height: number,
   *           draw: (host: HTMLElement) => VizPicture | Promise<VizPicture>,
   *           timeout?: number }} opts
   *   `draw` gets the host and returns the picture, or a promise of it.
   *   `timeout` is in ms, 20000 by default.
   * @returns {Promise<VizPicture>}
   */
  function capture(opts) {
    const doc = root.document;
    const scope = new V.Scope();
    const host = scope.node(doc.body.appendChild(doc.createElement('div')));
    host.className = 'blockr-capture-host';
    host.style.cssText = 'position:fixed;left:-20000px;top:0;width:' +
      opts.width + 'px;height:' + opts.height + 'px;';
    const ms = opts.timeout == null ? 20000 : opts.timeout;
    const late = new Promise((resolve, reject) => {
      scope.timeout(() => reject(new Error('the picture did not draw in time')), ms);
    });
    return Promise.race([Promise.resolve().then(() => opts.draw(host)), late])
      .finally(() => scope.dispose());
  }

  /**
   * Answer the capture service's request `req` with the picture `p` will
   * give, or with its error. Exactly one reply per request.
   * @param {string} req
   * @param {Promise<VizPicture>} p
   * @returns {Promise<void>}
   */
  function captureReply(req, p) {
    const reply = (/** @type {Record<string, unknown>} */ value) => {
      const shiny = root.Shiny;
      if (shiny && typeof shiny.setInputValue === 'function') {
        shiny.setInputValue('blockr_viz_capture_result', Object.assign({ req }, value),
                            { priority: 'event' });
      }
    };
    return p.then(
      (pic) => reply({ png: pic.png, width: pic.width, height: pic.height }),
      (err) => reply({ error: err && err.message ? err.message : String(err) })
    );
  }

  V.capture = capture;
  V.captureReply = captureReply;
})();
