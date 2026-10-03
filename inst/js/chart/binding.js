// @ts-check
/**
 * Chart block: the Shiny input binding and the messages R sends
 * (tests/js/CONTRACT.md):
 *
 *   drilldown-data     columns, rows (or none, when the client holds them)
 *                      and config; waits for its container if it has none
 *   drilldown-ready    R's answer to the <id>_ready announce
 *   drilldown-theme    an ECharts theme for one chart
 *   drilldown-capture  a picture of a chart, drawn off screen
 *
 * A message without rows the client does not hold asks R for them once per
 * data rev (<id>_need) and draws nothing meanwhile.
 */
(function () {
  'use strict';
  const NS = /** @type {any} */ (window.Blockr).chart;

  // Loaded twice, the scripts would answer every message twice.
  const reg = /** @type {any} */ (Shiny.inputBindings);
  if (reg.bindingNames && reg.bindingNames['blockr.drilldown']) {
    console.error('blockr.viz: the chart scripts are already loaded; this copy stops here.');
    return;
  }

  // The last undelivered payload and theme per container id: a message can
  // arrive before its container is bound (dock panels mount late).
  /** @type {Record<string, any>} */
  const pendingData = {};
  /** @type {Record<string, any>} */
  const pendingTheme = {};

  /** @param {any} blk @param {any} rev */
  const hasRows = (blk, rev) => !!blk && rev != null &&
    blk._lastDataRev === rev && Array.isArray(blk.data);

  // Ask R for the whole message, once per rev: R answers with its latest.
  /** @param {any} el @param {any} rev */
  const askRows = (el, rev) => {
    if (el._needRev === rev || !(window.Shiny && Shiny.setInputValue)) return;
    el._needRev = rev;
    Shiny.setInputValue(el.id + '_need', rev, { priority: 'event' });
  };

  // The busy cue ends once the picture is painted: two frames after the
  // option went in, or later if the chart has no canvas yet.
  const busyDone = (/** @type {string} */ id, /** @type {number} */ tries = 200) => {
    const b = /** @type {any} */ (window).Blockr?.busy;
    if (!b) return;
    requestAnimationFrame(() => requestAnimationFrame(() => {
      const el = document.getElementById(id);
      if (el && !el.querySelector('canvas') && tries > 0) {
        setTimeout(() => busyDone(id, tries - 1), 50);
        return;
      }
      b.stop(id);
    }));
  };

  /** @param {any} el @param {any} msg */
  const deliver = (el, msg) => {
    if (msg.data == null && msg.data_rev != null && !hasRows(el._block, msg.data_rev)) {
      // Drawing this config over no rows would flash the empty state.
      askRows(el, msg.data_rev);
      return;
    }
    el._needRev = null;
    el._block.setData(msg.columns, msg.data, msg.config, msg.arguments, msg.data_rev);
    busyDone(el.id);
  };

  // The live view per container id, so a dock re-mount (a new element under
  // the same id) disposes the old view's instances (B2).
  /** @type {Record<string, any>} */
  const live = {};

  // A scheme switch (bslib writes data-bs-theme on <html>) changes the
  // tokens without a draw; redraw every chart with rows so the canvas reads
  // the new values.
  new MutationObserver(() => {
    document.querySelectorAll('.drilldown-chart-container').forEach((el) => {
      const blk = /** @type {any} */ (el)._block;
      if (blk && blk.data && blk.data.length) blk._render();
    });
  }).observe(document.documentElement, {
    attributes: true, attributeFilter: ['data-bs-theme'], subtree: true
  });

  const binding = new Shiny.InputBinding();
  Object.assign(binding, {
    find: (/** @type {any} */ scope) => $(scope).find('.drilldown-chart-container'),
    getId: (/** @type {any} */ el) => el.id || null,
    getValue: () => null,
    subscribe: () => {},
    unsubscribe: () => {},
    initialize: (/** @type {any} */ el) => {
      if (el._block) el._block.dispose();
      const old = el.id ? live[el.id] : null;
      if (old && old !== el._block) old.dispose();
      el._block = new NS.ChartView(el);
      if (el.id) live[el.id] = el._block;
      if (el.id in pendingTheme) {
        el._block.setTheme(pendingTheme[el.id]);
        delete pendingTheme[el.id];
      }
      if (el.id in pendingData) {
        const p = pendingData[el.id];
        delete pendingData[el.id];
        deliver(el, p);
      } else if (window.Shiny && Shiny.setInputValue) {
        // Nothing waiting: a message sent before these scripts loaded was
        // dropped by Shiny. Announce; R answers with its last rev.
        Shiny.setInputValue(el.id + '_ready', Date.now(), { priority: 'event' });
      }
    }
  });
  Shiny.inputBindings.register(binding, 'blockr.drilldown');

  Shiny.addCustomMessageHandler('drilldown-capture', NS.captureHandler(NS.ChartView));

  Shiny.addCustomMessageHandler('drilldown-data', (/** @type {any} */ msg) => {
    const el = /** @type {any} */ (document.getElementById(msg.id));
    if (el?._block) {
      deliver(el, msg);
      return;
    }
    // A message without rows keeps the rows of an earlier one still waiting.
    const prev = pendingData[msg.id];
    pendingData[msg.id] = msg.data == null && prev && prev.data != null &&
      prev.data_rev === msg.data_rev
      ? Object.assign({}, msg, { data: prev.data }) : msg;
  });

  // R's answer to <id>_ready: the rev of its last message. A client holding
  // it is done; one without asks for the rows.
  Shiny.addCustomMessageHandler('drilldown-ready', (/** @type {any} */ msg) => {
    const el = /** @type {any} */ (document.getElementById(msg.id));
    if (!el?._block || hasRows(el._block, msg.data_rev)) return;
    askRows(el, msg.data_rev);
  });

  Shiny.addCustomMessageHandler('drilldown-theme', (/** @type {any} */ msg) => {
    const el = /** @type {any} */ (document.getElementById(msg.id));
    if (el?._block) el._block.setTheme(msg.theme);
    else pendingTheme[msg.id] = msg.theme;
  });
})();
