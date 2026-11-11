// @ts-check
/**
 * The columns' distinct counts, on request. A picker of a gear's host may
 * filter its columns by them, so a gear asks R for them once its tray is
 * open (R/kit-server.R, viz_counts_observer()); they are not in the payload.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /** The gears by view element id, for R's `blockr-viz:column-counts`.
   * @type {Map<string, any>} */
  const gears = new Map();
  /** One number for the page: an `<id>_counts` set twice arrives twice. */
  let asked = 0;

  V.gear.columnCounts = {
    /** @param {any} gear */
    add(gear) { gears.set(gear.id, gear); },
    /** @param {any} gear */
    remove(gear) { if (gears.get(gear.id) === gear) gears.delete(gear.id); },
    /** Ask R for the counts of the gear's current columns, once per set of
     * columns and only while its tray is open.
     * @param {any} gear */
    ask(gear) {
      const need = gear.host.needsCounts;
      if (gear.disposed || !gear._open || !need || gear._askedFor === gear._cols) return;
      if (!need(gear._cols)) return;
      gear._askedFor = gear._cols;
      V.util.send(gear.id, 'counts', ++asked);
    },
    /** Put counts, by column name, onto the columns. The property does not
     * enumerate, so the payloads' keys that stringify the columns stay as
     * they were.
     * @param {VizColumn[]} columns @param {Record<string, number>} counts */
    put(columns, counts) {
      for (const c of columns) {
        if (counts && Object.prototype.hasOwnProperty.call(counts, c.name)) {
          Object.defineProperty(c, 'n_unique', { value: counts[c.name], writable: true, configurable: true });
        }
      }
    }
  };

  /** R's `blockr-viz:column-counts`: the answer to a gear's ask.
   * @param {{ id?: string, counts?: Record<string, number> | null }} msg */
  V.gear.counts = (msg) => {
    const g = msg && msg.id ? gears.get(msg.id) : null;
    if (g && msg.counts) g.counts(msg.counts);
  };
  const shiny = root.Shiny;
  if (shiny && typeof shiny.addCustomMessageHandler === 'function') {
    shiny.addCustomMessageHandler('blockr-viz:column-counts', V.gear.counts);
  }
})();
