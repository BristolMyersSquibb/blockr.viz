// @ts-check
/**
 * Chart block: a memo keyed on what the computation read. A model or an
 * option builder runs on settings and a `derived` that record every field
 * read (`derived.x` for a field of derived); its result is reused while
 * those fields hold the same values and the other inputs (`extra`) are the
 * same. No list of fields is kept by hand, so a field that is read cannot be
 * missing from the key.
 *
 * The same inputs take the same path through the code and read the same
 * fields, so equal values in the fields read last time mean an equal
 * result. The inputs record reads only while the computation runs; a
 * reused result costs one comparison of those fields.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** A field's value: `derived.x` from derived, else from the settings.
   * @param {Record<string, any>} cfg @param {Record<string, any>} derived @param {string} f */
  const valueAt = (cfg, derived, f) => (f.startsWith('derived.') ? derived[f.slice(8)] : cfg[f]);

  /** @param {Record<string, any>} cfg @param {Record<string, any>} derived
   * @param {string[]} reads @param {string} extra */
  const keyOf = (cfg, derived, reads, extra) =>
    JSON.stringify(reads.map((f) => valueAt(cfg, derived, f))) + '\u0000' + extra;

  /**
   * `obj` with every field read recorded in `reads`, under `prefix`.
   * @template {Record<string, any>} T
   * @param {T} obj @param {Set<string>} reads @param {string} prefix @returns {T}
   */
  const tracking = (obj, reads, prefix) => new Proxy(obj, {
    get(t, k) {
      if (typeof k === 'string') reads.add(prefix + k);
      return t[/** @type {any} */ (k)];
    }
  });

  class ReadMemo {
    constructor() {
      /** The fields the last computation read, sorted. @type {string[] | null} */
      this.reads = null;
      /** @type {string | null} */
      this.key = null;
      /** @type {any} */
      this.value = null;
    }

    /**
     * The result for these settings, `derived` and `extra`: the last one
     * while what it read is unchanged, else `compute` run on recording
     * inputs.
     * @param {ChartSettings} cfg
     * @param {ChartDerived} derived
     * @param {string} extra The other inputs, as one string.
     * @param {(cfg: ChartSettings, derived: ChartDerived) => any} compute
     * @returns {boolean} Whether `compute` ran.
     */
    update(cfg, derived, extra, compute) {
      if (this.reads && this.key === keyOf(cfg, derived, this.reads, extra)) return false;
      /** @type {Set<string>} */
      const reads = new Set();
      this.value = compute(tracking(cfg, reads, ''), tracking(derived, reads, 'derived.'));
      this.reads = [...reads].sort();
      this.key = keyOf(cfg, derived, this.reads, extra);
      return true;
    }
  }

  NS.ReadMemo = ReadMemo;
})();
