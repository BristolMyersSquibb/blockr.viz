// @ts-check
/**
 * Chart block: a memo keyed on what the computation read. A model or an
 * option builder runs on a config that records every field it reads
 * (`derived.x` for a field of `cfg.derived`); its result is reused while
 * those fields hold the same values and the other inputs (`extra`) are the
 * same. No list of fields is kept by hand, so a field that is read cannot be
 * missing from the key.
 *
 * The same inputs take the same path through the code and read the same
 * fields, so equal values in the fields read last time mean an equal
 * result. The config records reads only while the computation runs; a
 * reused result costs one comparison of those fields.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** A field's value, `derived.x` from `cfg.derived`.
   * @param {Record<string, any>} cfg @param {string} f */
  const valueAt = (cfg, f) => (f.startsWith('derived.')
    ? (cfg.derived || {})[f.slice(8)] : cfg[f]);

  /** @param {Record<string, any>} cfg @param {string[]} reads @param {string} extra */
  const keyOf = (cfg, reads, extra) =>
    JSON.stringify(reads.map((f) => valueAt(cfg, f))) + '\u0000' + extra;

  /**
   * `cfg` with every field read recorded in `reads`.
   * @param {Record<string, any>} cfg @param {Set<string>} reads
   * @returns {Record<string, any>}
   */
  const tracking = (cfg, reads) => {
    /** @type {Record<string, any> | null} */
    let derived = null;
    return new Proxy(cfg, {
      get(t, k) {
        if (typeof k !== 'string') return t[/** @type {any} */ (k)];
        if (k === 'derived' && t.derived && typeof t.derived === 'object') {
          return derived || (derived = new Proxy(t.derived, {
            get(d, j) {
              if (typeof j === 'string') reads.add('derived.' + j);
              return d[/** @type {any} */ (j)];
            }
          }));
        }
        reads.add(k);
        return t[k];
      }
    });
  };

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
     * The result for `cfg` and `extra`: the last one while what it read is
     * unchanged, else `compute` run on a recording config.
     * @param {Record<string, any>} cfg
     * @param {string} extra The other inputs, as one string.
     * @param {(cfg: Record<string, any>) => any} compute
     * @returns {boolean} Whether `compute` ran.
     */
    update(cfg, extra, compute) {
      if (this.reads && this.key === keyOf(cfg, this.reads, extra)) return false;
      /** @type {Set<string>} */
      const reads = new Set();
      this.value = compute(tracking(cfg, reads));
      this.reads = [...reads].sort();
      this.key = keyOf(cfg, this.reads, extra);
      return true;
    }
  }

  NS.ReadMemo = ReadMemo;
})();
