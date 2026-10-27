// @ts-check
/**
 * A Scope owns everything a block view sets up outside its own element:
 * listeners on document and window, resize and mutation observers, timers,
 * animation frames, chart instances, nodes placed elsewhere in the page and
 * child objects. `dispose()` removes all of it. The binding gives each view a
 * Scope and disposes it when the view goes; eslint forbids setting any of
 * these up outside this file.
 *
 * After dispose, a new registration is undone at once, so a late callback of
 * a removed view cannot leave anything behind.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /** Scopes created and not yet disposed, for leak tests. */
  let openCount = 0;

  class Scope {
    constructor() {
      /** @type {Set<() => void>} */
      this._undo = new Set();
      this.disposed = false;
      openCount += 1;
    }

    /**
     * Keep `undo` until dispose; returns a function that undoes it early.
     * @param {() => void} undo
     * @returns {() => void}
     */
    _keep(undo) {
      if (this.disposed) {
        undo();
        return () => {};
      }
      const once = () => {
        if (this._undo.delete(once)) undo();
      };
      this._undo.add(once);
      return once;
    }

    /**
     * @param {EventTarget} target
     * @param {string} type
     * @param {EventListener} fn
     * @param {boolean | AddEventListenerOptions} [opts]
     */
    on(target, type, fn, opts) {
      target.addEventListener(type, fn, opts);
      return this._keep(() => target.removeEventListener(type, fn, opts));
    }

    /**
     * @param {Element} el
     * @param {ResizeObserverCallback} fn
     */
    resize(el, fn) {
      const obs = new ResizeObserver(fn);
      obs.observe(el);
      return this._keep(() => obs.disconnect());
    }

    /**
     * @param {Node} node
     * @param {MutationCallback} fn
     * @param {MutationObserverInit} opts
     */
    mutate(node, fn, opts) {
      const obs = new MutationObserver(fn);
      obs.observe(node, opts);
      return this._keep(() => obs.disconnect());
    }

    /**
     * @param {() => void} fn
     * @param {number} ms
     */
    timeout(fn, ms) {
      /** @type {() => void} */
      let cancel = () => {};
      const t = setTimeout(() => {
        cancel();
        fn();
      }, ms);
      cancel = this._keep(() => clearTimeout(t));
      return cancel;
    }

    /**
     * @param {() => void} fn
     * @param {number} ms
     */
    interval(fn, ms) {
      const t = setInterval(fn, ms);
      return this._keep(() => clearInterval(t));
    }

    /** @param {() => void} fn */
    frame(fn) {
      /** @type {() => void} */
      let cancel = () => {};
      const h = requestAnimationFrame(() => {
        cancel();
        fn();
      });
      cancel = this._keep(() => cancelAnimationFrame(h));
      return cancel;
    }

    /**
     * `fn` called once, `ms` after the last of a run of calls.
     * @template {unknown[]} A
     * @param {(...args: A) => void} fn
     * @param {number} ms
     * @returns {(...args: A) => void}
     */
    debounce(fn, ms) {
      /** @type {() => void} */
      let cancel = () => {};
      return (...args) => {
        cancel();
        cancel = this.timeout(() => fn(...args), ms);
      };
    }

    /**
     * Anything with a `dispose()`: an echarts instance, a gear engine, a
     * child Scope. Returned as given.
     * @template {{ dispose: () => void }} T
     * @param {T} obj
     * @returns {T}
     */
    own(obj) {
      this._keep(() => obj.dispose());
      return obj;
    }

    /**
     * A node placed outside the view's element (a popover on <body>).
     * @template {Node} T
     * @param {T} node
     * @returns {T}
     */
    node(node) {
      this._keep(() => {
        if (node.parentNode) node.parentNode.removeChild(node);
      });
      return node;
    }

    /** How many things the scope still holds. */
    size() {
      return this._undo.size;
    }

    /** Undo everything, newest first. A second call does nothing. */
    dispose() {
      if (this.disposed) return;
      this.disposed = true;
      openCount -= 1;
      // Each undo function removes itself from the set as it runs.
      for (const u of Array.from(this._undo).reverse()) u();
    }

    /** Scopes created and not disposed. */
    static open() {
      return openCount;
    }
  }

  V.Scope = Scope;
})();
