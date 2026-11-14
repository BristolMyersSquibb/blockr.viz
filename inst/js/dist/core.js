// blockr.viz bundle dd0e3685310d22a88769398be5f2e384, built by tools/bundle.js. Do not edit.
// sources: core/namespace.js core/util.js core/lazy.js core/tooltip.js core/scope.js core/busy.js core/binding.js core/download.js core/capture.js core/titles.js core/ticks.js core/slot-pair.js core/gear/tray-nav.js core/gear/sections.js core/gear/tiles.js core/gear/icons.js core/gear/controls.js core/gear/range.js core/gear/ticks.js core/gear/profile.js core/gear/tray.js core/gear/counts.js core/gear/focus.js core/gear/engine.js core/frame.js core/data-view.js core/script-band.js spec/chart.js spec/table.js
// --- core/namespace.js
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
// --- core/util.js
// @ts-check
/**
 * Helpers every block view shares: one HTML escaper, one way to send an
 * input to R (and a selection, with its nonce), what a column
 * description (VizColumn) says about its values, and a number for reading. Timers and listeners are
 * not here; they belong to a Scope (scope.js), so they go away with their
 * block.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /** @type {Record<string, string>} */
  const ENTITIES = { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' };

  /**
   * Text made safe for HTML content and for a quoted attribute value.
   * @param {unknown} x
   * @returns {string}
   */
  const esc = (x) => String(x == null ? '' : x).replace(/[&<>"']/g, (c) => ENTITIES[c]);

  /** The input handler type R registered for each input (R/kit-input.R). */
  const TYPES = { select: 'blockr.viz.selection', peek: 'blockr.viz.selection',
                  capture: 'blockr.viz.capture', data: 'blockr.viz.data_view' };

  /**
   * Send one of a view's inputs to R: `<id>_set`, `<id>_select`,
   * `<id>_peek` (a selection only the data view reads), `<id>_capture` or
   * `<id>_data` (core/data-view.js), typed so R's input
   * handler checks it on arrival, or
   * `<id>_need` (the data revision the view holds no rows for) or
   * `<id>_counts` (the gear asks for the columns' distinct counts). Priority
   * "event", so the same value sent twice arrives twice.
   * @param {string} id The view's element id.
   * @param {'set' | 'select' | 'peek' | 'capture' | 'data' | 'need' | 'counts'} kind
   * @param {unknown} value
   * @param {string} [block] For `set`: the block whose spec reads it.
   * @returns {boolean} Whether Shiny took it.
   */
  const send = (id, kind, value, block) => {
    const shiny = root.Shiny;
    if (!id || !shiny || typeof shiny.setInputValue !== 'function') return false;
    const type = kind === 'set' ? 'blockr.viz.' + block
      : kind === 'need' || kind === 'counts' ? null : TYPES[kind];
    const name = id + '_' + kind + (type ? ':' + type : '');
    shiny.setInputValue(name, value, { priority: 'event' });
    return true;
  };

  // One counter for the page: every selection a view sends carries a new
  // number, so R tells a second click on the same mark (or a second clear)
  // from a board re-evaluation that hands back the value it already had.
  let nonce = 0;

  /**
   * Send a view's selection to R as `<id>_select`: `{selection, nonce}`,
   * `selection` null for a clear.
   * @param {string} id The view's element id.
   * @param {Record<string, unknown> | null} selection
   * @returns {boolean} Whether Shiny took it.
   */
  const select = (id, selection) => send(id, 'select', { selection, nonce: ++nonce });

  /**
   * A column that holds numbers: a number or a duration. A date or
   * date-time is not; isTemporal() names those.
   * @param {VizColumn | null | undefined} c
   */
  const isNumeric = (c) => !!c && (c.class === 'number' || c.class === 'duration');

  /** @param {VizColumn | null | undefined} c */
  const isTemporal = (c) => !!c && (c.class === 'date' || c.class === 'datetime');

  /**
   * A number for reading: up to two decimals, and a value that would round
   * to 0 keeps two significant digits (0.0034). Display only; the data
   * keeps full precision. Anything else comes back as it is.
   * @param {unknown} v
   * @returns {unknown}
   */
  const num = (v) => {
    if (typeof v !== 'number' || !isFinite(v)) return v;
    if (Number.isInteger(v)) return v.toLocaleString();
    if (Math.abs(v) < 0.005) return v.toLocaleString(undefined, { maximumSignificantDigits: 2 });
    return v.toLocaleString(undefined, { maximumFractionDigits: 2 });
  };

  /**
   * A raw observation for reading: num() of it at three significant digits.
   * @param {unknown} v
   * @returns {unknown}
   */
  const num3 = (v) => (typeof v === 'number' && isFinite(v) ? num(Number(v.toPrecision(3))) : v);

  V.util = { esc, send, select, isNumeric, isTemporal, num, num3 };
})();
// --- core/lazy.js
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
// --- core/tooltip.js
// @ts-check
/**
 * The data tooltip both blocks draw: a headline and label/value rows,
 * styled by tooltip.css. The pieces take HTML the caller has escaped
 * (V.util.esc); a colour is escaped here.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const esc = V.util.esc;

  /** A colour swatch, or nothing without a colour. @param {unknown} c */
  const swatch = (c) => (typeof c === 'string' && c
    ? '<span class="vz-tt-sw" style="background:' + esc(c) + '"></span>' : '');

  /** @param {string} html @param {unknown} [color] */
  const head = (html, color) =>
    '<div class="vz-tt-head">' + swatch(color) + '<span>' + html + '</span></div>';

  /**
   * A label/value row. `hit` marks a row of a card listing every level of a
   * colour split: true for the level under the pointer, false for the others.
   * @param {string} label @param {unknown} value @param {unknown} [color]
   * @param {boolean | null} [hit]
   */
  const row = (label, value, color, hit) =>
    '<div class="vz-tt-row' + (hit === true ? ' is-hit' : hit === false ? ' is-dim' : '') +
    '">' + swatch(color) + '<span class="vz-tt-label">' +
    label + '</span><span class="vz-tt-value">' + value + '</span></div>';

  /** A muted line of its own, for what the rows below it are. @param {string} html */
  const note = (html) => '<div class="vz-tt-row vz-tt-label">' + html + '</div>';

  V.tooltip = { swatch, head, row, note, SEP: '<div class="vz-tt-sep"></div>' };
})();
// --- core/scope.js
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
// --- core/busy.js
// @ts-check
/**
 * The busy cue: an output being recomputed dims, and a line under its
 * sentence counts the wait ("drawing… 1.2 s"). Shared by every viz output and
 * by blocks outside this package that render through viz.
 *
 * R starts it with the "blockr-busy" message, sent before the slow work: a
 * custom message reaches the browser while R is still computing, whereas
 * Shiny's `.recalculating` arrives only after blockr.core's compute. The view
 * that owns the output ends it once the new picture is on screen, or R sends
 * "blockr-busy-done" when no new picture is coming (an identical payload, an
 * error).
 *
 * Nothing shows for the first BUSY_AFTER ms: a cue that flashes for a few
 * frames is noise. The class goes on the element with the output's id; the
 * line is its `.vz-busy-line` descendant, looked up on every tick since a
 * view may rebuild its DOM.
 *
 * Exposed as `Blockr.viz.busy` and, for the views and other packages,
 * `Blockr.busy`.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  const BUSY_AFTER = 300;
  // R's done waits this long: a block being built reads an input that is not
  // ready yet, sends done, and starts again a few ms later. One wait, one
  // clock.
  const DONE_SETTLE = 250;

  /** Owns every timer and listener of the cue; it lives as long as the page. */
  const page = new V.Scope();

  /** @typedef {{ delay: (() => void) | null, clock: (() => void) | null,
   *              ending: (() => void) | null }} Pending */
  /** @type {Record<string, Pending>} */
  const pending = {};
  /** Ids R has spoken about: never watched, since an idle one would tick on. */
  /** @type {Record<string, boolean>} */
  const heard = {};
  /** Ids whose cue a drawn view has ended. A view built again under such an
   * id draws Shiny's last value at once; no wait comes, so it is not watched. */
  /** @type {Record<string, boolean>} */
  const ended = {};
  /** @type {Record<string, string>} */
  const watched = {};

  /** @param {Pending} p */
  const cancel = (p) => {
    for (const k of /** @type {const} */ (['delay', 'clock', 'ending'])) {
      const c = p[k];
      if (c) c();
      p[k] = null;
    }
  };

  /** @param {string} id */
  function clear(id) {
    const p = pending[id];
    if (!p) return;
    cancel(p);
    delete pending[id];
  }

  /**
   * A second start while one is pending keeps the first clock: the wait the
   * reader sees began then.
   * @param {string} id @param {string} [label]
   */
  function start(id, label) {
    const q = pending[id];
    if (q) {
      if (q.ending) q.ending();
      q.ending = null;
      return;
    }
    const t0 = performance.now();
    /** @type {Pending} */
    const p = { delay: null, clock: null, ending: null };
    pending[id] = p;
    const tick = () => {
      if (pending[id] !== p) return;
      const el = root.document.getElementById(id);
      if (!el) return;
      el.classList.add('vz-busy');
      const line = el.querySelector('.vz-busy-line');
      if (line) {
        line.textContent = (label || 'computing') + '… ' +
          ((performance.now() - t0) / 1000).toFixed(1) + ' s';
      }
    };
    p.delay = page.timeout(() => {
      p.delay = null;
      tick();
      p.clock = page.interval(tick, 100);
    }, BUSY_AFTER);
  }

  /** @param {string} id */
  function stop(id) {
    ended[id] = true;
    clear(id);
    delete watched[id];
    const el = root.document.getElementById(id);
    if (el) el.classList.remove('vz-busy');
  }

  /** @param {string} id */
  function done(id) {
    heard[id] = true;
    const p = pending[id];
    if (!p) {
      stop(id);
      return;
    }
    if (!p.ending) {
      p.ending = page.timeout(() => {
        if (pending[id] === p) stop(id);
      }, DONE_SETTLE);
    }
  }

  /** @param {Element | null} el */
  function shown(el) {
    if (!el || !el.isConnected) return false;
    if (typeof el.checkVisibility === 'function') {
      return el.checkVisibility({ visibilityProperty: true });
    }
    for (let e = /** @type {Element | null} */ (el); e; e = e.parentElement) {
      const cs = root.getComputedStyle(e);
      if (cs.display === 'none' || cs.visibility === 'hidden') return false;
    }
    return true;
  }

  function sweep() {
    for (const id of Object.keys(watched)) {
      if (!shown(root.document.getElementById(id))) continue;
      const label = watched[id];
      delete watched[id];
      start(id, label);
    }
  }

  /**
   * An output R has not spoken about yet starts its clock the moment it
   * comes on screen. A board that builds blocks lazily makes one only when
   * its panel shows, and R's "blockr-busy" follows a second or more later.
   * An output that has drawn before is not watched: a dock that builds its
   * panel again sends no new value, so nothing would stop the clock.
   * @param {string} id @param {string} label
   */
  function watch(id, label) {
    if (heard[id] || ended[id] || pending[id]) return;
    watched[id] = label;
  }

  // dockViewR's bubbling event when a tab comes to the front. The panel shows
  // a frame or two later (blockr.dock moves the card in), so the check runs
  // on every frame for about a second.
  let frames = 0;
  const sweepFrames = () => {
    sweep();
    if (--frames > 0) page.frame(sweepFrames);
  };
  page.on(root.document, 'dockview:active-panel', () => {
    const idle = frames <= 0;
    frames = 60;
    if (idle) page.frame(sweepFrames);
  });

  const busy = { start, stop, watch };
  V.busy = busy;
  root.Blockr.busy = busy;

  const shiny = root.Shiny;
  if (shiny && typeof shiny.addCustomMessageHandler === 'function') {
    shiny.addCustomMessageHandler('blockr-busy', (/** @type {{ id: string, label?: string }} */ msg) => {
      heard[msg.id] = true;
      delete watched[msg.id];
      start(msg.id, msg.label);
    });
    shiny.addCustomMessageHandler('blockr-busy-done', (/** @type {{ id: string }} */ msg) => done(msg.id));
  }
})();
// --- core/binding.js
// @ts-check
/**
 * The Shiny output binding every block view shares. A Shiny output decides
 * when R builds a view's payload; the payload itself arrives as a
 * `blockr-viz:payload` message, sent the moment it is built, and the
 * output's value, which Shiny sends once the whole flush is done, is a token
 * that says so (R/kit-output.R). A session without messages (a test) sends
 * the payload as the output's value, and the binding draws that as well.
 *
 * What the binding adds to Shiny's:
 * - the last payload posted for an id is kept: an element that binds after
 *   the message came, or again after a move, draws from it without asking R;
 * - one view per element, built on the first value, with its own Scope;
 * - a value already drawn (Shiny replays it on every re-bind) is skipped;
 * - the view is told whether the rows are new to it (`rowsChanged`), so it
 *   parses and indexes them only then;
 * - rows are kept per id, because R leaves them out of a payload whose data
 *   revision the client holds; a payload without rows the client does not
 *   hold asks R for them once (`<id>_need`). The column description is kept
 *   with them: such a payload carries `columns` only when they changed;
 * - an error without a message (a silent req()) keeps the picture;
 * - a light/dark switch (data-bs-theme anywhere on the page) renders the
 *   last value again, so a view reads the new colours;
 * - with a `busy` label, the view is built as soon as Shiny finds the
 *   element, and the busy cue counts the wait for R's first value from
 *   when the element shows;
 * - when Shiny unbinds an element that has left the page, its view is
 *   disposed, and its rows are dropped after a grace period unless a new
 *   element under the same id mounts first.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /** @type {Map<string, { rev: number, data: unknown, columns: any }>} */
  const rows = new Map();
  /** The last payload R posted for each id. @type {Map<string, VizPayload>} */
  const posted = new Map();
  /** @type {Map<string, VizMount>} */
  const live = new Map();
  /** @type {Map<string, () => void>} */
  const dropping = new Map();
  /** Owns the timers that drop rows; it lives as long as the page. */
  const pageScope = new V.Scope();

  /**
   * @param {HTMLElement} el
   * @param {VizViewClass} View
   * @param {number} grace
   * @returns {VizMount}
   */
  function mount(el, View, grace) {
    const old = live.get(el.id);
    if (old) old.scope.dispose();
    const cancelDrop = dropping.get(el.id);
    if (cancelDrop) {
      cancelDrop();
      dropping.delete(el.id);
    }
    const scope = new V.Scope();
    /** @type {VizMount} */
    const m = { el, scope, view: scope.own(new View(el, scope)), last: null,
                needRev: null, drawn: null };
    scope.mutate(document.documentElement, () => { if (m.drawn) m.view.render(m.drawn, false); },
                 { attributes: true, attributeFilter: ['data-bs-theme'], subtree: true });
    live.set(el.id, m);
    // A re-bind of the same element unbinds it first; only an element that
    // has left the page loses its view.
    const $el = root.jQuery(el);
    const onUnbound = () => {
      scope.timeout(() => {
        if (!el.isConnected) unmount(m, grace);
      }, 0);
    };
    $el.on('shiny:unbound', onUnbound);
    scope.own({ dispose: () => $el.off('shiny:unbound', onUnbound) });
    /** @type {any} */ (el)._viz = m;
    // A payload posted before this element bound, or to the one before it.
    const waiting = posted.get(el.id);
    if (waiting) draw(m, waiting);
    return m;
  }

  /**
   * @param {VizMount} m
   * @param {number} grace
   */
  function unmount(m, grace) {
    m.scope.dispose();
    /** @type {any} */ (m.el)._viz = null;
    const id = m.el.id;
    if (live.get(id) !== m) return;
    live.delete(id);
    dropping.set(id, pageScope.timeout(() => {
      dropping.delete(id);
      if (!live.has(id)) {
        rows.delete(id);
        posted.delete(id);
      }
    }, grace));
  }

  /**
   * @param {HTMLElement} el
   * @param {VizPayload | null} payload
   * @param {VizViewClass} View
   * @param {number} grace
   */
  function render(el, payload, View, grace) {
    const m = /** @type {VizMount} */ (/** @type {any} */ (el)._viz || mount(el, View, grace));
    // The output's token: the payload came, or comes, as a message.
    if (payload && /** @type {any} */ (payload).posted != null) {
      const p = posted.get(el.id);
      if (p) {
        draw(m, p);
      } else if (m.needRev !== payload.data_rev) {
        // A token without its message: the page was not there to take it.
        m.needRev = payload.data_rev;
        V.util.send(el.id, 'need', payload.data_rev);
      }
      return;
    }
    draw(m, payload);
  }

  /**
   * R's `blockr-viz:payload`: keep it, and draw it if its element has a view.
   * @param {{ id?: string, payload?: VizPayload | null }} msg
   */
  function post(msg) {
    if (!msg || !msg.id || !msg.payload) return;
    posted.set(msg.id, msg.payload);
    const m = live.get(msg.id);
    if (m) draw(m, msg.payload);
  }

  /**
   * @param {VizMount} m
   * @param {VizPayload | null} payload
   */
  function draw(m, payload) {
    const el = m.el;
    if (payload == null || m.last === payload) return;
    m.last = payload;
    let p = payload;
    if (p.data != null) {
      rows.set(el.id, { rev: p.data_rev, data: p.data, columns: p.columns });
    } else {
      const held = rows.get(el.id);
      if (!held || held.rev !== p.data_rev) {
        if (m.needRev !== p.data_rev) {
          m.needRev = p.data_rev;
          V.util.send(el.id, 'need', p.data_rev);
        }
        return;
      }
      // `columns` absent: the ones held. Present, null included: the new ones.
      if (p.columns === undefined) {
        p = Object.assign({}, p, { data: held.data, columns: held.columns });
      } else {
        held.columns = p.columns;
        p = Object.assign({}, p, { data: held.data });
      }
    }
    m.needRev = null;
    // The view parses its rows only when they are new to it.
    const rowsChanged = !m.drawn || m.drawn.data_rev !== p.data_rev;
    m.drawn = p;
    m.view.render(p, rowsChanged);
  }

  /**
   * A found element without a view waits for R's first value. A board that
   * builds blocks lazily binds the element when its panel shows, and R
   * speaks a second or more later. The view is built now, so its busy line
   * exists, and the busy cue's watch() (core/busy.js) counts the wait.
   * @param {HTMLElement} el @param {VizViewClass} View @param {number} grace
   * @param {string} label
   */
  function early(el, View, grace, label) {
    if (!el.id || /** @type {any} */ (el)._viz) return;
    mount(el, View, grace);
    const busy = root.Blockr && root.Blockr.busy;
    if (busy && typeof busy.watch === 'function') busy.watch(el.id, label);
  }

  /**
   * Register the output binding for one kind of view.
   * @param {string} name The binding's name ("blockr.viz.chart").
   * @param {string} view The elements' `data-viz-view` ("chart").
   * @param {VizViewClass} View Builds the view: `new View(el, scope)`.
   * @param {{ grace?: number, busy?: string }} [opts] `grace`: how long, in
   *   ms, the rows of a removed element are kept for a new element under the
   *   same id. `busy`: the busy cue's word for the first wait ("drawing").
   * @returns {boolean} Whether Shiny took it.
   */
  function binding(name, view, View, opts = {}) {
    const shiny = root.Shiny;
    if (!shiny || !shiny.OutputBinding || !root.jQuery) return false;
    const grace = opts.grace == null ? 5000 : opts.grace;
    const b = new shiny.OutputBinding();
    Object.assign(b, {
      /** @param {unknown} scope */
      find: (scope) => {
        const els = root.jQuery(scope).find('.blockr-viz-output[data-viz-view="' + view + '"]');
        const label = opts.busy;
        if (label) for (const el of Array.from(els)) early(el, View, grace, label);
        return els;
      },
      /** @param {HTMLElement} el @param {VizPayload | null} payload */
      renderValue: (el, payload) => render(el, payload, View, grace),
      /** @param {HTMLElement} el @param {{ message?: string }} err */
      renderError: (el, err) => {
        const m = /** @type {any} */ (el)._viz;
        if (err && err.message && m && typeof m.view.error === 'function') {
          m.view.error(err.message);
        }
      },
      clearError: () => {},
      // Shiny marks a recalculating output with a class on its element, and
      // takes it off when the value arrives. The element holds the whole
      // view, so each change restyles every node under it: a table of
      // 57,000 nodes twice more than it needs. The view has its own busy
      // cue (core/busy.js), and its payload arrives before the output's
      // value, so the class would dim a picture that is already new.
      showProgress: () => {}
    });
    shiny.outputBindings.register(b, name);
    return true;
  }

  binding.post = post;
  V.binding = binding;
  const shiny0 = root.Shiny;
  if (shiny0 && typeof shiny0.addCustomMessageHandler === 'function') {
    shiny0.addCustomMessageHandler('blockr-viz:payload', post);
  }
})();
// --- core/download.js
// @ts-check
/**
 * A view's download control, built from the payload's `derived.downloads`
 * (R/export-registry.R, viz_downloads()): one format is a tool that
 * downloads, several are blockr.ui's action menu with one row per format.
 * A chart's menu has two sections, Chart and Data; a Data row carries its
 * table's count ("Rows 40 of 254"). R builds the tables behind the counts
 * only when asked: reaching for the control sets the entries' `ask` input,
 * and R answers with a `blockr-viz:counts` message, which fills the counts
 * in place.
 * The markup is blockr.ui's own (action_menu(), menu_item()), so its page
 * listeners open and close the menu. The links are Shiny download links,
 * bound here so their href arrives, and unbound when the view goes.
 *
 * Reaching for the control runs the view's `onOpen`, which sends R the
 * picture the downloads write. A click on a format while that picture is
 * still on its way is held until it has gone out, so the first click
 * writes the picture rather than failing for want of it.
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

  /** @param {VizDownload} d */
  const item = (d) => '<a id="' + esc(d.id) + '" class="shiny-download-link ' +
    'blockr-menu__item" role="menuitem" tabindex="-1" href="" download ' +
    'target="_blank"><span class="blockr-menu__label">' + esc(d.label) +
    (d.count || d.ask
      ? '<span class="vz-download__count">' + esc(d.count || '') + '</span>' : '') +
    '</span><span class="blockr-menu__meta">.' + esc(d.ext) + '</span></a>';

  /** The rows, with a title over each section (Chart, Data) when R names them.
   * @param {VizDownload[]} list */
  const rows = (list) => {
    let out = '';
    /** @type {string | undefined} */
    let section;
    for (const d of list) {
      if (d.section && d.section !== section) {
        section = d.section;
        out += '<div class="blockr-menu__title">' + esc(section) + '</div>';
      }
      out += item(d);
    }
    return out;
  };

  /** @param {VizDownload[]} list */
  const menu = (list) => '<span class="blockr-action-menu" data-align="end">' +
    '<button type="button" class="blockr-tool blockr-action-menu__trigger" ' +
    'aria-haspopup="menu" aria-expanded="false" aria-label="Download" ' +
    'data-blockr-tooltip="Download">' + ICON + '</button>' +
    '<div class="blockr-menu" role="menu" tabindex="-1" hidden>' +
    rows(list) + '</div></span>';

  /** @type {WeakMap<HTMLElement, { key: string, offs: (() => void)[] }>} */
  const drawn = new WeakMap();

  // One number for the page: an `ask` input set twice arrives twice.
  let asked = 0;

  /**
   * R's answer to an `ask`: the count of each data entry, by the entry's
   * element id. By id, since an open menu sits outside its host.
   * @param {{ counts?: Record<string, string> }} msg
   */
  const counts = (msg) => {
    const list = (msg && msg.counts) || {};
    for (const id of Object.keys(list)) {
      const link = root.document.getElementById(id);
      const span = link && link.querySelector('.vz-download__count');
      if (span) span.textContent = list[id];
    }
  };

  // After the picture is sent, Shiny posts the input on its next turn; the
  // download request must come after it.
  const SEND_MS = 30;

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
   * @param {{ onOpen?: () => unknown }} [opts] `onOpen` runs when the
   *   reader reaches for the control (opens the menu, or points at the one
   *   tool), which is when a view takes its picture. A promise it returns
   *   holds a click on a format until it settles.
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
    // The counts of the data entries, asked for when the reader reaches for
    // the control.
    const asks = Array.from(new Set(items.map((d) => d.ask).filter(Boolean)));
    if (asks.length) {
      const ask = () => {
        const shiny = root.Shiny;
        if (!shiny || typeof shiny.setInputValue !== 'function') return;
        for (const name of asks) {
          shiny.setInputValue(/** @type {string} */ (name), ++asked, { priority: 'event' });
        }
      };
      for (const type of ['pointerenter', 'pointerdown', 'focus']) {
        offs.push(scope.on(reach, type, ask));
      }
    }
    const onOpen = opts.onOpen;
    if (onOpen) {
      /** The picture on its way, or null. @type {Promise<unknown> | null} */
      let sending = null;
      const open = () => {
        const p = onOpen();
        if (!p || typeof (/** @type {any} */ (p)).then !== 'function') return;
        const mine = sending = Promise.resolve(p).catch(() => {})
          .then(() => new Promise((r) => scope.timeout(() => r(null), SEND_MS)))
          .then(() => { if (sending === mine) sending = null; });
      };
      // blockr.ui's menu takes the trigger's click for itself, so the
      // pointer's way there (and the keyboard's focus) is what runs onOpen.
      for (const type of ['pointerenter', 'pointerdown', 'focus']) {
        offs.push(scope.on(reach, type, open));
      }
      // A click held for the picture comes back with `through` set. On the
      // links themselves: an open menu sits outside the host.
      let through = false;
      /** @param {Event} e */
      const hold = (e) => {
        if (through) { through = false; return; }
        if (!sending) return;
        const link = /** @type {HTMLElement} */ (e.currentTarget);
        e.preventDefault();
        e.stopImmediatePropagation();
        sending.then(() => {
          through = true;
          link.click();
        });
      };
      for (const link of host.querySelectorAll('a.shiny-download-link')) {
        offs.push(scope.on(link, 'click', hold));
      }
    }
    const shiny = root.Shiny;
    if (shiny && typeof shiny.bindAll === 'function') shiny.bindAll(host);
  }

  downloads.counts = counts;
  V.downloads = downloads;
  const shiny0 = root.Shiny;
  if (shiny0 && typeof shiny0.addCustomMessageHandler === 'function') {
    shiny0.addCustomMessageHandler('blockr-viz:counts', counts);
  }
})();
// --- core/capture.js
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
    host.className = 'vz-capture-host';
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
// --- core/titles.js
// @ts-check
/**
 * Titles: the gear's template rows (title, subtitle, caption) with their
 * `@` menu, and the sentence on the block's face, whose words open their
 * setting's control.
 *
 * Both read `derived.titles` from the payload (VizTitles in gear.d.ts):
 * per band the parts R resolved (`{text, arg?, by?}`) and the settings it
 * would name if they were set (`offers`); `args`, what each `{@name}`
 * prints now; and `choices`, the values a sentence lists for a word. The JS never resolves
 * a template, so the face, the preview and the exports say the same.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const ui = () => /** @type {VizUi} */ (root.Blockr);

  /** @typedef {{ label: string, tok: string, val: string, indent?: boolean }} AtItem */
  /** @typedef {{ head: string, note?: string, items: AtItem[] }} AtGroup */
  /** @typedef {{ el: HTMLElement, pos: number, query: string, idx: number,
   *              items: { tok: string, el: HTMLElement }[], scope: VizScope }} AtState */

  /** What the `@` menu offers, in groups, under public names. The prepare script's
   * values get their own group: a script value takes the word of a block setting with the
   * same name, and the group's note says which.
   * @param {VizTitles} titles @param {VizColumn[]} columns
   * @param {(key: string) => string} label @param {(key: string) => VizWordOf} [wordOf]
   * @returns {AtGroup[]} */
  function menuGroups(titles, columns, label, wordOf) {
    const args = titles.args || {};
    /** @type {AtItem[]} */
    const settings = [];
    /** @type {AtItem[]} */
    const script = [];
    /** @type {string[]} */
    const shadowed = [];
    for (const name of Object.keys(args)) {
      const w = wordOf ? wordOf(name) : null;
      const into = w && w.script ? script : settings;
      if (w && w.script && w.shadows) shadowed.push('{@' + name + '}');
      const printed = args[name] == null ? '' : String(args[name]);
      into.push({ label: w ? w.label : label(name), tok: '{@' + name + '}', val: printed });
      const col = columns.find((c) => c.name === printed);
      if (col) {
        into.push({ label: 'as its variable label', indent: true,
                    tok: '{label(@' + name + ')}', val: col.label || col.name });
      }
    }
    /** @type {AtItem[]} */
    const cols = [];
    for (const c of columns) {
      cols.push({ label: c.name, tok: '{' + c.name + '}', val: c.label || '' },
                { label: 'as its variable label', indent: true,
                  tok: '{label(' + c.name + ')}', val: c.label || c.name },
                { label: 'as a count of distinct values', indent: true,
                  tok: '{n_distinct(' + c.name + ')}', val: '' });
    }
    /** @type {AtGroup[]} */
    const groups = [];
    if (settings.length) {
      groups.push({ head: "This block's settings", note: 'Printed as a word the reader can click.',
                    items: settings });
    }
    if (script.length) {
      const note = shadowed.length
        ? shadowed.join(', ') + " is the script's, not the block setting of that name, " +
          'which stays in the gear.'
        : 'Printed as a word the reader can click.';
      groups.push({ head: "The prepare script's values", note, items: script });
    }
    groups.push({ head: 'Counts and filters', items: [
      { label: 'Row count', tok: '{n}', val: '' },
      { label: 'Filter trail', tok: '{filters}', val: 'what the filters upstream applied' }
    ] }, { head: 'Structure', items: [
      { label: 'Optional clause', tok: '[ ]', val: 'drops when its setting is empty' },
      { label: 'Line break', tok: '\\n', val: 'breaks the sentence' }
    ] });
    if (cols.length) {
      groups.push({ head: 'Data columns', note: "Prints the column's values, not its name.",
                    items: cols });
    }
    return groups;
  }

  /** Tokens that name nothing R knows; R prints them as nothing.
   * @param {string} tpl @param {VizTitles} titles @param {VizColumn[]} columns */
  function unknownTokens(tpl, titles, columns) {
    const known = new Set(['n', 'filters']);
    for (const name of Object.keys(titles.args || {})) known.add('@' + name);
    for (const c of columns) known.add(c.name);
    /** @type {string[]} */
    const bad = [];
    for (const m of String(tpl).matchAll(/\{([^{}]+)\}/g)) {
      const tok = m[1].trim();
      const inner = /^[A-Za-z_.][A-Za-z0-9_.]*\(\s*(.+?)\s*\)$/.exec(tok);
      const name = (inner ? inner[1] : tok).trim().replace(/\s+in\s+.*$/, '');
      if (!known.has(name) && !bad.includes(m[0])) bad.push(m[0]);
    }
    return bad;
  }

  /** Every argument the title, subtitle and caption name: their words, and the settings
   * offered beside them because a clause dropped.
   * @param {VizTitles | null | undefined} titles @returns {string[]} */
  function namedArgs(titles) {
    /** @type {Set<string>} */
    const out = new Set();
    for (const b of [titles && titles.title, titles && titles.subtitle, titles && titles.caption]) {
      if (!b || typeof b !== 'object') continue;
      for (const p of b.parts || []) if (p && p.arg) out.add(p.arg);
      for (const o of b.offers || []) if (typeof o === 'string') out.add(o);
    }
    return Array.from(out);
  }

  /** The resolved band as text, its slot words marked, and up to three offers. A word's
   * tooltip names what it opens, and says so when that is the prepare script's control.
   * @param {HTMLElement} el @param {VizTitleBand | null | undefined} band
   * @param {(k: string) => string} label @param {(k: string) => VizWordOf} [wordOf] */
  function paintPreview(el, band, label, wordOf) {
    el.textContent = '';
    const parts = (band && band.parts) || [];
    if (!parts.length && band && band.text) el.textContent = band.text;
    for (const p of parts) {
      if (!p || !p.text) continue;
      if (!p.arg) { el.appendChild(document.createTextNode(p.text)); continue; }
      const w = document.createElement('span');
      w.className = 'blockr-slot blockr-slot--preview';
      w.textContent = p.text;
      const wo = wordOf ? wordOf(p.arg) : null;
      w.title = wo && wo.script ? wo.label + ', from the prepare script' : label(p.arg);
      el.appendChild(w);
    }
    for (const arg of ((band && band.offers) || []).slice(0, 3)) {
      const chip = document.createElement('span');
      chip.className = 'vz-template-offer';
      chip.textContent = '+ ' + label(arg);
      el.appendChild(chip);
    }
    el.classList.toggle('vz-template-preview--empty', !el.childNodes.length);
  }

  /** Under the field, flipped above it when there is no room below.
   * @param {HTMLElement} inp @param {HTMLElement} el */
  function place(inp, el) {
    const r = inp.getBoundingClientRect();
    const box = el.getBoundingClientRect();
    const M = 8;
    el.style.left = Math.max(M, Math.min(r.left, window.innerWidth - box.width - M)) + 'px';
    let top = r.bottom + 4;
    if (top + box.height > window.innerHeight - M) {
      const above = r.top - box.height - 4;
      top = above >= M ? above : Math.max(M, window.innerHeight - box.height - M);
    }
    el.style.top = top + 'px';
  }

  /** @param {AtItem} it @param {AtState} s @param {(i: number) => void} pick
   * @param {() => void} hl */
  function menuItem(it, s, pick, hl) {
    const b = document.createElement('button');
    b.type = 'button';
    b.className = 'vz-at-item' + (it.indent ? ' vz-at-item--sub' : '');
    const lab = document.createElement('b');
    lab.textContent = it.label;
    const val = document.createElement('span');
    val.className = 'vz-at-val' + (it.val ? '' : ' vz-at-val--unset');
    val.textContent = it.val || (it.indent ? '' : 'unset');
    const tok = document.createElement('span');
    tok.className = 'vz-at-tok';
    tok.textContent = it.tok;
    b.append(lab, val, tok);
    const idx = s.items.length;
    // mousedown, prevented: a click would blur the field first, and blur
    // commits it.
    s.scope.on(b, 'mousedown', (e) => { e.preventDefault(); pick(idx); });
    s.scope.on(b, 'mouseenter', () => { s.idx = idx; hl(); });
    s.items.push({ tok: it.tok, el: b });
    return b;
  }

  /** Fill an open menu with the items that match its query.
   * @param {AtState} s @param {AtGroup[]} groups @param {(i: number) => void} pick
   * @param {() => void} hl */
  function drawMenu(s, groups, pick, hl) {
    const q = s.query.toLowerCase();
    s.el.textContent = '';
    s.items = [];
    const hit = (/** @type {AtItem} */ it) =>
      (it.label + ' ' + it.tok + ' ' + it.val).toLowerCase().includes(q);
    groups.forEach((g, gi) => {
      // A sub-row rides on the row above it.
      const hits = g.items.filter((it, i) => {
        const parent = it.indent ? g.items.slice(0, i).reverse().find((x) => !x.indent) : null;
        return hit(it) || (!!parent && hit(parent));
      });
      if (!hits.length) return;
      const h = document.createElement('div');
      h.className = 'vz-at-head';
      h.textContent = g.head;
      s.el.appendChild(h);
      if (g.note && !q) {
        const n = document.createElement('div');
        n.className = 'vz-at-note';
        n.textContent = g.note;
        s.el.appendChild(n);
      }
      for (const it of hits) s.el.appendChild(menuItem(it, s, pick, hl));
      if (gi < groups.length - 1) {
        const sep = document.createElement('div');
        sep.className = 'vz-at-sep';
        s.el.appendChild(sep);
      }
    });
    if (!s.items.length) {
      const n = document.createElement('div');
      n.className = 'vz-at-note';
      n.textContent = 'Nothing matches "' + s.query + '".';
      s.el.appendChild(n);
    }
    if (s.idx >= s.items.length) s.idx = 0;
    hl();
  }

  /** The `@` menu of one template field. Portalled to <body>; Escape, a
   * click elsewhere, a space or brace typed, or the field leaving the screen
   * close it. A pick writes the token at the `@`.
   * @param {HTMLInputElement} inp @param {VizRowCtx} ctx */
  function atMenu(inp, ctx) {
    /** @type {AtState | null} */
    let st = null;
    const close = () => {
      const s = st;
      st = null;
      if (s) s.scope.dispose();
    };
    const hl = () => {
      if (!st) return;
      const s = st;
      s.items.forEach((it, i) => it.el.classList.toggle('vz-at-item--hl', i === s.idx));
      const cur = s.items[s.idx];
      if (cur && typeof cur.el.scrollIntoView === 'function') cur.el.scrollIntoView({ block: 'nearest' });
    };
    const pick = (/** @type {number} */ i) => {
      if (!st || !st.items[i]) return;
      const raw = st.items[i].tok;
      const tok = raw === '[ ]' ? '[, ]' : raw;
      const caret = inp.selectionStart || 0;
      const from = st.pos;
      inp.value = inp.value.slice(0, from) + tok + inp.value.slice(caret);
      close();
      const pos = from + (raw === '[ ]' ? tok.length - 1 : tok.length);
      inp.focus();
      inp.setSelectionRange(pos, pos);
      inp.dispatchEvent(new Event('input'));
    };
    const draw = () => {
      if (!st) return;
      drawMenu(st, menuGroups(ctx.titles, ctx.columns, ctx.labelOf, ctx.wordOf), pick, hl);
      place(inp, st.el);
    };
    const open = () => {
      close();
      const scope = ctx.scope.own(new V.Scope());
      const el = scope.node(document.body.appendChild(document.createElement('div')));
      el.className = 'vz-at-menu';
      st = { el, pos: Math.max(0, (inp.selectionStart || 1) - 1), query: '', idx: 0, items: [],
             scope };
      const layer = ui().layer;
      if (layer) {
        const h = layer(el, { escape: close, outside: close, from: inp });
        scope.own({ dispose: () => h.remove() });
      }
      scope.on(document, 'scroll', (e) => {
        if (!st || st.el.contains(/** @type {Node} */ (e.target))) return;
        const r = inp.getBoundingClientRect();
        if (r.bottom < 0 || r.top > window.innerHeight) close();
        else place(inp, st.el);
      }, true);
      draw();
    };
    const refilter = () => {
      if (!st) return;
      const caret = inp.selectionStart || 0;
      const q = inp.value.slice(st.pos + 1, caret);
      if (caret <= st.pos || inp.value[st.pos] !== '@' || /[\s{}[\]]/.test(q)) { close(); return; }
      st.query = q;
      draw();
    };
    const keydown = (/** @type {KeyboardEvent} */ e) => {
      if (!st) return false;
      const n = st.items.length;
      if (e.key === 'Escape') { e.preventDefault(); close(); return true; }
      if (!n) return false;
      if (e.key === 'Enter' || e.key === 'Tab') { e.preventDefault(); pick(st.idx); return true; }
      const step = e.key === 'ArrowDown' ? 1 : e.key === 'ArrowUp' ? -1 : 0;
      if (!step) return false;
      e.preventDefault();
      st.idx = (st.idx + step + n) % n;
      hl();
      return true;
    };
    return { open, refilter, keydown, close };
  }

  /** A template row: the field, what the card shows now, a warning for a
   * token nothing answers to, and the help line. An emptied field goes back
   * to the automatic text (`null`).
   * @param {HTMLElement} el @param {VizRowCtx} ctx */
  function row(el, ctx) {
    const band = /** @type {VizTitleBand | null | undefined} */ (
      /** @type {any} */ (ctx.titles)[ctx.key]);
    // A line break in the saved template (one written in R code) shows as the `\n` R reads
    // as a break: a one-line field would drop it, and the row would load as edited.
    const before = ctx.value == null ? '' : String(ctx.value).replace(/\r\n|\r|\n/g, '\\n');
    const inp = document.createElement('input');
    inp.type = 'text';
    inp.className = 'blockr-text-input vz-text vz-template';
    inp.value = before;
    inp.spellcheck = false;
    inp.setAttribute('aria-label', ctx.label);
    inp.setAttribute('data-gear-input', ctx.key);
    const ph = ctx.host.placeholder && ctx.host.placeholder(ctx.key, ctx.values);
    if (ph) inp.placeholder = ph;
    const shows = document.createElement('div');
    shows.className = 'vz-template-shows';
    const turn = document.createElement('span');
    turn.className = 'vz-template-turn';
    turn.setAttribute('aria-hidden', 'true');
    turn.textContent = '↳';
    const body = document.createElement('span');
    body.className = 'vz-template-preview';
    paintPreview(body, band, ctx.labelOf, ctx.wordOf);
    // Plain text with no slots, clause or break: the line would repeat it.
    shows.classList.toggle('is-same', before !== '' && !/[{[]|\\n/.test(before));
    const stale = document.createElement('span');
    stale.className = 'vz-template-stale';
    stale.textContent = 'not applied';
    shows.append(turn, body, stale);
    const warn = document.createElement('div');
    warn.className = 'vz-template-warn';
    const note = document.createElement('div');
    note.className = 'vz-template-script';
    const help = document.createElement('div');
    help.className = 'vz-help';
    const at = document.createElement('code');
    at.className = 'vz-at-key';
    at.textContent = '@';
    help.append('Type ', at, ' for a setting or a value');
    el.append(inp, shows, warn, note, help);

    const menu = atMenu(inp, ctx);
    const sync = () => {
      (el.closest('.vz-row') || el).classList.toggle('vz-row--dirty', inp.value !== before);
      const bad = unknownTokens(inp.value, ctx.titles, ctx.columns);
      warn.hidden = !bad.length;
      warn.textContent = bad.length
        ? 'Nothing is called ' + bad.join(', ') + '. It prints nothing, and takes its [ ] clause with it.'
        : '';
      note.textContent = ctx.shadowNote ? ctx.shadowNote(inp.value) : '';
      note.hidden = !note.textContent;
    };
    const commit = () => {
      const v = inp.value.trim() === '' ? null : inp.value;
      // Unchanged against the field as it loaded, so a template with a line break is not
      // sent back as an edit when the field is left.
      if ((v ?? '') !== before) ctx.set(v);
    };
    ctx.scope.on(inp, 'input', (e) => {
      if (/** @type {InputEvent} */ (e).data === '@') menu.open();
      else menu.refilter();
      sync();
    });
    const revert = () => { inp.value = before; sync(); inp.blur(); };
    ctx.scope.on(inp, 'keydown', (ev) => {
      const e = /** @type {KeyboardEvent} */ (ev);
      if (menu.keydown(e)) return;
      if (e.key === 'Enter') { e.preventDefault(); commit(); }
      if (e.key === 'Escape') revert();
    });
    // Blockr.layer takes Escape on window, before the field's own keydown,
    // and gives it to the top layer: the tray, which closes, and the blur
    // then commits the draft. While it has the focus the field is a layer
    // of its own, so Escape reverts it. The `@` menu opens above it and
    // takes the first Escape.
    /** @type {{ remove(): void } | null} */
    let layer = null;
    const unlayer = () => { if (layer) { layer.remove(); layer = null; } };
    ctx.scope.own({ dispose: unlayer });
    ctx.scope.on(inp, 'focus', () => {
      const ly = ui().layer;
      if (!layer && ly) layer = ly(inp, { escape: () => { layer = null; revert(); } });
    });
    ctx.scope.on(inp, 'blur', () => {
      unlayer();
      menu.close();
      if (inp.value !== before) commit();
    });
    sync();
  }

  /** @param {VizScope} scope @param {HTMLElement} el @param {() => void} fn */
  const wire = (scope, el, fn) => {
    el.setAttribute('role', 'button');
    el.setAttribute('tabindex', '0');
    scope.on(el, 'click', (e) => { e.stopPropagation(); fn(); });
    scope.on(el, 'keydown', (ev) => {
      const e = /** @type {KeyboardEvent} */ (ev);
      if (e.key !== 'Enter' && e.key !== ' ') return;
      e.preventDefault();
      fn();
    });
  };

  /** Type a value over its word: Enter or leaving commits, Escape puts the
   * word back. A number is held in its range and steps on the arrow keys
   * (by ten with Shift); an emptied number keeps its value.
   * @param {VizScope} scope @param {HTMLElement} anchor
   * @param {VizSlotOpener & { kind: 'number' | 'entry' }} how */
  function edit(scope, anchor, how) {
    const es = scope.own(new V.Scope());
    const num = how.kind === 'number' ? how : null;
    const cur = how.value;
    const before = how.kind === 'number' ? (cur == null ? '' : String(cur)) : how.toText(cur);
    const input = document.createElement('input');
    input.type = num ? 'number' : 'text';
    input.className = 'blockr-slot-input';
    input.value = before;
    input.setAttribute('aria-label', how.label);
    input.size = Math.max(3, before.length + 1);
    /** @param {number} v */
    const fix = (v) => (num ? Math.min(num.max, Math.max(num.min, v)) : v);
    let done = false;
    const finish = (/** @type {boolean} */ commit) => {
      if (done) return;
      done = true;
      const text = input.value.trim();
      es.dispose();
      input.replaceWith(anchor);
      if (!commit || text === before) { anchor.focus(); return; }
      if (num) {
        const v = parseFloat(text);
        if (Number.isFinite(v)) how.set(fix(v));
      } else if (how.kind === 'entry') {
        how.set(how.fromText(text));
      }
    };
    es.on(input, 'click', (e) => e.stopPropagation());
    es.on(input, 'keydown', (ev) => {
      const e = /** @type {KeyboardEvent} */ (ev);
      e.stopPropagation();
      if (num && (e.key === 'ArrowUp' || e.key === 'ArrowDown')) {
        e.preventDefault();
        const v = parseFloat(input.value);
        const by = num.step * (e.shiftKey ? 10 : 1) * (e.key === 'ArrowUp' ? 1 : -1);
        input.value = String(Number(fix((Number.isFinite(v) ? v : 0) + by).toFixed(10)));
      }
      if (e.key === 'Enter') { e.preventDefault(); finish(true); }
      if (e.key === 'Escape') { e.preventDefault(); finish(false); }
    });
    es.on(input, 'blur', () => finish(true));
    anchor.replaceWith(input);
    input.focus();
    input.select();
  }

  /** Fill a menu under a word with its setting's choices; null when the
   * page's blockr.ui has no menu for it.
   * @param {HTMLElement} anchor
   * @param {VizSlotOpener & { kind: 'menu' }} how @param {string | undefined} by
   * @param {() => void} onClose */
  function openMenu(anchor, how, by, onClose) {
    const Sel = ui().Select;
    if (!Sel || !Sel.menu || (how.multi && !Sel.menuMulti)) return null;
    const C = V.gear.controls;
    const { options, keys } = C.uiOptions(how.options);
    const at = (/** @type {unknown} */ v) => keys[how.options.findIndex((o) => C.same(o.value, v))];
    const back = (/** @type {string} */ k) => how.options[keys.indexOf(k)];
    anchor.classList.add('blockr-slot--open');
    return Sel.menu(anchor, {
      options, mode: how.multi ? 'multi' : 'single', title: how.label,
      selected: how.multi ? (/** @type {unknown[]} */ (how.selected || [])).map(at) : at(how.selected),
      labelFirst: by === 'label', searchPlaceholder: 'Filter',
      onChange: (/** @type {string | string[]} */ v) => {
        const val = Array.isArray(v) ? v.map((k) => back(k)).filter(Boolean).map((c) => c.value)
          : (back(v) || { value: how.selected }).value;
        how.set(val);
      },
      onClose: () => {
        anchor.classList.remove('blockr-slot--open');
        onClose();
      }
    });
  }

  /** The settings a sentence would name if they were set, three at most.
   * @param {HTMLElement} el @param {string[]} offers @param {VizScope} ps
   * @param {VizGear} gear @param {(key: string, chip: HTMLElement) => void} openSlot
   * @param {() => void} openGear */
  function paintOffers(el, offers, ps, gear, openSlot, openGear) {
    const MAX = 3;
    const keys = offers.filter((k) => typeof k === 'string');
    const shown = keys.length > MAX ? keys.slice(0, MAX - 1) : keys;
    for (const key of shown) {
      const c = document.createElement('span');
      c.className = 'blockr-slot-offer';
      const how = gear.slot(key);
      c.textContent = '+ ' + (how ? how.label : gear.label(key));
      wire(ps, c, () => openSlot(key, c));
      el.appendChild(c);
    }
    if (keys.length <= MAX) return;
    const more = document.createElement('span');
    more.className = 'blockr-slot-offer blockr-slot-offer--more';
    more.textContent = 'More settings';
    wire(ps, more, openGear);
    el.appendChild(more);
  }

  /** The sentence on the block's face. A word R marked with an argument is
   * a control when the gear has one for it: a flag toggles in place, a
   * number or a list of values is typed over, anything else opens its list
   * (blockr.ui's Blockr.Select.menu). Offers name settings the sentence
   * would show if they were set; past three the last reads "More settings".
   * @param {{ gear: VizGear, scope: VizScope, openGear?: () => void }} opts
   * @returns {VizSlots} */
  function slots(opts) {
    const { gear } = opts;
    const own = opts.scope.own(new V.Scope());
    /** @type {Map<HTMLElement, VizScope>} */
    const painted = new Map();
    /** @type {{ key: string, anchor: HTMLElement, menu: { close(): void } } | null} */
    let open = null;
    const openGear = () => { if (opts.openGear) opts.openGear(); };

    const close = () => {
      const o = open;
      open = null;
      if (!o) return;
      o.anchor.classList.remove('blockr-slot--open');
      o.menu.close();
    };

    /** `given` opens one word of a setting drawn as several (axis ticks).
     * @param {string} key @param {HTMLElement} anchor @param {VizScope} ps
     * @param {string} [by] @param {VizSlotOpener} [given] */
    const openSlot = (key, anchor, ps, by, given) => {
      const was = open && open.key;
      close();
      if (was === key) return;
      const how = given || gear.slot(key);
      if (!how) { openGear(); return; }
      if (how.kind === 'flag') { how.set(!how.value); return; }
      if (how.kind === 'entry' || how.kind === 'number') { edit(ps, anchor, how); return; }
      if (how.kind === 'pair') { edit(ps, anchor, how.ends[0]); return; }
      if (how.kind === 'words') { openSlot(key + '#0', anchor, ps, by, how.words[0].how); return; }
      const menu = openMenu(anchor, how, by, () => {
        if (open && open.anchor === anchor) open = null;
      });
      if (!menu) { openGear(); return; }
      open = { key, anchor, menu };
    };

    /** @param {HTMLElement} el @param {VizTitleBand | null | undefined} band */
    const paint = (el, band) => {
      const old = painted.get(el);
      if (old) old.dispose();
      const ps = new V.Scope();
      painted.set(el, ps);
      el.textContent = '';
      if (!band) return;
      const parts = band.parts || [];
      if (!parts.length && band.text) el.textContent = band.text;
      for (const p of parts) {
        if (!p || !p.text) continue;
        const how = p.arg ? gear.slot(p.arg) : null;
        if (!how) { el.appendChild(document.createTextNode(p.text)); continue; }
        if (how.kind === 'pair') { V.slotPair(el, ps, how.ends); continue; }
        const arg = /** @type {string} */ (p.arg);
        if (how.kind === 'words') { V.slotWords(el, ps, how.words, (i, w) => openSlot(arg + '#' + i, w, ps, p.by, how.words[i].how)); continue; }
        const w = document.createElement('span');
        w.className = 'blockr-slot';
        w.textContent = p.text;
        wire(ps, w, () => openSlot(arg, w, ps, p.by));
        el.appendChild(w);
      }
      paintOffers(el, band.offers || [], ps, gear, (key, c) => openSlot(key, c, ps), openGear);
    };

    own.own({ dispose: () => {
      close();
      for (const ps of painted.values()) ps.dispose();
      painted.clear();
    } });
    return { paint, close, dispose: () => own.dispose() };
  }

  V.titles = { row, menuGroups, unknownTokens, paintPreview, slots, namedArgs, edit, wire };
})();
// --- core/ticks.js
// @ts-check
/**
 * Axis ticks (`x_ticks`, `y_ticks`), read the way R checks them
 * (check_ticks() in R/spec-check.R): one value is a step, a number or a
 * count and a date unit ("7 days"); two or more are the ticks themselves.
 * The gear (gear/ticks.js), the sentence (gear/engine.js) and the chart
 * (chart/ticks.js) all read and write the value through here.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  const UNITS = ['day', 'week', 'month', 'quarter', 'year'];
  /** Months per unit; days and weeks step by a fixed length instead. */
  const MONTHS = /** @type {Record<string, number>} */ ({ month: 1, quarter: 3, year: 12 });
  const DAY_MS = 864e5;
  /** More ticks than this on one axis and the axis draws its own. */
  const MAX_TICKS = 60;
  const LOG_NOTE = 'A log axis takes a list, such as 1, 10, 100.';
  const LIST_NOTE = 'A list needs two values or more.';

  /** @param {number} x */
  const tidy = (x) => Number(x.toPrecision(12));

  /**
   * The value as parts: a step (`n`, and a date `unit` or null), a list, or
   * null for auto.
   * @param {unknown} v
   * @returns {{ form: 'every', n: number, unit: string | null } | { form: 'at', list: (number | string)[] } | null}
   */
  const parse = (v) => {
    if (v == null || v === '') return null;
    if (Array.isArray(v)) {
      const list = v.filter((x) => x != null && x !== '');
      return list.length ? { form: 'at', list: /** @type {(number | string)[]} */ (list) } : null;
    }
    if (typeof v === 'number') return v > 0 ? { form: 'every', n: v, unit: null } : null;
    const m = /^(\d*\.?\d+)?\s*(day|week|month|quarter|year)s?$/i.exec(String(v).trim());
    if (m) return { form: 'every', n: m[1] ? Number(m[1]) : 1, unit: m[2].toLowerCase() };
    const n = Number(v);
    return Number.isFinite(n) && n > 0 ? { form: 'every', n, unit: null } : null;
  };

  /** A step as the value R keeps: a number, or "<n> <unit>s".
   * @param {number} n @param {string | null} unit */
  const step = (n, unit) => (unit ? n + ' ' + unit + (n === 1 ? '' : 's') : n);

  /** A unit as a word after its count: "day", "days". @param {string} unit @param {number} n */
  const unitWord = (unit, n) => unit + (n === 1 ? '' : 's');

  /** The value as the sentence prints it (slot_ticks_text() in R).
   * @param {unknown} v */
  const text = (v) => {
    const p = parse(v);
    if (!p) return 'auto';
    if (p.form === 'at') return 'at ' + p.list.join(', ');
    if (!p.unit) return 'every ' + p.n;
    return 'every ' + (p.n === 1 ? p.unit : p.n + ' ' + unitWord(p.unit, p.n));
  };

  /** A list as typed, "0, 25, 50": numbers where they read as numbers.
   * @param {string} t @returns {(number | string)[]} */
  const parseList = (t) => t.split(/[,;]/).map((x) => x.trim()).filter(Boolean)
    .map((x) => x.replace(/^−/, '-'))
    .map((x) => (Number.isFinite(Number(x)) ? Number(x) : x));

  /** Text typed for the setting back to a value: "auto", "25", "every 7
   * days", "month", "at 0, 25, 50" or "0, 25, 50".
   * @param {string} t @returns {unknown} */
  const fromText = (t) => {
    const s = String(t ?? '').trim().replace(/^every\s+/i, '');
    if (!s || /^auto$/i.test(s)) return null;
    if (/^at\s+/i.test(s) || /[,;]/.test(s)) {
      const list = parseList(s.replace(/^at\s+/i, ''));
      return list.length > 1 ? list : (list.length ? list[0] : null);
    }
    const p = parse(s);
    return p && p.form === 'every' ? step(p.n, p.unit) : null;
  };

  /** Why a value cannot go on its axis, or null: a step on a log axis, a list
   * of one.
   * @param {unknown} v @param {boolean} log @returns {string | null} */
  const note = (v, log) => {
    const p = parse(v);
    if (!p) return null;
    if (p.form === 'every' && log) return LOG_NOTE;
    if (p.form === 'at' && p.list.length < 2) return LIST_NOTE;
    return null;
  };

  /** A tick value as the axis reads it: a date as UTC milliseconds.
   * @param {number | string} v @param {string} type @returns {number | null} */
  const axisValue = (v, type) => {
    let n = typeof v === 'number' ? v : Number(v);
    if (!Number.isFinite(n) && type === 'time') n = Date.parse(String(v));
    if (!Number.isFinite(n)) return null;
    if (type === 'log' && n <= 0) return null;
    return n;
  };

  /** The first day of the month at or after `ms`, in UTC. @param {number} ms */
  const monthStart = (ms) => {
    const d = new Date(ms);
    const at = Date.UTC(d.getUTCFullYear(), d.getUTCMonth(), 1);
    return at < ms ? Date.UTC(d.getUTCFullYear(), d.getUTCMonth() + 1, 1) : at;
  };

  /**
   * The tick values over [lo, hi] on an axis of `type` ('value', 'time' or
   * 'log'), or null when the axis should draw its own: unset, a step on a
   * log axis, fewer than two ticks in view, or more than MAX_TICKS.
   * Numbers step from the multiples of the step; days and weeks from `lo`;
   * months, quarters and years from the first of a month.
   * @param {unknown} v @param {number} lo @param {number} hi @param {string} type
   * @returns {number[] | null}
   */
  const values = (v, lo, hi, type) => {
    const p = parse(v);
    if (!p || !(hi > lo)) return null;
    /** @type {number[]} */
    let out = [];
    if (p.form === 'at') {
      out = /** @type {number[]} */ (p.list.map((x) => axisValue(x, type))
        .filter((x) => x != null && x >= lo && x <= hi));
    } else if (type === 'log') {
      return null;
    } else if (type === 'time' && p.unit && MONTHS[p.unit]) {
      const by = p.n * MONTHS[p.unit];
      const d0 = new Date(monthStart(lo));
      // Quarters and years start in January, April, July and October.
      const shift = p.unit === 'month' ? 0 : (12 - (d0.getUTCMonth() % MONTHS[p.unit])) % MONTHS[p.unit];
      for (let k = shift; out.length <= MAX_TICKS; k += by) {
        const t = Date.UTC(d0.getUTCFullYear(), d0.getUTCMonth() + k, 1);
        if (t > hi) break;
        out.push(t);
      }
    } else if (type === 'time') {
      const by = p.n * (p.unit === 'week' ? 7 : 1) * DAY_MS;
      for (let t = lo; t <= hi && out.length <= MAX_TICKS; t += by) out.push(t);
    } else {
      const s = p.n;
      if ((hi - lo) / s > MAX_TICKS) return null;
      for (let k = Math.ceil(tidy(lo / s)); tidy(k * s) <= hi; k++) out.push(tidy(k * s));
    }
    return out.length >= 2 && out.length <= MAX_TICKS ? out : null;
  };

  /** A step's tidy ends around [lo, hi] on a value axis, the auto ends of an
   * axis with a step; null for a list, a date axis or no step.
   * @param {unknown} v @param {number} lo @param {number} hi @returns {[number, number] | null} */
  const roundOut = (v, lo, hi) => {
    const p = parse(v);
    if (!p || p.form !== 'every' || p.unit || !Number.isFinite(lo) || !Number.isFinite(hi)) return null;
    const s = p.n;
    return [tidy(Math.floor(tidy(lo / s)) * s), tidy(Math.ceil(tidy(hi / s)) * s)];
  };

  /** The value a form starts with when picked in the gear or the sentence,
   * from the ticks drawn now: their step, or their list.
   * @param {'every' | 'at'} form @param {number[]} drawn @param {string} type */
  const start = (form, drawn, type) => {
    const ok = (drawn || []).filter(Number.isFinite);
    if (form === 'at') {
      const list = ok.length >= 2 ? ok : [0, 1];
      return type === 'time' ? list.map((t) => new Date(t).toISOString().slice(0, 10)) : list;
    }
    const d = ok.length >= 2 ? tidy(ok[1] - ok[0]) : 0;
    if (type === 'time') {
      const days = Math.max(1, Math.round(d / DAY_MS));
      if (days >= 360) return step(Math.max(1, Math.round(days / 365)), 'year');
      if (days >= 28) return step(Math.max(1, Math.round(days / 30)), 'month');
      return days % 7 === 0 ? step(days / 7, 'week') : step(days, 'day');
    }
    return d > 0 ? d : 1;
  };

  V.ticks = { UNITS, MAX_TICKS, LOG_NOTE, LIST_NOTE, parse, step, unitWord, text, fromText,
              parseList, note, values, roundOut, start, axisValue };
})();
// --- core/slot-pair.js
// @ts-check
/**
 * A pair setting on the block's face (axis limits): R prints it as one
 * phrase, "20 to auto"; the face draws each end as its own word, typed over
 * like a value (titles.js). The other end stays as it is. A setting of
 * several parts (axis ticks, "every 7 days") draws a word per part, each
 * opening its own control (slotWords).
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /** @param {HTMLElement} el @param {VizScope} ps @param {VizEndOpener[]} ends */
  V.slotPair = (el, ps, ends) => ends.forEach((end, i) => {
    if (i) el.appendChild(document.createTextNode(' to '));
    const w = el.appendChild(document.createElement('span'));
    w.className = 'blockr-slot';
    w.textContent = end.value == null ? 'auto' : String(end.value);
    V.titles.wire(ps, w, () => V.titles.edit(ps, w, end));
  });

  /** @param {HTMLElement} el @param {VizScope} ps @param {VizWordPart[]} words
   * @param {(i: number, w: HTMLElement) => void} open */
  V.slotWords = (el, ps, words, open) => words.forEach((wd, i) => {
    if (i) el.appendChild(document.createTextNode(' '));
    const w = el.appendChild(document.createElement('span'));
    w.className = 'blockr-slot';
    w.textContent = wd.text;
    V.titles.wire(ps, w, () => open(i, w));
  });
})();
// --- core/gear/tray-nav.js
// @ts-check
/**
 * Blockr.trayNav, the groups of a long gear tray, until blockr.ui ships it.
 * Copied from blockr.ui branch feat/tray-nav (708ef52); the CSS is at the end
 * of inst/css/gear.css. Delete both once a blockr.ui release has trayNav: a
 * blockr.ui that has one keeps its own, as this only fills the gap.
 *
 * The tray's sections are sorted into a few groups and one group shows at a
 * time. A tray 560px and wider gets an index on the left: the groups, and
 * under the open one its sections. A narrower tray gets the groups as tabs
 * above the sections. The sections are moved into the navigation's pane. A
 * group with one section hides that section's title. A click on a section in
 * the index brings it into view and marks it briefly.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {{ trayNav?: unknown }} */ (root.Blockr = root.Blockr || {});

  let n = 0;

  /** @param {string} cls @param {string} [tag] */
  function make(cls, tag) {
    const el = document.createElement(tag || 'div');
    el.className = cls;
    return el;
  }

  /** @param {string} cls @param {string} text @param {() => void} onClick */
  function button(cls, text, onClick) {
    const b = /** @type {HTMLButtonElement} */ (make(cls, 'button'));
    b.type = 'button';
    b.textContent = text;
    b.addEventListener('click', onClick);
    return b;
  }

  /** @param {HTMLElement} el */
  function scrollTo(el) {
    if (typeof el.scrollIntoView !== 'function') return;
    const still = typeof window.matchMedia === 'function' &&
      window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    el.scrollIntoView({ block: 'start', behavior: still ? 'auto' : 'smooth' });
  }

  // A section picked in the index moves to the top of the view, as far as
  // the page can scroll, and lights up for a moment, so the eye finds it
  // even where it could not move.
  /** @param {HTMLElement} sec */
  function point(sec) {
    scrollTo(sec);
    sec.classList.remove('blockr-tray-nav__flash');
    void sec.offsetWidth;   // restart the animation
    sec.classList.add('blockr-tray-nav__flash');
    // Off once played, or the group shown again would replay it.
    sec.addEventListener('animationend', function () {
      sec.classList.remove('blockr-tray-nav__flash');
    }, { once: true });
  }

  /**
   * @typedef {{ el: HTMLElement, title: string, titleEl?: HTMLElement | null }} NavSection
   * @typedef {{ key: string, title: string, sections: NavSection[] }} NavGroup
   */

  /**
   * @param {NavGroup[]} groups
   * @param {{ current?: string, onPick?: (key: string) => void, label?: string }} [opts]
   */
  function trayNav(groups, opts) {
    const o = opts || {};
    const list = groups.filter((g) => g.sections.length);
    let current = list.some((g) => g.key === o.current)
      ? /** @type {string} */ (o.current) : (list[0] ? list[0].key : '');
    const id = 'blockr-tray-nav-' + (++n);
    const label = o.label || 'Settings groups';

    const el = make('blockr-tray-nav');
    const index = make('blockr-tray-nav__index', 'nav');
    index.setAttribute('aria-label', label);
    const body = make('blockr-tray-nav__body');
    const tabs = make('blockr-tray-nav__tabs');
    tabs.setAttribute('role', 'tablist');
    tabs.setAttribute('aria-label', label);
    const pane = make('blockr-tray-nav__pane');
    pane.id = id + '-pane';
    pane.setAttribute('role', 'tabpanel');
    body.append(tabs, pane);
    el.append(index, body);

    /** @type {Record<string, HTMLButtonElement>} */
    const tabBtns = {};
    for (const g of list) {
      const one = g.sections.length === 1;
      for (const s of g.sections) {
        s.el.dataset.trayGroup = g.key;
        if (one && s.titleEl) s.titleEl.classList.add('blockr-tray-nav__hidden-title');
        pane.appendChild(s.el);
      }
      const t = button('blockr-tray-nav__tab', g.title, () => pick(g.key));
      t.id = id + '-tab-' + g.key;
      t.setAttribute('role', 'tab');
      t.setAttribute('aria-controls', pane.id);
      tabs.appendChild(t);
      tabBtns[g.key] = t;
    }

    // Arrow keys move between the tabs, as in any tab list; the index is a
    // list of buttons and takes Tab.
    tabs.addEventListener('keydown', (e) => {
      const keys = list.map((g) => g.key);
      const i = keys.indexOf(current);
      let next = e.key === 'ArrowRight' ? i + 1 : e.key === 'ArrowLeft' ? i - 1 :
        e.key === 'Home' ? 0 : e.key === 'End' ? keys.length - 1 : null;
      if (next === null) return;
      e.preventDefault();
      next = (next + keys.length) % keys.length;
      pick(keys[next]);
      tabBtns[keys[next]].focus();
    });

    /** @param {string} key */
    function pick(key) {
      if (key === current) return;
      current = key;
      draw();
      // A group opens at its top: when the pane's top has scrolled out of
      // view, it comes back.
      if (pane.getBoundingClientRect().top < 0) scrollTo(pane);
      if (o.onPick) o.onPick(key);
    }

    function draw() {
      index.innerHTML = '';
      for (const g of list) {
        const on = g.key === current;
        const b = button('blockr-tray-nav__group' + (on ? ' is-current' : ''), g.title,
          () => pick(g.key));
        if (on) b.setAttribute('aria-current', 'true');
        index.appendChild(b);
        if (on && g.sections.length > 1) {
          for (const s of g.sections) {
            if (s.title) index.appendChild(button('blockr-tray-nav__section', s.title, () => point(s.el)));
          }
        }
        tabBtns[g.key].classList.toggle('is-current', on);
        tabBtns[g.key].setAttribute('aria-selected', on ? 'true' : 'false');
        tabBtns[g.key].tabIndex = on ? 0 : -1;
        for (const s of g.sections) s.el.classList.toggle('blockr-tray-nav__off', !on);
      }
      pane.setAttribute('aria-labelledby', id + '-tab-' + current);
    }
    draw();

    return { el, show: (/** @type {string} */ key) => pick(key), current: () => current };
  }

  B.trayNav = B.trayNav || trayNav;
})();
// --- core/gear/sections.js
// @ts-check
/**
 * The gear's sections: which spec arguments go into which section, the
 * order of the sections, and how they are laid out in the tray.
 *
 * A section is a spec argument's `gear.group`. Sections fall into the tray's
 * groups (Data, Sort, Format, Titles, Interaction), so a user meets the same
 * groups in a chart and in a table. blockr.ui's `Blockr.trayNav` shows one
 * group at a time.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  V.gear = V.gear || /** @type {VizGearNamespace} */ ({});

  /** @type {[string, string][]} */
  const GROUPS = [
    ['data', 'Data'], ['sort', 'Sort'], ['format', 'Format'],
    ['titles', 'Titles'], ['interact', 'Interaction']
  ];

  /** Each section's tray group and its place there. A section not listed
   * goes to Format, after the listed ones, in spec order. */
  /** @type {Record<string, [string, number]>} */
  const SECTIONS = {
    'Chart type': ['data', 0], 'Table': ['data', 0], 'Columns': ['data', 10],
    'Variables': ['data', 15], 'Mapping': ['data', 20], 'Prepare script': ['data', 90],
    'Order': ['sort', 0], 'Rows shown': ['sort', 10],
    'Look': ['format', 0], 'Numbers': ['format', 10], 'Bars': ['format', 20],
    'Marks': ['format', 30], 'Reference lines': ['format', 40],
    'Titles': ['titles', 0],
    'Reader tools': ['interact', 0], 'Drill-down': ['interact', 10]
  };

  /** Optional mappings: shown in Mapping once set, else offered in its Add
   * menu. */
  const ADD = 'Add mapping';

  /**
   * Does the argument mean something for this chart type or table layout?
   * @param {VizArg} arg @param {unknown} type
   */
  const applies = (arg, type) => {
    const a = arg.applies;
    if (!a || type == null) return true;
    const t = String(type);
    if (a.only) return a.only.includes(t);
    if (a.except) return !a.except.includes(t);
    return true;
  };

  /**
   * The sections to draw, in tray order.
   * @param {VizSpec} spec
   * @param {(key: string) => boolean} shown Whether a row is visible now.
   * @param {(key: string) => boolean} isSet Whether an optional mapping is
   *   shown as a row (set, or added in this tray) rather than in the menu.
   * @param {VizProfile | null} [own] A block's own gear: its sections and
   *   tabs in place of the table's.
   * @returns {VizSectionPlan[]}
   */
  function plan(spec, shown, isSet, own) {
    /** @type {Map<string, VizSectionPlan>} */
    const out = new Map();
    let extra = 100;
    const where = own ? own.sections : SECTIONS;
    const section = (/** @type {string} */ title) => {
      let s = out.get(title);
      if (!s) {
        const [group, ord] = where[title] || ['format', extra++];
        s = { title, group, ord, keys: [], adds: [] };
        out.set(title, s);
      }
      return s;
    };
    for (const [key, arg] of Object.entries(spec)) {
      const g = arg.gear;
      if (!g || !shown(key)) continue;
      if (g.group === ADD) {
        const s = section('Mapping');
        if (isSet(key)) s.keys.push(key);
        else s.adds.push(key);
      } else {
        section(g.group).keys.push(key);
      }
    }
    // A two-cell text field goes last in its section, so it leaves no hole
    // in the row before it.
    const late = (/** @type {string} */ k) => spec[k].type.name === 'named';
    for (const s of out.values()) {
      s.keys = s.keys.filter((k) => !late(k)).concat(s.keys.filter(late));
    }
    const order = (own ? own.groups : GROUPS).map((x) => x[0]);
    return Array.from(out.values())
      .filter((s) => s.keys.length || s.adds.length)
      .sort((a, b) => order.indexOf(a.group) - order.indexOf(b.group) || a.ord - b.ord);
  }

  /**
   * An empty section: its title, then its rows as the section's own
   * children, which the stylesheet lays out in a grid (gear.css,
   * `.vz-section`). `body` is the section itself.
   * @param {string} title
   */
  function sectionEl(title) {
    const el = document.createElement('div');
    el.className = 'vz-section' + (title === 'Titles' ? ' vz-section--titles' : '');
    el.setAttribute('data-section', title);
    const titleEl = document.createElement('div');
    titleEl.className = 'vz-section-title';
    titleEl.textContent = title;
    el.appendChild(titleEl);
    return { el, titleEl, body: el };
  }

  /**
   * Put the built sections into the tray, grouped through blockr.ui's
   * Blockr.trayNav. The index lists the groups only: a section goes in
   * without a title, which trayNav leaves out of the index. Its header in
   * the pane stays.
   * @param {HTMLElement} pop
   * @param {{ plan: VizSectionPlan, el: HTMLElement, titleEl: HTMLElement }[]} built
   * @param {string | null} current The group to show.
   * @param {(g: string) => void} onPick
   * @param {string} label
   * @param {[string, string][]} [tabs] A block's own tabs, in place of GROUPS.
   */
  function assemble(pop, built, current, onPick, label, tabs) {
    const ui = /** @type {VizUi} */ (root.Blockr);
    const groups = (tabs || GROUPS).map(([key, title]) => ({
      key, title,
      sections: built.filter((b) => b.plan.group === key)
        .map((b) => ({ el: b.el, title: '', titleEl: b.titleEl }))
    }));
    if (typeof ui.trayNav !== 'function') {
      pop.appendChild(plainList(groups));
      return;
    }
    const nav = ui.trayNav(groups, { current: current || undefined, onPick, label });
    pop.appendChild(nav.el);
  }

  let warned = false;

  /**
   * The tray without blockr.ui's trayNav (an older blockr.ui): every group
   * under its own heading, one after the other. Said once in the console.
   * @param {{ key: string, title: string, sections: { el: HTMLElement }[] }[]} groups
   */
  function plainList(groups) {
    if (!warned) {
      warned = true;
      console.warn('blockr.viz: this blockr.ui has no Blockr.trayNav; the gear ' +
        'shows its groups as a plain list. Update blockr.ui for the index.');
    }
    const list = document.createElement('div');
    list.className = 'vz-tray-groups';
    for (const g of groups) {
      if (!g.sections.length) continue;
      const head = document.createElement('div');
      head.className = 'vz-tray-group-title';
      head.textContent = g.title;
      list.appendChild(head);
      for (const s of g.sections) list.appendChild(s.el);
    }
    return list;
  }

  V.gear.sections = { GROUPS, plan, sectionEl, assemble, applies };
})();
// --- core/gear/tiles.js
// @ts-check
/**
 * Picker tiles: a drawing over its name, in a grid, one of them chosen. The
 * chart type, the table layout and look, a summary column's mark and a
 * shading's mode are picked this way. A click on the chosen tile does
 * nothing; the arrow keys move and pick, skipping a disabled tile.
 *
 * The drawings are keyed `<argument>:<value>` (`type:bar`, `look:cards`) for
 * the spec's tiles controls; a summary column's marks bring their own.
 * One language for every tile: a 48 x 32 box, marks inside a 4px inset, in
 * currentColor so the tile's state colours it.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  V.gear = V.gear || /** @type {VizGearNamespace} */ ({});

  /**
   * @param {HTMLElement} parent
   * @param {VizTileOption[]} options
   * @param {string} current
   * @param {(v: string) => void} onPick
   * @param {VizScope} scope
   */
  function build(parent, options, current, onPick, scope) {
    const row = document.createElement('div');
    row.className = 'vz-tiles';
    row.setAttribute('role', 'radiogroup');
    /** @type {HTMLButtonElement[]} */
    const btns = [];
    const select = (/** @type {number} */ i) => btns.forEach((b, j) => {
      b.classList.toggle('vz-tile--on', j === i);
      b.setAttribute('aria-checked', j === i ? 'true' : 'false');
      b.tabIndex = j === i ? 0 : -1;
    });
    options.forEach((o, i) => {
      const b = document.createElement('button');
      b.type = 'button';
      b.className = 'vz-tile';
      b.setAttribute('role', 'radio');
      b.setAttribute('data-value', o.value);
      const face = document.createElement('span');
      face.className = 'vz-tile-face';
      face.innerHTML = o.icon;
      const name = document.createElement('span');
      name.className = 'vz-tile-name';
      name.textContent = o.label;
      b.append(face, name);
      if (o.disabled) {
        b.setAttribute('aria-disabled', 'true');
        b.classList.add('is-disabled');
        if (o.title) b.setAttribute('data-blockr-tooltip', o.title);
      }
      scope.on(b, 'click', () => {
        if (o.disabled || b.classList.contains('vz-tile--on')) return;
        select(i);
        onPick(o.value);
      });
      scope.on(b, 'keydown', (e) => {
        const k = /** @type {KeyboardEvent} */ (e).key;
        const step = k === 'ArrowRight' || k === 'ArrowDown' ? 1
          : k === 'ArrowLeft' || k === 'ArrowUp' ? -1 : 0;
        if (!step) return;
        e.preventDefault();
        let j = i;
        do { j = (j + step + btns.length) % btns.length; } while (j !== i && options[j].disabled);
        btns[j].focus();
        btns[j].click();
      });
      btns.push(b);
      row.appendChild(b);
    });
    // A group without the current value has no pick; its first tile still
    // takes the keyboard.
    const cur = options.findIndex((o) => o.value === current);
    select(cur);
    if (cur < 0 && btns[0]) btns[0].tabIndex = 0;
    parent.appendChild(row);
    return row;
  }

  // Inks: solid marks; words and sub-marks at half; areas, boxes and the
  // table header band as a 20% tint; row rules and grids as a faint line.
  /** @type {Record<string, string>} */
  const INK = {
    m: 'fill="currentColor"',
    w: 'fill="currentColor" fill-opacity=".5"',
    t: 'fill="currentColor" fill-opacity=".2"',
    ln: 'fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"',
    sub: 'fill="none" stroke="currentColor" stroke-opacity=".5"',
    rule: 'fill="none" stroke="currentColor" stroke-opacity=".2"'
  };
  /** @param {number} x @param {number} y @param {number} w @param {number} h @param {string} k @param {number} [rx] */
  const R = (x, y, w, h, k, rx) => '<rect x="' + x + '" y="' + y + '" width="' + w +
    '" height="' + h + '" rx="' + (rx == null ? 1 : rx) + '" ' + INK[k] + '/>';
  /** @param {number} x @param {number} y @param {number} r @param {string} [k] */
  const C = (x, y, r, k) => '<circle cx="' + x + '" cy="' + y + '" r="' + r + '" ' + INK[k || 'm'] + '/>';
  /** @param {string} d @param {string} k */
  const P = (d, k) => '<path d="' + d + '" ' + INK[k] + '/>';
  /** A bar on the baseline: rounded top, square foot.
   * @param {number} x @param {number} y @param {number} w @param {number} h */
  const B = (x, y, w, h) => P('M' + x + ' ' + (y + h) + 'V' + (y + 1) + 'a1 1 0 0 1 1-1h' +
    (w - 2) + 'a1 1 0 0 1 1 1V' + (y + h) + 'Z', 'm');
  const BASE = P('M4 28.5H44', 'sub');
  /** @param {number} [h] */
  const HEAD = (h) => R(4, 4, 40, h || 5, 't', 1.5);
  /** @param {number[]} xs @param {number[]} ws @param {number} y @param {string} k */
  const words = (xs, ws, y, k) => ws.map((w, i) => R(xs[i], y, w, 2, k)).join('');
  const C4 = [7, 18, 28, 37];
  /** @param {[number, number[]][]} rows */
  const lines = (rows) => rows.map(([y, ws]) => words(C4, ws, y, 'w')).join('');
  /** A boxplot: whiskers, box, median.
   * @param {number[][]} list */
  const boxes = (list) => list.map(([x, w0, q1, q3, w1, md]) =>
    P('M' + x + ' ' + w0 + 'V' + q1 + 'M' + x + ' ' + q3 + 'V' + w1, 'sub') +
    R(x - 5, q1, 10, q3 - q1, 't', 0) + R(x - 5, q1, 10, q3 - q1, 'ln', 0) +
    P('M' + (x - 5) + ' ' + md + 'H' + (x + 5), 'ln')).join('');
  /** @type {Record<string, string>} */
  const D = {
    'type:bar': B(7, 14, 8, 14) + B(20, 6, 8, 22) + B(33, 18, 8, 10) + BASE,
    'type:histogram': [[8, 7], [14, 14], [20, 22], [26, 17], [32, 10], [38, 4]]
      .map(([x, h]) => R(x, 28 - h, 5, h, 'm', 0)).join('') + BASE,
    'type:waterfall': P('M13 14.5H16M23 8.5H26M33 16.5H36', 'sub') + B(6, 14, 7, 14) +
      R(16, 8, 7, 6, 'm') + R(26, 8, 7, 9, 'w') + B(36, 17, 7, 11) + BASE,
    'type:pie': C(24, 16, 12, 't') + C(24, 16, 12, 'ln') + P('M24 16V4A12 12 0 0 1 34.39 22Z', 'm'),
    'type:treemap': R(6, 4, 18, 24, 'm') + R(26, 4, 16, 13, 'm') + R(26, 19, 7, 9, 'm') +
      R(35, 19, 7, 9, 'm'),
    'type:boxplot': boxes([[15, 6, 11, 21, 26, 16], [33, 10, 14, 23, 27, 18]]),
    'type:pointrange': P('M12 10V26M24 6V20M36 9V22', 'ln') + C(12, 18, 2.5) + C(24, 12, 2.5) +
      C(36, 15, 2.5),
    'type:scatter': [[8, 24], [14, 20], [22, 23], [30, 18], [38, 20], [11, 15], [19, 12],
      [27, 9], [34, 11], [41, 5]].map(([x, y]) => C(x, y, 2)).join(''),
    'type:line': P('M4 24L12 18L20 21L28 11L36 14L44 6', 'ln'),
    'type:area': P('M4 28V20L12 15L20 18L28 9L36 12L44 6V28Z', 't') +
      P('M4 20L12 15L20 18L28 9L36 12L44 6', 'ln') + BASE,
    'type:band': P('M4 14L12 10L20 13L28 5L36 8L44 4V16L36 20L28 17L20 25L12 22L4 26Z', 't') +
      P('M4 20L12 16L20 19L28 11L36 14L44 10', 'ln'),
    'type:gantt': R(6, 5, 18, 5, 'm') + R(14, 13.5, 20, 5, 'm') + R(24, 22, 18, 5, 'm'),

    'layout:records': HEAD() + words(C4, [7, 6, 6, 5], 5.5, 'm') +
      lines([[12, [8, 5, 6, 4]], [16.5, [7, 6, 5, 5]], [21, [9, 4, 6, 4]], [25.5, [6, 5, 6, 5]]]),
    'layout:groups': HEAD() + words([7, 20, 34], [8, 6, 6], 5.5, 'm') +
      [[13.5, 8, 12, 0], [19.5, 9, 7, 1], [25.5, 7, 9, -1]].map(([c, lw, bw, o]) =>
        R(7, c - 1, lw, 2, 'w') + R(20, c - 1.5, bw, 3, 'm') +
        P('M' + (34 + o) + ' ' + c + 'H' + (43 + o), 'sub') +
        R(36.5 + o, c - 2, 4, 4, 't', 0) + R(36.5 + o, c - 2, 4, 4, 'sub', 0)).join(''),
    'layout:variables': HEAD() + words([24, 32, 40], [4, 4, 4], 5.5, 'm') +
      [[11, 12, [14, 17]], [20.5, 9, [23.5, 26.5]]].map(([y, nw, ss]) =>
        R(7, /** @type {number} */ (y), /** @type {number} */ (nw), 2, 'm') +
        /** @type {number[]} */ (ss).map((sy, i) => R(10, sy, i ? 5 : 7, 2, 'w') +
          words([24, 32, 40], [4, 4, 4], sy, 'w')).join('')).join(''),

    'look:table': HEAD() + words(C4, [7, 6, 6, 5], 5.5, 'm') + P('M4 16H44M4 22H44', 'rule') +
      lines([[12.5, [8, 5, 6, 4]], [18.5, [7, 6, 5, 5]], [24.5, [9, 4, 6, 4]]]),
    'look:compact': HEAD(4) + words([6, 15, 24, 33], [7, 6, 6, 5], 5, 'm') +
      [10, 13, 16, 19, 22, 25].map((y, j) =>
        words([6, 15, 24, 33], [8, 5, 6, 4].map((w) => w - (j % 2)), y, 'w')).join(''),
    'look:cards': [[4.5, 4.5], [25.5, 4.5], [4.5, 17], [25.5, 17]].map(([x, y], i) =>
      R(x, y, 18, 10.5, 'sub', 2) + R(x + 3, y + 2.5, [8, 6, 9, 7][i], 2, 'w') +
      R(x + 3, y + 6, [10, 8, 11, 9][i], 2.5, 'm')).join('')
  };

  /** @type {Record<string, string>} */
  const icons = {};
  for (const k of Object.keys(D)) {
    icons[k] = '<svg viewBox="0 0 48 32" width="48" height="32" aria-hidden="true">' + D[k] + '</svg>';
  }

  V.gear.tiles = { build };
  V.gear.icons = icons;
})();
// --- core/gear/icons.js
// @ts-check
/**
 * Small icons for the gear's choices, keyed `<argument>:<value>` as the
 * tiles are (`position:dodge`, `show:box`). A segmented control whose spec
 * entry says `icons` draws them beside its words; the table's column types
 * (`summary:<type>`) head the Add menu and each column.
 *
 * One language with the tiles (tiles.js): one colour (currentColor), solid
 * marks, a second series at 45%, areas at 20%, 1.5px round lines that keep
 * their width at any size, guides at 1px and 35%. Choices are drawn on a
 * 16 grid and shown at 16px; column types on a 24 grid, shown at 20px.
 * Marks keep 1.5 from the edge, and table marks lie as they do in a row.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  V.gear = V.gear || /** @type {VizGearNamespace} */ ({});

  /** @typedef {[string, ...(string | number)[]]} Prim */

  // Inks: m a mark, s a second series or word, t an area, o a hollow
  // outline, k a data line, ks a second line, k1 a thin tick, g a guide; a
  // number is a swatch at that opacity.
  const LINE = 'fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" ' +
    'vector-effect="non-scaling-stroke"';
  /** @type {Record<string, string>} */
  const INK = {
    m: 'fill="currentColor"',
    s: 'fill="currentColor" fill-opacity=".45"',
    t: 'fill="currentColor" fill-opacity=".2"',
    o: LINE + ' stroke-width="1.5"',
    k: LINE + ' stroke-width="1.5"',
    ks: LINE + ' stroke-width="1.5" stroke-opacity=".5"',
    k1: LINE + ' stroke-width="1"',
    g: LINE + ' stroke-width="1" stroke-opacity=".35"'
  };
  /** @param {string | number} k */
  const ink = (k) => (typeof k === 'number' ? 'fill="currentColor" fill-opacity="' + k + '"' : INK[k]);

  /** @param {Prim} p @param {number} n The grid. */
  function prim(p, n) {
    if (p[0] === 'p') return '<path d="' + p[1] + '" ' + ink(p[2]) + '/>';
    if (p[0] === 'c') return '<circle cx="' + p[1] + '" cy="' + p[2] + '" r="' + p[3] + '" ' + ink(p[4]) + '/>';
    let [, x, y, w, h, k, rx] = /** @type {[string, number, number, number, number, string | number, number?]} */ (p);
    // A box's outline sits inside the box, as the tint does.
    if (k === 'ob') { x += 0.75; y += 0.75; w -= 1.5; h -= 1.5; k = 'o'; }
    if (rx == null) rx = n === 24 ? 1.5 : 1;
    return '<rect x="' + x + '" y="' + y + '" width="' + w + '" height="' + h + '" rx="' + rx + '" ' +
      ink(k) + '/>';
  }

  /** Facet layout: one summary as solid squares, the other as half-ink dots,
   * under two headers. By summary AA | BB, by level AB | AB.
   * @param {string} order @returns {Prim[]} */
  function facet(order) {
    const xs = [2.9, 6.1, 9.9, 13.1];
    /** @type {Prim[]} */
    const out = [['p', 'M1.5 4.5H6.8M9.2 4.5H14.5', 'k']];
    order.split('').forEach((t, i) => out.push(t === 'A'
      ? ['r', xs[i] - 1.5, 8.5, 3, 3, 'm', 0.6]
      : ['c', xs[i], 10, 1.6, 's']));
    return out;
  }
  /** Data points on a line shape: where the riser sits against them tells
   * the steps apart. @param {string} d @returns {Prim[]} */
  const line = (d) => [['p', d, 'k'],
    ...[[2, 12], [6, 6], [10, 9], [14, 4]].map(([x, y]) => /** @type {Prim} */ (['c', x, y, 1.5, 'm']))];
  /** Scatter points under a smoother. @param {Prim[]} fit @returns {Prim[]} */
  const scatter = (fit) => [[3, 11.5], [5.5, 8.5], [7.5, 12], [9.5, 7], [12, 4.5], [13, 9.5]]
    .map(([x, y]) => /** @type {Prim} */ (['c', x, y, 1.4, fit.length ? 's' : 'm'])).concat(fit);
  /** @param {number[]} ops @returns {Prim[]} */
  const ramp = (ops) => ops.map((o, i) => /** @type {Prim} */ (['r', 1.5 + i * 2.6, 4.5, 2.6, 7, o, 0]));
  /** @type {Record<string, Prim[]>} */
  const SCALE = {
    identity: [['p', 'M3.5 1.5V14.5H14.5', 'g'], ['p', 'M1.5 3H3.5M1.5 6.5H3.5M1.5 10H3.5M1.5 13.5H3.5', 'k1'],
      ['p', 'M5.5 12.5L14 3', 'k']],
    log10: [['p', 'M3.5 1.5V14.5H14.5', 'g'], ['p', 'M1.5 2.5H3.5M1.5 4.5H3.5M1.5 7.5H3.5M1.5 12H3.5', 'k1'],
      ['p', 'M5.5 12.5C7 6.5 10 4 14 3', 'k']]
  };
  /** @type {Record<string, Prim[]>} */
  const SORT = {
    asc: [['r', 2, 2.5, 2.5, 2.6, 'm'], ['r', 2, 6.7, 4.5, 2.6, 'm'], ['r', 2, 10.9, 7, 2.6, 'm'],
      ['p', 'M12.5 3V13M10.5 11L12.5 13L14.5 11', 'k']],
    desc: [['r', 2, 2.5, 7, 2.6, 'm'], ['r', 2, 6.7, 4.5, 2.6, 'm'], ['r', 2, 10.9, 2.5, 2.6, 'm'],
      ['p', 'M12.5 13V3M10.5 5L12.5 3L14.5 5', 'k']]
  };

  /** The 16 grid. @type {Record<string, Prim[]>} */
  const SMALL = {
    // A summary column's mark, as it sits in a table cell.
    'show:bar': [['p', 'M2 1.5V14.5', 'g'], ['r', 2, 3, 11, 3.5, 'm'], ['r', 2, 9.5, 7, 3.5, 'm']],
    'show:number': [['p', 'M3 5.6L5.6 4V12.5', 'k'],
      ['p', 'M8.4 5.8C8.7 4.6 9.6 4 10.6 4c1.2 0 2.1.8 2.1 2 0 2-4.3 3.6-4.3 6.5h4.5', 'k']],
    'show:dot': [['p', 'M2 5H14M2 11H14', 'k1'], ['c', 10.5, 5, 2.2, 'm'], ['c', 6, 11, 2.2, 'm']],
    'show:tile': [['r', 2, 2, 5.5, 5.5, 1], ['r', 8.5, 2, 5.5, 5.5, 0.45], ['r', 2, 8.5, 5.5, 5.5, 0.2],
      ['r', 8.5, 8.5, 5.5, 5.5, 0.75]],
    'show:pointrange': [['p', 'M2 8H14', 'g'], ['p', 'M3.5 8H12.5', 'k'], ['c', 8.5, 8, 2.4, 'm']],
    'show:box': [['p', 'M2 8H4.5M11.5 8H14', 'k'], ['p', 'M2 6V10M14 6V10', 'k1'], ['r', 4.5, 4.5, 7, 7, 's', 1],
      ['p', 'M8 4.5V11.5', 'k']],
    'show:text': [['p', 'M2 12.5L5 3.5L8 12.5M3 9.5H7', 'k'], ['c', 11.6, 10.3, 2.1, 'o'], ['p', 'M13.8 7.8V12.5', 'k']],
    'show:dumbbell': [['p', 'M4.5 8H11.5', 'k'], ['c', 4, 8, 2.2, 'o'], ['c', 12, 8, 2.4, 'm']],
    // A colour split: lying, as bar charts and table bars do by default.
    'position:stack': [['r', 2, 3, 6, 4, 'm'], ['r', 8, 3, 4, 4, 's'], ['r', 2, 9, 3, 4, 'm'], ['r', 5, 9, 4, 4, 's']],
    'position:dodge': [['r', 2, 2, 10, 2.5, 'm'], ['r', 2, 4.5, 7, 2.5, 's'], ['r', 2, 9, 6, 2.5, 'm'],
      ['r', 2, 11.5, 11, 2.5, 's']],
    'position:fill': [['p', 'M14.5 2V14', 'k1'], ['r', 2, 3, 7, 4, 'm'], ['r', 9, 3, 4.5, 4, 's'], ['r', 2, 9, 4, 4, 'm'],
      ['r', 6, 9, 7.5, 4, 's']],
    'orientation:horizontal': [['p', 'M1.5 1.5V14.5', 'g'], ['r', 2, 2.5, 8, 3, 'm'], ['r', 2, 6.5, 12, 3, 'm'],
      ['r', 2, 10.5, 5, 3, 'm']],
    'orientation:vertical': [['p', 'M1.5 14.5H14.5', 'g'], ['r', 2.5, 6, 3, 8, 'm'], ['r', 6.5, 2, 3, 12, 'm'],
      ['r', 10.5, 9, 3, 5, 'm']],
    'sort_dir:asc': SORT.asc,
    'sort_dir:desc': SORT.desc,
    'legend:bottom': [['p', 'M2 9.5L6 7L9.5 8L14 5.5', 'k'], ['p', 'M2 5.5L6 3L9.5 4L14 1.5', 'ks'],
      ['c', 3.2, 13, 1.6, 'm'], ['r', 5.4, 12.1, 3, 1.8, 's', 0.9], ['c', 10.6, 13, 1.6, 's'],
      ['r', 12.8, 12.1, 2, 1.8, 's', 0.9]],
    'legend:top': [['c', 3.2, 3, 1.6, 'm'], ['r', 5.4, 2.1, 3, 1.8, 's', 0.9], ['c', 10.6, 3, 1.6, 's'],
      ['r', 12.8, 2.1, 2, 1.8, 's', 0.9], ['p', 'M2 14L6 11.5L9.5 12.5L14 10', 'k'],
      ['p', 'M2 10L6 7.5L9.5 8.5L14 6', 'ks']],
    'legend:marks': [['p', 'M2 13L5 10.5L8 11.5L10.5 9.5', 'k'], ['p', 'M2 7L5 4.5L8 6L10.5 3.5', 'ks'],
      ['r', 11.5, 8.5, 3.5, 2, 'm', 1], ['r', 11.5, 2.5, 3.5, 2, 's', 1]],
    'legend:none': [['p', 'M2 12.5L6 9.5L9.5 11L14 6.5', 'k'], ['p', 'M2 8L6 5L9.5 6.5L14 2.5', 'ks']],
    'y_transform:identity': SCALE.identity,
    'y_transform:log10': SCALE.log10,
    'x_transform:identity': SCALE.identity,
    'x_transform:log10': SCALE.log10,
    'connect:straight': line('M2 12L6 6L10 9L14 4'),
    'connect:monotone': line('M2 12C4 8 4.6 6 6 6S8.6 9 10 9 12.4 5.2 14 4'),
    'connect:step_start': line('M2 12V6H6V9H10V4H14'),
    'connect:step_middle': line('M2 12H4V6H8V9H12V4H14'),
    'connect:step_end': line('M2 12H6V6H10V9H14V4'),
    'smoother:none': scatter([]),
    'smoother:lm': scatter([['p', 'M2 12L14 4', 'k']]),
    'smoother:loess': scatter([['p', 'M2 12C6.5 12 7.5 4 14 4', 'k']]),
    'box_points:none': [['p', 'M8 2.5V5M8 11V13.5', 'k'], ['p', 'M6.5 2.5H9.5M6.5 13.5H9.5', 'k1'],
      ['r', 4.5, 5, 7, 6, 's', 1], ['p', 'M4.5 8H11.5', 'k']],
    'box_points:outliers': [['p', 'M8 4.5V6M8 10V11.5', 'k'], ['r', 4.5, 6, 7, 4, 's', 1], ['p', 'M4.5 8H11.5', 'k'],
      ['c', 8, 2.3, 1.2, 'm'], ['c', 8, 13.7, 1.2, 'm']],
    'facet_layout:by_summary': facet('AABB'),
    'facet_layout:by_level': facet('ABAB'),
    'mode:diverging': ramp([1, 0.5, 0.1, 0.5, 1]).concat([['p', 'M8 3V13', 'k1']]),
    'mode:sequential': ramp([0.1, 0.3, 0.5, 0.75, 1]),
    'mode:bar': [['p', 'M1.5 2.5H14.5V7.5H1.5ZM1.5 8.5H14.5V13.5H1.5Z', 'g'], ['r', 2.5, 3.5, 8, 3, 'm'],
      ['r', 2.5, 9.5, 4.5, 3, 'm']]
  };

  /** The 24 grid: the table's column types. @type {Record<string, Prim[]>} */
  const LARGE = {
    'summary:simple': [['p', 'M3 2.5V21.5', 'g'], ['r', 3, 4.5, 13, 4, 'm'], ['r', 3, 10, 8, 4, 'm'],
      ['r', 3, 15.5, 17, 4, 'm']],
    'summary:dist': [['p', 'M2.5 12H8M16 12H21.5', 'k'], ['p', 'M2.5 9V15M21.5 9V15', 'k1'],
      ['r', 8, 7, 8, 10, 't', 1.5], ['r', 8, 7, 8, 10, 'ob', 1.5], ['p', 'M12 7V17', 'k']],
    'summary:field': [['r', 3, 4.5, 7, 3.5, 'm', 1.75], ['r', 11.5, 4.5, 4.5, 3.5, 's', 1.75],
      ['r', 17, 4.5, 4, 3.5, 's', 1.75], ['r', 3, 10.25, 5, 3.5, 's', 1.75], ['r', 9.5, 10.25, 8, 3.5, 'm', 1.75],
      ['r', 3, 16, 8, 3.5, 's', 1.75], ['r', 12.5, 16, 5, 3.5, 'm', 1.75]],
    'summary:sparkline': [['p', 'M2.5 20H21.5', 'g'], ['p', 'M2.5 16L6.5 11L10 13.5L14 7L17.5 9.5L21 5', 'k'],
      ['c', 21, 5, 1.8, 'm']],
    'summary:spans': [['p', 'M2 7H22M2 12H22M2 17H22', 'g'], ['r', 3, 5, 9, 4, 'm'], ['r', 8, 10, 12, 4, 'm'],
      ['r', 4, 15, 6, 4, 'm'], ['r', 12, 15, 7, 4, 's']],
    'summary:pair': [['p', 'M7 8H16.5M10 16H15.5', 'k'], ['p', 'M5.5 5.5L8 8L5.5 10.5L3 8Z', 'o'],
      ['c', 18, 8, 2.5, 'm'], ['p', 'M17.5 13.5L20 16L17.5 18.5L15 16Z', 'o'], ['c', 8.5, 16, 2.5, 'm']],
    'summary:expr': [['p', 'M9.5 20V8C9.5 5.8 10.8 4.5 13 4.5M6.5 10H12.5', 'k'],
      ['p', 'M13.5 13.5L19.5 20M19.5 13.5L13.5 20', 'k']],
    'summary:custom': [['p', 'M8.5 6.5L3.5 12L8.5 17.5M15.5 6.5L20.5 12L15.5 17.5M13.5 5L10.5 19', 'k']]
  };

  /** @type {Record<string, string>} */
  const glyphs = {};
  for (const [set, n] of /** @type {[Record<string, Prim[]>, number][]} */ ([[SMALL, 16], [LARGE, 24]])) {
    for (const k of Object.keys(set)) {
      const size = n === 24 ? 20 : 16;
      glyphs[k] = '<svg class="vz-glyph" viewBox="0 0 ' + n + ' ' + n + '" width="' + size + '" height="' + size +
        '" aria-hidden="true">' + set[k].map((p) => prim(p, n)).join('') + '</svg>';
    }
  }

  V.gear.glyphs = glyphs;
})();
// --- core/gear/controls.js
// @ts-check
/**
 * The gear's controls, one builder per `gear.control` of the spec. A builder
 * gets the row's context (VizRowCtx: the argument, its value, the columns,
 * the host's hooks, a Scope and `set()`), draws into the row and calls
 * `set(value)` with a value in its wire form. It never sends anything itself.
 *
 * The widgets are blockr.ui's (Blockr.Select, Blockr.segmented,
 * Blockr.checkbox), which every block's dependency attaches.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  V.gear = V.gear || /** @type {VizGearNamespace} */ ({});
  const ui = () => /** @type {VizUi} */ (root.Blockr);

  /** @param {unknown} v */
  const isSet = (v) => {
    if (v == null || v === '') return false;
    if (Array.isArray(v)) return v.length > 0;
    if (typeof v === 'object') return Object.keys(/** @type {object} */ (v)).length > 0;
    return true;
  };
  /** @param {unknown} a @param {unknown} b */
  const same = (a, b) => JSON.stringify(a == null ? null : a) === JSON.stringify(b == null ? null : b);

  /** The number formats both blocks offer (type `format`). */
  /** @type {VizChoice[]} */
  const FORMATS = [
    { value: 'number', label: 'Auto' }, { value: 0, label: '1,235' },
    { value: 1, label: '1,234.6' }, { value: 2, label: '1,234.57' },
    { value: 3, label: '1,234.568' }, { value: 4, label: '1,234.5679' },
    { value: 'compact', label: '1.2K' }, { value: 'percent', label: '12.3%' }
  ];

  /** @type {Record<string, string>} */
  const PLACEHOLDERS = {
    lines: 'e.g. 2 or Target: 4',
    ranges: 'e.g. Recession: 2008-01-01 to 2009-06-30'
  };

  // -- values as text ---------------------------------------------------------

  /** @param {string} t */
  const scalar = (t) => (t !== '' && Number.isFinite(Number(t)) ? Number(t) : t);

  /** "Target: 4; 6" as `[{value: 4, label: "Target"}, {value: 6, label: null}]`.
   * A colon needs a space after it, so a time of day is no label; numbers
   * apart by spaces are several lines.
   * @param {string} text @param {boolean} numbers */
  function parseLines(text, numbers) {
    /** @type {{ value: number | string, label: string | null }[]} */
    const out = [];
    for (const raw of String(text).split(/[;,]/)) {
      const part = raw.trim();
      if (!part) continue;
      const m = /^([^:]+):\s+(.*)$/.exec(part);
      const label = m ? m[1].trim() : null;
      const v = (m ? m[2] : part).trim();
      if (!v) continue;
      const tok = v.split(/\s+/);
      if (tok.length > 1 && tok.every((t) => Number.isFinite(Number(t)))) {
        tok.forEach((t, i) => out.push({ value: Number(t), label: i ? null : label }));
      } else {
        const val = scalar(v);
        if (numbers && typeof val !== 'number') continue;
        out.push({ value: val, label });
      }
    }
    return out;
  }

  /** "Recession: 2008-01-01 to 2009-06-30; ..." as a named list of two values.
   * @param {string} text */
  function parseRanges(text) {
    /** @type {Record<string, { value: number | string, label: null }[]>} */
    const out = {};
    let n = 0;
    for (const raw of String(text).split(';')) {
      const part = raw.trim();
      if (!part) continue;
      const m = /^(.*?):\s+(.*)$/.exec(part);
      const span = (m ? m[2] : part).split(/\s+to\s+/i).map((b) => scalar(b.trim()));
      if (span.length !== 2 || span.some((b) => b === '')) continue;
      n += 1;
      const name = (m && m[1].trim()) || 'Range ' + n;
      out[name] = span.map((value) => ({ value, label: null }));
    }
    return out;
  }

  /** @param {unknown} x */
  const labelled = (x) => /** @type {{ value: unknown, label: string | null }[]} */ (
    Array.isArray(x) ? x : []);

  /** The value as one line of text.
   * @param {VizArg} arg @param {unknown} value @returns {string} */
  function toText(arg, value) {
    if (value == null) return '';
    const parse = arg.gear && arg.gear.parse;
    if (parse === 'ranges') {
      return Object.entries(/** @type {Record<string, unknown>} */ (value)).map(([name, span]) =>
        name + ': ' + labelled(span).map((b) => String(b.value)).join(' to ')).join('; ');
    }
    if (parse === 'lines') {
      return labelled(value).map((l) => (l.label ? l.label + ': ' : '') + String(l.value)).join('; ');
    }
    if (arg.type.name === 'limits' && Array.isArray(value)) return value.map((v) => (v ?? 'auto')).join(' to ');
    return Array.isArray(value) ? value.join(', ') : String(value);
  }

  /** Limits as typed, "20 to 80" or "auto to 80": an end is a number, else text (a
   * date); blank or "auto" is auto, both auto null. @param {string} t */
  const parseLimits = (t) => {
    const pair = t.split(/\s+to\s+/).slice(0, 2).concat(['']).slice(0, 2).map((x) => x.trim())
      .map((e) => (e === '' || e === 'auto' ? null : (Number.isFinite(Number(e)) ? Number(e) : e)));
    return pair.every((x) => x == null) ? null : pair;
  };

  /** Text back to the wire form; `null` for an emptied nullable field.
   * @param {VizArg} arg @param {string} text @returns {unknown} */
  function fromText(arg, text) {
    const t = String(text).trim();
    const parse = arg.gear && arg.gear.parse;
    const name = arg.type.name;
    let v;
    if (parse === 'ranges') v = parseRanges(t);
    else if (name === 'limits') v = parseLimits(t);
    else if (parse === 'lines') v = parseLines(t, name === 'numbers');
    else if (name === 'number' || name === 'integer') {
      const n = Number(t);
      v = t === '' || !Number.isFinite(n) ? null : (name === 'integer' ? Math.round(n) : n);
    } else v = t;
    if (isSet(v)) return v;
    return arg.nullable ? null : (name === 'string' ? '' : arg.default ?? null);
  }

  // -- choices ----------------------------------------------------------------

  /** @param {VizRowCtx} ctx @param {VizColumn[]} cols @returns {VizChoice[]} */
  function columnChoices(ctx, cols) {
    const named = ctx.host.choices ? ctx.host.choices(ctx.key, ctx.values, ctx.columns) : null;
    /** @type {VizChoice[]} */
    const out = (ctx.arg.also || []).map((w) => {
      const c = named && named.find((x) => x.value === w);
      return { value: w, label: c ? c.label : w.charAt(0).toUpperCase() + w.slice(1) };
    });
    for (const c of cols) out.push({ value: c.name, label: c.name, note: c.label || '' });
    return out;
  }

  /** What a select-like control offers, in order, the current value always
   * among them.
   * @param {VizRowCtx} ctx @returns {VizChoice[]} */
  function choicesFor(ctx) {
    const { arg, host } = ctx;
    const name = arg.type.name;
    /** @type {VizChoice[]} */
    let out;
    if (name === 'column' || (name === 'columns' && ctx.arg.gear?.control === 'column')) {
      const cols = host.columns ? host.columns(ctx.key, ctx.values, ctx.columns) : ctx.columns;
      out = columnChoices(ctx, cols);
    } else if (name === 'columns') {
      const cols = host.columns ? host.columns(ctx.key, ctx.values, ctx.columns) : ctx.columns;
      out = cols.map((c) => ({ value: c.name, label: c.name, note: c.label || '' }));
    } else if (name === 'format') {
      out = FORMATS.slice();
    } else if (arg.values) {
      // The host may word the values for the current settings.
      const named = host.choices && host.choices(ctx.key, ctx.values, ctx.columns);
      const labels = (arg.gear && arg.gear.labels) || [];
      out = named || arg.values.map((v, i) => ({ value: v, label: labels[i] || String(v) }));
    } else {
      out = (host.choices && host.choices(ctx.key, ctx.values, ctx.columns)) || [];
    }
    const cur = Array.isArray(ctx.value) && name !== 'columns' && name !== 'enums'
      ? null : ctx.value;
    const curs = Array.isArray(cur) ? cur : (isSet(cur) ? [cur] : []);
    for (const c of curs) {
      if (!out.some((o) => same(o.value, c))) {
        // A column the data lacks says so; one the host does not offer (a
        // facet with too many values) shows as it is.
        const missing = (name === 'column' || name === 'columns') &&
          !(ctx.columns || []).some((col) => col.name === c);
        out.push({ value: c, label: String(c) + (missing ? ' (missing)' : '') });
      }
    }
    return out;
  }

  /** Whether a single pick may be left empty, and what empty is called.
   * @param {VizRowCtx} ctx */
  function emptyChoice(ctx) {
    if (ctx.required || !ctx.arg.nullable) return null;
    const name = ctx.arg.type.name;
    return { value: null, label: name === 'column' || name === 'columns' ? '(none)' : 'Auto' };
  }

  // -- widgets ----------------------------------------------------------------

  /** Choices as Blockr.Select options. Select prints an object as its value
   * with its label muted, which suits a column (name, variable label); a
   * fixed option goes in as its label alone. `keys[i]` is the string that
   * stands for choice i.
   * @param {VizChoice[]} choices */
  function uiOptions(choices) {
    /** @type {string[]} */
    const keys = [];
    const options = choices.map((c) => {
      let k = c.note != null ? String(c.value ?? c.label) : c.label;
      while (keys.includes(k)) k += ' ';
      keys.push(k);
      return c.note ? { value: k, label: c.note } : k;
    });
    return { options, keys };
  }

  /** What an empty pick shows: the host's words for the key ("All
   * columns"), else nothing. @param {VizRowCtx} ctx */
  const placeholderOf = (ctx) =>
    (ctx.host.placeholder && ctx.host.placeholder(ctx.key, ctx.values)) ||
    (ctx.arg.gear && ctx.arg.gear.placeholder) || undefined;

  /** A single pick among `choices`; `onPick` gets the chosen value.
   * @param {HTMLElement} el @param {VizChoice[]} choices @param {unknown} value
   * @param {(v: unknown) => void} onPick @param {VizRowCtx} ctx */
  function pickOne(el, choices, value, onPick, ctx) {
    const at = choices.findIndex((c) => same(c.value, value));
    const { options, keys } = uiOptions(choices);
    const inst = ui().Select.single(el, {
      options, selected: at < 0 ? '' : keys[at], allowEmpty: true, bordered: true,
      label: ctx.label, placeholder: placeholderOf(ctx),
      onChange: (/** @type {string} */ v) => {
        const i = keys.indexOf(v);
        if (i >= 0) onPick(choices[i].value);
      }
    });
    ctx.scope.own({ dispose: () => inst.destroy() });
  }

  /** Several picks among `choices`, in pick order.
   * @param {HTMLElement} el @param {VizChoice[]} choices @param {unknown[]} values
   * @param {(v: unknown[]) => void} onPick @param {VizRowCtx} ctx */
  function pickMany(el, choices, values, onPick, ctx) {
    const idx = values.map((v) => choices.findIndex((c) => same(c.value, v))).filter((i) => i >= 0);
    const { options, keys } = uiOptions(choices);
    const inst = ui().Select.multi(el, {
      options, selected: idx.map((i) => keys[i]), bordered: true, label: ctx.label,
      placeholder: placeholderOf(ctx),
      onChange: (/** @type {string[]} */ v) =>
        onPick(v.map((k) => keys.indexOf(k)).filter((i) => i >= 0).map((i) => choices[i].value))
    });
    ctx.scope.own({ dispose: () => inst.destroy() });
  }

  /** @param {HTMLElement} el @param {string} label @param {boolean} on
   * @param {(on: boolean) => void} onToggle @param {VizRowCtx} ctx */
  function checkbox(el, label, on, onToggle, ctx) {
    const h = ui().checkbox(label, on, onToggle);
    h.input.setAttribute('data-gear-input', ctx.key);
    el.appendChild(h.el);
  }

  /** @param {HTMLElement} el @param {VizChoice[]} choices @param {unknown} value
   * @param {(v: unknown) => void} onPick @param {VizRowCtx} ctx */
  function segments(el, choices, value, onPick, ctx) {
    const at = String(choices.findIndex((c) => same(c.value, value)));
    const opts = choices.map((c, i) => ({ value: String(i), label: c.label }));
    const pick = (/** @type {string} */ v) => onPick(choices[Number(v)].value);
    const seg = ui().segmented(opts, at, pick, { label: ctx.label }).el;
    el.appendChild(seg);
    if (ctx.arg.gear && ctx.arg.gear.icons) withIcons(el, seg, choices, ctx);
  }

  /** Each segment its icon (icons.js) before its word. Where the words
   * would not fit the row, they go, and each icon is named by its tooltip.
   * @param {HTMLElement} el @param {HTMLElement} seg @param {VizChoice[]} choices
   * @param {VizRowCtx} ctx */
  function withIcons(el, seg, choices, ctx) {
    const glyphs = V.gear.glyphs || {};
    const btns = /** @type {HTMLElement[]} */ (Array.from(seg.children));
    btns.forEach((b, i) => {
      const icon = document.createElement('span');
      icon.className = 'vz-seg-icon';
      icon.innerHTML = glyphs[ctx.key + ':' + String(choices[i].value)] || '';
      const word = document.createElement('span');
      word.className = 'vz-seg-word';
      word.textContent = choices[i].label;
      b.textContent = '';
      b.append(icon, word);
    });
    seg.classList.add('vz-seg--icons');
    const fit = () => {
      const room = el.getBoundingClientRect().width;
      if (!room) return;
      seg.classList.remove('vz-seg--bare');
      // The control fills its cell, so words that do not fit overflow it.
      const bare = Math.max(seg.scrollWidth, seg.getBoundingClientRect().width) > room + 0.5;
      seg.classList.toggle('vz-seg--bare', bare);
      btns.forEach((b, i) => {
        if (bare) {
          b.setAttribute('data-blockr-tooltip', choices[i].label);
          b.setAttribute('aria-label', choices[i].label);
        } else {
          b.removeAttribute('data-blockr-tooltip');
          b.removeAttribute('aria-label');
        }
      });
    };
    fit();
    if (typeof root.ResizeObserver === 'function') {
      const ro = new root.ResizeObserver(fit);
      ro.observe(el);
      ctx.scope.own({ dispose: () => ro.disconnect() });
    }
  }

  /** A text field that commits on Enter or when it is left; Escape puts the
   * value back.
   * @param {HTMLElement} el @param {VizRowCtx} ctx
   * @param {{ multiline?: boolean, type?: string, onInput?: (inp: HTMLInputElement, e: Event) => void,
   *           onKeydown?: (e: KeyboardEvent, inp: HTMLInputElement) => boolean,
   *           commit?: (text: string) => void }} [opts] */
  function field(el, ctx, opts = {}) {
    const inp = /** @type {HTMLInputElement} */ (document.createElement(
      opts.multiline ? 'textarea' : 'input'));
    if (!opts.multiline) inp.type = opts.type || 'text';
    inp.className = 'blockr-text-input vz-text' + (opts.multiline ? ' vz-script-editor' : '');
    const before = ctx.arg.type.name === 'template' ? String(ctx.value ?? '')
      : toText(ctx.arg, ctx.value);
    inp.value = before;
    inp.setAttribute('aria-label', ctx.label);
    inp.setAttribute('data-gear-input', ctx.key);
    const ph = placeholderOf(ctx) ||
      PLACEHOLDERS[(ctx.arg.gear && ctx.arg.gear.parse) || ''] || '';
    if (ph) inp.placeholder = ph;
    const commit = opts.commit || ((/** @type {string} */ t) => {
      const v = fromText(ctx.arg, t);
      if (!same(v, ctx.value)) ctx.set(v);
    });
    ctx.scope.on(inp, 'keydown', (ev) => {
      const e = /** @type {KeyboardEvent} */ (ev);
      if (opts.onKeydown && opts.onKeydown(e, inp)) return;
      if (e.key === 'Enter' && !(opts.multiline && !e.metaKey && !e.ctrlKey)) {
        e.preventDefault();
        commit(inp.value);
      } else if (e.key === 'Escape') {
        inp.value = before;
        inp.blur();
      }
    });
    ctx.scope.on(inp, 'blur', () => { if (inp.value !== before) commit(inp.value); });
    if (opts.onInput) {
      const f = opts.onInput;
      ctx.scope.on(inp, 'input', (e) => f(inp, e));
    }
    el.appendChild(inp);
    return inp;
  }

  // -- the controls -----------------------------------------------------------

  /** One column; also the column of a highlight, and one column of a
   * `columns` argument (an interval's low and high).
   * @param {HTMLElement} el @param {VizRowCtx} ctx */
  function column(el, ctx) {
    const name = ctx.arg.type.name;
    if (name === 'named') return namedColumn(el, ctx);
    const many = name === 'columns';
    const cur = many ? (Array.isArray(ctx.value) ? ctx.value[0] : null) : ctx.value;
    const empty = emptyChoice(ctx);
    const choices = /** @type {VizChoice[]} */ (empty ? [empty] : []).concat(choicesFor({ ...ctx, value: cur }));
    pickOne(el, choices, cur ?? null, (v) => ctx.set(many ? (v == null ? null : [v]) : v), ctx);
  }

  /** A column and some of its values, as `{column: [{value, label}]}`.
   * @param {HTMLElement} el @param {VizRowCtx} ctx */
  function namedColumn(el, ctx) {
    const cur = /** @type {Record<string, unknown>} */ (ctx.value || {});
    const col = Object.keys(cur)[0] || null;
    const cols = ctx.host.columns ? ctx.host.columns(ctx.key, ctx.values, ctx.columns) : ctx.columns;
    const choices = /** @type {VizChoice[]} */ ([{ value: null, label: '(none)' }]).concat(columnChoices(ctx, cols));
    pickOne(el, choices, col, (v) => ctx.set(v == null ? null : { [String(v)]: [] }), ctx);
    if (!col) return;
    const vals = labelled(cur[col]).map((x) => x.value);
    const offered = (ctx.host.choices && ctx.host.choices(ctx.key, ctx.values, ctx.columns)) || [];
    const all = offered.concat(vals.filter((v) => !offered.some((o) => same(o.value, v)))
      .map((v) => ({ value: v, label: String(v) })));
    const sub = document.createElement('div');
    sub.className = 'vz-row-sub';
    el.appendChild(sub);
    pickMany(sub, all, vals, (vs) =>
      ctx.set({ [col]: vs.map((value) => ({ value, label: null })) }), ctx);
  }

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function columns(el, ctx) {
    const cur = Array.isArray(ctx.value) ? ctx.value : [];
    pickMany(el, choicesFor(ctx), cur, (vs) =>
      ctx.set(vs.length || !ctx.arg.nullable ? vs : null), ctx);
  }

  /** Some values of the column another argument names, offered by the host.
   * @param {HTMLElement} el @param {VizRowCtx} ctx */
  function valuesCtl(el, ctx) {
    const cur = labelled(ctx.value).map((x) => x.value);
    const choices = choicesFor({ ...ctx, value: cur });
    pickMany(el, choices, cur, (vs) => ctx.set(vs.length ? vs.map((value) => ({ value, label: null }))
      : (ctx.arg.nullable ? null : [])), ctx);
  }

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function select(el, ctx) {
    if (ctx.arg.type.name === 'columns' || ctx.arg.type.name === 'enums') return columns(el, ctx);
    const empty = emptyChoice(ctx);
    const choices = /** @type {VizChoice[]} */ (empty ? [empty] : []).concat(choicesFor(ctx));
    pickOne(el, choices, ctx.value ?? null, (v) => ctx.set(v), ctx);
  }

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function segmented(el, ctx) {
    if (ctx.arg.type.name === 'bool') {
      const labels = (ctx.arg.gear && ctx.arg.gear.labels) || [];
      checkbox(el, labels[0] || ctx.label, ctx.value === true, (on) => ctx.set(on), ctx);
      return;
    }
    segments(el, choicesFor(ctx), ctx.value ?? null, (v) => ctx.set(v), ctx);
  }

  /** A checkbox; on a column it switches the setting on with its first
   * keyword (drill "auto") and shows the column picker beside it. The box's
   * words are the first of the gear's labels, the row's label above it.
   * @param {HTMLElement} el @param {VizRowCtx} ctx */
  function toggle(el, ctx) {
    const words = ((ctx.arg.gear && ctx.arg.gear.labels) || [])[0] || ctx.label;
    if (ctx.arg.type.name === 'bool') {
      checkbox(el, words, ctx.value === true, (on) => ctx.set(on), ctx);
      return;
    }
    const on = isSet(ctx.value);
    const choices = choicesFor(ctx);
    checkbox(el, words, on, (now) =>
      ctx.set(now ? (choices[0] ? choices[0].value : null) : null), ctx);
    if (!on) return;
    const sub = document.createElement('div');
    sub.className = 'vz-row-sub';
    el.appendChild(sub);
    pickOne(sub, choices, ctx.value, (v) => ctx.set(v), ctx);
  }

  /** @param {VizArg} arg */
  const bounds = (arg) => {
    const b = arg.bounds || { min: null, max: null, minOpen: false };
    const step = arg.type.name === 'integer' ? 1 : 0.1;
    const min = b.min == null ? -Infinity : (b.minOpen ? b.min + step : b.min);
    return { min, max: b.max == null ? Infinity : b.max, step };
  };

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function number(el, ctx) {
    const { min, max } = bounds(ctx.arg);
    const inp = field(el, ctx, {
      type: 'number',
      commit: (t) => {
        let v = /** @type {number | null} */ (fromText(ctx.arg, t));
        if (v != null) v = Math.min(max, Math.max(min, v));
        if (!same(v, ctx.value)) ctx.set(v);
      }
    });
    if (Number.isFinite(min)) inp.min = String(min);
    if (Number.isFinite(max)) inp.max = String(max);
    inp.step = ctx.arg.type.name === 'integer' ? '1' : 'any';
  }

  /** The prepare script sits behind a checkbox until there is one;
   * unticking it drops the script.
   * @param {HTMLElement} el @param {VizRowCtx} ctx */
  function scriptCtl(el, ctx) {
    const box = document.createElement('div');
    box.className = 'vz-script-wrap';
    const show = (/** @type {boolean} */ on) => { box.style.display = on ? '' : 'none'; };
    show(isSet(ctx.value));
    checkbox(el, 'Use a prepare script', isSet(ctx.value), (on) => {
      show(on);
      if (!on && isSet(ctx.value)) ctx.set(null);
    }, { ...ctx, key: ctx.key + '_on' });
    el.appendChild(box);
    field(box, ctx, { multiline: true });
  }

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function textCtl(el, ctx) {
    field(el, ctx);
  }

  /** Text over several lines: an R expression or function.
   * @param {HTMLElement} el @param {VizRowCtx} ctx */
  function code(el, ctx) {
    field(el, ctx, { multiline: true });
  }

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function slider(el, ctx) {
    const { min, max, step } = bounds(ctx.arg);
    const inp = document.createElement('input');
    inp.type = 'range';
    inp.className = 'vz-slider';
    inp.min = String(Number.isFinite(min) ? min : 0);
    inp.max = String(Number.isFinite(max) ? max : 10);
    inp.step = String(step);
    inp.value = String(ctx.value ?? ctx.arg.default ?? inp.min);
    inp.setAttribute('aria-label', ctx.label);
    inp.setAttribute('data-gear-input', ctx.key);
    const out = document.createElement('span');
    out.className = 'vz-slider-value';
    out.textContent = inp.value + '×';
    ctx.scope.on(inp, 'input', () => { out.textContent = inp.value + '×'; });
    ctx.scope.on(inp, 'change', () => ctx.set(Number(inp.value)));
    el.append(inp, out);
  }

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function tiles(el, ctx) {
    const choices = choicesFor(ctx);
    const icon = (/** @type {unknown} */ v) => (ctx.host.icon ? ctx.host.icon(ctx.key, v) : '') ||
      V.gear.icons[ctx.key + ':' + String(v)] || '';
    const groups = (ctx.host.tiles && ctx.host.tiles(ctx.key, ctx.values)) ||
      [{ values: choices.map((c) => c.value) }];
    for (const g of groups) {
      // One block per group: its heading over its tiles.
      const box = document.createElement('div');
      box.className = 'vz-tile-group';
      el.appendChild(box);
      if (g.label) {
        const head = document.createElement('div');
        head.className = 'vz-tile-group-head';
        head.textContent = g.label;
        box.appendChild(head);
      }
      const opts = choices.filter((c) => g.values.some((v) => same(v, c.value)))
        .map((c) => ({ value: String(c.value), label: c.label, icon: icon(c.value) }));
      V.gear.tiles.build(box, opts, String(ctx.value), (v) => {
        const c = choices.find((x) => String(x.value) === v);
        if (c) ctx.set(c.value);
      }, ctx.scope);
    }
  }

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function template(el, ctx) {
    V.titles.row(el, ctx);
  }

  /** The view's own editor for the row (host.custom), or nothing.
   * @param {HTMLElement} el @param {VizRowCtx} ctx */
  function custom(el, ctx) {
    if (!ctx.host.custom) return;
    ctx.host.custom(ctx.key, el, { values: ctx.values, columns: ctx.columns, scope: ctx.scope,
                                   set: ctx.set });
  }

  /** @type {Record<string, (el: HTMLElement, ctx: VizRowCtx) => void>} */
  const BUILD = { column, columns, values: valuesCtl, select, segmented, toggle, number,
                  text: textCtl, code, script: scriptCtl, slider,
                  tiles, template, custom };

  /** Draw a row's controls. A template row draws its own field, preview and
   * help.
   * @param {HTMLElement} el @param {VizRowCtx} ctx */
  function build(el, ctx) {
    const control = (ctx.arg.gear && ctx.arg.gear.control) || 'text';
    (BUILD[control] || (V.gear.more || {})[control] || textCtl)(el, ctx);
  }

  V.gear.controls = { build, options: choicesFor, toText, fromText, isSet, same, emptyChoice,
                       uiOptions, field };
})();
// --- core/gear/range.js
// @ts-check
/**
 * The gear's range control: two fields, from and to, for axis limits. Each
 * is blank for auto; either commits the pair, both blank is null. A log axis
 * starts above 0, so an end at or below 0 is not sent while the axis is log
 * (`<role>_transform` is "log10"); the note under the fields says why. The
 * sentence's words for the same setting ask limitsNote() too (engine.js).
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const C = V.gear.controls;

  /** Why a pair cannot be set on its axis, or null.
   * @param {VizValues} values @param {string} key @param {unknown} pair @returns {string | null} */
  const limitsNote = (values, key, pair) => {
    const tk = key.replace(/_limits$/, '_transform');
    const arg = /** @type {any} */ (V.spec.chart || {})[tk];
    const only = arg && arg.applies && arg.applies.only;
    const log = values[tk] === 'log10' && (!only || only.includes(values.type));
    const low = Array.isArray(pair) && pair.some((v) => typeof v === 'number' && v <= 0);
    return log && low ? 'A log axis starts above 0. Leave the field empty for auto.' : null;
  };

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function range(el, ctx) {
    const cur = Array.isArray(ctx.value) ? ctx.value : [null, null];
    const box = el.appendChild(Object.assign(document.createElement('div'), { className: 'vz-range' }));
    const note = el.appendChild(Object.assign(document.createElement('div'), { className: 'vz-range-note' }));
    note.hidden = true;
    /** @type {HTMLInputElement[]} */
    const inputs = [];
    const commit = () => {
      const v = C.fromText(ctx.arg, inputs.map((inp) => inp.value.trim() || 'auto').join(' to '));
      const why = limitsNote(ctx.values, ctx.key, v);
      note.textContent = why || '';
      note.hidden = !why;
      if (!why && !C.same(v, ctx.value)) ctx.set(v);
    };
    ['from', 'to'].forEach((end, i) => {
      if (i) box.appendChild(Object.assign(document.createElement('span'), { className: 'vz-range-sep', textContent: 'to' }));
      const arg = /** @type {VizArg} */ ({ ...ctx.arg, type: { name: 'string' }, gear: { ...ctx.arg.gear, parse: undefined } });
      const inp = C.field(box, { ...ctx, arg, key: ctx.key + '_' + end, label: ctx.label + ' ' + end,
                                 value: cur[i] == null ? '' : String(cur[i]) }, { commit });
      inp.placeholder = 'auto';
      inputs.push(inp);
    });
  }

  V.gear.more = { ...(V.gear.more || {}), range };
  V.gear.limitsNote = limitsNote;
})();
// --- core/gear/ticks.js
// @ts-check
/**
 * The gear's ticks control, for `x_ticks` and `y_ticks`: Auto | Every | At,
 * then the fields of that form. Every takes a number and, on a date axis, a
 * unit; At takes a comma list. Picking a form starts from the ticks the axis
 * draws now (V.ticks.start), so nothing has to be typed to see it work. A
 * log axis takes a list only: Every is off and a set step says why. The
 * sentence's words for the same setting come from ticksSlot(), which the
 * engine asks for a `ticks` word.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const C = V.gear.controls;
  const T = V.ticks;
  const ui = () => /** @type {VizUi} */ (root.Blockr);

  const FORMS = [{ value: 'auto', label: 'Auto' }, { value: 'every', label: 'Every' },
                 { value: 'at', label: 'At' }];

  /** The sentence's words for a ticks setting, one per part as the gear has a
   * control per part: the form ("auto", "every", "at", a menu), the step ("7",
   * typed over) and its unit on a date axis ("days", a menu), or the list ("0,
   * 25, 50", typed over). A form picked starts from the ticks drawn now.
   * @param {VizGearHost} host @param {string} key @param {unknown} value @param {string} label
   * @param {(v: unknown) => void} set @param {VizValues} values @returns {VizSlotOpener} */
  function ticksSlot(host, key, value, label, set, values) {
    const axis = (host.ticks && host.ticks(key, values)) || { type: 'value', drawn: [] };
    const log = axis.type === 'log';
    const p = T.parse(value);
    const form = p ? p.form : 'auto';
    /** @param {unknown} v */
    const send = (v) => { if (!T.note(v, log)) set(v); };
    /** @param {VizChoice[]} options @param {unknown} selected @param {string} lab
     * @param {(v: unknown) => void} pick @returns {VizWordPart['how']} */
    const menu = (options, selected, lab, pick) => ({ kind: 'menu', options, selected, multi: false,
                                                      label: lab, set: pick });
    const forms = ['auto', 'every', 'at'].filter((f) => !(log && f === 'every'))
      .map((f) => ({ value: f, label: f }));
    /** @type {VizWordPart[]} */
    const words = [{ text: form, how: menu(forms, form, label, (f) => {
      if (f === form) return;
      // The ticks drawn now, read at the pick: the words may predate the draw.
      const now = (host.ticks && host.ticks(key, values)) || axis;
      send(f === 'auto' ? null : T.start(/** @type {'every' | 'at'} */ (f), now.drawn, now.type));
    }) }];
    if (p && p.form === 'every') {
      words.push({ text: String(p.n), how: { kind: 'entry', value: p.n, label: label + ' step',
        toText: (v) => (v == null ? '' : String(v)),
        fromText: (t) => { const n = Number(String(t).trim()); return Number.isFinite(n) && n > 0 ? n : null; },
        set: (n) => { if (typeof n === 'number') send(T.step(n, p.unit)); } } });
      if (p.unit) {
        const units = T.UNITS.map((u) => ({ value: u, label: T.unitWord(u, p.n) }));
        words.push({ text: T.unitWord(p.unit, p.n),
                     how: menu(units, p.unit, label + ' unit', (u) => send(T.step(p.n, String(u)))) });
      }
    }
    if (p && p.form === 'at') {
      words.push({ text: p.list.join(', '), how: { kind: 'entry', value: p.list, label,
        toText: (v) => (Array.isArray(v) ? v.join(', ') : ''),
        fromText: (t) => T.parseList(t),
        set: (l) => { if (Array.isArray(l) && l.length >= 2) send(l); } } });
    }
    return { kind: 'words', value, words, set, label };
  }

  /** @param {HTMLElement} el @param {VizRowCtx} ctx */
  function ticks(el, ctx) {
    const axis = (ctx.host.ticks && ctx.host.ticks(ctx.key, ctx.values)) || { type: 'value', drawn: [] };
    const log = axis.type === 'log';
    const p = T.parse(ctx.value);
    const form = p ? p.form : 'auto';
    const box = el.appendChild(Object.assign(document.createElement('div'), { className: 'vz-ticks' }));
    const note = el.appendChild(Object.assign(document.createElement('div'), { className: 'vz-range-note' }));
    const say = (/** @type {string | null} */ why) => { note.textContent = why || ''; note.hidden = !why; };
    say(T.note(ctx.value, log));
    /** @param {unknown} v */
    const send = (v) => {
      const why = T.note(v, log);
      say(why);
      if (!why && !C.same(v, ctx.value)) ctx.set(v);
    };

    const seg = ui().segmented(FORMS, form, (/** @type {string} */ f) => {
      // The ticks drawn now, read at the click: the row may predate the draw.
      const now = (ctx.host.ticks && ctx.host.ticks(ctx.key, ctx.values)) || axis;
      send(f === 'auto' ? null : T.start(/** @type {'every' | 'at'} */ (f), now.drawn, now.type));
    }, { label: ctx.label });
    box.appendChild(seg.el);
    if (log) {
      const every = /** @type {HTMLButtonElement | undefined} */ (seg.el.children[1]);
      if (every) {
        every.disabled = true;
        every.title = T.LOG_NOTE;
      }
    }

    /** A text field in the row, holding `text`, committing through `commit`.
     * @param {string} key @param {string} label @param {string} text
     * @param {(t: string) => void} commit @param {string} cls */
    const field = (key, label, text, commit, cls) => {
      const arg = { ...ctx.arg, type: { name: 'string' }, gear: { ...ctx.arg.gear, parse: undefined } };
      const inp = C.field(box, /** @type {VizRowCtx} */ ({ ...ctx, arg, key: ctx.key + '_' + key,
                                                           label: ctx.label + ' ' + label, value: text }),
                          { commit });
      inp.classList.add(cls);
      return inp;
    };

    if (p && p.form === 'every') {
      const step = p;
      const inp = field('step', 'step', String(step.n), (t) => {
        const n = Number(String(t).trim());
        if (Number.isFinite(n) && n > 0) send(T.step(n, step.unit));
        else say('A step is a number above 0.');
      }, 'vz-ticks-step');
      inp.inputMode = 'decimal';
      if (axis.type === 'time' || step.unit) {
        const unit = step.unit || 'day';
        const host = box.appendChild(Object.assign(document.createElement('div'), { className: 'vz-ticks-unit' }));
        const opts = T.UNITS.map((u) => ({ value: u, label: T.unitWord(u, step.n) }));
        const inst = ui().Select.single(host, {
          options: opts.map((o) => o.label), selected: T.unitWord(unit, step.n), bordered: true,
          label: ctx.label + ' unit',
          onChange: (/** @type {string} */ v) => {
            const o = opts.find((x) => x.label === v);
            if (o) send(T.step(step.n, o.value));
          }
        });
        ctx.scope.own({ dispose: () => inst.destroy() });
      }
    }
    if (p && p.form === 'at') {
      field('list', 'values', p.list.join(', '), (t) => {
        const list = T.parseList(t);
        if (list.length < 2) { say(T.LIST_NOTE); return; }
        send(list);
      }, 'vz-ticks-list');
    }
  }

  V.gear.more = { ...(V.gear.more || {}), ticks };
  V.gear.ticksSlot = ticksSlot;
})();
// --- core/gear/profile.js
// @ts-check
/**
 * A block's own gear (R's table_gear(), sent as `derived.gear`): its tabs and
 * sections in place of the table's, and the rows they name. A row is a
 * setting of the spec, moved into the gear's section; a value the prepare
 * script declares; the column checklist; the "In the subtitle" ticks; or an
 * editor a package registers (`V.gear.editors[key]`). Settings the gear does
 * not name keep their spec entry, so a word in the titles still opens them,
 * but have no row.
 *
 * The rows the spec does not have write settings it does have: a script
 * row `script_values`, the checklist `summaries`, the ticks `subtitle`
 * (translate()). An editor writes what it likes through `ctx.setValues`.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  V.gear = V.gear || /** @type {VizGearNamespace} */ ({});
  V.gear.editors = V.gear.editors || {};

  /** A script row's key: its variable's name behind a prefix no setting has. */
  const SV = 'sv.';

  /** A declared control as a spec argument the gear can draw (R's
   * script_control_records(): kind select, multi, segmented, number, date,
   * text). @param {VizScriptControl} sp @returns {VizArg} */
  const scriptArg = (sp) => {
    const gear = { label: sp.label, group: '', placeholder: sp.placeholder || undefined };
    const bounds = { min: sp.min ?? null, max: sp.max ?? null, minOpen: false };
    switch (sp.kind) {
      case 'select': return /** @type {VizArg} */ ({ type: { name: 'enum' }, values: sp.options || [],
                                                     gear: { ...gear, control: 'select' } });
      case 'multi': return /** @type {VizArg} */ ({ type: { name: 'enums' }, values: sp.options || [],
                                                    gear: { ...gear, control: 'select' } });
      case 'segmented': return /** @type {VizArg} */ ({ type: { name: 'bool' },
                                                        gear: { ...gear, control: 'toggle' } });
      case 'number': return /** @type {VizArg} */ ({ type: { name: 'number' }, bounds,
                                                     gear: { ...gear, control: 'number' } });
      default: return /** @type {VizArg} */ ({ type: { name: 'string' }, nullable: true,
                                               gear: { ...gear, control: 'text' } });
    }
  };

  /** The control's value as the row holds it: R sends a flag as "on"/"off"
   * and a single choice as a one-element array (a map's values).
   * @param {VizScriptControl} sp @param {unknown} v */
  const scriptValue = (sp, v) => {
    if (sp.kind === 'segmented') return v === 'on' || v === true;
    if (sp.kind !== 'multi' && Array.isArray(v)) return v[0] ?? null;
    return v;
  };

  /** Is a clause in the template? @param {string} tpl @param {string} c */
  const has = (tpl, c) => tpl.includes(c);

  /**
   * The subtitle with exactly the ticked clauses: an unticked one taken out
   * with its comma, a ticked one put in after the nearest earlier clause the
   * subtitle has (in the gear's sentence order), else before the nearest
   * later one, else at the end. Text the builder typed stays.
   * @param {string} tpl @param {[string, string][]} clauses In sentence order.
   * @param {Set<string>} on
   */
  function setWords(tpl, clauses, on) {
    let t = tpl;
    for (const [k, c] of clauses) {
      if (on.has(k) || !has(t, c)) continue;
      if (has(t, ', ' + c)) t = t.replace(', ' + c, '');
      else if (has(t, c + ', ')) t = t.replace(c + ', ', '');
      else t = t.replace(c, '');
    }
    clauses.forEach(([k, c], i) => {
      if (!on.has(k) || has(t, c)) return;
      const before = clauses.slice(0, i).reverse().find(([, p]) => has(t, p));
      const after = clauses.slice(i + 1).find(([, p]) => has(t, p));
      if (before) {
        const at = t.indexOf(before[1]) + before[1].length;
        t = t.slice(0, at) + ', ' + c + t.slice(at);
      } else if (after) {
        const at = t.indexOf(after[1]);
        t = t.slice(0, at) + c + ', ' + t.slice(at);
      } else {
        t = t ? t + ', ' + c : c;
      }
    });
    return t.trim();
  }

  /**
   * The gear's spec, sections and tabs. Rows come first, in the gear's order,
   * then the rest of the spec, which has no row.
   * @param {VizSpec} base @param {VizGearWire} wire @param {VizScript | null} script
   * @returns {VizProfile}
   */
  function build(base, wire, script) {
    /** @type {VizSpec} */
    const spec = {};
    /** @type {Record<string, [string, number]>} */
    const sections = {};
    /** @type {[string, string][]} */
    const groups = [];
    /** @type {Set<string>} */
    const keys = new Set();
    /** @type {Record<string, VizProfileRow>} */
    const rows = {};
    const inputs = /** @type {VizScriptControl[]} */ (
      /** @type {VizScriptInput[]} */ ((script && script.inputs) || [])
        .filter((sp) => sp && sp.kind !== 'error'));
    const columns = wire.columns || [];
    const clauses = Object.entries(wire.clauses || {});
    let ord = 0;
    /** @param {string} key @param {VizArg} arg @param {string} group */
    const put = (key, arg, group) => {
      spec[key] = Object.assign({}, arg, { gear: Object.assign({}, arg.gear, { group }) });
      keys.add(key);
    };
    (wire.tabs || []).forEach((tab, ti) => {
      const gk = 'tab' + ti;
      groups.push([gk, tab.title]);
      for (const sec of tab.sections || []) {
        // A section title is its key in the plan: one used by an earlier tab
        // is told apart by its tab.
        const title = sections[sec.title] ? sec.title + ' (' + tab.title + ')' : sec.title;
        sections[title] = [gk, ord++];
        for (const k of /** @type {string[]} */ ([]).concat(sec.keys || [])) {
          const sp = inputs.find((x) => x.name === k);
          const ed = V.gear.editors[k];
          if (sp) {
            rows[SV + k] = { kind: 'script', name: k, sp };
            put(SV + k, scriptArg(sp), title);
          } else if (k === 'columns' && columns.length) {
            rows[k] = { kind: 'columns' };
            put(k, /** @type {VizArg} */ ({ type: { name: 'values' },
              gear: { label: 'Listing columns', control: 'profile' } }), title);
          } else if (k === 'group' && (wire.groups?.values || []).length) {
            const g = /** @type {{ values: string[], labels: string[] }} */ (wire.groups);
            rows[k] = { kind: 'group' };
            put(k, /** @type {VizArg} */ ({ type: { name: 'enum' }, values: [''].concat(g.values),
              gear: { label: 'Group by', control: 'select', labels: ['None'].concat(g.labels) } }),
              title);
          } else if (k === 'words' && clauses.length) {
            rows[k] = { kind: 'words' };
            put(k, /** @type {VizArg} */ ({ type: { name: 'values' },
              gear: { label: 'In the subtitle', control: 'profile' } }), title);
          } else if (base[k] && base[k].gear) {
            put(k, base[k], title);
          } else if (ed) {
            rows[k] = { kind: 'editor' };
            put(k, /** @type {VizArg} */ ({ type: { name: 'any' },
              gear: { label: ed.label || k, control: 'profile' } }), title);
          }
        }
      }
    });
    for (const [k, arg] of Object.entries(base)) if (!spec[k]) spec[k] = arg;
    return { spec, sections, groups, keys, rows, wire, clauses,
             sig: JSON.stringify([wire, inputs.map((sp) => [sp.name, sp.kind, sp.options])]) };
  }

  /** The values of the rows the spec does not have, read off the settings
   * and the script's values. @param {VizProfile} prof @param {VizValues} cfg
   * @param {VizScript | null} script @returns {VizValues} */
  function values(prof, cfg, script) {
    /** @type {VizValues} */
    const out = {};
    const sent = /** @type {Record<string, unknown>} */ (
      script && !Array.isArray(script.values) ? script.values || {} : {});
    const names = new Set((prof.wire.columns || []).map((c) => c.name));
    for (const [key, r] of Object.entries(prof.rows)) {
      if (r.kind === 'script' && r.sp) out[key] = scriptValue(r.sp, sent[r.sp.key]);
      else if (r.kind === 'columns') {
        out[key] = (Array.isArray(cfg.summaries) ? cfg.summaries : [])
          .map((s) => s && s.name).filter((n) => names.has(n));
      } else if (r.kind === 'group') {
        const by = Array.isArray(cfg.by) ? cfg.by : [];
        out[key] = by.length > 1 ? by[0] : '';
      } else if (r.kind === 'words') {
        const tpl = String(cfg.subtitle ?? '');
        out[key] = prof.clauses.filter(([, c]) => has(tpl, c)).map(([k]) => k);
      }
    }
    return out;
  }

  /** The settings a change of the gear's rows writes: script rows into
   * `script_values`, the checklist into `summaries`, the ticks into
   * `subtitle`; an editor's own key into nothing (it writes through
   * `setValues`). @param {VizProfile} prof @param {VizValues} changed
   * @param {VizValues} now @returns {VizValues} */
  function translate(prof, changed, now) {
    /** @type {VizValues} */
    const out = {};
    for (const [key, v] of Object.entries(changed)) {
      const r = prof.rows[key];
      if (!r) {
        out[key] = v;
      } else if (r.kind === 'script') {
        const held = /** @type {Record<string, unknown>} */ (
          out.script_values || now.script_values || {});
        out.script_values = Object.assign({}, held, { [/** @type {string} */ (r.name)]: v });
      } else if (r.kind === 'columns') {
        const pick = new Set(/** @type {string[]} */ (v || []));
        const cols = prof.wire.columns || [];
        const names = new Set(cols.map((c) => c.name));
        const other = (Array.isArray(now.summaries) ? now.summaries : [])
          .filter((s) => !s || !names.has(s.name));
        out.summaries = cols.filter((c) => pick.has(c.name)).map((c) => c.summary).concat(other);
      } else if (r.kind === 'group') {
        // The outer column of a nested `by`: picked, replaced or dropped.
        const by = /** @type {string[]} */ (Array.isArray(now.by) ? now.by : []);
        const inner = by.length > 1 ? by.slice(1) : by;
        out.by = v ? [String(v)].concat(inner) : inner;
      } else if (r.kind === 'words') {
        out.subtitle = setWords(String(now.subtitle ?? ''), prof.clauses,
                                new Set(/** @type {string[]} */ (v || [])));
      }
    }
    return out;
  }

  /** @param {HTMLElement} el @param {string} label @param {boolean} on
   * @param {(on: boolean) => void} onToggle @param {string} key */
  const box = (el, label, on, onToggle, key) => {
    const h = root.Blockr.checkbox(label, on, onToggle);
    h.input.setAttribute('data-gear-input', key);
    el.appendChild(h.el);
  };

  /** A list of checkboxes, one per [key, label], writing the ticked keys.
   * @param {HTMLElement} el @param {VizRowCtx} ctx @param {[string, string][]} items */
  function checklist(el, ctx, items) {
    const now = new Set(/** @type {string[]} */ (Array.isArray(ctx.value) ? ctx.value : []));
    const list = document.createElement('div');
    list.className = 'vz-checklist';
    el.appendChild(list);
    for (const [k, label] of items) {
      box(list, label, now.has(k), (on) => {
        const next = new Set(now);
        if (on) next.add(k); else next.delete(k);
        ctx.set(items.map(([x]) => x).filter((x) => next.has(x)));
      }, ctx.key + ':' + k);
    }
  }

  /** The control of a row the spec does not have (`gear.control: "profile"`).
   * @param {HTMLElement} el @param {VizRowCtx} ctx */
  function draw(el, ctx) {
    const prof = ctx.profile;
    const r = prof && prof.rows[ctx.key];
    if (!prof || !r) return;
    if (r.kind === 'columns') {
      checklist(el, ctx, (prof.wire.columns || []).map((c) => [c.name, c.label || c.name]));
    } else if (r.kind === 'words') {
      // A setting by its label; its clause in the template says where it goes.
      checklist(el, ctx, prof.clauses.map(([k]) => [k, ctx.labelOf(k)]));
    } else if (r.kind === 'editor') {
      const ed = V.gear.editors[ctx.key];
      if (ed) ed.draw(el, ctx);
    }
  }

  /** The row a title word opens in a block's own gear: `{@parent}`, the rows'
   * grouping, is the group row when there is one. @param {VizProfile | null} prof
   * @param {string} key */
  const alias = (prof, key) => (prof && key === 'parent' && prof.rows.group ? 'group' : key);

  V.gear.more = Object.assign(V.gear.more || {}, { profile: draw });
  V.gear.profile = { build, values, translate, setWords, scriptArg, scriptValue, alias, SV };
})();
// --- core/gear/tray.js
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
// --- core/gear/counts.js
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
// --- core/gear/focus.js
// @ts-check
/**
 * The field being typed in, kept across a re-render of the gear's tray: the
 * engine saves it before it draws the rows again and restores it after.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  V.gear.focus = {
    /** The focused field under `el`, with its text and caret, or null.
     * @param {HTMLElement} el @returns {VizGearFocus | null} */
    save(el) {
      const a = /** @type {HTMLInputElement | null} */ (document.activeElement);
      if (!a || !el.contains(a)) return null;
      const key = a.getAttribute('data-gear-input');
      if (!key) return null;
      const typed = a.tagName === 'INPUT' || a.tagName === 'TEXTAREA';
      return { key, text: typed ? a.value : null,
               start: typed ? a.selectionStart : null, end: typed ? a.selectionEnd : null };
    },
    /** @param {HTMLElement} el @param {VizGearFocus | null} f */
    restore(el, f) {
      if (!f) return;
      const field = /** @type {HTMLInputElement | null} */ (
        el.querySelector('[data-gear-input="' + f.key + '"]'));
      if (!field) return;
      if (f.text != null && field.value !== f.text) {
        field.value = f.text;
        field.dispatchEvent(new Event('input'));
      }
      field.focus();
      if (f.start != null && typeof field.setSelectionRange === 'function') {
        try { field.setSelectionRange(f.start, f.end); } catch (e) { /* not a text field */ }
      }
    }
  };
})();
// --- core/gear/engine.js
// @ts-check
/**
 * The gear engine: one per block view. It draws the gear tray from the
 * block's generated spec (Blockr.viz.spec.<block>), holds the settings the
 * tray shows, and sends a change as `{rev, values}` with only the keys that
 * changed (`<id>_set`, read in R by viz_read_set()).
 *
 * The view mounts it once and hands it every payload:
 *
 *   const gear = scope.own(Blockr.viz.gear.mount(trayEl, { block, id, scope }));
 *   gear.update(payload);
 *
 * Everything it sets up goes through Scopes: the view's for the engine's
 * life, a fresh child per render for the controls of that render.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  V.gear = V.gear || /** @type {VizGearNamespace} */ ({});
  const S = V.gear.sections;
  const C = V.gear.controls;
  const K = V.gear.columnCounts;
  const P = V.gear.profile;

  /** Controls taller than the control line: they take the whole row and
   * grow with their content. @param {string} control */
  const isTall = (control) => ['tiles', 'custom', 'template', 'code', 'script'].includes(control);

  /**
   * How wide a row is in the section's grid (gear.css): tiles, editors,
   * templates, multi-line text and segments of four or more icons (so their
   * words have room) take the whole width; a control with a second one
   * beside it (drill, highlight) two cells. Every other row is one cell.
   * @param {VizRowCtx} ctx @param {boolean} boxed
   */
  const rowClass = (ctx, boxed) => {
    const g = ctx.arg.gear;
    const control = g ? g.control : '';
    if (control === 'code' || control === 'script') return ' vz-row--code vz-row--tall';
    if (isTall(control)) return ' vz-row--wide vz-row--tall';
    if (g && g.icons && (ctx.arg.values || []).length >= 4) return ' vz-row--wide';
    if (control === 'profile') return ' vz-row--wide'; // a block's own checklist or editor
    if ((control === 'toggle' && ctx.arg.type.name !== 'bool') || ctx.arg.type.name === 'named') {
      return ' vz-row--paired';
    }
    return boxed ? ' vz-row--check' : '';
  };

  /** A local edit not yet echoed by R: its value, the value R had, and how
   * many more payloads may still predate it. */
  /** @typedef {{ value: unknown, before: unknown, left: number }} Pending */

  class Gear {
    /** @param {HTMLElement} el @param {VizGearOptions} opts */
    constructor(el, opts) {
      const spec = opts.spec || /** @type {VizSpec | undefined} */ (V.spec[opts.block]);
      if (!spec) throw new Error('blockr.viz: no spec for block "' + opts.block + '"');
      this.el = el;
      this.spec = spec;
      /** The block's spec; `spec` is it, or its own gear's (profile.js, `derived.gear`). */
      this._base = spec;
      /** @type {VizProfile | null} */ this._prof = null;
      this.block = opts.block;
      this.id = opts.id;
      this.scope = opts.scope;
      this.host = opts.host || {};
      this.title = opts.title || 'Settings';
      this.typeKey = opts.typeKey || (spec.type ? 'type' : (spec.layout ? 'layout' : ''));
      /** @type {VizValues} */
      this._cfg = {};
      /** @type {Map<string, Pending>} */
      this._pending = new Map();
      /** @type {VizColumn[]} */
      this._cols = [];
      /** @type {VizTitles} */
      this._titles = {};
      /** The prepare script's controls (`derived.script`), for the words that name them. */
      /** @type {VizScript | null} */
      this._script = null;
      this._rev = 0;
      /** Optional mappings added from the Add menu, shown while unset. */
      /** @type {Set<string>} */
      this._added = new Set();
      /** @type {string | null} */
      this._group = null;
      /** @type {VizScope | null} */
      this._renderScope = null;
      this._drawn = '';
      /** The tray is open; the columns whose counts were asked for; counts arrived. */
      this._open = false;
      this._askedFor = /** @type {VizColumn[] | null} */ (null);
      this._countsRev = 0;
      this.disposed = false;
      K.add(this);
    }

    /** Open, the gear asks R for the columns' distinct counts (counts.js).
     * @param {boolean} on */
    setOpen(on) {
      this._open = on;
      K.ask(this);
    }

    /** R's answer: the counts go onto the columns held (the view's columns are these
     * objects) and the tray is drawn again with them.
     * @param {Record<string, number>} counts */
    counts(counts) {
      if (this.disposed) return;
      K.put(this._cols, counts);
      this._countsRev += 1;
      if (this._key() !== this._drawn) this.render();
    }

    /** @param {VizPayload} p */
    update(p) {
      if (this.disposed) return;
      const cfg = this._config(p);
      for (const [key, pend] of this._pending) {
        const now = cfg[key];
        pend.left -= 1;
        if (C.same(now, pend.value) || !C.same(now, pend.before) || pend.left <= 0) {
          this._pending.delete(key);
        }
      }
      this.prime(p);
      this._rev = p.state_rev || 0;
      K.ask(this);
      if (this._key() !== this._drawn) this.render();
    }

    /** The settings as R sent them, with the local edits on top. */
    values() {
      /** @type {VizValues} */
      const out = Object.assign({}, this._cfg);
      for (const [key, pend] of this._pending) out[key] = pend.value;
      return out;
    }

    /** The settings as the rows show them. An optional column the data
     * lacks reads as unset, as the block draws it: a board's `Subgroup`
     * before one is picked, a column renamed upstream. The state keeps the
     * name, so it applies again once the column is back; a pick replaces it.
     * A required column stays, and its picker names it "(missing)".
     * @param {VizValues} values @returns {VizValues} */
    _live(values) {
      if (!this._cols.length) return values;
      const has = new Set(this._cols.map((c) => c.name));
      let out = values;
      for (const [key, arg] of Object.entries(this.spec)) {
        const v = values[key];
        if (!arg.gear || arg.type.name !== 'column' || !arg.nullable) continue;
        if (typeof v !== 'string' || has.has(v) || (arg.also || []).includes(v)) continue;
        if (this.host.required && this.host.required(key, values)) continue;
        if (out === values) out = Object.assign({}, values);
        out[key] = null;
      }
      return out;
    }

    /** Write settings and send the ones that changed.
     * @param {VizValues} changes */
    set(changes) {
      if (this.disposed) return;
      const now = this.values();
      /** @type {VizValues} */
      const changed = {};
      for (const [key, v] of Object.entries(changes)) {
        if (!this.spec[key] || C.same(now[key], v)) continue;
        changed[key] = v;
        this._pending.set(key, { value: v, before: this._cfg[key], left: 2 });
      }
      if (!Object.keys(changed).length) return;
      // A block's own rows write the settings they stand for; those are held too,
      // for the words and the script band that read them.
      const send = this._prof ? P.translate(this._prof, changed, now) : changed;
      for (const [key, v] of Object.entries(send)) {
        if (!(key in changed)) this._pending.set(key, { value: v, before: this._cfg[key], left: 2 });
      }
      if (!Object.keys(send).length) {
        this.render();
        return;
      }
      const msg = { rev: this._rev, values: send };
      if (this.host.send) this.host.send(msg);
      else V.util.send(this.id, 'set', msg, this.block);
      if (this.host.onChange) this.host.onChange(send, this.values());
      this.render();
    }

    /** What decides the drawing; a payload that leaves it alone keeps the
     * tray as it is, open lists included, R's echo of an edit among them.
     * The rows reach the tray only through the host's choices, so those
     * count, not the data revision a new table layout brings. The titles'
     * arguments, which differ by layout, show only in a template row's
     * warning; the `@` menu reads them when it opens (ctx.titles is live). */
    _key() {
      const values = this.values();
      const host = this.host;
      const t = this._titles;
      const shown = Object.keys(this.spec).filter((k) => this._shown(k, values)).map((k) => [
        k, this.label(k, values), !!(host.required && host.required(k, values)),
        (host.choices && host.choices(k, values, this._cols)) || null,
        /** @type {VizGearField} */ (this.spec[k].gear).control === 'template'
          ? V.titles.unknownTokens(String(values[k] ?? ''), t, this._cols) : null
      ]);
      // The selection is no row: a click moves it and must not rebuild the tray.
      const rest = Object.assign({}, values);
      delete rest.selection;
      return JSON.stringify([rest, this._cols, this._countsRev, [t.title, t.subtitle, t.caption],
                             Array.from(this._added), shown, this._prof ? this._prof.sig : null]);
    }

    /** @param {string} key @param {VizValues} [values] */
    label(key, values) {
      key = P.alias(this._prof, key);
      const vals = values || this.values();
      const own = this.host.label && this.host.label(key, vals);
      if (own) return own;
      const arg = this.spec[key];
      if (arg && arg.gear) return arg.gear.label;
      const words = String(key).replace(/[_.]+/g, ' ').trim();
      return words.charAt(0).toUpperCase() + words.slice(1);
    }

    /** What the title word `{@key}` names, for the gear's titles rows: the prepare
     * script's control when the script declares `key` (see slot()), with the label of the
     * block setting of that name it takes the word from, else the setting.
     * @param {string} key @param {VizValues} [values] @returns {VizWordOf} */
    wordOf(key, values) {
      const vals = values || this.values();
      const sp = this.scriptInput(key);
      if (!sp) return { label: this.label(key, vals), script: false, shadows: null };
      const own = this.host.slot && this.host.slot(key, vals);
      const arg = this.spec[key];
      // The setting as the gear shows it: its label, under its group's heading.
      const g = arg && arg.gear;
      const under = g && g.group && g.group !== g.label ? ' under ' + g.group : '';
      // A setting a block's own gear has no row for is no place to look.
      const hidden = this._prof && !this._prof.keys.has(key);
      const shadows = own ? own.label : g && !hidden ? this.label(key, vals) + under : null;
      return { label: sp.label, script: true, shadows };
    }

    /** The script values a template names that take the word of a block setting with the
     * same name, as one line under the titles field ("" when there are none).
     * @param {string} tpl @param {VizValues} [values] */
    shadowNote(tpl, values) {
      /** @type {string[]} */
      const out = [];
      for (const m of String(tpl).matchAll(/@([A-Za-z_.][A-Za-z0-9_.]*)/g)) {
        const w = this.wordOf(m[1], values);
        const tok = '{@' + m[1] + '}';
        if (!w.script || !w.shadows || out.some((x) => x.startsWith(tok))) continue;
        out.push(tok + " prints the prepare script's " + w.label + ', not the setting ' +
                 w.shadows + ', which stays in the gear.');
      }
      return out.join(' ');
    }

    /** Is the row for `key` drawn now?
     * @param {string} key @param {VizValues} values */
    _shown(key, values) {
      const arg = this.spec[key];
      if (!arg || !arg.gear) return false;
      if (this._prof && !this._prof.keys.has(key)) return false;
      if (arg.gear.control === 'custom' && !this.host.custom) return false;
      if (this.typeKey && !S.applies(arg, values[this.typeKey])) return false;
      if (arg.gear.when && !C.isSet(values[arg.gear.when])) return false;
      return !this.host.visible || this.host.visible(key, values) !== false;
    }

    /** @param {string} key @param {VizValues} values @param {VizScope} scope
     * @returns {VizRowCtx} */
    _ctx(key, values, scope) {
      const required = !!(this.host.required && this.host.required(key, values));
      const gear = this;
      return {
        key, arg: this.spec[key], label: this.label(key, values), value: values[key], values,
        columns: this._cols, required, scope, host: this.host,
        // The latest payload's, as a payload may leave the row drawn.
        get titles() { return gear._titles; },
        labelOf: (k) => this.wordOf(k, values).label,
        wordOf: (k) => this.wordOf(k, values),
        shadowNote: (tpl) => this.shadowNote(tpl, values),
        profile: this._prof,
        script: this._script,
        setValues: (changes) => this.set(changes),
        set: (v) => this.set({ [key]: v })
      };
    }

    /** Show a block's own gear at its tab titled `title`. @param {string} title */
    showTab(title) {
      const g = this._prof && this._prof.groups.find(([, t]) => t === title);
      if (!g || this._group === g[0]) return;
      this._group = g[0];
      this.render();
    }

    render() {
      if (this.disposed) return;
      const focus = V.gear.focus.save(this.el);
      if (this._renderScope) this._renderScope.dispose();
      const rs = this._renderScope = new V.Scope();
      const values = this._live(this.values());
      this._drawn = this._key();
      this.el.textContent = '';
      this.el.setAttribute('role', 'dialog');
      this.el.setAttribute('aria-label', this.title);
      const plan = S.plan(this.spec, (k) => this._shown(k, values),
        (k) => C.isSet(values[k]) || this._added.has(k), this._prof);
      const built = plan.map((p) => {
        const sec = S.sectionEl(p.title);
        const alone = p.keys.length === 1 && !p.adds.length;
        for (const key of p.keys) this._row(sec.body, alone, this._ctx(key, values, rs));
        if (p.adds.length) this._addMenu(sec.body, p.adds, values, rs);
        return { plan: p, el: sec.el, titleEl: sec.titleEl };
      });
      S.assemble(this.el, built, this._group, (g) => { this._group = g; }, this.title,
                 this._prof ? this._prof.groups : undefined);
      V.gear.focus.restore(this.el, focus);
    }

    /** One row: its label over its controls. A checkbox has a label too,
     * with a short verb beside the box. The one row without a label is a
     * tall control alone in its section (the chart type, the table's
     * columns, the prepare script): the section's title names it.
     * @param {HTMLElement} body @param {boolean} alone Whether the row is
     *   its section's only one. @param {VizRowCtx} ctx */
    _row(body, alone, ctx) {
      const g = /** @type {VizGearField} */ (ctx.arg.gear);
      const boxed = g.control === 'toggle' ||
        (g.control === 'segmented' && ctx.arg.type.name === 'bool');
      const labelled = !(alone && isTall(g.control));
      const row = document.createElement('div');
      row.className = 'vz-row' + rowClass(ctx, boxed) + (labelled ? '' : ' vz-row--nolabel');
      row.setAttribute('data-role', ctx.key);
      if (labelled) {
        const head = document.createElement('div');
        head.className = 'vz-row-head';
        const lbl = document.createElement('span');
        lbl.className = 'blockr-label';
        lbl.textContent = ctx.label + (ctx.required ? ' *' : '');
        head.appendChild(lbl);
        row.appendChild(head);
      }
      const controls = document.createElement('div');
      controls.className = 'vz-row-controls';
      row.appendChild(controls);
      body.appendChild(row);
      C.build(controls, ctx);
      // An empty required pick takes blockr.ui's amber cue. A custom editor
      // knows what empty means for its value and sets the cue itself.
      const ui = /** @type {VizUi} */ (root.Blockr);
      if (ctx.required && g.control !== 'custom' && ui.setRequiredEmpty) {
        ui.setRequiredEmpty(row, !C.isSet(ctx.value));
      }
    }

    /** The optional mappings not shown, as a menu that adds a row.
     * @param {HTMLElement} body @param {string[]} keys @param {VizValues} values
     * @param {VizScope} scope */
    _addMenu(body, keys, values, scope) {
      const row = document.createElement('div');
      row.className = 'vz-row vz-add vz-add-mapping';
      body.appendChild(row);
      const add = (/** @type {string} */ key) => {
        if (!key) return;
        this._added.add(key);
        this.render();
      };
      const labels = keys.map((k) => this.label(k, values));
      const inst = /** @type {VizUi} */ (root.Blockr).Select.single(row, {
        options: labels, selected: '', allowEmpty: true, placeholder: '+ Add a mapping',
        label: 'Add a mapping', onChange: (/** @type {string} */ v) => add(keys[labels.indexOf(v)])
      });
      scope.own({ dispose: () => inst.destroy() });
    }

    /** Take what the words in the titles read from a payload (the settings, the columns,
     * the titles and the prepare script's controls) without drawing the tray. A view
     * whose titles are painted before the gear's update (the table's layouts paint them
     * before the gear reads what they draw) primes the gear first; otherwise a word that
     * needs the columns (a column menu) or the current value (a flag) would be plain text,
     * or wrong, until the next payload.
     * @param {VizPayload} p */
    prime(p) {
      if (this.disposed) return;
      this._config(p);
    }

    /** Read a payload: the settings (with a block's own rows' values), the columns,
     * the titles, the prepare script's controls and the block's own gear.
     * @param {VizPayload} p @returns {VizValues} The settings. */
    _config(p) {
      const d = /** @type {any} */ (p.derived || {});
      this._cols = p.columns || [];
      this._titles = /** @type {VizTitles} */ (d.titles || {});
      this._script = d.script || null;
      const cfg = p.config || {};
      if (d.gear) {
        this._prof = P.build(this._base, d.gear, this._script);
        this.spec = this._prof.spec;
        this._cfg = Object.assign({}, cfg, P.values(this._prof, cfg, this._script));
      } else {
        this._prof = null;
        this.spec = this._base;
        this._cfg = cfg;
      }
      return this._cfg;
    }

    /** What a click on the sentence's word for `key` opens: the prepare script's control
     * when the script declares `key` (a template names a script value by its variable,
     * `{@min_share}`), else the block's setting of that name. The declaration wins, as in
     * R's script_title_args(): the word prints the script's value, so it opens that
     * control, and the setting of the same name stays in the gear.
     * @param {string} key @returns {VizSlotOpener | null} */
    slot(key) {
      key = P.alias(this._prof, key);
      if (this.scriptInput(key)) return this.scriptSlot(key);
      const values = this._live(this.values());
      // The host first: a word that names part of a setting (the groups layout's
      // `{@value}` is its first summary's column) or names a setting differently.
      const own = this.host.slot && this.host.slot(key, values);
      if (own) {
        return this._argSlot(key, own.arg, own.value, own.label,
                             (v) => this.set(own.write(v)), values);
      }
      if (this.spec[key]) {
        return this._argSlot(key, this.spec[key], values[key], this.label(key, values),
                             (v) => this.set({ [key]: v }), values);
      }
      return this.scriptSlot(key);
    }

    /** The opener for a word whose setting is `arg`, holding `value`, written by `set`.
     * @param {string} key @param {VizArg} arg @param {unknown} value @param {string} label
     * @param {(v: unknown) => void} set @param {VizValues} values
     * @returns {VizSlotOpener | null} */
    _argSlot(key, arg, value, label, set, values) {
      if (!arg || !arg.gear || arg.gear.control === 'custom') return null;
      const name = arg.type.name;
      const listed = this._listed(key);
      if (name === 'bool') return { kind: 'flag', value: value === true, set, label };
      if ((name === 'number' || name === 'integer') && !listed) {
        const b = arg.bounds || { min: null, max: null, minOpen: false };
        return { kind: 'number', min: b.min == null ? -Infinity : b.min,
                 max: b.max == null ? Infinity : b.max, step: name === 'integer' ? 1 : 0.1,
                 value, set, label };
      }
      if (name === 'limits') return this._pairSlot(key, arg, value, label, set);
      if (name === 'ticks') return V.gear.ticksSlot(this.host, key, value, label, set, values);
      if ((name === 'numbers' || name === 'values') && !listed) {
        return { kind: 'entry', value, set, label,
                 toText: (v) => C.toText(arg, v), fromText: (s) => C.fromText(arg, s) };
      }
      if (['map', 'records', 'records_by', 'named'].includes(name)) return null;
      // Free text stays in the gear; a string picked from a list is a menu.
      if ((name === 'string' || name === 'template') && arg.gear.control !== 'select') return null;
      const ctx = Object.assign(this._ctx(key, values, this.scope), { arg, value, label, set });
      const empty = name === 'columns' || name === 'enums' ? null : C.emptyChoice(ctx);
      let options = /** @type {VizChoice[]} */ (empty ? [empty] : []).concat(C.options(ctx));
      if (listed) {
        options = options.filter((o) => o.value == null || listed.includes(String(o.value)) ||
          C.same(o.value, value));
      }
      if (!options.length) return null;
      return { kind: 'menu', options, selected: value ?? null,
               multi: name === 'columns' || name === 'enums', set, label };
    }

    /** Axis limits on the face: one word per end. An end typed over keeps the
     * other as the gear holds it now; a pair whose from is not below its to is
     * not sent (R refuses it).
     * @param {string} key @param {VizArg} arg @param {unknown} value @param {string} label
     * @param {(v: unknown) => void} set @returns {VizSlotOpener} */
    _pairSlot(key, arg, value, label, set) {
      const now = (/** @type {unknown} */ v) => (Array.isArray(v) ? v : [null, null]);
      const pair = now(value);
      const num = (/** @type {unknown} */ v) => (typeof v === 'number' ? v : Date.parse(String(v)));
      const end = (/** @type {number} */ i) => /** @type {VizEndOpener} */ ({
        kind: 'entry', value: pair[i], label: label + (i ? ' to' : ' from'),
        toText: (v) => (v == null ? '' : String(v)),
        fromText: (t) => { const p = /** @type {unknown[] | null} */ (C.fromText(arg, t)); return p ? p[0] : null; },
        set: (v) => {
          const next = now(this.values()[key]).slice();
          next[i] = v;
          if (next[0] != null && next[1] != null && !(num(next[0]) < num(next[1]))) return;
          if (V.gear.limitsNote(this.values(), key, next)) return;
          set(next.every((x) => x == null) ? null : next);
        }
      });
      return { kind: 'pair', value, ends: [end(0), end(1)], set, label };
    }

    /** The prepare script's working declaration of `name`, if the block has a script.
     * A declaration that failed is none: its name stays the setting's, as in R.
     * @param {string} name @returns {VizScriptControl | null} */
    scriptInput(name) {
      if (!this._script || !this.spec.script_values) return null;
      const sp = /** @type {VizScriptControl | undefined} */ (
        (this._script.inputs || []).find((x) => x && x.name === name && x.kind !== 'error'));
      return sp || null;
    }

    /**
     * The prepare script's control for the variable `name`, as a word opens it: a flag
     * toggles, a number or a text is typed over, a choice opens its list. It writes
     * `script_values[name]`, as the script band's own control does. A block setting of
     * the same name does not stop it (see slot()). Null when the script declares no
     * working control of that name, or when there is no script.
     * @param {string} name @returns {VizSlotOpener | null}
     */
    scriptSlot(name) {
      const sp = this.scriptInput(name);
      if (!sp || !this._script) return null;
      const held = /** @type {Record<string, unknown>} */ (this.values().script_values || {});
      const sent = /** @type {Record<string, unknown>} */ (
        Array.isArray(this._script.values) ? {} : this._script.values || {});
      // A local edit R has not echoed yet, else the value R resolved for the control.
      // R's echo of `script_values` is no such edit: it sends a map's values as
      // arrays (wire_map()), and a held `["Patients"]` matches no option of a
      // single choice, so its menu would bold nothing.
      const pend = this._pending.get('script_values');
      const mine = /** @type {Record<string, unknown>} */ ((pend && pend.value) || {});
      let value = Object.prototype.hasOwnProperty.call(mine, name) ? mine[name] : sent[sp.key];
      if (sp.kind !== 'multi' && Array.isArray(value)) value = value[0];
      const set = (/** @type {unknown} */ v) => this.set({
        script_values: Object.assign({}, held, { [name]: v })
      });
      const label = sp.label;
      const text = { toText: (/** @type {unknown} */ v) => (v == null ? '' : String(v)),
                     fromText: (/** @type {string} */ s) => s };
      switch (sp.kind) {
        case 'segmented':
          return { kind: 'flag', value: value === true || value === 'on', set, label };
        case 'number': {
          const num = (/** @type {unknown} */ x) => (typeof x === 'number' ? x : null);
          const lo = num(sp.min), hi = num(sp.max);
          return { kind: 'number', min: lo == null ? -Infinity : lo,
                   max: hi == null ? Infinity : hi, step: sp.step || 1, value, set, label };
        }
        case 'select':
        case 'multi': {
          const options = /** @type {string[]} */ (sp.options || [])
            .map((o) => ({ value: String(o), label: String(o) }));
          if (!options.length) return null;
          return { kind: 'menu', options, selected: value ?? null, multi: sp.kind === 'multi',
                   set, label };
        }
        default:
          return { kind: 'entry', value, set, label, ...text };
      }
    }

    /** The script controls a word in the titles opens, by variable name: the script band
     * leaves them out, so one setting has one control on the face.
     * @returns {Set<string>} */
    scriptWords() {
      /** @type {Set<string>} */
      const out = new Set();
      for (const arg of V.titles.namedArgs(this._titles)) {
        if (this.scriptSlot(arg)) out.add(arg);
      }
      // A block's own gear holds its script's values, as rows or in its own editors.
      if (this._prof && this._script) {
        for (const sp of this._script.inputs || []) if (sp && sp.name) out.add(sp.name);
      }
      return out;
    }

    /** The values a sentence lists for a word (`{@func in mean, median}`).
     * @param {string} key @returns {string[] | null} */
    _listed(key) {
      const all = this._titles.choices;
      const v = all && all[key];
      if (v == null) return null;
      const list = (Array.isArray(v) ? v : [v]).map(String).filter((x) => x);
      return list.length ? list : null;
    }

    dispose() {
      if (this.disposed) return;
      this.disposed = true;
      K.remove(this);
      if (this._renderScope) this._renderScope.dispose();
      this._renderScope = null;
      this._pending.clear();
      this.el.textContent = '';
    }
  }

  /**
   * Mount a gear into the tray element `el`.
   * @param {HTMLElement} el @param {VizGearOptions} opts @returns {VizGear}
   */
  V.gear.mount = (el, opts) => new Gear(el, opts);
})();
// --- core/frame.js
// @ts-check
/**
 * The frame every block view draws in: a header with the titles on the left
 * and the tools (the download control, a view's own tools) and the gear on
 * the right, the gear's tray under the header, the body the view draws
 * into, the caption, and a footer with a count and the status line.
 *
 * The status line says what the selection holds ("Filtered: Arm = Placebo")
 * with a button that clears it, a note on the picture after it, and for a
 * moment after a drill sent to another block the receipt of that drill
 * instead. The frame owns the words' place and timing; the view writes them.
 *
 * The header's layout is in frame.css (`.vz-head`); in a narrow block the
 * tools take their own row.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  // How long the footer holds a drill receipt, and how long it takes to
  // leave. The receipt's age drives the animation, so a repainted footer
  // resumes it. The grace is a backstop for an off-screen block, where
  // animationend never fires.
  const RECEIPT_HOLD_MS = 1400;
  const RECEIPT_FADE_MS = 1100;
  const RECEIPT_GRACE_MS = 600;

  /**
   * @param {string} tag
   * @param {string} cls
   * @param {HTMLElement} [parent]
   */
  function el(tag, cls, parent) {
    const e = document.createElement(tag);
    e.className = cls;
    if (parent) parent.appendChild(e);
    return e;
  }

  /**
   * Say why there is nothing to draw in `parent`, or take the message away.
   * The message is the parent's first child.
   * @param {HTMLElement} parent
   * @param {VizEmpty | null | undefined} e
   * @returns {boolean} Whether a message is shown.
   */
  function message(parent, e) {
    let msg = /** @type {HTMLElement | null} */ (parent.querySelector(':scope > .vz-empty'));
    if (!e) {
      if (msg) msg.remove();
      return false;
    }
    if (!msg) {
      msg = el('div', 'vz-empty');
      el('p', 'vz-empty-text', msg);
      parent.prepend(msg);
    }
    msg.setAttribute('data-kind', e.kind || 'empty');
    /** @type {HTMLElement} */ (msg.firstElementChild).textContent = e.message || '';
    return true;
  }

  /** A title band as R sends it: a band, a plain string, or nothing.
   * @param {VizTitleBand | string | null | undefined} b @returns {VizTitleBand | null} */
  const band = (b) => (typeof b === 'string' ? { text: b } : b || null);

  /** A band shows when it has text, or settings to offer where a clause dropped.
   * @param {VizTitleBand | null} b */
  const shows = (b) => !!b && (!!b.text || (Array.isArray(b.offers) && b.offers.length > 0));

  class Frame {
    /**
     * @param {HTMLElement} host The output element; the frame fills it.
     * @param {VizScope} scope
     * @param {{ cls?: string }} [opts] `cls`: the block's own classes on the frame.
     */
    constructor(host, scope, opts = {}) {
      this.id = host.id;
      this.scope = scope;
      this.el = el('div', 'vz-frame' + (opts.cls ? ' ' + opts.cls : ''));
      host.replaceChildren(this.el);
      scope.own({ dispose: () => this.el.remove() });
      this.head = el('div', 'blockr-gear-header vz-head', this.el);
      this.titleBox = el('div', 'vz-head__titles', this.head);
      this.titleEl = el('div', 'vz-title', this.titleBox);
      this.subEl = el('div', 'vz-subtitle', this.titleBox);
      this.tools = el('div', 'vz-head__tools', this.head);
      this.downloadSlot = el('div', 'vz-download', this.tools);
      el('div', 'vz-busy-line', this.el).setAttribute('aria-live', 'polite');
      this.body = el('div', 'vz-body', this.el);
      this.capEl = el('div', 'vz-caption', this.el);
      this.foot = el('div', 'vz-foot', this.el);
      this.countEl = el('span', 'vz-count', this.foot);
      this.countEl.setAttribute('aria-live', 'polite');
      this.statusEl = el('div', 'vz-status', this.foot);
      // blockr.ui's quiet button, smallest size.
      this.clearBtn = /** @type {HTMLButtonElement} */ (el('button',
        'vz-status-clear blockr-btn blockr-btn--quiet blockr-btn--xs'));
      this.clearBtn.type = 'button';
      this.clearBtn.textContent = 'Clear';
      this.clearBtn.setAttribute('aria-label', 'Clear filter');
      scope.on(this.clearBtn, 'click', () => { if (this._line.onClear) this._line.onClear(); });
      /** The resting line. @type {{ text: string | null, onClear: (() => void) | null }} */
      this._line = { text: null, onClear: null };
      /** @type {string | null} */
      this._note = null;
      /** The drill receipt and when it was taken. @type {{ text: string, at: number } | null} */
      this._receipt = null;
      /** @type {(() => void) | null} */
      this._receiptTimer = null;
      this._returning = false;
      /** Title words that open the gear; none without a gear. @type {VizSlots | null} */
      this.slots = null;
      /** @type {VizGear | null} */
      this.gear = null;
      this.openGear = () => {};
      this.titles(null);
      this.downloads(null);
      this._paintStatus();
    }

    /**
     * Mount the gear in a tray under the header, and the title words that
     * open its controls.
     * @param {{ block: string, typeKey: string, title: string, host: VizGearHost }} o
     * @returns {VizGear}
     */
    mountGear(o) {
      const t = V.gear.tray(this.head, this.scope, o.title);
      this.tray = t;
      this.openGear = () => t.set(true);
      this.gear = this.scope.own(V.gear.mount(t.band, {
        block: o.block, id: this.id, scope: this.scope, typeKey: o.typeKey,
        title: o.title, host: o.host
      }));
      // The gear asks R for what it needs of the columns once its tray is open.
      this.scope.mutate(t.button, () => {
        if (this.gear) this.gear.setOpen(t.button.getAttribute('aria-expanded') === 'true');
      }, { attributes: true, attributeFilter: ['aria-expanded'] });
      this.slots = this.scope.own(V.titles.slots({
        gear: this.gear, scope: this.scope, openGear: this.openGear
      }));
      return this.gear;
    }

    /**
     * The title, subtitle and caption, each hidden while empty. With a gear,
     * words that name an argument open its control.
     * @param {VizTitles | Record<string, VizTitleBand | string | null> | null | undefined} t
     */
    titles(t) {
      const x = /** @type {Record<string, VizTitleBand | string | null | undefined>} */ (t || {});
      const [title, sub, cap] = [band(x.title), band(x.subtitle), band(x.caption)];
      /** @param {HTMLElement} e @param {VizTitleBand | null} b */
      const paint = (e, b) => {
        if (this.slots) this.slots.paint(e, b);
        else e.textContent = (b && b.text) || '';
        e.hidden = !shows(b);
      };
      paint(this.titleEl, title);
      paint(this.subEl, sub);
      paint(this.capEl, cap);
      this.titleBox.hidden = this.titleEl.hidden && this.subEl.hidden;
    }

    /**
     * The download control (core/download.js) with the formats R offers,
     * hidden without any. `onOpen` runs when the reader reaches for it; a
     * promise it returns (the picture on its way to R) holds a click on a
     * format until it settles.
     * @param {VizDownload[] | null | undefined} list
     * @param {() => unknown} [onOpen]
     */
    downloads(list, onOpen) {
      const offer = Array.isArray(list) ? list : [];
      this.downloadSlot.hidden = offer.length === 0;
      V.downloads(this.downloadSlot, offer, this.scope, onOpen ? { onOpen } : {});
    }

    /**
     * Say in the body why there is nothing to draw, or take the message away.
     * @param {VizEmpty | null | undefined} e
     * @returns {boolean} Whether a message is shown.
     */
    empty(e) {
      return message(this.body, e);
    }

    /** @param {string} text The count left of the status line ("86 rows"). */
    count(text) {
      this.countEl.textContent = text;
    }

    /**
     * The resting status line: what the selection holds, with a clear
     * button when `onClear` is given; null for none.
     * @param {string | null} text
     * @param {(() => void) | null} [onClear]
     */
    status(text, onClear) {
      this._line = { text: text || null, onClear: onClear || null };
      this._paintStatus();
    }

    /** A note after the line, warning about the picture; null for none.
     * @param {string | null} text */
    note(text) {
      this._note = text || null;
      this._paintStatus();
    }

    /**
     * The receipt of a drill sent to another block: it takes the line's place
     * for a moment and fades.
     * @param {string} text
     */
    flash(text) {
      this._receipt = { text, at: performance.now() };
      this._paintStatus();
      if (this._receiptTimer) this._receiptTimer();
      this._receiptTimer = this.scope.timeout(() => this._dropReceipt(),
        RECEIPT_HOLD_MS + RECEIPT_FADE_MS + RECEIPT_GRACE_MS);
    }

    _dropReceipt() {
      if (!this._receipt) return;
      this._receipt = null;
      this._returning = true;
      this._paintStatus();
      this._returning = false;
    }

    // Repainted whole on every change. A live receipt wins the line.
    _paintStatus() {
      const bar = this.statusEl;
      bar.textContent = '';
      this.clearBtn.hidden = true;
      if (this._receipt) {
        const age = performance.now() - this._receipt.at;
        if (age < RECEIPT_HOLD_MS + RECEIPT_FADE_MS) {
          const rec = el('span', 'vz-status-text vz-status-receipt', bar);
          rec.textContent = this._receipt.text;
          // A negative delay resumes the fade where the receipt is.
          rec.style.animation = `vz-receipt-out ${RECEIPT_FADE_MS}ms linear ` +
            `${RECEIPT_HOLD_MS - age}ms both`;
          this.scope.on(rec, 'animationend', () => this._dropReceipt());
          return;
        }
        this._receipt = null;
      }
      const line = this._line;
      if (line.text) {
        el('span', 'vz-status-text' + (this._returning ? ' vz-status-returning' : ''), bar)
          .textContent = line.text;
        if (line.onClear) {
          bar.appendChild(this.clearBtn);
          this.clearBtn.hidden = false;
        }
      }
      if (this._note) el('span', 'vz-status-note', bar).textContent = this._note;
    }

    /** End R's busy cue once the view is on screen: two frames, so the
     * browser has painted. */
    busyDone() {
      const busy = root.Blockr.busy;
      if (!busy || !this.id) return;
      this.scope.frame(() => this.scope.frame(() => busy.stop(this.id)));
    }
  }

  V.Frame = Object.assign(Frame, { message });
})();
// --- core/data-view.js
// @ts-check
/**
 * A view's data as a table under it, after Power BI's "Show as a table". A
 * tool in the frame's header, left of the download control, opens it. Its
 * tabs, each with its count: Summary, one row per mark, and Rows, the rows
 * behind the selection ("Rows 40 of 240"). A view without a Summary (a
 * chart that draws rows) shows the rows and their count alone. R builds
 * both tables (R/chart-data-view.R); the table view (table/view.js) draws
 * them.
 *
 * The reader's choice goes to R as `<id>_data` ({on, tab, n}). While it is
 * on, every payload carries `derived.data_view`: the counts and the table
 * view's payload for the tab that shows. The table's rows come once per
 * data revision and are kept here; a payload without rows this view does
 * not hold (a re-mounted block) asks R again with a new request count `n`.
 * Shiny keeps the input, so a re-mounted block opens the table again from
 * its first payload. Nothing is saved with the board.
 *
 * A row click on Summary goes to R as `<id>-data_select` and selects that
 * mark; a selected mark lights its row.
 *
 * Rows is blockr.ui's table preview, the one behind a block's eye icon: the
 * Shiny output `<id>-data_rows`, bound when the tab first shows. Shiny
 * leaves it alone while the tab is hidden.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  const ICON = '<svg width="14" height="14" viewBox="0 0 16 16" aria-hidden="true" ' +
    'fill="none" stroke="currentColor" stroke-width="1.5" stroke-linejoin="round">' +
    '<rect x="2" y="2.5" width="12" height="11" rx="1.2"/>' +
    '<path d="M2 6.2 H14 M2 9.8 H14 M6.5 6.2 V13.5"/></svg>';

  /** One request count for the page, so a re-mounted view never repeats
   * the count R last answered. */
  let requests = 0;

  /**
   * @param {string} tag @param {string} cls @param {HTMLElement} [parent]
   */
  function el(tag, cls, parent) {
    const e = document.createElement(tag);
    e.className = cls;
    if (parent) parent.appendChild(e);
    return e;
  }

  /** "40 of 240", or "240" when nothing is filtered.
   * @param {VizDataViewState} dv */
  function rowsCount(dv) {
    if (dv.rows == null) return '';
    return dv.total != null && dv.total !== dv.rows ? dv.rows + ' of ' + dv.total : String(dv.rows);
  }

  class DataView {
    /**
     * @param {VizFrame} frame The view's frame: the tool goes in its header,
     *   the table at the end of its body.
     * @param {VizScope} scope
     * @param {{ onToggle?: () => void }} [opts] `onToggle`: the view's room
     *   changed (the chart resizes).
     */
    constructor(frame, scope, opts = {}) {
      this.frame = frame;
      this.scope = scope;
      this.onToggle = opts.onToggle || (() => {});
      this.on = false;
      /** @type {'summary' | 'rows'} */
      this.tab = 'summary';
      /** What the reader asked R for and R has not answered.
       * @type {{ on: boolean, tab: string } | null} */
      this.pending = null;
      /** @type {VizDataViewState | null} */
      this.state = null;
      /** The table's rows of the last data revision R sent.
       * @type {{ rev: number, data: unknown } | null} */
      this.rows = null;
      /** The data revision rows were asked for, to ask once. @type {number | null} */
      this.asked = null;
      this.tool = /** @type {HTMLButtonElement} */ (el('button', 'blockr-tool vz-data-tool'));
      this.tool.type = 'button';
      this.tool.innerHTML = ICON;
      frame.tools.insertBefore(this.tool, frame.downloadSlot);
      scope.on(this.tool, 'click', () => this.toggle());
      this.panel = el('div', 'vz-data', frame.body);
      this.bar = el('div', 'vz-data__bar', this.panel);
      this.host = el('div', 'vz-data__table', this.panel);
      this.host.id = frame.id + '-data';
      /** The Rows tab's output, bound on first show. @type {HTMLElement | null} */
      this.out = null;
      /** The table view, built when a table first arrives.
       * @type {VizView | null} */
      this.table = null;
      this._label();
    }

    _label() {
      const t = this.on ? 'Hide table' : 'Show as table';
      this.tool.setAttribute('aria-label', t);
      this.tool.setAttribute('data-blockr-tooltip', t);
      this.tool.setAttribute('aria-pressed', String(this.on));
    }

    /** @param {boolean} on */
    _show(on) {
      if (on === this.on) return;
      this.on = on;
      this.frame.el.classList.toggle('vz-data-open', on);
      this._label();
      this.onToggle();
    }

    _send() {
      this.pending = { on: this.on, tab: this.tab };
      V.util.send(this.frame.id, 'data', { on: this.on, tab: this.tab, n: ++requests });
    }

    /** Open or close the table. */
    toggle() {
      this._show(!this.on);
      this._send();
    }

    /** Show a tab; R sends its table. @param {'summary' | 'rows'} tab */
    pick(tab) {
      if (tab === this.tab) return;
      this.tab = tab;
      this._bar();
      this._send();
    }

    /**
     * R's `derived.data_view`, or none while the view is closed. A payload
     * that does not answer the reader's last request yet is skipped: R sends
     * the answer next.
     * @param {VizDataViewState | null | undefined} dv
     */
    update(dv) {
      const p = this.pending;
      if (p) {
        const answers = p.on ? !!dv && (dv.tab === p.tab || dv.summary == null) : !dv;
        if (!answers) return;
        this.pending = null;
      }
      if (!dv) {
        this._show(false);
        return;
      }
      this._show(true);
      this.state = dv;
      this.tab = dv.tab;
      this._bar();
      const rows = dv.tab === 'rows';
      this.host.hidden = rows;
      if (rows) this._rows();
      else if (this.out) this.out.hidden = true;
      if (!rows && dv.table) this._draw(dv.table);
    }

    /** Show the Rows tab's output; bind it the first time. */
    _rows() {
      const shiny = /** @type {any} */ (root).Shiny;
      if (!this.out) {
        const out = el('div', 'vz-data__rows shiny-html-output', this.panel);
        out.id = this.frame.id + '-data_rows';
        this.out = out;
        if (shiny && shiny.bindAll) {
          shiny.bindAll(this.panel);
          this.scope.own({ dispose: () => shiny.unbindAll(out) });
        }
      }
      this.out.hidden = false;
      // Shiny resumes an output that becomes visible on its next check.
      if (shiny && shiny.bindAll) root.jQuery(this.out).trigger('shown');
    }

    /** The table, with the rows of its data revision; without them, ask R.
     * @param {VizPayload} p */
    _draw(p) {
      const T = V.table;
      if (!T) return;
      if (p.data != null) {
        this.rows = { rev: p.data_rev, data: p.data };
      } else if (this.rows && this.rows.rev === p.data_rev) {
        p = Object.assign({}, p, { data: this.rows.data });
      } else {
        if (this.asked !== p.data_rev) {
          this.asked = p.data_rev;
          this._send();
        }
        return;
      }
      this.asked = null;
      if (!this.table) {
        const View = /** @type {new (el: HTMLElement, scope: VizScope, o: object) => VizView} */ (
          /** @type {unknown} */ (T.View));
        const scope = this.scope.own(new V.Scope());
        this.table = scope.own(new View(this.host, scope, { gear: false, still: false }));
      }
      this.table.render(p);
    }

    // The tabs with their counts; with no Summary, the count of rows alone.
    _bar() {
      const dv = this.state;
      this.bar.textContent = '';
      if (!dv) return;
      if (dv.summary == null) {
        el('span', 'vz-data__meta', this.bar).textContent = rowsCount(dv) + ' rows';
        return;
      }
      const opts = [
        { value: 'summary', label: 'Summary', count: String(dv.summary) },
        { value: 'rows', label: 'Rows', count: rowsCount(dv) }
      ];
      const ui = /** @type {VizUi} */ (root.Blockr);
      const seg = ui.segmented(opts, this.tab,
        (/** @type {string} */ v) => this.pick(v === 'rows' ? 'rows' : 'summary'),
        { size: 'xs', label: 'Table' });
      seg.el.querySelectorAll('.blockr-segmented__seg').forEach((b, i) => {
        if (opts[i].count) el('span', 'vz-data__n', /** @type {HTMLElement} */ (b)).textContent = opts[i].count;
      });
      this.bar.appendChild(seg.el);
    }
  }

  V.DataView = DataView;
})();
// --- core/script-band.js
// @ts-check
/**
 * The prepare script's band above a view (`derived.script`). One control per
 * value the script declares, drawn by the gear's own control builder, and
 * "Script failed: ..." when the script threw. A change writes
 * `script_values`, keyed by the variable's name, through the gear, which
 * holds an edit R has not echoed yet.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /** @param {string} tag @param {string} cls @param {HTMLElement} parent */
  const el = (tag, cls, parent) => {
    const e = document.createElement(tag);
    e.className = cls;
    parent.appendChild(e);
    return e;
  };

  /** A declared control as a spec argument the gear can draw (R's dd_script_roles(): kind select,
   * multi, segmented, number, date, text). @param {VizScriptControl} sp @returns {VizArg} */
  const argOf = (sp) => {
    const gear = { label: sp.label };
    const bounds = { min: sp.min ?? null, max: sp.max ?? null, minOpen: false };
    switch (sp.kind) {
      case 'select': return /** @type {VizArg} */ ({ type: { name: 'enum' }, values: sp.options || [],
                                                     gear: { ...gear, control: 'select' } });
      case 'multi': return /** @type {VizArg} */ ({ type: { name: 'enums' }, values: sp.options || [],
                                                    gear: { ...gear, control: 'select' } });
      case 'segmented': return /** @type {VizArg} */ ({ type: { name: 'bool' },
                                                        gear: { ...gear, control: 'toggle' } });
      case 'number': return /** @type {VizArg} */ ({ type: { name: 'number' }, bounds,
                                                     gear: { ...gear, control: 'number' } });
      default: return /** @type {VizArg} */ ({ type: { name: 'string' },
                                               gear: { ...gear, control: 'text' } });
    }
  };

  /** The control's value as R holds it for the band ("on"/"off" for a flag).
   * @param {VizScriptControl} sp @param {unknown} v */
  const valueOf = (sp, v) => (sp.kind === 'segmented' ? v === 'on' || v === true : v);

  /**
   * The band, appended to `parent` and hidden while the script declares
   * nothing and did not fail.
   * @param {HTMLElement} parent @param {VizScope} scope @param {VizGear} gear
   * @returns {VizScriptBand}
   */
  V.scriptBand = (parent, scope, gear) => {
    const band = el('div', 'vz-script-band', parent);
    band.hidden = true;
    /** @type {string | null} */
    let last = null;
    /** Owns the controls of the band as drawn. @type {VizScope | null} */
    let drawn = null;

    /** @param {VizScriptControl} sp @param {unknown} value @param {VizScope} s */
    const control = (sp, value, s) => {
      const row = el('div', 'vz-row vz-script-band-row', band);
      row.setAttribute('data-role', sp.key);
      const arg = argOf(sp);
      if (arg.gear?.control !== 'toggle') {
        el('span', 'blockr-label', el('div', 'vz-row-head', row)).textContent = sp.label;
      }
      const set = (/** @type {unknown} */ v) => gear.set({
        script_values: { .../** @type {object} */ (gear.values().script_values || {}), [sp.name]: v }
      });
      V.gear.controls.build(el('div', 'vz-row-controls', row), {
        key: sp.key, arg, label: sp.label, value: valueOf(sp, value), values: {},
        columns: [], required: true, scope: s, host: {}, titles: {},
        labelOf: (/** @type {string} */ k) => k, set
      });
    };

    // A declaration that could not become a control, named with its reason.
    /** @param {VizScriptFailed} sp */
    const failed = (sp) => {
      const row = el('div', 'vz-row vz-script-band-row vz-script-input-error', band);
      el('span', 'blockr-label', el('div', 'vz-row-head', row)).textContent = sp.label;
      el('span', 'vz-help', row).textContent = sp.error || 'no control';
    };

    /** Redrawn only when the script's inputs, values or error changed, or the words that
     * stand in for its controls. A control a word in the titles opens is left out: the
     * word is its control (`{@min_share}` in the subtitle), and two live copies of one
     * value on one face would be one too many. Call it after the gear's update, which
     * carries the titles the words come from.
     * @param {VizScript | null | undefined} script */
    const update = (script) => {
      const s = /** @type {Partial<VizScript>} */ (script || {});
      const words = gear.scriptWords ? gear.scriptWords() : new Set();
      const key = JSON.stringify([s, Array.from(words).sort()]);
      if (key === last) return;
      last = key;
      if (drawn) drawn.dispose();
      const own = drawn = scope.own(new V.Scope());
      band.textContent = '';
      const inputs = (Array.isArray(s.inputs) ? s.inputs : [])
        .filter((sp) => sp.kind === 'error' || !words.has(sp.name));
      const values = /** @type {Record<string, unknown>} */ (s.values || {});
      for (const sp of inputs) {
        if (sp.kind === 'error') failed(sp);
        else control(sp, values[sp.key], own);
      }
      if (s.error) {
        el('div', 'vz-row vz-script-band-row vz-script-failed', band).textContent =
          'Script failed: ' + s.error;
      }
      band.hidden = !(inputs.length || s.error);
    };

    return { el: band, update };
  };
})();
// --- spec/chart.js
// Generated by tools/gen-spec.R from the R spec of the chart block. Do not edit.
// @ts-check
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || {});
  V.spec = V.spec || {};
  V.spec.chart = {
    "type": {
      "type": {
        "name": "enum"
      },
      "default": "bar",
      "values": ["bar", "histogram", "waterfall", "pie", "treemap", "boxplot", "pointrange", "scatter", "line", "area", "band", "gantt"],
      "nullable": false,
      "gear": {
        "group": "Chart type",
        "control": "tiles",
        "label": "Chart type",
        "labels": ["Bar", "Histogram", "Waterfall", "Pie", "Treemap", "Boxplot", "Pointrange", "Scatter", "Line", "Area", "Band", "Gantt"]
      },
      "slot": true,
      "ctrl": true
    },
    "category": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "applies": {
        "only": ["bar", "waterfall", "pie", "treemap", "boxplot", "pointrange"]
      },
      "gear": {
        "group": "Mapping",
        "control": "column",
        "label": "Category"
      },
      "slot": true,
      "ctrl": true
    },
    "value": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "applies": {
        "only": ["bar", "waterfall", "pie", "treemap", "boxplot", "pointrange"]
      },
      "gear": {
        "group": "Mapping",
        "control": "column",
        "label": "Value"
      },
      "slot": true,
      "ctrl": true
    },
    "func": {
      "type": {
        "name": "enum"
      },
      "default": "count",
      "values": ["count", "count_distinct", "pct_distinct", "mean", "median", "sum", "min", "max", "identity"],
      "nullable": false,
      "applies": {
        "only": ["bar", "waterfall", "pie", "treemap"]
      },
      "gear": {
        "group": "Mapping",
        "control": "select",
        "label": "Aggregate",
        "labels": ["Count", "Count distinct", "% of panel", "Mean", "Median", "Sum", "Min", "Max", "None (as is)"]
      },
      "slot": true,
      "ctrl": true
    },
    "x": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "applies": {
        "only": ["scatter", "line", "area", "band", "histogram", "gantt"]
      },
      "gear": {
        "group": "Mapping",
        "control": "column",
        "label": "X"
      },
      "slot": true,
      "ctrl": true
    },
    "xend": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "applies": {
        "only": ["gantt"]
      },
      "gear": {
        "group": "Mapping",
        "control": "column",
        "label": "X end"
      },
      "slot": true,
      "ctrl": true
    },
    "open": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "applies": {
        "only": ["gantt"]
      },
      "gear": {
        "group": "Add mapping",
        "control": "column",
        "label": "Open end"
      },
      "slot": true,
      "ctrl": true
    },
    "y": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "applies": {
        "only": ["scatter", "line", "area", "band", "gantt"]
      },
      "gear": {
        "group": "Mapping",
        "control": "column",
        "label": "Y"
      },
      "slot": true,
      "ctrl": true
    },
    "color": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "applies": {
        "except": ["waterfall", "pie", "treemap"]
      },
      "gear": {
        "group": "Add mapping",
        "control": "column",
        "label": "Color"
      },
      "slot": true,
      "ctrl": true
    },
    "facet": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "gear": {
        "group": "Add mapping",
        "control": "column",
        "label": "Facet"
      },
      "slot": true,
      "ctrl": true
    },
    "facet_row": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "gear": {
        "group": "Add mapping",
        "control": "column",
        "label": "Facet rows"
      },
      "slot": true,
      "ctrl": true
    },
    "series": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "applies": {
        "only": ["scatter", "line", "area", "band", "gantt"]
      },
      "gear": {
        "group": "Add mapping",
        "control": "column",
        "label": "Series"
      },
      "slot": true,
      "ctrl": true
    },
    "label": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "applies": {
        "only": ["bar", "pie", "treemap", "scatter", "line", "area", "gantt"]
      },
      "gear": {
        "group": "Add mapping",
        "control": "column",
        "label": "Label"
      },
      "slot": true,
      "ctrl": true
    },
    "orientation": {
      "type": {
        "name": "enum"
      },
      "values": ["horizontal", "vertical"],
      "nullable": true,
      "applies": {
        "only": ["bar", "boxplot", "pointrange"]
      },
      "gear": {
        "group": "Look",
        "control": "segmented",
        "label": "Orientation",
        "labels": ["Horizontal", "Vertical"],
        "icons": true
      },
      "slot": true,
      "ctrl": true
    },
    "position": {
      "type": {
        "name": "enum"
      },
      "default": "stack",
      "values": ["stack", "dodge", "fill"],
      "nullable": false,
      "applies": {
        "only": ["bar", "area", "histogram"]
      },
      "gear": {
        "group": "Look",
        "control": "segmented",
        "label": "Stacking",
        "labels": ["Stacked", "Side by side", "100%"],
        "when": "color",
        "icons": true
      },
      "slot": true,
      "ctrl": true
    },
    "sort_by": {
      "type": {
        "name": "string"
      },
      "nullable": true,
      "applies": {
        "only": ["bar", "waterfall", "pie", "treemap", "boxplot", "pointrange", "gantt"]
      },
      "gear": {
        "group": "Order",
        "control": "select",
        "label": "Sort"
      },
      "slot": true,
      "ctrl": true
    },
    "sort_dir": {
      "type": {
        "name": "enum"
      },
      "values": ["asc", "desc"],
      "nullable": true,
      "applies": {
        "only": ["bar", "waterfall", "pie", "treemap", "boxplot", "pointrange", "gantt"]
      },
      "gear": {
        "group": "Order",
        "control": "segmented",
        "label": "Direction",
        "labels": ["Ascending", "Descending"],
        "icons": true
      },
      "slot": true,
      "ctrl": true
    },
    "value_labels": {
      "type": {
        "name": "bool"
      },
      "default": false,
      "nullable": false,
      "applies": {
        "only": ["bar", "waterfall", "histogram"]
      },
      "gear": {
        "group": "Numbers",
        "control": "segmented",
        "label": "Value labels",
        "labels": ["Show", "Off"]
      },
      "slot": true,
      "ctrl": true
    },
    "format": {
      "type": {
        "name": "format"
      },
      "default": "number",
      "nullable": false,
      "applies": {
        "only": ["bar", "waterfall", "pie", "treemap", "boxplot", "pointrange", "scatter", "line", "area", "histogram"]
      },
      "gear": {
        "group": "Numbers",
        "control": "select",
        "label": "Number format"
      },
      "slot": true,
      "ctrl": true
    },
    "unit": {
      "type": {
        "name": "string"
      },
      "default": "",
      "nullable": false,
      "applies": {
        "only": ["bar", "waterfall", "pie", "treemap", "boxplot", "pointrange", "scatter", "line", "area", "histogram"]
      },
      "gear": {
        "group": "Numbers",
        "control": "text",
        "label": "Unit"
      },
      "slot": true,
      "ctrl": true
    },
    "y_transform": {
      "type": {
        "name": "enum"
      },
      "default": "identity",
      "values": ["identity", "log10"],
      "nullable": false,
      "applies": {
        "only": ["scatter", "line", "band"]
      },
      "gear": {
        "group": "Look",
        "control": "segmented",
        "label": "Y scale",
        "labels": ["Linear", "Log"],
        "icons": true
      },
      "slot": true,
      "ctrl": true
    },
    "x_transform": {
      "type": {
        "name": "enum"
      },
      "default": "identity",
      "values": ["identity", "log10"],
      "nullable": false,
      "applies": {
        "only": ["scatter"]
      },
      "gear": {
        "group": "Look",
        "control": "segmented",
        "label": "X scale",
        "labels": ["Linear", "Log"],
        "icons": true
      },
      "slot": true,
      "ctrl": true
    },
    "x_limits": {
      "type": {
        "name": "limits"
      },
      "nullable": true,
      "applies": {
        "only": ["scatter", "line", "area", "band", "gantt"]
      },
      "gear": {
        "group": "Look",
        "control": "range",
        "label": "X limits"
      },
      "slot": true,
      "ctrl": true
    },
    "y_limits": {
      "type": {
        "name": "limits"
      },
      "nullable": true,
      "applies": {
        "only": ["scatter", "line", "area", "band", "bar", "histogram", "waterfall", "boxplot", "pointrange"]
      },
      "gear": {
        "group": "Look",
        "control": "range",
        "label": "Y limits"
      },
      "slot": true,
      "ctrl": true
    },
    "x_ticks": {
      "type": {
        "name": "ticks"
      },
      "nullable": true,
      "applies": {
        "only": ["scatter", "line", "area", "band", "gantt"]
      },
      "gear": {
        "group": "Look",
        "control": "ticks",
        "label": "X ticks"
      },
      "slot": true,
      "ctrl": true
    },
    "y_ticks": {
      "type": {
        "name": "ticks"
      },
      "nullable": true,
      "applies": {
        "only": ["scatter", "line", "area", "band", "bar", "histogram", "waterfall", "boxplot", "pointrange"]
      },
      "gear": {
        "group": "Look",
        "control": "ticks",
        "label": "Y ticks"
      },
      "slot": true,
      "ctrl": true
    },
    "tooltip": {
      "type": {
        "name": "columns"
      },
      "nullable": true,
      "applies": {
        "only": ["bar", "scatter", "line", "area", "band", "gantt"]
      },
      "gear": {
        "group": "Add mapping",
        "control": "columns",
        "label": "Tooltip fields"
      },
      "slot": false,
      "ctrl": true
    },
    "drill": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "also": ["auto"],
      "gear": {
        "group": "Drill-down",
        "control": "toggle",
        "label": "On click",
        "labels": ["Filter on", "Off"]
      },
      "slot": true,
      "ctrl": true
    },
    "line_width": {
      "type": {
        "name": "number"
      },
      "default": 1,
      "bounds": {
        "min": 0.5,
        "max": 3,
        "minOpen": false
      },
      "nullable": false,
      "applies": {
        "only": ["scatter", "line", "area", "band"]
      },
      "gear": {
        "group": "Marks",
        "control": "slider",
        "label": "Line width"
      },
      "slot": true,
      "ctrl": true
    },
    "point_size": {
      "type": {
        "name": "number"
      },
      "default": 1,
      "bounds": {
        "min": 0.5,
        "max": 3,
        "minOpen": false
      },
      "nullable": false,
      "applies": {
        "only": ["scatter", "line", "area", "band"]
      },
      "gear": {
        "group": "Marks",
        "control": "slider",
        "label": "Point size"
      },
      "slot": true,
      "ctrl": true
    },
    "connect": {
      "type": {
        "name": "enum"
      },
      "default": "straight",
      "values": ["straight", "monotone", "step_start", "step_middle", "step_end"],
      "nullable": false,
      "applies": {
        "only": ["line", "area"]
      },
      "gear": {
        "group": "Marks",
        "control": "segmented",
        "label": "Line",
        "labels": ["Straight", "Monotone", "Step at start", "Step at middle", "Step at end"],
        "icons": true
      },
      "slot": true,
      "ctrl": true
    },
    "value_lines": {
      "type": {
        "name": "numbers"
      },
      "nullable": true,
      "applies": {
        "only": ["bar", "waterfall", "boxplot", "pointrange", "scatter", "line", "area", "histogram"]
      },
      "gear": {
        "group": "Reference lines",
        "control": "text",
        "label": "Horizontal",
        "parse": "lines"
      },
      "slot": true,
      "ctrl": true
    },
    "x_lines": {
      "type": {
        "name": "values",
        "of": "x"
      },
      "nullable": true,
      "applies": {
        "only": ["bar", "scatter", "line", "area"]
      },
      "gear": {
        "group": "Reference lines",
        "control": "text",
        "label": "Vertical",
        "parse": "lines"
      },
      "slot": true,
      "ctrl": true
    },
    "ymin": {
      "type": {
        "name": "columns"
      },
      "nullable": true,
      "applies": {
        "only": ["scatter", "line", "area"]
      },
      "gear": {
        "group": "Marks",
        "control": "column",
        "label": "Interval low"
      },
      "slot": false,
      "ctrl": true
    },
    "ymax": {
      "type": {
        "name": "columns"
      },
      "nullable": true,
      "applies": {
        "only": ["scatter", "line", "area"]
      },
      "gear": {
        "group": "Marks",
        "control": "column",
        "label": "Interval high"
      },
      "slot": false,
      "ctrl": true
    },
    "linetype": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "applies": {
        "only": ["line", "area"]
      },
      "gear": {
        "group": "Add mapping",
        "control": "column",
        "label": "Dash by"
      },
      "slot": true,
      "ctrl": true
    },
    "shape": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "applies": {
        "only": ["scatter", "line"]
      },
      "gear": {
        "group": "Add mapping",
        "control": "column",
        "label": "Shape by"
      },
      "slot": true,
      "ctrl": true
    },
    "x_ranges": {
      "type": {
        "name": "named",
        "value": {
          "type": {
            "name": "values"
          },
          "nullable": false,
          "required": true,
          "slot": false,
          "ctrl": true
        }
      },
      "nullable": true,
      "applies": {
        "only": ["bar", "scatter", "line", "area"]
      },
      "gear": {
        "group": "Reference lines",
        "control": "text",
        "label": "Shaded",
        "parse": "ranges"
      },
      "slot": false,
      "ctrl": true
    },
    "legend": {
      "type": {
        "name": "enum"
      },
      "default": "bottom",
      "values": ["bottom", "top", "marks", "none"],
      "nullable": false,
      "gear": {
        "group": "Look",
        "control": "segmented",
        "label": "Legend",
        "labels": ["Below", "Above", "On the marks", "None"],
        "when": "color",
        "icons": true
      },
      "slot": true,
      "ctrl": true
    },
    "highlight": {
      "type": {
        "name": "named",
        "value": {
          "type": {
            "name": "values"
          },
          "nullable": false,
          "required": true,
          "slot": false,
          "ctrl": true
        },
        "max": 1
      },
      "nullable": true,
      "applies": {
        "only": ["bar", "scatter", "line", "area"]
      },
      "gear": {
        "group": "Add mapping",
        "control": "column",
        "label": "Highlight"
      },
      "slot": false,
      "ctrl": true
    },
    "smoother": {
      "type": {
        "name": "enum"
      },
      "default": "none",
      "values": ["none", "lm", "loess"],
      "nullable": false,
      "applies": {
        "only": ["scatter"]
      },
      "gear": {
        "group": "Marks",
        "control": "segmented",
        "label": "Smoother",
        "labels": ["None", "Linear", "Loess"],
        "icons": true
      },
      "slot": true,
      "ctrl": true
    },
    "identity_line": {
      "type": {
        "name": "bool"
      },
      "default": false,
      "nullable": false,
      "applies": {
        "only": ["scatter"]
      },
      "gear": {
        "group": "Reference lines",
        "control": "segmented",
        "label": "Identity line",
        "labels": ["Show y = x", "Off"]
      },
      "slot": true,
      "ctrl": true
    },
    "box_points": {
      "type": {
        "name": "enum"
      },
      "default": "none",
      "values": ["none", "outliers"],
      "nullable": false,
      "applies": {
        "only": ["boxplot", "band"]
      },
      "gear": {
        "group": "Marks",
        "control": "segmented",
        "label": "Points",
        "labels": ["None", "Outliers"],
        "icons": true
      },
      "slot": true,
      "ctrl": true
    },
    "summary": {
      "type": {
        "name": "enum"
      },
      "values": ["median_q1_q3", "mean_sd", "mean_2sd", "mean_se", "mean_ci95", "p5_p95", "p10_p90", "min_max"],
      "nullable": true,
      "applies": {
        "only": ["boxplot", "pointrange", "band"]
      },
      "gear": {
        "group": "Marks",
        "control": "select",
        "label": "Inner range",
        "labels": ["Median · Q1–Q3", "Mean ± SD", "Mean ± 2 SD", "Mean ± SE", "Mean · 95% CI", "5th–95th percentile", "10th–90th percentile", "Min–Max"]
      },
      "slot": true,
      "ctrl": true
    },
    "whiskers": {
      "type": {
        "name": "enum"
      },
      "values": ["tukey", "median_q1_q3", "mean_sd", "mean_2sd", "mean_se", "mean_ci95", "p5_p95", "p10_p90", "min_max"],
      "nullable": true,
      "applies": {
        "only": ["boxplot", "band"]
      },
      "gear": {
        "group": "Marks",
        "control": "select",
        "label": "Outer range",
        "labels": ["Tukey (1.5×IQR)", "Median · Q1–Q3", "Mean ± SD", "Mean ± 2 SD", "Mean ± SE", "Mean · 95% CI", "5th–95th percentile", "10th–90th percentile", "Min–Max"]
      },
      "slot": true,
      "ctrl": true
    },
    "connect_centers": {
      "type": {
        "name": "bool"
      },
      "default": false,
      "nullable": false,
      "applies": {
        "only": ["pointrange"]
      },
      "gear": {
        "group": "Marks",
        "control": "segmented",
        "label": "Centers",
        "labels": ["Connect", "Off"]
      },
      "slot": true,
      "ctrl": true
    },
    "band_window": {
      "type": {
        "name": "enum"
      },
      "default": "adaptive",
      "values": ["adaptive", "fixed"],
      "nullable": false,
      "applies": {
        "only": ["band"]
      },
      "gear": {
        "group": "Marks",
        "control": "select",
        "label": "Window",
        "labels": ["Adaptive (by subjects)", "Fixed (x units)"]
      },
      "slot": true,
      "ctrl": true
    },
    "band_size": {
      "type": {
        "name": "number"
      },
      "default": 45,
      "bounds": {
        "min": 0,
        "max": null,
        "minOpen": true
      },
      "nullable": false,
      "applies": {
        "only": ["band"]
      },
      "gear": {
        "group": "Marks",
        "control": "text",
        "label": "Subjects per window"
      },
      "slot": true,
      "ctrl": true
    },
    "band_min_n": {
      "type": {
        "name": "integer"
      },
      "default": 12,
      "bounds": {
        "min": 1,
        "max": null,
        "minOpen": false
      },
      "nullable": false,
      "applies": {
        "only": ["band"]
      },
      "gear": {
        "group": "Marks",
        "control": "text",
        "label": "Cut below n"
      },
      "slot": true,
      "ctrl": true
    },
    "id": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "gear": {
        "group": "Numbers",
        "control": "column",
        "label": "Individual id"
      },
      "slot": true,
      "ctrl": true
    },
    "ref_min": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "applies": {
        "only": ["band"]
      },
      "gear": {
        "group": "Reference lines",
        "control": "column",
        "label": "Reference low"
      },
      "slot": true,
      "ctrl": true
    },
    "ref_max": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "applies": {
        "only": ["band"]
      },
      "gear": {
        "group": "Reference lines",
        "control": "column",
        "label": "Reference high"
      },
      "slot": true,
      "ctrl": true
    },
    "ref_lines": {
      "type": {
        "name": "columns"
      },
      "nullable": true,
      "applies": {
        "only": ["scatter", "line", "area"]
      },
      "gear": {
        "group": "Reference lines",
        "control": "columns",
        "label": "From columns"
      },
      "slot": false,
      "ctrl": true
    },
    "ref_line_colors": {
      "type": {
        "name": "named",
        "value": {
          "type": {
            "name": "string"
          },
          "nullable": false,
          "required": true,
          "slot": false,
          "ctrl": true
        }
      },
      "nullable": true,
      "applies": {
        "only": ["scatter", "line", "area"]
      },
      "slot": false,
      "ctrl": true
    },
    "waterfall_totals": {
      "type": {
        "name": "values",
        "of": "category"
      },
      "nullable": true,
      "applies": {
        "only": ["waterfall"]
      },
      "gear": {
        "group": "Look",
        "control": "values",
        "label": "Totals"
      },
      "slot": false,
      "ctrl": true
    },
    "bins": {
      "type": {
        "name": "integer"
      },
      "default": 30,
      "bounds": {
        "min": 1,
        "max": 200,
        "minOpen": false
      },
      "nullable": false,
      "applies": {
        "only": ["histogram"]
      },
      "gear": {
        "group": "Mapping",
        "control": "number",
        "label": "Bins"
      },
      "slot": true,
      "ctrl": true
    },
    "show_n": {
      "type": {
        "name": "enum"
      },
      "default": "none",
      "values": ["none", "axis", "facet", "both"],
      "nullable": false,
      "gear": {
        "group": "Numbers",
        "control": "select",
        "label": "Group counts",
        "labels": ["Off", "On category axis", "On facet labels", "On axis + facets"]
      },
      "slot": true,
      "ctrl": true
    },
    "missing": {
      "type": {
        "name": "enum"
      },
      "default": "level",
      "values": ["level", "drop"],
      "nullable": false,
      "applies": {
        "only": ["bar"]
      },
      "gear": {
        "group": "Mapping",
        "control": "segmented",
        "label": "Missing",
        "labels": ["Own level", "Drop"]
      },
      "slot": true,
      "ctrl": true
    },
    "percent_of": {
      "type": {
        "name": "enum"
      },
      "default": "facet",
      "values": ["facet", "category", "color", "total"],
      "nullable": false,
      "applies": {
        "only": ["bar"]
      },
      "gear": {
        "group": "Numbers",
        "control": "select",
        "label": "Percent of",
        "labels": ["Facet", "Category", "Color", "Whole chart"]
      },
      "slot": true,
      "ctrl": true
    },
    "facet_scales": {
      "type": {
        "name": "enum"
      },
      "default": "fixed",
      "values": ["fixed", "free", "free_y"],
      "nullable": false,
      "applies": {
        "except": ["pie", "treemap"]
      },
      "gear": {
        "group": "Look",
        "control": "select",
        "label": "Panel scales",
        "labels": ["Shared across panels", "Free x and y per panel", "Free y per panel"]
      },
      "slot": true,
      "ctrl": true
    },
    "facet_cols": {
      "type": {
        "name": "integer"
      },
      "option": "blockr.viz.facet_cols",
      "bounds": {
        "min": 1,
        "max": null,
        "minOpen": false
      },
      "nullable": true,
      "gear": {
        "group": "Look",
        "control": "select",
        "label": "Panel columns",
        "when": "facet"
      },
      "slot": true,
      "ctrl": true
    },
    "download": {
      "type": {
        "name": "bool"
      },
      "default": true,
      "nullable": false,
      "gear": {
        "group": "Reader tools",
        "control": "segmented",
        "label": "Download",
        "labels": ["Show button", "Off"]
      },
      "slot": true,
      "ctrl": false
    },
    "title": {
      "type": {
        "name": "template"
      },
      "nullable": true,
      "gear": {
        "group": "Titles",
        "control": "template",
        "label": "Title"
      },
      "slot": false,
      "ctrl": true
    },
    "subtitle": {
      "type": {
        "name": "template"
      },
      "nullable": true,
      "gear": {
        "group": "Titles",
        "control": "template",
        "label": "Subtitle"
      },
      "slot": false,
      "ctrl": true
    },
    "caption": {
      "type": {
        "name": "template"
      },
      "option": "blockr.viz.default_caption",
      "nullable": true,
      "gear": {
        "group": "Titles",
        "control": "template",
        "label": "Caption"
      },
      "slot": false,
      "ctrl": true
    },
    "drill_target": {
      "type": {
        "name": "string"
      },
      "default": "",
      "nullable": false,
      "gear": {
        "group": "Drill-down",
        "control": "select",
        "label": "Target filter"
      },
      "slot": false,
      "ctrl": true
    },
    "drill_table": {
      "type": {
        "name": "string"
      },
      "default": "",
      "nullable": false,
      "gear": {
        "group": "Drill-down",
        "control": "text",
        "label": "Table",
        "when": "drill_target"
      },
      "slot": false,
      "ctrl": true
    },
    "script": {
      "type": {
        "name": "string"
      },
      "nullable": true,
      "gear": {
        "group": "Prepare script",
        "control": "script",
        "label": "Prepare script"
      },
      "slot": false,
      "ctrl": true
    },
    "script_values": {
      "type": {
        "name": "map"
      },
      "default": {},
      "nullable": false,
      "slot": false,
      "ctrl": true
    },
    "selection": {
      "type": {
        "name": "map"
      },
      "nullable": true,
      "slot": false,
      "ctrl": false
    }
  };
})();
// --- spec/table.js
// Generated by tools/gen-spec.R from the R spec of the table block. Do not edit.
// @ts-check
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || {});
  V.spec = V.spec || {};
  V.spec.table = {
    "layout": {
      "type": {
        "name": "enum"
      },
      "default": "records",
      "values": ["records", "groups", "variables"],
      "nullable": false,
      "gear": {
        "group": "Table",
        "control": "tiles",
        "label": "Layout",
        "labels": ["List", "Per group", "Per variable"]
      },
      "slot": true,
      "ctrl": true
    },
    "by": {
      "type": {
        "name": "columns"
      },
      "default": [],
      "nullable": false,
      "gear": {
        "group": "Table",
        "control": "columns",
        "label": "One row per"
      },
      "slot": true,
      "ctrl": true
    },
    "rowname": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "applies": {
        "only": ["records"]
      },
      "gear": {
        "group": "Table",
        "control": "column",
        "label": "Row names"
      },
      "slot": true,
      "ctrl": true
    },
    "columns": {
      "type": {
        "name": "columns"
      },
      "nullable": true,
      "applies": {
        "only": ["records"]
      },
      "gear": {
        "group": "Table",
        "control": "columns",
        "label": "Columns"
      },
      "slot": false,
      "ctrl": true
    },
    "summaries": {
      "type": {
        "name": "records_by",
        "key": "type",
        "variants": {
          "simple": {
            "name": {
              "type": {
                "name": "string"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "text",
                "label": "Name"
              },
              "slot": false,
              "ctrl": true
            },
            "facet": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Facet"
              },
              "slot": false,
              "ctrl": true
            },
            "show": {
              "type": {
                "name": "enum"
              },
              "default": "bar",
              "values": ["bar", "number", "dot", "tile"],
              "nullable": false,
              "gear": {
                "group": "Columns",
                "control": "segmented",
                "label": "Show",
                "labels": ["Bar", "Number", "Dot", "Tile"],
                "icons": true
              },
              "slot": false,
              "ctrl": true
            },
            "func": {
              "type": {
                "name": "enum"
              },
              "default": "count",
              "values": ["count", "count_distinct", "sum", "mean", "median", "min", "max", "identity"],
              "nullable": false,
              "gear": {
                "group": "Columns",
                "control": "select",
                "label": "Aggregate",
                "labels": ["Count", "Count distinct", "Sum", "Mean", "Median", "Min", "Max", "None (as is)"]
              },
              "slot": false,
              "ctrl": true
            },
            "col": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Of column"
              },
              "slot": false,
              "ctrl": true
            },
            "position": {
              "type": {
                "name": "enum"
              },
              "values": ["stack", "dodge", "fill"],
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "segmented",
                "label": "Stacking",
                "labels": ["Stacked", "Side by side", "100%"],
                "when": "color",
                "icons": true
              },
              "slot": false,
              "ctrl": true
            },
            "fill": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Paint by"
              },
              "slot": false,
              "ctrl": true
            },
            "color": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Color"
              },
              "slot": false,
              "ctrl": true
            }
          },
          "dist": {
            "name": {
              "type": {
                "name": "string"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "text",
                "label": "Name"
              },
              "slot": false,
              "ctrl": true
            },
            "facet": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Facet"
              },
              "slot": false,
              "ctrl": true
            },
            "show": {
              "type": {
                "name": "enum"
              },
              "default": "pointrange",
              "values": ["pointrange", "box", "text"],
              "nullable": false,
              "gear": {
                "group": "Columns",
                "control": "segmented",
                "label": "Show",
                "labels": ["Dot range", "Box", "Text"],
                "icons": true
              },
              "slot": false,
              "ctrl": true
            },
            "col": {
              "type": {
                "name": "column"
              },
              "nullable": false,
              "required": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Value"
              },
              "slot": false,
              "ctrl": true
            },
            "summary": {
              "type": {
                "name": "enum"
              },
              "default": "median_q1_q3",
              "values": ["median_q1_q3", "mean_sd", "mean_2sd", "mean_se", "mean_ci95", "p5_p95", "p10_p90", "min_max", "none"],
              "nullable": false,
              "gear": {
                "group": "Columns",
                "control": "select",
                "label": "Inner range",
                "labels": ["Median · Q1–Q3", "Mean ± SD", "Mean ± 2 SD", "Mean ± SE", "Mean · 95% CI", "5th–95th percentile", "10th–90th percentile", "Min–Max", "None"]
              },
              "slot": false,
              "ctrl": true
            },
            "whiskers": {
              "type": {
                "name": "enum"
              },
              "values": ["tukey", "median_q1_q3", "mean_sd", "mean_2sd", "mean_se", "mean_ci95", "p5_p95", "p10_p90", "min_max", "none"],
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "select",
                "label": "Outer range",
                "labels": ["Tukey (1.5×IQR)", "Median · Q1–Q3", "Mean ± SD", "Mean ± 2 SD", "Mean ± SE", "Mean · 95% CI", "5th–95th percentile", "10th–90th percentile", "Min–Max", "None"]
              },
              "slot": false,
              "ctrl": true
            },
            "color": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Color"
              },
              "slot": false,
              "ctrl": true
            }
          },
          "field": {
            "name": {
              "type": {
                "name": "string"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "text",
                "label": "Name"
              },
              "slot": false,
              "ctrl": true
            },
            "col": {
              "type": {
                "name": "column"
              },
              "nullable": false,
              "required": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Column"
              },
              "slot": false,
              "ctrl": true
            }
          },
          "sparkline": {
            "name": {
              "type": {
                "name": "string"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "text",
                "label": "Name"
              },
              "slot": false,
              "ctrl": true
            },
            "facet": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Facet"
              },
              "slot": false,
              "ctrl": true
            },
            "x": {
              "type": {
                "name": "column"
              },
              "nullable": false,
              "required": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Order (x)"
              },
              "slot": false,
              "ctrl": true
            },
            "col": {
              "type": {
                "name": "column"
              },
              "nullable": false,
              "required": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Value (y)"
              },
              "slot": false,
              "ctrl": true
            },
            "ymin": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Band low"
              },
              "slot": false,
              "ctrl": true
            },
            "ymax": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Band high"
              },
              "slot": false,
              "ctrl": true
            },
            "summary": {
              "type": {
                "name": "enum"
              },
              "default": "none",
              "values": ["none", "mean", "mean_sd", "median_q1_q3"],
              "nullable": false,
              "gear": {
                "group": "Columns",
                "control": "select",
                "label": "Reference",
                "labels": ["None", "Mean", "Mean ± SD", "Median · Q1–Q3"]
              },
              "slot": false,
              "ctrl": true
            }
          },
          "spans": {
            "name": {
              "type": {
                "name": "string"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "text",
                "label": "Name"
              },
              "slot": false,
              "ctrl": true
            },
            "facet": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Facet"
              },
              "slot": false,
              "ctrl": true
            },
            "x": {
              "type": {
                "name": "column"
              },
              "nullable": false,
              "required": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Start"
              },
              "slot": false,
              "ctrl": true
            },
            "xend": {
              "type": {
                "name": "column"
              },
              "nullable": false,
              "required": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "End"
              },
              "slot": false,
              "ctrl": true
            },
            "label": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Label"
              },
              "slot": false,
              "ctrl": true
            },
            "tooltip": {
              "type": {
                "name": "columns"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "columns",
                "label": "Tooltip fields"
              },
              "slot": false,
              "ctrl": true
            },
            "size": {
              "type": {
                "name": "enum"
              },
              "default": "md",
              "values": ["md", "lg", "xl"],
              "nullable": false,
              "gear": {
                "group": "Columns",
                "control": "segmented",
                "label": "Width",
                "labels": ["Regular", "Wide", "Widest"]
              },
              "slot": false,
              "ctrl": true
            },
            "color": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Color"
              },
              "slot": false,
              "ctrl": true
            },
            "ticks": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Ticks at"
              },
              "slot": false,
              "ctrl": true
            },
            "band_x": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Line from"
              },
              "slot": false,
              "ctrl": true
            },
            "band_xend": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Line to"
              },
              "slot": false,
              "ctrl": true
            },
            "event_x": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Events at"
              },
              "slot": false,
              "ctrl": true
            },
            "event_shape": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Event shape",
                "when": "event_x"
              },
              "slot": false,
              "ctrl": true
            },
            "open": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Open end"
              },
              "slot": false,
              "ctrl": true
            },
            "lane": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Lane"
              },
              "slot": false,
              "ctrl": true
            },
            "lane_full": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Full lanes"
              },
              "slot": false,
              "ctrl": true
            },
            "seg_style": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Segment style"
              },
              "slot": false,
              "ctrl": true
            },
            "event_tag": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Event tag",
                "when": "event_x"
              },
              "slot": false,
              "ctrl": true
            },
            "tip": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Tooltip text"
              },
              "slot": false,
              "ctrl": true
            },
            "lane_size": {
              "type": {
                "name": "enum"
              },
              "default": "sm",
              "values": ["sm", "md"],
              "nullable": false,
              "gear": {
                "group": "Columns",
                "control": "segmented",
                "label": "Lane height",
                "labels": ["Thin", "Regular"]
              },
              "slot": false,
              "ctrl": true
            },
            "track": {
              "type": {
                "name": "bool"
              },
              "default": false,
              "nullable": false,
              "gear": {
                "group": "Columns",
                "control": "toggle",
                "label": "Track behind the bars"
              },
              "slot": false,
              "ctrl": true
            },
            "bar_label": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Bar label"
              },
              "slot": false,
              "ctrl": true
            },
            "overlap": {
              "type": {
                "name": "enum"
              },
              "default": "draw",
              "values": ["draw", "worst"],
              "nullable": false,
              "gear": {
                "group": "Columns",
                "control": "segmented",
                "label": "Overlaps",
                "labels": ["Drawn over each other", "Worst level"]
              },
              "slot": false,
              "ctrl": true
            },
            "parent_lanes": {
              "type": {
                "name": "bool"
              },
              "default": false,
              "nullable": false,
              "gear": {
                "group": "Columns",
                "control": "toggle",
                "label": "Lanes on group rows"
              },
              "slot": false,
              "ctrl": true
            },
            "strip": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Hover strip"
              },
              "slot": false,
              "ctrl": true
            }
          },
          "pair": {
            "name": {
              "type": {
                "name": "string"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "text",
                "label": "Name"
              },
              "slot": false,
              "ctrl": true
            },
            "facet": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Facet"
              },
              "slot": false,
              "ctrl": true
            },
            "x": {
              "type": {
                "name": "column"
              },
              "nullable": false,
              "required": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "From"
              },
              "slot": false,
              "ctrl": true
            },
            "xend": {
              "type": {
                "name": "column"
              },
              "nullable": false,
              "required": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "To"
              },
              "slot": false,
              "ctrl": true
            },
            "x_func": {
              "type": {
                "name": "enum"
              },
              "default": "identity",
              "values": ["identity", "mean", "median", "min", "max"],
              "nullable": false,
              "gear": {
                "group": "Columns",
                "control": "select",
                "label": "From function",
                "labels": ["None (as is)", "Mean", "Median", "Min", "Max"]
              },
              "slot": false,
              "ctrl": true
            },
            "xend_func": {
              "type": {
                "name": "enum"
              },
              "default": "max",
              "values": ["identity", "mean", "median", "min", "max"],
              "nullable": false,
              "gear": {
                "group": "Columns",
                "control": "select",
                "label": "To function",
                "labels": ["None (as is)", "Mean", "Median", "Min", "Max"]
              },
              "slot": false,
              "ctrl": true
            },
            "ref_min": {
              "type": {
                "name": "string"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "text",
                "label": "Reference low"
              },
              "slot": false,
              "ctrl": true
            },
            "ref_max": {
              "type": {
                "name": "string"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "text",
                "label": "Reference high"
              },
              "slot": false,
              "ctrl": true
            },
            "x_lines": {
              "type": {
                "name": "numbers"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "text",
                "label": "Reference line",
                "parse": "lines"
              },
              "slot": false,
              "ctrl": true
            },
            "linetype": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Dash by"
              },
              "slot": false,
              "ctrl": true
            },
            "color": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Color"
              },
              "slot": false,
              "ctrl": true
            }
          },
          "expr": {
            "name": {
              "type": {
                "name": "string"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "text",
                "label": "Name"
              },
              "slot": false,
              "ctrl": true
            },
            "facet": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Facet"
              },
              "slot": false,
              "ctrl": true
            },
            "expr": {
              "type": {
                "name": "string"
              },
              "nullable": false,
              "required": true,
              "gear": {
                "group": "Columns",
                "control": "code",
                "label": "Expression"
              },
              "slot": false,
              "ctrl": true
            }
          },
          "custom": {
            "name": {
              "type": {
                "name": "string"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "text",
                "label": "Name"
              },
              "slot": false,
              "ctrl": true
            },
            "facet": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Facet"
              },
              "slot": false,
              "ctrl": true
            },
            "show": {
              "type": {
                "name": "enum"
              },
              "values": ["bar", "number", "dumbbell", "box", "pointrange", "text"],
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "segmented",
                "label": "Show",
                "labels": ["Bar", "Number", "Dumbbell", "Box", "Dot range", "Text"],
                "icons": true
              },
              "slot": false,
              "ctrl": true
            },
            "fn": {
              "type": {
                "name": "string"
              },
              "nullable": false,
              "required": true,
              "gear": {
                "group": "Columns",
                "control": "code",
                "label": "Summary of d"
              },
              "slot": false,
              "ctrl": true
            },
            "id": {
              "type": {
                "name": "column"
              },
              "nullable": true,
              "gear": {
                "group": "Columns",
                "control": "column",
                "label": "Percent of"
              },
              "slot": false,
              "ctrl": true
            }
          }
        }
      },
      "default": [],
      "nullable": false,
      "applies": {
        "only": ["groups", "variables"]
      },
      "gear": {
        "group": "Columns",
        "control": "custom",
        "label": "Columns"
      },
      "slot": false,
      "ctrl": true
    },
    "id": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "applies": {
        "only": ["variables"]
      },
      "gear": {
        "group": "Variables",
        "control": "select",
        "label": "Individual id"
      },
      "slot": true,
      "ctrl": true
    },
    "percent_of": {
      "type": {
        "name": "enum"
      },
      "default": "facet",
      "values": ["facet", "total"],
      "nullable": false,
      "applies": {
        "only": ["groups"]
      },
      "gear": {
        "group": "Numbers",
        "control": "segmented",
        "label": "Percent of",
        "labels": ["Facet", "Table"]
      },
      "slot": true,
      "ctrl": true
    },
    "missing": {
      "type": {
        "name": "enum"
      },
      "default": "level",
      "values": ["level", "drop"],
      "nullable": false,
      "gear": {
        "group": "Rows shown",
        "control": "segmented",
        "label": "Missing",
        "labels": ["Own level", "Drop"]
      },
      "slot": true,
      "ctrl": true
    },
    "color": {
      "type": {
        "name": "column"
      },
      "default": "auto",
      "nullable": true,
      "also": ["auto"],
      "applies": {
        "only": ["records"]
      },
      "gear": {
        "group": "Look",
        "control": "column",
        "label": "Color by"
      },
      "slot": true,
      "ctrl": true
    },
    "shadings": {
      "type": {
        "name": "records",
        "fields": {
          "mode": {
            "type": {
              "name": "enum"
            },
            "default": "diverging",
            "values": ["diverging", "sequential", "bar"],
            "nullable": false,
            "gear": {
              "group": "Color",
              "control": "segmented",
              "label": "Mode",
              "labels": ["Diverging", "Sequential", "Data bar"],
              "icons": true
            },
            "slot": false,
            "ctrl": true
          },
          "columns": {
            "type": {
              "name": "columns"
            },
            "nullable": true,
            "gear": {
              "group": "Color",
              "control": "columns",
              "label": "on"
            },
            "slot": false,
            "ctrl": true
          },
          "domain": {
            "type": {
              "name": "numbers"
            },
            "nullable": true,
            "slot": false,
            "ctrl": true
          }
        }
      },
      "default": [],
      "nullable": false,
      "applies": {
        "only": ["records"]
      },
      "gear": {
        "group": "Look",
        "control": "custom",
        "label": "Shade cells"
      },
      "slot": false,
      "ctrl": true
    },
    "format": {
      "type": {
        "name": "format"
      },
      "default": "number",
      "nullable": false,
      "applies": {
        "only": ["records", "groups"]
      },
      "gear": {
        "group": "Numbers",
        "control": "select",
        "label": "Number format"
      },
      "slot": true,
      "ctrl": true
    },
    "unit": {
      "type": {
        "name": "string"
      },
      "default": "",
      "nullable": false,
      "applies": {
        "only": ["records", "groups"]
      },
      "gear": {
        "group": "Numbers",
        "control": "text",
        "label": "Unit"
      },
      "slot": true,
      "ctrl": true
    },
    "look": {
      "type": {
        "name": "enum"
      },
      "default": "table",
      "values": ["table", "compact", "cards"],
      "nullable": false,
      "gear": {
        "group": "Look",
        "control": "tiles",
        "label": "Style",
        "labels": ["Table", "Compact", "Cards"]
      },
      "slot": true,
      "ctrl": true
    },
    "facet_layout": {
      "type": {
        "name": "enum"
      },
      "default": "by_summary",
      "values": ["by_summary", "by_level"],
      "nullable": false,
      "applies": {
        "only": ["groups"]
      },
      "gear": {
        "group": "Look",
        "control": "segmented",
        "label": "Facet layout",
        "labels": ["By summary", "By level"],
        "icons": true
      },
      "slot": true,
      "ctrl": true
    },
    "group_rows": {
      "type": {
        "name": "enum"
      },
      "default": "name",
      "values": ["name", "summary"],
      "nullable": false,
      "applies": {
        "only": ["groups"]
      },
      "gear": {
        "group": "Look",
        "control": "segmented",
        "label": "Group rows",
        "labels": ["Name only", "With summary"]
      },
      "slot": true,
      "ctrl": true
    },
    "open": {
      "type": {
        "name": "enum"
      },
      "default": "none",
      "values": ["none", "all"],
      "nullable": false,
      "applies": {
        "only": ["groups"]
      },
      "gear": {
        "group": "Look",
        "control": "segmented",
        "label": "Groups at start",
        "labels": ["Closed", "Open"]
      },
      "slot": true,
      "ctrl": true
    },
    "sort_by": {
      "type": {
        "name": "string"
      },
      "default": "value",
      "nullable": false,
      "applies": {
        "only": ["groups"]
      },
      "gear": {
        "group": "Order",
        "control": "select",
        "label": "Sort"
      },
      "slot": true,
      "ctrl": true
    },
    "sort_dir": {
      "type": {
        "name": "enum"
      },
      "values": ["asc", "desc"],
      "nullable": true,
      "applies": {
        "only": ["groups"]
      },
      "gear": {
        "group": "Order",
        "control": "segmented",
        "label": "Direction",
        "labels": ["Ascending", "Descending"],
        "icons": true
      },
      "slot": true,
      "ctrl": true
    },
    "top_n": {
      "type": {
        "name": "integer"
      },
      "bounds": {
        "min": 1,
        "max": null,
        "minOpen": false
      },
      "nullable": true,
      "applies": {
        "only": ["groups"]
      },
      "gear": {
        "group": "Rows shown",
        "control": "number",
        "label": "Top"
      },
      "slot": true,
      "ctrl": true
    },
    "value_labels": {
      "type": {
        "name": "bool"
      },
      "default": true,
      "nullable": false,
      "applies": {
        "only": ["groups"]
      },
      "gear": {
        "group": "Bars",
        "control": "segmented",
        "label": "Value labels",
        "labels": ["Show", "Off"]
      },
      "slot": true,
      "ctrl": true
    },
    "column_axis": {
      "type": {
        "name": "bool"
      },
      "default": true,
      "nullable": false,
      "applies": {
        "only": ["groups"]
      },
      "gear": {
        "group": "Bars",
        "control": "segmented",
        "label": "Column axis",
        "labels": ["Show", "Off"]
      },
      "slot": true,
      "ctrl": true
    },
    "bar_width": {
      "type": {
        "name": "enum"
      },
      "default": "fit",
      "values": ["fit", "narrow", "medium", "wide"],
      "nullable": false,
      "applies": {
        "only": ["groups"]
      },
      "gear": {
        "group": "Bars",
        "control": "segmented",
        "label": "Bar width",
        "labels": ["Fit", "Narrow", "Medium", "Wide"]
      },
      "slot": true,
      "ctrl": true
    },
    "drill": {
      "type": {
        "name": "column"
      },
      "nullable": true,
      "also": ["auto", "source"],
      "gear": {
        "group": "Drill-down",
        "control": "toggle",
        "label": "On click",
        "labels": ["Filter on", "Off"]
      },
      "slot": true,
      "ctrl": true
    },
    "search": {
      "type": {
        "name": "bool"
      },
      "default": true,
      "nullable": false,
      "gear": {
        "group": "Reader tools",
        "control": "segmented",
        "label": "Search bar",
        "labels": ["Show", "Off"]
      },
      "slot": true,
      "ctrl": true
    },
    "sortable": {
      "type": {
        "name": "bool"
      },
      "default": true,
      "nullable": false,
      "gear": {
        "group": "Reader tools",
        "control": "segmented",
        "label": "Header sorting",
        "labels": ["Allow", "Off"]
      },
      "slot": true,
      "ctrl": true
    },
    "collapsible": {
      "type": {
        "name": "bool"
      },
      "default": true,
      "nullable": false,
      "applies": {
        "only": ["records", "variables"]
      },
      "gear": {
        "group": "Look",
        "control": "segmented",
        "label": "Group rows",
        "labels": ["Collapsible", "Off"]
      },
      "slot": true,
      "ctrl": true
    },
    "download": {
      "type": {
        "name": "bool"
      },
      "default": true,
      "nullable": false,
      "gear": {
        "group": "Reader tools",
        "control": "segmented",
        "label": "Download",
        "labels": ["Show button", "Off"]
      },
      "slot": true,
      "ctrl": true
    },
    "stats": {
      "type": {
        "name": "enums"
      },
      "default": [
        "mean_sd"
      ],
      "values": ["n", "n_pct", "mean", "sd", "mean_sd", "median", "median_q1_q3", "q1_q3", "min_max"],
      "nullable": false,
      "applies": {
        "only": ["variables"]
      },
      "gear": {
        "group": "Variables",
        "control": "columns",
        "label": "Statistics",
        "labels": ["N", "n (%)", "Mean", "SD", "Mean (SD)", "Median", "Median (Q1, Q3)", "Q1, Q3", "Min, Max"]
      },
      "slot": false,
      "ctrl": true
    },
    "sections": {
      "type": {
        "name": "columns"
      },
      "default": [],
      "nullable": false,
      "applies": {
        "only": ["variables"]
      },
      "gear": {
        "group": "Variables",
        "control": "columns",
        "label": "Section by"
      },
      "slot": false,
      "ctrl": true
    },
    "total": {
      "type": {
        "name": "bool"
      },
      "default": false,
      "nullable": false,
      "applies": {
        "only": ["variables"]
      },
      "gear": {
        "group": "Variables",
        "control": "toggle",
        "label": "Overall column",
        "labels": ["Add", "Off"]
      },
      "slot": true,
      "ctrl": true
    },
    "total_label": {
      "type": {
        "name": "string"
      },
      "default": "Total",
      "nullable": false,
      "applies": {
        "only": ["variables"]
      },
      "gear": {
        "group": "Variables",
        "control": "text",
        "label": "Label"
      },
      "slot": true,
      "ctrl": true
    },
    "nest": {
      "type": {
        "name": "bool"
      },
      "default": false,
      "nullable": false,
      "applies": {
        "only": ["variables"]
      },
      "gear": {
        "group": "Variables",
        "control": "toggle",
        "label": "Nest hierarchy",
        "labels": ["Nest", "Off"]
      },
      "slot": true,
      "ctrl": true
    },
    "title": {
      "type": {
        "name": "template"
      },
      "nullable": true,
      "gear": {
        "group": "Titles",
        "control": "template",
        "label": "Title"
      },
      "slot": false,
      "ctrl": true
    },
    "subtitle": {
      "type": {
        "name": "template"
      },
      "nullable": true,
      "gear": {
        "group": "Titles",
        "control": "template",
        "label": "Subtitle"
      },
      "slot": false,
      "ctrl": true
    },
    "caption": {
      "type": {
        "name": "template"
      },
      "option": "blockr.viz.default_caption",
      "nullable": true,
      "gear": {
        "group": "Titles",
        "control": "template",
        "label": "Caption"
      },
      "slot": false,
      "ctrl": true
    },
    "drill_target": {
      "type": {
        "name": "string"
      },
      "default": "",
      "nullable": false,
      "gear": {
        "group": "Drill-down",
        "control": "select",
        "label": "Target filter"
      },
      "slot": false,
      "ctrl": true
    },
    "drill_table": {
      "type": {
        "name": "string"
      },
      "default": "",
      "nullable": false,
      "gear": {
        "group": "Drill-down",
        "control": "text",
        "label": "Table",
        "when": "drill_target"
      },
      "slot": false,
      "ctrl": true
    },
    "script": {
      "type": {
        "name": "string"
      },
      "nullable": true,
      "gear": {
        "group": "Prepare script",
        "control": "script",
        "label": "Prepare script"
      },
      "slot": false,
      "ctrl": true
    },
    "script_values": {
      "type": {
        "name": "map"
      },
      "default": {},
      "nullable": false,
      "slot": false,
      "ctrl": true
    },
    "selection": {
      "type": {
        "name": "map"
      },
      "nullable": true,
      "slot": false,
      "ctrl": false
    }
  };
})();
//# sourceMappingURL=core.js.map
