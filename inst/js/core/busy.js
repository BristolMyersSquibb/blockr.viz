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
