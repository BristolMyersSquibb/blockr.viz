/* The busy cue: an output that is being recomputed dims, and a line under its
 * sentence counts the wait ("drawing… 1.2 s"). Shared by every viz output and
 * by blocks that render through viz (the sandbox composer block).
 *
 * R starts it with the "blockr-busy" message, sent BEFORE the slow work: a
 * custom message reaches the browser while R is still computing, whereas
 * Shiny's own .recalculating only arrives after blockr.core's compute. The
 * owner of the output ends it once the new picture is on screen
 * (chart/binding.js after a draw), or R sends "blockr-busy-done" when no new
 * picture is coming (an identical payload, an error).
 *
 * Nothing shows for the first BUSY_AFTER ms: a cue that flashes for a few
 * frames is noise. The output the cue belongs to is found by element id, or
 * by the table's data-dt-elem-id; the class goes on that element, the line is
 * its `.blockr-busy-line` descendant (looked up on every tick, since a chart
 * rebuilds its DOM).
 */
(function () {
  'use strict';

  var BUSY_AFTER = 300;
  // R's done waits this long: a block being built reads an input that is not
  // ready yet, sends done, and starts again a few ms later. One wait, one
  // clock.
  var DONE_SETTLE = 250;
  var pending = {};

  function rootFor(id) {
    return document.getElementById(id) ||
      document.querySelector('[data-dt-elem-id="' + id + '"]');
  }

  function clear(id) {
    var p = pending[id];
    if (!p) return;
    if (p.delay) clearTimeout(p.delay);
    if (p.clock) clearInterval(p.clock);
    if (p.ending) clearTimeout(p.ending);
    delete pending[id];
  }

  /* A second start while one is pending keeps the first clock: the wait the
   * reader sees began then. */
  function start(id, label) {
    var q = pending[id];
    if (q) {
      if (q.ending) clearTimeout(q.ending);
      q.ending = null;
      return;
    }
    var t0 = performance.now();
    var p = { delay: null, clock: null, ending: null };
    pending[id] = p;
    var tick = function () {
      if (pending[id] !== p) return;
      var root = rootFor(id);
      if (!root) return;
      root.classList.add('blockr-busy');
      var line = root.querySelector('.blockr-busy-line');
      if (line) {
        line.textContent = (label || 'computing') + '… ' +
          ((performance.now() - t0) / 1000).toFixed(1) + ' s';
      }
    };
    p.delay = setTimeout(function () {
      p.delay = null;
      tick();
      p.clock = setInterval(tick, 100);
    }, BUSY_AFTER);
  }

  function stop(id) {
    clear(id);
    delete watched[id];
    var root = rootFor(id);
    if (root) root.classList.remove('blockr-busy');
  }

  /* An output R has not spoken about yet starts its clock the moment it
   * comes on screen. A board that builds blocks lazily makes one only once
   * its panel shows, and R's "blockr-busy" goes out after that, a second or
   * more after the click; the panel would sit blank and uncounted until then.
   * R's start keeps this clock, R's done or the drawn picture ends it. An id
   * R has already spoken about is never watched: that block exists, and an
   * idle one would leave the clock running. */
  var heard = {};
  var watched = {};

  function shown(el) {
    if (!el || !el.isConnected) return false;
    if (el.checkVisibility) return el.checkVisibility({ visibilityProperty: true });
    for (var e = el; e; e = e.parentElement) {
      var cs = getComputedStyle(e);
      if (cs.display === 'none' || cs.visibility === 'hidden') return false;
    }
    return true;
  }

  function sweep() {
    Object.keys(watched).forEach(function (id) {
      if (!shown(rootFor(id))) return;
      var label = watched[id];
      delete watched[id];
      start(id, label);
    });
  }

  function watch(id, label) {
    if (heard[id] || pending[id]) return;
    watched[id] = label;
  }

  // dockViewR's bubbling event when a tab comes to the front. The panel shows
  // a frame or two later (blockr.dock moves the card in), so the check runs
  // on every frame for about a second.
  var frames = 0;
  function sweepFrames() {
    sweep();
    if (--frames > 0) requestAnimationFrame(sweepFrames);
  }
  document.addEventListener('dockview:active-panel', function () {
    var idle = frames <= 0;
    frames = 60;
    if (idle) requestAnimationFrame(sweepFrames);
  });

  window.Blockr = window.Blockr || {};
  window.Blockr.busy = { start: start, stop: stop, watch: watch };

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler('blockr-busy', function (msg) {
      heard[msg.id] = true;
      delete watched[msg.id];
      start(msg.id, msg.label);
    });
    Shiny.addCustomMessageHandler('blockr-busy-done', function (msg) {
      heard[msg.id] = true;
      var p = pending[msg.id];
      if (!p) return stop(msg.id);
      if (!p.ending) {
        p.ending = setTimeout(function () {
          if (pending[msg.id] === p) stop(msg.id);
        }, DONE_SETTLE);
      }
    });
  }
})();
