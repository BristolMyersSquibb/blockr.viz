/* The busy cue: an output that is being recomputed dims, and a line under its
 * sentence counts the wait ("drawing… 1.2 s"). Shared by every viz output and
 * by blocks that render through viz (the sandbox composer block).
 *
 * R starts it with the "blockr-busy" message, sent BEFORE the slow work: a
 * custom message reaches the browser while R is still computing, whereas
 * Shiny's own .recalculating only arrives after blockr.core's compute. The
 * owner of the output ends it once the new picture is on screen (chart.js
 * after a draw), or R sends "blockr-busy-done" when no new picture is coming
 * (an identical payload, an error).
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
    delete pending[id];
  }

  /* A second start while one is pending keeps the first clock: the wait the
   * reader sees began then. */
  function start(id, label) {
    if (pending[id]) return;
    var t0 = performance.now();
    var p = { delay: null, clock: null };
    pending[id] = p;
    var tick = function () {
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
    var root = rootFor(id);
    if (root) root.classList.remove('blockr-busy');
  }

  window.Blockr = window.Blockr || {};
  window.Blockr.busy = { start: start, stop: stop };

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler('blockr-busy', function (msg) {
      start(msg.id, msg.label);
    });
    Shiny.addCustomMessageHandler('blockr-busy-done', function (msg) {
      stop(msg.id);
    });
  }
})();
