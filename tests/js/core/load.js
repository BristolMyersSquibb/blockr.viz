/* Load blockr.viz's core scripts into a fresh happy-dom window, in the order
 * inst/js/core/scripts.txt lists them, with a Shiny stub that records every
 * setInputValue call. */
'use strict';

const fs = require('node:fs');
const path = require('node:path');
const { after } = require('node:test');
const { Window } = require('happy-dom');

const CORE_DIR = path.join(__dirname, '..', '..', '..', 'inst', 'js', 'core');

const CORE_FILES = fs.readFileSync(path.join(CORE_DIR, 'scripts.txt'), 'utf8')
  .split('\n').map((l) => l.trim()).filter((l) => l && !l.startsWith('#'));

// Every window a test opens is closed when the test file ends; an open one
// keeps node running.
const windows = [];
after(async () => {
  for (const w of windows) {
    w.happyDOM.abort();
    await w.happyDOM.close();
  }
});

/** @param {{ shiny?: boolean }} [opts] */
function loadCore(opts = {}) {
  const win = new Window();
  windows.push(win);
  const sent = [];
  const outputs = {};
  const bound = [];
  if (opts.shiny !== false) {
    win.Shiny = {
      setInputValue: (name, value, o) => sent.push({ name, value, opts: o }),
      OutputBinding: class {},
      bindAll: (el) => bound.push(['bind', el]),
      unbindAll: (el) => bound.push(['unbind', el]),
      outputBindings: { register: (b, name) => { outputs[name] = b; } }
    };
    // The few jQuery calls the binding makes: find, and on / off for
    // Shiny's jQuery-only "shiny:unbound" event, which trigger() fires.
    win.jQuery = (x) => ({
      find: (sel) => Array.from(x.querySelectorAll(sel)),
      on: (type, fn) => { (x.__jq = x.__jq || {})[type] = fn; },
      off: (type) => { if (x.__jq) delete x.__jq[type]; }
    });
  }
  // With a sourceURL, so c8 maps the coverage to the file.
  for (const f of CORE_FILES) {
    const file = path.join(CORE_DIR, f);
    win.eval(fs.readFileSync(file, 'utf8') + '\n//# sourceURL=file://' + file);
  }
  const trigger = (el, type) => { if (el.__jq && el.__jq[type]) el.__jq[type](); };
  return { win, V: win.Blockr.viz, sent, outputs, trigger, bound };
}

module.exports = { loadCore, CORE_FILES };
