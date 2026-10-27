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
  if (opts.shiny !== false) {
    win.Shiny = {
      setInputValue: (name, value, o) => sent.push({ name, value, opts: o })
    };
  }
  for (const f of CORE_FILES) {
    win.eval(fs.readFileSync(path.join(CORE_DIR, f), 'utf8'));
  }
  return { win, V: win.Blockr.viz, sent };
}

module.exports = { loadCore, CORE_FILES };
