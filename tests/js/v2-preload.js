/* Preloaded by `npm run test:v2` (node --require). Under CHART_ENGINE=v2 it
 * skips the tests v2-pending.js lists, by wrapping node:test's `test` for
 * every test file. The test files themselves stay as they are.
 */
'use strict';

const Module = require('node:module');
const path = require('node:path');

if (process.env.CHART_ENGINE === 'v2' && process.env.V2_RUN_PENDING !== '1') {
  const pending = require('./v2-pending');
  /** @type {Map<string, string>} key -> family */
  const skip = new Map();
  for (const [family, keys] of Object.entries(pending)) {
    for (const k of keys) skip.set(k, family);
  }

  const load = Module._load;
  Module._load = function (request, parent) {
    const mod = load.apply(this, arguments);
    if (request !== 'node:test' && request !== 'test') return mod;
    // The file node runs, not the module asking: the drawing snapshots are
    // registered by run-case.js on behalf of chart-*.test.js.
    const file = path.basename((require.main && require.main.filename) ||
                               (parent && parent.filename) || '');
    const wrapped = function (name, opts, fn) {
      const family = skip.get(file + ': ' + name);
      if (!family) return mod.apply(this, arguments);
      if (typeof opts === 'function') { fn = opts; opts = {}; }
      return mod(name, { ...(opts || {}), skip: `v2 does not draw the ${family} family yet` }, fn);
    };
    return Object.assign(wrapped, mod);
  };
}
