/* Tests v2 skips by name until it draws their chart family.
 *
 * CHART_ENGINE=v2 runs every test in tests/js against v2; the ones listed
 * here are skipped, keyed "<test file>: <test name>" (the drawing snapshots
 * are all registered by run-case.js). Each build step removes its family's
 * entries. A test of a built family must match v1 or have its difference
 * listed in v2-differences.json.
 *
 * V2_RUN_PENDING=1 runs them anyway, to see which already pass.
 */
'use strict';

// Every family is drawn: nothing is skipped. Step 2d (interactions,
// chrome, export, the R side of `filters`) adds no skips; its changes show
// as listed differences.
module.exports = {};
