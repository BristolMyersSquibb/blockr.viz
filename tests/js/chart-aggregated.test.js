/* Characterization snapshots, aggregated family and the empty input
 * (cases.js, __snapshots__/). */
'use strict';

const { aggregated, other } = require('./cases');
const { snapshotTests } = require('./run-case');

snapshotTests(aggregated);
snapshotTests(other);
