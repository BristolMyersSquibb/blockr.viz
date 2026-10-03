/* Characterization snapshots, aggregated family (cases.js, __snapshots__/). */
'use strict';

const { aggregated } = require('./cases');
const { snapshotTests } = require('./run-case');

snapshotTests(aggregated);
