/* Characterization snapshots, individual family (cases.js, __snapshots__/). */
'use strict';

const { individual } = require('./cases');
const { snapshotTests } = require('./run-case');

snapshotTests(individual);
