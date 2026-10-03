/* Characterization snapshots, timeline family (cases.js, __snapshots__/). */
'use strict';

const { timeline } = require('./cases');
const { snapshotTests } = require('./run-case');

snapshotTests(timeline);
