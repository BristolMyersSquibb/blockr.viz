// Standalone node runner for the golden aggregation cross-tests
// (tests/testthat/test-agg-golden.R, test-agg-identity.R). Loads the tests'
// JavaScript aggregation (tests/js/agg-reference.js, path = argv[2]), feeds
// it {rows, config} JSON from stdin and writes aggregate(rows, config) as
// JSON to stdout, so testthat can compare its numbers against R's.
'use strict';

const { aggregate } = require(process.argv[2]);

const chunks = [];
process.stdin.on('data', (c) => chunks.push(c));
process.stdin.on('end', () => {
  const input = JSON.parse(Buffer.concat(chunks).toString('utf8'));
  process.stdout.write(JSON.stringify(aggregate(input.rows, input.config)));
});
