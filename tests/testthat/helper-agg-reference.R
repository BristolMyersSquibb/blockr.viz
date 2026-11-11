# The tests' JavaScript aggregation (tests/js/agg-reference.js): what the
# browser ran until R took the chart's aggregation over, kept as a second
# implementation to compare R's with. It lives beside the JS tests, so a
# built package has none and the tests that need it skip.
agg_reference <- function() {
  testthat::test_path("..", "js", "agg-reference.js")
}
