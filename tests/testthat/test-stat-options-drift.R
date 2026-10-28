# The variables layout offers the statistics its spec lists; each one has to
# be one R can format (SUMMARY_STATS_CATALOG), in the catalog's order.
test_that("the spec's statistics are the R catalog's", {
  spec <- viz_block_spec("table")$args$stats
  expect_identical(spec$values, names(SUMMARY_STATS_CATALOG))
})
