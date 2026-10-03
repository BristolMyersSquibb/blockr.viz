# R/report-call.R :: how a block states its printed form for a document.

test_that("non-viz blocks print bare", {
  expect_null(report_call(blockr.core::new_head_block(), "h"))
})

test_that("chart blocks have no report call", {
  expect_null(report_call(new_chart_block(group = "Species"), "c1"))
})
