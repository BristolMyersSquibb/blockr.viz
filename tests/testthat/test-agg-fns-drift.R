# Drift guard: the JS aggregation vocabulary (inst/js/drilldown-agg.js,
# consumed by the chart / table / tile gear) MUST mirror the R `AGG_FNS`
# (R/block-arguments.R, the registry/LLM argument enum). Each side is the
# single source for its language; this test is the only thing tying them.
test_that("JS AGG_FNS mirrors R AGG_FNS", {
  js_path <- system.file("js", "drilldown-agg.js", package = "blockr.viz")
  expect_true(nzchar(js_path))
  js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")

  # Extract the `value: '...'` entries of the `const AGG_FNS = [...]` literal.
  arr <- regmatches(js, regexpr("AGG_FNS = \\[[^]]*\\]", js))
  expect_length(arr, 1L)
  vals <- regmatches(arr, gregexpr("value: '[a-z_]+'", arr))[[1]]
  vals <- sub("^value: '", "", sub("'$", "", vals))

  expect_identical(vals, AGG_FNS)
})
