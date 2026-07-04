# Drift guard: the summary-table gear's JS stat catalog (STAT_OPTIONS in
# inst/js/summary-table-block.js) MUST mirror the R SUMMARY_STATS_CATALOG
# (R/summary-table.R) -- keys AND labels. Each side is the single source for
# its language; this test is the only thing tying them (same recipe as the
# AGG_FNS drift guard).
test_that("JS STAT_OPTIONS mirrors R SUMMARY_STATS_CATALOG", {
  js_path <- system.file("js", "summary-table-block.js", package = "blockr.viz")
  expect_true(nzchar(js_path))
  js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")

  # Extract `{ key: '...', label: '...' }` entries of the STAT_OPTIONS array.
  arr <- regmatches(js, regexpr("STAT_OPTIONS = \\[[^]]*\\]", js))
  expect_length(arr, 1L)
  entries <- regmatches(
    arr,
    gregexpr("key: '[^']+',\\s*label: '[^']+'", arr)
  )[[1]]
  keys <- sub("^key: '([^']+)'.*$", "\\1", entries)
  labels <- sub("^.*label: '([^']+)'$", "\\1", entries)

  expect_identical(keys, names(SUMMARY_STATS_CATALOG))
  expect_identical(
    labels,
    vapply(SUMMARY_STATS_CATALOG, `[[`, "", "label", USE.NAMES = FALSE)
  )
})
