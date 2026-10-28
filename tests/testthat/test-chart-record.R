# The chart block replayed against its recordings (helper-chart-record.R,
# written by dev/chart-record.R on the reference). The rewritten chart is
# fed the same steps through the kit's inputs (helper-chart-rewrite.R). A
# difference is a change in what the block holds, emits, sends downstream or
# computes for the drawing.

target <- record_target_rewrite()

for (nm in names(record_scenarios())) {
  local({
    nm <- nm
    sc <- record_scenarios()[[nm]]
    test_that(paste("chart recording:", nm), {
      path <- file.path(record_dir(), paste0(nm, ".rds"))
      skip_if_not(file.exists(path), "no recording")
      skip_if(isTRUE(sc$wire), "the reference's browser messages")
      if (nm %in% names(record_rw_open)) {
        skip(paste("open behaviour change,", record_rw_open[[nm]]))
      }
      got <- suppressWarnings(record_run(sc, target))
      expect_equal(record_rw_got(got), record_rw_expected(readRDS(path)))
    })
  })
}

# The constructor's errors are the spec's now (api-proposal.md section 1);
# what stays is which calls fail and which warn.
test_that("chart recording: constructor cases fail and warn as before", {
  path <- file.path(record_dir(), "ctor-cases.rds")
  skip_if_not(file.exists(path), "no recording")
  want <- readRDS(path)[names(record_ctor_cases())]
  got <- lapply(record_ctor_cases()[names(want)], record_ctor_run, target)
  failed <- function(x) {
    vapply(x, function(r) inherits(r$value, "record_error"), logical(1L))
  }
  expect_identical(failed(got), failed(want))
  warns <- function(x) lapply(x, function(r) record_rw_warnings(r$warnings))
  expect_identical(warns(got), warns(want))
})
