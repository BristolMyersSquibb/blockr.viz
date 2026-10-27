# The block specs against the recorded behaviour of the reference: every
# state a recorded scenario went through, restored the way a saved board is
# (legacy_restore(), R/legacy.R), must pass the spec's constructor check and
# come back unchanged through the browser's wire. Runs once the block's spec
# and the legacy restore exist.

recorded_states <- function(dir, translate = identity) {
  files <- list.files(test_path("fixtures", dir), "\\.rds$",
                      full.names = TRUE)
  out <- list()
  for (f in files) {
    r <- readRDS(f)
    if (!is.list(r) || !all(c("initial", "steps") %in% names(r))) {
      next
    }
    r <- translate(r)
    states <- c(list(r$initial), lapply(r$steps, `[[`, "state"))
    names(states) <- paste0(basename(f), "#", seq_along(states) - 1L)
    out <- c(out, states)
  }
  out
}

expect_spec_takes <- function(block, states) {
  spec <- viz_block_spec(block)
  expect_gt(length(states), 0L)
  for (nm in names(states)) {
    st <- legacy_restore(block, states[[nm]], version = "0.3.0")
    checked <- expect_silent(viz_check(spec, st))
    wire <- jsonlite::fromJSON(viz_wire(spec, checked),
                               simplifyVector = FALSE)
    back <- viz_read_set(spec, list(rev = 0, values = wire))
    expect_identical(back$rejected, character(), label = nm)
    expect_identical(back$values, checked, label = nm)
  }
}

skip_until_written <- function(block) {
  ns <- asNamespace("blockr.viz")
  for (fn in c(paste0(block, "_spec"), "legacy_restore")) {
    skip_if_not(exists(fn, envir = ns, inherits = FALSE),
                paste0(fn, "() is not written yet"))
  }
}

test_that("every recorded chart state passes the chart spec", {
  skip_until_written("chart")
  expect_spec_takes("chart", recorded_states("chart-record"))
})

test_that("every recorded table state passes the table spec", {
  skip_until_written("table")
  vocab <- list(args = TRUE, summary_table = TRUE)
  expect_spec_takes("table", recorded_states(
    "table-golden", function(r) table_golden_translate(r, vocab)
  ))
})
