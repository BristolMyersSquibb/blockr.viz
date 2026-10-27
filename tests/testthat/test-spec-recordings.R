# The block specs against the recorded behaviour of the reference: every
# state a recorded scenario went through must pass the spec's constructor
# check, and must come back unchanged through the browser's wire. Runs once
# the block's spec exists.

recorded_states <- function(dir, translate = identity) {
  files <- list.files(test_path("fixtures", dir), "\\.rds$",
                      full.names = TRUE)
  out <- list()
  for (f in files) {
    r <- readRDS(f)
    if (!is.list(r) || !all(c("initial", "steps") %in% names(r))) {
      next
    }
    states <- c(list(r$initial), lapply(r$steps, `[[`, "state"))
    names(states) <- paste0(basename(f), "#", seq_along(states) - 1L)
    out <- c(out, lapply(states, translate))
  }
  out
}

expect_spec_takes <- function(spec, states) {
  expect_gt(length(states), 0L)
  for (nm in names(states)) {
    st <- states[[nm]]
    checked <- expect_silent(viz_check(spec, st))
    wire <- jsonlite::fromJSON(viz_wire(spec, checked),
                               simplifyVector = FALSE)
    back <- viz_read_set(spec, list(rev = 0, values = wire))
    expect_identical(back$rejected, character(), label = nm)
    expect_identical(back$values, checked, label = nm)
  }
}

spec_or_skip <- function(block) {
  fn <- paste0(block, "_spec")
  skip_if_not(exists(fn, envir = asNamespace("blockr.viz"), inherits = FALSE),
              paste0(fn, "() is not written yet"))
  viz_block_spec(block)
}

test_that("every recorded chart state passes the chart spec", {
  spec <- spec_or_skip("chart")
  expect_spec_takes(spec, recorded_states("chart-record"))
})

test_that("every recorded table state passes the table spec", {
  spec <- spec_or_skip("table")
  vocab <- list(args = TRUE, summary_table = TRUE)
  expect_spec_takes(spec, recorded_states(
    "table-golden", function(st) table_golden_translate(st, vocab)
  ))
})
