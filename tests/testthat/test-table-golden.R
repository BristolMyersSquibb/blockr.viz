# The table block replayed against its recordings (helper-table-golden.R,
# written by dev/table-golden-record.R on the reference). The rewritten
# table is fed the same steps through the kit's inputs
# (helper-table-rewrite.R). A difference is a change in what the block
# holds, emits, sends downstream or shows the reader. Each part of a step is
# its own expectation, so a failure names the edge that moved.

tg_rw_expect <- function(got, want, label) {
  expect_identical(record_rw_sorted(got$ctor_warnings),
                   record_rw_sorted(want$ctor_warnings),
                   label = paste(label, "constructor warnings"))
  expect_identical(got$initial, want$initial,
                   label = paste(label, "starting state"))
  expect_identical(got$saved, want$saved, label = paste(label, "saved state"))
  expect_identical(length(got$steps), length(want$steps),
                   label = paste(label, "steps"))
  for (i in seq_along(want$steps)) {
    g <- tg_rw_compare(got$steps[[i]], want$steps[[i]])
    for (part in names(want$steps[[i]])) {
      expect_identical(g[[part]], want$steps[[i]][[part]],
                       label = sprintf("%s step %d %s", label, i, part))
    }
  }
}

record_rw_sorted <- function(x) sort(as.character(x))

for (nm in names(table_golden_scenarios())) {
  local({
    nm <- nm
    sc <- table_golden_scenarios()[[nm]]
    test_that(paste("table recording:", nm), {
      path <- file.path(table_golden_dir(), paste0(nm, ".rds"))
      skip_if_not(file.exists(path), "no recording")
      got <- tg_rw_run(sc)
      want <- tg_rw_apply_known(tg_rw_expected(readRDS(path)), got, nm, sc)
      tg_rw_expect(got, want, nm)
    })
  })
}

# Code with a value an argument does not take fails now, where the
# reference healed it (behaviour-changes.md row 7); old names warn
# (api-proposal.md section 2). What stays: the calls that work still work.
test_that("table recording: constructor cases", {
  path <- file.path(table_golden_dir(), "ctor-cases.rds")
  skip_if_not(file.exists(path), "no recording")
  want <- readRDS(path)
  # An unknown name is a block attribute, as on the chart (blockr.dock's
  # `visible`; behaviour-changes.md row 28), so `unknown_arg` builds.
  fails_now <- c("bad_layout", "bad_look", "bad_pct_of", "bad_missing",
                 "bad_format", "format_clamp")
  for (nm in names(want)) {
    res <- tryCatch(suppressWarnings(table_golden_ctor(
      tg_rw_args(table_golden_ctor_cases()[[nm]])
    )), error = function(e) e)
    if (nm %in% fails_now) {
      expect_s3_class(res, "error")
    } else {
      expect_s3_class(res, "table_block")
      expect_identical(
        blockr.core::blockr_ser(blockr.core::blockr_deser(
          blockr.core::blockr_ser(res)
        ))$payload,
        blockr.core::blockr_ser(res)$payload,
        label = paste("ctor", nm, "round trip")
      )
    }
  }
})

test_that("table recording: restored boards", {
  path <- file.path(table_golden_dir(), "restore-cases.rds")
  skip_if_not(file.exists(path), "no recording")
  want <- readRDS(path)
  for (nm in names(want)) {
    case <- table_golden_restore_cases()[[nm]]
    got <- tg_rw_restore_run(case)
    w <- tg_rw_restore_apply(tg_rw_restore_expected(want[[nm]], case), got,
                             nm)
    for (k in c("class", "saved", "constructor")) {
      expect_identical(got[[k]], w[[k]], label = paste("restore", nm, k))
    }
    g <- tg_rw_compare(got$steps[[1L]], w$steps[[1L]])
    for (part in names(w$steps[[1L]])) {
      expect_identical(g[[part]], w$steps[[1L]][[part]],
                       label = paste("restore", nm, part))
    }
  }
})
