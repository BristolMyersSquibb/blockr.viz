# The table block replayed against its recordings (helper-table-golden.R,
# written by dev/table-golden-record.R). A difference here is a change in
# what the block holds, emits, sends or writes. Each part of a step is its
# own expectation, so a failure names the edge that moved: `state`, `expr`
# and `result` (what downstream sees), `messages` and `outputs` (what the
# browser gets), `ctrl` (control-channel sends), `download` (the files).

table_golden_expect <- function(got, want, label) {
  expect_identical(got$ctor_warnings, want$ctor_warnings,
                   label = paste(label, "constructor warnings"))
  expect_identical(got$initial, want$initial,
                   label = paste(label, "starting state"))
  expect_identical(got$saved, want$saved, label = paste(label, "saved state"))
  expect_identical(length(got$steps), length(want$steps),
                   label = paste(label, "steps"))
  for (i in seq_along(want$steps)) {
    for (part in names(want$steps[[i]])) {
      expect_identical(got$steps[[i]][[part]], want$steps[[i]][[part]],
                       label = sprintf("%s step %d %s", label, i, part))
    }
  }
}

for (nm in names(table_golden_scenarios())) {
  local({
    nm <- nm
    test_that(paste("table recording:", nm), {
      path <- file.path(table_golden_dir(), paste0(nm, ".rds"))
      skip_if_not(file.exists(path), "no recording")
      table_golden_expect(
        table_golden_run(table_golden_scenarios()[[nm]]),
        table_golden_translate(readRDS(path)),
        nm
      )
    })
  })
}

test_that("table recording: constructor cases", {
  path <- file.path(table_golden_dir(), "ctor-cases.rds")
  skip_if_not(file.exists(path), "no recording")
  want <- table_golden_translate(readRDS(path))
  got <- lapply(table_golden_ctor_cases(), table_golden_ctor_run)
  expect_identical(names(got), names(want))
  for (nm in names(want)) {
    expect_identical(got[[nm]], want[[nm]], label = paste("ctor", nm))
  }
})

test_that("table recording: restored boards", {
  path <- file.path(table_golden_dir(), "restore-cases.rds")
  skip_if_not(file.exists(path), "no recording")
  want <- table_golden_translate(readRDS(path))
  got <- lapply(table_golden_restore_cases(), table_golden_restore_run)
  expect_identical(names(got), names(want))
  for (nm in names(want)) {
    for (part in names(want[[nm]])) {
      expect_identical(got[[nm]][[part]], want[[nm]][[part]],
                       label = paste("restore", nm, part))
    }
  }
})

test_that("table recording: registry entry", {
  path <- file.path(table_golden_dir(), "registry.rds")
  skip_if_not(file.exists(path), "no recording")
  expect_identical(
    list(arguments = table_arguments(), guidance = table_guidance()),
    readRDS(path)
  )
})
