# The chart block replayed against its recordings (helper-chart-record.R,
# written by dev/chart-record.R). A difference is a change in what the
# block holds, emits, sends downstream or shows.

target <- record_target()

for (nm in names(record_scenarios())) {
  local({
    nm <- nm
    sc <- record_scenarios()[[nm]]
    test_that(paste("chart recording:", nm), {
      path <- file.path(record_dir(), paste0(nm, ".rds"))
      skip_if_not(file.exists(path), "no recording")
      skip_if(isTRUE(sc$wire) && !target$wire,
              "the target speaks other browser messages")
      got <- record_run(sc, target)
      want <- readRDS(path)
      if (!target$wire) {
        got <- record_public(got)
        want <- record_public(want)
      }
      expect_identical(got, want)
    })
  })
}

test_that("chart recording: constructor cases", {
  path <- file.path(record_dir(), "ctor-cases.rds")
  skip_if_not(file.exists(path), "no recording")
  expect_identical(lapply(record_ctor_cases(), record_ctor_run, target),
                   readRDS(path))
})

test_that("chart recording: registry entry", {
  path <- file.path(record_dir(), "registry.rds")
  skip_if_not(file.exists(path), "no recording")
  expect_identical(
    list(arguments = chart_arguments(), guidance = chart_guidance()),
    readRDS(path)
  )
})
