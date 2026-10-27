# The chart block replayed against its recordings (helper-chart-golden.R,
# written by dev/chart-golden-record.R). A difference here is a change in
# what the block sends, emits or holds.

for (nm in names(golden_scenarios())) {
  local({
    nm <- nm
    test_that(paste("chart recording:", nm), {
      path <- file.path(golden_dir(), paste0(nm, ".rds"))
      skip_if_not(file.exists(path), "no recording")
      expect_identical(golden_run(golden_scenarios()[[nm]]), readRDS(path))
    })
  })
}

test_that("chart recording: constructor cases", {
  path <- file.path(golden_dir(), "ctor-cases.rds")
  skip_if_not(file.exists(path), "no recording")
  expect_identical(lapply(golden_ctor_cases(), golden_ctor_run),
                   readRDS(path))
})

test_that("chart recording: registry entry", {
  path <- file.path(golden_dir(), "registry.rds")
  skip_if_not(file.exists(path), "no recording")
  expect_identical(
    list(arguments = chart_arguments(), guidance = chart_guidance()),
    readRDS(path)
  )
})
