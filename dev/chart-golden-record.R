# Writes the chart block recordings that tests/testthat/test-chart-golden.R
# replays. Run from the package root:
#
#   Rscript dev/chart-golden-record.R            # every scenario
#   Rscript dev/chart-golden-record.R bar_count  # some
#
# Re-record only when a change in behaviour is intended, and read the diff
# the test printed first.

pkgload::load_all(".", quiet = TRUE)
source("tests/testthat/helper-chart-golden.R")

dir <- file.path("tests", "testthat", "fixtures", "chart-golden")
dir.create(dir, recursive = TRUE, showWarnings = FALSE)

which <- commandArgs(trailingOnly = TRUE)
scs <- golden_scenarios()
if (length(which)) scs <- scs[which]

for (nm in names(scs)) {
  rec <- golden_run(scs[[nm]])
  saveRDS(rec, file.path(dir, paste0(nm, ".rds")), version = 3)
  cat(nm, ":", length(rec$steps), "steps\n")
}

if (!length(which)) {
  ctor <- lapply(golden_ctor_cases(), golden_ctor_run)
  saveRDS(ctor, file.path(dir, "ctor-cases.rds"), version = 3)
  reg <- list(arguments = chart_arguments(), guidance = chart_guidance())
  saveRDS(reg, file.path(dir, "registry.rds"), version = 3)
  cat("ctor cases and registry\n")
}
