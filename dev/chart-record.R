# Writes the chart recordings that tests/testthat/test-chart-record.R
# replays. Run from the package root, with the blockr.core the reference
# needs (origin/main, for core#379):
#
#   Rscript dev/chart-record.R            # every scenario
#   Rscript dev/chart-record.R bar_count  # some
#
# Re-record only for a change Christoph accepted, and note it in
# _scratch/viz-cran-review/features-chart.md.

pkgload::load_all(".", quiet = TRUE)
source("tests/testthat/helper-chart-reference.R")
source("tests/testthat/helper-chart-record.R")

dir <- file.path("tests", "testthat", "fixtures", "chart-record")
dir.create(dir, recursive = TRUE, showWarnings = FALSE)

which <- commandArgs(trailingOnly = TRUE)
scs <- record_scenarios()
if (length(which)) scs <- scs[which]

for (nm in names(scs)) {
  rec <- record_run(scs[[nm]])
  saveRDS(rec, file.path(dir, paste0(nm, ".rds")), version = 3)
  cat(nm, ":", length(rec$steps), "steps\n")
}

if (!length(which)) {
  ctor <- lapply(record_ctor_cases(), record_ctor_run)
  saveRDS(ctor, file.path(dir, "ctor-cases.rds"), version = 3)
  reg <- list(arguments = chart_arguments(), guidance = chart_guidance())
  saveRDS(reg, file.path(dir, "registry.rds"), version = 3)
  cat("ctor cases and registry\n")
}
