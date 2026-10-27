# Writes the table block recordings that tests/testthat/test-table-golden.R
# replays. Run from the package root:
#
#   Rscript dev/table-golden-record.R              # everything
#   Rscript dev/table-golden-record.R rec_drill    # some scenarios
#
# Re-record only when a change in behaviour is accepted, and read the diff
# the test printed first. The saved boards the restore cases read are
# written by dev/table-golden-old-boards.R, with blockr.viz 0.2.223.

pkgload::load_all(".", quiet = TRUE)
source("tests/testthat/helper-table-golden.R")

dir <- file.path("tests", "testthat", "fixtures", "table-golden")
dir.create(dir, recursive = TRUE, showWarnings = FALSE)

which <- commandArgs(trailingOnly = TRUE)
scs <- table_golden_scenarios()
if (length(which)) scs <- scs[intersect(which, names(scs))]

for (nm in names(scs)) {
  rec <- table_golden_run(scs[[nm]])
  saveRDS(rec, file.path(dir, paste0(nm, ".rds")), version = 3)
  cat(nm, ":", length(rec$steps), "steps\n")
}

if (!length(which)) {
  ctor <- lapply(table_golden_ctor_cases(), table_golden_ctor_run)
  saveRDS(ctor, file.path(dir, "ctor-cases.rds"), version = 3)
  rst <- lapply(table_golden_restore_cases(), table_golden_restore_run)
  saveRDS(rst, file.path(dir, "restore-cases.rds"), version = 3)
  reg <- list(arguments = table_arguments(), guidance = table_guidance())
  saveRDS(reg, file.path(dir, "registry.rds"), version = 3)
  cat(length(ctor), "ctor cases,", length(rst), "restore cases, registry\n")
}
