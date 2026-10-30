# Writes legacy-script-block.json: a summarize table saved by blockr.viz
# 0.2.200, the version production boards ran, whose subtitle names its prepare
# script's values (`{@min_share}`) across a line break. apps/viz-flows restores
# it, so test-e2e-flows.R checks such a block on the rewrite: the words are the
# controls and the gear's Titles row loads unedited ("not applied" hidden).
#
#   R_LIBS=<library with blockr.viz 0.2.200 (3ea4c47) and its blockr.core>
#     Rscript make-legacy-script-block.R
library(blockr.core)
library(blockr.viz)
stopifnot(packageVersion("blockr.viz") < "0.3.0")

blk <- new_summarize_table_block(
  by = "ARM",
  summaries = list(list(type = "simple", name = "n", func = "count_distinct",
                        col = "USUBJID", show = "number")),
  script = paste(
    "min_share <- 1  #| number(min = 0, max = 100, label = 'Minimum threshold')",
    "share_unit <- factor('Patients', levels = c('Patients', 'Percent'))  #| label = 'Threshold unit'",
    "rows <- factor('Arm', levels = c('Arm', 'Arm and sex'))  #| label = 'Rows'",
    "data[data$AGE >= 50 + min_share, , drop = FALSE]",
    sep = "\n"
  ),
  values = list(min_share = 2),
  # A real line break, as a template written in R code has one.
  subtitle = "By: {@rows}\n[with at least {@min_share} {@share_unit}]",
  title = "Subjects by arm (0.2.x)",
  block_name = "Legacy script"
)

out <- file.path(dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1L]))),
  "legacy-script-block.json")
writeLines(typedjson::json_write_str(blockr_ser(blk)), out)
message("blockr.viz ", packageVersion("blockr.viz"), ": wrote ", out)
