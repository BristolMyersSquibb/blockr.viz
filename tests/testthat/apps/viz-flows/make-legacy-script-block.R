# Writes two summarize tables saved by blockr.viz 0.2.200, the version
# production boards ran, for apps/viz-flows and test-e2e-flows.R:
#
# - legacy-script-block.json: a subtitle that names its prepare script's
#   values (`{@min_share}`) across a line break. The words are the controls,
#   and the gear's Titles row loads unedited ("not applied" hidden).
# - legacy-group-block.json: `By {@group}, top {@top_n}`, both words links on
#   0.2.x. Restore names the first `{@by}`; both stay live.
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

# A summarize table whose subtitle names its row column and its top n, the
# words 0.2.x drew as links: legacy-group-block.json.
grp <- new_summarize_table_block(
  by = "SEX",
  summaries = list(list(type = "simple", name = "n", func = "count_distinct",
                        col = "USUBJID", show = "bar")),
  top_n = 2,
  subtitle = "By {@group}, top {@top_n}",
  title = "Subjects by sex (0.2.x)",
  block_name = "Legacy words"
)

here <- dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])))
for (x in list(list(blk, "legacy-script-block.json"),
               list(grp, "legacy-group-block.json"))) {
  out <- file.path(here, x[[2L]])
  writeLines(typedjson::json_write_str(blockr_ser(x[[1L]])), out)
  message("blockr.viz ", packageVersion("blockr.viz"), ": wrote ", out)
}
