# Writes tests/testthat/fixtures/board-0.2.200.json: a board saved by
# blockr.viz 0.2.200, the version in production, with a click held on a bar
# chart, a summarize table and a table (`filter_*` is what the Save button
# writes for a click) and a prepare script set away from its default.
# test-e2e-flows.R restores it in the browser.
#
# Needs 0.2.200 (3ea4c47) in a library of its own, with the blockr.core it
# was deployed with:
#
#   R_LIBS=<that library> Rscript dev/board-0.2.200.R
#
# Run from the package root.

library(blockr.core)
library(blockr.viz)
stopifnot(packageVersion("blockr.viz") == "0.2.200")

set.seed(1)
n <- 120
ae <- data.frame(
  USUBJID = sprintf("S%02d", sample(1:30, n, TRUE)),
  ARM = sample(c("Placebo", "Low", "High", NA), n, TRUE, prob = c(.3, .3, .3, .1)),
  AETERM = sample(c("Rash", "Nausea", "Fever", "Cough", "Headache"), n, TRUE),
  AESEV = sample(c("MILD", "MODERATE", "SEVERE"), n, TRUE),
  ADY = round(runif(n, 1, 90)),
  ADT = as.Date("2024-01-01") + sample(0:60, n, TRUE),
  AVAL = round(rnorm(n, 30, 5), 1),
  stringsAsFactors = FALSE
)
script <- paste(
  "keep <- 3 #| number(min = 1, max = 5, label = 'Terms kept')",
  "dplyr::filter(data, AETERM %in% sort(unique(AETERM))[seq_len(keep)])",
  sep = "\n"
)

b <- new_board(
  blocks = c(
    d = new_static_block(ae),
    c_bar = new_chart_block(
      chart_type = "bar", group = "AETERM", color = "AESEV", drill = "AETERM",
      title = "AEs by {@group}",
      filter_column = "AETERM", filter_values = list("Rash")
    ),
    c_date = new_chart_block(chart_type = "scatter", x = "ADT", y = "AVAL",
                             drill = "auto"),
    c_script = new_chart_block(chart_type = "bar", group = "AETERM",
                               script = script, values = list(keep = 2)),
    t_grp = new_summarize_table_block(
      group = "ARM", top_n = 3L, drill = "ARM",
      subtitle = "By {@group}, top {@top_n}",
      filter_column = "ARM", filter_values = list("Low")
    ),
    t_rec = new_table_block(
      rowname = "USUBJID", values = c("ARM", "AETERM"), drill = "USUBJID",
      filter_column = "USUBJID", filter_values = list("S04")
    )
  ),
  links = c(new_link("d", "c_bar"), new_link("d", "c_date"),
            new_link("d", "c_script"), new_link("d", "t_grp"),
            new_link("d", "t_rec"))
)

writeLines(typedjson::json_write_str(blockr_ser(b)),
           "tests/testthat/fixtures/board-0.2.200.json")
