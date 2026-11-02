# Composer tables (interactive_table() on an annotated frame), for
# test-table-strong-rows.R: a bold parameter row above two normal visit
# rows, and a gtsummary-style table whose variable rows have no values.
#
# Loads blockr.viz from BLOCKR_VIZ_SRC (the source tree under test) when set.

src <- Sys.getenv("BLOCKR_VIZ_SRC")
if (nzchar(src)) pkgload::load_all(src, quiet = TRUE) else library(blockr.viz)

x <- data.frame(
  .label = c("Alanine Aminotransferase (U/L)", "Baseline", "Week 4"),
  .indent = c(0L, 1L, 1L),
  .strong = c(TRUE, FALSE, FALSE),
  `Value||N` = c("200", "198", "190"),
  `Value||Mean` = c("24.1", "23.8", "25.0"),
  stringsAsFactors = FALSE, check.names = FALSE
)

t1 <- data.frame(
  .label = c("AGE (years)", "Mean (SD)", "Min, Max", "SEX", "F", "M"),
  .indent = c(0L, 1L, 1L, 0L, 1L, 1L),
  Active = c(NA, "61.4 (9.8)", "34, 82", NA, "58 (49%)", "60 (51%)"),
  Placebo = c(NA, "60.9 (10.3)", "29, 84", NA, "61 (51%)", "59 (49%)"),
  stringsAsFactors = FALSE
)

shiny::shinyApp(
  shiny::fluidPage(
    interactive_table(x, drill = FALSE, elem_id = "composer"),
    interactive_table(t1, drill = FALSE, elem_id = "t1")
  ),
  function(input, output, session) {}
)
