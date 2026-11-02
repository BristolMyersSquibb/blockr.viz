# A composer table (interactive_table() on an annotated frame) with a bold
# parameter row above two normal visit rows, for test-table-strong-rows.R.
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

shiny::shinyApp(
  shiny::fluidPage(interactive_table(x, drill = FALSE, elem_id = "composer")),
  function(input, output, session) {}
)
