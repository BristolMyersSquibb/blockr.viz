# A lab report as a blockr.page document, with the panel grid (facet_row)
# drawn inline.
#
#   Rscript dev/page-facet-grid.R [port]
#
# Run from the blockr.viz checkout. blockr.page is loaded from its checkout
# next to it (../blockr.page, or ../../blockr.page from a worktree); the
# other packages from ~/.cache/blockr-prodlike-lib when it exists.
lib <- path.expand("~/.cache/blockr-prodlike-lib")
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
port <- as.integer(c(commandArgs(TRUE), Sys.getenv("BLOCKR_PORT"), "4012")[1])

page_dir <- Filter(dir.exists, c("../blockr.page", "../../blockr.page"))[1]
library(blockr.core)
pkgload::load_all(page_dir, quiet = TRUE)
pkgload::load_all(".", quiet = TRUE)

params <- c("Alanine Aminotransferase (U/L)",
            "Aspartate Aminotransferase (U/L)",
            "Creatinine (umol/L)",
            "Albumin (g/L)")
lb <- pharmaverseadam::adlb
lb <- lb[lb$PARAM %in% params & grepl("^(Baseline|Week )", lb$AVISIT),
         c("USUBJID", "TRTA", "PARAM", "AVISIT", "AVISITN", "AVAL")]
lb <- as.data.frame(lb[!is.na(lb$AVAL) & nzchar(lb$TRTA), ])

options(blockr.tabular_display = blockr.ui::html_table_display)

board <- new_page_board(
  blocks = c(
    labs = new_static_block(lb, block_name = "ADLB, four parameters"),
    grid = new_chart_block(
      type = "line", x = "AVISITN", y = "AVAL", series = "USUBJID",
      facet = "TRTA", facet_row = "PARAM", facet_scales = "free_y",
      line_width = 0.5, title = "Lab values by visit",
      subtitle = "One line per subject", block_name = "Lab values by visit", visible = "inputs"),
    wrap = new_chart_block(
      type = "line", x = "AVISITN", y = "AVAL", series = "USUBJID",
      facet = "TRTA", line_width = 0.5, title = "All parameters on one scale",
      block_name = "Without rows", visible = "inputs"),
    bar = new_chart_block(
      type = "bar", category = "AVISIT", value = "AVAL", func = "mean",
      orientation = "vertical", sort_by = "AVISITN", facet = "TRTA",
      facet_row = "PARAM", facet_scales = "free_y",
      title = "Mean by visit", block_name = "Mean by visit", visible = "inputs")
  ),
  links = links(from = c("labs", "labs", "labs"),
                to = c("grid", "wrap", "bar")),
  items = list(
    list(text = "# Liver and kidney labs by treatment"),
    list(text = paste(
      "Four lab parameters from the CDISC pilot study (pharmaverseadam),",
      "baseline to week 26.", length(unique(lb$USUBJID)), "subjects,",
      nrow(lb), "measurements."
    )),
    list(block = "labs"),
    list(section = "Every subject"),
    list(text = paste(
      "Rows are parameters, columns are treatment arms. Each row has its own",
      "y axis, because the parameters have different units. Within a row the",
      "arms share the axis, so they compare directly."
    )),
    list(block = "grid"),
    list(text = paste(
      "Without the rows, all four parameters land in one panel per arm and",
      "share one scale. Albumin and creatinine flatten out next to the",
      "transaminases."
    )),
    list(block = "wrap"),
    list(section = "The means"),
    list(text = paste(
      "The same grid for the mean value at each visit."
    )),
    list(block = "bar")
  )
)

message("blockr.page facet grid demo on http://127.0.0.1:", port)
shiny::runApp(serve(board, id = "page"), port = port, host = "127.0.0.1",
              launch.browser = FALSE)
