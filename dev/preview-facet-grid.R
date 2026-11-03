# Preview: a panel grid (facet_row) on unaggregated lab data.
#
#   Rscript dev/preview-facet-grid.R [port]
#
# Run from the blockr.viz checkout. One line per subject, AVAL over AVISITN,
# rows by PARAM (each its own unit, so y is free per row) and columns by
# treatment. The "Wrap" view is the same chart without facet_row, as before.
# The "Bar" view is an aggregated chart in a grid.
#
# Dependencies come from ~/.cache/blockr-prodlike-lib when it exists; only
# blockr.viz is loaded from source.
lib <- path.expand("~/.cache/blockr-prodlike-lib")
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
port <- as.integer(c(commandArgs(TRUE), Sys.getenv("BLOCKR_PORT"), "4010")[1])
options(shiny.port = port, shiny.host = "127.0.0.1")
options(blockr.dock_is_locked = FALSE)

library(blockr.core)
library(blockr.dock)
pkgload::load_all(".", quiet = TRUE)

params <- c("Alanine Aminotransferase (U/L)",
            "Aspartate Aminotransferase (U/L)",
            "Creatinine (umol/L)",
            "Albumin (g/L)")
lb <- pharmaverseadam::adlb
lb <- lb[lb$PARAM %in% params & grepl("^(Baseline|Week )", lb$AVISIT),
         c("USUBJID", "TRTA", "PARAM", "AVISIT", "AVISITN", "AVAL")]
# One combination without rows, so the grid shows an empty cell.
lb <- lb[!(lb$PARAM == "Albumin (g/L)" & lb$TRTA == "Xanomeline Low Dose"), ]
lb <- as.data.frame(lb)

board <- new_dock_board(
  blocks = c(
    labs = new_static_block(lb, block_name = "ADLB, four parameters"),
    grid = new_chart_block(
      type = "line", x = "AVISITN", y = "AVAL", series = "USUBJID",
      facet = "TRTA", facet_row = "PARAM", facet_scales = "free_y",
      line_width = 0.5, title = "Lab values by visit",
      subtitle = "rows: parameter, columns: treatment",
      block_name = "Grid"),
    wrap = new_chart_block(
      type = "line", x = "AVISITN", y = "AVAL", series = "USUBJID",
      facet = "TRTA", facet_scales = "free_y", line_width = 0.5,
      title = "Lab values by visit", block_name = "Wrap"),
    bar = new_chart_block(
      type = "bar", category = "AVISIT", value = "AVAL", func = "mean",
      orientation = "vertical", sort_by = "AVISITN", facet = "TRTA",
      facet_row = "PARAM", facet_scales = "free_y", block_name = "Bar")
  ),
  links = links(from = c("labs", "labs", "labs"),
                to = c("grid", "wrap", "bar")),
  grids = list(Grid = dock_grid("grid"), Wrap = dock_grid("wrap"),
               Bar = dock_grid("bar")),
  active = "Grid"
)

message("http://127.0.0.1:", port)
serve(board)
