# Local dev runner: table-renderer comparison.
#
# Same deps-from-source setup as blockr.cdex/dev/run-view.R, so it runs anywhere
# the monorepo is checked out (inside OR outside the dev container) -- just run
# it from the workspace root, where the blockr.* packages are sibling dirs.
#
# Usage (serves on 3838, falls back to a random free port if taken):
#   Rscript blockr.bi/dev/table-renderer-compare.R
#
# Shows ONE summary_table (the CEDX demographics config: AGE/SEX/RACE/DTHFL by
# TRT, + overall) feeding three renderers stacked top-to-bottom, to compare the
# same clinical "Table 1" across:
#   - gt_table    : static / publication style (current CEDX renderer)
#   - html_table  : dashboard-native static ("gt alternative", same nesting)
#   - table_block : interactive drilldown (the convergence target)

root <- "."

host <- "0.0.0.0"
port <- tryCatch(
  httpuv::randomPort(min = 3838L, max = 3838L, n = 1L, host = host),
  error = function(e) httpuv::randomPort(host = host)
)

options(
  shiny.autoload.r = FALSE,
  shiny.maxRequestSize = 50 * 1024^2,
  shiny.port = port,
  shiny.host = host
)

deps <- c(
  "blockr.core", "blockr.dag", "blockr.dock", "blockr.dm",
  "blockr.dplyr", "blockr.io", "blockr.pharma", "blockr.stats",
  "blockr.extra", "blockr.bi", "blockr.ai", "blockr.session",
  "blockr.theme", "blockr.ui"
)
for (d in deps) {
  pkgload::load_all(
    file.path(root, d),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
}

# Realistic demographics input (same vars CEDX uses for pop_demog).
adsl <- pharmaverseadam::adsl
demog <- adsl[, c("USUBJID", "AGE", "SEX", "RACE", "DTHFL", "TRT01A")]
names(demog)[names(demog) == "TRT01A"] <- "TRT"

board <- new_dock_board(
  blocks = c(
    data  = new_static_block(demog),
    demog = new_summary_table_block(
      state = list(vars = c("AGE", "SEX", "RACE", "DTHFL"),
                   by = "TRT", add_overall = TRUE, id_var = "USUBJID"),
      block_name = "Demographics summary (summary_table)"),
    gt    = new_gt_table_block(block_name = "gt_table - static / publication"),
    html  = new_html_table_block(block_name = "html_table - dashboard-native static"),
    drill = new_table_block(block_name = "table_block - interactive drilldown")
  ),
  links = c(
    new_link("data",  "demog", "data"),
    new_link("demog", "gt",    "data"),
    new_link("demog", "html",  "data"),
    new_link("demog", "drill", "data")
  ),
  extensions = list(blockr.dag::new_dag_extension())
)

message("Serving TABLE RENDERER COMPARISON (dock + dag) on http://127.0.0.1:", port, "/")
serve(board)
