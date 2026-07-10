# Table-block control section: does it paint before the data arrives?
#
# Reproduces the production complaint: switching to a view whose blocks are
# dormant, the filter block appears instantly (static JS-hydrated DOM) while
# the table block's control section used to wait for the whole upstream chain
# to evaluate, because `output$dt_result` req()'d `dt_is_structured(data())`.
#
# The Setup view is the landing view, so everything in Report starts dormant.
# Switch to Report and watch: the search box, gear and scroll container are
# already there; only the table body streams in.
#
# Throttle the browser (DevTools > Network: Slow 3G, Performance: 6x CPU) to
# feel it the way production feels it.
#
# Run from the workspace root:
#   Rscript blockr.viz/dev/preview-view-switch-latency.R [port]

port <- as.integer(Sys.getenv("BLOCKR_PORT", unset = "3838"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) port <- as.integer(args[[1]])

root <- if (file.exists("blockr.viz/DESCRIPTION")) "." else ".."
for (p in c("blockr.core", "blockr.dplyr", "blockr.viz", "blockr.dock")) {
  pkgload::load_all(file.path(root, p), quiet = TRUE)
}

# No DAG extension here on purpose: an installed blockr.dag against a
# load_all()'d blockr.dock errors in register_actions(). Unrelated to what this
# script demonstrates.
options(shiny.port = port)

serve(
  new_dock_board(
    blocks = c(
      data = new_dataset_block(dataset = "mtcars", package = "datasets"),
      # JS-driven block: static div, hydrated client-side. The reference for
      # "appears immediately, populated".
      filt = new_filter_block(block_name = "Filter"),
      # summary_table -> table = the STRUCTURED (Table-1) path, the one prod
      # hits: section headers, two-tier arm headers, stub indent.
      summ = new_summary_table_block(vars = c("mpg", "hp", "disp"), by = "cyl"),
      structured_tbl = new_table_block(block_name = "Structured table (Table 1)"),
      # A FLAT table off the same filter, to confirm it stays plain (no
      # Table-1 delta CSS leaking onto it).
      flat_tbl = new_table_block(block_name = "Flat table")
    ),
    links = list(
      list(from = "data", to = "filt", input = "data"),
      list(from = "filt", to = "summ", input = "data"),
      list(from = "summ", to = "structured_tbl", input = "data"),
      list(from = "filt", to = "flat_tbl", input = "data")
    ),
    grids = list(
      Setup    = dock_grid("filt"),
      Report   = dock_grid(
        panels("structured_tbl", "flat_tbl", active = "structured_tbl")
      )
    ),
    # Land on Setup so every Report block starts dormant -- that is the state
    # a production view switch wakes up.
    active = "Setup"
  )
)
