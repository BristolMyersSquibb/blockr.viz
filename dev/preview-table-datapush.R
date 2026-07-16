# Table data-push + windowed rendering preview (dev/table-data-push-design.md).
#
# Three tables over one 10k-row frame in a dock board:
#   big    — 10k flat rows, drill on USUBJID: the WINDOWED path. Scroll it;
#            only ~100 rows are in the DOM at any time (inspect the tbody).
#            Search and sort act on the client model, no R round trip.
#   heat   — same frame, diverging heatmap shading (style-chunk transport).
#   small  — 6-row aggregate with group drill (the non-windowed model path).
#
# What to look for vs the renderUI era: the body appears without a Shiny
# output swap, a drill click updates downstream without re-shipping the
# table, and scroll position + search text survive a gear edit.
#
# Run from the workspace root:
#   Rscript blockr.viz/dev/preview-table-datapush.R [port]

port <- as.integer(Sys.getenv("BLOCKR_PORT", unset = "3838"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) port <- as.integer(args[[1]])

root <- if (file.exists("blockr.viz/DESCRIPTION")) "." else ".."
for (p in c("blockr.core", "blockr.dplyr", "blockr.dock")) {
  pkgload::load_all(file.path(root, p), quiet = TRUE)
}
# blockr.viz from THIS script's tree (works from the feat/table-data-push
# worktree as well as the main checkout).
viz_dir <- normalizePath(file.path(
  dirname(sub("--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])),
  ".."
))
pkgload::load_all(viz_dir, quiet = TRUE)

set.seed(42)
n <- 10000L
big_df <- data.frame(
  USUBJID = sprintf("SUBJ-%05d", seq_len(n)),
  SITE    = sample(sprintf("Site %02d", 1:20), n, replace = TRUE),
  ARM     = sample(c("Placebo", "Low dose", "High dose"), n, replace = TRUE),
  AGE     = sample(18:85, n, replace = TRUE),
  BMI     = round(rnorm(n, 26, 4), 3),
  CHG     = round(rnorm(n, 0, 2), 4),
  stringsAsFactors = FALSE
)
big_df$CHG[sample(n, 200)] <- NA  # NA cells: em-dash + nodrill coverage

options(shiny.port = port)

serve(
  new_dock_board(
    blocks = c(
      data  = new_static_block(data = big_df),
      big   = new_table_block(rowname = "USUBJID", drill = "USUBJID"),
      heat  = new_table_block(
        rowname = "USUBJID",
        shadings = list(list(mode = "diverging", cols = list("CHG")))
      ),
      small = new_table_block(
        group = "ARM",
        summaries = list(list(func = "mean", cols = list("BMI"))),
        drill = "auto"
      )
    ),
    links = c(
      new_link("data", "big", "data"),
      new_link("data", "heat", "data"),
      new_link("data", "small", "data")
    )
  )
)
