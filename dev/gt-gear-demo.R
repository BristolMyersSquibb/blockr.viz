# Summary table -> gt table demo — the same "Table 1" summary rendered as a
# static / publication-style gt table. The gt block's panel carries its config
# form (Title / Subtitle / NA / toggles).
#
# Run from the workspace root (works inside or outside the dev container):
#   Rscript blockr.viz/dev/gt-gear-demo.R
# open the local URL serve() prints (or uncomment the options line to pin 3838).

options(blockr.html_table_preview = TRUE)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.viz")

board <- new_dock_board(
  blocks = c(
    data = new_dataset_block("CO2"),
    summ = new_summary_table_block(state = list(
      vars = list("uptake", "Type"),
      by   = list("Treatment"),
      add_overall = TRUE
    )),
    gt = new_gt_table_block(title = "Demographics")
  ),
  links = links(
    from = c("data", "summ"),
    to   = c("summ", "gt")
  ),
  extensions = list(blockr.dag::new_dag_extension())
)

# options(shiny.port = 3838L, shiny.host = "0.0.0.0")  # uncomment to pin
serve(board)
