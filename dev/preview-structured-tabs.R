# Mini repro: several structured ("Table 1") tables as dock TABS -- the
# cedx-style setup (there the annotated frames come from composer /
# summary chains; summary_table_block emits the same structured shape, so
# this is the same rendering path with none of the cedx data plumbing).
#
# Flip between the Table tabs and watch the body:
#   - viz from THIS tree (default): the body is NOT redrawn on a flip --
#     the data-push table's DOM survives, and the payload guard suppresses
#     identical re-sends. A section you collapsed stays collapsed.
#   - VIZ_DIR=/workspace/blockr.viz (the renderUI-era table): the body
#     re-renders on tab activations (the redraw seen in cedx run-view).
#
# Run from the workspace root (serves on 3838):
#   Rscript /workspace/_scratch/wt-table-datapush/dev/preview-structured-tabs.R
#   VIZ_DIR=/workspace/blockr.viz Rscript /workspace/_scratch/wt-table-datapush/dev/preview-structured-tabs.R

port <- as.integer(Sys.getenv("BLOCKR_PORT", unset = "3838"))

for (p in c("blockr.core", "blockr.dplyr", "blockr.dock", "blockr.ui")) {
  pkgload::load_all(file.path("/workspace", p), quiet = TRUE)
}
viz_default <- normalizePath(file.path(
  dirname(sub("--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])),
  ".."
))
viz_dir <- Sys.getenv("VIZ_DIR", unset = viz_default)
message("blockr.viz from: ", viz_dir)
pkgload::load_all(viz_dir, quiet = TRUE)

set.seed(1)
adsl <- data.frame(
  ARM  = sample(c("Placebo", "Low dose", "High dose"), 300, replace = TRUE),
  SEX  = sample(c("F", "M"), 300, replace = TRUE),
  RACE = sample(c("WHITE", "BLACK", "ASIAN"), 300, replace = TRUE),
  AGE  = sample(18:85, 300, replace = TRUE),
  BMI  = round(rnorm(300, 26, 4), 1),
  WGT  = round(rnorm(300, 78, 12), 1),
  stringsAsFactors = FALSE
)

options(shiny.port = port)
serve(
  new_dock_board(
    blocks = c(
      data  = new_static_block(data = adsl),
      # three summary chains -> three structured Table-1 tabs
      demo_sum = new_summary_table_block(vars = c("AGE", "SEX"), by = "ARM"),
      demo     = new_table_block(),
      body_sum = new_summary_table_block(vars = c("BMI", "WGT"), by = "ARM"),
      body     = new_table_block(),
      race_sum = new_summary_table_block(vars = "RACE", by = "ARM"),
      race     = new_table_block()
    ),
    links = c(
      new_link("data", "demo_sum", "data"),
      new_link("demo_sum", "demo", "data"),
      new_link("data", "body_sum", "data"),
      new_link("body_sum", "body", "data"),
      new_link("data", "race_sum", "data"),
      new_link("race_sum", "race", "data")
    )
  )
)
