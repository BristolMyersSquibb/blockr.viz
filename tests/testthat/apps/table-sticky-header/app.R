# Three tables taller than their panel, one per view, for
# test-table-sticky-header.R: a flat records table, a structured table with
# a spanner row over its leaf headers (the shape of a composer table), and a
# groups table.
#
# Loads blockr.viz from BLOCKR_VIZ_SRC (the source tree under test) when set,
# and blockr.ui from BLOCKR_UI_SRC.

ui_src <- Sys.getenv("BLOCKR_UI_SRC")
if (nzchar(ui_src)) pkgload::load_all(ui_src, quiet = TRUE)
src <- Sys.getenv("BLOCKR_VIZ_SRC")
if (nzchar(src)) pkgload::load_all(src, quiet = TRUE) else library(blockr.viz)
library(blockr.core)
library(blockr.dock)

set.seed(1)
n <- 150
flat <- data.frame(
  USUBJID = sprintf("S%03d", seq_len(n)),
  ARM = sample(c("Placebo", "Drug X"), n, TRUE),
  SITE = sprintf("Site %02d", sample(1:60, n, TRUE)),
  AGE = sample(20:80, n, TRUE),
  stringsAsFactors = FALSE
)

# Two header rows, "Placebo" spanning its F and M columns, and two
# sections, which make it structured.
k <- 80
spanned <- data.frame(
  .label = sprintf("Row %02d", seq_len(k)),
  .group1_level = rep(c("Sex", "Age"), each = k / 2),
  `Placebo||F` = as.character(seq_len(k)),
  `Placebo||M` = as.character(seq_len(k) * 2L),
  Total = as.character(seq_len(k) * 3L),
  check.names = FALSE, stringsAsFactors = FALSE
)
attr(spanned$.label, "label") <- "Characteristic"
attr(spanned, "spanner_labels") <- c(Placebo = "Placebo\nN = 80")

serve(new_dock_board(
  blocks = c(
    d = new_static_block(flat, block_name = "Subjects"),
    s = new_static_block(spanned, block_name = "Spanned"),
    flat = new_table_block(block_name = "Flat"),
    span = new_table_block(block_name = "Spanner"),
    grp = new_table_block(layout = "groups", by = "SITE", block_name = "Groups")
  ),
  links = links(from = c("d", "s", "d"), to = c("flat", "span", "grp")),
  views = list(
    flat = dock_view("flat", name = "flat"),
    span = dock_view("span", name = "span"),
    grp = dock_view("grp", name = "grp")
  ),
  active = "flat"
))
