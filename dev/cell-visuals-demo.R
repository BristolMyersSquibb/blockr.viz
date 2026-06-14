# Cell visuals demo — data bars + diverging-centred-on-0 + column scope.
#
# Verifies the new drilldown_table_color() modes:
#   - type = "bar"        in-cell data bars (per-column abs-magnitude)
#   - type = "diverging"  centred on 0 with a symmetric inferred domain
#   - columns = ...       restrict the effect to named columns (empty = all)
#
# Run:  Rscript blockr.viz-cellvis/dev/cell-visuals-demo.R
# Loads THIS worktree's package from source (pkgload::load_all) — works on any
# machine, no install needed, and picks up the R + inst/js changes.

# No fixed port: let Shiny pick a free one and print the URL. Set CELLVIS_PORT
# to pin a specific port if you need a stable address.
local({
  p <- Sys.getenv("CELLVIS_PORT", "")
  if (nzchar(p)) options(shiny.port = as.integer(p))
})
options(shiny.host = "127.0.0.1")
options(blockr.html_table_preview = TRUE)

suppressMessages({
  library(blockr.core)
  # The package is still named blockr.bi on this branch (the .bi->.viz rename
  # lives on another branch). Once that merges, this load_all target becomes the
  # renamed package.
  args <- commandArgs(trailingOnly = FALSE)
  this <- sub("^--file=", "", args[grep("^--file=", args)])
  pkg_root <- dirname(dirname(normalizePath(this)))   # dev/ -> package root
  pkgload::load_all(pkg_root, quiet = TRUE)
})

# AE-count style data — the data-bar use case (sortable/searchable "patients
# with most adverse events"). AEcount + Severe have different maxima, so
# per-column normalisation is visible.
ae <- data.frame(
  Subject = sprintf("S%03d", 1:12),
  AEcount = c(40, 3, 25, 12, 0, 8, 33, 17, 5, 21, 9, 1),
  Severe  = c(10, 0, 6, 2, 0, 1, 9, 4, 1, 5, 2, 0),
  stringsAsFactors = FALSE
)

# Correlation matrix — diverging, all values computed (incl. some negatives).
cmat <- round(stats::cor(mtcars[, c("mpg", "disp", "hp", "wt", "qsec")]), 2)
cor_df <- data.frame(Variable = rownames(cmat), cmat,
                     check.names = FALSE, row.names = NULL)

board <- new_board(
  blocks = c(
    ae_data = new_static_block(ae, block_name = "AE counts"),
    ae_tbl  = new_table_block(
      rowname    = "Subject",
      cell_color = drilldown_table_color("bar", columns = "AEcount"),
      block_name = "AE counts (data bar on AEcount)"),
    cor_data = new_static_block(cor_df, block_name = "Correlation matrix"),
    cor_tbl  = new_table_block(
      rowname    = "Variable",
      cell_color = drilldown_table_color("diverging"),  # inferred symmetric
      block_name = "Correlation (diverging, centred on 0)")
  ),
  links = links(
    from = c("ae_data", "cor_data"),
    to   = c("ae_tbl",  "cor_tbl")
  )
)

serve(board)
