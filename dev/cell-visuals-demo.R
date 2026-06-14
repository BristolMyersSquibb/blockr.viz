# Cell visuals demo — data bars + diverging-centred-on-0 + column scope.
#
# Verifies the new drilldown_table_color() modes:
#   - type = "bar"        in-cell data bars (per-column abs-magnitude)
#   - type = "diverging"  centred on 0 with a symmetric inferred domain
#   - columns = ...       restrict the effect to named columns (empty = all)
#
# Run:  Rscript blockr.viz-cellvis/dev/cell-visuals-demo.R
# (Uses an ISOLATED install of this worktree's blockr.bi from /tmp/cellvis-lib
#  so it never touches the shared library.)

.libPaths(c("/tmp/cellvis-lib", .libPaths()))
# 3838 is the forwarded port for the user; CELLVIS_PORT overrides it for
# automated in-container checks that must not collide with another app.
options(shiny.port = as.integer(Sys.getenv("CELLVIS_PORT", "3838")),
        shiny.host = "0.0.0.0")
options(blockr.html_table_preview = TRUE)

suppressMessages({
  library(blockr.core)
  library(blockr.bi)
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
