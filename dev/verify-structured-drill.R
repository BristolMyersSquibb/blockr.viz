# Structured-table drilldown, end to end -- the clinician flow:
#
#   dm (safetyData ADaM) -> pull adae -> summary_table (AESEV + AEDECOD by
#   TRTA) -> drilldown table.  The drilled output feeds
#   blockr.dm::new_dm_filter_by_data_block (UNCHANGED) as its `by` input;
#   dm_filter() cascades through the FKs, so clicking ABDOMINAL PAIN restricts
#   the whole dm to the patients with that AE -- verified on a downstream
#   adsl pull.
#
# The bridge auto-picks its key column from the name intersection: the drilled
# output carries the selection as a real column (AEDECOD for a term click,
# AESEV for a severity click), so EVERY section drills against ONE bridge.
#
# Run from the workspace root: Rscript blockr.viz/dev/verify-structured-drill.R
# Serves on port 3838 (the only forwarded port).

options(shiny.port = 3838L, shiny.host = "0.0.0.0")
options(blockr.dock_is_locked = FALSE)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.theme")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.viz")

stopifnot(requireNamespace("safetyData", quietly = TRUE))

board <- new_dock_board(
  blocks = c(
    dmx = new_dm_example_block(dataset = "safetydata_adam",
                               block_name = "Safety ADaM dm"),
    adae = new_dm_pull_block(table = "adae", block_name = "Pull ADAE"),
    summ = new_summary_table_block(
      vars = list("AESEV", "AEDECOD"),
      by = list("TRTA"),
      block_name = "AE summary (severity + term)"
    ),
    tbl = new_table_block(block_name = "AE by severity table"),
    bridge = new_dm_filter_by_data_block(
      table = "adae",
      block_name = "dm restricted by selection"
    ),
    pts = new_dm_pull_block(table = "adsl", block_name = "Patients (ADSL)")
  ),
  links = links(
    new_link(from = "dmx",  to = "adae",   input = "data"),
    new_link(from = "adae", to = "summ",   input = "data"),
    new_link(from = "summ", to = "tbl",    input = "data"),
    new_link(from = "dmx",  to = "bridge", input = "data"),
    new_link(from = "tbl",  to = "bridge", input = "by"),
    new_link(from = "bridge", to = "pts",  input = "data")
  ),
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
