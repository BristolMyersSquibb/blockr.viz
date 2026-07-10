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
# Run from the workspace root or from the package dir:
#   Rscript blockr.viz/dev/verify-structured-drill.R
#
# Serves on 3838 (the only forwarded port) unless overridden -- a positional
# arg wins, then BLOCKR_PORT.  Long-lived demo servers from earlier sessions
# routinely squat on 3838, hence the override:
#   Rscript blockr.viz/dev/verify-structured-drill.R 3900
#   BLOCKR_PORT=3900 Rscript blockr.viz/dev/verify-structured-drill.R

port <- local({
  arg <- commandArgs(trailingOnly = TRUE)[1L]
  env <- Sys.getenv("BLOCKR_PORT", unset = "")
  raw <- if (!is.na(arg)) arg else if (nzchar(env)) env else "3838"
  p <- suppressWarnings(as.integer(raw))
  if (is.na(p)) stop("Not a port: ", raw, call. = FALSE)
  p
})

options(shiny.port = port, shiny.host = "0.0.0.0")
options(blockr.dock_is_locked = FALSE)

# load_all() ALL of them, never a mix: packages resolve each other's
# htmlDependency assets via system.file(), which pkgload only shims inside
# namespaces it loaded itself.
root <- if (file.exists("blockr.viz/DESCRIPTION")) "." else ".."
for (p in c("blockr.core", "blockr.ui", "blockr.dock", "blockr.dag",
            "blockr.theme", "blockr.dm", "blockr.viz")) {
  pkgload::load_all(file.path(root, p), quiet = TRUE)
}

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

# Shiny announces the *bind* address ("Listening on http://0.0.0.0:PORT"), which
# is not a URL you can click.  We bind 0.0.0.0 on purpose -- it is what lets the
# devcontainer forward the port -- so print the reachable address ourselves.
#
# Deferred onto the event loop, which serve() starts only after httpuv has bound
# the port: that keeps this line *below* Shiny's own, and stops it promising a
# URL that would still refuse the connection for another few seconds.
later::later(
  function() message("\n  blockr dock ready:  http://127.0.0.1:", port, "/\n")
)

serve(board)
