# In-block drill sender (beta), end to end -- the sender-less cohort flow:
#
#   dm (safetyData ADaM) -> pull adsl -> summary_table (SEX, RACE by ARM)
#   -> table (drill = auto, ctrl_target = "cohort")     --.
#   adsl -> chart (RACE bar, drill = auto, ctrl_target)  --+--> cohort
#   adsl -> tile (SEX cards, drill, ctrl_target)         --'   (value filter)
#                                                              -> downstream
#
# No standalone ctrl_filter_block anywhere: each viz block pushes its own
# drill claim into the SAME value filter over the control channel, via the
# new `ctrl_target` (beta) argument. The value filter is fed the plain adsl
# data frame, so `ctrl_table` stays empty end to end (the optional-table
# path). The gear's "Send to filter (beta)" section shows the target picker
# (needs the ctrl bridge extension, registered below).
#
# Run from the workspace root or from the package dir:
#   Rscript blockr.viz/dev/verify-ctrl-send-beta.R
#
# Serves on 3838 (the only forwarded port) unless overridden -- a positional
# arg wins, then BLOCKR_PORT:
#   Rscript blockr.viz/dev/verify-ctrl-send-beta.R 4272

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
# core@main + dock@main disagree on the on-screen contract (named `visible`
# status vs flat id vector) -- ungated evaluation sidesteps it, like the
# blockr.pharma cohort-drill previews do.
options(blockr.gate_visibility = FALSE)

# load_all() ALL of them, never a mix: packages resolve each other's
# htmlDependency assets via system.file(), which pkgload only shims inside
# namespaces it loaded itself.
root <- if (file.exists("blockr.viz/DESCRIPTION")) "." else ".."
for (p in c("blockr.core", "blockr.ui", "blockr.dock", "blockr.dag",
            "blockr.theme", "blockr.dm", "blockr.viz")) {
  pkgload::load_all(file.path(root, p), quiet = TRUE)
}

stopifnot(requireNamespace("safetyData", quietly = TRUE))

serve(
  new_dock_board(
    blocks = c(
      dmx = new_dm_example_block(dataset = "safetydata_adam",
                                 block_name = "Safety ADaM dm"),
      adsl = new_dm_pull_block(table = "adsl", block_name = "Pull ADSL"),

      # Structured sender: annotated summary -> table, ARD-identity claim.
      summ = new_summary_table_block(
        vars = list("SEX", "RACE"),
        by = list("ARM"),
        block_name = "Demographics summary"
      ),
      tbl = new_table_block(
        drill = "auto",
        ctrl_target = "cohort",
        block_name = "Sex / Race (sends)"
      ),

      # Chart sender: categorical bar click, source-column claim.
      chart = new_chart_block(
        group = "RACE",
        drill = "auto",
        ctrl_target = "cohort",
        block_name = "Race bars (sends)"
      ),

      # Tile sender: card click on the group column.
      tile = new_tile_block(
        group = "SEX",
        summaries = list(list(func = "count", cols = list())),
        drill = TRUE,
        ctrl_target = "cohort",
        block_name = "Sex cards (sends)"
      ),

      # The one cohort authority all three push into. Fed the PLAIN adsl
      # data frame -> conditions carry no `table`.
      cohort = new_value_filter_block(block_name = "Cohort"),

      # Downstream observer: row count proves the cohort narrowed.
      result = new_table_block(block_name = "Cohort rows")
    ),
    links = list(
      list(from = "dmx", to = "adsl", input = "data"),
      list(from = "adsl", to = "summ", input = "data"),
      list(from = "summ", to = "tbl", input = "data"),
      list(from = "adsl", to = "chart", input = "data"),
      list(from = "adsl", to = "tile", input = "data"),
      list(from = "adsl", to = "cohort", input = "data"),
      list(from = "cohort", to = "result", input = "data")
    ),
    extensions = list(
      dag_extension = new_dag_extension(),
      ctrl_bridge = new_ctrl_bridge_extension()
    ),
    grids = list(
      Senders = dock_grid(
        list("tbl", "chart"),
        list("tile", "cohort"),
        list("result"),
        sizes = c(2, 2, 1)
      ),
      Pipeline = dock_grid("dag_extension"),
      Data = dock_grid(c("dmx", "adsl", "summ"))
    ),
    active = "Senders"
  )
)
