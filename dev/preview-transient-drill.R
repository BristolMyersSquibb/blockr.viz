# Transient drill: a chart and a table with a ctrl_target send the click and
# keep nothing.
#
#   data (synthetic ADSL) -> chart   (bar, drill auto, ctrl_target = "vf")
#                         -> summ -> t1   (structured Table 1, ctrl_target = "vf")
#                         -> flat         (flat table, drill SEX, ctrl_target = "vf")
#                         -> vf (drill filter) -> cohort
#
# No data link runs from a sender to the filter; the claim travels over the
# board's control channel. The senders are not downstream of what they feed,
# which is why their DOM survives their own click.
#
# Things to try:
#   - Click a bar. The bar itself lights, holds, releases; the footer says
#     "Sent ARM = ...". The chart does NOT dim or filter: every bar stays.
#   - Click a row label in Table 1: the row lights. Click an arm HEADER: the
#     column lights. Click a NUMBER: row + column, one step darker where they
#     cross -- the same ladder the latched paint used.
#   - Click the SAME thing again. It sends again (no toggle-off).
#   - Undo lives in the filter block, not in any sender.
#
# Run from the workspace root (or from the package dir):
#   Rscript blockr.viz/dev/preview-transient-drill.R          # first free port
#   Rscript blockr.viz/dev/preview-transient-drill.R 3839

root <- if (file.exists("blockr.viz/DESCRIPTION")) "." else ".."
for (p in c("blockr.core", "blockr.theme", "blockr.dplyr", "blockr.dm",
            "blockr.viz", "blockr.dock", "blockr.dag")) {
  pkgload::load_all(file.path(root, p), quiet = TRUE)
}

port <- local({
  arg <- commandArgs(trailingOnly = TRUE)[1L]
  env <- Sys.getenv("BLOCKR_PORT", unset = "")
  raw <- if (!is.na(arg)) arg else if (nzchar(env)) env else NA_character_
  if (is.na(raw)) {
    # First free port in the devcontainer's forwarded range, when we are in one.
    return(if (exists("blockr_port")) blockr_port() else 3838L)
  }
  p <- suppressWarnings(as.integer(raw))
  if (is.na(p)) stop("Not a port: ", raw, call. = FALSE)
  p
})

options(shiny.port = port, shiny.host = "0.0.0.0", shiny.launch.browser = FALSE,
        "g6R.preserve_elements_position" = TRUE)
message("transient drill demo on http://127.0.0.1:", port, "/")

# No-UI extension whose only job is to expose the board update channel to
# block code (blockr.viz::ctrl_send).
new_ctrl_bridge_extension <- function() {
  new_dock_extension(
    server = function(id, board, update, ...) {
      shiny::moduleServer(id, function(input, output, session) {
        blockr.viz::install_ctrl_send(update)
        list(state = list())
      })
    },
    ui = function(ns, ...) htmltools::div(),
    name = "Control bridge",
    class = "ctrl_bridge_extension"
  )
}

set.seed(1)
adsl <- data.frame(
  ARM  = sample(c("Placebo", "Low dose", "High dose"), 300, replace = TRUE),
  SEX  = sample(c("F", "M"), 300, replace = TRUE),
  RACE = sample(c("WHITE", "BLACK", "ASIAN"), 300, replace = TRUE),
  AGE  = sample(18:85, 300, replace = TRUE),
  BMI  = round(rnorm(300, 26, 4), 1),
  stringsAsFactors = FALSE
)

# The structured frame, with BOTH axes stamped: rows carry `.variable` /
# `.variable_level` (summary_table does that), columns carry `column_keys`
# (a composer's colgroup does that; here, by hand). Spanner labels give the
# headers their Big N the way a real Table 1 has it.
table1 <- summary_table(adsl, vars = c("AGE", "SEX", "RACE"), by = "ARM",
                        stats = c("mean_sd", "n_pct"))
arms <- intersect(c("Placebo", "Low dose", "High dose"), names(table1))
attr(table1, "column_keys") <- stats::setNames(
  lapply(arms, function(a) list(list(column = "ARM", values = a))), arms
)
attr(table1, "spanner_labels") <- stats::setNames(
  vapply(arms, function(a) paste0(a, "\nN = ", sum(adsl$ARM == a)), character(1)),
  arms
)

serve(
  new_dock_board(
    blocks = c(
      data = new_static_block(data = adsl),
      chart = new_chart_block(
        chart_type = "bar",
        group = "ARM", value = "AGE", func = "mean",
        drill = "auto",
        ctrl_target = "vf",
        block_name = "Mean age by arm (sends)"
      ),
      # Structured Table 1: row labels, arm COLUMNS, and the cells where the
      # two cross -- the three claim shapes a table can make. Built here rather
      # than by new_summary_table_block() because that producer stamps only the
      # ROW axis; the column axis is the optional `column_keys` attribute
      # (see annotated_column_keys), which is what a composer colgroup carries
      # and what makes a header and a cell clickable.
      t1_data = new_static_block(data = table1),
      t1 = new_table_block(drill = "auto", ctrl_target = "vf",
                           block_name = "Table 1 (sends)"),
      # Flat table: the row drill.
      flat = new_table_block(drill = "SEX", ctrl_target = "vf",
                             block_name = "Subjects (sends)"),
      vf = new_drill_filter_block(),
      cohort = new_table_block(block_name = "Cohort"),
      # A table whose data a drill never touches. It is here to answer one
      # question: does a board update (which every ctrl_send is) replace a
      # table block's DOM even when its output has not changed? If it did, a
      # class put on a row by JS could not survive the click that set it.
      probe = new_table_block(block_name = "Untouched by the drill")
    ),
    links = list(
      list(from = "data", to = "chart", input = "data"),
      list(from = "t1_data", to = "t1", input = "data"),
      list(from = "data", to = "flat", input = "data"),
      list(from = "data", to = "vf", input = "data"),
      list(from = "vf", to = "cohort", input = "data"),
      list(from = "data", to = "probe", input = "data")
    ),
    extensions = list(
      dag_extension = new_dag_extension(),
      ctrl_bridge = new_ctrl_bridge_extension()
    ),
    grids = list(
      Chart = dock_grid(list("chart", "vf"), list("cohort", "probe"),
                        sizes = c(1, 1)),
      Table = dock_grid(list("t1", "flat"), "vf", sizes = c(2, 1)),
      Data = dock_grid(c("t1_data", "cohort", "probe")),
      Pipeline = dock_grid("dag_extension")
    ),
    active = "Table"
  )
)
