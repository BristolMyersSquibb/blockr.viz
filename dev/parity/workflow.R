# The parity states as a real blockr workflow: one dock board carrying every
# state in dev/parity/states.R as an interactive chart block, with an outline
# (report/deck) extension whose exhibits render through report_call() -- the
# same path a pptx deck takes.
#
#   Rscript blockr.viz/dev/parity/workflow.R [port]      # default 3838
#
# What to look at (the Outline pane and a chart panel are side by side, so
# both renders of one state are on screen at once):
#
#   Outline tab    the rendered document. Switch it to Output (the eye, next
#                  to <>) and every exhibit draws: Output evaluates the
#                  generated script top to bottom, so it does NOT wait for
#                  the chart panels. Code view shows the emitted call instead,
#                  and there an exhibit reads "Evaluating..." until its chart
#                  tab has been fronted once (deferred dock panels only
#                  evaluate when their panel first renders).
#   Charts stack   the live canvas chart, one tab per state -- front a tab to
#                  compare it with its exhibit, and open its gear to change
#                  the state and watch both sides follow
#
# report_style picks which printed form the exhibits use:
#   BLOCKR_REPORT_STYLE=static   blockr.viz::static_chart() -- the parity target
#   BLOCKR_REPORT_STYLE=code     the compiled dplyr + ggplot2 pipeline (default)
#
# For a strict side-by-side of both renderers against the canvas, use
# dev/parity/preview.R instead; for pixel comparison, drive.R + static.R.

viz <- local({
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  d <- if (length(f)) normalizePath(f[[1L]]) else "dev/parity/workflow.R"
  dirname(dirname(dirname(d)))
})
root <- dirname(viz)

port <- local({
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a)) as.integer(a[[1L]]) else
    as.integer(Sys.getenv("BLOCKR_PORT", "3838"))
})

options(shiny.port = port, shiny.host = "0.0.0.0")
options(blockr.dock_is_locked = FALSE)
options(blockr.background_construction_delay = 0)
options(blockr.viz.report_style = Sys.getenv("BLOCKR_REPORT_STYLE", "code"))

for (d in c("blockr.core", "blockr.ui", "blockr.dplyr", "blockr.dock",
            "blockr.dag", "blockr.outline")) {
  pkgload::load_all(
    file.path(root, d),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
}
pkgload::load_all(viz, helpers = FALSE, attach_testthat = FALSE,
                  export_all = FALSE)

source(file.path(viz, "dev", "parity", "states.R"))

specs <- parity_dataset_specs()
ids <- names(parity_states)

# Dataset blocks, not static blocks: the outline's Output view evaluates the
# generated script (see parity_dataset_specs()).
data_blocks <- lapply(names(specs), function(nm) {
  s <- specs[[nm]]
  eval(as.call(list(
    quote(new_dataset_block), s$dataset, package = s$package, block_name = nm
  )))
})
names(data_blocks) <- names(specs)

# blockr.core resolves the ctor from sys.call(), so the constructor must be
# CALLED by name -- do.call(fn, ...) breaks resolve_ctor. Build the call.
chart_blocks <- lapply(ids, function(id) {
  s <- parity_states[[id]]
  eval(as.call(c(quote(new_chart_block), s$args, list(block_name = id))))
})
names(chart_blocks) <- ids

# Each chart is an exhibit; the state it was built from is the description, so
# the document says what the render is meant to show.
annotations <- c(
  lapply(names(specs), function(nm) list(description = nm, report = FALSE)),
  lapply(ids, function(id) {
    s <- parity_states[[id]]
    list(
      description = paste0(
        "`", paste(names(s$args), unlist(lapply(s$args, format)),
                   sep = " = ", collapse = ", "), "`"
      ),
      report = TRUE
    )
  })
)
names(annotations) <- c(names(specs), ids)

board <- new_dock_board(
  blocks = c(data_blocks, chart_blocks),
  links = links(
    from = vapply(parity_states, `[[`, character(1L), "data"),
    to = ids
  ),
  stacks = stacks(
    data = new_dock_stack(names(specs), name = "Data", color = "#6b7280"),
    charts = new_dock_stack(ids, name = "Charts", color = "#7c3aed")
  ),
  extensions = list(
    blockr.dag::new_dag_extension(),
    blockr.outline::new_outline_extension(
      title = "static chart parity",
      annotations = annotations
    )
  )
)

message("Open http://127.0.0.1:", port, "/  (report_style = ",
        getOption("blockr.viz.report_style"), ")")

serve(board)
