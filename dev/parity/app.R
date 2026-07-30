# Canvas side of the static-chart parity harness: one dock board carrying
# every state in dev/parity/states.R as an interactive chart block, block
# names = state ids so the drive script can front tabs by name.
#
#   Rscript dev/parity/app.R [port]
#
# Every package is load_all'd (source JS is served that way); blockr.viz
# comes from THIS worktree.

port <- local({
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args)) as.integer(args[[1L]]) else
    as.integer(Sys.getenv("BLOCKR_PORT", "4271"))
})

options(shiny.port = port, shiny.host = "0.0.0.0")
options(blockr.dock_is_locked = FALSE)
options(blockr.background_construction_delay = 0)

viz <- local({
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  d <- if (length(f)) normalizePath(f[[1L]]) else "dev/parity/app.R"
  dirname(dirname(dirname(d)))
})
root <- dirname(viz)

for (d in c("blockr.core", "blockr.ui", "blockr.dplyr", "blockr.dock",
            "blockr.dag")) {
  pkgload::load_all(
    file.path(root, d),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
}
pkgload::load_all(viz, helpers = FALSE, attach_testthat = FALSE,
                  export_all = FALSE)

source(file.path(viz, "dev", "parity", "states.R"))

ds <- parity_datasets()

data_blocks <- lapply(names(ds), function(nm) {
  new_static_block(ds[[nm]], block_name = nm)
})
names(data_blocks) <- names(ds)

# blockr.core resolves the ctor from sys.call(), so the constructor must be
# CALLED by name -- do.call(fn, ...) breaks resolve_ctor. Build the call.
chart_blocks <- lapply(names(parity_states), function(id) {
  s <- parity_states[[id]]
  eval(as.call(c(quote(new_chart_block), s$args, list(block_name = id))))
})
names(chart_blocks) <- names(parity_states)

board <- new_dock_board(
  blocks = c(data_blocks, chart_blocks),
  links = links(
    from = vapply(parity_states, `[[`, character(1L), "data"),
    to = names(parity_states)
  ),
  stacks = stacks(
    data = new_dock_stack(names(ds), name = "Data", color = "#6b7280"),
    charts = new_dock_stack(names(parity_states), name = "Charts",
                            color = "#7c3aed")
  ),
  extensions = list(blockr.dag::new_dag_extension())
)

message("Open http://127.0.0.1:", port, "/")

serve(board)
