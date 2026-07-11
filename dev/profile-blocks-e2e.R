# =============================================================================
# End-to-end block "time to appear" profiler (throttled browser)
# =============================================================================
#
# Measures what the USER feels: from a view switch that wakes a dormant block,
# how long until that block's content is painted. Covers the full path --
# client -> visibility report -> server construct/eval/render -> round trip(s)
# -> paint -- under a throttled CPU, the way production is felt.
#
# Companion to profile-blocks-r.R (which isolates the pure R render cost). Use
# both: if a block appears slowly here but its R render is cheap there, the cost
# is plumbing (round trips / dep rebuild / relayout), not compute.
#
# Two processes:
#   1. Launch the board:   Rscript blockr.viz/dev/profile-blocks-e2e.R app   [port]
#   2. Drive + measure:     Rscript blockr.viz/dev/profile-blocks-e2e.R drive [port] [cpu]
#
# Each block sits alone in its own view, landing view is "home", so every
# measured block starts dormant and the switch wakes it cold.
#
# CAVEAT (load_all): under pkgload::load_all, some blocks' htmlDependency src
# resolves to '' and the page dies with "Couldn't normalize path in
# addResourcePath (blockr-core-js)". This is a load_all shim artifact, NOT a
# production bug -- installed packages resolve fine. Until the multi-block board
# is run against an installed, mutually-consistent package set, use the
# preview-view-switch-latency.R board (which renders under load_all) to measure
# the table block, and this harness only for whatever subset renders. The
# table-block E2E numbers in the writeup came from that preview board.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a
mode <- commandArgs(trailingOnly = TRUE)[1] %||% "app"
port <- as.integer(commandArgs(trailingOnly = TRUE)[2] %||% Sys.getenv("BLOCKR_PORT", "3838"))

# Each view: id -> a CSS selector that exists only once the block's content
# (not its chrome) has painted. Kept next to the board so the driver and the
# app agree on what "appeared" means.
APPEAR <- list(
  home       = NULL,
  v_table    = "table.blockr-table",
  v_chart    = ".drilldown-chart-container canvas",
  v_tile     = ".blockr-tile, .tile-card, [class*=tile]",
  v_summary  = "table.blockr-table",
  v_filter   = ".blockr-filter, .filter-block-container, [class*=filter] input",
  v_ggplot   = ".shiny-image-output img, .shiny-plot-output img"
)

# ---------------------------------------------------------------- app process
if (identical(mode, "app")) {
  root <- if (file.exists("blockr.viz/DESCRIPTION")) "." else ".."
  # Known-good load_all set (matches the working dev previews). Adding
  # blockr.ggplot here crashes addResourcePath with src='' for blockr-core-js
  # (the load_all shim only maps system.file inside namespaces it loaded, and
  # ggplot's dep chain trips it). ggplot's build cost is in profile-blocks-r.R.
  suppressMessages(for (p in c("blockr.core","blockr.viz","blockr.dplyr","blockr.dock"))
    pkgload::load_all(file.path(root, p), quiet = TRUE))
  options(shiny.port = port, blockr.log_level = "warn")

  data_blk <- new_dataset_block(dataset = "mtcars", package = "datasets")

  serve(
    new_dock_board(
      blocks = c(
        data    = data_blk,
        tbl     = new_table_block(),
        summ    = new_summary_table_block(vars = c("mpg","hp"), by = "cyl"),
        summtbl = new_table_block(),
        filt    = new_filter_block()
      ),
      links = list(
        list(from="data", to="tbl",   input="data"),
        list(from="data", to="summ",  input="data"),
        list(from="summ", to="summtbl", input="data"),
        list(from="data", to="filt",  input="data")
      ),
      # `data` is an off-grid feeder (never placed in a view) -- rendering the
      # dataset block's UI under load_all trips the blockr-core-js resource
      # path. Land on the filter view (a cheap JS block, always visible); every
      # measured block (table / summary-table) starts dormant in its own view.
      grids = list(
        home     = dock_grid("filt"),
        v_table  = dock_grid("tbl"),
        v_summary= dock_grid("summtbl")
      ),
      active = "home"
    )
  )
}

# -------------------------------------------------------------- drive process
if (identical(mode, "drive")) {
  cpu <- as.numeric(commandArgs(trailingOnly = TRUE)[3] %||% "6")
  library(chromote)
  b <- ChromoteSession$new(); on.exit(try(b$close(), silent = TRUE), add = TRUE)
  b$Page$navigate(sprintf("http://127.0.0.1:%d/", port)); Sys.sleep(15)
  js <- function(x) b$Runtime$evaluate(x, awaitPromise = TRUE, returnByValue = TRUE)$result$value
  ns <- js('(() => document.querySelector(".blockr-view-dock").id.split("-")[0])()')

  b$Emulation$setCPUThrottlingRate(rate = cpu)
  b$Network$enable()
  b$Network$emulateNetworkConditions(offline = FALSE, latency = 150,
    downloadThroughput = 1e6/8, uploadThroughput = 1e6/8)
  cat(sprintf("Throttle: CPU %sx, latency 150ms\n\n", cpu))

  views <- c(v_table="table.blockr-table", v_summary="table.blockr-table")

  measure <- function(view, sel) {
    js(sprintf('(() => { window.__t0 = performance.now(); window.__done = null;
      window.__sel = "%s";
      window.__poll = setInterval(() => {
        if (document.querySelector(window.__sel)) {
          window.__done = performance.now() - window.__t0; clearInterval(window.__poll);
        }
      }, 10);
      Shiny.setInputValue("%s-view_nav", "%s", {priority:"event"});
      return 1; })()', gsub('"', '\\\\"', sel), ns, view))
    # wait up to 20s (throttled) for the selector to appear
    t <- NA
    for (k in 1:200) {
      Sys.sleep(0.1)
      t <- js('(() => window.__done)()')
      if (!is.null(t)) break
    }
    # reset to home so the next block is dormant again
    js(sprintf('(() => { Shiny.setInputValue("%s-view_nav","home",{priority:"event"}); return 1;})()', ns))
    Sys.sleep(4)
    if (is.null(t)) NA_real_ else round(t, 0)
  }

  cat(sprintf("%-12s %12s\n", "view", "appear(ms)"))
  cat(strrep("-", 26), "\n")
  for (v in names(views)) {
    ms <- measure(v, views[[v]])
    cat(sprintf("%-12s %12s\n", v, if (is.na(ms)) "TIMEOUT" else ms))
  }
}
