# Palette ROLES demo: the same board under a chosen house theme, laid out so
# each role is visible in one screen.
#
#   BLOCKR_THEME=vanilla Rscript blockr.viz/dev/theme-palette-demo.R [port]
#   BLOCKR_THEME=blockr  Rscript blockr.viz/dev/theme-palette-demo.R
#   BLOCKR_THEME=bms     Rscript blockr.viz/dev/theme-palette-demo.R
#
# What each panel proves:
#   * Bars (echarts)   -> `categorical`. The JS engine's own BLOCKR_PALETTE is
#                         now a fallback only; the resolved pool rides the
#                         chart config, so this recolours from R.
#   * Shaded table     -> `sequential`. dt_color_fun() interpolates between the
#                         role's two endpoints.
#   * Summary table    -> `bands`. Header fills, and the emphasis triple.
# Run with BLOCKR_THEME=vanilla first: nothing should differ from before the
# role plumbing existed.

port <- local({
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a)) as.integer(a[[1L]]) else as.integer(Sys.getenv("BLOCKR_PORT", "3838"))
})
options(shiny.port = port, shiny.host = "0.0.0.0")
options(blockr.dock_is_locked = FALSE)
options(blockr.background_construction_delay = 0)

root <- Sys.getenv("BLOCKR_ROOT", "/workspace")
deps <- c("blockr.core", "blockr.dag", "blockr.dock", "blockr.viz",
          "blockr.theme")
for (d in deps) {
  pkgload::load_all(file.path(root, d), helpers = FALSE,
                    attach_testthat = FALSE, export_all = FALSE)
}

which <- tolower(Sys.getenv("BLOCKR_THEME", "bms"))

if (identical(which, "bms")) {
  pkgload::load_all(file.path(root, "blockr.sandbox", "inst", "blockr.bms"),
                    helpers = FALSE, attach_testthat = FALSE,
                    export_all = FALSE)
}

theme <- switch(
  which,
  vanilla = NULL,
  blockr  = blockr.theme::theme_blockr(),
  bms     = blockr.bms::theme_bms(),
  stop("BLOCKR_THEME must be vanilla|blockr|bms")
)

if (!is.null(theme)) {
  blockr.theme::apply_theme_options(theme)
  head_tags <- blockr.theme::theme_head(theme)
  local({
    dock_ui <- getS3method("blockr_app_ui", "dock_board")
    registerS3method(
      "blockr_app_ui", "dock_board",
      function(id, x, plugins, options, ...) {
        dock_ui(id, x, plugins, options, ..., head_tags)
      },
      envir = asNamespace("blockr.core")
    )
  })
}

# What the roles resolve to for this run, echoed so the screen can be checked
# against the values rather than against memory.
message("\nTheme: ", which)
for (r in c("categorical", "sequential", "diverging", "bands")) {
  v <- blockr.viz:::viz_palette(
    r,
    if (r == "sequential") 2L else if (r == "diverging") 3L else NULL,
    switch(r,
           categorical = blockr.viz:::DD_PALETTE_FALLBACK,
           sequential = blockr.viz:::DT_SEQUENTIAL_FALLBACK,
           diverging = blockr.viz:::DT_DIVERGING_FALLBACK,
           NULL)
  )
  message(sprintf("  %-12s %s", r,
                  if (is.null(v)) "(none -- viz keeps its own)"
                  else paste(v, collapse = " ")))
}
message("\n  http://127.0.0.1:", port, "/\n")

# A small clinical-shaped frame: an arm with several levels for the
# categorical role, and a numeric column for the sequential ramp.
set.seed(42)
n <- 240
dat <- data.frame(
  ARM = sample(c("Placebo", "Low dose", "High dose", "Open label"), n,
               replace = TRUE, prob = c(.3, .25, .25, .2)),
  SEX = sample(c("F", "M"), n, replace = TRUE),
  AEBODSYS = sample(c("Gastrointestinal", "Nervous system",
                      "Skin", "Infections", "Vascular"), n, replace = TRUE),
  AVAL = round(rnorm(n, 55, 14), 1)
)

board <- new_dock_board(
  blocks = c(
    data = new_static_block(dat, block_name = "AE data"),
    bars = blockr.viz::new_chart_block(
      chart_type = "bar", group = "AEBODSYS", color = "ARM",
      orientation = "horizontal", bar_mode = "stacked",
      block_name = "categorical role"
    ),
    shaded = blockr.viz::new_table_block(
      rowname = "AEBODSYS", group = "ARM",
      summaries = list(list(func = "mean", cols = "AVAL")),
      shadings = list(list(mode = "sequential", cols = character())),
      block_name = "sequential role"
    ),
    summ = blockr.viz::new_summary_table_block(
      vars = "AVAL", by = "ARM", block_name = "bands role"
    )
  ),
  links = links(
    from = c("data", "data", "data"),
    to = c("bars", "shaded", "summ")
  ),
  stacks = stacks(
    roles = new_dock_stack(c("data", "bars", "shaded", "summ"),
                           name = "Palette roles", color = "#7c3aed")
  ),
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
