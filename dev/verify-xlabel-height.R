# Rotated x-axis labels lengthen the CANVAS instead of eating the plot.
#
# Run from the WORKSPACE ROOT (the load_all paths are relative to it):
#   BLOCKR_PORT=3838 Rscript blockr.viz/dev/verify-xlabel-height.R
#
# _xAxisLabels() (inst/js/chart.js) rotates categorical x labels 90 degrees
# when they don't fit their per-category slot. Rotated text spends its pixel
# WIDTH on the grid's bottom gutter, so the gutter used to be carved out of a
# fixed canvas and the plot area shrank. Each builder now adds `xlab.bottom`
# to its own height instead:
#
#   vertical bar  __panelH   = 350 + xlab.bottom
#   waterfall     __panelH   = 350 + xlab.bottom
#   line/scatter  slot height = 400 + xlab.bottom
#
# Twelve deliberately long categories, so every label rotates AND exceeds the
# 160px truncation cap.
#
# What to check:
#   1. VBAR   -- bars are full height (not squashed), labels read ~27 chars
#                before the ellipsis, axis title sits BELOW the rotated text.
#   2. WFALL  -- same, on the cumulative bridge.
#   3. LINE   -- categorical x; the line region is the same size it would be
#                with short labels, the panel just got taller.
#   4. Widen / narrow the browser: labels flip flat once each category slot is
#      wide enough, and the canvas snaps back to 350 / 400px.
options(shiny.port = as.integer(Sys.getenv("BLOCKR_PORT", "3838")),
        shiny.host = "0.0.0.0")
options(blockr.tabular_display = blockr.ui::html_table_display)
options(blockr.dock_is_locked = FALSE)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.viz")

long <- paste0(
  c("Xanomeline High Dose", "Xanomeline Low Dose", "Placebo Comparator",
    "Open Label Follow Up", "Screening Failure", "Early Termination",
    "Completed Treatment", "Lost To Follow Up", "Adverse Event Exit",
    "Protocol Deviation", "Withdrawal By Subject", "Physician Decision"),
  " Extended Cohort"
)

bars <- data.frame(
  cat = rep(long, each = 3),
  val = as.numeric(seq_along(rep(long, each = 3))) %% 17 + 3
)
bridge <- data.frame(
  step = long,
  value = c(1200, 480, -210, -90, 300, -150, 220, -80, 140, -60, 90, -40)
)
lines_df <- data.frame(
  visit = rep(long, times = 2),
  score = as.numeric(seq_along(rep(long, times = 2))) %% 13 + 5,
  who   = rep(c("A", "B"), each = length(long))
)

# Crowded ORDERED axis: 90 visits, so each category slot falls well under the
# ~13px a rotated label needs and _xAxisLabels thins them. Factor levels keep
# the run in sequence (the point of decimating: the reader interpolates).
visits <- sprintf("CYCLE %d DAY %d PRE-DOSE ASSESSMENT", rep(1:30, each = 3),
                  rep(c(1L, 8L, 15L), times = 30))
crowded <- data.frame(
  visit = factor(rep(visits, times = 2), levels = visits),
  score = as.numeric(seq_along(rep(visits, times = 2))) %% 11 + 4,
  who   = rep(c("A", "B"), each = length(visits))
)

board <- new_dock_board(
  blocks = c(
    bar_data = new_static_block(bars, block_name = "Bar data"),
    vbar = new_chart_block(
      chart_type = "bar", group = "cat", value = "val", func = "sum",
      orientation = "vertical", block_name = "VBAR (350 + gutter)"),
    wf_data = new_static_block(bridge, block_name = "Bridge data"),
    wf = new_chart_block(
      chart_type = "waterfall", group = "step", value = "value",
      func = "sum", block_name = "WFALL (350 + gutter)"),
    ln_data = new_static_block(lines_df, block_name = "Line data"),
    ln = new_chart_block(
      chart_type = "line", x = "visit", y = "score", series = "who",
      block_name = "LINE (400 + gutter)"),
    cr_data = new_static_block(crowded, block_name = "Crowded visits"),
    cr = new_chart_block(
      chart_type = "line", x = "visit", y = "score", series = "who",
      block_name = "CROWDED (thinned labels)")
  ),
  links = links(
    from = c("bar_data", "wf_data", "ln_data", "cr_data"),
    to   = c("vbar", "wf", "ln", "cr")
  ),
  grids = list(
    Pipeline = dock_grid("dag"),
    Bar      = dock_grid("vbar"),
    Waterfall = dock_grid("wf"),
    Line     = dock_grid("ln"),
    Crowded  = dock_grid("cr"),
    All_three = dock_grid("vbar", "wf", "ln", orientation = "vertical")
  ),
  options = dock_board_options(),
  active = "Crowded",
  extensions = list(blockr.dag::new_dag_extension())
)

cat("\n  ->  http://127.0.0.1:",
    getOption("shiny.port"), "/\n\n", sep = "")

serve(board)
