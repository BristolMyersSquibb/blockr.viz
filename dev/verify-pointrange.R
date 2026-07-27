# Verify the distribution marks (spec: _blockr.design/open/distribution-marks).
#
#   Rscript blockr.viz/dev/verify-pointrange.R [port]   (default 3838)
#
# Charts view
#   pr       — pointrange: mean ± SE of AVAL per AVISIT × TRT01A, centers
#              connected, visits in AVISITN order. ONE whisker per
#              (visit, arm) slot; whisker ends INSIDE the value-axis extent.
#   box      — untouched boxplot: must render the textbook Tukey box
#              (median + Q1/Q3 body, 1.5×IQR whiskers), same as before the
#              rework. Gear shows Box + Whiskers selects, both at defaults.
#   box_stat — boxplot with a non-default convention: mean ± SD body,
#              5–95th percentile whiskers, outlier points past the whiskers.
#
# Gear checks (manual or via the drive script)
#   - pr gear: "Interval" select + "Connect centers" checkbox, NO whiskers.
#   - box gear: "Box" + "Whiskers" selects, box_points None/Outliers only.
#   - switching pointrange ⇄ boxplot keeps group / value / color.
#
# load_all EVERYTHING before touching any blockr namespace: an option set via
# `blockr.ui::` above these lines would resolve against the INSTALLED package,
# which may be older than source.
pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.viz")

options(blockr.tabular_display = blockr.ui::html_table_display)
options(blockr.dock_is_locked = FALSE)
options(
  shiny.port = {
    args <- commandArgs(trailingOnly = TRUE)
    p <- if (length(args)) args[[1]] else Sys.getenv("BLOCKR_PORT", "3838")
    as.integer(p)
  },
  shiny.host = "0.0.0.0"
)

cat(sprintf("\nOpen: http://127.0.0.1:%d/\n\n", getOption("shiny.port")))

adlb <- as.data.frame(pharmaverseadam::adlb)
alt <- adlb[
  adlb$PARAMCD == "ALT" & !is.na(adlb$AVAL) & !is.na(adlb$AVISITN) &
    nzchar(adlb$AVISIT),
  c("USUBJID", "AVISIT", "AVISITN", "AVAL", "TRT01A")
]

serve(
  new_dock_board(
    blocks = c(
      data = new_static_block(alt, block_name = "ALT raw observations"),
      pr = new_chart_block(
        chart_type = "pointrange", group = "AVISIT", value = "AVAL",
        color = "TRT01A", summary = "mean_se", connect_centers = TRUE,
        sort_by = "AVISITN", sort_dir = "asc",
        block_name = "Pointrange: mean ± SE per visit × arm"
      ),
      box = new_chart_block(
        chart_type = "boxplot", group = "TRT01A", value = "AVAL",
        block_name = "Boxplot: untouched (textbook Tukey)"
      ),
      box_stat = new_chart_block(
        chart_type = "boxplot", group = "TRT01A", value = "AVAL",
        summary = "mean_sd", whiskers = "p5_p95", box_points = "outliers",
        block_name = "Boxplot: mean ± SD body, 5–95% whiskers"
      )
    ),
    links = list(
      list(from = "data", to = "pr", input = "data"),
      list(from = "data", to = "box", input = "data"),
      list(from = "data", to = "box_stat", input = "data")
    ),
    extensions = new_dag_extension(),
    grids = list(
      Charts = dock_grid("pr", "box", "box_stat")
    ),
    active = "Charts"
  )
)
