# Verify dataseries.org-style drag-to-zoom on line charts.
#
#   Rscript blockr.viz/dev/verify-line-zoom.R   (serves on 3838)
#
# Two line charts:
#   plain  — no drill -> drag-to-zoom cursor ARMED by default (drag a
#            horizontal window on the plot to zoom x; y rescales; reset icon).
#   drill  — drill = "Tree" -> click-to-drill stays the default gesture; the
#            magnifier icon opts into zoom (armed cursor would swallow clicks).
options(blockr.tabular_display = blockr.ui::html_table_display)
options(blockr.dock_is_locked = FALSE)
options(
  shiny.port = as.integer(Sys.getenv("BLOCKR_PORT", "3838")),
  shiny.host = "0.0.0.0"
)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.viz")

# One line per tree, numeric x (age) -> a clean continuous line to zoom.
orange <- datasets::Orange

serve(
  new_dock_board(
    blocks = c(
      orange_data = new_static_block(orange, block_name = "Orange trees"),
      plain = new_chart_block(
        chart_type = "line", x = "age", y = "circumference", series = "Tree",
        block_name = "Plain line (drag to zoom)"),
      drill = new_chart_block(
        chart_type = "line", x = "age", y = "circumference", series = "Tree",
        drill = "Tree",
        block_name = "Drill line (click drills; magnifier zooms)"),
      drill_tbl = new_table_block(
        rowname = "Tree", values = c("age", "circumference"),
        block_name = "Drilled rows")
    ),
    links = list(
      list(from = "orange_data", to = "plain", input = "data"),
      list(from = "orange_data", to = "drill", input = "data"),
      list(from = "drill", to = "drill_tbl", input = "data")
    ),
    extensions = new_dag_extension(),
    grids = list(
      Plain = dock_grid("plain"),
      Drill = dock_grid("drill", "drill_tbl")
    ),
    active = "Plain"
  )
)
