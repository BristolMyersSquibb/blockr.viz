# Verify dataseries.org-style drag-to-zoom on line charts.
#
#   Rscript blockr.viz/dev/verify-line-zoom.R   (serves on 3838)
#
# Drag-to-zoom is armed on BOTH charts: an armed dataZoomSelect cursor does not
# swallow series clicks, so zoom and click-to-drill coexist.
#   plain  — no drill -> drag zooms x (y rescales), double-click resets.
#   drill  — drill = "Tree" -> drag zooms AND a click still drills to the table.
# Starts on the Drill view so the Plain chart builds HIDDEN (the reveal path).
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
        block_name = "Plain line (drag zooms, dblclick resets)"),
      drill = new_chart_block(
        chart_type = "line", x = "age", y = "circumference", series = "Tree",
        drill = "Tree",
        block_name = "Drill line (drag zooms AND click drills)"),
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
    active = "Drill"  # Plain builds HIDDEN -> exercises the reveal re-arm path
  )
)
