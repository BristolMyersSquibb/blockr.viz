# Measure switch block demo — one curated control, everything else fixed.
#
# The scenario from the design record (blockr.docs/design-system/target/
# measure-switch-proposals.html): a locked CDEX-style view where the viewer
# picks WHICH measure a chart shows, and nothing else. The switch block
# standardizes the pick into measure + value (long shape, always), so the
# downstream chart's mappings never change.
#
# Views
#   1. Single measure  segmented control (2 choices) -> scatter, y = value;
#                      the y-axis label follows the picked column's label
#   2. Multi measure   pills (multiple = TRUE) -> scatter facetted by measure;
#                      toggling a pill adds/removes a facet, no reconfig
#
# Run from the workspace root:
#   Rscript blockr.viz/dev/measure-switch-demo.R [port]
# Port: argument, else BLOCKR_PORT, else 3838.

port <- suppressWarnings(as.integer(commandArgs(trailingOnly = TRUE)[1]))
if (is.na(port)) {
  port <- as.integer(Sys.getenv("BLOCKR_PORT", "3838"))
}
options(shiny.port = port, shiny.host = "0.0.0.0")

options(blockr.tabular_display = blockr.ui::html_table_display)
options(blockr.dock_is_locked = FALSE)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.viz")

# iris with labels on the two offered measures — the switch shows labels,
# never raw names, and carries them through to the chart.
iris2 <- datasets::iris
attr(iris2$Sepal.Length, "label") <- "Sepal length (cm)"
attr(iris2$Sepal.Width, "label") <- "Sepal width (cm)"

board <- new_dock_board(
  blocks = c(
    data = new_static_block(iris2, block_name = "Iris (labelled)"),

    # 1. SINGLE — two choices render as a segmented control; the chart maps
    #    y = value, fixed; picking swaps the measure under the same mapping.
    switch_one = new_measure_switch_block(
      choices = c("Sepal.Length", "Sepal.Width"),
      selected = "Sepal.Length",
      block_name = "Measure"
    ),
    chart_one = new_chart_block(
      chart_type = "scatter", x = "Petal.Length", y = "value",
      color = "Species",
      block_name = "Sepal measure vs petal length"
    ),

    # 2. MULTI — pills; the chart facets by measure, so a second pick grows
    #    a second panel without touching the chart config.
    switch_multi = new_measure_switch_block(
      choices = c("Sepal.Length", "Sepal.Width"),
      selected = c("Sepal.Length", "Sepal.Width"),
      multiple = TRUE,
      block_name = "Measures"
    ),
    chart_multi = new_chart_block(
      chart_type = "scatter", x = "Petal.Length", y = "value",
      color = "Species", facet = "measure",
      block_name = "Sepal measures vs petal length (facetted)"
    )
  ),
  links = links(
    from = c("data", "switch_one", "data", "switch_multi"),
    to   = c("switch_one", "chart_one", "switch_multi", "chart_multi")
  ),
  grids = list(
    single = dock_grid(
      "switch_one", "chart_one",
      orientation = "vertical", sizes = c(1, 2)
    ),
    multi = dock_grid(
      "switch_multi", "chart_multi",
      orientation = "vertical", sizes = c(1, 2)
    )
  ),
  options = dock_board_options(),
  active = "single",
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
