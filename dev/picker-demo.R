# Picker block demo — curated controls, everything else fixed.
#
# The scenario from the design records (blockr.docs/design-system/target/
# measure-switch-proposals.html + select-controls.html): locked CDEX-style
# views where the viewer picks WHICH column(s) a chart shows, and nothing
# else. Each picker lands its pick in a stable, named output column (`into`),
# so the downstream chart's mappings never change. Picker definitions live
# in the gear settings band (open the gear: into / columns offered /
# multiple / remove, plus "+ Add picker").
#
# Views
#   1. Single picker   one select ("value") -> scatter, y = value; the
#                      y-axis label follows the picked column's label
#   2. Multi picker    multiple = TRUE -> pivots long; chart facets by
#                      value_measure; toggling a pick adds/removes a facet
#   3. Two pickers     x_value + y_value -> viewer picks BOTH axes of a
#                      scatter; copies, so measure-vs-itself is legal
#
# Run from the workspace root:
#   Rscript blockr.viz/dev/picker-demo.R [port]
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

# iris with labels on the measures — pickers show labels as sublabels and
# carry them through to the chart.
iris2 <- datasets::iris
attr(iris2$Sepal.Length, "label") <- "Sepal length (cm)"
attr(iris2$Sepal.Width, "label") <- "Sepal width (cm)"
attr(iris2$Petal.Length, "label") <- "Petal length (cm)"
attr(iris2$Petal.Width, "label") <- "Petal width (cm)"

board <- new_dock_board(
  blocks = c(
    data = new_static_block(iris2, block_name = "Iris (labelled)"),

    # 1. SINGLE — one picker into "value"; the chart maps y = value, fixed.
    pick_one = new_picker_block(
      state = list(pickers = list(
        list(
          into = "value",
          choices = c("Sepal.Length", "Sepal.Width"),
          selected = "Sepal.Length",
          multiple = FALSE
        )
      )),
      block_name = "Measure"
    ),
    chart_one = new_chart_block(
      chart_type = "scatter", x = "Petal.Length", y = "value",
      color = "Species",
      block_name = "Sepal measure vs petal length"
    ),

    # 2. MULTI — one multiple picker; picks pivot long, the chart facets by
    #    value_measure, so a second pick grows a second panel.
    pick_multi = new_picker_block(
      state = list(pickers = list(
        list(
          into = "value",
          choices = c("Sepal.Length", "Sepal.Width"),
          selected = c("Sepal.Length", "Sepal.Width"),
          multiple = TRUE
        )
      )),
      block_name = "Measures"
    ),
    chart_multi = new_chart_block(
      chart_type = "scatter", x = "Petal.Length", y = "value",
      color = "Species", facet = "value_measure",
      block_name = "Sepal measures vs petal length (facetted)"
    ),

    # 3. TWO PICKERS — x and y both viewer-picked; copies allow picking the
    #    same measure on both axes (agreement plot).
    pick_xy = new_picker_block(
      state = list(pickers = list(
        list(
          into = "x_value",
          choices = c("Petal.Length", "Petal.Width"),
          selected = "Petal.Length",
          multiple = FALSE
        ),
        list(
          into = "y_value",
          choices = c("Sepal.Length", "Sepal.Width"),
          selected = "Sepal.Length",
          multiple = FALSE
        )
      )),
      block_name = "X and Y"
    ),
    chart_xy = new_chart_block(
      chart_type = "scatter", x = "x_value", y = "y_value",
      color = "Species",
      block_name = "Picked measure vs picked measure"
    ),

    # 4. OPTIONAL — an optional "Color" picker: the face offers "(none)".
    #    Picking (none) leaves the picker inert, so no Color column is
    #    emitted and the chart drops the colour legend entirely (instead of
    #    a single phantom group). Toggle in the gear via "Optional".
    pick_opt = new_picker_block(
      state = list(pickers = list(
        list(
          into = "Color",
          choices = c("Species"),
          selected = "Species",
          multiple = FALSE,
          optional = TRUE
        )
      )),
      block_name = "Colour (optional)"
    ),
    chart_opt = new_chart_block(
      chart_type = "scatter", x = "Petal.Length", y = "Sepal.Length",
      color = "Color",
      block_name = "Coloured by picked column (or none)"
    )
  ),
  links = links(
    from = c("data", "pick_one", "data", "pick_multi", "data", "pick_xy",
             "data", "pick_opt"),
    to   = c("pick_one", "chart_one", "pick_multi", "chart_multi",
             "pick_xy", "chart_xy", "pick_opt", "chart_opt")
  ),
  grids = list(
    single = dock_grid(
      "pick_one", "chart_one",
      orientation = "vertical", sizes = c(1, 2)
    ),
    multi = dock_grid(
      "pick_multi", "chart_multi",
      orientation = "vertical", sizes = c(1, 2)
    ),
    xy = dock_grid(
      "pick_xy", "chart_xy",
      orientation = "vertical", sizes = c(1, 2)
    ),
    optional = dock_grid(
      "pick_opt", "chart_opt",
      orientation = "vertical", sizes = c(1, 2)
    )
  ),
  options = dock_board_options(),
  active = "single",
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
