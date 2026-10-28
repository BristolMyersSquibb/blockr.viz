#' Chart block
#'
#' A block that draws its input as an interactive chart (ECharts) and, when
#' asked to, filters the blocks downstream by the marks a reader clicks. Its
#' output is its input, filtered by the last click.
#'
#' Every argument that names a column names a data column, never a literal
#' value. Each does one thing: `color` colors, `series` splits into series,
#' `facet` splits into panels, `label` writes on the marks.
#'
#' @section The axes:
#' `x` and `y` are the screen axes for every type, as in ggplot2. The
#' aggregated types (bar, waterfall, pie, treemap, boxplot, pointrange) count
#' or aggregate rows per category: one of `x` and `y` is the category, the
#' other the column `func` aggregates, and which way the chart lies follows
#' from that. `x = "class"` draws vertical bars of counts, `y = "class"`
#' horizontal ones, and `x = "sales", y = "region", func = "sum"` horizontal
#' bars of summed sales. With one column and a count that column is the
#' category; with two, a numeric column beside a non-numeric one is the
#' value; otherwise the chart is vertical. `orientation` decides when the
#' data cannot, for instance a distinct count of one text column by another.
#' For pie and treemap `x` is the slice and `y` the value; for a gantt `x`
#' and `xend` are the interval and `y` the lane; a histogram bins `x`.
#'
#' `ymin` and `ymax` name intervals in pairs, the first pair the outermost:
#' bands on a line or an area, error bars on a scatter. They must name the
#' same number of columns, or the block is an error.
#'
#' @section Clicks:
#' A click filters only when `drill` is set: to a column, or to `"auto"`,
#' the clicked mark's own category, bin or lane. The block's output is then
#' the rows behind the clicked mark until the reader clicks another mark or
#' Reset. A brush on a scatter or a line filters to the brushed range. With
#' `drill_target` set, a click is sent to that filter block instead, and the
#' chart keeps no selection of its own.
#'
#' @section Titles:
#' `title`, `subtitle` and `caption` are templates. `NULL` takes the input's
#' own label, subtitle or caption, and `""` shows nothing. In a template,
#' `{col}` lists a column's values (up to eight), `{label(col)}` its label,
#' `{n}` the number of rows, `{n_distinct(col)}` the number of distinct
#' values, `{filters}` the filters upstream, `{highlight}` what `highlight`
#' picks, and `{@arg}` the value of one of the block's arguments, which the
#' reader can change by clicking it. Text in `[...]` disappears when a value
#' in it is empty.
#'
#' @section Older code and boards:
#' Argument names and values of blockr.viz before 0.3.0 (`chart_type`,
#' `group`, `value`, `bar_mode`, `y_scale`, ...) still work in code, with a
#' warning, and boards saved with them restore unchanged. A value an
#' argument does not take is an error. Any other name is kept as a block
#' attribute, for blockr.core and other packages (blockr.dock's `visible`).
#'
#' @eval viz_spec_params("chart", dots = paste(
#'   "Arguments for [blockr.core::new_block()] (`block_name`, ...), and",
#'   "argument names of blockr.viz before 0.3.0, which are translated with a",
#'   "warning."
#' ))
#' @return A chart block (class `chart_block`), for a blockr board.
#' @seealso [new_table_block()]
#' @examples
#' # Aggregated: mean sepal length per species, as vertical bars
#' new_chart_block(type = "bar", x = "Species", y = "Sepal.Length",
#'                 func = "mean")
#'
#' # One mark per row: a scatter colored by species, filtering on click
#' new_chart_block(type = "scatter", x = "Sepal.Length", y = "Petal.Length",
#'                 color = "Species", drill = "auto")
#'
#' # A line over time
#' new_chart_block(type = "line", x = "date", y = "unemploy")
#'
#' # Distribution: a box per category
#' new_chart_block(type = "boxplot", x = "cyl", y = "mpg")
#'
#' # Histogram: one numeric column, cut into bins
#' new_chart_block(type = "histogram", x = "mpg", bins = 10)
#'
#' # Timeline: intervals on lanes
#' new_chart_block(type = "gantt", x = "start", xend = "end", y = "task")
#'
#' if (interactive()) {
#'   blockr.core::serve(
#'     new_chart_block(type = "bar", x = "Species", y = "Sepal.Length",
#'                     func = "mean", drill = "auto"),
#'     data = list(data = iris)
#'   )
#' }
#' @name new_chart_block
NULL
