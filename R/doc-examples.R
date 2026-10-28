# Examples for exported functions whose roxygen sits in other files. Each
# block adds an @examples section to that function's help page.

#' @name as_annotated_df
#' @rdname as_annotated_df
#' @examples
#' tbl <- summary_table(iris, vars = "Species")
#' as_annotated_df(tbl)
NULL

#' @name as_plain_df
#' @rdname as_plain_df
#' @examples
#' tbl <- summary_table(iris, vars = "Sepal.Length", by = "Species")
#' as_plain_df(tbl)
NULL

#' @name drill_source
#' @rdname drill_source
#' @examples
#' tbl <- summary_table(iris, vars = "Species")
#' # The rows behind the second row of the table, a click on "versicolor"
#' nrow(drill_source(tbl[2, ], source = iris))
NULL

#' @name new_viz_exhibit
#' @rdname new_viz_exhibit
#' @examples
#' tbl <- summary_table(iris, vars = "Sepal.Length", by = "Species")
#' ex <- new_viz_exhibit(tbl, title = "Sepal length", kind = "table",
#'                       meta = list(dataset = "iris"))
#' exhibit_kind(ex)
#' exhibit_titles(ex)$title
#' exhibit_data(ex)
NULL

#' @name exhibit-accessors
#' @rdname exhibit-accessors
#' @examples
#' ex <- new_viz_exhibit(summary_table(iris, vars = "Species"),
#'                       title = "Species", kind = "table")
#' exhibit_data(ex)
#' exhibit_titles(ex)
#' exhibit_kind(ex)
#' exhibit_meta(ex)
#' exhibit_picture(ex)
#' exhibit_render(ex)
NULL

#' @name register_viz_exporter
#' @rdname register_viz_exporter
#' @examples
#' # A CSV download, offered by every block from now on
#' register_viz_exporter(
#'   "csv", "CSV",
#'   function(exhibit, file) {
#'     utils::write.csv(exhibit_data(exhibit), file, row.names = FALSE)
#'   }
#' )
NULL

#' @name html_exhibit
#' @rdname html_exhibit
#' @examples
#' tbl <- summary_table(iris, vars = "Sepal.Length", by = "Species")
#' html_exhibit(tbl, title = "Sepal length by species")
NULL

#' @name report_call
#' @rdname report_call
#' @examples
#' report_call(new_table_block(), "tbl1")
#' report_call(new_chart_block(), "chart1")
NULL

#' @name write_annotated_xlsx
#' @rdname write_annotated_xlsx
#' @examplesIf requireNamespace("openxlsx", quietly = TRUE)
#' f <- tempfile(fileext = ".xlsx")
#' write_annotated_xlsx(
#'   summary_table(iris, vars = "Sepal.Length", by = "Species"),
#'   f,
#'   title = "Sepal length by species"
#' )
#' unlink(f)
NULL

#' @name write_exhibit_png
#' @rdname write_exhibit_png
#' @examplesIf requireNamespace("systemfonts", quietly = TRUE)
#' ex <- static_summarize_table(
#'   mtcars, by = "cyl",
#'   summaries = list(list(type = "simple", func = "count", show = "bar"))
#' )
#' f <- tempfile(fileext = ".png")
#' write_exhibit_png(ex, f)
#' unlink(f)
NULL

#' @name ctrl_targets
#' @rdname ctrl_targets
#' @examples
#' # Outside a board with the control channel there is nothing to target
#' ctrl_targets(session = shiny::MockShinySession$new())
NULL

#' @name ctrl_auto_target
#' @rdname ctrl_auto_target
#' @examples
#' ctrl_auto_target(session = shiny::MockShinySession$new())
NULL

# Exported for other blockr packages, not for users: no example needed.

#' @name dt_download_control
#' @rdname dt_download_control
#' @keywords internal
NULL

#' @name tooltip_card_dep
#' @rdname tooltip_card_dep
#' @keywords internal
NULL
