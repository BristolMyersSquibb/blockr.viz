#' The table view's scripts and stylesheets
#'
#' blockr.ui's controls (the gear engine draws with them), the JS core and
#' the specs (viz_core_dep()), the chart's header and gear styles, then the
#' scripts in inst/js/table/scripts.txt and the view's stylesheets.
#' @noRd
table_view_dep <- memoise0(function() {
  script <- file.path("table", js_script_list("table"))
  css <- c("table.css", "table-structured.css", "table-groups.css",
           "table-view.css", "table-cards.css")
  htmltools::tagList(
    blockr.ui::controls_dep(),
    viz_core_dep(),
    chart_css_dep(),
    htmltools::htmlDependency(
      name = "blockr-viz-table",
      version = js_version(script),
      src = system.file(package = "blockr.viz"),
      script = file.path("js", script),
      stylesheet = file.path("css", css)
    )
  )
})

#' A table drawn without Shiny
#'
#' The element `inst/js/table/static.js` draws on a page without Shiny (the
#' Web page download, a knitted document): the payload as JSON, under the
#' table view's scripts and stylesheets. The view draws every row, with no
#' gear and nothing sent.
#'
#' @param model A TableModel list, or `NULL` for none.
#' @param config The settings the view reads: `layout`, `look`, `by`,
#'   `sortable`, `column_axis`, `value_labels`, `bar_width`.
#' @param derived `titles` (strings), `legend`, `empty`.
#' @param max_height A CSS height for the box, or `NULL` for the table's
#'   full length.
#' @return A tag list.
#' @noRd
table_static <- function(model, config, derived = list(), max_height = NULL) {
  derived <- derived[!vapply(derived, is.null, logical(1L))]
  payload <- list(
    state_rev = 1L, data_rev = 1L, columns = list(),
    config = config,
    derived = if (length(derived)) derived else structure(list(), names = character()),
    data = model %||% structure(list(), names = character())
  )
  # A "</script>" inside a label must not end the block.
  json <- gsub("</", "<\\/", as.character(wire_json(payload)), fixed = TRUE)
  htmltools::tagList(
    table_view_dep(),
    htmltools::div(
      class = "blockr-viz-static", `data-viz-view` = "table",
      style = if (!is.null(max_height)) {
        paste0("max-height:", max_height, ";overflow:auto;")
      },
      htmltools::tags$script(type = "application/json", htmltools::HTML(json))
    )
  )
}

#' The titles of a printed table, as the view's `derived$titles`
#' @noRd
table_static_titles <- function(title, subtitle, caption) {
  txt <- function(x) if (is.character(x) && length(x) == 1L && nzchar(x)) x
  list(title = txt(title), subtitle = txt(subtitle), caption = txt(caption))
}

#' A printed groups table as the view's model and settings
#'
#' @param x A `summarize_exhibit`.
#' @return `list(model, config, derived)` for table_static().
#' @noRd
table_static_groups <- function(x) {
  args <- x$args
  cells <- x$cells
  # A printed table has nobody to click its chevrons, so a nested one opens
  # whole, as the painted page draws every row too.
  if (!is.null(cells)) cells$expanded <- TRUE
  axis <- !isFALSE(args$axis)
  list(
    model = table_groups_view(list(prep = x$prep, cells = cells), axis = axis),
    config = list(
      layout = "groups", look = "table",
      by = I(as.character(unlist(args$by))),
      sortable = !isFALSE(args$sortable), column_axis = axis,
      value_labels = !isFALSE(args$value_labels),
      bar_width = args$bar_width %||% "fit"
    ),
    derived = list(
      legend = summarize_legend_spec(x$prep),
      empty = if (!is.null(x$prep$err)) {
        list(kind = "message", message = x$prep$err)
      }
    )
  )
}
