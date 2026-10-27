#' The table view's scripts and stylesheets
#'
#' blockr.ui's controls (the gear engine draws with them), the JS core and
#' the specs (viz_core_dep()), the chart's header and gear styles, then the
#' scripts in inst/js/table/scripts.txt and the view's stylesheets.
#' @noRd
table_view_dep <- memoise0(function() {
  script <- file.path("table", js_script_list("table"))
  css <- c("table-structured.css", "table-groups.css", "table-view.css",
           "table-cards.css")
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
