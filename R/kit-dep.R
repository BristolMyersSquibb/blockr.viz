# The browser side of a block view: the core scripts every view shares
# (inst/js/core/scripts.txt, the binding, the gear engine, the title slots),
# then each view's own scripts and stylesheets.

# Scripts listed in a scripts.txt under inst/js, in load order, relative to
# inst/js. The node test harness reads the same lists.
viz_scripts <- function(dir) {
  lines <- trimws(readLines(
    system.file("js", dir, "scripts.txt", package = "blockr.viz"),
    warn = FALSE
  ))
  file.path(dir, lines[nzchar(lines) & !startsWith(lines, "#")])
}

#' The core scripts every block view loads
#'
#' blockr.ui's controls first: the gear engine draws with them.
#' @noRd
viz_core_dep <- memoise0(function() {
  htmltools::tagList(
    blockr.ui::controls_dep(),
    htmltools::htmlDependency(
      name = "blockr-viz-core",
      version = as.character(utils::packageVersion("blockr.viz")),
      src = system.file("js", package = "blockr.viz"),
      script = viz_scripts("core")
    )
  )
})

#' The table view's scripts and stylesheets
#'
#' After the core: the table's generated spec, the scripts in
#' inst/js/table/scripts.txt, and the view's stylesheets, which the chart's
#' header and gear styles sit under.
#' @noRd
table_view_dep <- memoise0(function() {
  htmltools::tagList(
    viz_core_dep(),
    chart_css_dep(),
    htmltools::htmlDependency(
      name = "blockr-viz-table",
      version = as.character(utils::packageVersion("blockr.viz")),
      src = system.file(package = "blockr.viz"),
      script = file.path("js", c("spec/table.js", viz_scripts("table"))),
      stylesheet = file.path("css", c("table-structured.css",
                                      "table-groups.css", "table-view.css",
                                      "table-cards.css"))
    )
  )
})
