# The stylesheet of the tables R writes as HTML itself (html_table(), the
# records Web page): the view's structured rules, rescoped. The groups and
# cards pages are drawn by the table view (table_view_dep(), R/table-dep.R).

# A stylesheet of inst/css as one string, read once per session.
viz_css_file <- local({
  cache <- new.env(parent = emptyenv())
  function(name) {
    if (is.null(cache[[name]])) {
      path <- system.file("css", name, package = "blockr.viz")
      x <- readChar(path, file.info(path)$size, useBytes = TRUE)
      Encoding(x) <- "UTF-8"
      cache[[name]] <- x
    }
    cache[[name]]
  }
})

#' The structured table's rules for a page R writes itself
#'
#' The view's own sheet (inst/css/table-structured.css), its scope
#' `.drilldown-table-structured` swapped for `scope`. The chevron's stroke
#' rule hangs off `.blockr-html-table-container` in either case.
#' @param scope The container class the rules hang off.
#' @noRd
html_table_delta_css <- function(scope = ".blockr-html-table-container") {
  gsub(".drilldown-table-structured", scope,
       viz_css_file("table-structured.css"), fixed = TRUE)
}
