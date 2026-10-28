# The stylesheets of the tables R writes as HTML itself (html_table(), the
# records Web page): inst/css/exhibit-table-*.css, the text the R strings
# had. The groups and cards pages are drawn by the table view, with its own
# stylesheets (table_view_dep(), R/table-dep.R).

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

#' The table body and header cell rules (inst/css/exhibit-table-delta.css)
#'
#' @param scope The container class the rules hang off. The structured
#'   table's renderer passes its own, so the rules do not reach a flat table
#'   in the same container.
#' @noRd
html_table_delta_css <- function(scope = ".blockr-html-table-container") {
  css <- viz_css_file("exhibit-table-delta.css")
  if (!identical(scope, ".blockr-html-table-container")) {
    css <- gsub(".blockr-html-table-container", scope, css, fixed = TRUE)
  }
  css
}

#' The chevron's stroke (inst/css/exhibit-table-chev.css)
#' @noRd
html_table_chev_css <- function() {
  viz_css_file("exhibit-table-chev.css")
}
