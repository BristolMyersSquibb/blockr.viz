# The stylesheets of the pages R writes itself: the web page download, the
# groups and cards exhibits, interactive_table() and the old table chrome
# (inst/css/exhibit-table-*.css, the text the R strings had). The table view
# has its own (table_view_dep(), R/table-dep.R); the exporters switch to
# those once their markup carries the classes the view's rules read.

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

#' The base table rules (inst/css/exhibit-table-base.css)
#' @noRd
html_table_shared_css_fallback <- function() {
  viz_css_file("exhibit-table-base.css")
}

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

#' The groups layout's rules (inst/css/exhibit-table-groups.css)
#' @noRd
summarize_table_css <- function() {
  viz_css_file("exhibit-table-groups.css")
}
