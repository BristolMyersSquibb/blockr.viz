# The payload a page without Shiny carries for the table view
# (table_static()), read back from rendered HTML.
static_payloads <- function(html) {
  html <- paste(as.character(html), collapse = "\n")
  m <- regmatches(html, gregexpr(
    "<div class=\"blockr-viz-static\"[^>]*>\\s*<script type=\"application/json\">(.*?)</script>",
    html
  ))[[1L]]
  lapply(m, function(x) {
    json <- sub("^.*<script type=\"application/json\">", "", x)
    json <- sub("</script>$", "", json)
    jsonlite::fromJSON(gsub("<\\/", "</", json, fixed = TRUE),
                       simplifyVector = FALSE)
  })
}
