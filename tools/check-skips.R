# Skipped R tests, from testthat's JUnit results. Fails when a browser test
# (shinytest2, chromote) was skipped: those have to run in CI.
#
#   Rscript tools/check-skips.R results/testthat.xml

`%|%` <- function(x, y) ifelse(is.na(x), y, x)

doc <- xml2::read_xml(commandArgs(TRUE)[1L])
skips <- xml2::xml_find_all(doc, "//testcase[skipped]")
suite <- vapply(skips, function(x) {
  xml2::xml_attr(xml2::xml_find_first(x, "ancestor::testsuite"), "name")
}, character(1L))
why <- vapply(skips, function(x) {
  s <- xml2::xml_find_first(x, "skipped")
  trimws(paste(xml2::xml_attr(s, "message") %|% "", xml2::xml_text(s)))
}, character(1L))
browser <- grepl("browser|chrom|e2e", why, ignore.case = TRUE)

cat(sprintf("skipped R tests: %d, of them browser tests: %d\n",
            length(skips), sum(browser)))
if (length(skips)) {
  tab <- sort(table(paste0(suite, ": ", substr(why, 1L, 80L))),
              decreasing = TRUE)
  writeLines(sprintf("  %3d  %s", as.integer(tab), names(tab)))
}
quit(status = if (any(browser)) 1L else 0L)
