# The chart-only "pct_distinct" aggregation ("% of panel"): the share of the
# PANEL's distinct values, not of the bar (bar_mode "percent" already does
# that) and not of the grid. Like `identity` it lives only in the JS engine and
# is deliberately absent from the shared AGG_FNS, so it has no R twin and
# cannot join the golden cross-test; this JS-only test guards it instead,
# through the same shipped engine and node runner.
#
# What makes it worth having rather than a precomputed percentage column: the
# denominator is counted from the rows the panel actually holds, AFTER
# faceting. So it is right for whatever the user facets by, it follows an
# upstream filter, and it cannot fall out of sync the way a joined-on N does.
# Its partner is na_group = "drop" (test-agg-golden.R): rows that exist only to
# be counted -- a subject with no event -- draw no bar and still count.

skip_if_no_node <- function() {
  skip_on_cran()
  skip_if(Sys.which("node") == "", "node not available")
}

js_aggregate <- function(rows, config) {
  runner <- testthat::test_path("fixtures", "agg-golden-runner.js")
  engine <- system.file("js", "drilldown-agg.js", package = "blockr.viz")
  payload <- jsonlite::toJSON(
    list(rows = rows, config = config),
    dataframe = "rows", na = "null", auto_unbox = TRUE
  )
  out <- system2(
    Sys.which("node"), shQuote(c(runner, engine)),
    input = as.character(payload), stdout = TRUE, stderr = TRUE
  )
  jsonlite::fromJSON(paste(out, collapse = ""), simplifyVector = FALSE)
}

val_by_group <- function(js) {
  stats::setNames(lapply(js, function(r) r$value),
                  vapply(js, function(r) r$group, ""))
}

# The worked example from the design note. Four subjects in one arm; S4 had no
# adverse event and is carried as a row with no term, which is what a
# right-join flatten produces. Diarrhoea is 2 of the 4 TREATED subjects, not 2
# of the 3 who happen to appear in the event table.
ae <- data.frame(
  USUBJID = c("S1", "S1", "S2", "S3", "S4"),
  AEDECOD = c("Diarrhoea", "Diarrhoea", "Diarrhoea", "Nausea", NA),
  TRT     = "A",
  stringsAsFactors = FALSE
)

test_that("the denominator is the panel's subjects, not the bars'", {
  skip_if_no_node()
  js <- js_aggregate(ae, list(group = "AEDECOD", value = "USUBJID",
                              func = "pct_distinct", na_group = "drop"))
  v <- val_by_group(js)
  expect_setequal(names(v), c("Diarrhoea", "Nausea"))
  expect_equal(v[["Diarrhoea"]], 2 / 4)   # NOT 2/3
  expect_equal(v[["Nausea"]], 1 / 4)
})

test_that("the row with no category still counts, which is the whole point", {
  skip_if_no_node()
  # Same data minus S4: the identical bars, a different denominator. If a
  # transform upstream ever drops the population rows this is the shape of the
  # damage -- 67% instead of 50%, plausible and wrong.
  no_pop <- ae[!is.na(ae$AEDECOD), ]
  js <- js_aggregate(no_pop, list(group = "AEDECOD", value = "USUBJID",
                                  func = "pct_distinct", na_group = "drop"))
  expect_equal(val_by_group(js)[["Diarrhoea"]], 2 / 3)
})

test_that("each facet divides by its own population", {
  skip_if_no_node()
  d <- rbind(
    ae,
    data.frame(USUBJID = c("T1", "T2"), AEDECOD = c("Diarrhoea", NA),
               TRT = "B", stringsAsFactors = FALSE)
  )
  js <- js_aggregate(d, list(group = "AEDECOD", facet = "TRT",
                             value = "USUBJID", func = "pct_distinct",
                             na_group = "drop"))
  by_facet <- stats::setNames(
    lapply(js, function(r) r$value),
    vapply(js, function(r) paste(r$facet, r$group), "")
  )
  expect_equal(by_facet[["A Diarrhoea"]], 2 / 4)
  expect_equal(by_facet[["B Diarrhoea"]], 1 / 2)  # its own panel, not 3/6
})

test_that("a colour split shares the panel denominator, so segments sum to it", {
  skip_if_no_node()
  # One subject per (term, grade) after an upstream worst-case dedup, which is
  # what makes the stack meaningful: the segments of a term sum to that term's
  # share of the panel rather than over-totalling.
  d <- data.frame(
    USUBJID = c("S1", "S2", "S3", "S4"),
    AEDECOD = c("Diarrhoea", "Diarrhoea", "Nausea", NA),
    GRADE   = c("1", "2", "1", NA),
    stringsAsFactors = FALSE
  )
  js <- js_aggregate(d, list(group = "AEDECOD", color = "GRADE",
                             value = "USUBJID", func = "pct_distinct",
                             na_group = "drop"))
  diarrhoea <- Filter(function(r) r$group == "Diarrhoea", js)
  expect_equal(sum(vapply(diarrhoea, function(r) r$value, 0)), 2 / 4)
})

test_that("a panel with no usable value is a gap, not a fabricated zero", {
  skip_if_no_node()
  d <- data.frame(USUBJID = c(NA, NA), AEDECOD = c("Diarrhoea", "Nausea"),
                  stringsAsFactors = FALSE)
  js <- js_aggregate(d, list(group = "AEDECOD", value = "USUBJID",
                             func = "pct_distinct"))
  expect_true(all(vapply(js, function(r) is.null(r$value), TRUE)))
})

test_that("without na_group = 'drop' the population row becomes a category", {
  skip_if_no_node()
  # The default is unchanged behaviour, so an existing board keeps its
  # nameless bar rather than silently gaining a different denominator.
  js <- js_aggregate(ae, list(group = "AEDECOD", value = "USUBJID",
                              func = "pct_distinct"))
  expect_true("" %in% vapply(js, function(r) r$group, ""))
})
